(in-package #:cffi-object)

(defparameter *global-cobjects* nil)

(defun global-cobjects-bytes ()
  (loop :with definitions := *cobject-class-definitions*
        :for (name . value) :in *global-cobjects*
        :for cobject := (symbol-value name)
        :for type := (cobject-type (symbol-value name))
        :for (definition ctype) := (multiple-value-list (cobject-class-definition type))
        :for constructor := (cobject-class-definition-internal-constructor definition)
        :for size := (cffi:foreign-type-size ctype)
        :nconc (ccoerce (pointer-carray (cobject-pointer cobject) '(unsigned-byte 8) size) 'list) :into data
        :collect (let ((constructor (ensure-function constructor)) (offset offset) (symbol name))
                   (lambda (carray)
                     (setf (symbol-value symbol) (funcall constructor :pointer (cffi:inc-pointer (carray-pointer carray) offset) :shared-from carray))))
          :into initializers
        :sum size :into offset
        :finally (return (values (replace (cffi:make-shareable-byte-vector offset) data) initializers))))

(defparameter *global-cobject-initializer* nil)

(defun load-global-cobjects ()
  (funcall *global-cobject-initializer*))

(pushnew 'load-global-cobjects uiop:*image-restore-hook*)

(defun save-global-cobjects ()
  (multiple-value-bind (bytes initializers) (global-cobjects-bytes)
    (setf *global-cobject-initializer*
          (lambda ()
            (loop :with carray := (make-carray (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes)
                  :for initializer :in initializers
                  :do (funcall initializer carray))))))

(pushnew 'save-global-cobjects uiop:*image-dump-hook*)

(defparameter *define-global-cobject* 'defparameter)

(defmacro define-global-cobject (name val-form)
  `(progn
     (setf (assoc-value *global-cobjects* ',name) (lambda () ,val-form))
     (,*define-global-cobject* ,name ,val-form)))
