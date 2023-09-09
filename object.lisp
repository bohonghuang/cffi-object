(in-package #:cffi-object)

(defstruct cobject
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer :read-only t)
  (shared-from nil :type (or cobject null) :read-only t))

(defun cobject-eq (a b)
  (cffi:pointer-eq (cobject-pointer a) (cobject-pointer b)))

(defun cobject-class-object-size (type)
  (when-let ((type (nth-value 1 (cobject-class-definition type))))
    (cffi:foreign-type-size type)))

(defun pointer-cobject (pointer type)
  (funcall
   (cobject-class-definition-internal-constructor
    (cobject-class-definition type))
   :pointer pointer))

(defun manage-cobject (cobject)
  (let ((pointer (cobject-pointer cobject)))
    (tg:finalize cobject (lambda () (cffi:foreign-free pointer)))))

(defun unmanage-cobject (cobject)
  (tg:cancel-finalization cobject)
  (cobject-pointer cobject))
