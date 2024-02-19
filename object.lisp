(in-package #:cffi-object)

(declaim (inline make-cobject))
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
  (let ((pointer (cobject-pointer cobject))
        (deallocator (cobject-allocator-deallocator *cobject-allocator*)))
    (if (eq deallocator #'values) cobject (tg:finalize cobject (lambda () (funcall deallocator pointer))))))

(defun unmanage-cobject (cobject)
  (tg:cancel-finalization cobject)
  (cobject-pointer cobject))

(defgeneric cobject-type (object)
  (:method (object)
    (type-of object)))
