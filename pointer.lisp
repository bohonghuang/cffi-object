(in-package #:cffi-object)

(defstruct (cpointer (:include cobject)
                     (:constructor %make-cpointer))
  (element-type nil :type (or symbol cons)))

(defmethod print-object ((pointer cpointer) stream)
  (print-unreadable-object (pointer stream)
    (format stream #.(concatenate 'string "~A @0x~" (prin1-to-string (* 2 (cffi:foreign-type-size :size))) ",'0X")
            (cpointer-element-type pointer)
            (cffi:pointer-address (cobject-pointer pointer)))))

(defun cref (cpointer &optional (subscript 0))
  (multiple-value-bind (definition type)
        (cobject-class-definition (cpointer-element-type cpointer))
      (if definition
          (funcall
           (cobject-class-definition-internal-constructor definition)
           :pointer (cffi:mem-aptr (cobject-pointer cpointer) type subscript)
           :shared-from cpointer)
          (cffi:mem-aref (cobject-pointer cpointer) type subscript))))

(defun (setf cref) (value cpointer &optional (subscript 0))
  (multiple-value-bind (definition type)
      (cobject-class-definition (cpointer-element-type cpointer))
    (if definition
        (let* ((element-size (cffi:foreign-type-size type))
               (pointer (cffi:inc-pointer (cobject-pointer cpointer) (* element-size subscript))))
          (memcpy pointer (cobject-pointer value) element-size)
          (funcall
           (cobject-class-definition-internal-constructor definition)
           :pointer pointer
           :shared-from cpointer))
        (setf (cffi:mem-aref (cobject-pointer cpointer) type subscript) value))))

(defun cpointer-equal (pointer1 pointer2 &optional (count 1))
  (unless (cobject-type= (cpointer-element-type pointer1) (cpointer-element-type pointer2))
    (return-from cpointer-equal nil))
  (zerop (memcmp (cobject-pointer pointer1)
                 (cobject-pointer pointer2)
                 (* (cobject-class-object-size (cpointer-element-type pointer1)) count))))

(defun cpointer-eq (pointer1 pointer2)
  (cffi:pointer-eq (cobject-pointer pointer1) (cobject-pointer pointer2)))

(defun make-unmanaged-cpointer (pointer element-type)
  (%make-cpointer :pointer pointer :element-type element-type))

(defun make-managed-cpointer (pointer element-type)
  (manage-cobject (make-unmanaged-cpointer pointer element-type)))

(setf (fdefinition 'unmanage-cpointer) (fdefinition 'unmanage-cobject))
