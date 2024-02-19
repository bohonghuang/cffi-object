(in-package #:cffi-object)

(defstruct (cpointer (:include cobject)
                     (:constructor %make-cpointer))
  (element-type nil :type (or symbol cons)))

(defmethod cobject-type ((pointer cpointer))
  `(cpointer ,(cpointer-element-type pointer)))

(defmethod print-object ((pointer cpointer) stream)
  (if *print-readably*
      (progn
        (format stream "#.")
        (prin1 `(pointer-cpointer
                 (cffi:make-pointer
                  ',(cffi:pointer-address (cpointer-pointer pointer)))
                 ',(cpointer-element-type pointer))
               stream))
      (print-unreadable-object (pointer stream)
        (format stream #.(concatenate 'string "~A @0x~" (prin1-to-string (* 2 (cffi:foreign-type-size :size))) ",'0X")
                (cpointer-element-type pointer)
                (cffi:pointer-address (cobject-pointer pointer))))))

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
          (multiple-value-call (cobject-class-definition-copier definition)
            value (funcall
                   (cobject-class-definition-internal-constructor definition)
                   :pointer pointer
                   :shared-from cpointer)))
        (setf (cffi:mem-aref (cobject-pointer cpointer) type subscript) value))))

(defun cpointer-equal (pointer1 pointer2 &optional (count 1))
  (unless (cobject-type= (cpointer-element-type pointer1) (cpointer-element-type pointer2))
    (return-from cpointer-equal nil))
  (zerop (memcmp (cobject-pointer pointer1)
                 (cobject-pointer pointer2)
                 (* (cobject-class-object-size (cpointer-element-type pointer1)) count))))

(defun cpointer-eq (pointer1 pointer2)
  (cffi:pointer-eq (cobject-pointer pointer1) (cobject-pointer pointer2)))

(defun pointer-cpointer (pointer element-type)
  (%make-cpointer :pointer pointer :element-type element-type))
