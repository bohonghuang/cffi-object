(in-package #:cffi-object)

(defstruct (carray (:include cpointer)
                   (:constructor %make-carray))
  (dimensions '(0) :type (cons fixnum null)))

(defun caref (array &rest subscripts &aux (subscript (first subscripts)))
  (unless (<= 0 subscript (1- (first (carray-dimensions array))))
    (error "Index ~D is out of bound." subscript))
  (cref array subscript))

(defun (setf caref) (value array &rest subscripts &aux (subscript (first subscripts)))
  (unless (<= 0 subscript (1- (first (carray-dimensions array))))
    (error "Index ~D is out of bound." subscript))
  (setf (cref array subscript) value))

(defmethod print-object ((array carray) stream)
  (print-unreadable-object (array stream)
    (loop :with length := (first (carray-dimensions array))
          :for i :below length
          :if (< i 10)
            :unless (zerop i)
              :do (format stream "~%  ")
            :end
            :and :do (prin1 (caref array i) stream)
          :else
            :return (format stream " ... [~D elements elided]" (- length 10)))))

(defstruct (displaced-carray (:include carray)
                             (:constructor %make-displaced-carray))
  (displaced-index-offset 0 :type fixnum))

(defun displaced-carray-displaced-to (instance)
  (displaced-carray-shared-from instance))

(defun carray-displacement (array)
  (typecase array
    (displaced-carray
     (values (displaced-carray-displaced-to array)
             (displaced-carray-displaced-index-offset array)))
    (t (values nil nil))))

(declaim (inline clength))
(defun clength (carray)
  (first (carray-dimensions carray)))

(defun make-carray (dimensions
                    &key element-type
                      initial-element initial-contents
                      displaced-to
                      (displaced-index-offset 0))
  (unless (listp dimensions)
    (setf dimensions (list dimensions)))
  (let* ((primitive-type-p (primitive-type-p element-type))
         (pointer-type-p (and (listp element-type) (eq (first element-type) 'cpointer)))
         (element-size (cobject-class-object-size element-type))
         (total-size (* element-size (reduce #'* dimensions)))
         (pointer (if displaced-to (cffi:inc-pointer (cobject-pointer displaced-to) (* element-size displaced-index-offset))
                      (cffi:foreign-alloc :uint8 :count total-size)))
         (array (if displaced-to
                    (progn
                      (assert (<= 0 displaced-index-offset (+ displaced-index-offset (first dimensions)) (first (carray-dimensions displaced-to))))
                      (assert (cobject-type= element-type (carray-element-type displaced-to)))
                      (%make-displaced-carray :pointer pointer
                                              :dimensions dimensions
                                              :element-type element-type
                                              :shared-from displaced-to
                                              :displaced-index-offset displaced-index-offset))
                    (manage-cobject (%make-carray :pointer pointer
                                                  :dimensions dimensions
                                                  :element-type element-type)))))
    (when initial-element
      (assert (null initial-contents))
      (assert (null displaced-to))
      (cond
        (primitive-type-p
         (loop :for i :of-type fixnum :below (first dimensions)
               :do (setf (cffi:mem-aref pointer primitive-type-p i) initial-element)))
        (pointer-type-p
         (loop :for i :of-type fixnum :below (first dimensions)
               :do (setf (cffi:mem-aref pointer :pointer i) (cobject-pointer initial-element))))
        (t (loop :with src := (cobject-pointer initial-element)
                 :for i :of-type fixnum :below (first dimensions)
                 :do (memcpy (cffi:inc-pointer pointer (* i element-size)) src element-size)))))
    (when initial-contents
      (assert (null initial-element))
      (assert (null displaced-to))
      (etypecase initial-contents
        (carray
         (assert (equal dimensions (carray-dimensions initial-contents)))
         (memcpy pointer (cobject-pointer initial-contents) total-size))
        (sequence
         (assert (= (first dimensions) (length initial-contents)))
         (let ((i 0))
           (map nil (cond
                      (primitive-type-p
                       (lambda (object)
                         (setf (cffi:mem-aref pointer primitive-type-p i) object)
                         (incf i)))
                      (pointer-type-p
                       (lambda (object)
                         (setf (cffi:mem-aref pointer :pointer i) (cobject-pointer object))
                         (incf i)))
                      (t (lambda (object)
                           (memcpy (cffi:inc-pointer pointer (* i element-size))
                                   (cobject-pointer object) element-size)
                           (incf i))))
                initial-contents)))))
    array))

(defun make-unmanaged-carray (pointer element-type dimensions)
  (unless (listp dimensions) (setf dimensions (list dimensions)))
  (%make-carray :pointer pointer :dimensions dimensions :element-type element-type))

(defun make-managed-carray (pointer element-type dimensions)
  (manage-cobject (make-unmanaged-carray pointer element-type dimensions)))

(setf (fdefinition 'unmanage-carray) (fdefinition 'unmanage-cobject))

(defun creplace (target-carray1 source-carray2
                 &key
                   (start1 0) (end1 (clength target-carray1))
                   (start2 0) (end2 (clength source-carray2)))
  (assert (cobject-type= (carray-element-type target-carray1) (carray-element-type source-carray2)))
  (assert (<= 0 (- end2 start2) (- end1 start1)))
  (let ((element-size (cobject-class-object-size (carray-element-type target-carray1))))
    (memcpy (cffi:inc-pointer (cobject-pointer target-carray1) (* start1 element-size))
            (cffi:inc-pointer (cobject-pointer source-carray2) (* start2 element-size))
            (* (- end2 start2) element-size))
    target-carray1))

(defun cfill (carray item &key (start 0) (end (clength carray)))
  (loop :for i :from start :below end
        :do (setf (caref carray i) item)
        :finally (return carray)))

(defun carray-equal (array1 array2)
  (unless (= (clength array1) (clength array2))
    (return-from carray-equal nil))
  (cpointer-equal array1 array2 (clength array1)))

(defun carray-list (array)
  (loop :for i :below (clength array)
        :collect (caref array i)))

(defun carray-array (carray)
  (loop :with length := (clength carray)
        :with array := (make-array length)
        :for i :below length
        :do (setf (aref array i) (caref carray i))
        :finally (return array)))
