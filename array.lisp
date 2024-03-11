(in-package #:cffi-object)

(defstruct (carray (:include cpointer)
                   (:constructor %make-carray))
  (dimensions '(0) :type (cons fixnum null)))

(defmethod cobject-type ((array carray))
  `(carray ,(carray-element-type array) ,(carray-dimensions array)))

(defun cpointer-carray (cpointer dimensions)
  (%make-carray :pointer (cpointer-pointer cpointer)
                :shared-from (cpointer-shared-from cpointer)
                :element-type (cpointer-element-type cpointer)
                :dimensions (ensure-cons dimensions)))

(defun caref (array &rest subscripts &aux (subscript (first subscripts)))
  (unless (<= 0 subscript (1- (first (carray-dimensions array))))
    (error "Index ~D is out of bound." subscript))
  (cref array subscript))

(defun (setf caref) (value array &rest subscripts &aux (subscript (first subscripts)))
  (unless (<= 0 subscript (1- (first (carray-dimensions array))))
    (error "Index ~D is out of bound." subscript))
  (setf (cref array subscript) value))

(declaim (inline clength)
         (ftype (function (carray) non-negative-fixnum) clength))
(defun clength (carray)
  (first (carray-dimensions carray)))

(defgeneric ccoerce (cobject type))

(defgeneric (setf ccoerce) (value cobject type))

(defmethod ccoerce ((array carray) (type (eql 'list)))
  (declare (ignore type))
  (loop :for i :below (clength array)
        :collect (caref array i)))

(defmethod ccoerce ((cobject cobject) (type list))
  (ccoerce cobject (car type)))

(defmethod ccoerce ((array carray) (type (eql 'simple-vector)))
  (declare (ignore type))
  (make-array (clength array) :initial-contents (ccoerce array 'list)))

(defmethod ccoerce ((array carray) (type (eql 'simple-array)))
  (declare (ignore type))
  (if (symbolp (carray-element-type array))
      (make-array (clength array) :element-type (carray-element-type array)
                                  :initial-contents (ccoerce array 'list))
      (ccoerce array 'simple-vector)))

(defmethod ccoerce ((array carray) (type (eql 'vector)))
  (declare (ignore type))
  (ccoerce array 'simple-array))

(defmethod ccoerce ((array carray) (type (eql 'array)))
  (declare (ignore type))
  (ccoerce array 'simple-array))

(defmethod ccoerce ((array carray) (type (eql 'string)))
  (declare (ignore type))
  (cffi:foreign-string-to-lisp (carray-pointer array)))

(defmethod (setf ccoerce) ((value string) (array carray) (type (eql 'string)))
  (declare (ignore type))
  (cffi:lisp-string-to-foreign value (carray-pointer array) (clength array)))

(defmethod print-object ((array carray) stream)
  (if *print-readably*
      (progn
        (format stream "#.")
        (prin1
         `(make-carray
           ',(carray-dimensions array)
           :element-type ',(carray-element-type array)
           :initial-contents ',(ccoerce array 'array))
         stream))
      (print-unreadable-object (array stream)
        (loop :named print-element-loop
              :with length := (first (carray-dimensions array))
              :initially
                 (case (carray-element-type array)
                   (character (ignore-errors
                               (return-from print-element-loop
                                 (print-object (ccoerce array 'string) stream)))))
              :for i :below length
              :if (< i 10)
                :unless (zerop i)
                  :do (format stream "~%  ")
              :end
              :and :do (prin1 (caref array i) stream)
              :else
                :return (format stream " ... [~D elements elided]" (- length 10))))))

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

(defun make-carray (dimensions
                    &key element-type
                      initial-element initial-contents
                      displaced-to
                      (displaced-index-offset 0))
  (let* ((dimensions (ensure-cons dimensions))
         (primitive-type-p (primitive-type-p element-type))
         (pointer-type-p (and (listp element-type) (eq (first element-type) 'cpointer)))
         (character-type-p (eq element-type 'character))
         (element-foreign-type (nth-value 1 (cobject-class-definition element-type)))
         (element-size (cffi:foreign-type-size element-foreign-type))
         (total-size (* element-size (reduce #'* dimensions)))
         (pointer (if displaced-to (cffi:inc-pointer (cobject-pointer displaced-to) (* element-size displaced-index-offset))
                      (funcall (cobject-allocator-allocator *cobject-allocator*) (make-instance 'cffi::foreign-array-type
                                                                                                :element-type element-foreign-type
                                                                                                :dimensions dimensions))))
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
    (declare (type non-negative-fixnum element-size total-size)
             (type (cons non-negative-fixnum t) dimensions))
    (when initial-element
      (assert (null initial-contents))
      (assert (null displaced-to))
      (cond
        (character-type-p
         (memset pointer (char-code initial-element) total-size))
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
         (unless character-type-p
           (assert (= (first dimensions) (length initial-contents))))
         (let ((i 0))
           (declare (type non-negative-fixnum i))
           (map nil (cond
                      (character-type-p
                       (cffi:lisp-string-to-foreign (coerce initial-contents 'string) pointer total-size)
                       (return-from make-carray array))
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

(defun pointer-carray (pointer element-type dimensions)
  (unless (listp dimensions) (setf dimensions (list dimensions)))
  (%make-carray :pointer pointer :dimensions dimensions :element-type element-type))

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
