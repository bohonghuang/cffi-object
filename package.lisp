(defpackage cffi-object
  (:use #:cl #:alexandria)
  (:nicknames #:cobj)
  (:export #:cobject
           #:cobject-pointer
           #:define-struct-cobject
           #:carray
           #:make-carray
           #:make-unmanaged-carray
           #:make-managed-carray
           #:unmanage-carray
           #:carray-dimensions
           #:carray-displacement
           #:carray-element-type
           #:caref
           #:clength
           #:creplace
           #:cfill
           #:carray-equal))

(in-package #:cffi-object)

(declaim (inline memcpy))
(cffi:defcfun "memcpy" :void
  (dest :pointer)
  (src :pointer)
  (n :size))

(declaim (inline memcmp))
(cffi:defcfun "memcmp" :int
  (s1 :pointer)
  (s2 :pointer)
  (n :size))

(defstruct cobject
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer))

(defstruct cobject-class-definition
  (class nil :type symbol)
  (internal-constructor nil :type symbol)
  (reference-constructor nil :type symbol)
  (slot-accessors nil :type list)
  (copier nil :type symbol)
  (inplace-copier nil :type symbol)
  (predicate nil :type symbol)
  (equality-comparator nil :type symbol)
  (managed-constructor nil :type symbol)
  (unmanaged-constructor nil :type symbol)
  (unmanaged-pointer-accessor nil :type symbol))

(declaim (type list *cobject-class-definitions*))
(defvar *cobject-class-definitions* nil)

(defun primitive-type-p (type)
  (if (consp type)
      (destructuring-case type
        ((signed-byte n)
         (case n
           (8 :int8)
           (16 :int16)
           (32 :int32)
           (64 :int64)))
        ((unsigned-byte n)
         (case n
           (8 :uint8)
           (16 :uint16)
           (32 :uint32)
           (64 :uint64))))
      (case type
        (single-float :float)
        (double-float :double))))

(defun cobject-class-definition (class)
  (if-let ((cons (find class *cobject-class-definitions* :key (compose #'cobject-class-definition-class #'cdr))))
    (values (cdr cons) (car cons))
    (values nil (or (primitive-type-p class) (error "Undefined CFFI object class ~A." class)))))

(defun cobject-class-object-size (class)
  (if-let ((type (nth-value 1 (cobject-class-definition class))))
    (cffi:foreign-type-size type)))

(defun make-unmanaged-cobject (pointer class)
  (funcall
   (cobject-class-definition-internal-constructor
    (cobject-class-definition class))
   :pointer pointer))

(defun manage-cobject (cobject)
  (let ((pointer (cobject-pointer cobject)))
    (tg:finalize cobject (lambda () (cffi:foreign-free pointer)))))

(defun unmanage-cobject (cobject)
  (tg:cancel-finalization cobject)
  (cobject-pointer cobject))

(defun make-managed-cobject (pointer class)
  (manage-cobject (make-unmanaged-cobject pointer class)))

(defstruct (carray (:include cobject)
                   (:constructor %make-carray))
  (dimensions '(0) :type (cons fixnum null))
  (element-type nil :type (or symbol cons)))

(defun caref (array &rest subscripts &aux (subscript (first subscripts)))
  (unless (<= 0 subscript (1- (first (carray-dimensions array))))
    (error "Index ~D is out of bound." subscript))
  (multiple-value-bind (definition type)
      (cobject-class-definition (carray-element-type array))
    (if definition
        (funcall
         (cobject-class-definition-reference-constructor definition)
         :pointer (cffi:mem-aptr (cobject-pointer array) type subscript)
         :source array)
        (cffi:mem-aref (cobject-pointer array) type subscript))))

(defun (setf caref) (value array &rest subscripts &aux (subscript (first subscripts)))
  (unless (<= 0 subscript (1- (first (carray-dimensions array))))
    (error "Index ~D is out of bound." subscript))
  (multiple-value-bind (definition type)
      (cobject-class-definition (carray-element-type array))
    (if definition
        (let* ((element-size (cffi:foreign-type-size type))
               (pointer (cffi:inc-pointer (cobject-pointer array) (* element-size subscript))))
          (memcpy pointer (cobject-pointer value) element-size)
          (funcall
           (cobject-class-definition-reference-constructor definition)
           :pointer pointer
           :source array))
        (setf (cffi:mem-aref (cobject-pointer array) type subscript) value))))

(defmethod print-object ((array carray) stream)
  (print-unreadable-object (array stream)
    (loop :with length := (first (carray-dimensions array))
          :for i :below length
          :if (< i 10)
            :unless (zerop i)
              :do (format stream "~%  ")
            :end
            :and :do (princ (caref array i) stream)
          :else
            :return (format stream " ... [~D elements elided]" (- length 10)))))

(defstruct (displaced-carray (:include carray)
                             (:constructor %make-displaced-carray))
  (displaced-to 0 :type carray)
  (displaced-index-offset nil :type fixnum))

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
         (element-size (cobject-class-object-size element-type))
         (total-size (* element-size (reduce #'* dimensions)))
         (pointer (if displaced-to (cffi:inc-pointer (cobject-pointer displaced-to) (* element-size displaced-index-offset))
                      (cffi:foreign-alloc :uint8 :count total-size)))
         (array (if displaced-to
                    (progn
                      (assert (<= 0 displaced-index-offset (+ displaced-index-offset (first dimensions)) (first (carray-dimensions displaced-to))))
                      (assert (equal element-type (carray-element-type displaced-to)))
                      (%make-displaced-carray :pointer pointer
                                              :dimensions dimensions
                                              :element-type element-type
                                              :displaced-to displaced-to
                                              :displaced-index-offset displaced-index-offset))
                    (manage-cobject (%make-carray :pointer pointer
                                                  :dimensions dimensions
                                                  :element-type element-type)))))
    (when initial-element
      (assert (null initial-contents))
      (assert (null displaced-to))
      (if primitive-type-p
          (loop :for i :of-type fixnum :below (first dimensions)
                :do (setf (cffi:mem-aref pointer primitive-type-p i) initial-element))
          (loop :with src := (cobject-pointer initial-element)
                :for i :of-type fixnum :below (first dimensions)
                :do (memcpy (cffi:inc-pointer pointer (* i element-size)) src element-size))))
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
           (map nil (if primitive-type-p
                        (lambda (object)
                          (setf (cffi:mem-aref pointer primitive-type-p i) object)
                          (incf i))
                        (lambda (object)
                          (memcpy (cffi:inc-pointer pointer (* i element-size))
                                  (cobject-pointer object) element-size)
                          (incf i)))
                initial-contents)))))
    array))

(defun make-unmanaged-carray (pointer dimensions element-type)
  (%make-carray :pointer pointer :dimensions dimensions :element-type element-type))

(defun make-managed-carray (pointer dimensions element-type)
  (manage-cobject (make-unmanaged-carray pointer dimensions element-type)))

(setf (fdefinition 'unmanage-carray) (fdefinition 'unmanage-cobject))

(defun creplace (target-carray1 source-carray2
                 &key
                   (start1 0) (end1 (clength target-carray1))
                   (start2 0) (end2 (clength source-carray2)))
  (assert (equal (carray-element-type target-carray1) (carray-element-type source-carray2)))
  (assert (<= 0 (- end2 start2) (- end1 start1)))
  (let ((element-size (cobject-class-object-size (carray-element-type target-carray1))))
    (memcpy (cffi:inc-pointer (cobject-pointer target-carray1) (* start1 element-size))
            (cffi:inc-pointer (cobject-pointer source-carray2) (* start2 element-size))
            (* (- end2 start2) element-size))))

(defun cfill (carray item &key (start 0) (end (clength carray)))
  (loop :for i :from start :below end
        :do (setf (caref carray i) item)))

(defun carray-equal (carray1 carray2)
  (unless (equal (carray-element-type carray1) (carray-element-type carray2))
    (return-from carray-equal nil))
  (unless (= (clength carray1) (clength carray2))
    (return-from carray-equal nil))
  (zerop (memcmp (cobject-pointer carray1)
                 (cobject-pointer carray2)
                 (* (cobject-class-object-size
                     (carray-element-type carray1))
                    (clength carray1)))))

(defun find-cobject-class-definition (type)
  (or (assoc-value *cobject-class-definitions* type)
      (error "Cannot find the CFFI object class for type ~A." (cffi::name type))))

(defmacro define-struct-cobject ((name ctype) &aux (*package* (symbol-package name)))
  (let* ((type (cffi::ensure-parsed-base-type ctype))
         (predicate (symbolicate name '#:-p))
         (equality-comparator (symbolicate name '#:-equal))
         (constructor (symbolicate '#:make- name))
         (internal-constructor (symbolicate '#:%make- name))
         (copier (symbolicate '#:copy- name))
         (inplace-copier (symbolicate '#:copy- name '#:-into))
         (reference (symbolicate name '#:-reference))
         (reference-constructor (symbolicate '#:make- reference))
         (managed-constructor (symbolicate '#:make-managed- name))
         (unmanaged-constructor (symbolicate '#:make-unmanaged- name))
         (unmanaged-pointer-accessor (symbolicate '#:unmanange- name))
         (slots (cffi:foreign-slot-names type))
         (slot-accessors (mapcar #'cons slots (mapcar (curry #'symbolicate name '#:-) slots))))
    (check-type type cffi::foreign-struct-type)
    (with-gensyms (pointer instance value stream destination)
      `(progn
         (declaim (inline ,internal-constructor))
         (defstruct (,name
                     (:include cobject)
                     (:predicate ,predicate)
                     (:copier nil)
                     (:constructor ,internal-constructor)))
         (declaim (inline ,reference-constructor))
         (defstruct (,reference
                     (:include ,name)
                     (:constructor ,reference-constructor))
           (source nil :type t))
         ,@(loop :for (slot . slot-accessor) :in slot-accessors
                 :for slot-type := (cffi::ensure-parsed-base-type (cffi:foreign-slot-type type slot))
                 :nconc `((declaim (inline ,slot-accessor))
                          (defun ,slot-accessor (,instance)
                            ,(typecase slot-type
                               (cffi::foreign-struct-type
                                `(,(cobject-class-definition-reference-constructor
                                    (find-cobject-class-definition slot-type))
                                  :source ,instance
                                  :pointer (cffi:foreign-slot-pointer (cobject-pointer ,instance) ',type ',slot)))
                               (t `(cffi:foreign-slot-value (cobject-pointer ,instance) ',type ',slot))))
                          (declaim (inline (setf ,slot-accessor)))
                          (defun (setf ,slot-accessor) (,value ,instance)
                            ,(typecase slot-type
                               (cffi::foreign-struct-type
                                `(memcpy (cffi:foreign-slot-pointer (cobject-pointer ,instance) ',type ',slot)
                                         (cobject-pointer ,value) (cffi:foreign-type-size ,slot-type)))
                               (t `(setf (cffi:foreign-slot-value (cobject-pointer ,instance) ',type ',slot) ,value))))))
         (declaim (inline ,constructor))
         (defun ,constructor (&key . ,slots)
           (let* ((,pointer (cffi:foreign-alloc ',type))
                  (,instance (,internal-constructor :pointer ,pointer)))
             ,@(loop :for slot :in slots
                     :collect `(when ,slot
                                 (setf (,(assoc-value slot-accessors slot) ,instance) ,slot)))
             (manage-cobject ,instance)))
         (declaim (inline ,equality-comparator))
         ,(with-gensyms (instance1 instance2)
            `(defun ,equality-comparator (,instance1 ,instance2)
               (zerop (memcmp (cobject-pointer ,instance1)
                              (cobject-pointer ,instance2)
                              (cffi:foreign-type-size ',type)))))
         (declaim (inline ,inplace-copier))
         (defun ,inplace-copier (,instance ,destination)
           (check-type ,instance ,name)
           (check-type ,destination ,name)
           (memcpy (cobject-pointer ,destination) (cobject-pointer ,instance) (cffi:foreign-type-size ',type))
           ,destination)
         (declaim (inline ,copier))
         (defun ,copier (,instance)
           (check-type ,instance ,name)
           (let* ((,pointer (cffi:foreign-alloc ',type))
                  (,destination (,internal-constructor :pointer ,pointer)))
             (manage-cobject
              (,inplace-copier ,instance ,destination))))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (fdefinition ',unmanaged-pointer-accessor) (fdefinition 'unmanage-cobject)))
         (declaim (inline ,unmanaged-constructor))
         (defun ,unmanaged-constructor (,pointer)
           (,internal-constructor :pointer ,pointer))
         (declaim (inline ,managed-constructor))
         (defun ,managed-constructor (,pointer)
           (manage-cobject (,unmanaged-constructor ,pointer)))
         (defmethod print-object ((,instance ,name) ,stream)
           (print-unreadable-object (,instance ,stream)
             (princ ,(string name) ,stream)
             ,@(loop :for (slot . slot-accessor) :in slot-accessors
                     :collect `(format ,stream ," :~A ~A" ,(symbol-name slot) (,slot-accessor ,instance)))
             (format ,stream ,(concatenate 'string " @0x~" (prin1-to-string (cffi:foreign-type-size :size)) ",'0X")
                     (cffi:pointer-address (cobject-pointer ,instance)))))
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (assoc-value *cobject-class-definitions* ',type)
                 (make-cobject-class-definition
                  :class ',name
                  :internal-constructor ',internal-constructor
                  :reference-constructor ',reference-constructor
                  :slot-accessors ',slot-accessors
                  :copier ',copier
                  :inplace-copier ',inplace-copier
                  :predicate ',predicate
                  :equality-comparator ',equality-comparator
                  :managed-constructor ',managed-constructor
                  :unmanaged-constructor ',unmanaged-constructor
                  :unmanaged-pointer-accessor ',unmanaged-pointer-accessor)))))))
