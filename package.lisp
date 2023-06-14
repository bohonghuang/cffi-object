(defpackage cffi-object
  (:use #:cl #:alexandria)
  (:nicknames #:cobj)
  (:export #:cobject
           #:cobject-pointer
           #:define-struct-cobject
           #:carray
           #:make-carray
           #:caref
           #:carray-dimensions
           #:carray-displacement
           #:carray-element-type))

(in-package #:cffi-object)

(declaim (inline memcpy))
(cffi:defcfun "memcpy" :void
  (dest :pointer)
  (src :pointer)
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
  (managed-constructor nil :type symbol)
  (unmanaged-constructor nil :type symbol)
  (unmanaged-pointer-accessor nil :type symbol))

(declaim (type list *cobject-class-definitions*))
(defvar *cobject-class-definitions* nil)

(defun cobject-class-definition (class)
  (if-let ((cons (find class *cobject-class-definitions* :key (compose #'cobject-class-definition-class #'cdr))))
    (values (cdr cons) (car cons))
    (error "Undefined CFFI object class ~A." class)))

(defun cobject-class-object-size (class)
  (cffi:foreign-type-size (nth-value 1 (cobject-class-definition class))))

(defun make-unmanaged-cobject (pointer class)
  (funcall
   (cobject-class-definition-internal-constructor
    (cobject-class-definition class))
   :pointer pointer))

(declaim (inline cobject-attach-finalizer))
(defun cobject-attach-finalizer (cobject)
  (let ((pointer (cobject-pointer cobject)))
    (tg:finalize cobject (lambda () (cffi:foreign-free pointer)))))

(defun make-managed-cobject (pointer class)
  (cobject-attach-finalizer
   (make-unmanaged-cobject pointer class)))

(defun find-cobject-class-definition (type)
  (or (assoc-value *cobject-class-definitions* type)
      (error "Cannot find the CFFI object class for type ~A." (cffi::name type))))

(defmacro define-struct-cobject ((name ctype) &aux (*package* (symbol-package name)))
  (let* ((type (cffi::ensure-parsed-base-type ctype))
         (predicate (symbolicate name '#:-p))
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
             (cobject-attach-finalizer ,instance)))
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
             (cobject-attach-finalizer
              (,inplace-copier ,instance ,destination))))
         (declaim (inline ,unmanaged-pointer-accessor))
         (defun ,unmanaged-pointer-accessor (,instance)
           (tg:cancel-finalization ,instance)
           (cobject-pointer ,instance))
         (declaim (inline ,unmanaged-constructor))
         (defun ,unmanaged-constructor (,pointer)
           (,internal-constructor :pointer ,pointer))
         (declaim (inline ,managed-constructor))
         (defun ,managed-constructor (,pointer)
           (cobject-attach-finalizer (,unmanaged-constructor ,pointer)))
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
                  :managed-constructor ',managed-constructor
                  :unmanaged-constructor ',unmanaged-constructor
                  :unmanaged-pointer-accessor ',unmanaged-pointer-accessor)))))))

(defstruct (carray (:include cobject)
                   (:constructor %make-carray))
  (dimensions '(0) :type (cons fixnum null))
  (element-type nil :type symbol))

(defun caref (array &rest subscripts &aux (subscript (first subscripts)))
  (unless (<= 0 subscript (1- (first (carray-dimensions array))))
    (error "Index ~D is out of bound." subscript))
  (multiple-value-bind (definition type)
      (cobject-class-definition (carray-element-type array))
    (funcall
     (cobject-class-definition-reference-constructor definition)
     :pointer (cffi:mem-aptr (cobject-pointer array) type subscript)
     :source array)))

(defun (setf caref) (value array &rest subscripts &aux (subscript (first subscripts)))
  (unless (<= 0 subscript (1- (first (carray-dimensions array))))
    (error "Index ~D is out of bound." subscript))
  (multiple-value-bind (definition type)
      (cobject-class-definition (carray-element-type array))
    (let* ((element-size (cffi:foreign-type-size type))
           (pointer (cffi:inc-pointer (cobject-pointer array) (* element-size subscript))))
      (memcpy pointer (cobject-pointer value) element-size)
      (funcall
       (cobject-class-definition-reference-constructor definition)
       :pointer pointer
       :source array))))

(defmethod print-object ((array carray) stream)
  (print-unreadable-object (array stream)
    (loop :with length := (first (carray-dimensions array))
          :for i :below length
          :if (< i 10)
            :unless (zerop i)
              :do (format stream " ")
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

(defun make-carray (dimensions
                    &key element-type
                      initial-element initial-contents
                      displaced-to
                      (displaced-index-offset 0))
  (unless (listp dimensions)
    (setf dimensions (list dimensions)))
  (let* ((ctype (car (find element-type *cobject-class-definitions*
                           :key (compose #'cobject-class-definition-class #'cdr))))
         (element-size (cffi:foreign-type-size ctype))
         (total-size (* element-size (reduce #'* dimensions)))
         (pointer (if displaced-to (cffi:inc-pointer (cobject-pointer displaced-to) (* element-size displaced-index-offset))
                      (cffi:foreign-alloc :uint8 :count total-size)))
         (array (if displaced-to
                    (progn
                      (assert (<= 0 displaced-index-offset (+ displaced-index-offset (first dimensions)) (first (carray-dimensions displaced-to))))
                      (assert (eq element-type (carray-element-type displaced-to)))
                      (%make-displaced-carray :pointer pointer
                                              :dimensions dimensions
                                              :element-type element-type
                                              :displaced-to displaced-to
                                              :displaced-index-offset displaced-index-offset))
                    (cobject-attach-finalizer (%make-carray :pointer pointer
                                                            :dimensions dimensions
                                                            :element-type element-type)))))
    (when initial-element
      (assert (null initial-contents))
      (assert (null displaced-to))
      (loop :with src := (cobject-pointer initial-element)
            :for i :of-type fixnum :below (first dimensions)
            :do (memcpy (cffi:inc-pointer pointer (* i element-size)) src element-size)))
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
           (map nil (lambda (object)
                      (memcpy (cffi:inc-pointer pointer (* i element-size))
                              (cobject-pointer object) element-size)
                      (incf i))
                initial-contents)))))
    array))
