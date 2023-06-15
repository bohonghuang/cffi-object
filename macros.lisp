(in-package #:cffi-object)

(defmacro define-struct-cobject ((name ctype) &aux (*package* (symbol-package name)))
  (let* ((type (cffi::ensure-parsed-base-type ctype))
         (predicate (symbolicate name '#:-p))
         (equality-comparator (symbolicate name '#:-equal))
         (constructor (symbolicate '#:make- name))
         (internal-constructor (symbolicate '#:%make- name))
         (copier (symbolicate '#:copy- name))
         (inplace-copier (symbolicate '#:copy- name '#:-into))
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
         ,@(loop :for (slot . slot-accessor) :in slot-accessors
                 :for slot-type := (cffi::ensure-parsed-base-type (cffi:foreign-slot-type type slot))
                 :for slot-pointer := `(cffi:foreign-slot-pointer (cobject-pointer ,instance) ',type ',slot)
                 :for slot-value := `(cffi:foreign-slot-value (cobject-pointer ,instance) ',type ',slot)
                 :nconc `((declaim (inline ,slot-accessor))
                          (defun ,slot-accessor (,instance)
                            ,(typecase slot-type
                               (cffi::foreign-struct-type
                                `(,(cobject-class-definition-internal-constructor
                                    (find-cobject-class-definition slot-type))
                                  :pointer ,slot-pointer
                                  :shared-from ,instance))
                               (cffi::foreign-array-type
                                `(%make-carray
                                  :pointer ,slot-pointer
                                  :shared-from ,instance
                                  :dimensions ',(cffi::dimensions slot-type)
                                  :element-type ',(cobject-class-definition-class
                                                   (find-cobject-class-definition
                                                    (cffi::ensure-parsed-base-type
                                                     (cffi::element-type slot-type))))))
                               (cffi::foreign-pointer-type
                                `(%make-cpointer
                                  :pointer ,slot-value
                                  :shared-from ,instance
                                  :element-type ',(cobject-class-definition-class
                                                   (find-cobject-class-definition
                                                    (cffi::ensure-parsed-base-type
                                                     (cffi::pointer-type slot-type))))))
                               (t slot-value)))
                          (declaim (inline (setf ,slot-accessor)))
                          (defun (setf ,slot-accessor) (,value ,instance)
                            ,(typecase slot-type
                               (cffi::foreign-struct-type
                                `(memcpy ,slot-pointer (cobject-pointer ,value) (cffi:foreign-type-size ',slot-type)))
                               (cffi::foreign-array-type
                                `(creplace (,slot-accessor ,instance) ,value))
                               (cffi::foreign-pointer-type
                                `(setf ,slot-value (cobject-pointer ,value)))
                               (t `(setf ,slot-value ,value))))))
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
                  :slot-accessors ',slot-accessors
                  :copier ',copier
                  :inplace-copier ',inplace-copier
                  :predicate ',predicate
                  :equality-comparator ',equality-comparator
                  :managed-constructor ',managed-constructor
                  :unmanaged-constructor ',unmanaged-constructor
                  :unmanaged-pointer-accessor ',unmanaged-pointer-accessor)))))))
