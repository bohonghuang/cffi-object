(in-package #:cffi-object)

(defmacro with-new-cobject-class-definition ((name-form type-form) &body body)
  (with-gensyms (name slots type)
    `(let* ((,name ,name-form)
            (,type (cffi::ensure-parsed-base-type ,type-form))
            (,slots (cffi:foreign-slot-names ,type))
            (predicate (symbolicate ,name '#:-p))
            (equality-comparator (symbolicate ,name '#:-equal))
            (constructor (symbolicate '#:make- ,name))
            (internal-constructor (symbolicate '#:%make- ,name))
            (copier (symbolicate '#:copy- ,name))
            (inplace-copier (symbolicate '#:copy- ,name '#:-into))
            (managed-constructor (symbolicate '#:make-managed- ,name))
            (unmanaged-constructor (symbolicate '#:make-unmanaged- ,name))
            (unmanaged-pointer-accessor (symbolicate '#:unmanange- ,name))
            (slot-accessors (mapcar #'cons ,slots (mapcar (curry #'symbolicate ,name '#:-) ,slots)))
            (cobject-class-definition `(make-cobject-class-definition :class ',name
                                                                      :constructor ',constructor
                                                                      :internal-constructor ',internal-constructor
                                                                      :slot-accessors ',slot-accessors
                                                                      :copier ',copier
                                                                      :inplace-copier ',inplace-copier
                                                                      :predicate ',predicate
                                                                      :equality-comparator ',equality-comparator
                                                                      :managed-constructor ',managed-constructor
                                                                      :unmanaged-constructor ',unmanaged-constructor
                                                                      :unmanaged-pointer-accessor ',unmanaged-pointer-accessor)))
       (declare (ignorable cobject-class-definition))
       . ,body)))

(defmacro with-parsed-desc ((name type) desc-form &body body)
  (with-gensyms (desc parsed-type)
    `(let ((,desc ,desc-form))
       (destructuring-bind (,name &optional ,type) (if (and (listp ,desc) (not (keywordp (car ,desc))))
                                                       ,desc (list ,desc))
         (let* ((,parsed-type (or ,type ,name))
                (,parsed-type (typecase ,type
                                (cffi::foreign-type ,parsed-type)
                                (t (cffi::parse-type ,parsed-type)))))
           (unless ,type
             (setf ,name (cffi::name ,parsed-type)))
           (setf ,type ,parsed-type)
           (let ((*package* (symbol-package ,name)))
             ,@body))))))

(defmacro define-prototype-cobject (desc)
  (with-parsed-desc (name type) desc
    (with-new-cobject-class-definition (name type)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (assoc-value *cobject-class-definitions* ',type) ,cobject-class-definition)))))

(defmacro define-struct-cobject (desc)
  (with-parsed-desc (name ctype) desc
    (let* ((type (cffi::ensure-parsed-base-type ctype))
           (slots (cffi:foreign-slot-names type)))
      (with-new-cobject-class-definition (name ctype)
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
                         :collect `(format ,stream ," :~A ~S" ,(symbol-name slot) (,slot-accessor ,instance)))
                 (format ,stream ,(concatenate 'string " @0x~" (prin1-to-string (* 2 (cffi:foreign-type-size :size))) ",'0X")
                         (cffi:pointer-address (cobject-pointer ,instance)))))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (setf (assoc-value *cobject-class-definitions* ',type) ,cobject-class-definition))))))))

(defmacro define-type-cobject (desc)
  (with-parsed-desc (name type) desc
    (let ((base-type (cffi::ensure-parsed-base-type type)))
      (with-new-cobject-class-definition (name type)
        (if-let ((definition (find-cobject-class-definition base-type)))
          `(progn
             (setf (find-class ',name) (find-class ',(cobject-class-definition-class definition))
                   (fdefinition ',predicate) (fdefinition ',(cobject-class-definition-predicate definition))
                   (fdefinition ',equality-comparator) (fdefinition ',(cobject-class-definition-equality-comparator definition))
                   (fdefinition ',constructor) (fdefinition ',(cobject-class-definition-constructor definition))
                   (fdefinition ',internal-constructor) (fdefinition ',(cobject-class-definition-internal-constructor definition))
                   (fdefinition ',copier) (fdefinition ',(cobject-class-definition-copier definition))
                   (fdefinition ',inplace-copier) (fdefinition ',(cobject-class-definition-inplace-copier definition))
                   (fdefinition ',managed-constructor) (fdefinition ',(cobject-class-definition-managed-constructor definition))
                   (fdefinition ',unmanaged-constructor) (fdefinition ',(cobject-class-definition-unmanaged-constructor definition))
                   (fdefinition ',unmanaged-pointer-accessor) (fdefinition ',(cobject-class-definition-unmanaged-pointer-accessor definition))
                   . ,(mapcan (lambda (var val) `((fdefinition ',(cdr var)) (fdefinition ',(cdr val)))) slot-accessors (cobject-class-definition-slot-accessors definition)))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (setf (assoc-value *cobject-class-definitions* ',type) ,cobject-class-definition)))
          (error "Definition for the base type of ~A is not found." type))))))

(defmacro define-package-cobject (desc)
  (unless (listp desc)
    (setf desc (list desc)))
  (destructuring-bind (target &optional source) desc
    (unless source
      (shiftf source target *package*))
    (loop :with source-package := (find-package source) :and target-package := (find-package target)
          :with definitions :and type-set := (make-hash-table)
          :for (source-name . type-getter) :in (nconc (hash-table-alist cffi::*default-type-parsers*)
                                                      (hash-table-alist cffi::*struct-type-parsers*))
          :when (eql (symbol-package source-name) source-package)
            :do (symbol-macrolet ((name (intern (symbol-name (cffi::name type)) target-package)))
                  (labels ((push-definition (type)
                             (case (gethash type type-set)
                               (push
                                (format t "Note: Found circular type reference, generating forward declaration for ~A.~%" name)
                                (push `(define-prototype-cobject (,name ,type)) definitions))
                               ((nil)
                                (setf (gethash type type-set) 'push)
                                (prog1 (typecase type
                                         (cffi::foreign-type-alias
                                          (push-definition (cffi::actual-type type))
                                          (when (typep (cffi::actual-type type) 'cffi::foreign-struct-type)
                                            (push `(define-type-cobject (,name ,type)) definitions)))
                                         (cffi::foreign-pointer-type
                                          (push-definition (cffi::pointer-type type)))
                                         (cffi::foreign-struct-type
                                          (mapc (compose #'push-definition #'cffi::parse-type #'cffi::slot-type)
                                                (hash-table-values (cffi::slots type)))
                                          (push `(define-struct-cobject (,name ,type)) definitions)))
                                  (setf (gethash type type-set) t))))))
                    (ignore-some-conditions (warning) (push-definition (funcall type-getter)))))
          :finally (return `(progn . ,(nreverse definitions))))))
