(in-package #:cffi-object)

(defmacro with-new-cobject-class-definition ((name-form type-form) &body body)
  (with-gensyms (name slots type)
    `(let* ((,name ,name-form)
            (,type (cffi::ensure-parsed-base-type ,type-form))
            (,slots (cffi:foreign-slot-names ,type))
            (predicate (symbolicate ,name '#:-p))
            (equality-comparator (symbolicate ,name '#:-equal))
            (constructor (symbolicate '#:make- ,name))
            (internal-constructor (symbolicate '#:%%%make- ,name))
            (in-place-constructor (symbolicate '#:%%make- ,name))
            (copier (symbolicate '#:copy- ,name))
            (slot-accessors (mapcar #'cons ,slots (mapcar (curry #'symbolicate ,name '#:-) ,slots)))
            (cobject-class-definition `(make-cobject-class-definition :class ',name
                                                                      :constructor ',constructor
                                                                      :internal-constructor ',internal-constructor
                                                                      :in-place-constructor ',in-place-constructor
                                                                      :slot-accessors ',slot-accessors
                                                                      :copier ',copier
                                                                      :predicate ',predicate
                                                                      :equality-comparator ',equality-comparator)))
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

(defmacro define-prototype-cobject-class (desc)
  (with-parsed-desc (name type) desc
    `(progn
       ,(with-new-cobject-class-definition (name type)
          `(eval-when (:compile-toplevel :load-toplevel :execute)
             (setf (assoc-value *cobject-class-definitions* ',type) ,cobject-class-definition)))
       ,(let ((base-type (cffi::ensure-parsed-base-type type)))
          (unless (or (eq type base-type) (assoc-value *cobject-class-definitions* base-type))
            (with-new-cobject-class-definition (name base-type)
              `(eval-when (:compile-toplevel :load-toplevel :execute)
                 (setf (assoc-value *cobject-class-definitions* ',base-type) ,cobject-class-definition))))))))

(defmacro define-struct-cobject-class (desc &rest options)
  (let ((options (reduce #'append options)))
    (with-parsed-desc (name ctype) desc
      (let* ((type (cffi::ensure-parsed-base-type ctype))
             (slots (cffi:foreign-slot-names type))
             (slot-supplied-p-list (mapcar (compose #'gensym #'symbol-name) slots)))
        (with-new-cobject-class-definition (name ctype)
          (when-let ((constructor-option (getf options :constructor)))
            (setf (getf (cdr cobject-class-definition) :constructor) `',(setf constructor constructor-option)))
          (with-gensyms (pointer instance value stream destination)
            (let ((*cobject-class-definitions* (acons type (eval cobject-class-definition) *cobject-class-definitions*)))
              `(progn
                 (declaim (inline ,internal-constructor))
                 (defstruct (,name
                             (:include cobject)
                             (:predicate ,predicate)
                             (:copier nil)
                             (:constructor ,internal-constructor)))
                 ,@(loop :with slots := (cffi::slots type)
                         :for (slot-name . slot-accessor) :in slot-accessors
                         :for slot := (gethash slot-name slots)
                         :for slot-type := (cffi::ensure-parsed-base-type (cffi:foreign-slot-type type slot-name))
                         :for slot-pointer := `(cffi:foreign-slot-pointer (cobject-pointer ,instance) ',type ',slot-name)
                         :for slot-value := `(cffi:foreign-slot-value (cobject-pointer ,instance) ',type ',slot-name)
                         :nconc `((declaim (inline ,slot-accessor))
                                  (defun ,slot-accessor (,instance)
                                    ,(flet ((access-simple-slot ()
                                              (typecase slot-type
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
                                                                      (cffi-element-type slot-type))))))
                                                (cffi::foreign-pointer-type
                                                 `(%make-cpointer
                                                   :pointer ,slot-value
                                                   :element-type ',(cobject-class-definition-class
                                                                    (find-cobject-class-definition
                                                                     (cffi::ensure-parsed-base-type
                                                                      (cffi-pointer-type slot-type))))))
                                                (t slot-value))))
                                       (etypecase slot
                                         (cffi::aggregate-struct-slot
                                          (case (cffi::slot-count slot)
                                            (0 `(%make-cpointer
                                                 :pointer ,slot-pointer
                                                 :shared-from ,instance
                                                 :element-type ',(cobject-class-definition-class (find-cobject-class-definition slot-type))))
                                            (1 (access-simple-slot))
                                            (t `(%make-carray
                                                 :pointer ,slot-pointer
                                                 :shared-from ,instance
                                                 :dimensions '(,(cffi::slot-count slot))
                                                 :element-type ',(cobject-class-definition-class (find-cobject-class-definition slot-type))))))
                                         (cffi::simple-struct-slot (access-simple-slot))))))
                         :nconc `((declaim (inline (setf ,slot-accessor)))
                                  (defun (setf ,slot-accessor) (,value ,instance)
                                    ,(flet ((access-simple-slot ()
                                              (typecase slot-type
                                                (cffi::foreign-struct-type
                                                 `(memcpy ,slot-pointer (cobject-pointer ,value) (cffi:foreign-type-size ',slot-type)))
                                                (cffi::foreign-array-type
                                                 `(creplace (,slot-accessor ,instance) ,value))
                                                (cffi::foreign-pointer-type
                                                 `(setf ,slot-value (cobject-pointer ,value)))
                                                (t `(setf ,slot-value ,value)))))
                                       (etypecase slot
                                         (cffi::aggregate-struct-slot
                                          (case (cffi::slot-count slot)
                                            (1 (access-simple-slot))
                                            (t `(creplace (,slot-accessor ,instance) ,value))))
                                         (cffi::simple-struct-slot
                                          (access-simple-slot)))))))
                 (declaim (inline ,in-place-constructor))
                 (defun ,in-place-constructor (,pointer &key . ,(mapcar #'list slots (mapcar (constantly nil) slots) slot-supplied-p-list))
                   (let ((,instance (,internal-constructor :pointer ,pointer)))
                     (declare (ignorable ,instance) (dynamic-extent ,instance))
                     ,@(loop :for slot :in slots
                             :for slot-type := (cffi::ensure-parsed-base-type (cffi:foreign-slot-type type slot))
                             :for slot-supplied-p :in slot-supplied-p-list
                             :if (typep slot-type '(or cffi::foreign-struct-type cffi::foreign-array-type cffi::foreign-pointer-type))
                               :collect `(when ,slot-supplied-p
                                           (let ((,value (make-cobject :pointer ,slot)))
                                             (declare (dynamic-extent ,value))
                                             (setf (,(assoc-value slot-accessors slot) ,instance) ,value)))
                             :else
                               :collect `(when ,slot-supplied-p
                                           (setf (,(assoc-value slot-accessors slot) ,instance) ,slot)))
                     ,pointer))
                 (declaim (inline ,constructor))
                 (defun ,constructor (&key . ,(mapcar #'list slots (mapcar (constantly nil) slots) slot-supplied-p-list))
                   (let* ((,pointer (funcall (cobject-allocator-allocator *cobject-allocator*) ',type))
                          (,instance (,internal-constructor :pointer ,pointer)))
                     ,@(loop :for slot :in slots
                             :for slot-type := (cffi::ensure-parsed-base-type (cffi:foreign-slot-type type slot))
                             :for slot-supplied-p :in slot-supplied-p-list
                             :collect `(when ,slot-supplied-p
                                         (setf (,(assoc-value slot-accessors slot) ,instance) ,slot)))
                     (manage-cobject ,instance)))
                 (declaim (inline ,equality-comparator))
                 ,(with-gensyms (instance1 instance2)
                    `(defun ,equality-comparator (,instance1 ,instance2)
                       (zerop (memcmp (cobject-pointer ,instance1)
                                      (cobject-pointer ,instance2)
                                      (cffi:foreign-type-size ',type)))))
                 (declaim (inline ,copier))
                 (defun ,copier (,instance &optional (,destination (manage-cobject (,internal-constructor :pointer (funcall (cobject-allocator-allocator *cobject-allocator*) ',type)))))
                   (check-type ,instance ,name)
                   (check-type ,destination ,name)
                   (memcpy (cobject-pointer ,destination) (cobject-pointer ,instance) (cffi:foreign-type-size ',type))
                   ,destination)
                 ,(with-gensyms (print-slots)
                    `(defmethod print-object ((,instance ,name) ,stream)
                       (flet ((,print-slots ()
                                ,@(loop :for (slot . slot-accessor) :in slot-accessors
                                        :collect `(format ,stream ," :~A ~S" ',slot (,slot-accessor ,instance)))))
                         (if *print-readably*
                             (progn
                               (format ,stream "#.(~S" ',constructor)
                               (,print-slots)
                               (format ,stream ")"))
                             (print-unreadable-object (,instance ,stream)
                               (format ,stream "~S" ',name)
                               (,print-slots)
                               (format ,stream ,(concatenate 'string " @0x~" (prin1-to-string (* 2 (cffi:foreign-type-size :size))) ",'0X")
                                       (cffi:pointer-address (cobject-pointer ,instance))))))))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (setf (assoc-value *cobject-class-definitions* ',type) ,cobject-class-definition))))))))))

(defmacro define-type-cobject-class (desc)
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
                   (fdefinition ',in-place-constructor) (fdefinition ',(cobject-class-definition-in-place-constructor definition))
                   (fdefinition ',copier) (fdefinition ',(cobject-class-definition-copier definition))
                   . ,(mapcan
                       (lambda (var val)
                         `((fdefinition ',(cdr var)) (fdefinition ',(cdr val))
                           (fdefinition '(setf ,(cdr var))) (fdefinition '(setf ,(cdr val)))))
                       slot-accessors (cobject-class-definition-slot-accessors definition)))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (setf (assoc-value *cobject-class-definitions* ',type) ,cobject-class-definition)))
          (error "Definition for the base type of ~A is not found." type))))))

(defmacro define-package-cobject-classes (desc)
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
                                (push `(define-prototype-cobject-class (,name ,type)) definitions))
                               ((nil)
                                (setf (gethash type type-set) 'push)
                                (prog1 (typecase type
                                         (cffi::foreign-type-alias
                                          (push-definition (cffi::actual-type type))
                                          (when (typep (cffi::actual-type type) 'cffi::foreign-struct-type)
                                            (push `(define-type-cobject-class (,name ,type)) definitions)))
                                         (cffi::foreign-pointer-type
                                          (push-definition (cffi-pointer-type type)))
                                         (cffi::foreign-struct-type
                                          (mapc (compose #'push-definition #'cffi::parse-type #'cffi::slot-type)
                                                (hash-table-values (cffi::slots type)))
                                          (push `(define-struct-cobject-class (,name ,type)) definitions)))
                                  (setf (gethash type type-set) t))))))
                    (ignore-some-conditions (warning) (push-definition (funcall type-getter)))))
          :finally (return `(progn . ,(nreverse definitions))))))

(defmacro define-cobject-class (desc &rest options)
  (if (and (or (keywordp desc) (and (symbolp desc) (not (symbol-package desc)))) (find-package desc))
      `(define-package-cobject-classes ,desc . ,options)
      `(define-struct-cobject-class ,desc . ,options)))
