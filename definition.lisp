(in-package #:cffi-object)

(defstruct cobject-class-definition
  (class nil :type (or symbol list))
  (internal-constructor nil :type (or symbol function))
  (constructor nil :type (or symbol function))
  (in-place-constructor nil :type (or symbol function))
  (slot-accessors nil :type list)
  (copier nil :type (or symbol function))
  (predicate nil :type (or symbol function))
  (equality-comparator nil :type (or symbol function)))

(defun cobject-class-definition-symbols (definition &optional internalp)
  (remove-if-not
   (conjoin #'symbolp #'identity)
   (nconc
    (list
     (cobject-class-definition-class definition)
     (cobject-class-definition-constructor definition)
     (cobject-class-definition-copier definition)
     (cobject-class-definition-predicate definition)
     (cobject-class-definition-equality-comparator definition))
    (mapcar #'cdr (cobject-class-definition-slot-accessors definition))
    (when internalp (list (cobject-class-definition-internal-constructor definition))))))

(declaim (type list *cobject-class-definitions*))
(defvar *cobject-class-definitions* nil)

(declaim (ftype function %make-carray %make-cpointer))
(defun cobject-class-definition (type)
  "Get the class definition of a cobject at runtime."
  (if-let ((cons (find type *cobject-class-definitions* :key (compose #'cobject-class-definition-class #'cdr))))
    (values (cdr cons) (car cons))
    (if-let ((primitive-type (primitive-type-p type)))
      (values
       (case primitive-type
         (:char (make-cobject-class-definition
                 :class type
                 :internal-constructor (lambda (&key pointer shared-from)
                                         (declare (ignore shared-from))
                                         (values (code-char (mod (cffi:mem-aref pointer :char) 255)) pointer))
                 :copier (lambda (src dest &optional pointer)
                           (declare (ignore dest))
                           (when pointer (setf (cffi:mem-aref pointer :char) (char-code src)))
                           (values src))))
         (t nil))
       primitive-type)
      (if (listp type)
          (symbol-macrolet ((as-array (let ((ctype (make-instance
                                                    'cffi::foreign-array-type
                                                    :element-type (nth-value 1 (cobject-class-definition element-type))
                                                    :dimensions dimensions))
                                            (internal-constructor (lambda (&key pointer shared-from)
                                                                    (%make-carray :pointer pointer
                                                                                  :shared-from shared-from
                                                                                  :element-type element-type
                                                                                  :dimensions dimensions))))
                                        (values (make-cobject-class-definition
                                                 :class type
                                                 :internal-constructor internal-constructor
                                                 :constructor (lambda () (manage-cobject (funcall internal-constructor :pointer (cffi:foreign-alloc ctype)))))
                                                ctype)))
                            (as-pointer (values (make-cobject-class-definition
                                                 :class type
                                                 :internal-constructor (lambda (&key pointer shared-from)
                                                                         (declare (ignore shared-from))
                                                                         (%make-cpointer :pointer (cffi:mem-ref pointer :pointer)
                                                                                         :element-type element-type)))
                                                (make-instance 'cffi::foreign-pointer-type :pointer-type (nth-value 1 (cobject-class-definition element-type))))))
            (destructuring-ecase type
              ((carray element-type &optional dimensions)
               (if dimensions
                   (if (listp dimensions)
                       (if (every #'integerp dimensions) as-array as-pointer)
                       (if (integerp dimensions) (progn (setf dimensions (list dimensions)) as-array) as-pointer))
                   as-pointer))
              ((cpointer element-type) as-pointer)))
          (error "Undefined CFFI object class ~A." type)))))

(define-condition cobject-class-definition-not-found-error (error)
  ((type :initform nil :initarg :type :type cffi::foreign-type))
  (:report (lambda (condition stream)
             (format stream "Cannot find the CFFI object class for type ~A." (cffi::name (slot-value condition 'type))))))

(defun find-cobject-class-definition (type)
  "Get the class definition of a cobject at compile-time."
  (check-type type cffi::foreign-type)
  (or (assoc-value *cobject-class-definitions* type)
      (make-cobject-class-definition
       :class (case type
                (#.(cffi::ensure-parsed-base-type :float) 'single-float)
                (#.(cffi::ensure-parsed-base-type :double) 'double-float)
                (#.(mapcar #'cffi::ensure-parsed-base-type '(:int8 :int16 :int32 :int64))
                 `(signed-byte ,(* (cffi:foreign-type-size type) 8)))
                (#.(mapcar #'cffi::ensure-parsed-base-type '(:uint8 :uint16 :uint32 :uint64))
                 `(unsigned-byte ,(* (cffi:foreign-type-size type) 8)))
                (#.(cffi::ensure-parsed-base-type :void) 'null)
                (t (typecase type
                     (cffi::foreign-string-type 'string)
                     (cffi::foreign-array-type
                      `(carray ,(cobject-class-definition-class
                                 (find-cobject-class-definition (cffi::ensure-parsed-base-type (cffi-element-type type))))
                               ,(cffi::dimensions type)))
                     (cffi::foreign-pointer-type
                      `(cpointer ,(cobject-class-definition-class
                                   (find-cobject-class-definition (cffi::ensure-parsed-base-type (cffi-pointer-type type))))))
                     (t (error 'cobject-class-definition-not-found-error :type type))))))))
