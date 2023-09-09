(in-package #:cffi-object)

(defstruct cobject-class-definition
  (class nil :type (or symbol list))
  (internal-constructor nil :type (or symbol function))
  (constructor nil :type symbol)
  (slot-accessors nil :type list)
  (copier nil :type symbol)
  (predicate nil :type symbol)
  (equality-comparator nil :type symbol))

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
                                         (code-char (mod (cffi:mem-aref pointer :char) 255)))))
         (t nil))
       primitive-type)
      (if (listp type)
          (symbol-macrolet ((as-array (values (make-cobject-class-definition
                                               :class type
                                               :internal-constructor (lambda (&key pointer shared-from)
                                                                       (%make-carray :pointer pointer
                                                                                     :shared-from shared-from
                                                                                     :element-type element-type
                                                                                     :dimensions dimensions)))
                                              `(:array ,(nth-value 1 (cobject-class-definition element-type)) . ,dimensions)))
                            (as-pointer (values (make-cobject-class-definition
                                                 :class type
                                                 :internal-constructor (lambda (&key pointer shared-from)
                                                                         (declare (ignore shared-from))
                                                                         (%make-cpointer :pointer (cffi:mem-ref pointer :pointer)
                                                                                         :element-type element-type)))
                                                `(:pointer ,(nth-value 1 (cobject-class-definition element-type))))))
            (destructuring-ecase type
              ((carray element-type &optional dimensions)
               (if dimensions
                   (if (listp dimensions)
                       (if (every #'integerp dimensions) as-array as-pointer)
                       (if (integerp dimensions) (progn (setf dimensions (list dimensions)) as-array) as-pointer))
                   as-pointer))
              ((cpointer element-type) as-pointer)))
          (error "Undefined CFFI object class ~A." type)))))

(defun find-cobject-class-definition (type)
  "Get the class definition of a cobject at compile-time."
  (check-type type cffi::foreign-type)
  (or (assoc-value *cobject-class-definitions* type)
      (and (typep type 'cffi::foreign-built-in-type)
           (make-cobject-class-definition
            :class (case type
                     (#.(cffi::ensure-parsed-base-type :float) 'single-float)
                     (#.(cffi::ensure-parsed-base-type :double) 'double-float)
                     (#.(cffi::ensure-parsed-base-type :string) 'string)
                     (#.(mapcar #'cffi::ensure-parsed-base-type '(:int8 :int16 :int32 :int64))
                      `(signed-byte ,(* (cffi:foreign-type-size type) 8)))
                     (#.(mapcar #'cffi::ensure-parsed-base-type '(:uint8 :uint16 :uint32 :uint64))
                      `(unsigned-byte ,(* (cffi:foreign-type-size type) 8)))
                     (#.(cffi::ensure-parsed-base-type :void) 'null)
                     (t (etypecase type
                          (cffi::foreign-array-type
                           `(carray ,(cobject-class-definition-class
                                      (find-cobject-class-definition (cffi::element-type type)))
                                    ,(cffi::dimensions type)))
                          (cffi::foreign-pointer-type
                           `(cpointer ,(cobject-class-definition-class
                                        (find-cobject-class-definition (cffi::pointer-type type))))))))))
      (error "Cannot find the CFFI object class for type ~A." (cffi::name type))))
