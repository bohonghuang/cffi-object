(in-package #:cffi-object)

(defun cffi-pointer-type-p (type)
  (and (typep (setf type (cffi::ensure-parsed-base-type type)) 'cffi::foreign-pointer-type) type))

(defun cffi-object-type-p (type)
  (when-let ((type (cffi-pointer-type-p type)))
    (and (typep (setf type (cffi::ensure-parsed-base-type (cffi::pointer-type type))) 'cffi::foreign-struct-type) type)))

(defparameter *return-argument-names* '(#:%%claw-result-))

(defun symbol-name= (sym1 sym2)
  (string= (symbol-name sym1) (symbol-name sym2)))

(defconstant +defcfun+ (macro-function 'cffi:defcfun))

(setf (macro-function 'defcfun) +defcfun+)

(defmacro defcobjfun (name result &rest args)
  (destructuring-bind (name symbol) name
    (let* ((should-define-wrapper-p (not (member '&rest args)))
           (internal-symbol (if should-define-wrapper-p (intern (format nil "%~A" symbol) (symbol-package symbol)) symbol))
           (return-object-p (member (caar args) *return-argument-names* :test #'symbol-name=)))
      `(progn
         (declaim (inline ,internal-symbol))
         (defcfun (,name ,internal-symbol) ,result . ,args)
         (export ',internal-symbol ',(symbol-package internal-symbol))
         ,(when should-define-wrapper-p
            (if return-object-p
                (let* ((object-type (cffi-object-type-p (cadar args)))
                       (object-constructor
                         (if-let ((definition (when object-type (assoc-value *cobject-class-definitions* object-type))))
                           (cobject-class-definition-constructor definition)
                           (if-let ((type-name (when object-type (cffi::name object-type))))
                             (intern (format nil "~A~A" '#:make- type-name) (symbol-package type-name))
                             (error "Defining a C function that returns non-structure pointer is currently not supported.")))))
                  `(progn
                     (declaim (ftype function ,object-constructor)
                              (notinline ,object-constructor))
                     (defun ,symbol ,(mapcar #'car (cdr args))
                       (let ((,(caar args) (,object-constructor)))
                         (progn
                           (,internal-symbol . ,(loop :for (name type) :in args :collect (if (cffi-pointer-type-p type) `(cobj:cobject-pointer ,name) name)))
                           ,(caar args))))))
                `(progn
                   (defun ,symbol ,(mapcar #'car args)
                     (,internal-symbol . ,(loop :for (name type) :in args :collect (if (cffi-pointer-type-p type) `(cobj:cobject-pointer ,name) name)))))))))))
