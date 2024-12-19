(in-package #:cffi-object)

(defun cffi-pointer-type-p (type)
  (and (typep (setf type (cffi::ensure-parsed-base-type type)) 'cffi::foreign-pointer-type) type))

(defun cffi-object-type-p (type)
  (when-let ((type (cffi-pointer-type-p type)))
    (and (typep (setf type (cffi::ensure-parsed-base-type (cffi-pointer-type type))) 'cffi::foreign-struct-type) type)))

(defparameter *return-argument-names* '(#:%%claw-result-))

(defparameter *return-pointer-as-object-p* nil)

(defparameter *optimize-object-allocation-p* t)

(defparameter *optimize-out-temporary-object-p* t)

(defun symbol-name= (sym1 sym2)
  (string= (symbol-name sym1) (symbol-name sym2)))

(defgeneric funcall-dynamic-extent-form (function args))

(defgeneric funcall-form-type (function args))

(defconstant +defcfun+ (macro-function 'cffi:defcfun))

(setf (macro-function 'defcfun) +defcfun+)

(defun cobject-type-constructor (object-type)
  (if-let ((definition (when object-type (assoc-value *cobject-class-definitions* object-type))))
    (values (cobject-class-definition-constructor definition)
            (cobject-class-definition-internal-constructor definition)
            (cobject-class-definition-copier definition))
    (if-let ((type-name (when object-type (cffi::name object-type))))
      (values (intern (format nil "~A~A" '#:make- type-name) (symbol-package type-name))
              (intern (format nil "~A~A" '#:copy- type-name) (symbol-package type-name))
              (intern (format nil "~A~A" '#:%%%make- type-name) (symbol-package type-name)))
      (error "Defining a C function that returns non-structure pointer is currently not supported."))))

(defun frob-return-pointer-from-result (return-pointer-from-result-p result)
  (labels ((resolve (return-pointer-from-result-p)
             (if (not (cffi-pointer-type-p (cffi-pointer-type return-pointer-from-result-p)))
                 (or (ignore-some-conditions (cobject-class-definition-not-found-error)
                       (cobject-class-definition-class
                        (find-cobject-class-definition
                         (cffi-pointer-type return-pointer-from-result-p))))
                     (cffi::name (cffi-pointer-type return-pointer-from-result-p)))
                 `(:pointer ,(resolve (cffi-pointer-type return-pointer-from-result-p))))))
    `(pointer-cpointer ,result
                       ',(resolve
                          return-pointer-from-result-p))))

(defmacro defcobjfun (name result &rest args)
  (destructuring-bind (name symbol) name
    (let* ((should-define-wrapper-p (not (member '&rest args)))
           (internal-symbol (if should-define-wrapper-p (intern (format nil "%~A" symbol) (symbol-package symbol)) symbol))
           (return-pointer-from-result-p (cffi-pointer-type-p result))
           (return-object-from-result-p (cffi-object-type-p result))
           (return-object-from-argument-p (member (caar args) *return-argument-names* :test #'symbol-name=)))
      `(progn
         (declaim (inline ,internal-symbol))
         (defcfun (,name ,internal-symbol) ,result . ,args)
         (export ',internal-symbol ',(symbol-package internal-symbol))
         ,(when should-define-wrapper-p
            (if return-object-from-argument-p
                (let ((object-type (cffi-object-type-p (cadar args))))
                  (multiple-value-bind (object-constructor object-copier object-internal-constructor) (cobject-type-constructor object-type)
                    `(progn
                       (declaim (ftype function ,object-constructor)
                                (notinline ,object-constructor))
                       (defun ,symbol ,(mapcar #'car (cdr args))
                         (let ((,(caar args) (,object-constructor)))
                           (progn
                             (,internal-symbol . ,(loop :for (name type) :in args :collect (if (cffi-pointer-type-p type) `(cobj:cobject-pointer ,name) name)))
                             ,(caar args))))
                       ,(with-gensyms (function function-args body dynamic-extent-forms dynamic-extent-form temp-vars form name result)
                          `(progn
                             (defmethod funcall-form-type ((,function (eql ',symbol)) ,function-args)
                               (declare (ignore ,function))
                               `(:object ,(cffi::unparse-type ',object-type)))
                             (defmethod funcall-dynamic-extent-form ((,function (eql ',symbol)) ,function-args)
                               (declare (ignore ,function))
                               (destructuring-bind ,(mapcar #'car (cdr args)) ,function-args
                                 (let ((,temp-vars (list . ,(loop :for (name nil) :in args :collect `(cons ',name (gensym ,(symbol-name name)))))))
                                   (declare (ignorable ,temp-vars))
                                   (let ((,dynamic-extent-forms nil))
                                     ,@(loop :for (name type) :in (cdr args)
                                             :if (cffi-pointer-type-p type)
                                               :collect `(if-let ((,dynamic-extent-form (when (consp ,name) (funcall-dynamic-extent-form (car ,name) (cdr ,name)))))
                                                           (push (cons ',name (compose (curry ,dynamic-extent-form (assoc-value ,temp-vars ',name)) #'list)) ,dynamic-extent-forms)
                                                           (push (cons nil (compose (lambda (,body) `(let ((,(assoc-value ,temp-vars ',name) ,,name)) . ,,body)) #'list)) ,dynamic-extent-forms))
                                             :else
                                               :collect `(push (cons nil (compose (lambda (,body) `(let ((,(assoc-value ,temp-vars ',name) ,,name)) . ,,body)) #'list)) ,dynamic-extent-forms))
                                     (nreversef ,dynamic-extent-forms)
                                     (lambda (,(caar args) ,body)
                                       `(cffi:with-foreign-object (,,(caar args) ',',(cffi-pointer-type (cffi::ensure-parsed-base-type (cadar args))))
                                          ,(reduce #'funcall ,(if *optimize-out-temporary-object-p*
                                                                  `(loop :for (,name . ,form) :in ,dynamic-extent-forms
                                                                         :if ,name
                                                                           :collect (let ((,form ,form))
                                                                                      (compose
                                                                                       (lambda (,body)
                                                                                         (let ((,result (funcall ,form ,body)))
                                                                                           `(,@(subseq ,result 0 3) ,@,body)))
                                                                                       #'list))
                                                                         :else
                                                                           :collect ,form)
                                                                  `(mapcar #'cdr ,dynamic-extent-forms))
                                                   :initial-value (list ',internal-symbol ,(caar args)
                                                                        . ,(loop :for (name type) :in (cdr args)
                                                                                 :collect (if (cffi-pointer-type-p type)
                                                                                              (if *optimize-out-temporary-object-p*
                                                                                                  `(if (assoc-value ,dynamic-extent-forms ',name)
                                                                                                       (assoc-value ,temp-vars ',name)
                                                                                                       `(cobj:cobject-pointer ,(assoc-value ,temp-vars ',name)))
                                                                                                  ``(cobj:cobject-pointer ,(assoc-value ,temp-vars ',name)))
                                                                                              `(assoc-value ,temp-vars ',name))))
                                                   :from-end t)
                                          (let ((,,(caar args) (,',object-internal-constructor :pointer ,,(caar args))))
                                            (declare (dynamic-extent ,,(caar args)))
                                            ,@,body)))))))))
                       ,(when *optimize-object-allocation-p*
                          (let ((args (cdr args)))
                            (with-gensyms (var)
                              `(define-compiler-macro ,symbol ,(mapcar #'car args)
                                 (with-gensyms (,var)
                                   (funcall (funcall-dynamic-extent-form ',symbol (list . ,(mapcar #'car args))) ,var `((,',object-copier ,,var)))))))))))
                (let ((result-wrapper
                        (with-gensyms (result)
                          `(lambda (,result)
                             ,(cond
                                ((and return-object-from-result-p *return-pointer-as-object-p*)
                                 (let ((internal-constructor (nth-value 2 (cobject-type-constructor return-object-from-result-p))))
                                   `(locally (declare (notinline ,internal-constructor)) (,internal-constructor :pointer ,result))))
                                (return-pointer-from-result-p
                                 (frob-return-pointer-from-result return-pointer-from-result-p result))
                                (t result))))))
                  `(progn
                     (defun ,symbol ,(mapcar #'car args)
                       (,result-wrapper (,internal-symbol . ,(loop :for (name type) :in args :collect (if (cffi-pointer-type-p type) `(cobj:cobject-pointer ,name) name)))))
                     ,(when (and *optimize-object-allocation-p* (loop :for (nil type) :in args :thereis (cffi-pointer-type-p type)))
                        `(define-compiler-macro ,symbol ,(mapcar #'car args)
                           ,(with-gensyms (dynamic-extent-forms dynamic-extent-form body temp-vars name form result)
                              `(let ((,temp-vars (list . ,(loop :for (name nil) :in args :collect `(cons ',name (gensym ,(symbol-name name))))))
                                     (,dynamic-extent-forms nil))
                                 ,@(loop :for (name type) :in args
                                         :if (cffi-pointer-type-p type)
                                           :collect `(if-let ((,dynamic-extent-form (when (consp ,name) (funcall-dynamic-extent-form (car ,name) (cdr ,name)))))
                                                       (push (cons ',name (compose (curry ,dynamic-extent-form (assoc-value ,temp-vars ',name)) #'list)) ,dynamic-extent-forms)
                                                       (push (cons nil (compose (lambda (,body) `(let ((,(assoc-value ,temp-vars ',name) ,,name)) . ,,body)) #'list)) ,dynamic-extent-forms))
                                         :else
                                           :collect `(push (cons nil (compose (lambda (,body) `(let ((,(assoc-value ,temp-vars ',name) ,,name)) . ,,body)) #'list)) ,dynamic-extent-forms))
                                 (nreversef ,dynamic-extent-forms)
                                 (reduce #'funcall ,(if *optimize-out-temporary-object-p*
                                                        `(loop :for (,name . ,form) :in ,dynamic-extent-forms
                                                               :if ,name
                                                                 :collect (let ((,form ,form))
                                                                            (compose
                                                                             (lambda (,body)
                                                                               (let ((,result (funcall ,form ,body)))
                                                                                 `(,@(subseq ,result 0 3) ,@,body)))
                                                                             #'list))
                                                               :else
                                                                 :collect ,form)
                                                        `(mapcar #'cdr ,dynamic-extent-forms))
                                         :initial-value (list ',result-wrapper
                                                              (list ',internal-symbol
                                                                    . ,(loop :for (name type) :in args
                                                                             :collect (if (cffi-pointer-type-p type)
                                                                                          (if *optimize-out-temporary-object-p*
                                                                                              `(if (assoc-value ,dynamic-extent-forms ',name)
                                                                                                   (assoc-value ,temp-vars ',name)
                                                                                                   `(cobj:cobject-pointer ,(assoc-value ,temp-vars ',name)))
                                                                                              ``(cobj:cobject-pointer ,(assoc-value ,temp-vars ',name)))
                                                                                          `(assoc-value ,temp-vars ',name)))))
                                         :from-end t)))))))))))))
