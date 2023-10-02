(defpackage cffi-object.ops
  (:use #:cl #:alexandria)
  (:nicknames #:cobj.ops)
  (:export #:enable-cobject-ops #:disable-cobject-ops))

(in-package #:cffi-object.ops)

(defconstant +form-type+ (fdefinition 'cffi-ops::form-type))

(defconstant +ctypes-slots+ (fdefinition 'cffi-ops::ctypes-slots))

(defconstant +pointer-type-p+ (fdefinition 'cffi-ops::pointer-type-p))

(defconstant +ensure-pointer-type+ (fdefinition 'cffi-ops::ensure-pointer-type))

(setf (fdefinition 'cobj::funcall-dynamic-extent-form) (fdefinition 'cffi-ops::funcall-dynamic-extent-form)
      (fdefinition 'cobj::funcall-form-type) (fdefinition 'cffi-ops::funcall-form-type))

(defun ctypes-slots-with-cobject (types)
  (funcall +ctypes-slots+ (mapcar (lambda (type)
                                    (if (and (listp type) (eq (car type) :object))
                                        (cons :pointer (cdr type))
                                        type))
                                  types)))

(defgeneric funcall-form-type (function args))

(defmethod funcall-form-type ((function (eql 'vector2-add)) args)
  (values '(:object (:struct abc)) (cons function args)))

(defun form-type-with-object-unwrapped (form)
  (multiple-value-bind (type form) (funcall +form-type+ form)
    (cond
      (cffi-ops::*value-required* (values type form))
      ((and (listp type) (member (car type) '(nil :object)))
       (values (cons :pointer (cdr type)) `(cobj:cobject-pointer ,form)))
      (t (values type form)))))

(defun pointer-or-object-type-p (type)
  (if (and (consp type) (eq (car type) :object)) t (funcall +pointer-type-p+ type)))

(defun ensure-pointer-or-object-type (type)
  (if (and (consp type) (eq (car type) :object)) type (funcall +ensure-pointer-type+ type)))

(defmacro & (form)
  `(cobj:cobject-pointer ,form))

(defun enable-cobject-ops ()
  (setf (fdefinition 'cffi-ops::form-type) #'form-type-with-object-unwrapped
        (fdefinition 'cffi-ops::ctypes-slots) #'ctypes-slots-with-cobject
        (fdefinition 'cffi-ops::pointer-type-p) #'pointer-or-object-type-p
        (fdefinition 'cffi-ops::ensure-pointer-type) #'ensure-pointer-or-object-type
        (fdefinition 'cffi-ops:&) #'cobj:cobject-pointer
        (compiler-macro-function 'cffi-ops:&) (macro-function '&)))

(defun disable-cobject-ops ()
  (setf (fdefinition 'cffi-ops::form-type) +form-type+
        (fdefinition 'cffi-ops::ctypes-slots) +ctypes-slots+
        (fdefinition 'cffi-ops::pointer-type-p) +pointer-type-p+
        (fdefinition 'cffi-ops::ensure-pointer-type) +ensure-pointer-type+
        (compiler-macro-function 'cffi-ops:&) nil)
  (fmakunbound 'cffi-ops:&))
