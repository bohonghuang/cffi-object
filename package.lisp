(defpackage cffi-object
  (:use #:cl #:alexandria)
  (:nicknames #:cobj)
  (:export #:cobject
           #:cobject-eq
           #:cobject-pointer
           #:cpointer
           #:make-unmanaged-cpointer
           #:make-managed-cpointer
           #:unmanage-cpointer
           #:cref
           #:cpointer-equal
           #:cpointer-eq
           #:carray
           #:make-carray
           #:make-unmanaged-carray
           #:make-managed-carray
           #:unmanage-carray
           #:carray-dimensions
           #:carray-displacement
           #:carray-element-type
           #:carray-list
           #:carray-array
           #:caref
           #:clength
           #:creplace
           #:cfill
           #:carray-equal
           #:define-struct-cobject
           #:define-type-cobject
           #:define-prototype-cobject
           #:define-package-cobject))

(in-package #:cffi-object)
