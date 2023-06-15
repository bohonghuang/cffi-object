(defpackage cffi-object
  (:use #:cl #:alexandria)
  (:nicknames #:cobj)
  (:export #:cobject
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
           #:caref
           #:clength
           #:creplace
           #:cfill
           #:carray-equal
           #:define-struct-cobject))

(in-package #:cffi-object)
