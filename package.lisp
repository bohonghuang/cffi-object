(defpackage cffi-object
  (:use #:cl #:alexandria)
  (:nicknames #:cobj))

(in-package #:cffi-object)

(defstruct cobject
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer))
