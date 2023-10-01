(in-package #:cffi-object)

(defstruct foreign-allocator
  (allocator (constantly (cffi:null-pointer)) :type (function (non-negative-fixnum) (values cffi:foreign-pointer)))
  (deallocator #'values :type (function (cffi:foreign-pointer))))

(declaim (type foreign-allocator *foreign-allocator*))
(defparameter *foreign-allocator* (make-foreign-allocator :allocator #'cffi-sys:%foreign-alloc :deallocator #'cffi-sys:foreign-free))
