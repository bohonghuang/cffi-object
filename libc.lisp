(in-package #:cffi-object)

(declaim (inline memcpy))
(cffi:defcfun "memcpy" :void
  (dest :pointer)
  (src :pointer)
  (n :size))

(declaim (inline memcmp))
(cffi:defcfun "memcmp" :int
  (s1 :pointer)
  (s2 :pointer)
  (n :size))

(declaim (inline memset))
(cffi:defcfun "memset" :int
  (s :pointer)
  (c :int)
  (n :size))
