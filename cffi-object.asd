(defsystem cffi-object
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :description "A Common Lisp library that enables fast and convenient interoperation with foreign objects."
  :homepage "https://github.com/bohonghuang/cffi-object"
  :bug-tracker "https://github.com/bohonghuang/cffi-object/issues"
  :source-control (:git "https://github.com/bohonghuang/cffi-object.git")
  :serial t
  :components ((:file "package")
               (:file "libc")
               (:file "type")
               (:file "definition")
               (:file "allocator")
               (:file "object")
               (:file "allocator-ecl" :if-feature :ecl)
               (:file "pointer")
               (:file "array")
               (:file "macros")
               (:file "defcfun")
               (:file "global"))
  :depends-on (#:uiop #:alexandria #:cffi #:trivial-garbage)
  :in-order-to ((test-op (test-op #:cffi-object/test))))

(defsystem cffi-object/test
  :depends-on (#:cffi-ops #:cffi-object #:cffi-object.ops #:parachute)
  :pathname "./test/"
  :components ((:file "package"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:cffi-object.test))))
