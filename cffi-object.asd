(defsystem cffi-object
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :description "A Common Lisp library that enables convenient and fast interoperation with CFFI objects as if they were native Lisp objects."
  :homepage "https://github.com/bohonghuang/cffi-object"
  :bug-tracker "https://github.com/bohonghuang/cffi-object/issues"
  :source-control (:git "https://github.com/bohonghuang/cffi-object.git")
  :components ((:file "package"))
  :depends-on (#:alexandria #:cffi #:trivial-garbage)
  :in-order-to ((test-op (test-op #:cffi-object/test))))

(defsystem cffi-object/test
  :depends-on (#:cffi-object #:parachute)
  :pathname "./test/"
  :components ((:file "package"))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:cffi-object.test))))
