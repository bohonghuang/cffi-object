(defsystem cffi-object.ops
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "Apache-2.0"
  :description "A Common Lisp library that enables fast and convenient interoperation with foreign objects."
  :homepage "https://github.com/bohonghuang/cffi-object"
  :bug-tracker "https://github.com/bohonghuang/cffi-object/issues"
  :source-control (:git "https://github.com/bohonghuang/cffi-object.git")
  :depends-on (#:cffi-object #:cffi-ops)
  :components ((:file "ops"))
  :in-order-to ((test-op (test-op #:cffi-object/test))))
