(in-package #:cffi-object)

(defun foreign-alloc/ecl (type)
  (ext:with-backend
    :bytecodes (si:allocate-foreign-data :void (cffi:foreign-type-size type))
    :c/c++ (ffi:c-inline
            (type (cffi:foreign-type-size type)) (:object :fixnum) :object
            "ecl_allocate_foreign_data(#0, #1)"
            :one-liner t :side-effects t)))

(defun foreign-free/ecl (ptr)
  (declare (ignorable ptr))
  (ext:with-backend :bytecodes (cffi:foreign-free ptr) :c/c++ (progn)))

(defun setup-ecl-allocator ()
  (setf *default-cobject-allocator*
        (make-cobject-allocator
         :allocator #'foreign-alloc/ecl
         :deallocator #'foreign-free/ecl)
        *cobject-allocator* *default-cobject-allocator*
        (fdefinition 'manage-cobject)
        (let ((manage-cobject (fdefinition 'manage-cobject)))
          (named-lambda manage-cobject/ecl (cobject)
            (ext:with-backend
              :bytecodes (funcall manage-cobject cobject)
              :c/c++ (if (eq (cobject-allocator-deallocator *cobject-allocator*) #'foreign-free/ecl)
                         cobject (funcall manage-cobject cobject)))))
        (fdefinition 'unmanage-cobject)
        (let ((unmanage-cobject (fdefinition 'unmanage-cobject)))
          (named-lambda unmanage-cobject/ecl (cobject)
            (if (ext:get-finalizer cobject)
                (funcall unmanage-cobject cobject)
                (warn "Object ~A has no finalizer, so its memory cannot be unmanaged." cobject))
            (cobject-pointer cobject)))))

(setup-ecl-allocator)
