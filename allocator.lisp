(in-package #:cffi-object)

(defstruct cobject-allocator
  (allocator (constantly (cffi:null-pointer)) :type (function (non-negative-fixnum) (values cffi:foreign-pointer)))
  (deallocator #'values :type (function (cffi:foreign-pointer))))

(declaim (type cobject-allocator *default-cobject-allocator*))
(defparameter *default-cobject-allocator* (make-cobject-allocator :allocator #'cffi-sys:%foreign-alloc :deallocator #'cffi-sys:foreign-free))

(declaim (type cobject-allocator *cobject-allocator*))
(defparameter *cobject-allocator* *default-cobject-allocator*)

(declaim (inline %make-sized-monotonic-buffer-allocator))
(defstruct (sized-monotonic-buffer-allocator (:include cobject-allocator) (:constructor %make-sized-monotonic-buffer-allocator))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (offset 0 :type non-negative-fixnum))

(declaim (inline make-sized-monotonic-buffer-allocator))
(defun make-sized-monotonic-buffer-allocator (&key (pointer (cffi:null-pointer)) (size 0) (upstream *cobject-allocator*))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((allocator-1 nil)
         (allocator-2 (%make-sized-monotonic-buffer-allocator :allocator (lambda (size)
                                                                           (declare (type non-negative-fixnum size))
                                                                           (with-accessors ((offset sized-monotonic-buffer-allocator-offset)
                                                                                            (buffer-size sized-monotonic-buffer-allocator-size)
                                                                                            (pointer sized-monotonic-buffer-allocator-pointer)
                                                                                            (allocator sized-monotonic-buffer-allocator-allocator)
                                                                                            (deallocator sized-monotonic-buffer-allocator-deallocator))
                                                                               allocator-1
                                                                             (if (<= (+ offset size) buffer-size)
                                                                                 (prog1 (cffi:inc-pointer pointer offset)
                                                                                   (incf offset size))
                                                                                 (if upstream
                                                                                     (prog1 (funcall (cobject-allocator-allocator upstream) size)
                                                                                       (setf offset buffer-size)
                                                                                       (setf deallocator (cobject-allocator-deallocator upstream)))
                                                                                     (error "Cannot allocate a space of ~D byte~:P with allocator ~A." size allocator-1)))))
                                                              :deallocator #'values :size size :pointer pointer)))
    (setf allocator-1 allocator-2)
    allocator-2))

(defmacro with-monotonic-buffer-allocator ((&key
                                              buffer
                                              (size (if buffer `(length ,buffer) 128))
                                              (upstream '*cobject-allocator*)
                                              (values '#'values))
                                           &body body)
  (with-gensyms (buffer-var pointer size-var allocator)
    (flet ((wrap-with-buffer-var (form)
             (if buffer
                 `(let ((,buffer-var ,buffer)) ,form)
                 `(let ((,buffer-var (cffi:make-shareable-byte-vector ,size-var)))
                    (declare (dynamic-extent ,buffer-var)) ,form))))
      `(let ((,size-var ,size))
         ,(wrap-with-buffer-var
           `(cffi:with-pointer-to-vector-data (,pointer ,buffer-var)
              (let ((,allocator (make-sized-monotonic-buffer-allocator :pointer ,pointer :size ,size-var :upstream ,upstream)))
                (declare (dynamic-extent ,allocator))
                (multiple-value-call ,values
                  (let ((*cobject-allocator* ,allocator))
                    ,@body)))))))))

(defmacro with-default-allocator (&body body)
  `(let ((*cobject-allocator* *default-cobject-allocator*))
     ,@body))
