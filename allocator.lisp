(in-package #:cffi-object)

(defstruct foreign-allocator
  (allocator (constantly (cffi:null-pointer)) :type (function (non-negative-fixnum) (values cffi:foreign-pointer)))
  (deallocator #'values :type (function (cffi:foreign-pointer))))

(declaim (type foreign-allocator *default-foreign-allocator*))
(defparameter *default-foreign-allocator* (make-foreign-allocator :allocator #'cffi-sys:%foreign-alloc :deallocator #'cffi-sys:foreign-free))

(declaim (type foreign-allocator *foreign-allocator*))
(defparameter *foreign-allocator* *default-foreign-allocator*)

(declaim (inline %make-sized-monotonic-buffer-allocator))
(defstruct (sized-monotonic-buffer-allocator (:include foreign-allocator) (:constructor %make-sized-monotonic-buffer-allocator))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (offset 0 :type non-negative-fixnum))

(declaim (inline make-sized-monotonic-buffer-allocator))
(defun make-sized-monotonic-buffer-allocator (&key (pointer (cffi:null-pointer)) (size 0) (upstream *foreign-allocator*))
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
                                                                                     (prog1 (funcall (foreign-allocator-allocator upstream) size)
                                                                                       (setf offset buffer-size)
                                                                                       (setf deallocator (foreign-allocator-deallocator upstream)))
                                                                                     (error "Cannot allocate a space of ~D byte~:P with allocator ~A." size allocator-1)))))
                                                              :deallocator #'values :size size :pointer pointer)))
    (setf allocator-1 allocator-2)
    allocator-2))

(defmacro with-monotonic-buffer-allocator ((&key (size 128) (upstream '*foreign-allocator*) (values '#'values)) &body body)
  (with-gensyms (buffer pointer size-var allocator)
    `(let* ((,size-var ,size)
            (,buffer (cffi:make-shareable-byte-vector ,size-var)))
       (declare (dynamic-extent ,buffer))
       (cffi:with-pointer-to-vector-data (,pointer ,buffer)
         (let ((,allocator (make-sized-monotonic-buffer-allocator :pointer ,pointer :size ,size-var :upstream ,upstream)))
           (declare (dynamic-extent ,allocator))
           (multiple-value-call ,values
             (let ((*foreign-allocator* ,allocator))
               ,@body)))))))

(defmacro with-default-allocator (&body body)
  `(let ((*foreign-allocator* *default-foreign-allocator*))
     ,@body))
