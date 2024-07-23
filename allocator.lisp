(in-package #:cffi-object)

(declaim (inline make-cobject-allocator))
(defstruct cobject-allocator
  (allocator (constantly (cffi:null-pointer)) :type (function (cffi::foreign-type) (values cffi:foreign-pointer)))
  (deallocator #'values :type (function (cffi:foreign-pointer))))

(declaim (type cobject-allocator *default-cobject-allocator*))
(defparameter *default-cobject-allocator* (make-cobject-allocator
                                           :allocator (lambda (type) (cffi-sys:%foreign-alloc (cffi:foreign-type-size type)))
                                           :deallocator #'cffi-sys:foreign-free))

(declaim (type cobject-allocator *cobject-allocator*))
(defparameter *cobject-allocator* *default-cobject-allocator*)

(declaim (inline make-leaky-allocator))
(defun make-leaky-allocator (&key (allocator (cobject-allocator-allocator *cobject-allocator*)) (deallocator #'values))
  (make-cobject-allocator :allocator allocator :deallocator deallocator))

(defmacro with-leaky-allocator (&body body)
  (with-gensyms (allocator)
    `(let ((,allocator (make-leaky-allocator)))
       (declare (dynamic-extent ,allocator))
       (let ((*cobject-allocator* ,allocator)) . ,body))))

(declaim (inline %make-sized-monotonic-buffer-allocator))
(defstruct (sized-monotonic-buffer-allocator (:include cobject-allocator) (:constructor %make-sized-monotonic-buffer-allocator))
  (pointer (cffi:null-pointer) :type cffi:foreign-pointer)
  (size 0 :type non-negative-fixnum)
  (offset 0 :type non-negative-fixnum))

(declaim (inline make-sized-monotonic-buffer-allocator))
(defun make-sized-monotonic-buffer-allocator (&key (pointer (cffi:null-pointer)) (size 0) (upstream *cobject-allocator*))
  #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (let* ((allocator-1 nil)
         (allocator-2 (%make-sized-monotonic-buffer-allocator :allocator (lambda (type &aux (size (cffi:foreign-type-size type)) (align (cffi:foreign-type-alignment type)))
                                                                           (declare (type non-negative-fixnum size align))
                                                                           (with-accessors ((offset sized-monotonic-buffer-allocator-offset)
                                                                                            (buffer-size sized-monotonic-buffer-allocator-size)
                                                                                            (pointer sized-monotonic-buffer-allocator-pointer)
                                                                                            (allocator sized-monotonic-buffer-allocator-allocator)
                                                                                            (deallocator sized-monotonic-buffer-allocator-deallocator))
                                                                               allocator-1
                                                                             (let ((align-offset (mod (- align offset) align)))
                                                                               (if (<= (+ offset align-offset size) buffer-size)
                                                                                   (prog1 (cffi:inc-pointer pointer (incf offset align-offset))
                                                                                     (incf offset size))
                                                                                   (if upstream
                                                                                       (prog1 (funcall (cobject-allocator-allocator upstream) type)
                                                                                         (setf offset buffer-size)
                                                                                         (setf deallocator (cobject-allocator-deallocator upstream)))
                                                                                       (error "Cannot allocate a space of ~D byte~:P with allocator ~A." size allocator-1))))))
                                                              :deallocator #'values :size size :pointer pointer)))
    (setf allocator-1 allocator-2)
    allocator-2))

(defmacro with-monotonic-buffer-allocator ((&key
                                              buffer pointer
                                              (size (if buffer `(length ,buffer) 128))
                                              (upstream '*cobject-allocator*)
                                              (values '#'values))
                                           &body body)
  (with-gensyms (buffer-var pointer-var size-var allocator)
    (flet ((wrap-with-buffer-var (form)
             (cond
               (buffer `(let ((,buffer-var ,buffer)) ,form))
               (pointer form)
               (t `(let ((,buffer-var (cffi:make-shareable-byte-vector ,size-var)))
                     (declare (dynamic-extent ,buffer-var)) ,form))))
           (wrap-with-pointer-var (form)
             (if pointer
                 `(let ((,pointer-var ,pointer)) ,form)
                 `(cffi:with-pointer-to-vector-data (,pointer-var ,buffer-var) ,form))))
      `(let ((,size-var ,size))
         ,(wrap-with-buffer-var
           (wrap-with-pointer-var
            `(let ((,allocator (make-sized-monotonic-buffer-allocator :pointer ,pointer-var :size ,size-var :upstream ,upstream)))
               (declare (dynamic-extent ,allocator))
               (multiple-value-call ,values
                 (let ((*cobject-allocator* ,allocator))
                   ,@body)))))))))

(defmacro with-default-allocator (&body body)
  `(let ((*cobject-allocator* *default-cobject-allocator*))
     ,@body))
