(defpackage cffi-object.test
  (:use #:cl #:parachute #:cffi #:cffi-ops #:cffi-object))

(in-package #:cffi-object.test)

(define-test suite)

(defcstruct vector2
  (x :float)
  (y :float))

(define-cobject-class (vector2 (:struct vector2)))

(define-test basic-struct :parent suite
  (foreign-free
   (prog1 (let ((vec1 (make-vector2 :x 1.0 :y 2.0)))
            (let ((vec2 (copy-vector2 vec1 (make-vector2)))
                  (vec3 (copy-vector2 vec1)))
              (is = 1.0 (vector2-x vec1))
              (is = 2.0 (vector2-y vec1))
              (true (vector2-equal vec1 vec2))
              (setf (vector2-x vec1) 3.0
                    (vector2-y vec1) 4.0)
              (false (vector2-equal vec1 vec2))
              (is = 3.0 (vector2-x vec1))
              (is = 4.0 (vector2-y vec1))
              (is = 1.0 (vector2-x vec2))
              (is = 2.0 (vector2-y vec2))
              (is = 1.0 (vector2-x vec3))
              (is = 2.0 (vector2-y vec3)))
            (unmanage-cobject vec1))
     (tg:gc :full t))))

(defcstruct camera-2d
  (offset (:struct vector2))
  (target (:struct vector2))
  (rotation :float)
  (zoom :float))

(define-cobject-class (camera-2d (:struct camera-2d)))

(define-test struct-type-slot :parent suite
  ((lambda (vec)
     (is = 1.0 (vector2-x vec))
     (is = 2.0 (vector2-y vec)))
   (prog1 (let ((cam (make-camera-2d :offset (make-vector2 :x 3.0 :y 4.0)
                                     :rotation 90.0
                                     :zoom 1.0)))
            (setf (camera-2d-target cam) (camera-2d-offset cam)
                  (vector2-x (camera-2d-offset cam)) 1.0
                  (vector2-y (camera-2d-offset cam)) 2.0)
            (is = 1.0 (vector2-x (camera-2d-offset cam)))
            (is = 2.0 (vector2-y (camera-2d-offset cam)))
            (is = 3.0 (vector2-x (camera-2d-target cam)))
            (is = 4.0 (vector2-y (camera-2d-target cam)))
            (is = 90.0 (camera-2d-rotation cam))
            (is = 1.0 (camera-2d-zoom cam))
            (camera-2d-offset cam))
     (tg:gc :full t))))

(define-test struct-array :parent suite
  ((lambda (vec)
     (is = 3.0 (vector2-x vec))
     (is = 4.0 (vector2-y vec)))
   (prog1 (let ((arr1 (make-carray 10 :element-type 'vector2)))
            (setf (caref arr1 0) (make-vector2 :x 1.0 :y 2.0)
                  (caref arr1 9) (make-vector2 :x 3.0 :y 4.0))
            (is = 1.0 (vector2-x (caref arr1 0)))
            (is = 2.0 (vector2-y (caref arr1 0)))
            (is = 3.0 (vector2-x (caref arr1 9)))
            (is = 4.0 (vector2-y (caref arr1 9)))
            (caref arr1 9))
     (tg:gc :full t)))
  (let ((arr2 (make-carray 3 :element-type 'vector2
                             :initial-element (make-vector2 :x 1.0 :y 2.0))))
    (loop :for i :below 3
          :do (is = 1.0 (vector2-x (caref arr2 i)))
              (is = 2.0 (vector2-y (caref arr2 i)))))
  (let* ((list3 (list (make-vector2 :x 1.0 :y 2.0)
                      (make-vector2 :x 3.0 :y 4.0)
                      (make-vector2 :x 5.0 :y 6.0)))
         (arr3 (make-carray 3 :element-type 'vector2
                              :initial-contents list3))
         (arr4 (make-carray 3 :element-type 'vector2
                              :initial-contents arr3))
         (arr5 (make-carray 2 :element-type 'vector2
                              :displaced-to arr3
                              :displaced-index-offset 1)))
    (loop :for i :below 3
          :do (is = (vector2-x (nth i list3)) (vector2-x (caref arr3 i)))
              (is = (vector2-y (nth i list3)) (vector2-y (caref arr3 i)))
              (is = (vector2-x (caref arr3 i)) (vector2-x (caref arr4 i)))
              (is = (vector2-y (caref arr3 i)) (vector2-y (caref arr4 i)))
          :when (plusp i)
            :do (is = (vector2-x (caref arr3 i)) (vector2-x (caref arr5 (1- i))))
                (incf (vector2-x (caref arr5 (1- i))))
                (is = (vector2-x (caref arr3 i)) (vector2-x (caref arr5 (1- i))))
                (is = (vector2-y (caref arr3 i)) (vector2-y (caref arr5 (1- i))))
                (incf (vector2-y (caref arr3 i)))
                (is = (vector2-y (caref arr3 i)) (vector2-y (caref arr5 (1- i)))))
    (let ((arr6 (make-carray 3 :element-type 'vector2)))
      (is = 3 (clength arr6))
      (cfill arr6 (make-vector2 :x -1.0 :y 0.0) :end 2)
      (creplace arr6 arr4 :start1 1 :end2 2)
      (is = -1.0 (vector2-x (caref arr6 0)))
      (is = 0.0 (vector2-y (caref arr6 0)))
      (is = 1.0 (vector2-x (caref arr6 1)))
      (is = 2.0 (vector2-y (caref arr6 1)))
      (is = 3.0 (vector2-x (caref arr6 2)))
      (is = 4.0 (vector2-y (caref arr6 2))))))

(define-test primitive-type-array :parent suite
  (let ((arr1 (make-carray 3 :element-type '(unsigned-byte 32))))
    (cfill arr1 1)
    (creplace arr1 (make-carray 3 :element-type '(unsigned-byte 32)
                                  :initial-contents '(1 2 3))
              :start1 1 :start2 1 :end2 2)
    (setf (caref arr1 2) 3)
    (is = 1 (caref arr1 0))
    (is = 2 (caref arr1 1))
    (is = 3 (caref arr1 2))
    (let ((arr2 (make-carray 3 :element-type '(unsigned-byte 32)
                               :initial-contents arr1)))
      (is carray-equal arr1 arr2)
      (setf (caref arr2 0) 0)
      (isnt carray-equal arr1 arr2))))

(defcstruct sockaddr
  (sa-family :ushort)
  (sa-data (:array :char 14)))

(define-cobject-class (sockaddr (:struct sockaddr)))

(define-test primitive-type-array-in-struct :parent suite
  (let ((sockaddr (make-sockaddr))
        (port (make-carray 1 :element-type '(unsigned-byte 16)
                             :initial-element 8080))
        (addr (make-carray 4 :element-type '(unsigned-byte 8)
                             :initial-contents #(192 168 31 1))))
    (creplace (sockaddr-sa-data sockaddr)
              (pointer-carray (cobject-pointer port) '(signed-byte 8) 2)
              :start1 0)
    (creplace (sockaddr-sa-data sockaddr)
              (pointer-carray (cobject-pointer addr) '(signed-byte 8) 4)
              :start1 2)
    (is carray-equal
        (pointer-carray (cobject-pointer port) '(signed-byte 8) 2)
        (make-carray 2 :element-type '(signed-byte 8)
                       :displaced-to (sockaddr-sa-data sockaddr)
                       :displaced-index-offset 0))
    (loop :with addr := (manage-cobject (pointer-carray (unmanage-cobject addr) '(signed-byte 8) 4))
          :for i :below 4
          :for j :from 2
          :do (is = (caref addr i) (caref (sockaddr-sa-data sockaddr) j)))))

(defcstruct sample
  (left :float)
  (right :float))

(define-cobject-class (sample (:struct sample)))

(defcstruct sample-ring-buffer
  (data (:array (:struct sample) 2048))
  (free-position :size)
  (data-position :size)
  (free-count :size)
  (data-count :size))

(define-cobject-class (sample-ring-buffer (:struct sample-ring-buffer)))

(define-test object-array-in-struct :parent suite
  (loop :with ring-buffer := (make-sample-ring-buffer)
        :with sample-buffer := (make-carray 2048 :element-type 'sample)
        :for i :below 2048
        :for sample := (make-sample :left (coerce (sin (* i 1/2048 2 pi)) 'single-float)
                                    :right (coerce (cos (* i 1/2048 2 pi)) 'single-float))
        :do (setf (caref (sample-ring-buffer-data ring-buffer) i) sample
                  (caref sample-buffer i) sample)
        :finally (is carray-equal sample-buffer (sample-ring-buffer-data ring-buffer))))

(define-test nested-array :parent suite
  (let* ((arr1 (make-carray 2 :element-type '(carray sample 2)
                              :initial-element (make-carray 2 :element-type 'sample
                                                              :initial-element (make-sample :left 0.5 :right 1.0)))) 
         (arr2 (pointer-carray (cobject-pointer arr1) 'sample 4)))
    (loop :for i :below 4
          :for sample := (make-sample :left (/ i 3.0) :right (- (/ i 3.0)))
          :do (setf (caref arr2 i) sample)
          :do (multiple-value-bind (i j) (truncate i 2)
                (is sample-equal sample (caref (caref arr1 i) j))))))

(define-test nested-pointer :parent suite
  (let* ((pptr (foreign-alloc :pointer))
         (ptr (setf (mem-ref pptr :pointer) (foreign-alloc :uint32)))
         (value (setf (mem-ref ptr :uint32) 12345))
         (cpptr (manage-cobject (pointer-cpointer pptr '(cpointer (unsigned-byte 32)))))
         (cptr (manage-cobject (pointer-cpointer ptr '(unsigned-byte 32)))))
    (is cpointer-eq (cref cpptr) cptr)
    (is = value (cref (cref cpptr)))
    (is = value (cref cptr))))

(define-test array-pointer :parent suite
  (let* ((arr1 (make-carray 2048 :element-type 'sample))
         (arr2 (cref (pointer-cpointer (cobject-pointer arr1) '(carray sample 2048)))))
    (is carray-equal arr1 arr2)
    (is pointer-eq (cobject-pointer arr1) (cobject-pointer arr2))))

(defcstruct sample-vector
  (data (:pointer (:struct sample)))
  (size :size))

(define-cobject-class (sample-vector (:struct sample-vector)))

(define-test object-pointer-in-struct :parent suite
  (let* ((buffer (make-carray 4 :element-type 'sample))
         (buffer-pointer (pointer-cpointer (cobject-pointer buffer) 'sample))
         (vector1 (make-sample-vector :data buffer :size 4))
         (vector2 (make-sample-vector :data buffer-pointer :size 4)))
    (is cpointer-eq buffer-pointer (sample-vector-data vector1))
    (is cpointer-eq buffer-pointer (sample-vector-data vector2))))

(defcstruct foreign-string
  (data :string))

(define-cobject-class (foreign-string (:struct foreign-string)))

(define-test string :parent suite
  (let ((str (make-foreign-string :data "Test")))
    (is string= "Test" (foreign-string-data str)))
  (let ((strarr (make-carray 3 :element-type 'string :initial-contents '("123" "456" "789"))))
    (is string= "123" (caref strarr 0))
    (is string= "456" (caref strarr 1))
    (is string= "789" (caref strarr 2))
    (setf (caref strarr 0) "000")
    (is string= "000" (caref strarr 0))))

(define-test array-of-array-of-pointer :parent suite
  (let ((arr (make-carray 3 :element-type '(carray (cpointer (signed-byte 8)) 1)
                            :initial-element (make-carray 1 :element-type '(cpointer (signed-byte 8))
                                                            :initial-element (pointer-cpointer (cffi-sys::make-pointer 123) '(signed-byte 8))))))
    (loop :for i :below 3
          :do (is cpointer-eq (caref (caref arr i) 0) (pointer-cpointer (make-pointer 123) '(signed-byte 8))))))

(define-test pointer-of-pointer :parent suite
  (let* ((arr1 (make-carray 1 :element-type '(signed-byte 8)
                              :initial-element 1))
         (arr2 (make-carray 1 :element-type '(signed-byte 8)
                              :initial-element 2))
         (arr3 (make-carray 1 :element-type '(signed-byte 8)
                              :initial-element 3))
         (arr4 (make-carray 3 :element-type '(cpointer (signed-byte 8))
                              :initial-contents (list arr1 arr2 arr3))))
    (loop :for ptr :in (ccoerce arr4 'list)
          :for arr :in (list arr1 arr2 arr3)
          :do (is cobject-eq arr ptr)
              (is = (caref arr 0) (cref ptr)))))

(define-test array-of-array :parent suite
  (let* ((arr1 (make-carray 1 :element-type '(signed-byte 8)
                              :initial-element 1))
         (arr2 (make-carray 1 :element-type '(signed-byte 8)
                              :initial-element 2))
         (arr3 (make-carray 1 :element-type '(signed-byte 8)
                              :initial-element 3))
         (arr4 (make-carray 3 :element-type '(carray (signed-byte 8) 1)
                              :initial-contents (list arr1 arr2 arr3))))
    (loop :for ptr :in (ccoerce arr4 'list)
          :for arr :in (list arr1 arr2 arr3)
          :do (is = (caref arr 0) (caref ptr 0)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cobj.ops:enable-cobject-ops))

(define-test ops :parent suite
  (let* ((vec2 (make-vector2 :x 1.0 :y 2.0))
         (cam (make-camera-2d :offset vec2))
         (cam2 (copy-camera-2d cam)))
    (clocally (declare (ctype (:object (:struct vector2)) vec2)
                       (ctype (:object (:struct camera-2d)) cam))
      (is = 1.0 (-> vec2 x))
      (is = 2.0 (-> (& vec2) y))
      (is = 1.0 (-> cam offset x))
      (is = 2.0 (-> (& cam) offset y))
      (is eql (find-class 'camera-2d) (class-of cam))
      (of-type foreign-pointer (& cam))
      (of-type foreign-pointer (& cam2)))
    (of-type foreign-pointer (& cam))))

(define-test character-array :parent suite
  (let ((arr (make-carray 10 :element-type 'character)))
    (loop :for c :across "Hello"
          :for i :from 0
          :do (setf (caref arr i) c)
          :finally (setf (caref arr 5) #\Nul))
    (is string= "Hello" (ccoerce arr 'string))
    (is = 10 (length (ccoerce arr 'list)))
    (is string= "Hello" (coerce (subseq (ccoerce arr 'list) 0 5) 'string))
    (setf (ccoerce arr 'string) "World")
    (is string= "World" (ccoerce arr 'string)))
  (let ((arr (make-carray 5 :element-type 'character :initial-contents "Hello World!")))
    (is string= "Hell" (ccoerce arr 'string)))
  (let ((arr (make-carray 20 :element-type 'character :initial-contents "Hello World!")))
    (is string= "Hello World!" (ccoerce arr 'string)))
  (let ((arr (make-carray 20 :element-type 'character :initial-element #\Nul)))
    (is string= "" (ccoerce arr 'string))))

(define-test monotonic-buffer-allocator :parent suite
  (with-monotonic-buffer-allocator (:size 8)
    (with-monotonic-buffer-allocator (:size 8)
      (of-type cobj::sized-monotonic-buffer-allocator cobj::*cobject-allocator*)
      (make-vector2)
      (is = 8 (cobj::sized-monotonic-buffer-allocator-offset cobj::*cobject-allocator*))
      (make-vector2)
      (is = 8 (cobj::sized-monotonic-buffer-allocator-offset cobj::*cobject-allocator*))
      (is eq #'values (cobj::sized-monotonic-buffer-allocator-deallocator cobj::*cobject-allocator*))
      (make-vector2)
      (is = 8 (cobj::sized-monotonic-buffer-allocator-offset cobj::*cobject-allocator*))
      (isnt eq #'values (cobj::sized-monotonic-buffer-allocator-deallocator cobj::*cobject-allocator*)))
    (is = 8 (cobj::sized-monotonic-buffer-allocator-offset cobj::*cobject-allocator*))
    (make-vector2)
    (is = 8 (cobj::sized-monotonic-buffer-allocator-offset cobj::*cobject-allocator*))
    (isnt eq #'values (cobj::sized-monotonic-buffer-allocator-deallocator cobj::*cobject-allocator*)))
  (tg:gc :full t))

(define-test readable-cobject :parent suite :fix (*print-readably*)
  (setf *print-readably* t)
  (define-test readable-cpointer
    (let ((cpointer (pointer-cpointer (make-pointer 1234) '(unsigned-byte 32))))
      (is cpointer-eq cpointer (read-from-string (prin1-to-string cpointer)))))
  (define-test readable-carray
    (let* ((carray (make-carray 10 :element-type '(unsigned-byte 32)))
           (displaced-carray (make-carray 10 :element-type '(unsigned-byte 32)
                                             :displaced-to carray)))
      (is carray-equal carray (read-from-string (prin1-to-string carray)))
      (is carray-equal displaced-carray (read-from-string (prin1-to-string displaced-carray)))))
  (define-test readable-simple-cobject
    (let ((vector2 (make-vector2)))
      (is vector2-equal vector2 (read-from-string (prin1-to-string vector2)))))
  (define-test readable-complex-cobject
    (let ((camera-2d (make-camera-2d)))
      (is camera-2d-equal camera-2d (read-from-string (prin1-to-string camera-2d))))))

(defcstruct aggregate-struct
  (a (:array :uint8 3) :count 0)
  (b (:array :uint8 1) :count 1)
  (c :uint8 :count 2))

(define-cobject-class (:struct aggregate-struct))

(defvar *aggregate-struct* nil)

(define-test aggregate-struct-slot :parent suite :fix (*aggregate-struct*)
  (setf *aggregate-struct* (make-aggregate-struct :b (make-carray 1 :element-type '(unsigned-byte 8) :initial-contents '(1))
                                                  :c (make-carray 2 :element-type '(unsigned-byte 8) :initial-contents '(2 3))))
  (define-test count=0
    (of-type cpointer (aggregate-struct-a *aggregate-struct*))
    (is carray-equal (make-carray 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)) (cref (aggregate-struct-a *aggregate-struct*)))
    (fail (setf (aggregate-struct-a *aggregate-struct*) (make-carray 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3)))))
  (define-test count=1
    (of-type carray (aggregate-struct-b *aggregate-struct*))
    (is equal '(unsigned-byte 8) (carray-element-type (aggregate-struct-b *aggregate-struct*)))
    (is carray-equal (make-carray 1 :element-type '(unsigned-byte 8) :initial-contents '(1)) (aggregate-struct-b *aggregate-struct*)))
  (define-test count=2
    (of-type carray (aggregate-struct-c *aggregate-struct*))
    (is equal '(unsigned-byte 8) (carray-element-type (aggregate-struct-b *aggregate-struct*)))
    (is carray-equal (make-carray 2 :element-type '(unsigned-byte 8) :initial-contents '(2 3)) (aggregate-struct-c *aggregate-struct*))))

(defcstruct void-pointer-struct
  (a :pointer)
  (b (:pointer :pointer))
  (c (:pointer :void))
  (d (:pointer (:pointer :void))))

(define-cobject-class (:struct void-pointer-struct))

(define-test void-pointer :parent suite
  (let* ((carray (make-carray 1 :element-type '(unsigned-byte 32) :initial-contents '(123456)))
         (cpointer (make-carray 1 :element-type '(cpointer (unsigned-byte 32)) :initial-contents (list carray))))
    (is = 123456 (cref (cref cpointer)))
    (let ((struct (make-void-pointer-struct :a carray :b cpointer :c carray :d cpointer)))
      (is = 123456 (cref (pointer-cpointer (cobject-pointer (void-pointer-struct-a struct)) '(unsigned-byte 32))))
      (is = 123456 (cref (cref (pointer-cpointer (cobject-pointer (void-pointer-struct-b struct)) '(cpointer (unsigned-byte 32))))))
      (is = 123456 (cref (pointer-cpointer (cobject-pointer (void-pointer-struct-c struct)) '(unsigned-byte 32))))
      (is = 123456 (cref (cref (pointer-cpointer (cobject-pointer (void-pointer-struct-d struct)) '(cpointer (unsigned-byte 32)))))))))
