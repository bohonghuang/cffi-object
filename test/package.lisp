(defpackage cffi-object.test
  (:use #:cl #:parachute #:cffi #:cffi-object))

(in-package #:cffi-object.test)

(define-test suite)

(defcstruct vector2
  (x :float)
  (y :float))

(define-struct-cobject (vector2 (:struct vector2)))

(define-test basic-struct :parent suite
  (foreign-free
   (prog1 (let ((vec1 (make-vector2 :x 1.0 :y 2.0)))
            (let ((vec2 (copy-vector2-into vec1 (make-vector2)))
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
            (unmanange-vector2 vec1))
     (tg:gc :full t))))

(defcstruct camera-2d
  (offset (:struct vector2))
  (target (:struct vector2))
  (rotation :float)
  (zoom :float))

(define-struct-cobject (camera-2d (:struct camera-2d)))

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

(define-struct-cobject (sockaddr (:struct sockaddr)))

(define-test primitive-type-array-in-struct :parent suite
  (let ((sockaddr (make-sockaddr))
        (port (make-carray 1 :element-type '(unsigned-byte 16)
                             :initial-element 8080))
        (addr (make-carray 4 :element-type '(unsigned-byte 8)
                             :initial-contents #(192 168 31 1))))
    (creplace (sockaddr-sa-data sockaddr)
              (print (make-unmanaged-carray (cobject-pointer port) '(signed-byte 8) 2))
              :start1 0)
    (creplace (sockaddr-sa-data sockaddr)
              (print (make-unmanaged-carray (cobject-pointer addr) '(signed-byte 8) 4))
              :start1 2)
    (is carray-equal
        (make-unmanaged-carray (cobject-pointer port) '(signed-byte 8) 2)
        (make-carray 2 :element-type '(signed-byte 8)
                       :displaced-to (sockaddr-sa-data sockaddr)
                       :displaced-index-offset 0))
    (loop :with addr := (make-unmanaged-carray (cobject-pointer addr) '(signed-byte 8) 4)
          :for i :below 4
          :for j :from 2
          :do (is = (caref addr i) (caref (sockaddr-sa-data sockaddr) j)))))

(defcstruct sample
  (left :float)
  (right :float))

(define-struct-cobject (sample (:struct sample)))

(defcstruct sample-ring-buffer
  (data (:array (:struct sample) 2048))
  (free-position :size)
  (data-position :size)
  (free-count :size)
  (data-count :size))

(define-struct-cobject (sample-ring-buffer (:struct sample-ring-buffer)))

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
         (arr2 (make-unmanaged-carray (cobject-pointer arr1) 'sample 4)))
    (loop :for i :below 4
          :for sample := (make-sample :left (/ i 3.0) :right (- (/ i 3.0)))
          :do (setf (caref arr2 i) sample)
          :do (multiple-value-bind (i j) (truncate i 2)
                (is sample-equal sample (caref (caref arr1 i) j))))))

(define-test nested-pointer :parent suite
  (let* ((pptr (foreign-alloc :pointer))
         (ptr (setf (mem-ref pptr :pointer) (foreign-alloc :uint32)))
         (value (setf (mem-ref ptr :uint32) 12345))
         (cpptr (make-managed-cpointer pptr '(cpointer (unsigned-byte 32))))
         (cptr (make-managed-cpointer ptr '(unsigned-byte 32))))
    (is cpointer-eq (cref cpptr) cptr)
    (is = value (cref (cref cpptr)))
    (is = value (cref cptr))))

(define-test array-pointer :parent suite
  (let* ((arr1 (make-carray 2048 :element-type 'sample))
         (arr2 (cref (make-unmanaged-cpointer (cobject-pointer arr1) '(carray sample 2048)))))
    (is carray-equal arr1 arr2)
    (is pointer-eq (cobject-pointer arr1) (cobject-pointer arr2))))

(defcstruct sample-vector
  (data (:pointer (:struct sample)))
  (size :size))

(define-struct-cobject (sample-vector (:struct sample-vector)))

(define-test object-pointer-in-struct :parent suite
  (let* ((buffer (make-carray 4 :element-type 'sample))
         (buffer-pointer (make-unmanaged-cpointer (cobject-pointer buffer) 'sample))
         (vector1 (make-sample-vector :data buffer :size 4))
         (vector2 (make-sample-vector :data buffer-pointer :size 4)))
    (is cpointer-eq buffer-pointer (sample-vector-data vector1))
    (is cpointer-eq buffer-pointer (sample-vector-data vector2))))

(defcstruct foreign-string
  (data :string))

(define-struct-cobject (foreign-string (:struct foreign-string)))

(define-test string :parent suite
  (let ((str (make-foreign-string :data "Test")))
    (is string= "Test" (foreign-string-data str)))
  (let ((strarr (make-carray 3 :element-type 'string :initial-contents '("123" "456" "789"))))
    (is string= "123" (caref strarr 0))
    (is string= "456" (caref strarr 1))
    (is string= "789" (caref strarr 2))
    (setf (caref strarr 0) "000")
    (is string= "000" (caref strarr 0))))

(define-test array-of-pointer :parent suite
  (let ((arr (make-carray 3 :element-type '(carray (cpointer (signed-byte 8)) 1)
                            :initial-element (make-carray 1 :element-type '(cpointer (signed-byte 8))
                                                            :initial-element (make-unmanaged-cpointer (cffi-sys::make-pointer 123) '(signed-byte 8))))))
    (loop :for i :below 3
          :do (is cpointer-eq (caref (caref arr i) 0) (make-unmanaged-cpointer (make-pointer 123) '(signed-byte 8))))))
