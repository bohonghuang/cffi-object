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

(define-test primitive-array :parent suite
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
