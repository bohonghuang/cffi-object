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
              (setf (vector2-x vec1) 3.0
                    (vector2-y vec1) 4.0)
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
