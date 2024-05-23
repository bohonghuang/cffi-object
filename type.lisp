(in-package #:cffi-object)

(defun primitive-type-p (type)
  (if (consp type)
      (destructuring-case type
        ((signed-byte n)
         (case n
           (8 :int8)
           (16 :int16)
           (32 :int32)
           (64 :int64)))
        ((unsigned-byte n)
         (case n
           (8 :uint8)
           (16 :uint16)
           (32 :uint32)
           (64 :uint64))))
      (case type
        (single-float :float)
        (double-float :double)
        (character :char)
        (string :string))))

(defun cobject-type= (type1 type2)
  (if (and (listp type1) (listp type2)
           (member (car type1) '(carray cpointer))
           (member (car type2) '(carray cpointer)))
      (progn
        (unless (eq (first type1) (first type2))
          (return-from cobject-type= nil))
        (unless (cobject-type= (second type1) (second type2))
          (return-from cobject-type= nil))
        (unless (listp (third type1))
          (setf (third type1) (list (third type1))))
        (unless (listp (third type2))
          (setf (third type2) (list (third type2))))
        (equal type1 type2))
      (or (type= type1 type2)
          (and (symbolp type1) (symbolp type2)
               (eql (find-class type1 nil) (find-class type2 nil))))))

(setf (fdefinition 'cffi-element-type) (fdefinition 'cffi::element-type))

(defun cffi-pointer-type (type)
  (or (cffi::pointer-type type) :void))
