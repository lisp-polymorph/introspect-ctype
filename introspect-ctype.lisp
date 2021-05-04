;;;; introspect-ctype.lisp

(in-package #:introspect-ctype)




(defgeneric container-element-ctype (ctype))
(defmethod container-element-ctype ((ctype ctype::ctype)) (ctype::top)) ; ignorance
(defmethod container-element-ctype ((ctype ctype::cclass)) (ctype::bot))
(defmethod container-element-ctype ((ctype ctype::conjunction))
  (apply #'ctype::conjoin (mapcar #'container-element-ctype (ctype::junction-ctypes ctype))))
(defmethod container-element-ctype ((ctype ctype:disjunction))
  (apply #'ctype:disjoin (mapcar #'container-element-ctype (ctype:junction-ctypes ctype))))
(defmethod container-element-ctype ((ctype ctype:cvalues))
  (container-element-ctype (first (ctype:cvalues-required ctype))))
(defmethod container-element-ctype ((ctype ctype:ccons)) (ctype::bot))
(defmethod container-element-ctype ((ctype ctype:range)) (ctype::bot))
(defmethod container-element-ctype ((ctype ctype:fpzero)) (ctype::bot))
(defmethod container-element-ctype ((ctype ctype:ccomplex)) (ctype::bot))
;;TODO do something with cmember type!!
(defmethod container-element-ctype ((ctype ctype:carray)) (ctype:carray-eaet ctype))
(defmethod container-element-ctype ((ctype ctype:charset)) (ctype::bot))
(defmethod container-element-ctype ((ctype ctype:cfunction)) (ctype::bot))


(defmethod disjoin/2 ((dim1 (eql 'cl:*)) (dim2 t))
  'cl:*)

(defmethod disjoin/2 ((dim1 t) (dim2 (eql 'cl:*)))
  'cl:*)

(defmethod disjoin/2 ((dim1 integer) (dim2 list))
  'cl:*)
(defmethod disjoin/2 ((dim1 list) (dim2 integer))
  'cl:*)
(defmethod disjoin/2 ((dim1 integer) (dim2 integer))
  (if (= dim1 dim2)
      dim1
      'cl:*))

(defmethod disjoin/2 ((dim1 list) (dim2 list))
  (cond ((and (null dim1) (null dim2))
         nil)
        ((or (null dim1) (null dim2))
         'cl:*)
        ((= (length dim1) (length dim2))
         (mapcar #'disjoin/2 dim1 dim2))
        (t 'cl:*)))


(defmethod conjoin/2 ((dim1 (eql 'cl:*)) (dim2 list))
  dim2)

(defmethod conjoin/2 ((dim1 list) (dim2 (eql 'cl:*)))
  dim1)

(defmethod conjoin/2 ((dim1 (eql 'cl:*)) (dim2 integer))
  dim2)

(defmethod conjoin/2 ((dim1 integer) (dim2 (eql 'cl:*)))
  dim1)

(defmethod conjoin/2 ((dim1 (eql 'cl:*)) (dim2 (eql 'cl:*)))
  dim1)



(defmethod conjoin/2 ((dim1 integer) (dim2 list))
  'cl:*)
(defmethod conjoin/2 ((dim1 list) (dim2 integer))
  'cl:*)
(defmethod conjoin/2 ((dim1 integer) (dim2 integer))
  (if (= dim1 dim2)
      dim1
      'cl:*))

(defmethod conjoin/2 ((dim1 list) (dim2 list))
  (cond ((and (null dim1) (null dim2))
         nil)
        ((or (null dim1) (null dim2))
         'cl:*)
        ((= (length dim1) (length dim2))
         (mapcar #'disjoin/2 dim1 dim2))
        (t 'cl:*)))

(defun conjoin-dims (&rest dims)
  (reduce #'conjoin/2 dims))

(defun disjoin-dims (&rest dims)
  (reduce #'disjoin/2 dims))



(defgeneric container-dims (ctype))
(defmethod container-dims  ((ctype ctype::ctype)) 'cl:*) ; ignorance
(defmethod container-dims ((ctype ctype:cclass)) 'cl:*)
(defmethod container-dims ((ctype ctype:conjunction))
  (apply #'conjoin-dims (mapcar #'container-dims (ctype:junction-ctypes ctype))))
(defmethod container-dims ((ctype ctype:cvalues))
  (container-dims (first (ctype:cvalues-required ctype))))
(defmethod container-dims ((ctype ctype:disjunction))
  (apply #'disjoin-dims (mapcar #'container-dims (ctype:junction-ctypes ctype))))
(defmethod container-dims ((ctype ctype:ccons)) 'cl:*)
(defmethod container-dims((ctype ctype:range)) 'cl:*)
(defmethod container-dims ((ctype ctype:fpzero)) 'cl:*)
(defmethod container-dims ((ctype ctype:ccomplex)) 'cl:*)

(defmethod container-dims ((ctype ctype:carray)) (ctype:carray-dims ctype))
(defmethod container-dims ((ctype ctype:charset)) 'cl:*)
(defmethod container-dims ((ctype ctype:cfunction)) 'cl:*)
