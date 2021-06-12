;;; type-options.lisp
;;;
;;; Extracting the typename and type parameters from a CTYPE

(in-package #:introspect-ctype)

;;; Determine Typename

(defgeneric ctype-typename (type)
  (:documentation
   "Determine the typename from a given `CTYPE' TYPE.

    Returns a simple which identifies the type"))

(defmethod ctype-typename ((type carray))
  'array)

(defmethod ctype-typename ((type disjunction))
  (flet ((disjoin-type (&optional (t1 t) (t2 nil))
           (cond
             ((equal t1 t2) t1)
             ((eq t1 nil) t2)
             ((eq t2 nil) t1)
             (t `(or ,t1 ,t2)))))

    (reduce #'disjoin-type (mapcar #'ctype-typename (junction-ctypes type)))))

(defmethod ctype-typename ((type conjunction))
  (flet ((conjoin-type (&optional (t1 t) (t2 t))
           (cond
             ((equal t1 t2) t1)
             ((eq t1 t) t2)
             ((eq t2 t) t1)
             (t `(and ,t1 ,t2)))))

    (reduce #'conjoin-type (mapcar #'ctype-typename (junction-ctypes type)))))

(defmethod ctype-typename ((type cmember))
  (with-accessors ((members cmember-members)) type
    (if (length= 1 members)
        (constant-typename (first members))
        (call-next-method))))

(defmethod ctype-typename (type)
  (car (ensure-list (unparse type))))

(defgeneric constant-typename (constant)
  (:documentation
   "Determine the typename of a constant"))

(defmethod constant-typename ((constant array))
  'array)

(defmethod constant-typename (constant)
  (car (ensure-list (type-of constant))))


;;; Determine Type Parameters

(defgeneric ctype-options (typename ctype)
  (:documentation
   "Determine the parameters of a given type.

    TYPENAME is a simple which identifies the type class, as returned
    by CTYPE-TYPENAME.

    CTYPE is the actual type represented by a `CTYPE' object.

    Returns the list of type parameters."))

(defmethod ctype-options ((typename (eql 'array)) ctype)
  (list (unparse (container-element-ctype ctype))
        (container-dims ctype)))

(defmethod ctype-options ((typename t) ctype)
  (cdr (ensure-list (unparse ctype))))
