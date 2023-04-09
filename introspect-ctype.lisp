;;; introspect-ctype.lisp
;;;
;;; General type introspection utilities

(in-package #:introspect-ctype)

(deftype ind () `(integer 0 #.array-dimension-limit))

(deftype maybe (typename) `(or null ,typename))

(defparameter *default-impl* (make-hash-table))

(defun %form-type (form &optional env)
  (let ((cl-form-types:*handle-sb-lvars* t))
    (cl-form-types:nth-form-type form env 0 nil t)))

(defun %list-form-types (form &optional env)
  (let* ((cl-form-types:*handle-sb-lvars* t))
    (loop :for i :from 0
          :for type := (cl-form-types:nth-form-type form env i nil t)
          :while type
          :collect type)))

(defun %dimensions-comp (dimensions)
    (cond ((eql '* dimensions) 0)
          ((listp dimensions) (mapcar (lambda (x) (if (eql '* x) 0 x)) dimensions))
          (t dimensions)))

(defgeneric default-form (type &optional environment))

(macrolet ((gen (types form)
             `(progn
                ,@(loop :for type :in types
                        :collect `(defmethod default-form ((type (eql ',type)) &optional environment)
                                    (declare (ignorable environment))
                                    ,form)))))
  (gen (bit fixnum integer rational) 0)
  (gen (float double-float single-float long-float real) 0.0)
  (gen (number complex) #c(0 0))
  (gen (character base-char) #\Nul)
  (gen (standard-char) #\a)
  (gen (symbol t) t)
  (gen (keyword) :t)
  (gen (hash-table) '(make-hash-table))
  (gen (list boolean atom null) nil)
  (gen (pathname) #P"")
  (gen (vector) '(make-array 0 :adjustable t))
  (gen (bit-vector) '(make-array 0 :element-type 'bit :adjustable t :fill-pointer 0))
  (gen (simple-bit-vector) '(make-array 0 :element-type 'bit))
  (gen (string) '(make-array 0 :element-type 'character :adjustable t :initial-element #\Nul :fill-pointer 0))
  (gen (simple-array) (make-array 0)) ;;Maybe it should error here, since array dimension is not specified?
   ;;What happens with just array? Or just sequence? I guess nothing
  (gen (simple-string) '(make-array 0 :element-type 'character :initial-element #\Nul))
  (gen (simple-base-string) '(make-array 0 :element-type 'base-char :initial-element #\Nul)))


(defgeneric default-form-for-parametric-type (typename type &optional env))


(defmethod default-form-for-parametric-type ((typename (eql 'mod)) type &optional env)
  (declare (ignorable typename type env))
  0)

(defmethod default-form-for-parametric-type ((typename (eql 'unsigned-byte)) type &optional env)
  (declare (ignorable typename type env))
  0)

(defmethod default-form-for-parametric-type ((typename (eql 'signed-byte)) type &optional env)
  (declare (ignorable typename type env))
  0)

(defmethod default-form-for-parametric-type ((typename (eql 'integer)) type &optional env)
  (declare (ignorable typename env))
  (or (second type) 0))

(defmethod default-form-for-parametric-type ((typename (eql 'eql)) type &optional env)
  (declare (ignorable typename env))
  (second type))

(defmethod default-form-for-parametric-type ((typename (eql 'member)) type &optional env)
  (declare (ignorable typename env))
  (second type))

(defmethod default-form-for-parametric-type ((typename (eql 'rational)) type &optional env)
  (declare (ignorable typename env))
  (second type))

(defmethod default-form-for-parametric-type ((typename (eql 'real)) type &optional env)
  (declare (ignorable typename env))
  (second type))

(defmethod default-form-for-parametric-type ((typename (eql 'float)) type &optional env)
  (declare (ignorable typename env))
  (second type))

(defmethod default-form-for-parametric-type ((typename (eql 'complex)) type &optional env)
  (declare (ignorable typename))
  `(complex ,(default (second type) env) ,(default (second type) env)))

(defmethod default-form-for-parametric-type ((typename (eql 'cons)) type &optional env)
  (declare (ignorable typename))
  `(cons ,(default (second type) env) ,(default (third type) env)))

(defmethod default-form-for-parametric-type ((typename (eql 'vector)) type &optional env)
  (declare (ignorable typename))
  (let ((rest (rest type)))
    `(make-array ',(if (= 2 (length rest))
                       (%dimensions-comp (second rest))
                       0)
      :adjustable t
      :fill-pointer 0
      :element-type ',(or (first rest) t)
      :initial-element ,(if (first rest)
                            (default (first rest) env)
                            0))))

(defmethod default-form-for-parametric-type ((typename (eql 'bit-vector)) type &optional env)
  (declare (ignorable typename env))
  (let ((rest (rest type)))
    `(make-array ,(or (first rest) 0) :element-type 'bit :adjustable t :fill-pointer 0)))

(defmethod default-form-for-parametric-type ((typename (eql 'string)) type &optional env)
  (declare (ignorable typename env))
  (let ((rest (rest type)))
    `(make-array ',(if (= 2 (length rest))
                       (%dimensions-comp (second rest))
                       0)
      :element-type 'character
      :adjustable t
      :fill-pointer 0
      :initial-element #\Nul)))

(defmethod default-form-for-parametric-type ((typename (eql 'simple-array)) type &optional env)
  (declare (ignorable typename))
  (let ((rest (rest type)))
    `(make-array ',(if (= 2 (length rest))
                       (%dimensions-comp (second rest))
                       0)
      :element-type ',(or (first rest) t)
      :initial-element ,(if (first rest)
                            (default (first rest) env)
                            0))))

(defmethod default-form-for-parametric-type ((typename (eql 'simple-string)) type &optional env)
  (declare (ignorable typename env))
  (let ((rest (rest type)))
    `(make-array ',(if (= 2 (length rest))
                       (%dimensions-comp (second rest))
                       0)
      :element-type 'character
      :initial-element #\Nul)))

(defmethod default-form-for-parametric-type ((typename (eql 'simple-base-string)) type &optional env)
  (declare (ignorable typename env))
  (let ((rest (rest type)))
    `(make-array ',(if (= 2 (length rest))
                       (%dimensions-comp (second rest))
                       0)
      :element-type 'base-char
      :initial-element #\Nul)))

(defmethod default-form-for-parametric-type ((typename (eql 'array)) type &optional env)
  (declare (ignorable typename))
  (let ((rest (rest type)))
    (unless rest
      (error 'simple-error :format-control "Cannot create a default array with unknown dimensions"))
   `(make-array ',(if (= 2 (length rest))
                      (%dimensions-comp (second rest))
                      0)
     :element-type ',(or (first rest) t)
     :adjustable t
     :initial-element ,(if (first rest)
                           (default (first rest) env)
                           0))))


(defun default (type &optional environment)
  "Return a reasonable default object for a given type."
  (multiple-value-bind (item knownp) (gethash type *default-impl*)
    (if knownp
        item
        (progn
          (setf type (sb-ext:typexpand type environment))
          (if (symbolp type)
              (default-form type environment)
              (default-form-for-parametric-type (first type) type environment))))))

(defun normalize-type (type)
  "Normalize function types.

If TYPE is a FUNCTION type specifier, * specifiers occurring within
the lambda-list are replaced with T. Otherwise TYPE is returned as
is."

  (destructuring-case (ensure-list type)
    ((function &optional (params '*) (result '*))
     (typecase params
       (list
        `(function
          ,(loop
             :with in-key := nil
             :for arg :in params
             :collect
             (cond
               ((and in-key
                     (listp arg)
                     (= (length arg) 2)
                     (eq (second arg) '*))

                (list (first arg) t))

               ((eql arg '*) t))

             :do
             (when (eq arg '&key)
               (setf in-key t)))

          ,result))

       (otherwise
        type)))

    ((otherwise &rest args)
     (declare (ignore args))
     type)))

;;; Macros

(defmacro with-type-info ((whole (&optional (typename (gensym "TYPENAME")) &rest options-lambda-list) &optional env default) form &body body)
  "Determine the type of a form in a given environment

The type of the form is determined from the environment and
normalized.

WHOLE is a variable which will receive the entire type.

TYPENAME is a variable which will receive a symbol that identifies the
type class, as returned by by CTYPE-TYPENAME.

OPTIONS-LAMBDA-LIST is a destructuring lambda-list, for destructuring
the type's parameters, as returned by CTYPE-OPTIONS. An implicit &REST
parameter is added if the lambda-list does not have one already. Do
not use &AUX variables.

ENV is the environment in which the form is found.

DEFAULT is an optional variable, which, if given, receives a default
value of the same type as the form.

FORM is the form of which to determine the type.

BODY is a list of forms which are evaluated, in an implicit PROGN,
with the bindings variables WHOLE, TYPENAME, DEFAULT and the variables
in OPTIONS-LAMBDA-LIST visible."

  (with-gensyms (ctype)
    (once-only (form env)
      (flet ((destructure-type-options (lambda-list body)
               (if lambda-list
                   (with-gensyms (rest)
                     (let ((has-rest-p (member '&rest lambda-list)))
                       `((destructuring-bind
                               (,@lambda-list
                                ,@(unless has-rest-p
                                    `(&rest ,rest)))
                             (ctype-options ,typename ,ctype)
                           ,@(unless has-rest-p `((declare (ignorable ,rest))))
                           ,@body))))
                   body)))

        `(let* ((,ctype (ctype:specifier-ctype ;; FIXME Broken:when function is being passed in
                         (normalize-type (%form-type ,form ,env)) ,env)) ;; it returns values type
                (,whole (ctype:unparse ,ctype)))

           (declare (ignorable ,whole))

           (let ((,typename (ctype-typename ,ctype)))
             (declare (ignorable ,typename))

             ,@(destructure-type-options
                options-lambda-list
                (if default
                    `((let ((,default (default ,whole ,env)))
                        ,@body))

                    body))))))))

(defmacro when-types ((&rest types) else-form &body body)
  "Evaluate the forms in BODY only if the types match a given typenamme symbol.

TYPES is a list of types to compare. Each element is of the form (TYPE
EXPECTED) where TYPE (evaluated) is the form producing the actual type
and EXPECTED (not evaluated) is the expected type specifier. The types
are considered to match if every TYPE is EQUAL to the corresponding
EXPECTED type. This is best suited for comparing the value of the
TYPENAME in WITH-TYPE-INFO.

ELSE-FORM is the form which is evaluated (and returned) if the types
do not match.

BODY is the list of forms which are evaluated, in an implicit PROGN,
if the types match. The value returned by the last form is returned
from the WHEN-TYPES form."

  (flet ((make-type-check (type)
           (destructuring-bind (form exp) type
             `(equal ,form ',exp))))

    `(if (not (and ,@(mapcar #'make-type-check types)))
         ,else-form
         (progn ,@body))))

;;; Utility functions

(defun constant-array-dimensions-p (dim env)
  "Return true if all array dimensions in DIM are a compile-time constant."

  (and (constantp dim env)
       (every
        (lambda (x)
          (and (constantp x)
               (not (eql 'cl:* x))))
        (ensure-list dim))))

(defun constant-form-value (form env)
  "Return the value of a form if it is a constant.

FORM is a form.

ENV is the environment in which FORM is found.

Returns two values:

  1. The constant value if the form is constant in the
     environment. Otherwise is FORM itself.

  2. True if the form is constant, otherwise is NIL."

  (if (constantp form env)
      (let ((type (cl-form-types:nth-form-type form env 0 t t)))
        (cond
          ((and (listp type)
                (length= type 2)
                (eq (first type) 'eql))

           (values (second type) t))

          ((eq type 'null)
           (values nil t))

          (t
           (values form nil))))

      (values form nil)))
