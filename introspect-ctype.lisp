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

(defun %dimensions-comp (dimensions)
    (cond ((eql '* dimensions) 0)
          ((listp dimensions) (mapcar (lambda (x) (if (eql '* x) 0 x)) dimensions))
          (t dimensions)))

(defun default (type &optional environment)
  "Return a reasonable default object for a given type."
  (multiple-value-bind (item knownp) (gethash type *default-impl*)
    (if knownp
        item
        (progn
          (setf type (sb-ext:typexpand type environment))
          (if (symbolp type)
              (case type
                ((bit fixnum integer rational) 0)
                ((float double-float single-float long-float real) 0.0)
                ((number complex) #c(0 0))
                ((character base-char) #\Nul)
                (standard-char #\a)
                ((symbol t) t)
                (keyword :t)
                (hash-table `(make-hash-table))
                ((list boolean atom null) nil)
                (pathname #P"")
                (function '(lambda (&rest args)
                            (declare (ignore args)
                             (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))))
                (vector '(make-array 0 :adjustable t))
                (bit-vector '(make-array 0 :element-type 'bit :adjustable t))
                (string '(make-array 0 :element-type 'character :adjustable t :initial-element #\Nul))
                (simple-array (make-array 0)) ;;Maybe it should error here, since array dimension is nto specified?
                ;;What happens with just array? Or just sequence? I guess nothing
                (simple-string '(make-array 0 :element-type 'character :initial-element #\Nul))
                (simple-base-string '(make-array 0 :element-type 'base-char :initial-element #\Nul))
                (otherwise
                 (cond ((subtypep type 'structure-object environment)
                        (list (intern (concatenate 'string "MAKE-" (string type)))))
                       ((subtypep type 'standard-object environment)
                        `(make-instance ,type)))))
              (destructuring-bind (main . rest) type
                (case main
                  ((mod unsigned-byte singned-byte) 0)
                  ((integer eql member rational real float) (first rest))
                  (complex `(complex ,(default (first rest)) ,(default (first rest))))
                  (cons `(cons ,(default (first rest)) ,(default (first rest))))
                  (or (default (first rest)))
                  (vector `(make-array ',(if (= 2 (length rest))
                                             (%dimensions-comp (second rest))
                                             0)
                                       :adjustable t
                                       :element-type ',(or (first rest) t)
                                       :initial-element ,(if (first rest)
                                                             (default (first rest))
                                                             0)))
                  (bit-vector `(make-array ,(or (first rest) 0) :element-type 'bit :adjustable t))
                  (string `(make-array ',(if (= 2 (length rest))
                                             (%dimensions-comp (second rest))
                                             0)
                                       :element-type 'character
                                       :adjustable t
                                       :initial-element #\Nul))
                  (simple-array `(make-array ',(if (= 2 (length rest))
                                                   (%dimensions-comp (second rest))
                                                   0)
                                             :element-type ',(or (first rest) t)
                                             :initial-element ,(if (first rest)
                                                                   (default (first rest))
                                                                   0)))
                  (simple-string `(make-array ',(if (= 2 (length rest))
                                                    (%dimensions-comp (second rest))
                                                    0)
                                              :element-type 'character
                                              :initial-element #\Nul))
                  (simple-base-string `(make-array ',(if (= 2 (length rest))
                                                         (%dimensions-comp (second rest))
                                                         0)
                                                   :element-type 'base-char
                                                   :initial-element #\Nul))

                  (array `(make-array ',(if (= 2 (length rest))
                                            (%dimensions-comp (second rest))
                                            0)
                                      :element-type ',(or (first rest) t)
                                      :initial-element ,(if (first rest)
                                                            (default (first rest))
                                                            0))))))))))

;;; Function Type Normalization

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

        `(let* ((,ctype (ctype:specifier-ctype
                         (normalize-type (%form-type ,form ,env)) ,env))
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

(defun constant-array-dimensions-p (dim env)
  "Return true if all array dimensions in DIM are a compile-time constant."

  (and (constantp dim env)
       (every
        (lambda (x)
          (and (constantp x)
               (not (eql 'cl:* x))))
        (ensure-list dim))))
