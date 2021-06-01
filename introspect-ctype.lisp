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
              with in-key = nil
              for arg in params
              collect
                (cond
                  ((and in-key
                        (listp arg)
                        (= (length arg) 2)
                        (eq (second arg) '*))

                   (list (first arg) t))

                  ((eql arg '*) t))

              do
                (when (eq arg '&key)
                  (setf in-key t)))

          ,result))

       (otherwise
        type)))

    ((otherwise &rest args)
     (declare (ignore args))
     type)))

;;; Macros

(defmacro with-type-info ((whole (&optional (typename (gensym "TYPENAME")) (parameters (gensym "PARAMETERS"))) &optional env default) form &body body)
  "Determine the type of a form in a given environment

The type of the form is determined from the environment and
normalized.

WHOLE is a variable which will receive the entire type.

TYPENAME is a variable which will receive the name of the type. If the
type of the form is a list, this variable receives the CAR of the
list, otherwise it is bound to the symbol naming the type.

PARAMETERS is a variable which will receive the parameters of the type
of FORM. If the type of the form is a list, this variable receive the
CDR of the list, otherwise it is bound to NIL.

ENV is the environment in which the form is found.

DEFAULT is an optional variable, which, if given, receives a default
value of the same type as the form.

FORM is the form of which to determine the type.

BODY is a list of forms which are evaluated, in an implicit PROGN,
with the WHOLE, TYPENAME, PARAMETERS and DEFAULT variables visible to
them."

  (once-only (form env)
    `(let ((,whole (ctype:unparse
                    (ctype:specifier-ctype
                     (normalize-type (%form-type ,form ,env)) ,env))))

       (destructuring-bind (,typename &rest ,parameters)
           (ensure-list ,whole)

         (declare (ignorable ,typename ,parameters))

         ,@(if default
               `((let ((,default (default ,whole ,env)))
                   ,@body))

               body)))))

(defmacro with-array-info ((elt dim) array-form env &body body)
  (let ((init-form-type (gensym))
        (carray (gensym)))
    `(let ((,init-form-type (%form-type ,array-form ,env)))
       (let* ((,carray (ctype:specifier-ctype ,init-form-type))
              (,elt (ctype:unparse (container-element-ctype ,carray)))
              (,dim (container-dims ,carray)))
         ,@body))))

(defun constant-array-dimensions-p (dim env)
  "Return true if all array dimensions in DIM are a compile-time constant."

  (and (constantp dim env)
       (every
	(lambda (x)
	  (and (constantp x)
	       (not (eql 'cl:* x))))
	(ensure-list dim))))
