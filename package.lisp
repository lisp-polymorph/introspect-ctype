;;;; package.lisp

(defpackage #:introspect-ctype
  (:use #:cl
        #:ctype)

  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:ensure-list
                #:destructuring-case
                #:length=)

  (:export #:with-type-info
           #:when-types
           #:with-array-info
           #:container-element-ctype
           #:container-dims
           #:constant-array-dimensions-p
           #:constant-form-value
           #:default
           #:%form-type
           #:ind
           #:maybe))
