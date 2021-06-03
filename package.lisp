;;;; package.lisp

(defpackage #:introspect-ctype
  (:use #:cl
        #:ctype)

  (:import-from #:alexandria
                #:with-gensyms
                #:once-only
                #:ensure-list
                #:destructuring-case)

  (:export #:with-type-info
           #:with-array-info
           #:container-element-ctype
           #:container-dims
           #:constant-array-dimensions-p
           #:default
           #:ind
           #:maybe))
