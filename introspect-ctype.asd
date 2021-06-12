;;;; introspect-ctype.asd

(asdf:defsystem #:introspect-ctype
    :description "Ctype extractors"
    :author "Commander Thrashdin"
    :license  "MIT"
    :version "0.1"
    :serial t
    :depends-on (#:ctype #:cl-form-types #:alexandria)
    :components ((:file "package")
                 (:file "containers")
                 (:file "type-options")
                 (:file "introspect-ctype")))
