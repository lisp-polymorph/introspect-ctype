;;;; introspect-ctype.asd

(asdf:defsystem #:introspect-ctype
    :description "Ctype extractors"
    :author "Commander Thrashdin"
    :license  "MIT"
    :version "0.1"
    :serial t
    :depends-on (#:ctype)
    :components ((:file "package")
                 (:file "introspect-ctype")))
