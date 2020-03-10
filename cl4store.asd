(defsystem :cl4store
  :author  "Mariano Montone"
  :maintainer "Mariano Montone"
  :license "BSD"
  :description "4store"
  :version "0.0.1"
  :depends-on (:drakma
               :cl-rdfxml
               :log5
               :cl-sparql)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "cl4store")))
