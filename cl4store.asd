(defsystem :cl4store
    :author  "Mariano Montone"
    :maintainer "Mariano Montone"
    :license "BSD" 
    :description "4store"
    :version "0.0.1"
    :depends-on (:drakma
		 :split-sequence
		 :cl-ppcre
		 :puri
		 :fare-matcher
		 :cl-rdfxml
		 :log5
		 :fiveam)
    :serial t
    :components ((:file "package")
		 (:file "util")
		 (:file "sparql")
		 (:file "cl4store")
		 (:file "test")))
