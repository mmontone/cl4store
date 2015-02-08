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
		 ;:fare-matcher
		 :cl-rdfxml
		 :log5
		 :parser-combinators)
    :serial t
    :components ((:file "package")
		 (:file "util")
		 (:file "sparql")
		 (:file "cl4store"))
    :in-order-to ((asdf:test-op (asdf:test-op :cl4store-tests))))
