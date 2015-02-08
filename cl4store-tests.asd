(defsystem :cl4store-tests
    :author  "Mariano Montone"
    :maintainer "Mariano Montone"
    :license "BSD" 
    :description "4store"
    :version "0.0.1"
    :depends-on (:cl4store
		 :fiveam)
    :serial t
    :components ((:file "test"))
    :perform (asdf:test-op (o c)
			   (uiop:symbol-call :4store-test :run-tests)))
