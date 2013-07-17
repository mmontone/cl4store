(defpackage :4store
  (:use :cl :drakma :parser-combinators)
  (:export sparql-query
	   sparql-query*
	   with-4store-server
	   with-graph
	   get-triples-list
	   get-graphs-list
	   select-rdfs-classes
           sparql-server-put-data-request
           sparql-update
           insert-triples
           delete-triples
           delete-all-triples
           delete-graph
	   *4store-server*
	   *graph*
	   parse-literal
	   render-literal
	   make-uri
	   define-uri-prefix
	   get-uri-prefix
           ;; Subject to deprecation
           ;; - these may not belong here.
           sparql-server-status-request))
