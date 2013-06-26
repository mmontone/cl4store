(in-package :cl4store)

(defun bigdata-request (uri &key
			      (method :get)
			      parameters
			      content
			      (content-type "application/x-www-form-urlencoded")
			      additional-headers)
  (DRAKMA:HTTP-REQUEST "http://localhost:8080/bigdata/sparql" :METHOD :GET
		       :PARAMETERS
		       '(("query"
			  . "SELECT DISTINCT ?subject ?predicate ?object  WHERE { GRAPH <http://bonanza.cl.no> { ?subject ?predicate ?object  } }"))
		       :CONTENT NIL :CONTENT-TYPE
		       "application/x-www-form-urlencoded"
		       :accept "text/tab-separated-values"))

(defun sparql-update (data &optional  (method :post) (server-url *4store-server*))
  "Send a SPARQL update request to the server, and return the result.
  Expects a valid SPARQL query for its second argument, in a text string.
  Uses POST by default, but the :method keyword argument can be used to force
  POST, PUT, DELETE or whatever other method tickles your fancy."
  (4store-request (concatenate 'string server-url "/sparql")
		  :method method
		  :content data
		  :content-type "application/x-turtle"))

;#+bigdata
(defun insert-triples (triples &optional (graph *graph*)
				 (server-url *4store-server*))
  "Inserts a list of triples into the store.
  The 'triples argument is expected to be a list of proper lists, each
  containing subject, predicate and object."
  
  (let ((content (with-output-to-string
		   (outstr)
		 (mapcar #'(lambda (triple)
                               (format outstr "~A ~A ~A . "
                                       (literal (first triple))
                                       (literal (second triple))
                                       (literal (third triple))))
                           triples))))
  (4store-request (concatenate 'string server-url "/sparql")
		  :method :post
		  :content content
		  :parameters `(("context-uri" . ,(puri:render-uri graph nil)))
		  :accept "text/tab-separated-values"
		  :content-type "application/x-turtle")))

;; bigdata

(defun delete-triples (triples &optional (graph *graph*)
				 (server-url *4store-server*))
  "Remove the supplied set of triples from the graph.
  Expects the 'triples argument to be a list of three-element lists.
  If the Object is plain text, it's expected to already be quoted."
  (loop for triple in triples
       do
       (4store-request (concatenate 'string server-url "/sparql")
		       :method :delete
		       :accept "text/tab-separated-values"
		       :parameters `(("c" . ,(format nil "<~A>" (puri:render-uri graph nil)))
				     ("s" . ,(first triple))
				     ("p" . ,(second triple))
				     ("o" . ,(third triple)))
		       :content-type "application/x-turtle")))
