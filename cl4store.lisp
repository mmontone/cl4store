(in-package :4store)

(defdynvar *graph* nil "Knowledge base graph")
(defdynvar *4store-server* "http://localhost:8080" "4store host address")

;; Tell drakma to treat these additional types as text rather than
;; binary.
(defparameter *4store-text-content-types* (list (cons "application" "sparql-results+xml")
						(cons "application" "rdf+xml")))

(defun 4store-request (uri &key
			 (method :get)
			 parameters
			 content
			 (content-type "application/x-www-form-urlencoded")
			 additional-headers)
  (let ((drakma:*text-content-types* (append *4store-text-content-types*
					     drakma:*text-content-types*))
	(drakma:*drakma-default-external-format* :utf-8))
    (print `(http-request ,uri
		  :method ,method
		  :parameters ,parameters
		  :content ,content
		  :content-type ,content-type
		  :additional-headers ,additional-headers))
    (multiple-value-bind (request-result status-code)
	(http-request uri
		      :method method
		      :parameters parameters
		      :content content
		      :content-type content-type
		      :additional-headers additional-headers)
      (if (not (equalp status-code 200))
	  (error request-result))
      request-result)))



(defun sparql-server-put-data-request (filepath &optional
						  (graphname *graph*)
						  (server-url *4store-server*))
  "Perform an HTTP put request with the data contained in a file.
  Arguments:
  - the base url pathname of the SPARQL server
  - relative URL component (i.e, the graph name)
  - the path to the file
  Assumes that the input file is valid RDF/XML"
  (let ((drakma:*text-content-types* *4store-text-content-types*)
	(drakma:*drakma-default-external-format* :utf-8))
    (drakma:http-request (concatenate 'string server-url "data/" graphname)
                         ;; Tell 4store to replace the entire graph:
                         :method :put
                         :content-type "application/rdf+xml"
                         :content filepath
                         ;; Force drakma to compute the content-length instead of
                         ;; using chunked encoding:
                         :content-length t)))

(defun sparql-server-status-request (&optional (server-url *4store-server*))
  "Returns the numeric HTTP status code from the server.
If all is well, the return code will be 200 (for OK)."
  (nth-value 1 (4store-request (concatenate 'string server-url "status"))))

(defun select-rdfs-classes (&optional (server-url *4store-server*))
  "Select all the RDFS classses in the knowledgebase. Return the multiple values from the query POSTed to the knowledgebase's sparql http end-point. The first value is the body of the response in the sparql query results XML format."
  (4store-request (format nil "~A/sparql/" server-url)
		:method :post
		:parameters `(("query" . "prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                                          prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                                          select distinct ?type
                                          where {
                                             ?x a ?type .
                                          }
                                          order by ?type"))))


(defun sparql-query (query &optional
			     (server-url *4store-server*))
  (4store-request 
   (concatenate 'string server-url "/sparql/")
   :parameters `(("query" . ,query)                      
		 ("output" . "text"))))

(defun tsv-to-lists (tsv)
  "Convert a 4store TSV result string to a list of lisp lists"
  (delete-if #'null
             (with-input-from-string (instr tsv)
               (loop for line = (read-line instr nil nil)
                     while line collect (unless (cl-ppcre:scan "^\\?" line)
                                          (split-sequence:split-sequence #\Tab line))))))

(defun tsv-to-list (tsv)
  "Convert a 4store TSV result string to a list of lisp lists"
  (delete-if #'null
             (with-input-from-string (instr tsv)
               (loop for line = (read-line instr nil nil)
                     while line appending (unless (cl-ppcre:scan "^\\?" line)
                                          (split-sequence:split-sequence #\Tab line))))))

(defun sparql-query* (return-vars query-params &optional optional-params
						 (graph *graph*)
						 (server-url *4store-server*))
  "Send a SPARQL query to the server, and return the result in TSV format.
  XML and JSON are available as return formats in 4store, but TSV is faster and
  simpler to parse into lists for further processing.
  Arguments:
  - server-url (bare string)
  - graph ID (string containing the URI serving as the graph ID)
  - return-vars: a list of strings naming the return variables. Question-marks
  are prepended automatically.
  - query-params: a list of the triples that comprise the query itself. It's
  currently necessary to manually prepend question-marks to the return-var
  names here.
  - optional-params: a list of optional query-triples. If one or more of the
  return-vars is optional, put the relevant query-triples here. If they can be
  satisfied, the resulting value will be returned; if not, NIL is returned."
  (tsv-to-lists
   (let* ((optional-parameters (if optional-params
				   (format nil ".~% OPTIONAL { ~{~{~A ~}~^.~%~} } " optional-params)
				   ""))
	  (query (format nil "SELECT DISTINCT ~{?~A ~} WHERE { GRAPH ~A { ~{~{~A ~}~^.~%~}~A } }"
			 return-vars
			 graph
			 query-params
			 optional-parameters)))
     (sparql-query query server-url))))

(defun get-triples-list (&optional (graph *graph*)
			   (server-url *4store-server*))
  "Retrieves all triples in the store.
  Useful for smoke-testing; use with caution in large stores, because it returns
  _everything_."
  (sparql-query*
   '("subject" "predicate" "object")
   '(("?subject" "?predicate" "?object"))
   nil
   graph
   server-url))

(defun get-graphs-list (&optional (server-url *4store-server*))
  (tsv-to-list
   (sparql-query
    "SELECT DISTINCT ?g
     WHERE {
         GRAPH ?g {
            ?s ?p ?o
     }}"
    server-url)))

;;;; Currently tested indirectly, via 'insert-triples
(defun sparql-update (data &optional  (method :post) (server-url *4store-server*))
  "Send a SPARQL update request to the server, and return the result.
  Expects a valid SPARQL query for its second argument, in a text string.
  Uses POST by default, but the :method keyword argument can be used to force
  POST, PUT, DELETE or whatever other method tickles your fancy."
  (4store-request (concatenate 'string server-url "/update/")
		  :method method
		  :parameters `(("update" . ,data)
				("mime-type" . "application/x-turtle"))))

(defun insert-triples (triples &optional (graph *graph*)
				 (server-url *4store-server*))
  "Inserts a list of triples into the store.
  The 'triples argument is expected to be a list of proper lists, each
  containing subject, predicate and object."
  (sparql-update 
   (with-output-to-string
                   (outstr)
                   (format outstr "INSERT DATA { GRAPH ~A { " graph)
                   (mapcar #'(lambda (triple)
                               (format outstr "~A ~A ~A . "
                                       (first triple)
                                       (second triple)
                                       (third triple)))
                           triples)
                   (format outstr "} } ")
                   outstr)
   :post
   server-url))

(defun delete-triples (triples &optional (graph *graph*)
				 (server-url *4store-server*))
  "Remove the supplied set of triples from the graph.
  Expects the 'triples argument to be a list of three-element lists.
  If the Object is plain text, it's expected to already be quoted."
  (sparql-update 
                 (with-output-to-string
                   (outstr)
                   (format outstr "DELETE DATA { GRAPH ~A { " graph)
                   (mapcar #'(lambda (triple)
                               (format outstr "~A ~A ~A . "
                                       (first triple)
                                       (second triple)
                                       (third triple)))
                           triples)
                   (format outstr "} } ")
                   outstr)
		 :post
		 server-url))

(defun delete-all-triples (&optional (graph *graph*)
			     (server-url *4store-server*))
  "Deletes _all_ triples in the specified graph."
  (let ((triples (get-triples-list graph server-url)))
    (when triples (delete-triples triples graph server-url))))

(defun delete-graph (&optional (graph-name *graph*)
		       (server-url *4store-server*))
  "Deletes the identified graph.
  Currently known not to work; despite being semantically correct and receiving
  a success code from the server."
  (drakma:http-request (concatenate 'string server-url "/data/")
                       :method :delete
                       :parameters `(("graph" . ,graph-name))))

(defun parse-result (xml-result)
  "Use cxml to parse a sparql result XML formatted string into a list structure."
  (cxml:parse xml-result (cxml-xmls:make-xmls-builder)))
