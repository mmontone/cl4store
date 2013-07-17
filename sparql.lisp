(in-package :4store)

(define-condition parse-failure (simple-condition)
  ())

(defun parse-failure (reason &rest args)
  (error 'parse-failure
	 :format-control reason
	 :format-arguments args))

;; (defun many (parser input)

;; (defun or-else (parsers input)
;;   (let ((input-copy (alexandria:copy-sequence 'cons input)))
;;     (handler-case
;; 	(funcall parser input-copy)
;;       (

;; (defun or-parse (&rest parsers)
 

(defun sparql-compile (sparql)
  (let ((sparql-input (alexandria:copy-sequence 'cons sparql)))
    (ecase (first sparql-input)
      (:select (sparql-compile-select sparql-input)))))

(defun sparql-compile-select (input)
  (when (not (equalp (first input) :select))
    (parse-failure "No :select keyword"))
  (pop input)
  (let ((vars (parse-select-vars input))
	(where (parse-select-where input))
	(options (parse-select-options input)))
    (with-output-to-string (s)
      (format s "SELECT ")
      (render-select-vars vars s)
      (format s " ")
      (render-select-where where s)
      (when options
	(format s " ")
	(render-select-options input s)))))

(defun parse-select-where (input)
  (when (equalp (first input) :where)
    (parse-failure "No :where keyword"))
  (let ((where-body (rest input)))
    (if (listp (first where-body))
	(parse-where-graph (first where-body))
	(parse-where-body (rest input)))))

(defun parse-where-graph (input)
  (when (not (equalp ))

(defun parse-select-options (input)
  )

(defun parse-var (input)
  (let ((var-string (symbol-name input)))
    (mdo
      (<- prefix (parse-string (choice #\? #\$) var-string))
      (<- var-name (many1* #'alphanum?))
      (result (list :var var-name)))))

(def-cached-parser sparql-var
  "Parser: return a token satisfying a predicate."
  #'(lambda (inp)
      (typecase inp
        (end-context (constantly nil))
        (parser-combinators::context
	 (let ((symbol (parser-combinators::context-peek inp)))
	   (if (not (symbolp symbol))
	       (constantly nil)
	       ; else
	       (let ((var (symbol-name symbol)))
		 (let ((prefix (char var 0)))
		   (if (not (or (equalp prefix #\?)
				(equalp prefix #\$)))
		       (constantly nil)
		       ; else
		       (let ((closure-value
			      (make-instance 'parser-combinators::parser-possibility
					     :tree (list :var
							 (subseq var 1 (length var)))
					     :suffix (parser-combinators::context-next inp))))
			 #'(lambda ()
			     (when closure-value
			       (prog1
				   closure-value
				 (setf closure-value nil))))))))))))))

(defun sparql-select ()
  (named-seq?
   :select
   (<- vars (many1* (sparql-var)))
   (<- where (sparql-where))
   (<- options
       (many* 
	(choices (sparql-order-by)
		 (sparql-limit)
		 (sparql-offset))))
   (list :select (apply #'list :vars vars)
	 where
	 options)))

(defun sparql-order-by ()
  (seq-list?
   :order-by
   (item)))

(defun sparql-limit ()
  (seq-list?
   :limit
   (item)))

(defun sparql-offset ()
  (seq-list?
   :offset
   (item)))

(defun sparql-where ()
  (seq-list?
   :where
   (sparql-triples)))

(defun sparql-triples ()
  (many1* (sparql-triple)))

(defun sparql-triple ()
  (named-seq?
   '[
   (<- x (triple-subject))
   (<- y (triple-predicate))
   (<- z (triple-object))
   ']
   (list :triple x y z)
   ))

(defun triple-subject ()
  (choice1 (sparql-var)
	   (mdo
	     (<- x (item))
	     (result (list :eval x)))))

(defun triple-predicate ()
  (choice1 (sparql-var)
	   (mdo
	     (<- x (item))
	     (result (list :eval x)))))

(defun triple-object ()
  (choice1 (sparql-var)
	   (mdo
	     (<- x (item))
	     (result (list :eval x)))))

(defun parse-select-vars (input)
  (let (token vars)
    (setf token (pop input))
    (loop while (and input (not (equalp token :where)))
	 do (progn
	      (push token vars)
	      (setf token (pop input))))
    (cond
      ((not (equalp token :where))
       (parse-failure ":where not found"))
      ((null vars)
       (parse-failure "Selecting no vars"))
      (t
       (progn
	 (push token input)
	 (mapcar #'parse-var vars))))))

(defun render-select-vars (vars out)
  (format out "~A" vars))

(defun render-select-where (where out)
  (format out "WHERE { ~A }" where))

(defun render-select-options (options out)
  (format out " OPTIONS ~A" options))

(defun parse-var (input)
  (let ((var (symbol-name input)))
    (let ((prefix (char var 0)))
      (if (not (or (equalp prefix #\?)
		   (equalp prefix #\$)))
	  (parse-failure "Invalid variable ~A" input)
					; else
	  (list :var
		(subseq var 1 (length var)))))))  

;; (sparql-select user prop val
;; 	       :where ?belonging (property "source") ?user
;; 	              ?belonging (property "type") "belonging"
;; 		      ?belonging (property "target") group-uri
;; 		      ?user (property type) "user"
;; 		      ?user ?prop ?val)

;; (sparql SELECT ?user ?prop ?val
;; 	WHERE {
;; 	?belonging (property "source") ?user .
;; 	?belonging (property "type") "belonging" .
;; 	?belonging (property "target") group-uri .
;; 	?user (property type) "user" .
;; 	?user ?prop ?val . })
	       

;; (defmacro sparql-select (&rest return-vars &key where)
;;   (4store:sparql-query* (list ,@(mapcar #'symbol-name return-vars))
;; 			   
