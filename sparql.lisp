(in-package :4store)

;; SPARQL toplevel

(defun sparql-compile (sparql)
  (let ((parsed
	 (parse-sequence* (sparql-select)
			  sparql)))
    (if (not parsed)
	(error "Error parsing sparql")
	(render-sparql parsed))))

(defmacro sparql (sparql)
  (sparql-compile sparql))

;; SPARQL parsing

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
   (<- distinct (choice :distinct (result nil)))
   (<- vars (many1* (sparql-var)))
   (<- where (sparql-where))
   (<- options
       (many* 
	(choices (sparql-order-by)
		 (sparql-limit)
		 (sparql-offset))))
   (list :select
	 distinct
	 (apply #'list :vars vars)
	 where
	 (list :options options))))

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
   (where-body)))

(defun where-body ()
  (many1*
   (choices
    (sparql-where-graph)
    (where-triple))))

(defun where-triple ()
  (choices
   (sparql-union)
   (sparql-triple)))

(def-cached-arg-parser transform (predicate)
  "Parser: return a token satisfying a predicate."
  #'(lambda (inp)
      (typecase inp
        (end-context (constantly nil))
        (parser-combinators::context
	 (let ((result (funcall predicate (parser-combinators::context-peek inp))))
           (if result
               (let ((closure-value
                      (make-instance 'parser-combinators::parser-possibility
                                     :tree result :suffix (parser-combinators::context-next inp))))
                 #'(lambda ()
                     (when closure-value
                       (prog1
                           closure-value
                         (setf closure-value nil)))))
               (constantly nil)))))))

(defun sparql-triple ()
  (transform
   (lambda (triple)
     (and (listp triple)
	  (parse-sequence*
	   (named-seq?
	    (<- x (triple-subject))
	    (<- y (triple-predicate))
	    (<- z (triple-object))
	    (list :triple x y z))
	   triple)))))

(defun sparql-union ()
  (transform
   (lambda (list)
     (and (listp list)
	  (parse-sequence*
	   (seq-list?
	    :union
	    (triples-block)
	    (triples-block))
	   list)))))

(defun sparql-where-graph ()
  (transform
   (lambda (list)
     (and (listp list)
	  (parse-sequence*
	   (seq-list?
	    :graph
	    (item)
	    (many1* (where-triple)))
	   list)))))

(defun triples-block ()
  (transform
   (lambda (list)
     (and (listp list)
	  (parse-sequence*
	   (many1* (sparql-triple))
	   list)))))

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

;; SPARQL rendering

(defun render-sparql (sparql)
  (let ((expanded (reduce-strings (expand sparql))))
    `(strcat (list ,@expanded))))

(defun reduce-strings (list)
  "Join adjacent strings in a list, leave other values intact."
  (let ((accum ())
        (span ""))
    (dolist (part list)
      (cond ((stringp part) (setf span (concatenate 'string span part)))
            (t (when (not (string= "" span))
                 (push span accum)
                 (setf span ""))
               (push part accum))))
    (if (not (string= "" span))
        (push span accum))
    (nreverse accum)))

(defun strcat (args)
  "Concatenate a list of strings into a single one."
  (let ((result (make-string (reduce #'+ args :initial-value 0 :key 'length))))
    (loop :for pos = 0 :then (+ pos (length arg))
          :for arg :in args
          :do (replace result arg :start1 pos))
    result))

(defun expand (term)
  (expand-term (first term) term))

(defmethod expand-term ((type (eql :select)) term)
  (append
   (list "SELECT ")
   (list (or (and (cadr term) "DISTINCT ")
	     ""))
   (loop for subterm in (cddr term)
      appending (expand subterm))))
	
(defmethod expand-term ((type (eql :vars)) vars)
  (let ((vars (rest vars)))
    (loop for var in vars
	 appending (expand var)
	 appending (list " "))))

(defmethod expand-term ((type (eql :var)) var)
  (list (format nil "?~A" (second var))))

(defmethod expand-term ((type (eql :eval)) form)
  (list
   `(4store::render-literal ,(second form))))

(defmethod expand-term ((type (eql :where)) where)
  (append (list "WHERE { ")
	  (loop for term in (cadr where)
	     appending (expand term))
	  (list "}")))

(defmethod expand-term ((type (eql :graph)) graph)
  (append
   (list "GRAPH ")
   (list `(render-literal ,(second graph)))
   (list " {")
   (loop for term in (caddr graph)
      appending (expand term))
   (list "} ")))

(defmethod expand-term ((type (eql :triple)) triple)
  (append
   (expand (second triple))
   (list " ")
   (expand (third triple))
   (list " ")
   (expand (nth 3 triple))
   (list " .")))

(defmethod expand-term ((type (eql :union)) union)
  (append
   (list "{")
   (loop for term in (second union)
	appending (expand term))
   (list "} UNION {")
   (loop for term in (third union)
	appending (expand term))
   (list "} ")))

(defmethod expand-term ((type (eql :options)) options)
  (loop for option in (cadr options)
       appending (expand option)))

(defmethod expand-term ((type (eql :order-by)) order-by)
  (list " ORDER BY " `(render-literal ,(second order-by))))

;; Example:
;; (sparql
;;  (:select ?z ?y
;; 	  :where (:graph #<rdf:type> (:union ((?x ?y ?z))
;; 					     ((?w ?z ?x))) (?x ?w ?w))
;; 	  :order-by 22))
