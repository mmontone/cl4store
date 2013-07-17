(in-package :4store)

(defmacro defdynvar (name &optional value doc)
  (let* ((name-string (symbol-name name))
	 (raw-name (subseq name-string
			   1
			   (1- (length name-string))))
	 (fname (intern (format nil "CALL-WITH-~A"
				raw-name)))
	 (mname (intern (format nil "WITH-~A"
				raw-name))))
    `(progn
       (defvar ,name ,value ,doc)
       (defun ,fname (value fun)
	 (let ((,name value))
	   (funcall fun)))
       (defmacro ,mname (value &body body)
	 (list ',fname value `(lambda () ,@body))))))

(defun make-keyword (string)
  (intern (string-upcase string) :keyword))

(defvar *uri-prefixes* (make-hash-table))

(defmacro define-uri-prefix (name prefix &optional documentation)
  (declare (ignore documentation))
  `(setf (gethash (make-keyword ',name)
		  *uri-prefixes*)
	(puri:parse-uri ,prefix)))

(defun get-uri-prefix (name)
  (gethash (make-keyword name)
	   *uri-prefixes*))

(defun make-uri (prefix uri &rest args)
  (puri:merge-uris
   (apply #'format nil (cons uri args))
   (get-uri-prefix prefix)))

(defun clean-literal (literal)
  (remove #\Return literal))

(defmethod render-literal (value)
  (clean-literal (prin1-to-string value)))
(defmethod render-literal ((value integer))
  (format nil "\"~A\"^^<http://www.w3.org/2001/XMLSchema#integer>" value))
(defmethod render-literal ((value string))
  (format nil "~S" value))
(defmethod render-literal ((uri puri:uri))
  (clean-literal (format nil "<~A>" (puri:render-uri uri nil))))
(defmethod render-literal ((x (eql t)))
  "\"true\"^^<http://www.w3.org/2001/XMLSchema#boolean>")
(defmethod render-literal ((x (eql nil)))
  "\"false\"^^<http://www.w3.org/2001/XMLSchema#boolean>")
(defmethod render-literal ((x symbol))
  (if (keywordp x)
      (format nil "\"~S\"^^<http://common-lisp.net#keyword>" x)
      (format nil "\"~S\"" x)))

(defun parse-typed-literal (literal)
  (cl-ppcre:register-groups-bind (value type)
      ("(.*)\\^\\^(.*)" literal)
    (cond
      ((equalp type "<http://www.w3.org/2001/XMLSchema#integer>")
       (values (parse-integer (read-from-string value))
	       :integer))
      ((equalp type "<http://www.w3.org/2001/XMLSchema#string>")
       (values (read-from-string value)
	       :string))
      ((equalp type "<http://www.w3.org/2001/XMLSchema#boolean>")
       (values (let ((value (read-from-string value)))
		 (if (equalp value "true")
		     t
		     nil))
	       :boolean))
      ((equalp type "<http://common-lisp.net#keyword>")
       (values (read-from-string (read-from-string value))
	       :keyword)))))

(defun parse-boolean-literal (literal)
  (cond
    ((equalp literal "true")
     (values t t))
    ((equalp literal "false")
     (values nil t)))
  (values nil nil))    

(defun parse-literal (literal)
  (let ((literal (clean-literal literal)))
    (let* ((first (char literal 0)))
      (multiple-value-bind (value type)
	  (parse-typed-literal literal)
	(when type
	  (return-from parse-literal value)))
      (cond
	((equalp first #\")
	 (subseq literal 1 (1- (length literal))))
	((equalp first #\<)
	 (puri:parse-uri (subseq literal 1 (1- (length literal)))))
	((parse-integer literal :junk-allowed t)
	 (parse-integer literal :junk-allowed t))
	((multiple-value-bind (value parsed-p)
	     (parse-boolean-literal literal)
	   (declare (ignore value))
	   parsed-p)
	 (parse-boolean-literal literal))	   
	(t literal)))))

(defun uri-reader (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((chars
	 (loop for char = (read-char stream)
	    while (not (equalp char #\>))
	    collect char)))
    (puri:uri (coerce chars 'string))))

(set-dispatch-macro-character
  #\# #\< #'uri-reader)

;; Example
;; #<http://www.google.com>
