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
