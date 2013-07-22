(in-package :4store)

(require :trivial-shell)

(defvar *store-description-file* "/home/marian/src/jena-sdb/sdb.ttl")
(defvar *jdbc-driver* "org.postgresql.Driver")
(defvar *sdb-root* "/home/marian/src/jena-sdb")
(defvar *sdb-user* "postgres")
(defvar *sdb-password* "postgres")

(define-condition jena-sdb-command-error (simple-error)
  ())

(defun jena-sdb-command-error (message &rest args)
  (error 'jena-sdb-command-error :format-control message :format-arguments args))

(defun jena-sdb-command (command &rest args)
  (let ((command
	 (format nil "/usr/bin/env ~{~A ~} ~A --sdb ~A ~{~A ~}"
		 (list (format nil "SDBROOT=~A" *sdb-root*)
		       (format nil "PATH=~A/bin:$PATH" *sdb-root*)
		       (format nil "SDB_USER=~A" *sdb-user*)
		       (format nil "SDB_PASSWORD=~A" *sdb-password*)
		       (format nil "SDB_JDBC=~A" *jdbc-driver*))
		 command
		 *store-description-file*		
		 args)))
    (multiple-value-bind (output error-output exit-status)
	(trivial-shell:shell-command command)
      (when (plusp exit-status)
	(jena-sdb-command-error error-output))
      output)))
		  
(defun jena-sdb-create ()
  (jena-sdb-command "sdbconfig" "--create"))

(defun jena-sdb-format ()
  (jena-sdb-command "sdbconfig" "--format"))

(defun jena-sdb-load (filename)
  (jena-sdb-command "sdbload" filename))

(defun jena-sdb-indexes ()
  (jena-sdb-command "sdbconfig" "--indexes"))

(defun jena-sdb-drop-indexes()
  (jena-sdb-command "sdbconfig" "--dropIndexes"))

(defun jena-sdb-query (query)
  (tsv-to-lists
   (jena-sdb-command "sdbquery" "--results" "TSV" (format nil "'~A'" query))))

(defun jena-sdb-sql (sql)
  (jena-sdb-command "sdbsql" sql))

(defun jena-sdb-info ()
  (jena-sdb-command "sdbinfo"))
