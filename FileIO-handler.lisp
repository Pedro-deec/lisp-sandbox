;;;-----------------------------------------------------------------------------
;;;Topic
;;;	Files and File I/O
;;;
;;;Description
;;;	Import or Export information based on a parameters file.
;;;	Updates a file based on parameter's value; Or updates parameter's 
;;;	value in session based on file.
;;;-----------------------------------------------------------------------------

(defvar *international.series* '((199 200)))

;;;-----------------------------------------------------------------------------
(defvar *national.series* '((1000 2000) (3000 4000)))


;;;-----------------------------------------------------------------------------
(defun set.international.series (value)
  (setf *international.series* value))

;;;-----------------------------------------------------------------------------
(defun set.national.series (value)
  (setf *national.series* value))


;;;-----------------------------------------------------------------------------
(defun get.international.series ()
  *international.series*)


;;;-----------------------------------------------------------------------------
(defun get.national.series ()
  *national.series*)


;;;-----------------------------------------------------------------------------
;;;UPDATE.PARAMETER
;;;Description
;;;	Updates content of specific.type in file's contents.
;;;
;;;Arguments
;;;	specific.type is a keyword.
;;;	content is a list.
;;;	filename is a pathname.
;;;
;;;Output
;;;	void
;;;
;;;Debug example:
;;;	(update.parameter :INTERNATIONAL.SERIES nil "C:/home/lisp-sandbox/parameters.data")
;;;-----------------------------------------------------------------------------
(defun update.parameter (specific.type content filename)
  (cond ((null (probe-file filename))
	 ;; File has not created yet. Then, create a new file.
	 (with-open-file (stream filename
				 :direction :output
				 :if-exists :supersede)
	   (with-standard-io-syntax
	     (format stream ":~a ~a~%" specific.type content))))
	((null (find.parameter specific.type filename))
	 ;; File has no data about the specific.type. Then, put content in EOF.
	 (with-open-file (stream filename
				 :direction :output
				 :if-exists :append)
	   (with-standard-io-syntax
	     (format stream ":~a ~a~%" specific.type content))))
	(t
	 ;; File has data about the specific.type. Then, update content.
	 (let ((tmp.filename (concatenate 'string filename ".tmp")))
	   ;; Open a temp file to write the result to
	   (with-open-file (stream.out tmp.filename
				       :direction :output
				       :if-exists :supersede
				       :if-does-not-exist :create)
	     ;; Open the original file for reading
	     (with-open-file (stream.in filename)
	       (loop
		  for line = (read-line stream.in nil 'eof)
		  until (eql line 'eof)
		  do (if (numberp (search (write-to-string specific.type) line))
			 (format stream.out ":~a ~a~%" specific.type content)
			 (write-line line stream.out)))))
	   ;; Swap in the temp file for the original
	   (delete-file filename)
	   (rename-file tmp.filename filename))))
  (values))


;;;-----------------------------------------------------------------------------
;;;SAVE.PARAMETER.TO.FILE
;;;Description
;;;	Gets content of parameter "International Series parameter" and then
;;;	saves it into a file.
;;;
;;;Arguments
;;;	specific.type is a keyword.
;;;	filename is a pathname.
;;;
;;;Output
;;;	void
;;;
;;;Debug example:
;;;	(save.parameter.to.file :international.series "C:/home/lisp-sandbox/parameters.data")
;;;-----------------------------------------------------------------------------
(defmethod save.parameter.to.file ((specific.type (eql :international.series)) filename)
  (let ((content (get.international.series)))
    (update.parameter specific.type content filename))
  (values))


(defmethod save.parameter.to.file ((specific.type (eql :national.series)) filename)
  (let ((content (get.national.series)))
    (update.parameter specific.type content filename))
  (values))


;;;-----------------------------------------------------------------------------
;;;CONVERT.PARAMETERS
;;;Description
;;;	Reads content of file and convert it to a plist.
;;;
;;;Arguments
;;;	filename is a pathname.
;;;
;;;Output
;;;	plist
;;;-----------------------------------------------------------------------------
(defun convert.parameters (filename)
  ;; Ensure file has content
  (when (not (zerop (file-length filename)))
    ;; Read the words as lisp symbols
    (with-open-file (stream filename)
      (loop while (peek-char nil stream nil nil)
	 collect (read stream)))))


(defun find.parameter (specific.type filename)
  (member specific.type (convert.parameters filename)))


;;;-----------------------------------------------------------------------------
;;;LOAD.PARAMETER.TO.FILE
;;;Description
;;;	Loads content from file. And then, updates content of
;;;	parameter "International Series".
;;;
;;;Arguments
;;;	specific.type is a keyword.
;;;	filename is a pathname.
;;;
;;;Output
;;;	void
;;;
;;;Debug example:
;;;	(load.parameter.from.file :international.series "C:/home/lisp-sandbox/parameters.data")
;;;-----------------------------------------------------------------------------
(defmethod load.parameter.from.file ((specific.type (eql :international.series)) filename)
  (let* ((file.content (convert.parameters filename))
	 (value (getf file.content specific.type)))
    (set.international.series value))
  (values))
(defmethod load.parameter.from.file ((specific.type (eql :national.series)) filename)
  (let* ((file.content (convert.parameters filename))
	 (value (getf file.content specific.type)))
    (set.international.series value))
  (values))

