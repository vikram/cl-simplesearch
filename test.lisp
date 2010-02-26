(in-package :com.search)

(defun test-new-reader ()
  (let ((doc "") (docid 0) (reader (checkpoint-file-reader (s+ *data-folder* "addresses.1000") :restart-p t)))
    (loop
       (setf doc (funcall reader))
       (when (string= doc "")
	 (return))
       (progn
	 (incf docid)
	 (print (cons docid doc))))))

