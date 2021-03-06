(in-package :com.search)

(defun checkpoint-file-reader (filename
			       &key (base-folder)
			       (delimiter (cl-ppcre:create-scanner '(:char-class #\return #\newline #\linefeed))) (buffer-size 512000) 
			       (restart-p nil) (abort-after nil))
  """Reads a file and breaks it up based on the delimiter.
   It remembers the last point until which it has read.
   API------>
   (let ((reader (checkpoint-file-reader "document-file-name"))
         (doc nil))
     (loop
       (setf doc (funcall reader))
       (when (or (not doc) (and (stringp doc) (string= doc "")))
         (return 'done))
       (progn
         do stuff)))
   """
  (let ((seq (make-array buffer-size :element-type 'character
			 :adjustable t
			 :fill-pointer buffer-size))
	(last-line nil)
	(lines '())
	(i 0)
	(checkpoints-folder (s+ base-folder "checkpoints/")))
    (labels ((checkpoint (filename &optional (pos nil))
	       (if pos
		   (with-open-file (out (s+ checkpoints-folder (make-hash-code filename))
					:direction :output :if-exists :supersede :if-does-not-exist :create)
		     (write pos :stream out))
		   (if (probe-file (s+ checkpoints-folder (make-hash-code filename)))
		       (with-open-file (in (s+ checkpoints-folder (make-hash-code filename)))
			 (read in))
		       0))))
      (ensure-directories-exist checkpoints-folder)
      (when restart-p
	(checkpoint filename 0))
      #'(lambda ()
	  (if (null lines)
	      (with-open-file (in filename)
		(print 'cp-reading)
		(file-position in (checkpoint filename))
		(setf (fill-pointer seq) (read-sequence seq in)) 
		(if (or (zerop (fill-pointer seq)))
		    (progn
		      (checkpoint filename (file-length in))
		      "")
		    (progn       
		      (setf lines (cl-ppcre:split delimiter seq))
		      (awhen (first lines)
			(setf last-line (car (last lines)))
			(setf lines (cdr (butlast lines)))
			(checkpoint filename (- (+ (checkpoint filename) (* (1+ i) buffer-size)) (length last-line)))
			(print (cons 'read it))
			(if (and abort-after (= (incf i) abort-after))
			    ""
			    it)))))
	      (awhen (first lines)
		(setf lines (rest lines))
		it))))))

;;;should implement these as a stream interface then 
;;;with-open-file will just work properly.....and so
;;;would read on the stream.
;;;the current interface sucks

;;;make-indexer gives the object
;;;when making 
