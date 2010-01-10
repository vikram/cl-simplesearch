(in-package :com.search)

(defmacro make-md5 (from to-list subject)
  (let ((to (gensym)) (rest (gensym)))
    `(let ((,to ,to-list)
	   (,rest (list ,from ,subject)))
       (format nil "~(~{~2,'0X~}~)" 
	       (map 'list #'identity 
		    (md5:md5sum-sequence 
		     (apply #'concatenate 'string (append ,rest ,to))))))))

(defun make-hash-code (string)
  (format nil "~(~{~2,'0X~}~)" 
	  (map 'list #'identity (md5:md5sum-sequence string))))

(defun CONVERT-INTEGER-TO-BIT-STRING (integer)
  (declare (fixnum integer)
	   (optimize (speed 3) (safety 0)))
   "Convert the integer to a bit string"
   (if (< integer 1) '(0)
   (loop with number = integer
       for 2-power = (biggest-2-divisor integer)
           then (/ 2-power 2)
       until (< 2-power 1)
       collect (if (< number 2-power) 0
                   (and (setf number (- number 2-power)) 1))
       into bits
      finally (return bits))))
 
(defun padd-bits (bits pad-upto)
  (declare (list bits) (fixnum pad-upto)
	   (optimize (speed 3) (safety 0)))
  (padd-these-bits (length bits) bits pad-upto))

(defun padd-these-bits (num bits pad-upto)
  (declare (list bits) (fixnum pad-upto num)
	   (optimize (speed 3) (safety 0)))
  (if (< num pad-upto)
      (padd-these-bits (incf num) (cons 0 bits) pad-upto)
      bits))

(defun BIGGEST-2-DIVISOR (integer)
  (declare (fixnum integer)
	   (optimize (speed 3) (safety 0)))
   "Find the larger power of 2 in the integer"
   (loop for 2-power = 1 then (the fixnum (+ 2-power 2-power))
       until (> 2-power integer)
       finally (return (/ 2-power 2))))
 
(defun CONVERT-BIT-STRING-TO-INTEGER (bit-string)
  (declare (list bit-string)
	   (optimize (speed 3) (safety 0)))
   "Convert the bit string to an integer using powers of 2"
   (loop for bit in (reverse (copy-list bit-string))
       for 2-power = 1 then (+ 2-power 2-power)
       summing (the fixnum (if (= bit 1) 2-power 0))))

(defun encode-gamma (integer)
  (if (= 0 integer)
      '(0)
      (let* ((code-length (truncate (log integer 2)))
	     (unary (encode-unary (1+ code-length)))
	     (gamma (padd-bits (convert-integer-to-bit-string (- integer (expt 2 code-length))) code-length)))
	(append unary gamma))))

(defun encode-delta (integer)
  (if (= 0 integer)
      '(0)
      (let* ((code-length (truncate (log integer 2)))
	     (gamma (encode-gamma (1+ code-length)))
	     (delta (padd-bits (convert-integer-to-bit-string (- integer (expt 2 code-length))) code-length)))
	(append gamma delta))))

(defun encode-unary (integer)
  (let ((results '(0)))
    (dotimes (i (1- integer))
      (push 1 results))
    results))

(defun bits->bytes (bits &optional (bytes '()))
  (if (null bits)
      bytes
      (bits->bytes (nthcdr 8 bits) (cons (convert-bit-string-to-integer (firstn bits 8)) bytes))))

(defun bytes->bits (bytes &optional (len (length bytes)))
  (let ((bits '()))
    (dotimes (i len)
      (dolist (bit (padd-bits (convert-integer-to-bit-string (aref bytes i)) 8))
	(push bit bits)))
    (nreverse bits)))

(defun encode-unary-other (integer)
  (let ((results '(1)))
    (dotimes (i (1- integer))
      (push 0 results))
    results))

(defun encode-variable-bytes (integer)
  (let* ((code (convert-integer-to-bit-string integer))
	 (count (length code))
	 (len (+ count (- 7 (mod count 7))))
	 (results '())
	 (main '())
	 (j 0))
    (dolist (i (padd-bits code len))
      (push i results)
      (when (zerop (mod (incf j) 7))
	(if (> len j)
	    (push 1 results)
	    (push 0 results))))
    (setf main (nreverse results))
    (map1-n #'(lambda (n) (convert-bit-string-to-integer (subseq main (* 8 (1- n)) (* 8 n))))
	    (/ len 7))))

(defun decode-variable-bytes (bytes &optional (acc '()))
  (if bytes
      (awhen (convert-integer-to-bit-string (first bytes))
	(if (eq (car (last it)) 1)
	    (decode-variable-bytes (rest bytes) (cons (padd-bits (butlast it) 7) acc))
	    (convert-bit-string-to-integer (apply #'append (nreverse (cons (padd-bits (butlast it) 7) acc))))))
      acc))
