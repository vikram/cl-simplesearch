(in-package :com.search)

(defmacro with-gensyms (syms &body body)
  "Create gensyms, useful for creating macros."   ; LMH
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defun mkstr (&rest args)
  "Make a string out of the printed representations of the arguments."   ; LMH
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Make a symbol out of the printed representations of the arguments."   ; LMH
  (values (intern (apply #'mkstr args))))

(defun make-logic-symbol (string)
  "Convert string to symbol, preserving case, except for AND/OR/NOT/FORALL/EXISTS."
  (cond ((find string '(and or not forall exists) :test #'string-equal))
        ((lower-case-p (char string 0)) 
	 (symb (string-upcase string)))
	((equal string "Nil") '|Nil|)
        (t (intern (string-upcase string)))))

(defun combinations (bag)
  (if (null bag)
      '(())
      (mapcan #'(lambda (e)
                  (mapcar #'(lambda (c) (cons e c))
                          (combinations (cdr bag))))
              (car bag))))

(defun permutations (bag)
  (mapcan #'(lambda (y)
              (mapcar #'(lambda (x) (list x y))
                      (remove y bag :test #'equal)))
          bag))

(defmacro with-collect ((&rest collectors) &body forms)
  "Evaluate forms, collecting objects into lists.
Within the FORMS, you can use local macros listed among collectors,
they are returned as multiple values.
E.g., (with-collect (c1 c2) (dotimes (i 10) (if (oddp i) (c1 i) (c2 i))))
 ==> (1 3 5 7 9); (0 2 4 6 8) [2 values]
In CLISP, push/nreverse is about 1.25 times as fast as pushing into the
tail, so this macro uses push/nreverse on CLISP and push into the tail
on other lisps (which is 1.5-2 times as fast as push/nreverse there)."
  #+clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors)))
    `(let (,@ret)
      (declare (list ,@ret))
      (macrolet ,(mapcar (lambda (co re) `(,co (form) `(push ,form ,',re)))
                         collectors ret)
        ,@forms
        (values ,@(mapcar (lambda (re) `(sys::list-nreverse ,re)) ret)))))
  #-clisp
  (let ((ret (mapcar (lambda (cc) (gensym (format nil "~s-RET-" cc)))
                     collectors))
        (tail (mapcar (lambda (cc) (gensym (format nil "~s-TAIL-" cc)))
                      collectors))
        (tmp (mapcar (lambda (cc) (gensym (format nil "~s-TMP-" cc)))
                     collectors)))
    `(let (,@ret ,@tail)
      (declare (list ,@ret ,@tail))
      (macrolet ,(mapcar (lambda (co re ta tm)
                           `(,co (form)
                             `(let ((,',tm (list ,form)))
                               (if ,',re (setf (cdr ,',ta) (setf ,',ta ,',tm))
                                   (setf ,',re (setf ,',ta ,',tm))))))
                         collectors ret tail tmp)
        ,@forms
        (values ,@ret)))))

(defmacro awhen (test-form &body body)
  `(let ((it ,test-form))
     (declare (ignorable it))
     (when it
       ,@body)))

(defun mapa-b (fn a b &optional (step 1))
 ; (declare (function fn) (fixnum a b step))   ; LMH
  "Apply the fn to the list of numbers a...b, stepping with step."  ; LMH
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
  ;  (declare (fixnum i))   ; LMH
    (push (funcall fn i) result)))

(defun map0-n (fn n)
  (declare (function fn) (fixnum n))   ; LMH
  "Apply the fn to the list of numbers 0...n."  ; LMH
  (mapa-b fn 0 n))

(defun map1-n (fn n)
;  (declare (function fn) (fixnum n))   ; LMH
  "Apply the fn to the list of numbers 1...n."  ; LMH
  (mapa-b fn 1 n))

(defun firstn (lst n)
  (if (or (null lst) (<= n 0))
      nil
      (cons (car lst)
            (firstn (cdr lst) (- n 1)))))

(defun s+ (&rest args)
  (declare (optimize (speed 3) (safety 0) (compilation-speed 0) (debug 0)))
  (apply #'concatenate 'string args))

(defun mappend (fn &rest lsts)
  (declare (function fn))   ; LMH
  "Nondestructive form of mapcan."   ; LMH
  (apply #'append (apply #'mapcar fn lsts)))

(defun map-adj (fn this adj rest list &optional (acc '()))
  (cond ((null list) (nreverse acc))
	((null (funcall rest (funcall rest list)))
	 (nreverse (cons (funcall fn (funcall this list) (funcall adj list))
			 acc)))
	(t (map-adj fn this adj rest (funcall rest list) 
		    (cons (funcall fn (funcall this list) (funcall adj list)) acc)))))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if: use `it' in then-form, else-form to
   refer to result of the test-form."   ; LMH
  `(let ((it ,test-form))
     (declare (ignorable it))		; LMH
     (if it ,then-form ,else-form)))

(defun hash-keys (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v) 
                 (declare (ignore v)) 
                 (push k acc))
             ht)
    (nreverse acc)))

(defun hash-vals (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (push v acc))
             ht)
    acc))

(defun hash-pairs (ht)
  (let ((acc nil))
    (maphash #'(lambda (k v)
                 (push (cons k v) acc))
             ht)
    acc))

(defun somehash (fn ht)
  (maphash #'(lambda (k v)
	       (declare (ignore k))
               (when (funcall fn v)
                 (return-from somehash v)))
           ht)
  nil)

(defun hash-nth (n ht)
  (gethash (nth n (hash-keys ht)) ht))

(defun hash-first (ht)
  (hash-nth 0 ht))

(defun hash-second (ht)
  (hash-nth 1 ht))

(defun hash-last (ht)
  (hash-nth (1- (hash-table-count ht)) ht))

(defun hash-table->alist (ht)
  "Return the alist with the same data as the hash-table.
Actually, the first element is the test: '(eql (key0 . val0) (key1 . val1)).
The inverse is `alist->hash-table'."
  (declare (hash-table ht))
  (cons (hash-table-test ht)
        (with-collect (co)
          (with-hash-table-iterator (iter ht)
            (loop (multiple-value-bind (re kk vv) (iter)
                    (unless re (return))
                    (co (cons kk vv))))))))

(defun alist->hash-table (alist &optional (value-fn #'identity))
  "Return the new hash-table based on this alist.
The inverse is `hash-table->alist'."
  (declare (list alist))
  (let ((ht (make-hash-table :test (car alist))))
    (dolist (co (cdr alist) ht)
      (setf (gethash (car co) ht) (funcall value-fn (cdr co))))))

(defmethod print-object ((ht hash-table) (out stream))
  (if *print-readably*
      (format out "~s" (hash-table->alist ht))
      (call-next-method)))

; Hash Tables

(defun nonempty-ht (ht)
  (maphash #'(lambda (k v) (declare (ignore k v)) (return-from nonempty-ht t))
           ht)
  nil)

(defun ht-car (ht)
  (maphash #'(lambda (k v) (declare (ignore k)) (return-from ht-car v))
           ht))
     
(defun key-match (ht1 ht2)
  (maphash #'(lambda (k v)
               (declare (ignore v))
               (when (gethash k ht2)
                 (return-from key-match k)))
           ht1)
  nil)

(defun write-hashtable-stream (stream ht)
  (when ht
    (maphash #'(lambda (key value)
		 (print (cons key value) stream)) 
	     ht)))

(defun write-hashtable (file ht)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (write-hashtable-stream stream ht)))

(defun read-hashtable-stream (stream)
  (let* ((ht (make-hash-table :test #'equal)))
    (loop for line = (read stream nil nil)
       until (null line)
       do
	 (setf (gethash (car line) ht) (cdr line)))
    ht))

(defun read-hashtable (file)
  (with-open-file (stream file :direction :input)
    (read-hashtable-stream stream)))

(defmacro hash (&key (test nil) (rehash-size 10) (keyvals nil) (hash nil))
  ;--------------------------
  ; Return a new hash table.
  ;--------------------------
  `(progn
    (let ((h (make-hash-table ,@(when test (list :test test)) ,@(list :rehash-size rehash-size))))
       (when ,keyvals
	 (hash-populate h ,keyvals))
       (when ,hash
	 (do-hash (k v ,hash) (hash-put h k v)))
       h)))

(defmacro hash-populate (hash kvlist)
  ;-----------------------------
  ; Insert KEY, VALUE elements 
  ;      from 2-element KVLIST
  ; Return Hash
  ; ----------------------------
  `(progn
     (dolist (kv ,kvlist)
       (hash-put ,hash (first kv) (second kv)))
     ,hash))

(defmacro hash-put (hash key value)
  ;-----------------------------
  ; Insert KEY-VALUE into HASH.
  ; Return HASH.
  ;-----------------------------
  `(progn
     (setf (gethash ,key ,hash) ,value)
     ,hash))

(defmacro hash-get (hash key)
  ;-----------------------------
  ; Get KEY-VALUE into HASH.
  ; Return HASH.
  ;-----------------------------
  `(progn
     (gethash ,key ,hash)
     ))

(defmacro do-hash ((k v hash) &body body)
  ;-------------------------------------------------------
  ; Iterate over elements of HASH.  On each iteration
  ; evaluate BODY with ELEM bound to the current element.
  ;-------------------------------------------------------
  `(maphash
    (lambda (,k ,v)
      ,@body)
    ,hash))

;; Returns a new hashset that is a union of the two
(defmacro hashset-union (hset1 hset2)
  `(let ((h (hash :test (hash-table-test ,hset1) :hash ,hset1)))
     (maphash #'(lambda (k v)
		  (hash-put h k v))
	      ,hset2)
     h))

;; Returns a new hashset that is a union of the two (destructive version)
(defmacro nhashset-union (hset1 hset2)
  `(progn
    (maphash #'(lambda (k v)
		 (hash-put ,hset1 k v))
	     ,hset2)
     ,hset1))

;; Returns a new hashset that is the intersection
(defmacro hashset-intersection (hset1 hset2)
  `(let* ((h (hash :test (hash-table-test ,hset1)))
	  (h1 (if (> (hash-table-size ,hset1) (hash-table-size ,hset2)) ,hset1 ,hset2))
	  (h2 (if (eq ,hset1 h1) ,hset2 ,hset1)))
     (maphash #'(lambda (k v)
		  (if (hash-get h1 k)
		      (hash-put h k v)))
	      h2)
     h))
  
(defmacro hashset-difference (hset1 hset2)
  `(let ((h (hash :test (hash-table-test ,hset1))))
     (maphash #'(lambda (k v)
		  (unless (hash-get ,hset2 k) 
		    (hash-put h k v)))
	      ,hset1)
     h))

(defun hash-empty? (hash)
  ;-------------------------
  ; True iff HASH is empty.
  ;-------------------------
  (null (hash-keys hash)))


(defun make-counter ()
  (hash :test 'equal))

(defun incf-counter (h key)
  (if (gethash key h)
      (incf (gethash key h))
      (setf (gethash key h) 1)))

(defun make-hash ()
  (make-hash-table :test 'equal))
