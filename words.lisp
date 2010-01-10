(in-package :com.search)

(defparameter *whitespace-chars* 
  '( #\tab #\space #\newline #\return #\linefeed #\page #\tab))

(defparameter *punctuation-chars* 
  '(#\& #\^ #\+ #\' #\_ #\- #\/ #\\ #\* #\% #\, #\. #\) #\( #\; #\: #\? #\! #\@))

(defparameter *funny-chars*
  (append *punctuation-chars* *whitespace-chars*))

(defun whitespacep (char)
  (find char *whitespace-chars*))

(defmacro replace-expander (lst str)
  (reduce #'(lambda (x y) 
	      `(cl-ppcre:regex-replace-all ,(car y) ,x ,(cdr y)))
	  `,lst
	  :initial-value str))

(defun trim-string (char-list string)
  (replace-expander (((cl-ppcre:create-scanner
		       (list :sequence
			     :START-ANCHOR 
			     (list :GREEDY-REPETITION 1 NIL `(:char-class ,@char-list))))
		      . "")
		     ((cl-ppcre:create-scanner
		       (list :sequence
			     (list :greedy-repetition 1 nil `(:char-class ,@char-list))
			     :end-anchor)) 
		      . ""))
		    string))

(defun funnies (punctuation)
  (cl-ppcre:create-scanner 
   (list :greedy-repetition 1 nil 
	 (append `(:char-class ,@*whitespace-chars*)
		 (if punctuation
		     *punctuation-chars*
		     nil)))))

(defparameter *funny-punc-scanner* (funnies t))
(defparameter *funny-scanner* (funnies nil))

(defmacro defscan (name string)
  (with-gensyms (str)
    `(progn
       (defparameter ,name
	 (cl-ppcre:create-scanner ,string))
       (defun ,(make-logic-symbol (subseq (symbol-name name) 1 (1- (length (symbol-name name))))) (,str)
	 (cl-ppcre:all-matches-as-strings ,name ,str)))))

(defscan *alpha-p* "[a-zA-Z]")
(defscan *numeric-p* "[0-9]")

(defscan *punctuation*
    (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*punctuation-chars*)))

(defscan *whitespace-scanner*
    (list :sequence
	  (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*whitespace-chars*))))

(defscan *word-punc-scanner*
    (list :alternation
	  (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*punctuation-chars*))
	  (list :greedy-repetition 1 nil `(:char-class (:RANGE #\a #\z)
						       (:RANGE #\A #\Z)
						       (:RANGE #\0 #\9)))))

(defscan *html-scanner*
    (list :alternation
	  (list :sequence
		(list :greedy-repetition 0 1 ":")
		(list :greedy-repetition 1 nil `(:char-class (:RANGE #\a #\z)
							     (:RANGE #\A #\Z)
							     (:RANGE #\0 #\9))))
	  (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*punctuation-chars*))))

(defscan *html-scanner-preserve-space*
    (list :alternation
	  (list :sequence
		(list :greedy-repetition 1 1 ":")
		(list :greedy-repetition 1 nil `(:char-class (:RANGE #\a #\z)
							     (:RANGE #\A #\Z)))
		(list :greedy-repetition 0 nil `(:char-class (:RANGE #\0 #\9))))
	  (list :sequence
		(list :greedy-repetition 1 nil `(:char-class (:RANGE #\a #\z)
							     (:RANGE #\A #\Z)
							     (:RANGE #\0 #\9))))
	  (list :greedy-repetition 1 nil " ")
	  (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*punctuation-chars*))))

(defscan *tag-scanner*
      (list :sequence
		(list :greedy-repetition 0 1 ":")
		(list :greedy-repetition 1 nil `(:char-class (:RANGE #\a #\z)
							     (:RANGE #\A #\Z)
							     (:RANGE #\0 #\9)))))

(defun trim (string)
  (trim-string *whitespace-chars* string))

(defun trim-funny (string)
  (replace-expander (((cl-ppcre:create-scanner
		       (list :sequence
			     :START-ANCHOR 
			     (list :GREEDY-REPETITION 1 NIL `(:char-class ,@*funny-chars*))))
		      . "")
		     ((cl-ppcre:create-scanner
		       (list :sequence
			     (list :greedy-repetition 1 nil `(:char-class ,@*funny-chars*))
			     :end-anchor)) 
		      . ""))
		    string))

(defscan *more-spaces* "[ ]+")

(defparameter *space* " ")

(defun rep (string pattern replace-with)
  (cl-ppcre:regex-replace-all pattern string replace-with))

(defun replace-funny (string &key (punctuation t) (with *space*))
  (trim-funny 
   (rep string 
	(if punctuation
	    *funny-punc-scanner*
	    *funny-scanner*)
	with)))

(defun first-string< (x y)
  (string< 
   (first x) 
   (first y)))

(defun just-words (tokens)
  (mapcar #'car tokens))

(defparameter *word-money-scanner*
  (cl-ppcre:create-scanner "\\b[\\\.\\\-a-zA-Z0-9,]+\\b" :multi-line-mode t))

(defparameter *word-scanner*
  (cl-ppcre:create-scanner "\\b[a-zA-Z0-9]*\\b" :multi-line-mode t))

(defun split-text (text)
  (cl-ppcre:all-matches-as-strings
   *word-money-scanner*
   text))

(defun whitespace-splitter (str)
  (cl-ppcre:split *whitespace-scanner* str))

(defun cleanup-text (string &key (upcase t) (punctuation t))
  (trim
   (cl-ppcre:regex-replace-all
    (cl-ppcre:create-scanner
     '(:register
       (:GREEDY-REPETITION 1 NIL (:CHAR-CLASS #\ ))))
    (when-bind (x (if upcase
		      (string-upcase string)
		      string))
      (if punctuation
	  (cl-ppcre:regex-replace-all
	   (cl-ppcre:create-scanner 
	    '(:register
	      (:char-class #\Newline #\linefeed #\return #\tab #\space #\. #\, #\) #\( #\; #\: #\? #\! #\@ #\& #\^ #\* #\+ #\' #\_ #\- #\/ #\\ #\&)))
	   x
	   " ")
	  (cl-ppcre:regex-replace-all
	   (cl-ppcre:create-scanner 
	    "\\\\")
	   x
	   "")))
    " ")))

(defun tokenizer (scanner)
  #'(lambda (doc)
      (let ((pos 0))
	(sort 
	 (mapcar 
	  #'(lambda (term) (cons term (incf pos)))
	  (cl-ppcre:all-matches-as-strings 
	   scanner
	   (cleanup-text doc)))
	 #'first-string<))))

(defmacro deftokenizer (name scanner)
  `(setf (symbol-function ',name)
	 (tokenizer ,scanner)))
  
(deftokenizer word-tokenizer *word-scanner*)
(deftokenizer word-money-tokenizer *word-money-scanner*)
(deftokenizer indexable-words "\\b[a-zA-Z0-9]+\\b")
(deftokenizer word-punc-tokenizer *word-punc-scanner*)

(defun word-punc-splitter (str)
  (cl-ppcre:all-matches-as-strings *word-punc-scanner* str))

(defun indexable-words-ranked (string-list)
  (let ((i 0))
    (mappend 
     #'(lambda (string-pair)
	 (incf i)
	 (mapcar 
	  #'(lambda (pair) (cons (car pair) i)) 
	  (indexable-words (cdr string-pair))))
     string-list)))

;;aserve
(defvar *max-word* 25)
(defparameter *english-stop-words*
  '("a" "an" "and" "are" "as" "at" "be" "but" "by" "for" "if" "see" "do" "has" "so" "why" "how" "www" "am"
    "in" "into" "is" "it" "no" "not" "of" "on" "or" "s" "such" "my" "can" "just" "no" "what" "me" "when"
    "t" "that" "the" "their" "then" "there" "these" "i" "you" "we" "our" "some" "have" "all" "which" "org"
    "they" "this" "to" "was" "will" "with" "its" "like" "com" "http" "co" "your" "from"))
(defvar ch-alpha 0)
(defvar ch-space 1)
(defvar ch-sep   2)  ; separators

(defvar *syntax-table*
    (let ((arr (make-array 100000 :initial-element ch-alpha)))
      
      ; the default so we don't have to set it
      #+ignore (do ((code (char-code #\!) (1+ code)))
	  ((> code #.(char-code #\~)))
	(setf (svref arr code) ch-alpha))
      
      (setf (svref arr (char-code #\space)) ch-space)
      (setf (svref arr (char-code #\Page)) ch-space)
      (setf (svref arr (char-code #\tab)) ch-space)
      (setf (svref arr (char-code #\return)) ch-space)
      (setf (svref arr (char-code #\linefeed)) ch-space)
       arr))

(defun stop-word-p (word)
  (declare (simple-base-string word) (cons *english-stop-words*) (optimize (safety 0) (speed 3)))
  (position word *english-stop-words* :test #'(lambda (x y) (string= (string-downcase x)
								     y))))

(cl-ppcre:define-parse-tree-synonym alpha
    (:CHAR-CLASS (:RANGE #\a #\z) (:RANGE #\A #\Z)))

(defparameter *letter-scanner* 
  (cl-ppcre:create-scanner "."))

(defun letter-tokenizer (string)
  (cl-ppcre:all-matches-as-strings *letter-scanner* string))

(defun letters-p (word)
  (cl-ppcre:scan *letter-scanner* word))

(defun freq (lst)
  (let ((V (make-counter)))
    (dolist (elem lst)
      (incf-counter v elem))
    V))

(defun word-stats (word-list)
  (let ((words (freq word-list)))
    (declare (optimize (speed 3) (safety 0) (space 0))
             (sequence word-list) (hash-table words))
    (cdr (hash-table->alist words))))

(defun make-word-list (string)
  (word-tokenizer string))
