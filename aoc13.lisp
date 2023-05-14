; clean functional
; NOTE: a lot of the code here is just for parsing the text input.
; furthermore, much of it is horrible code, if maybe technically functional (well, not really...)
; I know I could've used a regular expressions library, but I wanted to challenge myself with this task.
; therefore, I tried to implement every function I needed in a way I haven't done before instead of relying on a library,
; and as a result of that effort, some code may have been unnecessary, impractical, or plain inefficient.
(defmacro do-first-n (list n yes-form no-form)
  `(labels ((self (list n)
	      (if (> n 0)
		  ,yes-form
		  ,no-form)))
     (self ,list ,n)))
(defun group (list n)
  "Group every n element in list
   NOTE: I do not know why I created this abomination. I know I shouldn't have used a macro here. But it just seemed like a golden opportunity. I don't know why or even how it turned out this bad. I just know that it did, and it *works*."
  (labels ((f (list n)
	     (if list
		 (cons (do-first-n list n
			 (cons (car list) (self (cdr list) (1- n)))
			 nil)
		       (f (do-first-n list n
			    (self (cdr list) (1- n))
			    list)
			  n)))))
    (f list n)))

(defmacro letif (form else)
  (let ((var (gensym)))
    `(let ((,var ,form))
       (if ,var
	   ,var
	   ,else))))
(defun cut-str (str char)
  "Gets the substring from the start until the first occurance of char"
  (subseq str 0 (letif (position char str)
		       (length str))))

(defun split-str (str char)
  "Except this time it includes the split characters."
  (labels ((sub (str pos)
	     (if (and pos (not (string= str "")))
		 (cons (subseq str 0 pos)
		       (cons char
			     (sub (subseq str (1+ pos)) (position char (subseq str (1+ pos))))))
		 (cons str nil))))
    (sub str (position char str))))

(defun flatten (list)
  "flattens a list one level."
  (if list
      (if (listp (car list))
	  (append (car list)
		  (flatten (cdr list)))
	  (cons (car list)
		(flatten (cdr list))))
      nil))

(defmacro general-case (test key &body cases)
  "I've been writing similar macros for so long that I feel the need for this."
  (unless (null cases)
  `(if (or (,test ,key ,(caar cases))
	   (eq ,key t))
       (progn ,@(cdar cases))
       (general-case ,test ,key ,@(cdr cases)))))

(defmacro char-case (key &body cases)
  `(general-case CHAR= ,key ,@cases))

(defun parse-input (input)
  "Groups a list of lines so we can make our comparisons."
  (group (remove "" input :test #'string=) 2))

(defun process-list (str)
  "parses a list in brackets and comma notation, to an actual lisp list.
   Beware, there be dragons 'ere."
  (let* ((no-commas (remove #\, (split-str str #\,)))
	 (no-brackets1 (remove "" (flatten (mapcar (lambda (x) (split-str x #\[)) no-commas))
			       :test #'string=))
	 (final (remove "" (flatten (mapcar (lambda (x)
					      (if (stringp x)
						  (split-str x #\])
						  x))
					    no-brackets1))
			:test #'string=)))
    (labels ((p1 (item col)
	       (if item
		   (cond
		     ((stringp (car item)) ; if the first element is a string, we can just parse and collect it.
		      (p1 (cdr item) (cons (parse-integer (car item)) col)))
		     (t (char-case (car item)
			  (#\[ ;if it's an opening bracket, we call ourselves again.
			   ; the closing bracket returns where we left off, and what we collected.
			   ; this way we can take the "collected" list and collect *that* list here.
			   (multiple-value-bind (collected rest)
			       (p1 (cdr item) nil)
			     (p1 rest
				 (cons (reverse collected)
				       col))))
			   (#\]; closing bracket: we return the items we collected so far + where we left off.
			    (values col (cdr item))))))
		   col)))
      (car (p1 final nil)))))

(defun process-input-lists (input)
  "Turns the string representation of the lists into actual LISP lists."
  (mapcar (lambda (x)
	    (mapcar #'process-list x))
	  input))

(defun xor (p1 p2)
  "Returns 1 if only one predicate is true, 0 if both or neither is true."
  (if (and p1 p2)
      nil
      (if (and (not p1) (not p2))
	  nil
	  t)))

(defun handle-special (i1 i2)
  "Handle the special case for comparison where only one item is a list"
  (if (listp i2)
      (list (list i1) i2)
      (list i1 (list i2))))

(defun comparison (list1 list2)
  "Returns 'left if list1 < list2 (complying with the problem text)
   'right otherwise, and nil if they're equal."
  (cond
    ((and (null list1) (null list2)) nil)
    ((null list1) 'left)
    ((null list2) 'right)
    ;one list and one number
    ((xor (listp (car list1)) (listp (car list2)))
     (or (apply #'comparison (handle-special (car list1) (car list2)))
	 (comparison (cdr list1) (cdr list2))))
    ; both lists
    ((and (listp (car list1)) (listp (car list2)))
     (or (comparison (car list1) (car list2))
	 (comparison (cdr list1) (cdr list2))))
    ; both numbers
    (t (cond
	 ((< (car list1) (car list2)) 'left)
	 ((> (car list1) (car list2)) 'right)
	 (t (comparison (cdr list1) (cdr list2))))) 
    ))

(defun l< (list1 list2)
  "less than operator for the lists defined in the problem statement."
  (eq (comparison list1 list2) 'left))
(defun l> (list1 list2)
  (eq (comparison list1 list2) 'right))

(defun part1 (input)
  (apply #'+ (loop for i in (mapcar (lambda (x)
				      (apply #'comparison x))
				    (process-input-lists (parse-input input)))
		   for j upfrom 1
		   if (eq i 'left) collect j)))


(defun part2 (input)
  "okay, I assigned *one* variable here, but that's just making sure I don't execute the same form twice! I swear!"
  (let ((res (sort (append '(((2)) ((6)))
				  (flatten (process-input-lists (parse-input input))))
		   #'l<)))
    (* (+ (position '((2)) res :test 'equal) 1)
       (+ (position '((6)) res :test 'equal) 1))))

; dirty imperative
(defun read-input (f)
  (with-open-file (st f)
    (loop for i = (read-line st nil nil)
	  while i collect i)))

(defparameter *input* (read-input "input-aoc13"))
(print (part1 *input*))
(print (part2 *input*))
