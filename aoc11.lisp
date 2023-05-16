;yeah, I'm using a library this time.
; I would do the pattern-matching myself, but right now I'm not in the mood 
; to reinvent the wheel (or regex)... so I'll just use cl-ppcre and call it a day.
; the challenge here is presumably not input-processing anyhow, so I don't think it matters. 
(ql:quickload :cl-ppcre)

; I'm not proud of this solution, except for this specific part:

;OKAY, so we have something genuinely interesting here. We're checking the "new" operation.
; You see, at first I was going to make a boring implementation where I just match
; the operation and check if the right variable was also "old"...
; But then I got an idea! what if I made good use of LISP macros and just... used the operation
; in the monkey description along with a macro to ACTUALLY CREATE A FUNCTION FROM IT!??!
; yes. yes it works. and it's amazing. I love LISP. I love Common Lisp. Thank you LISP. very cool.
; i really couldn't've done it without ya.
(defun match-func (str)
  "matches the actual operation thingy."
  (multiple-value-bind (s regs)
      (cl-ppcre:scan-to-strings "([^\ ]*) ([*+/-]) ([^\ ]*)" str)
    s
    regs))

(defun get-symbol-or-int (str)
  "takes a string, returns either a symbol representation or int."
  (handler-case (parse-integer str)
    (error ()
      (intern (string-upcase str)))))
(defun monkey-op (desc)
  "Creates an actual lambda function from a monkey's operation description."
  (let ((matches (match-func desc)))
    (eval
     `(lambda (old)
	(,(intern (aref matches 1))
	 ,(get-symbol-or-int (aref matches 0))
	 ,(get-symbol-or-int (aref matches 2)))))))


(defun match-nums (str)
  (remove "" (cl-ppcre:all-matches-as-strings "([0-9]*)" str)
	  :test #'string=))
(defun get-nums (str)
  (mapcar #'parse-integer (match-nums str)))
(defun parse-monkey (mon)
  "Parses a monkey to create a property list that holds the monkey data"
  (list
   ;:number (parse-integer (first (match-nums (first mon))))
   :items (get-nums (second mon))
   :operation (monkey-op (third mon))
   :test (first (get-nums (fourth mon)))
   :iftrue (first (get-nums (fifth mon)))
   :ifnil (first (get-nums (sixth mon)))
   :counter 0
   ))

(defun parse-input (input)
  (mapcar #'parse-monkey input))


(defun check (item monkey)
  (if (= (mod item (getf monkey :test)) 0)
      (getf monkey :iftrue)
      (getf monkey :ifnil)))

(defmacro add-to-list (i l)
  `(setf ,l (append ,l (list ,i))))

(defun turn (monkey monkeys divisor &optional (part2 nil))
  "the i'th monkey is the current monkey. Returns new monkey list. Very imperative and inelegant. Sad."
  (setf (getf monkey :items)
	(mapcar (lambda (worry)
		  (let ((res (funcall (getf monkey :operation) worry)))
		    (mod (if part2
			     res
			     (floor (/ res 3)))
			 divisor)))
		(getf monkey :items)))
  (loop for i = (pop (getf monkey :items))
	if i do (progn
		  (incf (getf monkey :counter))
		  (add-to-list i
			     (getf (nth (check i monkey) monkeys)
				   :items)))
	  else do (return)))

(defun monkey-round (monkeys divisor &optional (part2 nil))
  (mapcar (lambda (m)
	    (turn m monkeys divisor part2))
	  monkeys))

(defun max-2 (nums)
  (let ((m (sort nums #'>)))
    (list (first m)
	  (second m))))
(defun solve (monkeys &optional (part2 nil))
  (let ((divisor (reduce #'* (loop for i in monkeys collect (getf i :test)))))
    (dotimes (i (if part2 10000 20))
      (monkey-round monkeys divisor part2)))
  (reduce #'* (max-2 (loop for i in monkeys collect (getf i :counter)))))


(defun read-input (f)
  (with-open-file (st f)
    (labels ((r ()
	       (read-line st nil nil))
	     (c (lines line)
	       (if (or (string= line "")
		       (null line))
		   (reverse lines)
		   (c (cons line lines)
		      (r))))
	     (final (mons mon)
	       (if mon
		   (final (cons mon mons) (c nil (r)))
		   (reverse mons))))
      (final nil (c nil (r))))))
(defparameter *input* '(parse-input (read-input #p"./input-aoc11")))
(print (solve (eval *input*) nil))
(print (solve (eval *input*) t))
