(ql:quickload :cl-ppcre)

; still not proud of it, but now it should be a little better.

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

(defun monkey-round (monkeys divisor &optional (part2 nil))
  "This has some imperative code. For this problem, a purely functional approach didn't make much sense..."
  (labels ((inspect-i (i m) "inspects item, returns new worry"
	     (mod (floor (/ (funcall (getf m :operation) i)
			    (if part2 1 3)))
		  divisor))
	   (throw-i (i m) "throws item to the apropriate monkey"
	     (add-to-list i (getf (nth (check i m) monkeys) :items)))
	   (f (m)
	     (mapcar (lambda (i) (throw-i i m))
		     (mapcar (lambda (i) (inspect-i i m)) (getf m :items)))
	     (incf (getf m :counter) (length (getf m :items)))
	     (setf (getf m :items) nil)))
    (mapcar #'f monkeys)))

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
