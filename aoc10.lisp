;clean functional
(defun split-str (str char)
  (labels ((sub (str pos)
	     (if (and pos (not (string= str "")))
		 (cons (subseq str 0 pos)
		       (sub (subseq str (1+ pos)) (position char (subseq str (1+ pos)))))
		 (cons str nil))))
    (sub str (position char str))))

(defmacro str-case (key &body cases)
  (unless (null cases)
    `(if (string= ,key ,(caar cases))
	 (progn
	   ,@(cdar cases))
	 (str-case ,key ,@(cdr cases)))))

(defun parse-input (input)
  (mapcar (lambda (x)
	    (split-str x #\Space))
	  input))

(defun execute-instructions (inss)
  ;(declare (optimize (debug 0) (safety 0) (speed 3)))
  (labels ((r (cur-x xs clock inss) ; here xs is a list of all previous x values. The first element is the most recent.
	     (if (null inss)
		 (cons cur-x xs)
		 (str-case (caar inss)
			   ("noop"
			    (r cur-x (cons cur-x xs) (+ clock 1) (cdr inss)))
			   ("addx"
			    (r (+ cur-x (parse-integer (cadar inss)))
			       (cons cur-x (cons cur-x xs))
			       (+ clock 2)
			       (cdr inss)))))))
    (reverse (r 1 nil 1 inss))))

(defun get-pixel (clock x)
  (<= (abs (- x clock)) 1))

(defun render-image (xs)
  "xs is the output of execute-instructions - a log of what X is at each clock cycle, with index 0 being cycle 1."
  (labels ((f (clock xs)
	     (if xs
		 (cons (get-pixel (mod (- clock 1) 40) (car xs))
		       (f (1+ clock) (cdr xs))))))
    (f 1 xs)))

(defun group (list n)
  "Divides list into groups of n items"
  (labels ((cdrer (l n)
	     (if (> n 0)
		 (cdrer (cdr l) (1- n))
		 l))
	   (collector (l n)
	     (if (> n 0)
		 (cons (car l) (collector (cdr l) (1- n)))
		 nil))
	   (f (l n)
	     (if l
		 (cons (collector l n)
		       (f (cdrer l n) n))
		 nil)))
    (f list n)))

(defun print-screen (scn)
  (loop for i in scn
	do (format t "~c" (if i #\# #\.)))
  (terpri))

(defun part1 (input)
  (apply #'+
	 (loop for i in (execute-instructions input)
	       for j upfrom 1
	       if (= (mod (- j 20) 40) 0) collect (* i j))))

(defun part2 (input)
  (mapcar #'print-screen
	  (group (render-image (execute-instructions input))
		 40)))

;dirty imperative
(defun read-input (f)
  (with-open-file (st f)
    (labels ((r ()
	       (read-line st nil nil))
	     (f (lines line)
	       (if line
		   (f (cons line lines) (r))
		   (nreverse lines))))
      (f nil (r)))))

(defparameter *input* (parse-input (read-input "input-aoc10")))
(print (part1 *input*))
(terpri)
(part2 *input*)
