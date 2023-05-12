;clean
(defun nth-block (n line)
  "nth block in given line of initial state"
  (aref line (- (* (- (* n 2) 1) 2) 1)))
(defun range (n1 n2)
  (if (>= n1 n2)
      (cons n1 nil)
      (cons n1 (range (1+ n1) n2))))
(defun parse-initial-state (state-lines)
  "Parse the initial state of the blocks (each block represented as a character)."
  (mapcar (lambda (n)
	    (remove #\Space
		    (butlast (mapcar (lambda (str) (nth-block n str))
				     state-lines))))
	  (range 1 9)))

(defun split-string (str char)
  "Splits the string at every occurance of char"
  (if (not (position char str))
      (unless (string= str "")
	(cons str nil))
      (cons (subseq str 0 (position char str))
	    (split-string (subseq str (1+ (position char str))) char))))

(defun mapcadr (function list)
  "Mapcar, but skip every other element."
  (if list
      (cons (funcall function (car list))
	    (mapcadr function (cdr (cdr list))))))

(defun parse-instructions (ins)
  "Parses the instructions to the format of (amount from to)"
  (mapcar (lambda (i)
	    (mapcar #'parse-integer
		    (mapcadr #'identity (cdr (split-string i #\Space)))))
	  ins))

(defun push-reverse-list (objects place)
  "Less dirty now"
  (if objects
      (progn
	(push (first (last objects)) place)
	(push-reverse-list (butlast objects) place))
      place))



(defun perform-instruction (instruction state)
  (dotimes (i (first instruction))
    (push (pop (nth (1- (second instruction)) state))
	  (nth (1- (third instruction)) state)))
  state)

(defun perform-part2 (instruction state)
  (labels ((pop-n (n) "Pop n times from the current 'from' place."
	     (if (> n 0)
		 (cons (pop (nth (1- (second instruction)) state))
		       (pop-n (1- n)))))
	   (push-list (list)
	     (when list
	       (push (car list)
		     (nth (1- (third instruction)) state))
	       (push-list (cdr list)))))
    (push-list (reverse (pop-n (first instruction))))
    state))

(defun perform-all (instructions state perform-function)
  (if instructions
      (perform-all (cdr instructions)
		   (funcall perform-function (car instructions) state)
		   perform-function)
      state))

(defun perform-all-part1 (instructions state)
  (perform-all instructions state #'perform-instruction))

(defun perform-all-part2 (instructions state)
  (perform-all instructions state #'perform-part2))

;dirty
(defmacro do-twice (form)
  "Evaluates the same form twice, puts the return values in a list"
  `(append (list ,form) (list ,form)))
(defun read-all ()
  (with-open-file (fstr #p"./input-aoc5")
    (labels ((keep-reading (line)
	       (if (or (null line) (string= line ""))
		   nil
		   (cons line (keep-reading (read-line fstr nil nil))))))
      (do-twice (keep-reading (read-line fstr nil nil))))))

(defparameter *full-input* (read-all))
(defparameter *initial-state* (parse-initial-state (first *full-input*)))
(defparameter *instructions* (parse-instructions (second *full-input*)))

(print
 (concatenate 'string (mapcar #'first
			      (perform-all-part1 (copy-list *instructions*)
					 (copy-list *initial-state*)))))

(print
 (concatenate 'string (mapcar #'first
			      (perform-all-part2 (copy-list *instructions*)
						 (copy-list *initial-state*)))))
