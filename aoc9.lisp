(defmacro char-case (char &body cases)
  "Like CASE except works with characters."
  `(if (CHAR= ,char ,(caar cases))
       (progn
	 ,@(cdar cases))
       ,(unless (null (cdr cases))
	  `(char-case ,char ,@(cdr cases)))))

(defun parse-input (line)
  "parses an input line into a list we can actually use."
  (list
   (char-case (aref line 0)
     (#\L 'left)
     (#\R 'right)
     (#\U 'up)
     (#\D 'down))
   (parse-integer (subseq line 2))))

(defun make-position (&optional (head '(0 0)) (tail '(0 0)))
  "Since keeping a 2D array for the grid is both memory-inefficient and unnecessary (since only the head and tail exist anyway...) we're just keeping the coordinates of the head and tail in a property list as 'state'"
  (list head tail))

(defun make-general-state (n)
  (loop for i below n collect '(0 0)))

(defun too-far-p (point1 point2)
  "Is the distance between head and tail big enough to make the tail move? list of coordinates for both parameters."
  (reduce (lambda (p1 p2) (or p1 p2))
	  (mapcar (lambda (x1 x2) (> (abs (- x1 x2)) 1))
		  point1 point2)))

(defun add-vecs (vec1 vec2)
  (mapcar #'+ vec1 vec2))


(defun move-point (point direction)
  (add-vecs point
	  (case direction
	    (left '(-1 0))
	    (right '(1 0))
	    (up '(0 1))
	    (down '(0 -1)))))


(defun delta (point1 point2)
  "The vector necessary to go from point2 to point1. I.e. point1 - point2"
  (mapcar #'- point1 point2))

(defun normalize (num)
  "Returns + or - 1 depending on num's sign"
  (if (>= num 0)
      1
      -1))

(defun normalize-vec (delt)
  "Look, I know my code can be horrible in terms of documentation (or lack thereof)... But this is taking it to a new height.
Anyway, this function takes a direction vector (err, vector as in physics) and gives a normalized vector (a vector that has no component larger than 1).

This is here because the next knot in the rope needs to kinda go in the direction of its head, but we can't just get it on top of it... so we use this helper function to figure out the exact vector we can apply to get just 'kinda there'."
  (cond
    ((= (car delt) 0) (list 0 (normalize (cadr delt))))
    ((= (cadr delt) 0) (list (normalize (car delt)) 0))
    (t `(,(normalize (car delt))
	 ,(normalize (cadr delt))))))

(defun make-move (state ins)
  (labels ((tail-movement (state)
	     (if (null (cdr state))
		 state
		 (if (too-far-p (first state) (second state))
		     (cons (car state)
			   (tail-movement (append (list (add-vecs (cadr state) (normalize-vec (delta (car state) (cadr state)))))
						  (cddr state))))
		     state)))
	   (f (state new-head ins tails)
	     (if (= (cadr ins) 0)
		 (values state tails)
		 (let ((new-state (tail-movement (cons new-head (cdr state)))))
		   (f new-state
		      (move-point new-head (car ins))
		      (list (car ins) (1- (cadr ins)))
		      (cons (first (last new-state)) tails))))))
    (f state (move-point (car state) (car ins)) ins nil)))

(defun part1 (inss &optional (state (make-general-state 2)) (tails nil))
  "Don't mind the name, the two parts were so similar that I just kinda ended up using the same code for both of them.
   Just call this with (make-general-state 10) as the second argument for the second part of the problem. No extra arguments necessary for the first."
  (if inss
      (multiple-value-bind (new-state new-tails)
	  (make-move state (car inss))
	(part1 (cdr inss) new-state (append tails new-tails)))
      (remove-duplicates tails
			 :test #'equal)))

; dirty imperial
(defun read-input (f)
  (with-open-file (st f)
    (labels ((r (lines line)
	       (if line
		   (r (cons line lines) (read-line st nil nil))
		   lines)))
      (r nil (read-line st nil nil)))))

(defparameter *input* (reverse (mapcar #'parse-input (read-input "input-aoc9"))))
(print (length (part1 *input*)))
(print (length (part1 *input* (make-general-state 10))))
