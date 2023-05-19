;trying to stick to functional programming this time.
(ql:quickload "cl-ppcre")

; this is a somewhat inefficient implementation, since I'm using linked lists and other such
; simple data structures to represent everything. Still, it runs for less than ~5 seconds so I'll just let it go.

; we'll use these to generate all coordinates that have rock on them, then check collision etc. with that.
(defun int-range (x1 x2)
  "inclusive range"
  (cond
    ((= x1 x2) (list x1))
    ((> x1 x2) (int-range x2 x1))
    (t (cons x1 (int-range (1+ x1) x2)))))
(defun pos-range (pos1 pos2)
  (cond
    ((equal pos1 pos2) pos1)
    ((equal (car pos1) (car pos2))
     (mapcar (lambda (x) (list (car pos1) x))
	     (int-range (cadr pos1) (cadr pos2))))
    ((equal (cadr pos1) (cadr pos2))
     (mapcar (lambda (x) (list x (cadr pos1)))
	     (int-range (car pos1) (car pos2))))
    (t (error "can't get range unless they share a coordinate..."))))
(defun parse-input (input)
  "Parses the input into a list of paths, which are a list of positions, which are a list if integers."
  (labels ((nums (l)
	     (mapcar (lambda (s)
		       (multiple-value-bind (r v)
			   (cl-ppcre:scan-to-strings "([0-9]*),([0-9]*)" s)
			 r
			 (loop for i across v collect (parse-integer i))))
		     (cl-ppcre:all-matches-as-strings "[0-9]*,[0-9]*" l)))
	   (f (input ls)
	     (if input
		 (f (cdr input) (cons (nums (car input)) ls))
		 ls)))
    (nreverse (f input nil))))

(defun mapcar2 (fun seq)
  "applies fun to first two elements of seq, then the second and third, third and fourth etc. until the last element."
  (if (>= (length seq) 2)
      (cons (funcall fun (first seq) (second seq))
	    (mapcar2 fun (cdr seq)))))
(defun get-stones (input)
  "The 'paths' in our input have a lot of duplicate coordinates anyway, so here we find all coordinates that
   have stone on them. Then we remove duplicates. There's a *lot*. Like, 90% of it or something is duplicates."
  (remove-duplicates
   (reduce #'append
	   (mapcan (lambda (l) (mapcar2 #'pos-range l))
		   (parse-input input)))
   :test #'equal))

(defun empty-below (sand stones &optional (part2 nil))
  (unless part2
    (reduce (lambda (x y) (and x y))
	    (mapcar (lambda (p)
		      (if (and (= (car p) (car sand))
			       (> (cadr p) (cadr sand)))
			  nil
			  t))
		    (loop for i being the hash-keys in stones collect i)))))

(defun sandfall (sand stones &optional (part2 nil) part2-maxy )
  "Returns the position where the sand stops."
  (labels ((check (pos)
	     (cond
	       ((gethash pos stones nil) nil)
	       (part2 (if (< (cadr pos) part2-maxy) pos nil))
	       (t pos)))
	   (new-pos (sand)
	     (or (check (list (car sand) (1+ (cadr sand))))
		 (check (list (1- (car sand)) (1+ (cadr sand))))
		 (check (list (1+ (car sand)) (1+ (cadr sand))))))
	   (f (sand new)
	     (if (or (null new)
		     (empty-below sand stones part2))
		 sand
		 (f new (new-pos new)))))
    (f sand (new-pos sand))))

(defun part1n2 (input &optional (part2 nil))
  (let* ((stones-list (get-stones input))
	 (stones-table (make-hash-table :test #'equal))
	 (sand-start '(500 0))
	 (maxy (+ 2 (reduce #'max (mapcar (lambda (p) (cadr p)) stones-list)))))
    (loop for i in stones-list do (setf (gethash i stones-table) t))
    ; This loop marks every sand position on the hashtable with a 'sand for its value.
    (labels ((f (stones new)
	       (if (and new (not (equal new sand-start))
			(not (empty-below new stones part2)))
		   (progn
		     (setf (gethash new stones) 'sand)
		     (f stones (sandfall sand-start stones part2 maxy)))
		   stones)))
      (f stones-table (sandfall sand-start stones-table)))))

(defun part1 (input)
  (let ((table (part1n2 input)))
    (length (loop for i being the hash-keys in table if (eql (gethash i table) 'sand) collect i))))

(defun part2 (input)
  (let ((table (part1n2 input t)))
    (1+ (length (loop for i being the hash-keys in table if (eql (gethash i table) 'sand) collect i)))))

; io
(defun read-input (file)
  (with-open-file (st file)
    (labels ((r () (read-line st nil nil))
	     (f (lines line)
	       (if line
		   (f (cons line lines) (r))
		   lines)))
      (nreverse (f nil (r))))))
(defparameter *input* (read-input #p"./input-aoc14"))
(print (part1 *input*))
(print (part2 *input*))
