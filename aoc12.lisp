; I will code in the functional style this time, hopefully.

(defun break-string (str)
  (loop for i across str collect i))

(defun search-char (map ch)
  (labels ((s (l x)
	     (when l
	       (if (char= (car l) ch)
		   x
		   (s (cdr l) (1+ x)))))
	   (f (m y)
	     (let ((x (s (car m) 0)))
	       (if x
		   (list x y)
		 (f (cdr m) (1+ y))))))
    (f map 0)))

(defun end-p (map pos)
  (equal pos (search-char map #\E)))
(defun pos-height (map pos)
  (char-code (let ((c (nth (car pos) (nth (cadr pos) map))))
	       (cond
		 ((char= c #\E) #\z)
		 ((char= c #\S) #\a)
		 (t c)))))
(defun valid-p (map src dst)
  (cond
    ; out of bounds
    ((remove-if (lambda (x) (>= x 0)) (append src dst)) nil)
    ; out of bounds 2: electric boogaloo
    ((or (>= (car src) (length (car map))) (>= (car dst) (length (car map)))) nil)
    ((or (>= (cadr src) (length map)) (>= (cadr dst) (length map))) nil)
    (t (<= (- (pos-height map dst) (pos-height map src))  1))))
(defun get-neighbors (map pos)
  "Gets the neighbors we can move towards"
  (remove-if-not (lambda (i) (valid-p map pos i))
	     (list (list (1+ (car pos)) (cadr pos)) (list (1- (car pos)) (cadr pos))
		   (list (car pos) (1+ (cadr pos))) (list (car pos) (1- (cadr pos))))))

(defun reverse-neighbors (map pos)
  "Gets the neighbors that can move to this position."
  (remove-if-not (lambda (i) (valid-p map i pos))
	     (list (list (1+ (car pos)) (cadr pos)) (list (1- (car pos)) (cadr pos))
		   (list (car pos) (1+ (cadr pos))) (list (car pos) (1- (cadr pos))))))

(defun pathfind (map start &optional (neighbors #'get-neighbors))
  "Dijktra's algorithm! woohoo! Simpler than I thought.
   And the best part? Since this creates a shortest-path tree of the *entire* map from a single point,
   we can supply a reverse-neighbors here, set the endpoint to the end, and get every possible starting point! amazing.
   NOTE: originally, I made this code actually record every single path to every other point on the map from start...
   But that was really slow. Besides, the problem didn't require me to find a *path*, it required a *length*, so
   in order to speed up the algorithm I changed it to work with lengths instead. This took execution time
   from ~90 seconds to ~5 seconds."
  (let ((paths (make-hash-table :test #'equal)))
    (labels ((p (path cur)
	       (multiple-value-bind (old-path success)
		   (gethash cur paths)
		 (if (or (not success)
			 (< path old-path))
		     (progn
		       (setf (gethash cur paths) path)
		       (mapcar (lambda (i) (p (1+ path) i))
			       (funcall neighbors map cur))))		 
		 )))
      (p 0 start)
      paths)))

(defun part1 (map searched-hashtable)
  (loop for i being the hash-keys in searched-hashtable
		     if (equal i (search-char map #\S))
		       do (return (gethash i searched-hashtable))))
(defun part2 (map searched-hashtable)
  (reduce #'min (loop for i being the hash-keys in searched-hashtable
	     if (= (pos-height map i) (char-code #\a))
	       collect (gethash i searched-hashtable))))

(defun part1n2 (map)
  "It's more efficient to do a reverse-search, since we can actually complete both parts in a single search."
  (let ((revsearch-res (pathfind map (search-char map #\E) #'reverse-neighbors)))
    (print (part1 map revsearch-res))
    (print (part2 map revsearch-res))))

; input handling
(defun read-input (f)
  (with-open-file (st f)
    (labels ((r () (read-line st nil nil))
	     (f (lines line)
	       (if line
		   (f (cons line lines) (r))
		   lines)))
      (reverse (f nil (r))))))
(defparameter *input* (mapcar #'break-string (read-input #p"./input-aoc12")))
(part1n2 *input*)
