;clean
(defun line-to-nums (line)
  "breaks a line of digits into a list of its individual digits"
  (unless (string= line "")
    (cons (parse-integer (subseq line 0 1))
	  (line-to-nums (subseq line 1)))))

(defun check-line (line &optional (max -1) (flip nil))
  "Checks a single line of trees for visibility (one way only from the start)."
  (when line
    (if (> (car line) max)
	(cons (if flip 0 1)
	      (check-line (cdr line)
		     (car line)))
	(cons (if flip 1 0)
	      (check-line (cdr line)
		     max)))))

(defun check-hori (map)
  (loop for i in map
	collect (check-line i)))

(defun flip-xy (map)
  (loop for i from 0 to (1- (length (car map)))
	collect (mapcar (lambda (x) (nth i x))
			map)))

(defun check-verti (map)
  (flip-xy (check-hori (flip-xy map))))

(defun ior (x y)
  "inclusive or. (operates with numerical 0 and 1 instead)"
  (cond
    ((= x 1) 1)
    ((= y 1) 1)
    (t 0)))

(defun ior-maps (map1 map2)
  "Assumes equal dimensions for both maps."
  (loop for i from 0 to (1- (length map1))
	collect (mapcar #'ior
			(nth i map1)
			(nth i map2))))

(defun flip-map-verti (map)
  (reverse map))

(defun flip-map-hori (map)
  (mapcar #'reverse map))

(defun get-visibility-map (map)
  "Checks the visibility from left, up, right, down; these all generate new maps with 0 or 1 indicating visibility."
  (reduce #'ior-maps
	  (list (check-hori map)
		(flip-map-hori (check-hori (flip-map-hori map)))
		(check-verti map)
		(flip-map-verti (check-verti (flip-map-verti map))))))

(defun handle-part1 (map)
  "Checks the visibility from left, up, right, down; these all generate new maps with 0 or 1 indicating visibility, then those maps are reduce'd to generate a total."
  (reduce #'+
	  (mapcar (lambda (x) (reduce #'+ x))
		  (get-visibility-map map))))

(defun ncdr (n list)
  (if (> n 0)
      (ncdr (1- n) (cdr list))
      list))

(defun get-line-score (line start)
  "This time goes right from a given point in the line, returns the amount of trees it can see."
  (labels ((check (val line count)
	     (cond
	       ((null line) count)
	       ((<= val (car line)) (1+ count))
	       (t (check val (cdr line) (1+ count))))))
    (check (car (ncdr start line)) (cdr (ncdr start line)) 0)))
  

(defun get-score (map posx posy)
  (* (get-line-score (nth posy map) posx)
     (get-line-score (reverse (nth posy map)) (- (length (car map)) posx 1))
     (get-line-score (nth posx (flip-xy map)) posy)
     (get-line-score (nth posx (flip-xy map)) (- (length map) posy 1))))

; forgive the dirtiness here, but it's only collecting values so i think it's okay?
; There's no real point in replacing *every* loop with recursion, is there?
; I mean, at least I think we're doing it to be more bug-resistant and actually
; organised, not just a mindless crusade against loops...
(defun handle-part2 (map)
  "It's kind of a bummer that we have to pretty much throw our entire solution out and start again for this part..."
  (loop for y from 0 to (1- (length map))
	collect (loop for x from 0 to (1- (length (car map)))
		 collect (get-score map x y))))

;dirty
(defun read-map (f)
  (with-open-file (fst f)
    (labels ((keep-reading (line)
	       (if line
		   (cons line
			 (keep-reading (read-line fst nil nil))))))
      (mapcar #'line-to-nums
	      (keep-reading (read-line fst nil nil))))))

(defvar *input-map* (read-map #p"./input-aoc8"))
(print (handle-part1 *input-map*))
(print (reduce #'max
	       (mapcar (lambda (x) (reduce #'max x))
		       (handle-part2 *input-map*))))
