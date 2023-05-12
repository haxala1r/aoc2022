; clean
(defun split-string (str char)
  (if (position char str)
      (cons (subseq str 0 (position char str))
	    (split-string (subseq str (1+ (position char str))) char))
      (cons str nil)))
(defun get-range (str)
  (mapcar #'parse-integer (split-string str #\-)))
(defun get-ranges (str)
  (mapcar #'get-range (split-string str #\,)))

(defun value-in-range (val range)
  (and (>= val (first range))
       (<= val (second range))))
(defun half-contained (range1 range2)
  "Checks if there are any values in range1 that are not in range2. Returns t if there aren't, nil if there are."
  (and (value-in-range (first range1) range2)
       (value-in-range (second range1) range2)))
(defun fully-contained-p (range1 range2)
  (or (half-contained range1 range2)
      (half-contained range2 range1)))

(defun half-overlap (range1 range2)
  (or (value-in-range (first range1) range2)
      (value-in-range (second range1) range2)))
(defun any-overlap-p (range1 range2)
  (or (half-overlap range1 range2)
      (half-overlap range2 range1)))

(defun get-total (all-ranges func)
  (reduce #'+ (mapcar (lambda (x)
			(if (funcall func (first x) (second x))
			    1
			    0))
		      all-ranges)))
(defun get-total-contained (all-ranges)
  (get-total all-ranges #'fully-contained-p))
(defun get-total-overlaps (all-ranges)
  (get-total all-ranges #'any-overlap-p))

; dirty
(defun read-assignments ()
  "I am amazed that this was an option all along. Functional programming... it's so, so beautiful. So serene, so graceful."
  (with-open-file (fst #p"./input-aoc4")
    (labels ((read-assignment (line)
	       (if line
		   (cons line
			 (read-assignment (read-line fst nil nil))))))
      (read-assignment (read-line fst nil nil)))))

(defparameter *assignments* (mapcar #'get-ranges (read-assignments)))
(print (get-total-contained *assignments*))
(print (get-total-overlaps *assignments*))
