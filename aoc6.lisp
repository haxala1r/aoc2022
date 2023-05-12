;clean
(defun unique-list-p (list)
  (cond
    ((null list) t)
    ((member (car list) (cdr list)) nil)
    (t (unique-list-p (cdr list)))))

(defun str-to-list (str)
  (if (string= str "")
      nil
      (cons (aref str 0) (str-to-list (subseq str 1)))))

(defun search-unique-length (str len)
  (cond
    ((< (length str) len) (length str))
    ((unique-list-p (str-to-list (subseq str 0 len))) len)
    (t (1+ (search-unique-length (subseq str 1) len)))))

(defun part1 (str)
    (search-unique-length str 4))

(defun part2 (str)
  (search-unique-length str 14))

;dirty
(defun read-input ()
  (with-open-file (fstr #p"./input-aoc6")
    (read-line fstr)))

(defparameter *input* (read-input))
(print (part1 *input*))
(print (part2 *input*))
