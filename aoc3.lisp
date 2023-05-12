(defmacro add-to-list (l i)
  `(setf ,l (append ,l (list ,i))))

(defun mysearch (l i &optional (pred #'eql))
  (loop for j from 0 to (1- (length l))
	do (if (funcall pred (aref l j) i)
	       (return-from mysearch j)))
  nil)

(defvar *alphabet* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
(defun get-priority (i)
  (1+ (mysearch *alphabet* i #'CHAR=)))

(defun get-uniq-chars (st)
  (let ((ret nil))
    (loop for i across st do
      (if (not (find i ret))
        (add-to-list ret i)))
    ret))

; We use a rucksack as just two lists of each unique character
; in each compartment.
(defun handle-rucksack (st)
  (let ((fhalf (subseq st 0 (/ (length st) 2)))
	(shalf (subseq st (/ (length st) 2))))
    (list (get-uniq-chars fhalf) (get-uniq-chars shalf))))

(defun read-rucksacks ()
  (let ((ret nil) (ln nil))
    (loop
     (setf ln (read-line *standard-input* nil ""))
     (if (string= ln "")
	 (return ret))
     (add-to-list ret (handle-rucksack ln)))))

(defun get-common (l1 l2)
  (loop for i in l1
	if (find i l2) collect i))

(defun main ()
  (let ((total 0) (rucksacks (read-rucksacks)))
    (loop for ruck in rucksacks do
      (setf total (+ total (get-priority (car (get-common (first ruck) (second ruck)))))))
    (print total)))

(defun read-groups ()
  (let ((ret nil) (group nil) (ln ""))
    (loop
     (loop for i from 0 to 2 do
       (setf ln (read-line *standard-input* nil ""))
       (if (string= ln "")
	   (return-from read-groups ret))
       (add-to-list group (get-uniq-chars ln)))
     (add-to-list ret group)
     (setf group nil))))

; second part of the challenge
(defun main-2 ()
  (let ((total 0) (groups (read-groups)))
    (loop for i in groups do
      (setf total (+ total (get-priority (car (get-common (get-common (first i) (second i)) (third i)))))))
    (print total)))
