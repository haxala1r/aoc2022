(defun str-empty-p (str)
  (string= str ""))

(defun sum-list (l)
  (let ((total 0))
    (loop for i in l
	  do (setf total (+ total i)))
    total))

(defmacro add-list (l i)
  `(setf ,l (append ,l (list ,i))))

(defun count-elves ()
  (let ((ret nil) (cur nil) (ln nil))
    (loop
      (setf ln (read-line *standard-input* nil ""))
      (cond
	((str-empty-p ln)
	 (if (null cur)
	    (return-from count-elves ret))
	 (add-list ret cur)
	 (setf cur nil))
	(t
	 (add-list cur (parse-integer ln)))))))

(defun main ()
  (let ((elves (loop for i in (count-elves) collect (sum-list i))))
    (setf elves (sort elves #'>))
    (format t "~D" (+ (nth 0 elves) (nth 1 elves) (nth 2 elves)))))
