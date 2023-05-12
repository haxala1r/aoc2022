(defun make-adj-str ()
  (make-array 0
	      :adjustable t
	      :fill-pointer 0
	      :element-type 'character))

(defmacro add-to-list (l i)
  `(setf ,l (append ,l (list ,i))))

(defun split-str (str ch)
  (let ((ret nil) (cur 0))
    (loop for i from 0 to (1- (length str)) do
      (cond
	((or (CHAR= (aref str i) ch)
	     (= i (1- (length str))))
	 (let ((sub (subseq str cur i)))
	   (if (not (string= sub ""))
	       (add-to-list ret sub)))
	 (setf cur (1+ i)))))
    ret))



(defun read-sectors ()
  (let ((ret nil) (ln ""))
    (loop
      (setf ln (read-line *standard-input* nil ""))
      (add-to-list ret ())))) ;; todo

(defun main ()
  )
