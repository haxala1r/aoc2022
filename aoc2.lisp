(defmacro switch-my (c prim &body body)
  (let ((nc (gensym)))
    `(let ((,nc ,c))
       (cond
	 ,@(loop for i in body collect
			  (append (list (list prim nc (first i)))
				  (rest i)))))))

(defmacro add-list (l i)
  `(setf ,l (append ,l (list ,i))))

(defun decode-move (m)
  (switch-my m CHAR=
    (#\A :rock)
    (#\B :paper)
    (#\C :scissors)))

(defun decode-xyz (first-m c)
  (switch-my c CHAR=
    (#\X ;loss
     (switch-my first-m eql
       (:rock :scissors)
       (:paper :rock)
       (:scissors :paper)))
    (#\Y ;draw
     (switch-my first-m eql
		(:rock :rock)
		(:paper :paper)
		(:scissors :scissors)))
    (#\Z ;win
     (switch-my first-m eql
		(:rock :paper)
		(:paper :scissors)
		(:scissors :rock))
    )))

; Since the second column determines
; what we play, this function returns t if m2 wins.
(defun is-win (m1 m2)
  (cond
    ((eql m2 :rock)
     (eql m1 :scissors))
    ((eql m2 :scissors)
     (eql m1 :paper))
    ((eql m2 :paper)
     (eql m1 :rock))))

(defun is-draw (m1 m2)
  (eql m1 m2))

; no need for is-loss
; if it isn't a draw or a win its a loss.
(defun read-guide ()
  (let ((ret nil) (ln ""))
    (loop
      (setf ln (read-line *standard-input* nil ""))
      (when (string= ln "")
	(return ret))
      (let ((f (decode-move (aref ln 0))))
	(add-list ret (list f (decode-xyz f (aref ln 2))))))))

(defun get-score (guide)
  (let ((total 0) (score 0))
    (loop for r in guide do
      (cond
	((is-win (nth 0 r) (nth 1 r))
	 (incf score 6))
	((is-draw (nth 0 r) (nth 1 r))
	 (incf score 3)))
      (cond
	((eql (nth 1 r) :rock) (incf score 1))
	((eql (nth 1 r) :paper) (incf score 2))
	((eql (nth 1 r) :scissors) (incf score 3)))
      (setf total (+ total score))
      (setf score 0))
    total))

(defun main ()
  (format t "~D" (get-score (read-guide))))
