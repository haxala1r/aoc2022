;clean.... or, it was. some time ago.
; oh ye who enter here, I pity thee.
; oh ye who heed my call, set this code free.
; oh ye who hear my words, rewrite this piece of shit. please.

(defmacro while (clause &body body)
  `(do () ((not ,clause) nil)
     ,@body))

(defmacro str-case (key &body body)
  (unless (null body)
    (let ((var (gensym)))
      `(let ((,var ,key))
	 (if (or (string= ,var ,(caar body))
		 (eq ,(caar body) t))
	     (progn ,@(cdar body))
	     (str-case ,var ,@(cdr body)))))))



(defun split-string (str char)
  (let ((pos (position char str)))
    (if pos
	(cons (subseq str 0 pos)
	      (split-string (subseq str (1+ pos)) char))
	(cons str nil))))
(defun line-command-p (line)
      (char= (aref line 0) #\$))
(defun to-next-command (lines)
  (if (or (null lines)
	  (line-command-p (car lines)))
      lines
      (to-next-command (cdr lines))))

(defclass node ()
  ((parent :accessor parent
	   :initarg :parent)
   (name :accessor name
	 :initarg :name)
   (children :accessor children
	     :initarg :children
	     :initform nil)
   (is-directory :accessor directory-p
                 :initarg :is-directory
		 :initform nil)
   (size :accessor size
	 :initarg :size
	 :initform nil)))

(defmethod print-object ((object node) stream)
  (format stream "#<~a>" (name object)))

(defun find-child (node name)
  (if (directory-p node)
      (find name (children node) :test (lambda (name node)
					 (string= name (name node))))
      nil))

(defun get-node (root path)
  (loop for i in (split-string path #\/)
	unless (string= i "") do
	  (setf root (find-child root i)))
  root)

(defun get-parent-path (path)
  (subseq path 0
	  (1+ (position #\/ (subseq path 0 (1- (length path))) :from-end t))))

(defun get-child-path (path child-name)
  (concatenate 'string path child-name "/"))

(defun handle-cd (cwd newdir)
  "meh, works well enough, and still has no side effects so maybe even functional? also kind of unnecessary now, since we're building a full on file tree for this. Fuck this."
  (cond
    ((char= (aref newdir 0) #\/) newdir)
    ((string= newdir "..") (get-parent-path cwd))
    (t (concatenate 'string cwd newdir "/"))))

(defun parse-listing (line cur-node)
  (let ((split (split-string line #\Space)))
    (if (string= (first split) "dir")
	(make-instance 'node
		       :is-directory t
		       :name (second split)
		       :children nil
		       :parent cur-node
		       :size nil)
	(make-instance 'node
		       :is-directory nil
		       :name (second split)
		       :children nil
		       :parent cur-node
		       :size (parse-integer (first split))))))

(defun handle-ls (lines cur-node)
  (unless (or (null lines)
	      (line-command-p (first lines)))
    (cons (parse-listing (car lines) cur-node)
	  (handle-ls (cdr lines) cur-node))))

(defun handle-part1 (lines &optional (root-node (make-instance 'node
							       :name "/"  :children nil
							       :is-directory t :parent nil))
			     (cwd "/"))
  (if (null lines)
      (return-from handle-part1 root-node))
  (let ((split-line (split-string (car lines) #\Space))
	(cur-node (get-node root-node cwd)))
    (if (null cur-node)
	(error "fuck"))
    (str-case (second split-line)
      ("cd" (handle-part1 (cdr lines) root-node
			  (str-case (third split-line)
			    ("/" "/")
			    (".." (get-parent-path cwd))
			    (t (get-child-path cwd (third split-line))))))
      ("ls"
       (setf (children cur-node) (handle-ls (cdr lines) cur-node))
       (handle-part1 (to-next-command (cdr lines)) root-node (print cwd))))))

(defconstant total-size  70000000)
(defconstant needed-size 30000000)
(defun handle-part2 (root)
  (labels ((all-dir-children (dir need)
	     "List of children that are above need (including self)"
	     (if (directory-p dir)
		 (nconc (if (> (size dir) need) (list dir))
			(reduce #'nconc
				(mapcar (lambda (x) (all-dir-children x need))
					(children dir)))))))
    (size
     (reduce (lambda (x y) (if (< (size x) (size y)) x y))
	    (all-dir-children root  (- needed-size (- total-size (size root))))))))

(defun calculate-size (dir-node)
  (if (size dir-node)
      (size dir-node)
      (setf (size dir-node)
	    (reduce #'+
	      (mapcar #'calculate-size (children dir-node))))))

(defun get-sizes (root)
  (+
   (if (directory-p root)
       (if (< (size root) 100000)
	   (size root)
	   0)
       0)
   (if (directory-p root)
       (reduce #'+ (mapcar #'get-sizes (children root)))
       0)))

(defun print-file-tree (root-node &optional (level 0))
  (loop for i in (children root-node)
	do (if (directory-p i)
	       (progn
		 (dotimes (k level) (format t "-"))
		 (format t "DIRECTORY ~a ~a:~%" (name i) (size i))
		 (print-file-tree i (1+ level)))
	       (progn
		 (dotimes (k level) (format t "-"))
		 (format t "~a ~a~%" (name i) (size i))))))

;dirty
(defun read-commands ()
  (with-open-file (fst #p"./input-aoc7")
    (labels ((keep-reading ()
	       (let ((a (read-line fst nil nil)))
		 (when a
		     (cons a (keep-reading))))))
      (keep-reading))))
