(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy (&body body)
  (let ((value (gensym))
		(forced (gensym)))
	`(let ((,value nil)
		   (,forced nil))
	   (lambda ()
		 (unless ,forced
		   (setf ,value (progn ,@body))
		   (setf ,forced t))
		 ,value))))

(defmacro lcons (a b)
  `(lazy (cons ,a ,b)))

(defun lcar (llist)
  (car (force llist)))

(defun lcdr (llist)
  (cdr (force llist)))

(defun lnth (n llist)
  (cond ((= n 0) (lcar llist))
		(t (lnth (- n 1) (lcdr llist)))))

(defun take (n llist)
  (cond ((= n 1) (cons (lcar llist) nil))
		(t (cons (lcar llist) (take (- n 1) (lcdr llist))))))

(defun take-all (llist)
  (cond ((null llist) nil)
		(t (cons (lcar llist) (take-all (lcdr llist))))))

(defun llist (lst)
  (cond ((null lst) '())
		(t (lcons (car lst) (llist (cdr lst))))))

(defun lfind-if (test llist)
  (cond ((null llist) nil)
		((funcall test (lcar llist)) (lcons (lcar llist) (lfind-if test (lcdr llist))))
		(t (lfind-if test (lcdr llist)))))

(defun lremove-if (test llist)
  (cond ((null llist) nil)
		((funcall test (lcar llist)) (lremove-if test (lcdr llist)))
		(t (lcons (lcar llist) (lremove-if test (lcdr llist))))))
