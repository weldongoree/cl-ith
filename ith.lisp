 (defun new-alphabet ()
  (let ((alphabet nil))
    (lambda (msg obj)
      	  (case msg
		(show alphabet)
		(add (let ((found (assoc obj alphabet :test #'equalp)))
	       		(if found
		   		(rplacd found (1+ (cdr found)))
		   		(setq alphabet (acons obj 1 alphabet)))))
		(sum (apply #'+ (mapcar #'cdr alphabet)))
		(length (length alphabet))))))


(defun show-alphabet (alph)
  (funcall alph 'show nil))

(defun add-to-alphabet (alph obj)
  (funcall alph 'add obj))

(defun alphabet-sum (alph)
  (funcall alph 'sum nil))

(defun alphabet-length (alph)
  (funcall alph 'length nil))

(defun frequencies (alph)
    (mapcar (lambda (x) (cons (car x) (/ (cdr x) (alphabet-sum alph)))) (show-alphabet alph)))

(defun entropy (alph &optional (base 2))
  (- (apply #'+ (mapcar (lambda (x) (* (cdr x) (log (cdr x) base))) (frequencies alph)))))

(defun chi-squared (alph)
  (let* ((n (alphabet-sum alph))
	 (f (frequencies alph))
	 (p (/ 1 n)))    
    (* n (apply #'+ (mapcar (lambda (x) (* p (expt (/ (- (cdr x) p) p) 2)))
			    f)))))




; making a modification

