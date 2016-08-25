;;;
;;; My Function
;;;

(defconst my-func-placeholder-prefix "$")
(defconst my-func-placeholder-rest
  (intern (concat my-func-placeholder-prefix "rest")))
(defconst my-func-placeholder-regex "^\$\\([0-9]+\\)$")

(defun my-func-placeholder (num)
  (intern (concat my-func-placeholder-prefix (number-to-string num))))

(defun my-func-placeholder-alist (args)
  (let ((n 0))
    (mapcar (lambda (arg)
	      (cons (my-func-placeholder (setq n (+ n 1))) arg)) args)))

(defun my-func-placeholder-to-number (sym)
  (if (and (symbolp sym)
	   (string-match "^\$\\([0-9]+\\)$" (symbol-name sym)))
      (string-to-number (match-string 1 (symbol-name sym)))
    -1))

(defun my-func-sexpp (x)
  (cond
   ((null x) t)
   ((consp x) (my-func-sexpp (cdr x)))
   (x nil)))

(defun my-func-functionp (obj)
  (or (functionp obj)
      (and (symbolp obj)
	   (or (subrp (symbol-function obj))
	       (byte-code-function-p (symbol-function obj))))))

(defun my-func-has-placeholder (sexp)
  (cond
   ((null sexp) nil)
   ((symbolp sexp)
    (string-match my-func-placeholder-regex (symbol-name sexp)))
   ((consp sexp)
    (or (my-func-has-placeholder (car sexp))
	(my-func-has-placeholder (cdr sexp))))
   (t nil)))

(defun my-func-map (func x)
  (cond
   ((null x) nil)
   ((consp x)
    (cons (funcall func (car x))
	  (my-func-map func (cdr x))))
   (t (funcall func x))))

(defun my-func-placeholder-eval (sexp)
  (cond
   ((null sexp) nil)
   ((and (consp sexp)
	 (my-func-functionp (car sexp))
	 (my-func-sexpp sexp)	 
	 (not (my-func-has-placeholder sexp)))
    (eval (mapcar
	   (lambda (x) (if (consp x) (my-func-placeholder-eval x) x))
	   sexp)))
   ((consp sexp)
    (my-func-map #'my-func-placeholder-eval sexp))
   (t sexp)))


(defun my-func-sexp-replace (sexp alist alist-ap)
"Replace symbols in S expression

  alist :: alist of a symbol and the value to replace
  alist-ap :: same as alist except using 'append' if the replacing value is a list"
  (cond
   ((null sexp) nil)
   ((consp sexp)
    (let ((cell (assq (car sexp) alist-ap)))
      (cond
       ((and cell (listp (cdr cell)))
	(append (cdr cell)
		(my-func-sexp-replace (cdr sexp) alist alist-ap)))
       (t
	(cons (my-func-sexp-replace (car sexp) alist alist-ap)
	      (my-func-sexp-replace (cdr sexp) alist alist-ap))))))
   (t
    (let ((cell (or (assq sexp alist) (assq sexp alist-ap))))
      (cond
       (cell (cdr cell))
       (t sexp))))))


(defun my-func-sexp-maxargs (sexp)
  (cond
   ((null sexp) -1)
   ((symbolp sexp) (my-func-placeholder-to-number sexp))
   ((consp sexp)
    (max (my-func-sexp-maxargs (car sexp))
	 (my-func-sexp-maxargs (cdr sexp))))
   (t -1)))

(defmacro my-func-compose (_sexp &optional lazy-eval)
  "Create a new closure by composing functions"
  `(lexical-let 
       ((sexp (if ,lazy-eval ',_sexp (my-func-placeholder-eval ',_sexp))))
     (lambda (&rest args) 
       (eval (my-func-sexp-replace
	      sexp
	      (my-func-placeholder-alist args)
	      (list
	       (cons my-func-placeholder-rest
		     (nthcdr (my-func-sexp-maxargs sexp) args))))))))

(defmacro my-func-defcompose (func sexp &optional lazy-eval)
  "Define a new closure by composing functions"
  `(fset ',func (my-func-compose ,sexp ,lazy-eval)))

(defun my-func-safe (_func)
  "Catch an error when executing function"
  (lexical-let
      ((func _func))
    (lambda (&rest args)
      (condition-case err
	  (apply func args)
	(error nil)
	))))

(defun my-func-cacheable (_func)
  "Make a function to cache the return value
   and return it second time and later"
  (lexical-let
      ((func _func)
       (cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (lexical-let
	  ((cached-value (gethash args cache 'cache)))
	(cond
	 ((eq 'cache cached-value)
	  (setf (gethash args cache)
		(apply func args))) ; execute the function
	 (t cached-value))))
    ))

(provide 'my-func)



