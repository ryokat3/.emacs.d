;;; -*- mode:Emacs-Lisp; coding:utf-8; lexical-binding:t -*-
;;;
;;; my-common.el

(require 'cl)

(defvar my-common-call-once-hash-table (make-hash-table :test #'equal))

(defun my-common-write-to-temp-file (content &optional suffix)
  (let ((filename 
	 (make-temp-file (expand-file-name "t_m_p_" temporary-file-directory)
			 nil suffix)))
    (with-temp-buffer
      (insert content)
      (write-region (point-min) (point-max) filename))
    filename))

(defmacro my-common-call-once (&rest blocks)
  `(lexical-let
       ((cached-value
	 (gethash ',blocks my-common-call-once-hash-table
		  'my-common-call-once-hash-table)))
     (if (not (eq cached-value 'my-common-call-once-hash-table))
	 cached-value
       (lexical-let
	   ((return-value (funcall (lambda () ,@blocks))))
	 (puthash ',blocks return-value my-common-call-once-hash-table)
	 return-value))))


(defmacro my-common-cached-function (func args &rest blocks)
  "Execute the function at once. The returned value is cached."
  `(lexical-let
       ((this-func #'(lambda ,args ,@blocks))
	(cache (make-hash-table :test #'equal)))
     (fset ',func
	   #'(lambda (&rest this-args)
	       ;; If no value is cached for this-args, execute the function
	       ;; 'cache is used as kind of 'void'
	       (lexical-let ((value (gethash this-args cache 'cache)))
		 (cond
		  ((eq 'cache value)
		   (setf (gethash this-args cache)
			 (apply this-func this-args))) ; execute the function
		  (t value)))))))

	 

(defun my-local-fset (sym func)
  (progn
    (fset sym func)
    (add-to-list 'my-local-fset-list sym)))

(defun my-local-fset-clear()
  (cl-labels
      ((clear-func
	(sym-list)
	(cond
	 ((null sym-list) nil)
	 (t (progn
	      (fmakunbound (car sym-list))
	      (clear-func (cdr sym-list)))))))
    (set 'my-local-fset-list
	 (clear-func my-local-fset-list))))

(defun my-common-show-in-buffer (output-buffer output)
  (save-selected-window
    (pop-to-buffer (get-buffer-create output-buffer))
    (goto-char (point-max))
    (insert output)))

(provide 'my-common)


