;;; -*- mode:Emacs-Lisp; coding:utf-8 -*-
;;;
;;; my-cmd.el -- command

(defun my-cmd-flat-list (tree &optional fn)
  "Make a flat list from tree. Remove nil"
  (labels
      ((rec
	(tree acc)
	(cond
	 ((null tree) acc)
	 ((atom tree) ; leaf
	  (let ((elm (if (null fn) tree (funcall fn tree))))
	    (cond ((null elm) tree)
		  (t (push elm acc)))))
	 (t ; node
	  (rec (car tree) (append (rec (cdr tree) nil) acc))))))
    (rec tree nil)))

(defun my-cmd-options (command-option-list)
  (my-cmd-flat-list
   (if (listp command-option-list)
       command-option-list
     (list command-option-list))
   #'(lambda (opt)
       (cond
	((stringp opt) opt)
	((numberp opt) (number-to-string opt))
	(t nil)))))

(defun my-cmd-exec (command &rest command-option-list)
  (with-temp-buffer
    (lexical-let
	((process-exit-code
	  (apply #'call-process command nil
		 (current-buffer) nil
		 (my-cmd-options command-option-list))))
      (list process-exit-code (buffer-string)))))

(defun my-cmd-exec (command &rest command-option-list)
  (with-temp-buffer
    (lexical-let
	((process-exit-code
	  (apply #'call-process command nil
		 (current-buffer) nil
		 (my-cmd-options command-option-list))))
      (list process-exit-code (buffer-string)))))

(provide 'my-cmd)
