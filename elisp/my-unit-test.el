;;; -*- mode:Emacs-Lisp; coding:utf-8 -*-
;;;
;;; my-unit-test.el -- unit test for Emacs lisp

(defcustom my-unit-test-buffer-name
  "Unit Test"
  "Buffer name for my-unit-test")

(defun my-unit-test ()
  "Execute the set of Unit Test.
   Unit test is the function of which prefix is \"my-unit-test-\".
   The function of Unit Test should return non-nil value if it succeed,
   nil if it fails"
  (interactive)
  (save-selected-window
    (pop-to-buffer (get-buffer-create my-unit-test-buffer-name))
    (erase-buffer)
    (goto-char (point-max))
    (insert "Unit Test\n")
    (mapatoms
     (lambda (sym)
       (lexical-let ((sym-name (symbol-name sym)))
	 (when (and (string-match "^my-unit-test-" sym-name)
		    (fboundp sym))
	   (condition-case error-description
	       (if (apply (symbol-function sym) nil)
		   (insert (format "%s -- OK\n" sym-name))
		 (insert (format "%s -- NG\n" sym-name)))
	     (error (insert 
		     (format "%s -- ERROR\n%s\n" sym-name error-description)))
	     )))))))

(provide 'my-unit-test)