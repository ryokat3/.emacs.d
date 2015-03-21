
(require 'cl)
(require 'my-common)
(require 'my-process)

(defcustom my-python-program "python" "Python program")

(defconst my-python-ps1 "--%%--##--" nil)
(defconst my-python-ps2 "--%%%--##--" nil)
(defconst my-python-ps1-regex
  (concat "^\\(\\(.\\|\n\\)*?\\)\\(\n\\)?" my-python-ps1 "$"))
(defconst my-python-ps2-regex
  (concat "^\\(\\(.\\|\n\\)*?\\)\\(\n\\)?" my-python-ps2 "$"))

(defconst my-python-console-script (concat
"
import sys
import code
console = code.InteractiveConsole()
sys.ps1='" my-python-ps1 "'
sys.ps2='" my-python-ps2 "'
console.interact('Hello, world')
"))

(defun my-python-start-console ()
  (lexical-let
      ((filename (my-common-write-to-temp-file
		  my-python-console-script ".py")))
    (add-hook 'kill-emacs-hook
	      (lambda () (when (file-exists-p filename)
			   (delete-file filename))))
    (let ((proc (start-process "python" nil my-python-program "-u" filename)))
      (set-process-sentinel proc (lambda (x y) (delete-file filename)))      
      (set-process-filter proc (lambda (x y) nil)) ;; discard the first prompt
      (process-send-string proc "") 
      proc)))

(defun my-python-eval (proc exp)      
  (if (eq 'run (process-status proc))
      (lexical-let ((buf (gensym)))
	(setf (symbol-value buf) "")
	(set-process-filter
	 proc (lambda (_proc output)
		(setf (symbol-value buf)
		      (concat (symbol-value buf) output))))
	(process-send-string proc (concat exp "\n"))
	(loop while
	      (and (eq 'run (process-status proc))
		   (not (string-match my-python-ps1-regex (symbol-value buf)))
		   (not (string-match my-python-ps2-regex (symbol-value buf))))
	      do (sit-for 0.1))
	(if (eq 'run (process-status proc))
	    (match-string 1 (symbol-value buf))
	  nil))
    nil))

(defun my-python-call (script &optional error-handler)
  (let ((result
	 (my-proc-call my-python-program nil nil nil "-c" script)))
    (cond
     ((= 0 (car result)) (cdr result))
     (t (if error-handler (funcall error-handler result))))))

(defun my-python-exec-list (proc lst)
  (if (null lst) nil
    (if (null (cdr lst))
	(my-python-exec-list proc (cdr lst))
      (my-python-eval proc (car lst))
      (my-python-exec-list proc (cdr lst)))))


(provide 'my-python)