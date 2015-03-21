;;; -*- mode:Emacs-Lisp; coding:utf-8; lexical-binding:t -*-

(require 'cl)
(require 'my-common)

(defun my-proc-args (s)
  "Make command line options"
  (cond
   ((null s) nil)
   ((listp s)
    (append (my-proc-args (car s)) (my-proc-args (cdr s))))
   ((stringp s) (list s))
   ((numberp s) (list (number-to-string s)))
   (t nil)))

(defun my-proc-start (filter sentinel name buffer program &rest args)
  (let
      ((proc (apply #'start-process name buffer program (my-proc-args args))))
    (when filter
      (set-process-filter proc filter))
    (when sentinel
      (set-process-sentinel proc sentinel))
    proc))

(defun my-proc-callback-sentinel (_callback _buffer)
"Call callback function after process terminates
callback RESULT OUTPUT EVENT"
  (lexical-let
      ((callback _callback)
       (buffer _buffer))
    (lambda (proc event)
      (let ((output (with-current-buffer buffer (buffer-string))))
	(kill-buffer buffer)
	(funcall callback
		 (if (string-match "finished" event) t nil)
		 output event)))))

(defun my-proc-callback (callback name program &rest args)
  (let*
      ((buffer (generate-new-buffer "*tempbuf*"))
       (sentinel (my-proc-callback-sentinel callback buffer)))
    (apply #'my-proc-start nil sentinel name buffer program args)))

(defun my-proc-call (program infile stderr-file display &rest args)
  (with-temp-buffer
    (cons
     (apply #'call-process
	    program infile (list (current-buffer) stderr-file) display args)
     (buffer-string))))

(defmacro my-proc-with-process-coding-system
  (proc decoding encoding &rest blocks)
  `(lexical-let*
       ((proc ,proc)
	(current-coding (process-coding-system proc))
	(decoding (or ,decoding (car current-coding)))
	(encoding (or ,encoding (cdr current-coding)))
	(result nil))
     (set-process-coding-system proc decoding encoding)
     (setq result (funcall (lambda () ,@blocks)))
     (set-process-coding-system proc (car current-coding) (cdr current-coding))
     result
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-process-sentinel-wrapper (_sentinel _idle-timer)
  (lexical-let
      ((sentinel _sentinel)
       (idle-timer _idle-timer))
    (lambda (proc msg)
      (progn
	(when (string-match "finish" msg)
	  (cancel-timer idle-timer)
	  )
	(funcall sentinel proc msg)
	))))

;;
;; Process Filter
;;
(defun my-process-create-read-string-filter (_regexp &optional _prompt _initial)
  (lexical-let
      ((regexp _regexp)
       (prompt _prompt)
       (initial _initial))
    (lambda (proc output)
      (when (string-match regexp output)
	(process-send-string
	 proc (read-string (or prompt output)))))))

(defun my-process-create-keep-output-filter (_sym)
  (lexical-let ((sym _sym))
    (setf (symbol-value sym) "")
    (lambda (proc output)
      (setf (symbol-value sym) (concat (symbol-value sym) output)))))

(defun my-process-set-filter-list (proc &rest _filter-list)
  (lexical-let ((filter-list _filter-list))
    (set-process-filter
     proc
     (lambda (inner-proc output)
       (dolist (filter filter-list)
	 (funcall filter inner-proc output))))))

;;
;; Process Sentinel
;;
(defun my-process-create-show-output-sentinel (_buffer _output-sym)
  (lexical-let
      ((buffer _buffer)
       (output-sym _output-sym))
    (lambda (proc event)
      (when (string-match "finished" event)
	(my-common-show-in-buffer buffer (symbol-value output-sym))
	))))

(defun my-process-create-show-error-sentinel (_buffer _output-sym)
  (lexical-let
      ((buffer _buffer)
       (output-sym _output-sym))
    (lambda (proc event)
      (unless (string-match "finished" event)
	(my-common-show-in-buffer
	 buffer
	 (concat "Event\n=====\n" event
		 "\n\nOutput\n======\n" (symbol-value output-sym))
	 )
	))))

(defun my-process-set-sentinel-list (proc &rest _sentinel-list)
  (lexical-let ((sentinel-list _sentinel-list))
    (set-process-sentinel
     proc
     (lambda (inner-proc event)
       (dolist (sentinel sentinel-list)
	 (funcall sentinel inner-proc event))))))

;;
;; Process Exec
;;
(defun my-process-command-line-args (sexp)
  "Make command line options"
  (cond
   ((null sexp) nil)
   ((stringp sexp) (list sexp))
   ((numberp sexp) (list (number-to-string tree)))
   ((listp sexp) (append
		  (my-process-command-line-args (car sexp))
		  (my-process-command-line-args (cdr sexp))))
   (t nil)))

(defun my-process-start (program &rest args)
  (apply #'start-process "my-process-start" nil program
	 (my-process-command-line-args args)))

(defun my-process-start-and-callback (_callback program &rest args)
  (lexical-let
      ((callback _callback)
       (proc (apply #'my-process-start program args))
       (output-sym (gensym)))
    (set-process-filter
     proc 
     (my-process-create-keep-output-filter output-sym))
    (set-process-sentinel
     proc
     (lambda (inner-proc event)
       (funcall callback inner-proc event (symbol-value output-sym))
       ))))

(defun my-process-show-output (buffer-name program &rest args)
  (lexical-let
      ((proc (apply #'my-process-start program args))
       (output-sym (gensym)))
    (my-process-set-sentinel-list
     proc
     (my-process-create-show-output-sentinel buffer-name output-sym)
     (my-process-create-show-error-sentinel
      (concat "Process Error (" buffer-name ")") output-sym))
    (my-process-set-filter-list
     proc 
     (my-process-create-keep-output-filter output-sym)
     )
    proc))

(defmacro my-process-with-process-coding-system
  (proc decoding encoding &rest blocks)
  `(lexical-let*
       ((proc ,proc)
	(current-coding (process-coding-system proc))
	(decoding (or ,decoding (car current-coding)))
	(encoding (or ,encoding (cdr current-coding)))
	(result nil))
     (set-process-coding-system proc decoding encoding)
     (setq result (funcall (lambda () ,@blocks)))
     (set-process-coding-system proc (car current-coding) (cdr current-coding))
     result
     ))

(provide 'my-process)
