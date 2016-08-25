;;; -*- mode:Emacs-Lisp; coding:utf-8; lexical-binding:t -*-
;;;
;;; my-openssl.el -- OpenSSL interface
;;;
;;;

;; Introduction
;; ============

;; my-openssl is an emacs lisp to encrypt/decrypt files with OpenSSL.

;; You can open an encrypted file in an emacs buffer with decrypted, and
;; save an plain text in a buffer with encrypted.

;; Thanks to using OpenSSL, you can decrypt files with openssl
;; command, without this emacs lisp. It helps you for your using
;; encrypted file in your shell scripts, etc.


;; Save the buffer
;; ===============

;; For the first time to save the buffer with encrypted, you execute
;; the command 'my-openssl-encrypt' in the buffer to be saved.



;; Decrypt in shell
;; ================

;; With shell commands, you can decode files encoded by this lisp script.

;;      $ openssl enc -d -<crypt-algorithm> -pass pass:<your-password> -in <encrypted-file>

;; For example, if you encrypted the file 'myfile.aes' with the algorithm
;; 'aes-256-cbc' and your password 'secret', then the command will be

;;      $ openssl enc -d -aes-256-cbc -pass pass:secret -in myfile.aes



;; Security Issue
;; ==============

;; * The password that has decripted a file is stored as a buffer local
;;   variable. It means that the password is stored in the memory of PC.
;;
;; * The plain text will be saved in the backup file and the auto-save file.
;;   Please disable those emacs feature if concerned.
;;
;;       (setq make-backup-files nil)
;;       (setq auto-save-default nil)
;;

(require 'my-common)
(require 'my-process)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Configuration
;;;

(defcustom my-openssl-program
  "openssl" "OpenSSL command")

(defcustom my-openssl-default-cipher-algorithm
  "aes-256-cbc" "Default cipher algorithm for OpenSSL")

(defconst my-openssl-cipher-algorithm-alist
  ;; '( <suffix> . <cipher-algorithm> )
  ;;
  ;; The first element is used as defalt
  ;;
  '(("aes" . "aes-256-cbc")
    ("bf" . "bf-cbc")
    ("camellia" . "camellia-256-cbc")
    ("cast5" . "cast5-cbc")
    ("des3" . "des3")))

;;;
;;; Configuration (End)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;; Constant Variable
;;;
(defconst my-openssl-file-suffix-regex
  (concat "\\.\\\("
	  (mapconcat #'car my-openssl-cipher-algorithm-alist "\\\|")
	  "\\\)$"))

(defconst my-openssl-cipher-algorithm-alist-regex
  (mapcar (lambda (pair)
	    `(,(concat "\\." (car pair) "$") . ,(cdr pair)))
	  my-openssl-cipher-algorithm-alist))


;;;  
;;; Buffer Local Variables
;;;

(make-variable-buffer-local
 (defvar my-openssl-buffer-cipher-algorithm
   my-openssl-default-cipher-algorithm "Cipher algorithm for current buffer"))

(make-variable-buffer-local
 (defvar my-openssl-buffer-cipher-password
   nil "Password of cipher algorithm for current buffer"))

(make-variable-buffer-local
 (defvar my-openssl-buffer-filename
   nil "Filename for current buffer"))

;;;
;;; Fuctions
;;;

(defun my-openssl (option-list &optional buffer-name)
  "Exec OpenSSL command"
  (interactive
   (list (read-from-minibuffer "OpenSSL options: ")))
  (my-process-show-output
   (or buffer-name (concat "openssl " option-list))
   my-openssl-program (split-string option-list " ")))

(defun my-openssl-list-standard-commands ()
  "Show the list of standard commands of OpenSSL"
  (interactive)
  (my-openssl
   "list-standard-commands"
   "OpenSSL Standard Commands"
   ))

(defun my-openssl-list-cipher-commands ()
  "Show the list of cipher commands of OpenSSL"
  (interactive)
  (my-openssl
   "list-cipher-commands"
   "OpenSSL Cipher Commands"
   ))

(defun my-openssl-process-send-password (proc passwd)
  (my-process-with-process-coding-system
   proc nil 'binary
   (process-send-string proc (concat passwd "\n"))))

(defun my-openssl-setup-keymap ()
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-x\C-s" 'my-openssl-encrypt)
    (set-keymap-parent map (current-local-map))
    (use-local-map map)))

;;
;; openssl enc -e -aes-128-cbc -pass stdin -in plain-file -out encrpted-file
;; and send a password to the standard input of process
;;

(defun my-openssl-encrypt-buffer-to-file (out-file algorithm passwd)
  "Encrypt buffer and write to file"
  (let*
      ((proc
	(my-process-start
	 my-openssl-program
	 "enc" "-e" (concat "-" algorithm)
	 "-pass" "stdin"
	 "-out" (convert-standard-filename out-file)
	 )))
    (my-openssl-process-send-password proc passwd)
    (process-send-region proc (point-min) (point-max))
    (process-send-eof proc)
    proc))

(defun my-openssl-decrypt-file-to-stdout (in-file algorithm passwd)
  "Decrypt file and write to stdout"
  (let*
      ((proc
	(my-process-start
	 my-openssl-program
	 "enc" "-d" (concat "-" algorithm)
	 "-pass" "stdin"
	 "-in"  (convert-standard-filename in-file)
	 )))
    (my-openssl-process-send-password proc passwd)
    proc))


(defun my-openssl-find-cipher-algorithm (file-name)
  (cdr (assoc (file-name-extension file-name)
	      my-openssl-cipher-algorithm-alist))
  )

(defun my-openssl-read-cipher-algorithm (&optional file-name)
  (let ((algorithm (if file-name (my-openssl-find-cipher-algorithm file-name))))
    (if algorithm algorithm
      (completing-read
       "Algorithm: "
       (mapcar #'cdr my-openssl-cipher-algorithm-alist)
       nil nil (cdr (car my-openssl-cipher-algorithm-alist))))
    ))

(defun my-openssl-encrypt ()
  (interactive)
  (let*
      ((algorithm (my-openssl-read-cipher-algorithm my-openssl-buffer-filename))
       (out-file (or my-openssl-buffer-filename
		     (expand-file-name
		      (convert-standard-filename
		       (read-file-name
			"File: " nil nil nil
			(concat (file-name-nondirectory (buffer-file-name))
				"."
				(car (rassoc algorithm my-openssl-cipher-algorithm-alist))
				))))))
       (passwd (or my-openssl-buffer-cipher-password 
		   (read-passwd "Password: " t)))
       (proc (my-openssl-encrypt-buffer-to-file out-file algorithm passwd)))
    (set-process-sentinel
     proc
     (lambda (proc event)
       (when (string-match "finished" event)
	 (set-buffer-modified-p nil)
	 (set-visited-file-modtime)
	 (setq my-openssl-buffer-cipher-algorithm algorithm)
	 (setq my-openssl-buffer-cipher-password passwd)
	 (setq my-openssl-buffer-filename out-file)
	 (message "Wrote: %s" out-file)
	 )))))

(defun my-openssl-decrypt ()
  (interactive)
  (let*
      ((in-file (expand-file-name
		 (convert-standard-filename (read-file-name "File: "))))
       (algorithm (my-openssl-read-cipher-algorithm in-file))
       (passwd (read-passwd "Password: "))
       (output-sym (gensym))
       (proc (my-openssl-decrypt-file-to-stdout in-file algorithm passwd)))
    (set-process-filter
     proc
     (my-process-create-keep-output-filter output-sym))
    (set-process-sentinel
     proc
     (lambda (proc event)
       (if (string-match "finished" event)
	   (progn
	     (pop-to-buffer
	      (get-buffer-create (file-name-nondirectory in-file)))
	     (insert (symbol-value output-sym))
	     (goto-char (point-min))
	     (set-auto-mode)
	     (setq my-openssl-buffer-cipher-algorithm algorithm)
	     (setq my-openssl-buffer-cipher-password passwd)
	     (setq my-openssl-buffer-filename in-file)
	     (my-openssl-setup-keymap)
	     )
	 (message "Error: %s" event))
       ))))

(defun my-openssl-load (filename)
  (let*
      ((in-file filename)
       (algorithm (my-openssl-read-cipher-algorithm in-file))
       (passwd (read-passwd "Password: "))
       (output-sym (gensym))
       (proc (my-openssl-decrypt-file-to-stdout in-file algorithm passwd)))
    (message "Decrypt %s ..." filename)
    (set-process-filter
     proc
     (my-process-create-keep-output-filter output-sym))
    (set-process-sentinel
     proc
     (lambda (proc event)
       (if (string-match "finished" event)
	   (progn
	     (erase-buffer)
	     (insert (symbol-value output-sym))
	     (goto-char (point-min))
	     (set-auto-mode)
	     (setq my-openssl-buffer-cipher-algorithm algorithm)
	     (setq my-openssl-buffer-cipher-password passwd)
	     (setq my-openssl-buffer-filename in-file)
	     (my-openssl-setup-keymap)
	     (set-buffer-modified-p nil)
	     (set-visited-file-modtime)
	     (message "Done")
	     )
	 (message "Error: %s" event))
       ))))


;;;
;;; hook
;;;
(defun my-openssl-after-find-file ()
  (when (string-match my-openssl-file-suffix-regex (buffer-file-name))
    (if (= (buffer-size) 0)
	(progn
	  (set-auto-mode)
	  (setq my-openssl-buffer-filename (buffer-file-name))
	  (my-openssl-setup-keymap))
      (my-openssl-load (buffer-file-name)))))

(add-hook 'find-file-hooks 'my-openssl-after-find-file)

(defun my-openssl-kill-buffer ()
  (when (and (boundp 'my-openssl-buffer-filename) 
	     my-openssl-buffer-filename
	     (buffer-modified-p))
    (my-openssl-encrypt)))

(add-hook 'kill-buffer-hook 'my-openssl-kill-buffer)


;;;
;;; End
;;;
(provide 'my-openssl)
