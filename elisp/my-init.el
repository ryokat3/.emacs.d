;;; -*- mode:Emacs-Lisp; coding:utf-8; lexical-binding:t -*-
;;;
;;; init.el -- Emacs init file
;;;
;;;
;;; SVN Reository:
;;;
;;;  - https://myprojectroot.googlecode.com/svn/trunk/myemacs/.emacs.d/
;;;  - username: wak109
;;;  - password: generated in https://code.google.com/hosting/settings
;;;
;;;
;;; Note:
;;;  - Google code accepts 'https' when commiting but never 'http'
;;;

;;;
;;; Customize
;;;
;;; Windows:
;;;    (setq my-svn-command "C:\\Program Files\\SlikSvn\\bin\\svn.exe")
;;;

;;;
;;; Variable Declaratoin
;;
(defvar my-http-proxy nil "HTTP Proxy")
(defvar my-https-proxy nil "HTTPS Proxy")
(defvar my-ftp-proxy nil "FTP Proxy")
(defvar my-no-proxy (list "localhost" "127.0.0.1") "No Proxy")
(defvar my-url-proxy-services nil "Proxy Services")
(defvar my-curl-command nil "cURL command")
(defvar my-local-file-cache-directory nil "Directory for cache")
(defvar my-svn-command nil "Subversion CUI command")
(defvar my-svn-username nil "Username for subversion")
(defvar my-svn-password nil "Password for subversion")
(defvar my-process-output-buffer "*Process Output*" "Buffer name for output from external process")
(defvar my-xrea-host nil "XREA Host")
(defvar my-xrea-username nil "XREA Username")
(defvar my-xrea-password nil "XREA Password")
(defvar my-home-directory nil "HOME Directory")
(defvar my-shell-file-name nil "Shell Command")
(defvar my-org-toodledo-userid nil "Toodledo Unique ID")
(defvar my-org-toodledo-password nil "Tooldedo Passowrd")
(defvar my-org-directory nil "Default Org Directory")
(defvar my-dropbox-directory nil "Default Dropbox Directory")
(defvar my-opening-file nil "Default File to open at start up")
(defvar my-publishing-base-directory nil "Org Directory to publish")
(defvar my-publishing-directory nil "Default HTML Directory")


;;;
;;; Basic Library
;;;
(require 'cl)
;(require 'cl-lib)

;;;
;;; My Function
;;;
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



;;;
;;; Basic Settings
;;;
(setq make-backup-files nil)
(define-key global-map "\C-h" 'delete-backward-char)
(global-set-key "\M-?" 'help-command)
(global-set-key [M-kanji] 'ignore)

(setq max-specpdl-size 5000)
(setq max-lisp-eval-depth 5000)
(setq resize-mini-windows t)

;;;
;;; Emacs UI
;;;
(setq inhibit-startup-message t)
(if window-system
    (menu-bar-mode 1)
  (menu-bar-mode -1))
(setq truncate-lines t)

;;;
;;; Settings for Japanese
;;;
(set-language-environment "Japanese")

(eval-after-load "quail"
  '(progn
     (define-key quail-translation-keymap "\C-h"
       'quail-delete-last-char)
     (define-key quail-translation-keymap "\C-?"
       'quail-translation-help)
     (define-key quail-conversion-keymap "\C-h"
       'quail-conversion-backward-delete-char)
     (define-key quail-conversion-keymap "\C-?"
       'quail-translation-help)
     ))
(eval-after-load "kkc"
  '(progn
     (define-key kkc-keymap "\C-h" 'kkc-cancel)
     (define-key kkc-keymap "\C-?" 'kkc-help)
     ))
(load "quail/japanese")
(setq quail-japanese-use-double-n t)
(setq redisplay-dont-pause t)


;;;
;;; System Type
(setq windows-p (eq system-type 'windows-nt))
(setq linux-p (eq system-type 'gnu/linux))
(setq cygwin-p (eq system-type 'cygwin))

;;;
;;; Japanese Fonts
;;;
(if windows-p
    (progn
      (setq ms-gothic-string (encode-coding-string "ＭＳ ゴシック" 'sjis))
      (set-default-font (concat ms-gothic-string " 12"))
      (set-fontset-font (frame-parameter nil 'font)
			'japanese-jisx0208
			(cons ms-gothic-string "unicode-bmp")
			)
      (set-fontset-font (frame-parameter nil 'font)
			'katakana-jisx0201
			(cons ms-gothic-string "unicode-bmp")
			)))

;;; 日本語入力のための設定
(if windows-p
    (progn
      (set-keyboard-coding-system 'cp932)
      (prefer-coding-system 'utf-8-dos)
      (set-file-name-coding-system 'cp932)
      (setq default-process-coding-system '(cp932 . cp932))
      (setq default-input-method "W32-IME")
      (setq-default w32-ime-mode-line-state-indicator "[Aa]")
      (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
      (setq w32-ime-buffer-switch-p nil)
      (w32-ime-initialize)
      ))

(if cygwin-p
    (progn
      (setq default-file-name-coding-system 'utf-8)
      (prefer-coding-system 'utf-8-dos)
      (when (eq window-system 'w32)
	(setq ms-gothic-string (encode-coding-string "ＭＳ ゴシック" 'sjis))
	(set-default-font (concat ms-gothic-string " 12"))
	(set-fontset-font (frame-parameter nil 'font)
			  'japanese-jisx0208
			  (cons ms-gothic-string "unicode-bmp")
			  )
	(set-fontset-font (frame-parameter nil 'font)
			  'katakana-jisx0201
			  (cons ms-gothic-string "unicode-bmp")
			  )
	(when (equal "DELL-PC" (getenv "COMPUTERNAME"))
	  (setq initial-frame-alist
		(append '((top . 10)(left.10)
			  (width . 120)(height . 45)) initial-frame-alist))
	  )
	(when (equal "E7E4115B54C2B9" (getenv "COMPUTERNAME"))
	  (setq initial-frame-alist
		(append '((top . 10)(left.10)
			  (width . 120)(height . 40)) initial-frame-alist))
	  )
	)))

(if linux-p
    (progn
      (set-keyboard-coding-system 'utf-8-unix)
      (prefer-coding-system 'utf-8-unix)
      (set-file-name-coding-system 'utf-8-unix)
      (setq default-process-coding-system '(utf-8 . utf-8))

      (load-library "anthy")
      (setq default-input-method "japanese-anthy")
      ))

;;;
;;; IME common
;;;
(set-cursor-color "red")
(add-hook 'input-method-activate-hook
	  (lambda() (set-cursor-color "green")))
(add-hook 'input-method-inactivate-hook
	  (lambda() (set-cursor-color "red")))

;;;
;;; Emacs Server
;;;
(require 'server)
(unless (server-running-p)
  (server-start)) ;; emacsclient

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; local
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; load-path
;;
(let ((emacs-lisp-dir
       (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path emacs-lisp-dir)
  (let ( ; default-directory is used in normal-to-level-add-subdirs-to-load-path
	(default-directory (expand-file-name "elisp" emacs-lisp-dir))
	(package-directory (expand-file-name "elpa" emacs-lisp-dir))
	)
    (add-to-list 'load-path default-directory)
    (add-to-list 'load-path package-directory)
    (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	(normal-top-level-add-subdirs-to-load-path))))



;;
;; my-xxx.el
;;
(require 'my-common)

(fset 'safe-require (my-func-safe #'require))
(fset 'safe-load-library (my-func-safe #'load-library))

(safe-require 'my-func)
(safe-require 'my-openssl)
(safe-require 'my-curl)


;;;
;;; Configuration
;;;
(if (string-equal system-type "cygwin")
    (safe-load-library "my-config")
  (safe-load-library "my-config-windows-nt"))

;;;
;;; Cygwin
;;;
(when (string-equal system-type "cygwin")
  (setenv "CYGWIN" "tty nodosfilewarning")
)

;;;
;;; Home Directory
;;;
(unless (null my-home-directory)
  (setenv "HOME" my-home-directory))

;;;
;;; Shell Mode
;;;
(setq explicit-shell-file-name my-shell-file-name)
(modify-coding-system-alist 'process ".*sh\\.bat" '(undecided-dos . euc-jp-unix))
;; (M-! and M-| and compile.el)
(setq shell-file-name "bash.exe")
(modify-coding-system-alist 'process ".*sh\\.exe" 'cp932)

;; Surpress ^M in shell-mode
(add-hook 'comint-output-filter-functions 'shell-strip-ctrl-m nil t)

;; Completion in shell-mode (for drive letter)
(setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@'`.,;()-")

;; Configuration for escape sequence
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)

(setq shell-mode-hook
      (function
       (lambda ()
	 ;; coding code in shell-mode
	 (set-buffer-process-coding-system 'sjis-dos 'sjis-unix)
	 (set-buffer-file-coding-system    'sjis-unix)
	 )))
;;;
;;; Macos
;;;
(defmacro defun-with-cached-result (func args &rest blocks)
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

;;;
;;; Functions
;;;
(defun my-flat-list (tree &optional fn)
  "Make a flat list from tree. Remove nil"
  (cl-labels
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

(defun my-command-options (command-option-list)
  (my-flat-list
   (if (listp command-option-list)
       command-option-list
     (list command-option-list))
   #'(lambda (opt)
       (cond
	((stringp opt) opt)
	((numberp opt) (number-to-string opt))
	(t nil)))))

(defun my-call-process (command &rest command-option-list)
  (with-temp-buffer
    (lexical-let
	((process-exit-code
	  (apply #'call-process command nil
		 (current-buffer) nil
		 (my-command-options command-option-list))))
      (list process-exit-code (buffer-string)))))

(defun my-call-process-with-output (command &rest command-option-list)
  (save-selected-window
    (lexical-let
	((process-result (my-call-process command command-option-list)))
      (pop-to-buffer (get-buffer-create my-process-output-buffer))
      (goto-char (point-max))
      (insert (concat
	       "\n\n================ " (current-time-string)
	       " ================\n\n"
	       (convert-standard-filename command) " "
	       (mapconcat 'identity (my-command-options command-option-list)" ")
	       "\n\nProcess Exit Code: " (number-to-string (car process-result))
	       "\n\nProcess Output:\n"
	       (cadr process-result)))
      process-result)))

(defun my-make-directory (path &optional is_file)
  (cl-labels
      ((rec
	(pdir sdir-list)
	(cond
	 ((null pdir) (rec (car sdir-list) (cdr sdir-list)))
	 ((null sdir-list) pdir)
	 ((and is_file (eq (cdr sdir-list) nil)) pdir)
	 (t
	  (let
	      ((curdir (expand-file-name (car sdir-list) pdir)))
	    (if (not (file-directory-p curdir))
		(make-directory curdir))
	    (rec curdir (cdr sdir-list)))))))
    (rec nil
	 (remove-if #'(lambda (str) (string-equal str ""))
		    (split-string (expand-file-name path) "/")))))

;;;
;;; exec-path
;;;
(dolist
    (dir
     (list
      "/sbin"
      "/usr/sbin"
      "/bin"
      "/usr/bin"
      "/opt/bin"
      "/usr/local/bin"
      (expand-file-name "~/bin")
      ))

(when (and (file-exists-p dir) (not (member dir exec-path)))
   (setenv "PATH" (concat dir ":" (getenv "PATH")))
   (setq exec-path (append (list dir) exec-path))))

;;;
;;; reStructuredTest
;;;
(safe-load-library "rst")

;;;
;;; Navi2ch
;;;
(safe-load-library "navi2ch")

;;;
;;; HTTP Proxy
;;;
;;; url-generic-parse-url
;;;
;;; The CL-style struct contains the following fields:
;;; TYPE USER PASSWORD HOST PORTSPEC FILENAME TARGET ATTRIBUTES FULLNESS.
;;; 1    2    3        4    5        6        7      8          9
;;;
(require 'url-parse)

;(defalias 'defcompose 'my-func-defcompose)

;(defcompose my-url-get-type (elt (url-generic-parse-url $1) 1))
;(defcompose my-url-get-user (elt (url-generic-parse-url $1) 2))
;(defcompose my-url-get-password (elt (url-generic-parse-url $1) 3))
;(defcompose my-url-get-host (elt (url-generic-parse-url $1) 4))
;(defcompose my-url-get-port (elt (url-generic-parse-url $1) 5))
;(defcompose my-url-get-filename (elt (url-generic-parse-url $1) 6))
;(defcompose my-url-get-target (elt (url-generic-parse-url $1) 7))
;(defcompose my-url-get-attributes (elt (url-generic-parse-url $1) 8))
;(defcompose my-url-get-fullness (elt (url-generic-parse-url $1) 9))

(defun my-net-split-host-port (hostport)
  (when (string-match "^\\(.+\\):\\([0-9]+\\)$" hostport)
    (list (match-string 1 hostport)
	  (string-to-number (match-string 2 hostport)))
    ))

(defun my-net-tcp-port-check (host port)
  "Check TCP connection
HOST (string) :: host name
PORT (int) :: port number
"
  (let ((bufname " my-net-tcp-port-check")
	(proc nil)
	)
    (condition-case nil
	(progn
	  (setq proc (open-network-stream bufname nil host port))
	  (set-process-query-on-exit-flag proc nil)
	  t)
      (error nil))))

(defun my-net-available-tcp-port-list (host-port-list)
  "services :: list of (host[string] port[int])"
  (delq nil (mapcar (lambda (host-port)
		      (when (apply #'my-net-tcp-port-check host-port nil)
			host-port)) host-port-list)))

(defun my-net-available-proxy-service-list (proxy-service-list)
  "Check the available proxy service"
  (cl-labels
      ((tcp-port-check
	(host port)
	(apply (my-func-cacheable #'my-net-tcp-port-check) '(host port))))
    (delq nil
	  (mapcar
	   (lambda (proxy-service)
	     (when (apply #'tcp-port-check
			  (my-net-split-host-port (cadr proxy-service)))
	       proxy-service))
	   proxy-service-list))))


(defun my-net-url-check (url)
  "Check the availabity of TCP port of URL"
  (let ()
    (setq urlobj (url-generic-parse-url url))
    (setq host (elt urlobj 4)) ; Host
    (setq port (elt urlobj 5)) ; Port
    (my-net-tcp-port-check host port)))

(defun-with-cached-result my-net-is-http-proxy-available (http-proxy-url)
	  (if (my-net-url-check http-proxy-url)
	      (progn
		(setq urlobj (url-generic-parse-url http-proxy-url))
		(setq http-proxy-host (elt urlobj 4)) ; Host
		(setq http-proxy-port (elt urlobj 5)) ; Port
		t
		)
	    ()))

(defun my-net-make-http-proxy-options (http-proxy-url proxy-option-func)
  (if (my-net-is-http-proxy-available http-proxy-url)
      (funcall proxy-option-func http-proxy-url)))

;;;
;;; Set url-proxy-services
;;;
(defun proxy-init ()
  (interactive)
  (if (my-net-is-http-proxy-available my-http-proxy)
      (let*
	  ((urlobj (url-generic-parse-url my-http-proxy))
	   (http-proxy-method (elt urlobj 1)) ; Method
	   (http-proxy-host (elt urlobj 4)) ; Host
	   (http-proxy-port (elt urlobj 5))) ; Port
	(setq url-proxy-services
	      (list (cons http-proxy-method
			  (concat http-proxy-host ":"
				  (number-to-string http-proxy-port))))))))


;;;
;;; Subversion (call SlikSVN)
;;;
(defconst my-svn-command-help "help")
(defconst my-svn-command-update "update")
(defconst my-svn-command-commit "commit")

(defun svn-help ()
  (interactive)
;  (my-call-process-with-output my-svn-command "help")
;  (switch-to-buffer (get-buffer-create my-process-output-buffer)))
;  (test-call-process-with-output my-svn-command "help"))
  (my-call-process-with-output my-svn-command "help"))

(defun svn-http-proxy-options (http-proxy-url)
  (let*
      ((urlobj (url-generic-parse-url http-proxy-url))
       (http-proxy-host (elt urlobj 4)) ; Host
       (http-proxy-port (elt urlobj 5))) ; Port
    (list
     "--config-option"
     (concat "servers:global:http-proxy-host=" http-proxy-host)
     "--config-option"
     (concat "servers:global:http-proxy-port="
	     (number-to-string http-proxy-port)))))

(defun svn-commit ()
  (interactive)
  (car
   (my-call-process-with-output
    my-svn-command "commit"
    (my-net-make-http-proxy-options my-http-proxy #'svn-http-proxy-options)
    "--username" my-svn-username
    "--password" my-svn-password
    "-m" "emacs"
    (convert-standard-filename buffer-file-name))))

(defun svn-update ()
  (interactive)
  (car
   (my-call-process-with-output
    my-svn-command "update"
    (my-net-make-http-proxy-options my-http-proxy #'svn-http-proxy-options)
    "--username" my-svn-username
    "--password" my-svn-password
    (convert-standard-filename buffer-file-name))))

(defun svn-add ()
  (interactive)
  (my-call-process-with-output
   my-svn-command "add"
   (convert-standard-filename buffer-file-name)))

(defun svn-delete ()
  (interactive)
  (my-call-process-with-output
   my-svn-command "delete"
   (convert-standard-filename buffer-file-name)))

(defun svn-revert ()
  (interactive)
  (my-call-process-with-output
   my-svn-command "revert"
   (convert-standard-filename buffer-file-name)))

;;;
;;; Gauche
;;;
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(setq scheme-program-name "gosh -i")

(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))
(define-key global-map "\C-cS" 'scheme-other-window)

;;;
;;; XREA
;;;
(defun my-curl-http-proxy-options (http-proxy-url)
  (let*
      ((urlobj (url-generic-parse-url http-proxy-url))
       (http-proxy-host (elt urlobj 4)) ; Host
       (http-proxy-port (elt urlobj 5))) ; Port
    (list
     "--proxy"
     (concat http-proxy-host ":" (number-to-string http-proxy-port)))
    ))

(defun xrea-init ()
  "Register IP address to XREA"
  (interactive)
  (lexical-let*
      ((process-result
	(my-call-process-with-output
	 my-curl-command
	 (my-net-make-http-proxy-options
	  my-http-proxy #'my-curl-http-proxy-options)
	 "--silent"
	 "http://dyn.value-domain.com/cgi-bin/dyn.fcg?ip"))
       (my-ip-address (cadr process-result)))
    (if (and (eq (car process-result) 0)
	     (not (null my-ip-address)))
	(progn
	  (my-call-process-with-output
	   my-curl-command
	   (my-net-make-http-proxy-options my-http-proxy #'my-curl-http-proxy-options)
	   "--show-error" "--insecure" "--silent"
	   "-d" (xrea-make-ssh-host-register-data my-ip-address)
	   (concat "https://ss1.xrea.com/www." my-xrea-host
		   ".xrea.com/jp/admin.cgi"))
	  (my-call-process-with-output
	   my-curl-command
	   (my-net-make-http-proxy-options my-http-proxy #'my-curl-http-proxy-options)
	   "--show-error" "--insecure" "--silent"
	   "-d" (xrea-make-ftp-host-register-data my-ip-address)
	   (concat "https://ss1.xrea.com/www." my-xrea-host
		   ".xrea.com/jp/admin.cgi"))
	  ))))

(defun xrea-make-ssh-host-register-data(my-ip-address)
  "Make POST data to register SSH Client"
  (concat
   "id=" my-xrea-username
   "&pass=" my-xrea-password
   "&remote_host=" my-ip-address
   (encode-coding-string "&ssh2=SSH登録" 'utf-8)))

(defun xrea-make-ftp-host-register-data(my-ip-address)
  "Make POST data to register FTP Client"
  (concat
   "id=" my-xrea-username
   "&pass=" my-xrea-password
   "&remote_host=" my-ip-address
   (encode-coding-string "&telnet2=FTP登録" 'utf-8)))

(defun xrea-download (file-path)
  "Download a file from URL by using curl"
  (interactive
   (list (read-from-minibuffer "File: ")))
  (progn
    (curl-download
     (concat "ftp://" my-xrea-host ".xrea.com/public_html/" file-path)
     my-xrea-username my-xrea-password)))

(defun xrea-upload () (interactive) (curl-upload my-xrea-username my-xrea-password))

;;;
;;; File
;;;
;;; Download a file with cURL command.
;;; The downloaded file is saved under the directory derived from URL
;;;
;;; <my-local-file-cache-directory>/<method>/<host>/<port>/<path>/.../<file-name>
;;;

(defun my-file-get-local-filename-from-url (url)
  "Make a local filename from URL"
  (let ((urlobj (url-generic-parse-url url)))
    (expand-file-name
     (concat "." (elt urlobj 6))
     (expand-file-name
      (number-to-string (elt urlobj 5))
      (expand-file-name
       (elt urlobj 4)
       (expand-file-name
	(elt urlobj 1)
	my-local-file-cache-directory))))))

(defun my-file-get-url-from-local-filename (filename)
  "Make a URL from the local filename"
  (let ((dir-list
	 (split-string
	  (file-relative-name
	   (expand-file-name filename)
	   (expand-file-name my-local-file-cache-directory)) "/")))
    (concat (car dir-list) "://" (cadr dir-list) ":" (caddr dir-list) "/"
	    (mapconcat 'identity (cdddr dir-list) "/"))))


;;;
;;; Curl
;;;

(setq curl-option-help "--help")
(setq curl-option-silent "--silent")
(setq curl-option-output-file "--output")

(defun my-curl-download-from-url (file-url &optional username password)
  "Download a file from URL and save it"
  (lexical-let*
      ((filename (my-file-get-local-filename-from-url file-url))
       (tempfile (make-temp-file "curl-download-temporary"))
       (process-result
	(my-call-process-with-output
	 my-curl-command
	 (my-net-make-http-proxy-options my-http-proxy #'my-curl-http-proxy-options)
	 "--output"
	 (convert-standard-filename tempfile)
	 "--user" (concat username ":" password)
	 "--quote" "TYPE I"
	 "--url" file-url)))
    (if (eq (car process-result) 0)
	(progn
	  (my-make-directory filename t)
	  (rename-file tempfile filename 1)))
    ))

(defun curl-download (file-url &optional username password)
  "Download a file from URL by using curl"
  (interactive
   (list (read-from-minibuffer "URL: ")))
  (progn
    (my-curl-download-from-url file-url username password)
    (find-file (my-file-get-local-filename-from-url file-url))))

(defun curl-upload-to-url (file-url &optional filename username password)
  "Upload a file to URL by using curl"
  (interactive
   (list
    (read-from-minibuffer "URL: ")))
  (progn
    (if (null filename)
	(setq filename (my-file-get-local-filename-from-url file-url)))
    (my-call-process
     my-curl-command
     (my-net-make-http-proxy-options my-http-proxy #'my-curl-http-proxy-options)
     "--upload-file"
     (convert-standard-filename filename)
     "--user" (concat username ":" password)
     "--quote" "TYPE I"
     "--url" file-url)))

(defun curl-help ()
  "Show the help of cURL command"
/ex  (interactive)
  (my-call-process-with-output
   my-curl-command
   "--help"))

(defun curl-manual ()
  "Show the full manual of cURL command"
  (interactive)
  (my-call-process-with-output
   my-curl-command
   "--manual"))

(defun curl-upload (&optional username password)
  "Upload the file of current buffer to URL by using cURL"
  (interactive)
  (curl-upload-to-url
   (my-file-get-url-from-local-filename buffer-file-name)
   buffer-file-name username password))


(defun curl-directory-list (path)
  (remove-if
   #'(lambda (dirname) (or (string= dirname ".") (string= dirname "..")))
   (directory-files path)))

(defun curl-all-directory-list (path)
  (cl-labels
      ((rec
	(dirname)
	(cons dirname
	      (mapcar
	       #'(lambda (child-dir)
		   (rec (concat dirname "/" child-dir)))
	       (curl-directory-list dirname)))))
    (let ((topdir (expand-file-name path)))
      (mapcar
       #'(lambda (dirname) (file-relative-name dirname topdir))
       (my-flat-list (rec topdir))))))

(defun curl-path-to-url (path)
  (let ((dir-list (split-string path "/")))
    (concat (car dir-list) "://"
	    (if (cadr dir-list)
		(concat (cadr dir-list) ":"
			(if (caddr dir-list)
			    (concat (caddr dir-list) "/"
				    (if (cdddr dir-list)
					(mapconcat 'identity (cdddr dir-list) "/")
				      ""))
			  ""))
	      ""))))
				    
(defun curl-read-url ()
  "Read a URL from mini buffer"
  (interactive)
  (completing-read
   "URL: " ; prompt
   (mapcar #'curl-path-to-url
	   (curl-all-directory-list my-local-file-cache-directory))
   nil
   t))

;;;
;;; Package
;;;
(when (safe-require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize)
  (safe-require 'melpa) ;; melpa
  (when (my-net-is-http-proxy-available my-http-proxy)
    (setq url-proxy-services my-url-proxy-services))
  )

;;;
;;; auto-install
;;;
(when (safe-load-library "auto-install")
  (add-to-list 'load-path auto-install-directory)
;  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup)
;  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  )

;;;
;;; Byte Compile
;;;
(when (safe-require 'auto-async-byte-compile)
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/\\|init.el\\|my-config.el")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
  )

;;;
;;; Haskell
;;;
(when (safe-load-library "haskell-site-file")
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  )

;;;
;;; Evernote
;;;
(when (safe-load-library "evernote-mode")
  (setq evernote-username my-evernote-username)
  (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8"))
  (global-set-key "\C-cec" 'evernote-create-note)
  (global-set-key "\C-ceo" 'evernote-open-note)
  (global-set-key "\C-ces" 'evernote-search-notes)
  (global-set-key "\C-ceS" 'evernote-do-saved-search)
  (global-set-key "\C-cew" 'evernote-write-note)
  (global-set-key "\C-cep" 'evernote-post-region)
  (global-set-key "\C-ceb" 'evernote-browser)
  (global-set-key "\C-cel" 'evernote-browsing-list-notebooks)
  )


;;;
;;; BAT mode
;;;
(when (safe-load-library "batch-mode")
  (setq auto-mode-alist 
	(append 
	 (list (cons "\\.[bB][aA][tT]$" 'batch-mode))
	 ;; For DOS init files
	 (list (cons "CONFIG\\."   'batch-mode))
	 (list (cons "AUTOEXEC\\." 'batch-mode))
	 auto-mode-alist)
	))
;;;
;;; Markdown Mode
;;;
(when (safe-load-library "markdown-mode")
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))

;;;
;;; w3m
;;;
(when (safe-load-library "w3m")
  (setq w3m-home-page "http://www.google.co.jp/")
  (setq w3m-use-cookies t)
  (setq w3m-redirect-with-get t)
  (setq w3m-follow-redirection 32)
  (setq w3m-bookmark-file "~/.w3m/bookmark.html")
  (when (my-net-is-http-proxy-available my-http-proxy)
    (setq w3m-command-arguments 
	  (list "-o" "use_proxy=1"
		"-o" (concat "http_proxy=" my-http-proxy)
		"-o" (concat "https_proxy=" my-https-proxy)
		"-o" (concat "ftp_proxy=" my-ftp-proxy)))
    (setq w3m-no-proxy-domains my-no-proxy))
  )

;;;
;;; popwin
;;;
(when (safe-load-library "popwin")
  (setq display-buffer-function 'popwin:display-buffer)
  )

;;;
;;; direx & popwin
;;;
(when (safe-load-library "direx")
  (when (safe-load-library "popwin")
    (push '(direx:direx-mode :position left :width 32 :dedicated t)
	  popwin:special-display-config))
  (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)
  )

;;;
;;; org mode & tree menu
;;;
(when (safe-require 'imenu-tree)
  (add-hook 'org-mode-hook
	    (lambda()
	      (require 'imenu-tree)))
  (global-set-key (kbd "M-h") 'imenu-tree))

;;;
;;; Mew
;;;
(when (safe-require 'mew)
  (autoload 'mew "mew" nil t)
  (autoload 'mew-send "mew" nil t)
  (setq mew-fcc "+outbox") ; 送信メールを保存
					; Stunnel
  (setq mew-prog-ssl "/bin/stunnel.exe")
					; IMAP for Gmail
  (setq mew-proto "%")
  (setq mew-imap-server "imap.gmail.com")
  (setq mew-imap-user "example@gmail.com")
  (setq mew-imap-auth  t)
  (setq mew-imap-ssl t)
  (setq mew-imap-ssl-port "993")
  (setq mew-smtp-auth t)
  (setq mew-smtp-ssl t)
  (setq mew-smtp-ssl-port "465")
  (setq mew-smtp-user "wak109@gmail.com")
  (setq mew-smtp-server "smtp.gmail.com")
  (setq mew-fcc "%Sent") ; 送信メイルを保存する
  (setq mew-imap-trash-folder "%[Gmail]/ゴミ箱")
  (setq mew-use-cached-passwd t)
  (setq mew-ssl-verify-level 0)
  )

;;;
;;; org-bullet
;;;
;(when (safe-require 'org-bullets)
;  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;  (setq org-bullets-bullet-list '("■" "●" "◆" "□" "○" "◇" )))

;;;
;;; GPG
;;;
(when (safe-require 'epa-file)
  (epa-file-enable)
  (setq epa-file-cache-passphrase-for-symmetric-encryption t))

;;;
;;; Org Mode
;;;
(setq org-directory my-org-directory)
(setq org-default-notes-file (expand-file-name "memo.org" org-directory))

(require 'org-install)
(require 'org-capture)
(setq org-capture-templates
      '(
        ("p" "Password" item (file+headline "password.org.gpg" "Inbox")
         "- %? :: [ID]  [PW]\n")
	("t" "Task" entry (file+headline nil "Task")
         "** TODO %?\n %T\n")
        ("n" "Notes" entry (file+headline nil "Note")
         "** %?\n %U\n")
        ))
(global-set-key (kbd "C-c c") 'org-capture)

;;;
;;; Theme
;;;
(load-theme 'wheatgrass t)

;;;
;;; Org OPML
;;;
(defun opml2org ()
  (interactive)
  (let*
      ((script (concat my-dropbox-directory "/rb/opml2org.rb")) ;script file
       (input-dir (concat my-dropbox-directory "/Outliner/")) ;input
       (output-file (concat my-dropbox-directory "/org/outliner.org")) ;output
       (command (concat "ruby -Ku " script " " input-dir " " output-file))) 
    (shell-command command)
    (if (not (eq (get-buffer "outliner.org") nil))
        (find-file-noselect output-file)
      ())
    ))

(defun org2opml ()
  (interactive)
  (let*
      ((script (concat my-dropbox-directory "/rb/org2opml.rb")) ;script file
       (outliner-dir (concat my-dropbox-directory "/Outliner/")) ;input
       (org-file (concat my-dropbox-directory "/org/outliner.org")) ;output
       (command (concat "ruby -Ku " script " " org-file " " outliner-dir))) 
    (shell-command command)
    ))
(add-hook 'kill-emacs-hook 'org2opml)
(add-hook 'emacs-startup-hook 'opml2org)
(global-set-key (kbd "C-c C-o") (lambda () (interactive) (find-file (concat my-dropbox-directory "/org/outliner.org"))))

;;;
;;; anything
;;;
(when (safe-require 'anything)
  (safe-require 'anything-config)
  )

;;;
;;; org-toodledo
;;;
(when (safe-require 'org-toodledo)
  (setq org-toodledo-userid my-org-toodledo-userid)
  (setq org-toodledo-password my-org-toodledo-password)
  (setq org-toodledo-sync-on-save "yes")
  (setq org-toodledo-folder-support-mode 'heading)
  (add-hook 'org-mode-hook
	    (lambda ()
	      (local-unset-key "\C-o")
	      (local-set-key "\C-od" 'org-toodledo-mark-task-deleted)
	      (local-set-key "\C-os" 'org-toodledo-sync)))
  (add-hook 'org-agenda-mode-hook
	    (lambda ()
	      (local-unset-key "\C-o")
	      (local-set-key "\C-od" 'org-toodledo-agenda-mark-task-deleted)))
  )


;;;
;;; Tramp
;;;
(require 'tramp)
(setq tramp-default-method "ssh")
(setq password-cache-expiry nil)

;;;
;;;
;(when (and my-opening-file (file-exists-p my-opening-file))
;  (find-file my-opening-file))

;;;
;;; Org Publish
;;;
(require 'org-publish)
(setq org-publish-project-alist
      `(
       ("org-notes"
         :base-directory ,my-publishing-base-directory
	 :base-extension "org"
         :publishing-directory ,my-publishing-directory
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4
	 :auto-preamble t
	 )
       ("org-static"
	 :base-directory ,my-publishing-base-directory
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory ,my-publishing-directory
	 :recursive t
	 :publishing-function org-publish-attachment
	 )
       ("org" :components ("org-notes" "org-static"))
	))
(when (require 'org-install nil t)
  (setq org-export-htmlize-output-type 'css))

