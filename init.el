;;; -*- mode:Emacs-Lisp; coding:utf-8; lexical-binding:t -*-
;;;
;;; Minimum Emacs init.el
;;;
;;;

;;;
;;; README
;;;
;;; Windows 7 - 10 ::
;;;
;;; ~/AppData/Roaming/.emacs.d/init.el
;;;
;;; (setenv "HOME" "C:/Users/XXXX")
;;; (setq user-emacs-directory "~/.emacs.d")
;;; (load-file (expand-file-name "init.el" user-emacs-directory))
;;;

;;;
;;; Registry File alist
;;;
;;;  ( hostname . registry-file )
;;;
(defconst my-default-registry-file "~/.registry.xml")
(setq my-registry-alist
      '(
	("JP00202153" . "~/Documents/etc/registry.xml")
	("SURFACEPRO3" . "~/Documents/etc/registry.xml")
	))

;;;
;;; Basic Settings
;;;
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(define-key global-map "\C-h" 'delete-backward-char)
(global-set-key "\M-?" 'help-command)
(global-set-key [M-kanji] 'ignore)

(setq max-specpdl-size 5000)
(setq max-lisp-eval-depth 5000)

(setq inhibit-startup-message t)

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
(load "quail/japanese")
(setq quail-japanese-use-double-n t)
(setq redisplay-dont-pause t)

;;;
;;; System Type
(setq windows-p (eq system-type 'windows-nt))
(setq linux-p (eq system-type 'gnu/linux))

;;;
;;; Japanese Fonts
;;;
(when (and window-system windows-p)
  (setq ms-gothic-string (encode-coding-string "MyricaM M" 'sjis))
  (set-default-font (concat ms-gothic-string " 12"))
  (set-fontset-font (frame-parameter nil 'font)
		    'japanese-jisx0208
		    (cons ms-gothic-string "unicode-bmp")
		    )
  (set-fontset-font (frame-parameter nil 'font)
		    'katakana-jisx0201
		    (cons ms-gothic-string "unicode-bmp")
		    )
  )

;; 日本語入力のための設定
(when windows-p
  (set-keyboard-coding-system 'cp932)
  (prefer-coding-system 'utf-8-dos)
  (set-file-name-coding-system 'cp932)
  (setq default-process-coding-system '(cp932 . cp932))
  (setq default-input-method "W32-IME")
  (setq-default w32-ime-mode-line-state-indicator "[Aa]")
  (setq w32-ime-mode-line-state-indicator-list '("[Aa]" "[あ]" "[Aa]"))
  (setq w32-ime-buffer-switch-p nil)
  (w32-ime-initialize)
  )
;;;
;;; Tool Bar
;;;
(tool-bar-mode 0)

;;;
;;; Emacs Server
;;;
(require 'server)
(server-start) ;; emacsclient

;;;
;;; Line Number
;;;
(when (require 'linum nil t)
  (global-linum-mode)
  )

;;;
;;; Semi-transparent
;;;
(when window-system
  (setq default-frame-alist
	(append (list
		 '(alpha . (95 90))
		 ) default-frame-alist))
  )

;;;
;;; MSYS Shell
;;;
(when (and window-system windows-p)
  ;; MSYS の bash を使用します。
  (setq explicit-shell-file-name "c:/local_data/app/msys64/usr/bin/bash.exe")
  (setq shell-file-name "c:/local_data/app/msys64/usr/bin/bash.exe")
  (setq explicit-sh-args '("-login" "-i"))
  ;; SHELL で ^M が付く場合は ^M を削除します。
  (add-hook 'shell-mode-hook
	    (lambda ()
	      (set-buffer-process-coding-system 'undecided-dos 'sjis-unix)))
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  ;; shell-mode での保管(for drive letter)
  (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  )

;;;
;;; Registry
;;;
(require 'xml)
(defun my-registry-add-value (current-value value)
  (cond
   ((null current-value) value)
   ((listp current-value)
    (if (member value current-value)
	current-value
      (cons value current-value)))
   (t
    (if (equal value current-value)
	current-value
      (cons value (cons current-value nil))))))

(defun my-registry-set-value (name value)
  (let ((sym (intern-soft name)))
    (cond
     ((null sym) (set (intern name) value))
     ((not (boundp sym)) (set sym value))
     (t (set sym (my-registry-add-value (symbol-value sym) value))))))

(defun my-registry-parser (node &optional var-name)
  (cond
   ((null node) nil)
   ((stringp node) (my-registry-set-value var-name node))
   ((listp node)
    (if (< (length node) 3)
	(my-registry-parser (car node) var-name)
      (mapcar
       (lambda (child)
	 (my-registry-parser
	  child
	  (concat (if (null var-name) "" (concat var-name "."))
		  (symbol-name (car node)))))
       (xml-node-children node))))
   (t nil)))

(let ((registry-file-name
       (or (cdr
	    (assoc (system-name) my-registry-alist))
	   my-default-registry-file)))
  (if (file-exists-p registry-file-name)    
;      (my-registry-parser (xml-parse-file registry-file-name))))
      (mapcar (lambda (node) (if (listp node) (my-registry-parser node)))
	      (xml-node-children
	       (car (xml-parse-file registry-file-name))))))

;;;
;;; Window Size
;;;
(when window-system
  (let ((frame (selected-frame)))
    (set-frame-size
     frame
     80 ;width
     (- (/ (- (x-display-pixel-height) 40) (frame-char-height)) 5)) ;height
    (set-frame-position frame 5 5)
    ))

;;;
;;; exec-path
;;;
(cond
 ((not (boundp 'pathlist.path)) nil)
 ((listp pathlist.path)
  (mapcar (lambda (path) (setq exec-path (cons path exec-path)))
	  pathlist.path))
 ((stringp pathlist.path)
  (setq exec-path (cons pathlist.path exec-path))))


;;;
;;; load-path
;;;
(let ((emacs-lisp-dir
       (file-name-directory (or load-file-name buffer-file-name))))
  (let (
	(default-directory (expand-file-name "elisp" emacs-lisp-dir)))
    (add-to-list 'load-path default-directory)))

;; auto-insert
;; ファイル形式に応じて自動でテンプレート挿入
(add-hook 'find-file-hooks 'auto-insert)
(setq auto-insert-directory
      (concat
       (file-name-directory (or load-file-name buffer-file-name))
       "template"))
(setq auto-insert-alist
      '(
	(org-mode . "org-template.org")
	))
;;;
;;; melpa
;;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;;;
;;; Theme
;;;
(load-theme 'misterioso t)

;;;
;;; Org
;;;
(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook 'turn-on-iimage-mode)

;;;
;;; web-mode
;;;
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;;;
;;; EWW
;;;
(setq eww-search-prefix "https://www.google.co.jp/search?q=")

;;;
;;; My OpenSSL
;;;
(require 'my-openssl nil t)

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

;;;
;;; Tabbar.el
;;;
(defun my-tabbar-buffer-list ()
  (remove-if
   (lambda (buffer)
     (find (aref (buffer-name buffer) 0) " *"))
   (buffer-list)))

(when (and (require 'tabbar nil t) window-system)
  (tabbar-mode)
  (global-set-key "\M-]" 'tabbar-forward)  ; 次のタブ
  (global-set-key "\M-[" 'tabbar-backward) ; 前のタブ
  (setq tabbar-separator '(1.0))
  ;; タブ上でマウスホイールを使わない
  (tabbar-mwheel-mode nil)
  ;; グループを使わない
  (setq tabbar-buffer-groups-function nil)
  ;; 左側のボタンを消す
  (dolist (btn '(tabbar-buffer-home-button
		 tabbar-scroll-left-button
		 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
		   (cons "" nil))))
  ;; 色設定
  ;;
  (set-face-attribute ; バー自体の色
   'tabbar-default nil
   :background "white"
   :family "Inconsolata"
   :height 0.75)  ; same font size with buffer if height is 1.0
  (set-face-attribute ; アクティブなタブ
   'tabbar-selected nil
   :background "black"
   :foreground "white"
   :weight 'bold
   :box nil)
  (set-face-attribute ; 非アクティブなタブ
   'tabbar-unselected nil
   :background "white"
   :foreground "black"
   :box nil)
  (setq tabbar-buffer-list-function 'my-tabbar-buffer-list)
  )


;;;
;;; https://www49.atwiki.jp/ntemacs/pages/28.html
;;;
;;; fakecygpty
;;;
(when windows-p
  ;; process-connection-type が nil で start-process が
  ;; コールされるけれども、fakecygpty を経由して
  ;; 起動したいプログラムの名称を列挙する
  (defvar fakecygpty-program-list '("bash" "sh" "scp" "ssh"))

  ;; fakecygpty を経由するかを判断してプログラムを起動する
  (advice-add
   'start-process
   :around (lambda (orig-fun &rest args)
	     (when
		 (and (nth 2 args)
		      (or process-connection-type
			  (member
			   (replace-regexp-in-string
			    "\\.exe$" ""
			    (file-name-nondirectory (nth 2 args)))
			   fakecygpty-program-list)))
	       (push "fakecygpty" (nthcdr 2 args)))
	     (apply orig-fun args))
   '((depth . 100)))

  ;; fakecygpty を経由して起動したプロセスに対し、
  ;; コントロールキーを直接送信する
  (cl-loop for (func ctrl-key) in
	   '((interrupt-process "C-c")
	     (quit-process      "C-\\")
	     (stop-process      "C-z")
	     (process-send-eof  "C-d"))
	   do (eval
	       `(advice-add
		 ',func
		 :around (lambda (orig-fun &rest args)
			   (let ((process
				  (or (nth 0 args)
				      (get-buffer-process (current-buffer)))))
			     (if (string= (car (process-command process)) "fakecygpty")
				 (process-send-string (nth 0 args) (kbd ,ctrl-key))
			       (apply orig-fun args)))))))

  (defconst w32-pipe-limit 4096)

  (defun ad-process-send-string (orig-fun &rest args)
    (if (not (eq (process-type (nth 0 args)) 'real))
	(apply orig-fun args)
      (let* ((process (or (nth 0 args)
			  (get-buffer-process (current-buffer))))
	     (send-string (encode-coding-string
			   (nth 1 args)
			   (cdr (process-coding-system (get-process process)))))
	     (send-string-length (length send-string)))
	(let ((inhibit-eol-conversion t)
	      (from 0)
	      to)
	  (while (< from send-string-length)
	    (setq to (min (+ from w32-pipe-limit) send-string-length))
	    (setf (nth 1 args) (substring send-string from to))
	    (apply orig-fun args)
	    (setq from to))))))

  (advice-add 'process-send-string :around #'ad-process-send-string)
  (setq tramp-copy-size-limit nil)
  )

;;;
;;; dired-open
;;;
(when (require 'dired-open nil t)
  (setq dired-open-extensions
	(mapcar (lambda (x) (cons x "start"))
	     '("exe" "docx" "doc" "xlsx" "xls" "pptx" "ppt"
	       "pcap" "pcapng"))))


;;;
;;; Open Startup
;;;
(when (boundp 'emacs.startup) (find-file emacs.startup))

;;;
;;; CUSTOM (DON'T EDIT)
;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-enabled-themes (quote (wheatgrass))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
