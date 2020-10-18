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

; (package-initialize)

;;;
;;; Spacemacs
;;;
;(setq spacemacs-start-directory "~/.emacs.d/spacemacs/")
;(load-file (concat spacemacs-start-directory "init.el"))



;;;
;;; Registry
;;;
(defconst my-default-registry-file "~/.registry.xml")
(setq my-registry-alist
      '(
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
(cd (getenv "HOME"))

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
  ;;(set-default-font (concat ms-gothic-string " 14"))
  ;;(set-frame-font (concat ms-gothic-string "-16"))
  (set-frame-font (concat ms-gothic-string "-14"))
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
  
  (when (fboundp 'w32-ime-initialize)
    (w32-ime-initialize)

    ;; (set-language-environment "UTF-8") ;; UTF-8 でも問題ないので適宜コメントアウトしてください
    (setq default-input-method "W32-IME")
    (setq-default w32-ime-mode-line-state-indicator "[--]")
    (setq w32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))

    ;; 日本語入力時にカーソルの色を変える設定 (色は適宜変えてください)
    (add-hook 'w32-ime-on-hook '(lambda () (set-cursor-color "coral4")))
    (add-hook 'w32-ime-off-hook '(lambda () (set-cursor-color "yellow")))

    ;; ミニバッファに移動した際は最初に日本語入力が無効な状態にする
    (add-hook 'minibuffer-setup-hook 'deactivate-input-method)

    ;; isearch に移行した際に日本語入力を無効にする
    (add-hook 'isearch-mode-hook
	      '(lambda ()
		 (deactivate-input-method)
		 (setq w32-ime-composition-window (minibuffer-window))))
    (add-hook 'isearch-mode-end-hook
	      '(lambda () (setq w32-ime-composition-window nil)))

    ;; helm 使用中に日本語入力を無効にする
    (advice-add 'helm :around
		'(lambda (orig-fun &rest args)
		   (let ((select-window-functions nil)
			 (w32-ime-composition-window (minibuffer-window)))
		     (deactivate-input-method)
		     (apply orig-fun args))))
    )
  )

;;;
;;; Tool Bar
;;;
(when window-system
  (tool-bar-mode 0)
  )

;;;
;;; Emacs Server
;;;
(require 'server)
(server-start) ;; emacsclient

;;;
;;; Line Number
;;;
(when (require 'linum nil t)
  (global-set-key [f9] 'linum-mode)
  )

;;;
;;; Misc
;;;
(defalias 'yes-or-no-p 'y-or-n-p)

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
     120 ;width
;     (- (/ (- (x-display-pixel-height) 40) (frame-char-height)) 5)) ;height
     48)
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
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;;
;;; GPG
;;;
(require 'epg)

;;;
;;; Theme
;;;
(load-theme 'misterioso t)

;;;
;;; MSYS Shell
;;;
(when (and window-system windows-p)
  ;; MSYS の bash を使用します。
  (setq explicit-shell-file-name emacs.shell)
  (setq shell-file-name "bash")
  (setenv "SHELL" shell-file-name) 
  ;;(setq explicit-sh-args '("-login" "-i"))
  (setq explicit-sh-args '("--noediting" "-i"))
  ;; SHELL で ^M が付く場合は ^M を削除します。
  (add-hook 'shell-mode-hook
	    (lambda ()
	      (set-buffer-process-coding-system 'undecided-dos 'sjis-unix)))
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  ;; shell-mode での保管(for drive letter)
  (setq shell-file-name-chars "~/A-Za-z0-9_^$!#%&{}@`'.,:()-")
  )

;;;
;;; Org
;;;
(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook 'turn-on-iimage-mode)
(require 'org-eshell nil t)

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
;;; Markdown
;;;
(when (require 'markdown-mode nil t)
  (setq markdown-command-needs-filename nil)
  (setq markdown-coding-system 'utf-8-dos)
  (setq markdown-command "pandoc")
  (setq markdown-css-paths '("C:/local_data/etc/github-markdown.css"))
  (setq markdown-xhtml-header-content "<link href=\"http://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css\" rel=\"stylesheet\" />")
  )


;;;
;;; YAML
;;;
(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
  (define-key yaml-mode-map "\C-m" 'newline-and-indent)
  )


;;;
;;; Bash Auto Completion
;;;
(when (require 'bash-completion nil t)
  (bash-completion-setup)
  )

;;;
;;; Neo Tree
;;;
(when (require 'neotree nil t)
  (setq neo-window-width 32)
  (setq neo-create-file-auto-open t)
  (setq neo-keymap-style 'concise)
  (setq neo-smart-open t)
  (global-set-key [f8] 'neotree-toggle)
  (when (require 'all-the-icons nil t)
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
    )
  )

;;;
;;; Icons for Neo Tree
;;;
;;; [NOTE] exec M-x all-the-icons-install-fonts
;;;

;;;
;;; ediff
;;;
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

;;;
;;; Persistent *scratch*
;;;
(when (require 'persistent-scratch nil t)
  (persistent-scratch-setup-default)
  )

;;;
;;; My Convenient
;;;
(global-set-key [f6] '(lambda ()
			(interactive)
			(progn (switch-to-buffer (get-buffer-create "*Markdown Memo*"))
			       (markdown-mode))))
(when (boundp 'emacs.startup)
  (global-set-key [f7] '(lambda () (interactive) (find-file emacs.startup)))
  )



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
 '(custom-enabled-themes (quote (wheatgrass)))
 '(package-selected-packages
   (quote
    (persistent-scratch markdown-mode py-autopep8 bash-completion tabbar recentf-ext pandoc-mode pandoc neotree magit helm evil dired-open all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
