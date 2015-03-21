;;; -*- mode:Emacs-Lisp; coding:utf-8; lexical-binding:t -*-
;;;
;;; Minimum Emacs init.el
;;;
;;;

;;;
;;; README
;;;
;;; Windows 7 ::
;;;
;;; ~/AppData/Roaming/.emacs.d/init.el
;;;
;;; (setenv "HOME" "C:/Users/XXXX")
;;; (setq user-emacs-directory "~/Dropbox/.emacs.d")
;;; (load-file (expand-file-name "init.el" user-emacs-directory))
;;;

;;;
;;; Registry File alist
;;;
(setq my-registry-alist
      '(
	("JP00086284" . "~/Documents/etc/mydata.xml")
	("DELL-PC" . "~/.mydata.xml")
	("SURFACEPRO3" . "~/.mydata.xml")
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

;; 日本語入力のための設定
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
;;;
;;; Tool Bar
;;;
(tool-bar-mode 0)

;;;
;;; Emacs Server
;;;
(require 'server)
(unless (server-running-p)
  (server-start)) ;; emacsclient

;;;
;;; Registry
;;;
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
       (cdr
	(assoc (system-name) my-registry-alist))))
  (if (file-exists-p registry-file-name)
      (my-registry-parser (xml-parse-file registry-file-name))))


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
 ((not (boundp 'mydata.emacs.exec-path)) nil)
 ((listp mydata.emacs.exec-path)
  (mapcar (lambda (path) (setq exec-path (cons path exec-path)))
	  mydata.emacs.exec-path))
 ((stringp mydata.emacs.exec-path)
  (setq exec-path (cons mydata.emacs.exec-path exec-path))))


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
;;;
;;; Tabber.el
;;;
(require 'tabbar)
(tabbar-mode)
(global-set-key "\M-]" 'tabbar-forward)  ; 次のタブ
(global-set-key "\M-[" 'tabbar-backward) ; 前のタブ
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

;;;
;;; GPG
;;;
(require 'epa-file)
(epa-file-enable)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;;;
;;; Theme
;;;
(load-theme 'misterioso t)

;;;
;;; Proxy
;;;
(defun proxy-init()
  (interactive)
  (setq url-proxy-services
	'(
	  ("http" . "jpyoip01.mgmt.ericsson.se:8080")
	  ("ftp" . "jpyoip01.mgmt.ericsson.se:8080")
	  ("https" . "jpyoip01.mgmt.ericsson.se:8080")
	  ("no_proxy" . "127.0.0.1,localhost")
	  )))
;;;
;;; Org
;;;
(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook 'turn-on-iimage-mode)

;;;
;;; EWW
;;;
(setq eww-search-prefix "https://www.google.co.jp/search?q=")

;;;
;;; My OpenSSL
;;;
(require 'my-openssl)

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
