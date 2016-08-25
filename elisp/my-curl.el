;;; -*- mode:Emacs-Lisp; coding:utf-8 -*-
;;;
;;; my-curl.el -- cURL interface

;;
;; cURL
;;
;; http://curl.haxx.se/
;;

;; Configuration
(defcustom my-curl-command "curl"
  "Path to cURL command")

;; Require
(require 'my-process)

(defun my-curl (option-string)
  "Exec cURL command"
  (interactive
   (list (read-from-minibuffer "cURL options: ")))
  (my-process-show-output
   (concat "curl " option-string)
   my-curl-command option-string))

(defun my-curl-help ()
  "Show the help of cURL command"
  (interactive)
  (my-process-show-output
   "cURL Help"
   my-curl-command
   "--help"))

(defun my-curl-manual ()
  "Show the help of cURL command"
  (interactive)
  (my-process-show-output
   "cURL Manual"
   my-curl-command
   "--manual"))


(provide 'my-curl)
