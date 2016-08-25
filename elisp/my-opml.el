;;; -*- mode:Emacs-Lisp; coding:utf-8  lexical-binding:t -*-
;;;
;;; Edit OPML in Org mode
;;;
(let* ((xml "<post time=\"20050716234509\" id=\"010101\">
               <login>Test</login>
               <msg>Here is the message</msg>
               <info>My UA</info>
             </post>")
       (root (with-temp-buffer
	       (insert xml)
	       (xml-parse-region (point-min) (point-max))))
       (post (car root))
       (attrs (xml-node-attributes post))
       (time (cdr (assq 'time attrs)))
       (msg (car (xml-get-children post 'msg)))
       (text (car (xml-node-children msg))))
  (message "time: %s, message '%s'" time text))



(defconst my-opml-file-suffix-regex "\\.opml$" "suffix for opml")

(make-variable-buffer-local
 (defvar my-opml-buffer-filename
   nil "Filename for current buffer"))

(defun my-opml-load (filename)
  (lexical-let*
      ((in-file filename)

)

(defun my-opml-after-find-file ()
  (when (string-match my-opml-file-suffix-regex (buffer-file-name))
    (if (= (buffer-size) 0)
	(progn
	  (set-auto-mode)
	  (setq my-opml-buffer-filename (buffer-file-name)))
      (my-opml-load (buffer-file-name)))))

(add-hook 'find-file-hooks 'my-openssl-after-find-file)

(provide 'my-opml)
