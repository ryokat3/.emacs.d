;;; -*- mode:Emacs-Lisp; coding:utf-8; lexical-binding:t -*-
;;;

(defvar toodledo-auth-url "https://api.toodledo.com/3/account/authorize.php")
(defvar toodledo-token-url "https://api.toodledo.com/3/account/token.php")

(defvar toodledo-account-get-url "https://api.toodledo.com/3/account/get.php")
(defvar toodledo-folder-get-url "http://api.toodledo.com/3/folders/get.php")
(defvar toodledo-tasks-get-url "http://api.toodledo.com/3/tasks/get.php")
(defvar toodledo-notes-get-url "http://api.toodledo.com/3/notes/get.php")
(defvar toodledo-folders-get-url "http://api.toodledo.com/3/folders/get.php")

(defvar toodledo-refresh-token-expire (* 60 60 24 30)) ; 30 days
  
(defvar toodledo-app-id mydata.toodledo.app-id)
(defvar toodledo-app-secret mydata.toodledo.app-pw)
(defvar toodledo-id mydata.toodledo.id)
(defvar toodledo-secret mydata.toodledo.pw)

(defvar my-toodledo-tmpdir (concat (getenv "HOME") "/.my-toodledo"))
(defvar my-toodledo-access-token-file-name "access_token.xml")
(defvar my-toodledo-tasks-file-name "tasks.xml")
(defvar my-toodledo-notes-file-name "notes.xml")
(defvar my-toodledo-folders-file-name "folders.xml")

(defvar my-toodledo-user-info nil)
(defvar my-toodledo-user-info-file-name "user-info.el")

;;;
;;; Initialize
;;;
(if (not (file-directory-p my-toodledo-tmpdir))
    (make-directory my-toodledo-tmpdir))

;;;
;;; URL
;;;
(defun my-toodledo-create-query-string (name-value-list)
  (mapconcat
   (lambda (query)
     (concat (url-hexify-string (car query)) "="
	     (url-hexify-string (car (cdr query)))))
   name-value-list
   "&"))

;;;
;;; System
;;;
(defun my-toodledo-system-current-gmt()
  (let ((_time (current-time)))
    (- (+ (* (car _time) (expt 2 16)) (car (cdr _time)))
       (car (current-time-zone)))))


;;;
;;; XML
;;;
(defun my-toodledo-xml-get-node (xml sym-list)
  (cond
   ((null xml) nil)
   ((null sym-list) nil)
   ((not (listp xml)) nil)
   ((not (listp sym-list)) nil)
   ((not (symbolp (car xml)))
    (delq nil (append (my-toodledo-xml-get-node (car xml) sym-list)
		      (my-toodledo-xml-get-node (cdr xml) sym-list))))
   ((null (car sym-list))
    (if (eq (car xml) (car (cdr sym-list)))
	(my-toodledo-xml-get-node xml (cdr sym-list))
      (my-toodledo-xml-get-node (xml-node-children xml) sym-list)))
   ((not (eq (car xml) (car sym-list))) nil)
   ((null (cdr sym-list)) (list xml))
   (t
    (let ((children (xml-node-children xml)))
      (delq nil (append
		 (my-toodledo-xml-get-node (car children) (cdr sym-list))
		 (my-toodledo-xml-get-node (cdr children) (cdr sym-list))))))
   ))

(defun my-toodledo-xml-get-value (xml sym)
  (car (cdr (cdr (car (my-toodledo-xml-get-node xml `(nil ,sym)))))))

;;;
;;; Cache File
;;;
(defun my-toodledo-cache-file-new (file-name)
  (concat my-toodledo-tmpdir "/"
	  (number-to-string (my-toodledo-system-current-gmt)) "." file-name))

(defun my-toodledo-cache-file-get-latest(file-name)
  (car
   (reverse
    (sort
     (file-expand-wildcards
      (concat my-toodledo-tmpdir "/*." file-name)) 'string<))))

(defun my-toodledo-cache-file-get-time (path-name)
  (let ((file-name (file-name-nondirectory path-name)))
    (string-match "\\([[:digit:]]*\\)\..*$" file-name)
    (substring file-name (match-beginning 1) (match-end 1))))


(defun my-toodledo-cache-file-save-response (buf file-name)
  "Save the HTTP response from buf"
  (with-current-buffer buf
    (let ((path-name (my-toodledo-cache-file-new file-name)))
      ;; (search-forward "\n\n") sometimes fail
      ;; Once write off to the file and read again
      (write-region (point-min) (point-max) path-name)
      (delete-region (point-min) (point-max))
      (insert-file path-name)
      (goto-char (point-min))
      (search-forward "\n\n")
      (write-region (point) (point-max) path-name)
      path-name)))

;;;
;;; Cache Lisp Object
;;;
(defun my-toodledo-cache-write-obj-to-file (obj file-name)
  (with-temp-buffer
    (insert (prin1-to-string obj))
    (let ((file-path (my-toodledo-cache-file-new  file-name)))
      (when (file-writable-p file-path)
	(write-region (point-min) (point-max) file-path)))))


(defun my-toodledo-cache-read-obj-from-file (file-name)
  (let ((file (my-toodledo-cache-file-get-latest file-name)))
    (if file
	(with-temp-buffer
	  (insert-file-contents file)
	  (read (current-buffer)))
      nil)))

;;;
;;; User Info
;;;
(defconst my-toodledo-ui-prompt-user"Toodledo User: "
  "The string prompted when prompting Toodledo user name")

(defconst my-toodledo-ui-prompt-password "Toodledo Password: "
  "The string prompted when prompting Toodledo user password")

(defun my-toodledo-ui-get-user-password (&optional user)
  "To prompt Toodledo user name and password"
  `(,(if user
	 (read-string my-toodledo-ui-prompt-user user)
       (read-string my-toodledo-ui-prompt-user)) .
       ,(read-passwd my-toodledo-ui-prompt-password)))

;; Initialize User Info
(setq my-toodledo-user-info
      (my-toodledo-cache-read-obj-from-file my-toodledo-user-info-file-name))

;;;
;;; Authentication
;;;
(defun my-toodledo-access-code-authentication
    (toodledo-app-id toodledo-id toodledo-secret)
  (let (
	(url-request-method "POST")
	(url-request-extra-headers
	 `(("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (my-toodledo-create-query-string
	  `(("playgournd" "0")
	    ("email" ,toodledo-id)
	    ("pass" ,toodledo-secret)
	    ("authorized" "Authorize")
	    ))))
    (with-current-buffer
	(url-retrieve-synchronously
	 (concat toodledo-auth-url "?"
		 (my-toodledo-create-query-string
		  `(("response_type" "code")
		    ("client_id" ,toodledo-app-id)
		    ("state" "HEHE")
		    ("scope" "basic tasks notes outlines lists share write folders")
		    ))))
      (let ((case-fold-search t))
	(goto-char (point-min))
	(re-search-forward "^location: .*[\&\?]code=\\(.*\\)[\&\?].*$")
	(match-string 1)
	)
      )))

(defun my-toodledo-access-code (toodledo-app-id)
  (let*
      ((id-cell (assq 'toodledo-id my-toodledo-user-info))
       (id-pw (my-toodledo-ui-get-user-password (cdr id-cell)))
       (access-code (my-toodledo-access-code-authentication
		     toodledo-app-id (car id-pw) (cdr id-pw))))
    (if access-code
	(if id-cell
	    (setcdr id-cell (car id-pw))
	  (append '(toodledo-id . ,(car id-pw)) my-toodledo-user-info)))
    access-code
  ))

;;;
;;; Refresh Token
;;;
(defun my-toodledo-refresh-token-expire-time ()
  (let ((file-name (my-toodledo-cache-file-get-latest
		    my-toodledo-access-token-file-name)))
    (if (null file-name) 0
      (+ (string-to-number (my-toodledo-cache-file-get-time file-name))
	 toodledo-refresh-token-expire))))


(defun my-toodledo-refresh-token-get-latest()
  (let ((file-name (my-toodledo-cache-file-get-latest
		    my-toodledo-access-token-file-name)))
    (if (null file-name) nil
      (car (xml-node-children
	    (car (my-toodledo-xml-get-node
		  (xml-parse-file file-name) '(nil refresh_token))))))))

;;;
;;; Access Token
;;;
(defun my-toodledo-access-token-download-with-authorization-code
    (access-code app-id app-pw)
  (let (
	(url-request-method "POST")
	(url-request-extra-headers
	 `(("Authorization" .
	    ,(concat "Basic " (base64-encode-string (concat app-id ":" app-pw))))
	   ("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (my-toodledo-create-query-string
	  `(("grant_type" "authorization_code")
	    ("code" ,access-code)
	    ("f" "xml")
	    ))))
    (my-toodledo-cache-file-save-response
     (url-retrieve-synchronously toodledo-token-url)
     my-toodledo-access-token-file-name)))


(defun my-toodledo-access-token-download-with-refresh-token
    (refresh-token app-id app-pw)
  (let (
	(url-request-method "POST")
	(url-request-extra-headers
	 `(("Authorization" .
	    ,(concat "Basic " (base64-encode-string (concat app-id ":" app-pw))))
	   ("Content-Type" . "application/x-www-form-urlencoded")))
	(url-request-data
	 (my-toodledo-create-query-string
	  `(("grant_type" "refresh_token")
	    ("refresh_token" ,refresh-token)
	    ("f" "xml")
	    ))))
    (my-toodledo-cache-file-save-response
     (url-retrieve-synchronously toodledo-token-url)
     my-toodledo-access-token-file-name)))


(defun my-toodledo-access-token-expire-time ()
  (let ((file-name (my-toodledo-cache-file-get-latest
		    my-toodledo-access-token-file-name)))
    (if (null file-name) 0
      (+ (string-to-number (my-toodledo-cache-file-get-time file-name))
	 (string-to-number
	  (car (xml-node-children
		(car (my-toodledo-xml-get-node
		      (xml-parse-file file-name) '(nil expires_in))))))))))


(defun my-toodledo-access-token-get-cache-file ()
  (if (or (null (my-toodledo-cache-file-get-latest
		 my-toodledo-access-token-file-name))
	  (< (my-toodledo-refresh-token-expire-time)
	     (my-toodledo-system-current-gmt)))
      ;; Download access token with user authentication
      (my-toodledo-access-token-download-with-authorization-code
;       (my-toodledo-auth-user
;	toodledo-app-id toodledo-id toodledo-secret)
       (my-toodledo-access-code toodledo-app-id)
       toodledo-app-id toodledo-app-secret)
    (if (< (my-toodledo-access-token-expire-time)
	   (my-toodledo-system-current-gmt))
	;; Download access token with refresh token
	(my-toodledo-access-token-download-with-refresh-token
	 (my-toodledo-refresh-token-get-latest)
	 toodledo-app-id toodledo-app-secret)
      ;; Get cached access token
      (my-toodledo-cache-file-get-latest
       my-toodledo-access-token-file-name))))

    
(defun my-toodledo-access-token-get-latest ()
  (let ((file-name (my-toodledo-access-token-get-cache-file)))
    (car (xml-node-children
	  (car (my-toodledo-xml-get-node
		(xml-parse-file file-name)
		'(nil access_token)))))))

;;;
;;; Task
;;;
(defun my-toodledo-tasks-download-all ()
  (let
      ((access-token (my-toodledo-access-token-get-latest))
       (url-request-method "GET")
       )
    (my-toodledo-cache-file-save-response
     (url-retrieve-synchronously
      (concat toodledo-tasks-get-url "?"
	      (my-toodledo-create-query-string
	       `(("access_token" ,access-token)
		 ("fields" "folder,context,goal,location,tag,startdate,duedate,duedatemod,starttime,duetime,remind,repeat,status,star,priority,length,timer,added,note,parent,children,order,meta,previous,attachment,shared,addedby,via,attachments")
		 ("f" "xml")))))
     my-toodledo-tasks-file-name)))

;;;
;;; Folders
;;;
(defun my-toodledo-folders-download-all ()
  (let
      ((access-token (my-toodledo-access-token-get-latest))
       (url-request-method "GET")
       )
    (my-toodledo-cache-file-save-response
     (url-retrieve-synchronously
      (concat toodledo-folders-get-url "?"
	      (my-toodledo-create-query-string
	       `(("access_token" ,access-token)
		 ("f" "xml")))))
     my-toodledo-folders-file-name)))

;;;
;;; Notes
;;;
(defun my-toodledo-notes-download-all ()
  (let
      ((access-token (my-toodledo-access-token-get-latest))
       (url-request-method "GET")
       )
    (my-toodledo-cache-file-save-response
     (url-retrieve-synchronously
      (concat toodledo-notes-get-url "?"
	      (my-toodledo-create-query-string
	       `(("access_token" ,access-token)
		 ("f" "xml")))))
     my-toodledo-notes-file-name)))

;;;
;;; Org
;;;
;;; Org Element API
;;;   http://orgmode.org/worg/dev/org-element-api.html
;;;
;;; Doc to AST :: org-element-parse-buffer
;;; AST to Doc :: org-element-interpret-data
;;;
;;; AST :: Abstract Syntax Tree
;;;
(require 'org-element)


(defun my-toodledo-tasks-task-xml-to-ast (xml)
  `(headline
    (:title ,(my-toodledo-xml-get-value xml 'title)
	    :level 2)
    (section
     nil
     (property-drawer
      nil
      (node-property
       (:key "ID"
	     :value ,(my-toodledo-xml-get-value xml 'id)))))))

(defun my-toodledo-xml-tasks-to-ast(xml)
  (mapcar #'my-toodledo-tasks-task-xml-to-ast
	  (my-toodledo-xml-get-node  xml '(nil task))))


;;;
;;; Folder data
;;;

(defun my-toodledo-xml-folder-compare (f1 f2)
  (< (string-to-number (my-toodledo-xml-get-value f1 'order))
     (string-to-number (my-toodledo-xml-get-value f2 'order))))

(defun my-toodledo-xml-folder-to-ast (xml)
  `(headline
    (:title ,(my-toodledo-xml-get-value xml 'name)
	    :level 1
	    :todo-type todo)
    (section
     nil
     (property-drawer
      nil
      (node-property
       (:key "ID"
	     :value ,(my-toodledo-xml-get-value xml 'id)))))))

(defun my-toodledo-xml-folders-to-ast(xml)
  (mapcar #'my-toodledo-xml-folder-to-ast
	  (sort
	   (my-toodledo-xml-get-node xml '(nil folder))
	   #'my-toodledo-xml-folder-compare)))

(defun my-toodledo-ast-to-string (ast)
  (org-element-interpret-data (cons 'org-data (cons nil ast))))


;;;
;;;
;;;
(provide 'my-toodledo)

;;;;
;;;; TEST
;;;;
(require 'my-toodledo)

;(my-toodledo-tasks-download-all)
;(my-toodledo-notes-download-all)
;(my-toodledo-folders-download-all)


(defun tasks-to-org ()
  (with-current-buffer (get-buffer-create "*my-toodledo*")
    (org-mode)
    (insert
     (my-toodledo-ast-to-string
      (my-toodledo-xml-tasks-to-ast
       (xml-parse-file (my-toodledo-cache-file-get-latest
			my-toodledo-tasks-file-name)))))
    (org-global-cycle)))

(defun folders-to-org ()
  (with-current-buffer (get-buffer-create "*my-toodledo*")
    (org-mode)
    (insert
     (my-toodledo-ast-to-string
      (my-toodledo-xml-folders-to-ast
       (xml-parse-file (my-toodledo-cache-file-get-latest
			my-toodledo-folders-file-name)))))
    (org-global-cycle)))


(defun org-to-xml ()
  (insert
   (with-current-buffer (get-buffer-create "hehe.org")
     (prin1-to-string (org-element-parse-buffer)))))

;(my-toodledo-ui-get-user-password "ryokat3@gmail.com")

;(my-toodledo-cache-write-obj-to-file
; '(("www" . "hoho") ("haha" . "kuku"))
; my-toodledo-user-info-file-name)

;(my-toodledo-cache-read-obj-from-file my-toodledo-user-info-file-name)

;(insert
; (prin1-to-string
;  (with-current-buffer "hehe.org"
;    (org-element-map (org-element-parse-buffer) 'headline
;      (lambda (headline)
;	(if (= 1 (org-element-property :level headline))
;	    (org-element-property :raw-value headline)))))))

