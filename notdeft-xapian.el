;;; notdeft-xapian.el --- Xapian backend for NotDeft
;; -*- lexical-binding: t; -*-

;; Copyright (C) 2017 by the author.
;; All rights reserved.
;; Author: Tero Hasu <tero@hasu.is>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; Xapian-specific functionality for NotDeft.

;;; Code:

(require 'notdeft-autoloads)

(defcustom notdeft-xapian-program nil
  "Xapian backend's executable program path.
Specified as an absolute path. When nil, the Xapian backend is
disabled, and filtering does not concern search results, but all
notes in `notdeft-directories'."
  :type '(choice (const :tag "None" nil)
		 (file :tag "Path"))
  :safe 'string-or-null-p
  :group 'notdeft)

(defcustom notdeft-xapian-max-results 100
  "Maximum number of Xapian query results.
\(I.e., '--max-count' for `notdeft-xapian-program'.)
No limit if 0."
  :type 'integer
  :safe 'integerp
  :group 'notdeft)

(defcustom notdeft-xapian-language "en"
  "Stemming language to use in Xapian indexing and searching."
  :type 'string
  :safe 'stringp
  :group 'notdeft)

(defcustom notdeft-xapian-order-by-time t
  "Whether to order file list by decreasing modification time.
Otherwise order by decreasing relevance, unless overridden by
a query modifier."
  :type 'boolean
  :safe 'booleanp
  :group 'notdeft)

(defcustom notdeft-xapian-boolean-any-case t
  "Whether to allow query operators in any case.
That is, whether the operator syntax also allows
lowercase characters (e.g., \"and\" and \"or\")."
  :type 'boolean
  :safe 'booleanp
  :group 'notdeft)

(defcustom notdeft-xapian-pure-not t
  "Whether to allow \"NOT\" in queries.
Using such queries is costly on performance."
  :type 'boolean
  :safe 'booleanp
  :group 'notdeft)

(defface notdeft-xapian-query-face
  '((t :inherit font-lock-string-face :bold t))
  "Face for NotDeft Xapian queries."
  :group 'notdeft-faces)

(defvar notdeft-xapian-query-history nil
  "Xapian query string history.
Not cleared between invocations of `notdeft-mode'.")

(defun notdeft-xapian-read-query ()
  "Read a Xapian query string, interactively.
Use and update `notdeft-xapian-query-history' in querying.
Return the read string, or nil if no query is given."
  (let ((s (read-from-minibuffer
	    "Query: " ;; PROMPT
	    nil nil nil ;; INITIAL-CONTENTS KEYMAP READ
	    'notdeft-xapian-query-history ;; HIST
	    nil ;; DEFAULT-VALUE
	    t ;; INHERIT-INPUT-METHOD
	    )))
    (when (and s (not (string= s "")))
      s)))

(eval-when-compile
  (defvar notdeft-extension)
  (defvar notdeft-secondary-extensions))

(defun notdeft-xapian-index-dirs (dirs &optional recreate)
  "Create or update a Xapian index for DIRS.
Each element of DIRS must be either a directory path string, or a
list of the form (directory-path . relative-file-path-list). With
RECREATE, truncate any existing index files."
  (with-temp-buffer
    (dolist (dir dirs)
      (if (stringp dir)
	  (insert ":idir\n" (file-relative-name dir "~") "\n")
	(let ((dir (car dir))
	      (files (cdr dir)))
	  (insert ":ifiles\n" (file-relative-name dir "~") "\n")
	  (insert (format "%d\n" (length files)))
	  (dolist (file files)
	    (insert file "\n")))))
    (let ((ret
	   (apply
	    'call-process-region
	    (point-min) ;; START
	    (point-max) ;; END
	    notdeft-xapian-program ;; PROGRAM
	    t ;; DELETE (delete input)
	    t ;; BUFFER (output to current buffer)
	    nil	;; DISPLAY (do not refresh)
	    `("index"
	      "--chdir" ,(expand-file-name "." "~")
	      ,@(if recreate '("--recreate") nil)
	      ,@(apply 'append
		       (mapcar
			(lambda (ext)
			  `("--extension" ,(concat "." ext)))
			(cons notdeft-extension
			      notdeft-secondary-extensions)))
	      "--lang" ,notdeft-xapian-language
	      "--input"))))
      (when (/= 0 ret)
	(error "Index generation failed: %s (%d): %s"
	       notdeft-xapian-program ret (buffer-string))))))

(defun notdeft-xapian-search (dirs &optional query)
  "On the Xapian indexes in DIRS, perform the search QUERY.
I.e., perform the query in terms of the Xapian indexes in the
specified DIRS. Where a query is not specified, use a query that
matches any file, and in that case consider
`notdeft-xapian-order-by-time' to be true. Return at most
`notdeft-xapian-max-results' results, as pathnames of the
matching files. Sort by relevance, modification time, or
non-directory filename, all descending, based on the
`notdeft-xapian-order-by-time' setting and any query modifiers."
  (let ((time-sort (if query notdeft-xapian-order-by-time t))
	(name-sort nil)
	(max-results notdeft-xapian-max-results)
	name-sort)
    (when query
      (while (string-match "^ *!\\([[:alpha:]]+\\)\\>" query)
	(let ((opt (match-string 1 query)))
	  (setq query (substring query (match-end 0)))
	  (pcase (downcase opt)
	    ("time" (setq time-sort t))
	    ("rank" (setq time-sort nil))
	    ("all" (setq max-results 0))
	    ("file" (setq name-sort t))
	    ))))
    (let* ((query (notdeft-chomp-nullify query))
	   (s (shell-command-to-string
	       (concat
		(shell-quote-argument notdeft-xapian-program) " search"
		(if name-sort " --name-sort" "")
		(if time-sort " --time-sort" "")
		" --lang " (shell-quote-argument notdeft-xapian-language)
		(if notdeft-xapian-boolean-any-case
		    " --boolean-any-case" "")
		(if notdeft-xapian-pure-not
		    " --pure-not" "")
		(if (> max-results 0)
		    (format " --max-count %d" notdeft-xapian-max-results)
		  "")
		(if query
		    (concat " --query " (shell-quote-argument query))
		  "")
		" " (mapconcat
		     (lambda (dir)
		       (shell-quote-argument
			(expand-file-name dir "~")))
		     dirs " "))))
	   (files
	    (mapcar
	     (lambda (file)
	       (expand-file-name file "~"))
	     (split-string s "\n" t))))
      files)))

(defvar notdeft-xapian-install-buffer-name " *Install notdeft-xapian"
  "Name of the buffer used for compiling notdeft-xapian.")

(defvar notdeft-directory-path
  (shell-quote-argument (file-name-directory (file-truename (locate-library "notdeft"))))
  "Directory path for notdeft.")

(defvar notdeft-xapian-executeable-path
  (shell-quote-argument (concat notdeft-directory-path "xapian/notdeft-xapian"))
  "Path for the notdeft-xapian executeable.")

(defvar notdeft-disable-notdeft-xapian-compilation nil
  "Disables the compilation of notdeft-xapian, when the notdeft-xapian
  executeable does not exist and notdeft is loaded.")

(defun notdeft-make-notdeft-xapian ()
  "This function compiles the notdeft-xapian application."
  (interactive)
  (let* ((change-dir (concat "cd " notdeft-directory-path "xapian;"))
         (make-commands (concat change-dir "make")))
    (unless (file-executable-p notdeft-xapian-executeable-path)
      (let* ((buffer (get-buffer-create notdeft-xapian-install-buffer-name)))
        (pop-to-buffer notdeft-xapian-install-buffer-name)
        (if (zerop (call-process "sh" nil buffer t "-c" make-commands))
            (message "Compilation of `notdeft-xapian' succeeded!")
          (error "Compilation of `notdeft-xapian' failed!"))))))

(defun notdeft-make-notdeft-xapian-when-not-found ()
  "Run `notdeft-make-notdeft-xapian', when the notdeft-xapian executable does not exist."
  (if (and (not (file-exists-p notdeft-xapian-executeable-path))
           (not notdeft-disable-notdeft-xapian-compilation))
      (notdeft-make-notdeft-xapian)))

(add-hook 'notdeft-load-hook 'notdeft-make-notdeft-xapian-when-not-found)

(provide 'notdeft-xapian)

;;; notdeft-xapian.el ends here
