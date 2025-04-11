;;; notdeft-xapian.el --- Xapian backend for NotDeft  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; See end of file for licensing information.

;;; Commentary:
;; Xapian-specific functionality for NotDeft.

(require 'cl-lib)
(require 'notdeft-base)
(require 'notdeft-util)

;;; Code:

(defcustom notdeft-xapian-program
  (let ((dir (expand-file-name
              "xapian"
               (file-name-directory
	        (locate-library "notdeft-xapian.el")))))
    (when (and dir (file-directory-p dir))
      (cl-some (lambda (cand)
                 (let ((exe (expand-file-name cand dir)))
                   (when (file-executable-p exe)
                     exe)))
               '("notdeft-xapian" "notdeft-xapian.exe"))))
  "Xapian backend's executable program path.
Specified as an absolute path. Must be set appropriately for the
`notdeft-xapian' feature to be usable, on which much of NotDeft
functionality relies. By default an attempt is made to compute
the path in terms of `locate-library' and default naming, which
will not succeed unless the program has already been compiled
into an executable."
  :type '(choice (const :tag "None" nil)
		 (file :tag "Path"))
  :safe #'string-or-null-p
  :group 'notdeft)

(defcustom notdeft-xapian-max-results 100
  "Maximum number of Xapian query results.
No limit if 0."
  :type 'integer
  :safe #'integerp
  :group 'notdeft)

(defcustom notdeft-xapian-language "en"
  "Stemming language to use in Xapian indexing and searching.
See Xapian documentation for a list of supported language names
and abbreviations. May be specified as \"none\" for no stemming.
The language identifier may be followed by the string \":cjk\" to
enable generation of n-grams from CJK text. The CJK option is
ignored unless a recent enough version of Xapian is used."
  :type 'string
  :safe #'stringp
  :group 'notdeft)

(defcustom notdeft-xapian-order-by 'time
  "Search result ordering criterion.
Whether to order file list by decreasing modification `time', by
file `name', or by decreasing query `relevance' (for which nil
may be used as a synonym). Any query modifier takes precedence
over this setting."
  :type '(choice (const time) (const name) (const relevance))
  :safe #'symbolp
  :group 'notdeft)

(defcustom notdeft-xapian-boolean-any-case t
  "Whether to allow query operators in any case.
That is, whether the operator syntax also allows
lowercase characters (e.g., \"and\" and \"or\")."
  :type 'boolean
  :safe #'booleanp
  :group 'notdeft)

(defcustom notdeft-xapian-pure-not t
  "Whether to allow \"NOT\" in queries.
Using such queries is costly on performance."
  :type 'boolean
  :safe #'booleanp
  :group 'notdeft)

(defface notdeft-xapian-query-face
  '((t :inherit font-lock-string-face :bold t))
  "Face for NotDeft Xapian queries."
  :group 'notdeft-faces)

(defvar notdeft-xapian-query-history nil
  "Xapian query string history.
Not cleared between invocations of `notdeft-mode'.")

(defun notdeft-xapian-read-query (&optional initial)
  "Read a Xapian query string, interactively.
Use and update `notdeft-xapian-query-history' in querying.
Optionally fill in the specified INITIAL input. Return the read
string, or nil if no query is given."
  (notdeft-read-query 'notdeft-xapian-query-history initial))

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
	    #'call-process-region
	    (point-min) ;; START
	    (point-max) ;; END
	    notdeft-xapian-program ;; PROGRAM
	    t ;; DELETE (delete input)
	    t ;; BUFFER (output to current buffer)
	    nil	;; DISPLAY (do not refresh)
	    `("index"
	      "--chdir" ,(expand-file-name "." "~")
	      ,@(when recreate '("--recreate"))
	      ,@(apply #'append
		       (mapcar
			(lambda (ext)
			  `("--extension" ,(concat "." ext)))
			(cons notdeft-extension
			      notdeft-secondary-extensions)))
	      "--lang" ,(or notdeft-xapian-language "none")
	      ,@(when notdeft-allow-org-property-drawers
		  '("--allow-org-property-drawers"))
	      "--input"))))
      (when (/= 0 ret)
	(error "Index generation failed: %s (%d): %s"
	       notdeft-xapian-program ret (buffer-string))))))

(defun notdeft-xapian-search (dirs &optional query)
  "On the Xapian indexes in DIRS, perform the search QUERY.
I.e., perform the query in terms of the Xapian indexes in the
specified DIRS. Where a query is not specified, use a query that
matches any file, and in that case consider
`notdeft-xapian-order-by' to be `time'. Return at most
`notdeft-xapian-max-results' results, as pathnames of the
matching files. Sort by relevance, modification time, or
non-directory filename, all descending, based on the
`notdeft-xapian-order-by' setting and any query modifiers."
  (let ((order-by (if query notdeft-xapian-order-by 'time))
	(max-results notdeft-xapian-max-results))
    (when query
      (save-match-data
	(while (string-match "^ *!\\([[:alpha:]]+\\)\\>" query)
	  (let ((opt (match-string 1 query)))
	    (setq query (substring query (match-end 0)))
	    (pcase (downcase opt)
              ("time" (setq order-by 'time))
              ("rank" (setq order-by 'relevance))
              ("file" (setq order-by 'name))
              ("all" (setq max-results 0)))))))
    (let* ((query (notdeft-chomp-nullify query))
	   (s (shell-command-to-string
	       (concat
		(shell-quote-argument notdeft-xapian-program) " search"
		(cond
                 ((eq order-by 'time) " --time-sort")
                 ((eq order-by 'name) " --name-sort")
                 (t ""))
		" --lang " (shell-quote-argument
			    (or notdeft-xapian-language "none"))
		(if notdeft-xapian-boolean-any-case
		    " --boolean-any-case" "")
		(if notdeft-xapian-pure-not
		    " --pure-not" "")
		(if (> max-results 0)
		    (format " --max-count %d" max-results)
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

(defun notdeft-xapian-list (dirs &optional kind)
  "From Xapian indexes in DIRS list all terms of KIND.
Return a list of strings. KIND is either: the symbol `keywords',
for full keywords, as originally declared; or `keyword-words',
for normalized words of keywords, meaning keywords split into one
or more constituent words that only contain what Xapian regards
as word characters. The default is `keywords'."
  (let ((str (shell-command-to-string
	      (concat
	       (shell-quote-argument notdeft-xapian-program) " list"
               (if (eq kind 'keyword-words) " --words" "")
	       " " (mapconcat
		    (lambda (dir)
		      (shell-quote-argument
		       (expand-file-name dir "~")))
		    dirs " ")))))
    (read str)))

(provide 'notdeft-xapian)

;;; notdeft-xapian.el ends here

;; NotDeft, a note manager for Emacs
;; Copyright (C) 2017-2025  Tero Hasu
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
