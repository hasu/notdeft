;;; notdeft-org.el --- some support for Org format NotDeft notes  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; See end of file for licensing information.

;;; Commentary:
;; Some NotDeft-specific support for `org-mode'. For Org mode version
;; 9 and higher.
;;
;; This feature requires no specific setup, as the public commands and
;; functions of this feature are autoloadable. However, see also
;; `notdeft-org9', which is an optional extension to this feature, and
;; does require setting up for use.

;;; Code:

(require 'org)
(require 'notdeft)

;;;###autoload
(defun notdeft-org-open-deft-link (link)
  "Visit the NotDeft note specified by LINK.
The argument is a non-directory filename, possibly followed by
search options (see the fourth argument of `org-open-file'). This
function defines the opening of Org \"deft:\" links."
  (let ((name link) search)
    (save-match-data
      (when (string-match "::\\(.+\\)\\'" link)
	(setq search (match-string 1 link)
	      name (substring link 0 (match-beginning 0)))))
    (let ((path (notdeft-file-by-basename name)))
      (if (not path)
	  (message "No NotDeft note %S" name)
	(org-open-file path t nil search)))))

;;;###autoload
(defun notdeft-org-complete-deft-link (&optional prefix)
  "Define completion for Org \"deft:\" links.
The optional PREFIX argument is ignored."
  (ignore prefix)
  (let* ((file (notdeft-select-note-file))
	 (name (when file
		 (file-name-nondirectory file))))
    (concat "deft:" (or name ""))))

(defvar notdeft-describe-link 'notdeft-title-from-file-content
  "Function to determine NotDeft note file link description.
The function is given the file name as its sole argument.")

(defun notdeft-org-read-link-description (&optional desc)
  "Read a link description, interactively.
If DESC is provided, it is used as the initial input. Returns a
string, or nil if no non-whitespace description was provided."
  (notdeft-chomp-nullify
   (read-string "Description: " desc nil nil t)))

(defun notdeft-make-deft-link (name &optional desc)
  "Turn NAME and DESC into a \"deft:\" link.
NAME should be a non-directory file name with extension."
  (org-link-make-string (concat "deft:" name) desc))

;;;###autoload
(defun notdeft-org-store-deft-link ()
  "Store a \"deft:\" link for the current note.
Like `org-store-link', store the link into `org-stored-links'."
  (interactive)
  (let ((old-file (notdeft-current-filename t t)))
    (when old-file
      (let* ((name (file-name-nondirectory old-file))
	     (link (concat "deft:" name))
	     (desc (notdeft-title-from-file-content old-file)))
	(push (list link desc) org-stored-links)
	(message "Stored: %s" (or desc link))))))

;;;###autoload
(defun notdeft-org-link-existing-note (notename &optional desc region)
  "Create a \"deft:\" link to an existing note.
Link to a note by NOTENAME, inserting a link description if DESC
is non-nil. Insert the created link at point, unless REGION in
specified \(as a list of two positions), in which case replace
that region. When called interactively: offer a list of notes
from which to choose the link target; query for a note
description, offering to use the text of any active region as the
title, or the result of calling `notdeft-describe-link'
otherwise; use any active region as REGION; if one
\\[universal-argument] is given, then insert a link without DESC;
and if two \\[universal-argument]s are given, use the title of
any note as the description. If multiple notes have the same
NOTENAME, pick any one of them for deriving a description."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (let* ((pfx (prefix-numeric-value current-prefix-arg))
	    (region (when mark-active
		      (list (region-beginning) (region-end))))
	    (desc (and region (= pfx 1)
		       (apply #'buffer-substring-no-properties region)))
	    (file
	     ;; Select note before prompting for any description.
	     ;; Provide any region text as a selection hint.
	     (let ((notdeft-select-note-file-initial-query desc)
                   (notdeft-xapian-order-by-time nil))
	       (notdeft-select-note-file)))
	    (desc
	     (when (and file (/= pfx 4))
	       (notdeft-org-read-link-description
		(or desc
		    (pcase pfx
		      (1 (notdeft-chomp-nullify
			  (funcall notdeft-describe-link file)))
		      (16 (notdeft-title-from-file-content file)))))))
	    (notename (when file
			(file-name-nondirectory file))))
       (list notename desc region))))
  (when notename
    (when region
      (apply #'delete-region region))
    (insert (notdeft-make-deft-link notename desc))))

(defalias 'notdeft-insert-org-link
  #'notdeft-org-link-existing-note
  "Deprecated. Use `notdeft-org-link-existing-note'.")

;;;###autoload
(defun notdeft-org-link-new-file (&optional dir notename ext data desc region)
  "Create a \"deft:\" link to a new note.
Return the filename of the created file. The arguments DIR,
NOTENAME, EXT, and DATA are as for `notdeft-create-file'. Use
DESC, if any, as the link description. Insert an Org \"deft:\"
link to the newly created note at point, except if REGION is
non-nil, in which case replace that buffer region \(specified as
a list of two position values) with the link. When called
interactively: query for a note title, offering to use the text
of any active region as the title; use any active region as
REGION; derive a NOTENAME based on the title, as usual; use the
default filename extension as EXT; if one \\[universal-argument]
is given, then insert a link without DESC; if two
\\[universal-argument]s are given, the query for a target DIR for
the new note."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (let* ((pfx (prefix-numeric-value current-prefix-arg))
	    (region (when mark-active
		      (list (region-beginning) (region-end))))
	    (title
	     (notdeft-chomp-nullify
	      (read-string "Title: "
			   (when region
			     (notdeft-chomp
			      (apply #'buffer-substring-no-properties region)))
			   nil nil t)))
	    (desc (unless (= pfx 4)
		    (notdeft-org-read-link-description title))))
       (list (and (= pfx 16) 'dir) ;; dir
	     (and title `(title, title)) ;; notename
	     nil ;; ext
	     title ;; data
	     desc
	     region))))
  (let* ((buf (current-buffer))
	 (name (file-name-nondirectory
		(notdeft-create-file dir notename ext data))))
    (switch-to-buffer buf)
    (when region
      (apply #'delete-region region))
    (insert (notdeft-make-deft-link name desc))))

(defalias 'notdeft-link-new-file
  #'notdeft-org-link-new-file
  "Deprecated. Use `notdeft-org-link-new-file'.")

;;;###autoload
(defun notdeft-org-open-notdeft-link (query &optional rich)
  "Open the NotDeft search specified by QUERY.
Optionally and potentially (if supported) do the search and
present results in a RICH manner. This defines the opening of Org
\"notdeft:\" links."
  (notdeft-open-search query rich))

;;;###autoload
(defun notdeft-org-store-notdeft-link (&optional select)
  "Store the current NotDeft search as an Org link.
Use `org-store-link' to invoke this function. If invoked in
`notdeft-mode', then store a link to the current Xapian query, if
any. In other modes use the latest entry in
`notdeft-xapian-query-history', if any. With a non-nil SELECT
argument let the user choose interactively from among the history
entries, regardless of mode."
  (let* ((query (cond
                 (select
                  (ido-completing-read "Query: " notdeft-xapian-query-history nil t))
                 ((and (eq major-mode 'notdeft-mode)
                       (boundp 'notdeft-xapian-query))
                  (eval 'notdeft-xapian-query t))
                 (t
                  (car notdeft-xapian-query-history))))
         (query (notdeft-chomp-nullify query)))
    (when query
      (org-link-store-props
       :type "notdeft"
       :link (concat "notdeft:" query)))))

;;;###autoload
(defun notdeft-org-search-for-heading (&optional as-phrase)
  "Query for current Org heading text.
Optionally execute the search as a phrase search if AS-PHRASE is
non-nil or when called interactively with a
\\[universal-argument] prefix argument."
  (interactive "P")
  (let ((title
	 (save-excursion
	   (org-back-to-heading t)
	   (nth 4 (org-heading-components)))))
    (notdeft-search-for-title title as-phrase)))

(provide 'notdeft-org)

;;; notdeft-org.el ends here

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
