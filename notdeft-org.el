;;; notdeft-org.el --- Org format NotDeft note support  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See end of file for licensing information.

;;; Commentary:
;; Some support for `org-mode' used together with NotDeft.
;;
;; This feature requires no specific setup, as the public commands and
;; functions of this feature are autoloadable. However, see also
;; `notdeft-org9', which is an optional extension to this feature, and
;; does require setting up for use.

;;; Code:

(eval-when-compile
  (require 'subr-x))

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
(defun notdeft-org-complete-deft-link (&optional rich)
  "Define completion for Org \"deft:\" links.
Optionally request RICH behavior from
`notdeft-select-file'."
  (let* ((file (notdeft-select-file :rich rich :initial-query (notdeft-string-from-region)))
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

(defun notdeft-make-notdeft-link (query &optional desc)
  "Turn QUERY and DESC into a \"notdeft:\" link.
QUERY should be a Xapian search query."
  (org-link-make-string (concat "notdeft:" query) desc))

;;;###autoload
(defun notdeft-org-store-note-deft-link ()
  "Store a \"deft:\" link for the current note.
Store it to the note, without any search string. Like
`org-store-link', store the link into `org-stored-links'."
  (interactive)
  (let ((old-file (notdeft-current-filename t t)))
    (when old-file
      (let* ((name (file-name-nondirectory old-file))
	     (link (concat "deft:" name))
	     (desc (notdeft-title-from-file-content old-file)))
        ;; We don't avoid duplicates here, which may not be the usual
        ;; behavior of Org (see `org-link--add-to-stored-links').
	(push (list link desc) org-stored-links)
	(message "Stored: %s" (or desc link))))))

;;;###autoload
(defun notdeft-org-save-deft-link-as-kill ()
  "Copy a \"deft:\" link for the current note.
Add it to the front of the kill ring. Save only the link without
the description, unlike with `notdeft-org-store-note-deft-link'."
  (interactive)
  (let ((old-file (notdeft-current-filename t t)))
    (when old-file
      (let* ((name (file-name-nondirectory old-file))
	     (link (concat "deft:" name)))
        (kill-new link)
	(message "Copied: %s" link)))))

;; Exists only in some installations, so do not `check-declare'.
(declare-function org-link-precise-link-target nil)

;;;###autoload
(defun notdeft-org-store-deft-link (&optional interactive?)
  "Store the current note location as a \"deft:\" link.
Use `org-store-link' to invoke this function. If invoked in
`notdeft-mode', then store a link to the selected note, if any. If the
function `org-link-precise-link-target' is undefined (as it is in older
versions of Org), then always return nil if in a note buffer. Otherwise
return a link to the note and any target within it as a \"deft:\" link.
Ignore the INTERACTIVE? argument, which is accepted for
`org-store-link-functions' compatibility."
  (ignore interactive?)
  (if (eq major-mode 'notdeft-mode)
      (when-let* ((old-file (notdeft-current-filename t)))
        (let* ((name (file-name-nondirectory old-file))
	       (link (concat "deft:" name))
	       (desc (notdeft-title-from-file-content old-file)))
          (org-link-store-props
           :type "deft"
           :link link
           :description desc)
          link))
    (when-let* (((or (not org-link-context-for-files)
                     (fboundp 'org-link-precise-link-target)))
                (old-file (notdeft-current-filename t)))
      (let* ((name (file-name-nondirectory old-file))
             (target ;; (SEARCH-STRING DESC POSITION)
              (and org-link-context-for-files
                   (funcall 'org-link-precise-link-target)))
	     (link (concat "deft:" name))
             desc)
        (if target
            (pcase-exhaustive target
              (`(,search-string ,search-desc ,_position)
               (when search-string
                 (setq link (concat link "::" search-string)))
               (when search-desc
                 (setq desc search-desc))))
          (when-let* ((title (notdeft-title-from-file-content old-file)))
            (setq desc title)))
        (org-link-store-props
         :type "deft"
         :link link
         :description desc)
        link))))

;;;###autoload
(defun notdeft-org-link-existing-note (notename &optional description region)
  "Create a \"deft:\" link to an existing note.
Link to a note by NOTENAME, inserting a link DESCRIPTION if it is
non-nil. Insert the created link at point, unless REGION in
specified (as a list of two positions), in which case replace
that region. When called interactively, use `notdeft-select-file'
to choose the link target NOTENAME. Query for a note DESCRIPTION,
offering to use the text of any active REGION as the title, or
the result of calling `notdeft-describe-link' otherwise. If
multiple notes have the same NOTENAME, pick any one of them for
deriving a description. When called with one
\\[universal-argument], ask for the DESCRIPTION first, and then
offer to use that as any search query. When called with two
\\[universal-argument]s offer to use a `notdeft-describe-link'
DESCRIPTION even if there is an active region."
  (interactive
   (progn
     (barf-if-buffer-read-only)
     (let* ((pfx (prefix-numeric-value current-prefix-arg))
            (desc-first (= pfx 4))
	    (region (when mark-active
		      (list (region-beginning) (region-end))))
	    (region-str (when region
                          (notdeft-chomp-nullify
		           (apply #'buffer-substring-no-properties region))))
            (description (if desc-first
                             (notdeft-org-read-link-description region-str)
                           region-str))
	    (file
	     (notdeft-select-file :initial-query description :order-by 'relevance))
	    (description
             (when file
	       (if (and desc-first description)
                   description
	         (notdeft-org-read-link-description
                  (if (or (not description) (= pfx 16))
                      (notdeft-chomp-nullify
		       (funcall notdeft-describe-link file))
                    description)))))
	    (notename (when file
			(file-name-nondirectory file))))
       (list notename description region)))
   org-mode)
  (when notename
    (when region
      (apply #'delete-region region))
    (insert (notdeft-make-deft-link notename description))))

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
  (notdeft-open-query :query query :rich rich))

;;;###autoload
(defun notdeft-org-store-notdeft-link (&optional interactive?)
  "Store the current NotDeft search as an Org link.
Use `org-store-link' to invoke this function. If invoked in
`notdeft-mode' without a selected note, then store a link to the current
Xapian query, if any, and return the link text. In other cases and modes
return nil. Ignore the INTERACTIVE? argument, which is accepted for
`org-store-link-functions' compatibility."
  (ignore interactive?)
  (when-let* (((and (fboundp 'notdeft-filename-at-point)
                    (eq major-mode 'notdeft-mode)
                    (not (funcall 'notdeft-filename-at-point))))
              (query (symbol-value 'notdeft-xapian-query))
              (query (notdeft-chomp-nullify query)))
    (let ((link (concat "notdeft:" query)))
      (org-link-store-props
       :type "notdeft"
       :link link)
      link)))

;;;###autoload
(defun notdeft-org-insert-notdeft-link-from-history (&optional direct)
  "Insert a past Xapian search as an Org link.
Let the user select an entry from `notdeft-xapian-query-history',
if any. Also ask for a description for the \"notdeft:\" link. If
DIRECT is non-nil or when called with \\[universal-argument] just
use the latest history entry, without asking for a description."
  (interactive "*P")
  (when-let ((query (if direct
                        (car notdeft-xapian-query-history)
                      (ido-completing-read "Query: " notdeft-xapian-query-history nil t)))
             (query (notdeft-chomp-nullify query)))
    (when query
      (let ((desc (unless direct
                    (notdeft-org-read-link-description))))
        (insert (notdeft-make-notdeft-link query desc))))))

;;;###autoload
(defun notdeft-org-search-for-heading (&optional as-phrase rich)
  "Query for current Org heading text.
Optionally execute the search AS-PHRASE search if called
interactively with a \\[universal-argument] prefix argument, or
with RICH options if called with two such arguments."
  (interactive
   (let ((prefix (prefix-numeric-value current-prefix-arg)))
     (list (= prefix 4)
	   (>= prefix 16)))
   org-mode)
  (when-let ((title
              (notdeft-chomp-nullify
	       (save-excursion
	         (org-back-to-heading t)
	         (nth 4 (org-heading-components))))))
    (notdeft-open-query :query (if as-phrase
                                   (notdeft-string-as-phrase-query title)
                                 title)
                        :rich rich)))

;;;###autoload
(defun notdeft-org-move-subtree-into-file (&optional pfx)
  "Move the current Org subtree into a separate NotDeft note file.
Use the heading as the \"#+TITLE\", and derive a default filename
based on it. Save the note file into a NotDeft directory relative
to the edited file, as applicable, falling back to the current
`notdeft-directory' or interactive querying. The prefix argument
PFX is as for `notdeft-new-file'."
  (interactive "*P" org-mode)
  (if (not (org-at-heading-p))
      (message "Not at a heading")
    (save-excursion
      (let ((title (nth 4 (org-heading-components))))
	(org-cut-subtree)
	(let* ((file (notdeft-sub--new-file nil nil title pfx))
	       (buf (get-file-buffer file)))
	  (with-current-buffer buf
	    (goto-char (point-min))
	    (insert "#+TITLE: " title "\n\n")
	    (org-paste-subtree 1)))))))

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
