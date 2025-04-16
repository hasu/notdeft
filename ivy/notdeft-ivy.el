;;; notdeft-ivy.el --- Ivy completion for NotDeft  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; Package-Requires: (ivy)
;; See end of file for licensing information.

;;; Commentary:
;; An Ivy-based implementation of `notdeft-compread-file-function'.
;;
;; Suggested use:
;;  (add-to-list 'ivy-re-builders-alist '(notdeft-ivy-compread-file . ivy--regex-ignore-order))
;;  (setq notdeft-compread-file-function 'notdeft-ivy-compread-file)

;;; Code:

(require 'ivy)

;;;###autoload
(defun notdeft-ivy-compread-file (files &optional prompt)
  "Present a choice of FILES with `ivy-read'.
Only present the non-directory component of each file. There may
be duplicates of the same non-directory name. If non-nil, use the
specified PROMPT. Return the path of the selected file."
  (let* ((choices
	  (mapcar
	   (lambda (file)
	     (propertize (file-name-nondirectory file) 'path file))
	   files))
	 (file
	  (get-text-property
	   0 'path
	   (ivy-read
	    (or prompt "File: ")
	    choices
	    :history 'notdeft-compread-file-history
	    :require-match t
	    :caller 'notdeft-ivy-compread-file))))
    file))

(provide 'notdeft-ivy)

;;; notdeft-ivy.el ends here

;; NotDeft, a note manager for Emacs
;; Copyright (C) 2020  Tero Hasu
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
