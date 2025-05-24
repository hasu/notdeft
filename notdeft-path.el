;;; notdeft-path.el --- NotDeft directory dynamic resolution  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See end of file for licensing information.

;;; Commentary:
;; A system for resolving `notdeft-directories' dynamically, based on
;; a configurable `notdeft-path' specification. Might be useful when
;; storing some NotDeft directories on removable filesystems, allowing
;; the command `notdeft-refresh' to be used to update the available
;; `notdeft-directories' list. The function
;; `notdeft-refresh-directories' should be called where necessary to
;; ensure that the list is kept up to date.
;;
;; Suggested use:
;;  (require 'notdeft-path)
;;  (notdeft-refresh-directories)
;;  (add-hook 'notdeft-pre-refresh-hook 'notdeft-refresh-directories)

(require 'cl-lib)
(require 'notdeft-base)

;;; Code:

(defcustom notdeft-path '("~/.deft/")
  "NotDeft directory search path.
A list of strings, or a function returning a list of strings. The
strings should name directories, which may or may not exist."
  :type '(choice
	  (repeat (string :tag "Directory"))
	  (function :tag "Function"))
  :safe (lambda (lst) (cl-every #'stringp lst))
  :group 'notdeft)

(defvar notdeft-directories-changed-hook nil
  "Hook run after each refresh of `notdeft-directories'.
It is called by `notdeft-refresh-directories'.")

(defun notdeft-existing-directories (dirs)
  "Return a list of existing directories DIRS."
  (mapcar #'file-name-as-directory
	  (cl-remove-if-not #'file-directory-p dirs)))

(defun notdeft-resolve-directories ()
  "Resolve directories from `notdeft-path'.
Return the result as a list of strings that are syntactically
directory names, and name existing directories."
  (let ((lst (if (functionp notdeft-path)
		 (funcall notdeft-path)
	       notdeft-path)))
    (unless (listp lst)
      (error "Expected a list: %S" lst))
    (dolist (elem lst)
      (unless (stringp elem)
	(error "Expected a string: %S" elem)))
    (notdeft-existing-directories lst)))

(defun notdeft-refresh-directories ()
  "Update `notdeft-directories' based on `notdeft-path'.
Only include existing directories. Also clear `notdeft-directory'
if it is no longer one of the `notdeft-directories'."
  (setq notdeft-directories (notdeft-resolve-directories))
  (when (and (boundp 'notdeft-directory) notdeft-directory)
    (unless (and (file-directory-p notdeft-directory)
		 (cl-some (lambda (dir)
			    (file-equal-p notdeft-directory dir))
			  notdeft-directories))
      (setq notdeft-directory nil)))
  (run-hooks 'notdeft-directories-changed-hook)
  notdeft-directories)

(provide 'notdeft-path)

;;; notdeft-path.el ends here

;; NotDeft, a note manager for Emacs
;; Copyright (C) 2018-2022  Tero Hasu
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
