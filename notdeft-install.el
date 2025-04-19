;;; notdeft-install.el --- NotDeft installer  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; See end of file for licensing information.

;;; Commentary:
;; Functionality for setting up NotDeft from its source distribution,
;; without using a package manager.
;;
;; Suggested use:
;;  (require 'notdeft-install)
;;  (notdeft-install)

;;; Code:

(require 'bytecomp)

(declare-function notdeft-xapian-make-program "notdeft-xapian-make")

(eval-when-compile
  (defmacro notdeft-static-if (cnd thn els)
    "Behave like `static-if'.
Check CND at expansion time and emit either THN or ELS."
    (declare (indent 2))
    (if (eval cnd lexical-binding) thn els)))

(defun notdeft-sub--install-autoloads (feature output-name)
  "Generate autoloads for package.
Locate the package by looking for contained FEATURE. Write autoloads
into file of OUTPUT-NAME."
  (when-let ((lib (locate-library feature)))
    (let* ((dir (file-name-directory lib))
           (output-file (expand-file-name output-name dir)))
      ;; Emacs 28.1 or later is required for
      ;; `make-directory-autoloads'. That in turn is already
      ;; deprecated in Emacs 29.1 in favor of `loaddefs-generate'.
      (notdeft-static-if (fboundp 'loaddefs-generate)
          (loaddefs-generate dir output-file)
        (make-directory-autoloads dir output-file)))
    (load output-name nil nil t)))

(defun notdeft-transient-installable-p ()
  "Whether `notdeft-transient' dependencies are installed."
  (and (require 'transient nil t)
       (boundp 'transient-version)
       (let ((ver (version-to-list transient-version)))
         (version-list-<= '(0 5) ver))))

(defun notdeft-install-autoloads ()
  "Generate NotDeft autoloads and load them.
Do that for `notdeft', and also for `notdeft-ivy' and
`notdeft-transient' if they exist and their dependencies are available
and installed."
  (notdeft-sub--install-autoloads "notdeft" "notdeft-autoloads.el")
  (when (require 'ivy nil t)
    (notdeft-sub--install-autoloads "notdeft-ivy" "notdeft-ivy-autoloads.el"))
  (when (notdeft-transient-installable-p)
    (notdeft-sub--install-autoloads "notdeft-transient" "notdeft-transient-autoloads.el")))

(defun notdeft-sub--byte-compile (sources feature force)
  "Byte-compile NotDeft SOURCES for FEATURE.
Do that only if the FEATURE can be located. Optionally FORCE the
compilation."
  (when-let ((lib (locate-library feature)))
    (let ((dir (file-name-directory lib)))
      (dolist (file sources)
        (let ((file (expand-file-name file dir)))
          (byte-recompile-file file force 0))))))

(defun notdeft-install-bytecode (&optional force)
  "Generate NotDeft Emacs Lisp \".elc\" files.
Optionally FORCE byte-compilation even when existing bytecode
files appear to be up-to-date."
  (notdeft-sub--byte-compile
   '("notdeft-base.el"
     "notdeft-util.el"
     "notdeft-xapian.el"
     "notdeft.el"
     "notdeft-global.el"
     "notdeft-org.el"
     "notdeft-org9.el"
     "notdeft-xapian-make.el"
     "notdeft-install.el"
     "notdeft-mode.el"
     "notdeft-config.el"
     "notdeft-path.el"
     "notdeft-develop.el")
   "notdeft" force)
  (when (require 'ivy nil t)
    (notdeft-sub--byte-compile '("notdeft-ivy.el") "notdeft-ivy" force))
  (when (notdeft-transient-installable-p)
    (notdeft-sub--byte-compile '("notdeft-transient.el") "notdeft-transient" force)))

;;;###autoload
(defun notdeft-install (&optional force)
  "Generate NotDeft autoloads and binaries.
Optionally FORCE byte-compilation even when existing bytecode
files appear to be up-to-date."
  (interactive "P")
  (notdeft-install-autoloads)
  (require 'notdeft-autoloads)
  (notdeft-install-bytecode force)
  (require 'notdeft-xapian-make)
  (notdeft-xapian-make-program force))

(provide 'notdeft-install)

;;; notdeft-install.el ends here

;; NotDeft, a note manager for Emacs
;; Copyright (C) 2020-2025  Tero Hasu
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
