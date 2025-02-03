;;; notdeft-develop.el --- Utilities for NotDeft development  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; See end of file for licensing information.

;;; Commentary:
;; A command for setting up `company-clang' completion for
;; "notdeft-xapian.cc". Intended for use with the "xapian" directory
;; as the `default-directory'. The implementation is somewhat platform
;; specific.

;;; Code:

(require 'files-x)

(defvar notdeft-xapian-clang-modes '(c++-mode c++-ts-mode)
  "Major modes relevant to C++ development.")

;;;###autoload
(defun notdeft-xapian-configure-company-clang-arguments ()
  "Set `company-clang-arguments' as a directory local.
Set them for `notdeft-xapian-clang-modes'."
  (interactive)
  (let ((args (when (and (executable-find "pkg-config")
                         (executable-find "xapian-config"))
                (let ((tclap-args
                       (split-string (shell-command-to-string "pkg-config --cflags tclap")))
                      (xapian-args
                       (split-string (shell-command-to-string "xapian-config --cxxflags"))))
                  (append '("-std=c++11") tclap-args xapian-args)))))
    (dolist (mode notdeft-xapian-clang-modes)
      (add-dir-local-variable mode 'company-clang-arguments args))))

(provide 'notdeft-develop)

;;; notdeft-develop.el ends here

;; NotDeft, a note manager for Emacs
;; Copyright (C) 2025  Tero Hasu
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
