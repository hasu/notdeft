;;; notdeft-develop.el --- Utilities for NotDeft development  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; See end of file for licensing information.

;;; Commentary:
;; Commands for use in setting up Emacs for editing and compiling
;; "notdeft-xapian.cc". Intended for use with the "xapian" directory
;; as the `default-directory'. The implementations are somewhat
;; platform specific.

;;; Code:

(require 'compile)
(require 'files-x)
(require 'notdeft-xapian-make)

(defvar notdeft-xapian-clang-modes '(c++-mode c++-ts-mode)
  "Major modes relevant to C++ development.")

;;;###autoload
(defun notdeft-xapian-configure-compile-command ()
  "Set `compile-command' as a directory local.
Set them for `notdeft-xapian-clang-modes'."
  (interactive)
  (let ((exe-file (notdeft-xapian-program-target-path)))
    (when exe-file
      (let ((command (notdeft-xapian-program-compile-command exe-file)))
        (dolist (mode notdeft-xapian-clang-modes)
          (add-dir-local-variable mode 'compile-command command))))))

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

;;;###autoload
(defun notdeft-xapian-set-compile-command ()
  "Set notdeft-xapian `compile-command' locally.
Set it locally for the current buffer, without persistence."
  (interactive)
  (let ((exe-file (notdeft-xapian-program-target-path)))
    (when exe-file
      (let ((command (notdeft-xapian-program-compile-command exe-file)))
        (setq-local compile-command command)
        (message "Local `compile-command': %s" command)))))

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
