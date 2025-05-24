;;; notdeft-config.el --- NotDeft configuration support  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See end of file for licensing information.

;;; Commentary:
;; An optional feature with some conveniences for configuring
;; `notdeft-note-mode' so that it gets activated automatically, but
;; only for file buffers whose file is under `notdeft-directories' and
;; has a file name `notdeft-extension' or one of the
;; `notdeft-secondary-extensions'.
;;
;; To enable this feature the `notdeft-note-mode-enable'
;; function should be registered with the mode hooks of all the
;; NotDeft note editing major modes.
;;
;; Suggested use:
;;  (add-hook 'text-mode-hook 'notdeft-note-mode-enable)

;;; Code:

(require 'notdeft-base) ;; for `notdeft-directories', `notdeft-extension', etc.

(declare-function notdeft-note-mode "notdeft") ;; autoloadable

;;;###autoload
(defun notdeft-note-mode-enable ()
  "Conditionally enable `notdeft-note-mode'."
  (when-let ((file (buffer-file-name))
             (ext (file-name-extension file)))
    (when (and (or (equal ext notdeft-extension)
                   (member ext notdeft-secondary-extensions))
               (cl-some
                (lambda (dir)
                  (file-in-directory-p file dir))
                notdeft-directories))
      (notdeft-note-mode 1))))

(provide 'notdeft-config)

;;; notdeft-config.el ends here

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
