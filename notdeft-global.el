;;; notdeft-global.el --- Global NotDeft keymap  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See end of file for licensing information.

;;; Commentary:
;; A keymap of NotDeft commands usable from outside `notdeft-mode'. It
;; is bound both as a variable and a function, to the name
;; `notdeft-global-map'. NotDeft does not predefine a keybinding for
;; the keymap, however, and so it is left to users to bind the
;; `notdeft-global-map' function to an appropriate prefix key if
;; desired.
;;
;; The keymap is quite conservative in what is included, favoring
;; commands that should be directly accessible due to their nature
;; (like `notdeft-lucky-find-file' and `notdeft-new-file'), and
;; commands that are likely to be used often (like
;; `notdeft-select-find-file' and `notdeft-rename-file'). It is
;; left to users to bind additional commands according to personal
;; tastes and requirements.
;;
;; The `notdeft-global' feature is optional in that no other NotDeft
;; feature depends on it, allowing users to replace it, create derived
;; keymaps, or to use different mechanisms for entering NotDeft
;; commands (e.g., Transient or Hydra menus).
;;
;; The feature is also designed to be loadable before `notdeft'
;; itself, and indeed some of the commands in the keymap (e.g.,
;; `notdeft' and `notdeft-select-find-file') are suitable for
;; launching NotDeft, and therefore causing the `notdeft' feature to
;; get loaded. In order to support this all the commands bound in the
;; keymap are autoloadable.

;;; Code:

(declare-function notdeft "notdeft-mode")
(declare-function notdeft-new-file "notdeft")
(declare-function notdeft-new-file-named "notdeft")
(declare-function notdeft-delete-file "notdeft")
(declare-function notdeft-rename-file "notdeft")
(declare-function notdeft-move-file "notdeft")
(declare-function notdeft-archive-file "notdeft")
(declare-function notdeft-show-file-directory "notdeft")
(declare-function notdeft-refresh "notdeft")
(declare-function notdeft-select-find-file "notdeft")
(declare-function notdeft-lucky-find-file "notdeft")
(declare-function notdeft-mode-open-query "notdeft-mode")
(declare-function notdeft-switch-to-note-buffer "notdeft")
(declare-function notdeft-switch-to-buffer "notdeft-mode")

(defvar notdeft-global-map (make-sparse-keymap)
  "Global keymap for NotDeft.

\\{notdeft-global-map}")

(fset 'notdeft-global-map notdeft-global-map)

;; UI
(define-key notdeft-global-map (kbd "e") #'notdeft)

;; file management
(define-key notdeft-global-map (kbd "C-n") #'notdeft-new-file)
(define-key notdeft-global-map (kbd "C-m") #'notdeft-new-file-named)
(define-key notdeft-global-map (kbd "C-d") #'notdeft-delete-file)
(define-key notdeft-global-map (kbd "C-r") #'notdeft-rename-file)
(define-key notdeft-global-map (kbd "m") #'notdeft-move-file)
(define-key notdeft-global-map (kbd "C-a") #'notdeft-archive-file)
(define-key notdeft-global-map (kbd "i") #'notdeft-show-file-directory)

;; state
(define-key notdeft-global-map (kbd "g") #'notdeft-refresh)

;; search
(define-key notdeft-global-map (kbd "C-f") #'notdeft-select-find-file)
(define-key notdeft-global-map (kbd "j") #'notdeft-lucky-find-file)
(define-key notdeft-global-map (kbd "o") #'notdeft-mode-open-query)

;; movement
(define-key notdeft-global-map (kbd "b") #'notdeft-switch-to-note-buffer)
(define-key notdeft-global-map (kbd "B") #'notdeft-switch-to-buffer)

(provide 'notdeft-global)

;;; notdeft-global.el ends here

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
