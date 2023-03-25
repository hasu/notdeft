;;; notdeft-global.el --- Global NotDeft keymap  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; See end of file for licensing information.

;;; Commentary:
;; A keymap of NotDeft commands usable from outside `notdeft-mode'. It
;; is bound both as a variable and a function, to the name
;; `notdeft-global-map'.
;;
;; The `notdeft-global' feature is intended to be loadable before
;; `notdeft' itself, and to support that all the commands bound here
;; should be autoloadable. They should also be such that they could
;; conceivably be invoked when NotDeft is not yet running. Additional
;; commands can be bound from outside this feature once they have been
;; defined or made `autoload'able.
;;
;; Additional predefined autoloadable commands that could have been
;; bound here include `notdeft' and `notdeft-open-query', as they are
;; useful for opening a `notdeft-mode' buffer. However, for these
;; important commands we leave the choice of a convenient keybinding
;; to the user.

;;; Code:

(defvar notdeft-global-map (make-sparse-keymap)
  "Global keymap for NotDeft.

\\{notdeft-global-map}")

(fset 'notdeft-global-map notdeft-global-map)

;; file management
(define-key notdeft-global-map (kbd "C-n") #'notdeft-new-file)
(define-key notdeft-global-map (kbd "C-m") #'notdeft-new-file-named)
(define-key notdeft-global-map (kbd "C-x C-f") #'notdeft-find-file)
(define-key notdeft-global-map (kbd "C-x C-w") #'notdeft-save-buffer)
(define-key notdeft-global-map (kbd "C-d") #'notdeft-delete-file)
(define-key notdeft-global-map (kbd "C-r") #'notdeft-rename-file)
(define-key notdeft-global-map (kbd "C-x s") #'notdeft-move-into-subdir)
(define-key notdeft-global-map (kbd "C-x e") #'notdeft-change-file-extension)
(define-key notdeft-global-map (kbd "C-a") #'notdeft-archive-file)
(define-key notdeft-global-map (kbd "C-x d") #'notdeft-open-in-deft)

;; state
(define-key notdeft-global-map (kbd "C-j") #'notdeft-chdir)
(define-key notdeft-global-map (kbd "C-x c") #'notdeft-gc)
(define-key notdeft-global-map (kbd "C-x r") #'notdeft-reindex)

;; search
(define-key notdeft-global-map (kbd "C-f") #'notdeft-query-select-find-file)
(define-key notdeft-global-map (kbd "C-x o") #'notdeft-lucky-find-file)

(provide 'notdeft-global)

;;; notdeft-global.el ends here

;; NotDeft, a note manager for Emacs
;; Copyright (C) 2017  Tero Hasu
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
