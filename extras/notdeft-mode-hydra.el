;;; notdeft-mode-hydra.el --- Hydra for `notdeft-mode'  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; See end of file for licensing information.

;;; Commentary:
;; One possible definition of a hydra for `notdeft-mode'. Intended to
;; make it more convenient to execute multiple note file operations
;; consecutively, using short (mostly single letter) key combinations,
;; which are mostly the same as for `notdeft-global-hydra'. The
;; difference here is that the commands are mostly chainable.
;;
;; To set the hydra up for use, you may:
;;  (autoload 'notdeft-mode-hydra/body "notdeft-mode-hydra" nil t)
;;  (eval-after-load "notdeft-mode"
;;   '(define-key notdeft-mode-map (kbd "C-c h") 'notdeft-mode-hydra/body))

;;; Code:

(require 'hydra)

(autoload 'notdeft-global-hydra/body "notdeft-global-hydra" nil t)

;; Mode-private functions without autoloads.
(declare-function notdeft-filter "notdeft-mode")
(declare-function notdeft-filter-clear "notdeft-mode")
(declare-function notdeft-grep-for-filter "notdeft-mode")
(declare-function notdeft-query-clear "notdeft-mode")
(declare-function notdeft-query-edit "notdeft-mode")
(declare-function notdeft-mode-open-file "notdeft-mode")

(defhydra notdeft-mode-hydra ()
  "notdeft-mode"
  ;; file management
  ("RET" notdeft-mode-open-file "open" :exit t)
  ("n" notdeft-new-file "create" :exit t)
  ("N" notdeft-new-file-named "create named" :exit t)
  ("d" notdeft-delete-file "delete")
  ("r" notdeft-rename-file "rename")
  ("m" notdeft-move-file "move")
  ("s" notdeft-move-into-subdir "move into subdir")
  ("e" notdeft-change-file-extension "change ext")
  ("a" notdeft-archive-file "archive")
  ("i" notdeft-show-file-directory "show dir")
  ("x d" notdeft-open-in-deft "Deft" :exit t)
  ;; state
  ("j" notdeft-chdir "chdir")
  ("g" notdeft-refresh "refresh")
  ;; filtering
  ("l" notdeft-filter "filter" :exit t)
  ("c" notdeft-filter-clear "clear filter")
  ("f" notdeft-grep-for-filter "grep for filter" :exit t)
  ;; querying
  ("o" notdeft-query-edit "query" :exit t)
  ("O" notdeft-query-clear "clear query")
  ;; movement
  ("<up>" previous-line)
  ("<down>" next-line)
  ("b" notdeft-switch-to-note-buffer "switch to note" :exit t)
  ("B" notdeft-switch-to-buffer "switch to buffer" :exit t)
  ;; other
  ("z" notdeft-global-hydra/body "more" :exit t)
  ("q" quit-window "quit" :exit t)
  ("C-g" nil "cancel" :exit t))

(provide 'notdeft-mode-hydra)

;;; notdeft-mode-hydra.el ends here

;; NotDeft, a note manager for Emacs
;; Copyright (C) 2018  Tero Hasu
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
