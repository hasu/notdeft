;;; notdeft-global-hydra.el --- Hydra for NotDeft  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; See end of file for licensing information.

;;; Commentary:
;; A hydra for some of NotDeft's globally usable commands; intended to
;; be “helpful” without much support for command chaining. Defines the
;; command `notdeft-global-hydra/body', and adds the hydra bindings to
;; an internal keymap. The `notdeft-global-hydra/body' command can be
;; bound as desired to access the commands with short key sequences
;; and textual hints in the Hydra command group context. It makes
;; available roughly the same bindings as in `notdeft-global-map', but
;; in a different way.
;;
;; To bind the hydra, one can for example:
;;   (global-set-key [f6] 'notdeft-global-hydra/body)

;;; Code:

(require 'hydra)
(require 'notdeft)
(require 'notdeft-mode)

(declare-function notdeft-mode-hydra/body "notdeft-mode-hydra")

;;;###autoload
(defhydra notdeft-global-hydra (:exit t)
  "NotDeft"
  ;; file management
  ("n" notdeft-new-file "create")
  ("m" notdeft-new-file-named "create named")
  ("C-f" notdeft-find-file "open")
  ("C-w" notdeft-save-buffer "save" :exit nil)
  ("d" notdeft-delete-file "delete")
  ("r" notdeft-rename-file "rename")
  ("v" notdeft-move-file "move")
  ("s" notdeft-move-into-subdir "move into subdir")
  ("e" notdeft-change-file-extension "change ext")
  ("a" notdeft-archive-file "archive")
  ("i" notdeft-show-file-directory "show dir" :exit nil)
  ;; state
  ("j" notdeft-chdir "chdir" :exit nil)
  ("g" notdeft-refresh "refresh" :exit nil)
  ("x c" notdeft-gc "GC" :exit nil)
  ("x r" notdeft-reindex "reindex" :exit nil)
  ;; search
  ("o" notdeft-open-query "search")
  ("f" notdeft-search-find-file "search/open")
  ("x o" notdeft-lucky-find-file "lucky search")
  ;; movement
  ("b" notdeft-switch-to-note-buffer "switch to note")
  ("B" notdeft-switch-to-buffer "switch to buffer")
  ;; other
  ("z" (when (notdeft-buffer-p) (notdeft-mode-hydra/body)) "more")
  ("." notdeft "NotDeft")
  ("C-g" nil "cancel"))

(provide 'notdeft-global-hydra)

;;; notdeft-global-hydra.el ends here

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
