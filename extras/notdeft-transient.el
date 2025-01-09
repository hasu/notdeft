;;; notdeft-transcient.el --- Notdeft transcient menu -*- lexical-binding: t; -*-

;; Require-package: ((transient))
;; Copyright (C) 2025 by the author.
;; All rights reserved.
;; Author: DarkBuffalo <db@gnu.re>
;; See "notdeft.el" for licensing information.

;;; Commentary:
;; Menu with transcient

;;; Code:

(require 'transient)

;;;###autoload
(transient-define-prefix notdeft-transient-global-menu ()
  "Help transient for notdeft."
  [
   ["File management"
    ("n" "Create" notdeft-new-file)
    ("m" "Create named" notdeft-new-file-named)
    ("C-f" "Open" notdeft-find-file)
    ("C-w" "Save" notdeft-save-buffer)
    ("d" "Delete" notdeft-delete-file)
    ("r" "Rename" notdeft-rename-file)
    ("v" "Move" notdeft-move-file)
    ("s" "Move into subdir" notdeft-move-into-subdir)
    ("e" "Change extension" notdeft-change-file-extension)
    ("a" "archive" notdeft-archive-file)
    ("i" "Show dir" notdeft-show-file-directory)
    ("x d" "Open in deft" notdeft-open-in-deft)]

   ["State"
    ("j" "chdir" notdeft-chdir)
    ("g" "refresh" notdeft-refresh)
    ("x c" "GC" notdeft-gc )
    ("x r" "reindex" notdeft-reindex)]

   ["Search"
    ("o" "Search" notdeft-open-query)
    ("f" "search/open" notdeft-query-select-find-file)
    ("x o" "Lucky search" notdeft-lucky-find-file)]

   ["Movement"
    ("b" "Switch to note" notdeft-switch-to-note-buffer)
    ("B" "Switch to buffer" notdeft-switch-to-buffer)]

   ["Other"
    ("." "NotDeft" notdeft)
    ("q" "Quit" transient-quit-one)]])

(provide 'notdeft-transient)
;;; notdeft-transient.el ends here
