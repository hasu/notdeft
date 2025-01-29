;;; notdeft-transcient.el --- Notdeft transcient menu -*- lexical-binding: t; -*-

;; Require-package: ((transient))
;; Copyright (C) 2025 by the author.
;; All rights reserved.
;; Author: DarkBuffalo <db@gnu.re>
;; See end of file for licensing information.

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
    ("f" "search/open" notdeft-search-find-file)
    ("x o" "Lucky search" notdeft-lucky-find-file)]

   ["Movement"
    ("b" "Switch to note" notdeft-switch-to-note-buffer)
    ("B" "Switch to buffer" notdeft-switch-to-buffer)]

   ["Other"
    ("." "NotDeft" notdeft)
    ("q" "Quit" transient-quit-one)]])

(provide 'notdeft-transient)
;;; notdeft-transient.el ends here

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above
;;    copyright notice, this list of conditions and the following
;;    disclaimer in the documentation and/or other materials provided
;;    with the distribution.
;; 3. Neither the names of the copyright holders nor the names of any
;;    contributors may be used to endorse or promote products derived
;;    from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.
