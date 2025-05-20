;;; notdeft-util.el --- Internal utilities for NotDeft  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;;   Jason R. Blevins <jrblevin@sdf.org>
;; Maintainer: Tero Hasu <tero@hasu.is>
;; SPDX-License-Identifier: GPL-3.0-or-later AND BSD-3-Clause
;; See end of file for licensing information.

;;; Commentary:
;; A small collection of utilities for internal use by NotDeft.

(eval-when-compile
  (require 'subr-x))

;;; Code:

(defun notdeft-chomp (str)
  "Trim leading and trailing whitespace from STR."
  (replace-regexp-in-string
   "\\(\\`[[:space:]\n\r]+\\|[[:space:]\n\r]+\\'\\)"
   "" str))

(defun notdeft-chomp-nullify (str &optional trim)
  "Return trimmed STR if it is non-empty.
Otherwise return nil. Optionally, use function TRIM to further
trim any result string."
  (when str
    (let ((str (notdeft-chomp str)))
      (unless (string= "" str)
	(if trim (funcall trim str) str)))))

(defun notdeft-read-query (hist &optional initial)
  "Read a query string, interactively.
Read it from the minibuffer. Use and update history bound to the
HIST symbol in querying. Optionally fill in the specified INITIAL
input. Return the read string, or nil if no non-blank query is
given."
  (let* ((hist (if (not initial)
		   hist
                 (let ((history-delete-duplicates t))
                   (add-to-history hist initial))
		 (cons hist 1)))
	 (s (read-from-minibuffer
	    "Query: " ;; PROMPT
	    initial nil nil ;; INITIAL-CONTENTS KEYMAP READ
	    hist ;; HIST
	    nil ;; DEFAULT-VALUE
	    t ;; INHERIT-INPUT-METHOD
	    )))
    (when (and s (not (string-blank-p s)))
      s)))

(provide 'notdeft-util)

;;; notdeft-util.el ends here

;; NotDeft, a note manager for Emacs
;; Copyright (C) 2011-2023  Tero Hasu
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

;; This file incorporates work covered by the following copyright and
;; permission notice:
;;
;; Copyright (C) 2011 Jason R. Blevins <jrblevin@sdf.org>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation  and/or other materials provided with the distribution.
;; 3. Neither the names of the copyright holders nor the names of any
;;    contributors may be used to endorse or promote products derived from
;;    this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;; POSSIBILITY OF SUCH DAMAGE.
