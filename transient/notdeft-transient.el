;;; notdeft-transient.el --- NotDeft commands with Transient menus  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; Package-Requires: (notdeft seq (transient "0.5.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later
;; See end of file for licensing information.

;;; Commentary:
;; This feature provides a higher-level `notdeft-search' command that
;; uses a Transient menu of options and actions to execute, so that it
;; is not necessary to remember all the available options and actions.
;;
;; The command can also be used as a `notdeft-open-query-function',
;; although due to its interactive nature it may not be particularly
;; fit for the purpose, as in the common case one likely wants to see
;; search results quicker and more directly.
;;
;; This feature requires Transient 0.5.0 or later, for the
;; refresh-suffixes support. Transient is built into Emacs 28.1 and
;; later, but not necessarily in a recent enough version, and
;; `package-install' may not always want to upgrade built-in packages.

;;; Code:

(require 'notdeft)
(require 'notdeft-mode)
(require 'seq)
(require 'subr-x)
(require 'transient)

(defvar-local notdeft-search-arguments nil
  "Current `notdeft-search' arguments, as a plist.
Used to track the search query through transient execution,
locally for a buffer.")

(defun notdeft-search-query ()
  "Current `notdeft-search' query, if any."
  (plist-get notdeft-search-arguments :query))

(defun notdeft-search-set-query (query)
  "Set QUERY to search arguments."
  (setq notdeft-search-arguments
        (notdeft-plist-put notdeft-search-arguments :query query)))

(defun notdeft-value-faced (str)
  "STR with `transient-value' face."
  (when str
    (propertize str 'face 'transient-value)))

(defun notdeft-search-query-description ()
  "Current `notdeft-search' query string description."
  (let ((query (notdeft-search-query)))
    (if query
        (propertize query 'face 'transient-value)
      (propertize "<unset>" 'face 'transient-inactive-value))))

(transient-define-suffix notdeft-search-query-display ()
  "Show info field showing current search query."
  :class 'transient-information
  :description #'notdeft-search-query-description
  (interactive))

(transient-define-suffix notdeft-search-query-edit ()
  "A `notdeft-search' query argument."
  :transient t
  (interactive)
  ;; We rely on :refresh-suffixes t refreshing the transient.
  ;; Alternatively we could use the private `transient--redisplay' API.
  (let ((query (notdeft-search-query)))
    (notdeft-search-set-query
     (notdeft-xapian-read-query query))))

(transient-define-suffix notdeft-search-query-clear ()
  "Clear `notdeft-search' query argument."
  :transient t
  :if 'notdeft-search-query
  (interactive)
  (notdeft-search-set-query nil))

(transient-define-suffix notdeft-search-query-add-tag ()
  "Add \"tag:\" for selected keyword."
  :transient t
  (interactive)
  (when-let ((keywords (notdeft-xapian-list-all-keywords))
             (keyword (completing-read "Keyword: " keywords)))
    (let ((tag (concat "tag:" keyword))
          (query (notdeft-search-query)))
      (notdeft-search-set-query
       (if query (concat tag " AND " query) tag)))))

(transient-define-suffix notdeft-search-query-as-phrase ()
  "Turn any query into a phrase query."
  :transient t
  :if 'notdeft-search-query
  (interactive)
  (when-let ((query (notdeft-search-query)))
    (notdeft-search-set-query
     (notdeft-string-as-phrase-query query))))

(defun notdeft-sample-string (str)
  "Return STR or a shorter sample of it."
  (declare (pure t))
  (when str
    (let ((len (length str)))
      (if (<= len 15)
          str
        (concat (substring str 0 7)
                (string 8230)
                (substring str (- len 7)))))))

(defun notdeft-search-use-region-description ()
  "Return `notdeft-search-use-region' description."
  (let ((str (notdeft-string-from-region)))
    (concat "Use region string"
            (if str
                (concat " \""
                        (notdeft-value-faced
                         (notdeft-sample-string str))
                        "\"")
              "")
            " as query")))

(transient-define-suffix notdeft-search-use-region ()
  "Use active region as `notdeft-search-query'."
  :transient t
  :if-non-nil 'mark-active
  :description #'notdeft-search-use-region-description
  (interactive)
  (notdeft-search-set-query
   (notdeft-chomp-nullify (notdeft-string-from-region))))

(transient-define-suffix notdeft-search-use-title ()
  "Use note title as `notdeft-search-query'."
  :transient t
  :if #'notdeft-note-buffer-p
  :description "Use note title as query"
  (interactive)
  (notdeft-search-set-query (notdeft-buffer-title)))

(defun notdeft-search-transient-args ()
  "List `notdeft-search' transient arguments.
The value of `notdeft-search-query' is not included."
  (transient-args 'notdeft-search))

(defun notdeft-search-transient-to-plist-args ()
  "Get search arguments as a plist.
Include any existing `notdeft-search-arguments' in addition to
translated `notdeft-search-transient-args'."
  (let ((args (notdeft-search-transient-args)))
    (seq-reduce
     (lambda (plist pair)
       (if pair (notdeft-plist-put plist (car pair) (cdr pair)) plist))
     (list (when-let ((v (transient-arg-value "--order-by=" args)))
             (cons :order-by (intern v)))
           (when (member "--new-buffer" args)
             (cons :new-buffer t))
           (when (member "--other-window" args)
             (cons :other-window t)))
     notdeft-search-arguments)))

(transient-define-suffix notdeft-search-show-arguments ()
  "Show `notdeft-search' arguments in CLI style.
Do that by showing a `message'."
  :transient t
  (interactive)
  (message "Search parameters: %S"
           (append (notdeft-search-transient-args)
                   (let ((query (notdeft-search-query)))
                     (and query (list query))))))

(transient-define-suffix notdeft-search-refresh ()
  "Refresh, with a completion message."
  :transient t
  (interactive)
  (notdeft-refresh)
  (message "Search index refreshed"))

(transient-define-suffix notdeft-search-select-find-file (&rest arguments)
  "Transient adapter for `notdeft-select-find-file'.
Pass ARGUMENTS to it."
  (interactive (notdeft-search-transient-to-plist-args))
  (when (plist-get arguments :query)
    (apply #'notdeft-select-find-file arguments)))

(transient-define-suffix notdeft-search-ido-find-file (&rest arguments)
  "Transient adapter for `notdeft-xapian-ido-search-find-file'.
Pass ARGUMENTS to it."
  (interactive (notdeft-search-transient-to-plist-args))
  (when (plist-get arguments :query)
    (apply #'notdeft-xapian-ido-search-find-file arguments)))

(transient-define-suffix notdeft-search-lucky-find-file (&rest arguments)
  "Transient adapter for `notdeft-lucky-find-file'.
Pass ARGUMENTS to it."
  (interactive (notdeft-search-transient-to-plist-args))
  (when (plist-get arguments :query)
    (apply #'notdeft-lucky-find-file arguments)))

(transient-define-suffix notdeft-search-open-query (&rest arguments)
  "Transient adapter for `notdeft-open-query'.
Pass ARGUMENTS to it."
  (interactive (notdeft-search-transient-to-plist-args))
  (when (plist-get arguments :query)
    (apply #'notdeft-open-query arguments)))

(transient-define-suffix notdeft-search-notdeft-mode-open-query (&rest arguments)
  "Transient adapter for `notdeft-mode-open-query'.
Pass ARGUMENTS to it."
  (interactive (notdeft-search-transient-to-plist-args))
  (when (plist-get arguments :query)
    (apply #'notdeft-mode-open-query arguments)))

(defun notdeft-search-initial-value ()
  "Initialize transient from `notdeft-search-arguments'.
Return the transient arguments value."
  (append
   (when (plist-get notdeft-search-arguments :new-buffer)
     '("--new-buffer"))
   (when (plist-get notdeft-search-arguments :other-window)
     '("--other-window"))
   (when-let ((order-by (plist-get notdeft-search-arguments :order-by)))
     (when (memq order-by '(relevance time name))
       (list (format "--order-by=%s" order-by))))))

;;;###autoload (autoload 'notdeft-search "notdeft-transient" nil t)
(transient-define-prefix notdeft-search (&rest arguments)
  "Search for files matching a query.
Accept ARGUMENTS as for `notdeft-open-query', but mostly ignore
them, instead letting the user choose interactively from a
variety of search and result presentation options and actions."
  :refresh-suffixes t
  ;; Not sure if it is documented that we can use a function to delay
  ;; transient initialization until `notdeft-search-arguments' is
  ;; already initialized. (ARGUMENTS is not in scope here.)
  :value #'notdeft-search-initial-value
  ["Options"
   ("-b" "Order results by" "--order-by=" :choices (relevance time name))
   ("-n" "Create new NotDeft buffer" "--new-buffer")
   ("-o" "Open file in other window" "--other-window")]
  ["Query"
   (notdeft-search-query-display)
   ("c" "Clear query" notdeft-search-query-clear)
   ("e" "Edit query" notdeft-search-query-edit)
   ("k" "Require keyword" notdeft-search-query-add-tag)
   ("q" "Quote as phrase" notdeft-search-query-as-phrase)
   ("r" notdeft-search-use-region)
   ("t" notdeft-search-use-title)]
  ["Action"
   ("f" "Select file" notdeft-search-select-find-file)
   ("g" "Refresh first" notdeft-search-refresh)
   ("i" "Select file with Ido" notdeft-search-ido-find-file)
   ("l" "Lucky search" notdeft-search-lucky-find-file)
   ("o" "Open query in NotDeft buffer" notdeft-search-notdeft-mode-open-query)
   ("p" "Show current parameters" notdeft-search-show-arguments)
   ("v" "Open query" notdeft-search-open-query)]
  (interactive)
  (setq notdeft-search-arguments
        (let ((query
               ;; We could also try falling back to
               ;; `notdeft-string-from-region' here, but it's also
               ;; quick to press "r" to fill it in.
               (or (plist-get arguments :query)
                   (plist-get arguments :initial-query))))
          (notdeft-plist-put arguments :query query)))
  (transient-setup 'notdeft-search))

(provide 'notdeft-transient)

;;; notdeft-transient.el ends here

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
