;;; notdeft-mode.el --- A Deft-style major mode for viewing search results  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;;   Jason R. Blevins <jrblevin@sdf.org>
;; Maintainer: Tero Hasu <tero@hasu.is>
;; SPDX-License-Identifier: GPL-3.0-or-later AND BSD-3-Clause
;; See end of file for licensing information.

;;; Commentary:
;; A dedicated major `notdeft-mode' for use in displaying search
;; results. The implementation is derived from code in Deft.
;;
;; This feature is optional, and need not be loaded in order to use
;; core NotDeft functionality. Some commands defined here (like
;; `notdeft' and `notdeft-mode-open-query') are autoloadable in order
;; to allow a NotDeft buffer to be opened before the feature has been
;; loaded.

;; File Browser

;; The NotDeft buffer is simply a local search engine result browser
;; which lists the titles of all text files matching a search query
;; (entered by first pressing TAB or `C-c C-o`), followed by short
;; summaries and last modified times. The title is taken to be the
;; first line of the file (or as specified by an Org "TITLE" file
;; property) and the summary is extracted from the text that follows.
;; By default, files are sorted in terms of the last modified date,
;; from newest to oldest.

;; Searching and Filtering

;; The primary operations of `notdeft-mode' are searching and
;; filtering. The list of files matching a search query can be further
;; narrowed down using a filter string, which will match both the
;; title and the body text. To initiate a filter, simply start typing.
;; Filtering happens on the fly. As you type, the file browser is
;; updated to include only files that match the current string.

;; To open the first matching file, simply press `RET`.  If no files
;; match your filter string, pressing `RET` will create a new file
;; using the string as the title.  This is a fast way to start
;; writing new notes.  The filename will be generated automatically.

;; To open files other than the first match, navigate up and down
;; using `C-p` and `C-n` and press `RET` on the file you want to open.

;; Press `C-c C-c` to clear the filter string and display all files
;; and `C-c e` to refresh the file browser using the current
;; filter string.

;; Static filtering is also possible by pressing `C-c C-l`.  This is
;; sometimes useful on its own, and it may be preferable in some
;; situations, such as over slow connections or on older systems,
;; where interactive filtering performance is poor.

;; Common file operations can also be carried out from within a
;; NotDeft buffer. Files can be renamed using `C-c C-r` or deleted
;; using `C-c C-d`. New files can also be created using `C-c C-n` for
;; quick creation or `C-c C-m` for a filename prompt. You can leave a
;; `notdeft-mode' buffer at any time with `C-c C-q`, which buries the
;; buffer, or kills it with a prefix argument `C-u`.

;; Archiving unused files can be carried out by pressing `C-c C-a`.
;; Files will be moved to `notdeft-archive-directory' under the note
;; file's NotDeft data directory. The archive directory is by default
;; named so that it gets excluded from searches.

;; Launching

;; Once you have NotDeft installed (with the necessary autoloads) you
;; can then run `M-x notdeft` to create a `notdeft-mode' buffer.
;; Alternatively, you may find it convenient to
;; `notdeft-mode-open-query' from anywhere, which then also opens a
;; `notdeft-mode' buffer for displaying the results.

;;; Code:

(require 'cl-lib)
(require 'notdeft)
(require 'widget)
(require 'wid-edit)

;;; Customization

(defcustom notdeft-time-format " %Y-%m-%d %H:%M"
  "Format string for modification times in the NotDeft browser.
Set to nil to hide."
  :type '(choice (string :tag "Time format")
		             (const :tag "Hide" nil))
  :safe #'string-or-null-p
  :group 'notdeft)

(defcustom notdeft-file-display-function nil
  "Formatter for file names in the NotDeft browser.
If a function, it must accept the filename and a maximum
width (as for `string-width') as its two arguments. Set to nil to
have no file information displayed."
  :type '(choice (function :tag "Formatting function")
		             (const :tag "Hide" nil))
  :safe #'null
  :group 'notdeft)

(defcustom notdeft-open-query-in-new-buffer nil
  "Whether to open query results in a new buffer.
More specifically, when this variable is non-nil, the
`notdeft-mode-open-query' command shows its matches in a freshly
created NotDeft buffer."
  :type 'boolean
  :safe #'booleanp
  :group 'notdeft)

(defcustom notdeft-cache-compaction-factor 20
  "Indicates file cache compaction frequency.
If nil, then no compaction takes place. If it is 0, then
compaction happens after every query. Otherwise the value should
be an integer specifying a limit for the cache size as a factor
of the maximum result set size."
  :type '(choice (integer :tag "Times maximum")
		             (const :tag "Unlimited" nil))
  :safe (lambda (v) (or (not v) (numberp v)))
  :group 'notdeft)

;;; Faces

(defgroup notdeft-faces nil
  "Faces used in NotDeft mode."
  :group 'notdeft
  :group 'faces)

(defface notdeft-header-face
  '((t :inherit font-lock-keyword-face :bold t))
  "Face for NotDeft header."
  :group 'notdeft-faces)

(defface notdeft-filter-string-face
  '((t :inherit font-lock-string-face))
  "Face for NotDeft filter string."
  :group 'notdeft-faces)

(defface notdeft-title-face
  '((t :inherit font-lock-function-name-face :bold t))
  "Face for NotDeft file titles."
  :group 'notdeft-faces)

(defface notdeft-separator-face
  '((t :inherit font-lock-comment-delimiter-face))
  "Face for NotDeft separator string."
  :group 'notdeft-faces)

(defface notdeft-summary-face
  '((t :inherit font-lock-comment-face))
  "Face for NotDeft file summary strings."
  :group 'notdeft-faces)

(defface notdeft-time-face
  '((t :inherit font-lock-variable-name-face))
  "Face for NotDeft last modified times."
  :group 'notdeft-faces)

;;; Constants

(defconst notdeft-buffer "*NotDeft*"
  "NotDeft buffer name.")

(defconst notdeft-separator " --- "
  "Text used to separate file titles and summaries.")

;;; Variables

(defvar notdeft-mode-hook nil
  "Hook run when entering NotDeft mode.")

(defvar notdeft-xapian-query nil
  "Current Xapian query string.
Where `notdeft-xapian-program' is available, it determines the
contents of `notdeft-all-files' for a NotDeft buffer. Local to
NotDeft mode buffers.")

(defvar notdeft-filter-string nil
  "Current filter string used by NotDeft.
A string that is treated as a list of whitespace-separated
strings (not regular expressions) that are required to match.
Local to a NotDeft mode buffer.")

(defvar notdeft-all-files nil
  "List of all files to list or filter.
Local to a NotDeft mode buffer.")

(defvar notdeft-current-files nil
  "List of files matching current filter.
Local to a NotDeft mode buffer.")

(defvar notdeft-hash-entries (make-hash-table :test 'equal)
  "Hash containing file information, keyed by filename.
Each value is of the form (MTIME CONTENT TITLE SUMMARY).")

(defvar notdeft-buffer-width nil
  "Width of NotDeft buffer, as currently drawn, or nil.
Local to a NotDeft mode buffer.")

(defvar notdeft-pending-updates 'requery
  "Whether there are pending updates for a NotDeft buffer.
Either nil for no pending updates, the symbol `redraw' for a
pending redrawing of the buffer, the symbol `refilter' for a
pending recomputation of `notdeft-current-files', or the symbol
`requery' for a pending querying of `notdeft-all-files'. Local to
a NotDeft mode buffer.")

;;; Metadata entry cache

(defun notdeft-cache-initialize ()
  "Initialize hash tables for caching files."
  (setq notdeft-hash-entries (make-hash-table :test 'equal)))

(defun notdeft-cache-clear ()
  "Clear the cache of file information."
  (clrhash notdeft-hash-entries))

(defun notdeft-cache-remove-file (file)
  "Remove FILE from the cache.
Do nothing if FILE is not in the cache."
  (remhash file notdeft-hash-entries))

(defun notdeft-buffers-mapc (function)
  "Call FUNCTION for each NotDeft buffer.
Do that for side effects, without passing any arguments, with the
buffer set as current. Return the value or the last call, or nil.
The called function may exit the loop early by calling
`cl-return', whose argument is then returned as the result of
this function."
  (cl-dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'notdeft-mode)
	      (funcall function)))))

(defun notdeft-cache-compact ()
  "Remove unused information from the file cache.
That is, remove information for files not currently in any
`notdeft-all-files' list. Return the compacted hash table."
  (let ((new-hash (make-hash-table :test 'equal)))
    (notdeft-buffers-mapc
     (lambda ()
       (cl-dolist (file notdeft-all-files)
	       (let ((entry (gethash file notdeft-hash-entries)))
	         (when entry
	           (puthash file entry new-hash))))))
    (setq notdeft-hash-entries new-hash)))

(defun notdeft-cache-gc ()
  "Remove obsolete file information from the cache.
That is, remove information for files that no longer exist. (This
is unsafe to do if currently using NotDeft mode buffers to view
search results including such files.) Return a list of the files
whose information was removed."
  (let (lst)
    (maphash (lambda (file _v)
	             (unless (file-exists-p file)
		             (setq lst (cons file lst))))
	           notdeft-hash-entries)
    (dolist (file lst lst)
      (notdeft-cache-remove-file file))))

(defun notdeft-cache-newer-file (file mtime)
  "Update cached information for FILE with given MTIME."
  (let* ((res (with-temp-buffer
		            (insert-file-contents file)
		            (notdeft-parse-buffer)))
	       (title (car res))
	       (summary (cadr res))
	       (contents
	        (concat file " "
		              (or title "") " "
		              (or (car (cddr res)) "") " "
		              (or summary ""))))
    (puthash file (list mtime contents title summary)
	           notdeft-hash-entries)))

(defun notdeft-cache-file (file)
  "Update file cache for FILE.
Keep any information for a non-existing file."
  (when (file-exists-p file)
    (let ((mtime-cache (notdeft-file-mtime file))
          (mtime-file (nth 5 (file-attributes file))))
      (when (or (not mtime-cache)
		            (time-less-p mtime-cache mtime-file))
	      (notdeft-cache-newer-file file mtime-file)))))

(defun notdeft-cache-update (files)
  "Update cached information for FILES."
  (mapc #'notdeft-cache-file files))

(defun notdeft-file-contents (file)
  "Retrieve complete contents of FILE from cache."
  (let ((entry (gethash file notdeft-hash-entries)))
    (nth 1 entry)))

(defun notdeft-file-mtime (file)
  "Retrieve modified time of FILE from cache."
  (let ((entry (gethash file notdeft-hash-entries)))
    (car entry)))

(defun notdeft-file-title (file)
  "Retrieve title of FILE from cache."
  (let ((entry (gethash file notdeft-hash-entries)))
    (nth 2 entry)))

(defun notdeft-file-summary (file)
  "Retrieve summary of FILE from cache."
  (let ((entry (gethash file notdeft-hash-entries)))
    (nth 3 entry)))

(defun notdeft-gc ()
  "Garbage collect to remove uncurrent NotDeft state.
More specifically, delete obsolete cached file and directory
information."
  (interactive)
  (notdeft-cache-gc))

;; File list display

(defun notdeft-print-header ()
  "Prints the NotDeft mode buffer header."
  (widget-insert
   (propertize "NotDeft: " 'face 'notdeft-header-face))
  (when notdeft-xapian-query
    (widget-insert
     (propertize (concat notdeft-xapian-query ": ")
		             'face 'notdeft-xapian-query-face)))
  (when notdeft-filter-string
    (widget-insert
     (propertize notdeft-filter-string 'face 'notdeft-filter-string-face)))
  (widget-insert "\n\n"))

(eval-when-compile
  (defvar notdeft-mode-map))

(defun notdeft-buffer-setup ()
  "Render the NotDeft file browser in the current buffer."
  (let ((line (max 3 (line-number-at-pos))))
    (setq notdeft-buffer-width (window-width))
    (let ((inhibit-read-only t))
      (erase-buffer))
    (remove-overlays)
    (notdeft-print-header)

    ;; Print the files list
    (if (not (or notdeft-directories notdeft-sparse-directories))
	      "No NotDeft data directories.\n"
      (if notdeft-current-files
	        (mapc #'notdeft-file-widget notdeft-current-files) ;; for side effects
	      (widget-insert
	       (if notdeft-filter-string
	           "No files match the current filter string.\n"
	         "No files found.\n"))))

    (widget-setup)

    (goto-char (point-min))
    (forward-line (1- line))))

(defun notdeft-string-width (str)
  "Like `string-width', but return 0 if STR is nil."
  (if str (string-width str) 0))

(defun notdeft-file-widget (file)
  "Add a line to the file browser for the given FILE."
  (let* ((entry (gethash file notdeft-hash-entries))
	       (title (nth 2 entry))
	       (summary (nth 3 entry))
	       (mtime (when notdeft-time-format
		              (format-time-string notdeft-time-format
				                              (car entry))))
	       ;; Use length instead of string-width for simple strings
	       (mtime-width (if mtime (length mtime) 0))
	       (line-width (- notdeft-buffer-width mtime-width))
	       (path (when notdeft-file-display-function
		             (funcall notdeft-file-display-function file line-width)))
	       (path-width (if path (length path) 0))
	       (up-to-path-width (- line-width path-width))
	       (title-str (or title ""))
	       ;; For potentially multibyte title, truncate smartly
	       (title-width (min up-to-path-width (length title-str)))
	       (truncated-title (if title
			                        (if (<= (length title-str) title-width)
				                          title-str
				                        (substring title-str 0 title-width))
			                      "[Empty file]"))
	       (summary-str (or summary ""))
	       (sep-len (length notdeft-separator))
	       (summary-width (max 0 (- up-to-path-width title-width sep-len)))
	       (truncated-summary (if (<= (length summary-str) summary-width)
				                        summary-str
			                        (substring summary-str 0 summary-width))))
    (widget-create 'link
		               :button-prefix ""
		               :button-suffix ""
		               :button-face 'notdeft-title-face
		               :format "%[%v%]"
		               :tag file
		               :help-echo "Edit this file"
		               :notify (lambda (widget &rest _ignore)
			                       (notdeft-find-file (widget-get widget :tag)))
		               truncated-title)
    (when (> summary-width 0)
      (widget-insert
       (propertize notdeft-separator 'face 'notdeft-separator-face))
      (widget-insert
       (propertize truncated-summary
		               'face 'notdeft-summary-face)))
    (when (or path mtime)
      (while (< (current-column) up-to-path-width)
	      (widget-insert " ")))
    (when path
      (widget-insert (propertize path 'face 'notdeft-time-face)))
    (when mtime
      (widget-insert (propertize mtime 'face 'notdeft-time-face)))
    (widget-insert "\n")))

(defun notdeft-filename-at-point (&optional fail)
  "Return the selected NotDeft note filename.
In a `notdeft-mode' buffer, return the currently selected file's
name. If no note is selected FAIL as specified for
`notdeft-fail'."
  (let ((fail (notdeft-fail fail)))
    (if (not (notdeft-buffer-p))
        (funcall fail (format "Not in a %s buffer" notdeft-buffer))
      (let ((file (widget-get (widget-at) :tag)))
	      (if (not file)
	          (funcall fail "No NotDeft note selected")
	        file)))))

;; State updates

(defun notdeft-pending-lessp (x y)
  "Whether pending status value X < Y."
  (let ((lst '(() redraw refilter requery)))
    (< (cl-position x lst) (cl-position y lst))))

(defun notdeft-set-pending-updates (value)
  "Set `notdeft-pending-updates' to at least VALUE."
  (when (notdeft-pending-lessp notdeft-pending-updates value)
    (setq notdeft-pending-updates value)))

(defun notdeft-visible-buffer ()
  "Return a visible NotDeft buffer, or nil."
  (cl-dolist (buf (buffer-list))
    (when (get-buffer-window buf 'visible)
      (with-current-buffer buf
	      (when (eq major-mode 'notdeft-mode)
	        (cl-return buf))))))

(defmacro notdeft-with-each-buffer (&rest body)
  "Evaluate BODY with each NotDeft buffer set as current."
  (declare (indent defun))
  (let ((x (cl-gensym "buf")))
    `(dolist (,x (buffer-list))
       (with-current-buffer ,x
	       (when (eq major-mode 'notdeft-mode)
	         ,@body)))))

(defun notdeft-set-all-files ()
  "Recompute `notdeft-all-files' for the current buffer.
Set its value for the buffer. Do that without any hints about
what has changed. Also update file information cache to ensure it
has information for those files. Do nothing else."
  (let ((files (notdeft-xapian-search-all-dirs notdeft-xapian-query)))
    (notdeft-cache-update files)
    (setq notdeft-all-files files)))

(defmacro notdeft-assert-major-mode ()
  "Assert that `major-mode' is the symbol `notdeft-mode'.
The check may get optimized away by the byte-compiler."
  '(cl-assert (eq major-mode 'notdeft-mode) t))

(defun notdeft-changed--query ()
  "Refresh NotDeft buffer after query change."
  (notdeft-assert-major-mode)
  (notdeft-set-pending-updates 'requery)
  (notdeft-do-pending))

(defun notdeft-changed--filter ()
  "Refresh NotDeft buffer after filter change."
  (notdeft-assert-major-mode)
  (notdeft-set-pending-updates 'refilter)
  (notdeft-do-pending))

(defun notdeft-changed--window ()
  "A `window-configuration-change-hook' for NotDeft.
Called with the change event concerning the `selected-window',
whose current buffer should be a NotDeft buffer, as the hook
is installed locally for NotDeft buffers only."
  (notdeft-assert-major-mode)
  (unless (equal notdeft-buffer-width (window-width))
    (unless notdeft-pending-updates
      (notdeft-set-pending-updates 'redraw)))
  (notdeft-do-pending))

(defun notdeft-do-pending ()
  "Perform any operations pending for a NotDeft buffer.
Postpone operations until such time that the buffer is visible.
Update `notdeft-pending-updates' to indicate the operations (if
any) that still remain pending after any performed operations."
  (notdeft-assert-major-mode)
  (when notdeft-pending-updates
    (when (get-buffer-window nil 'visible)
      (when (eq notdeft-pending-updates 'requery)
	      (notdeft-set-all-files)
	      (setq notdeft-pending-updates 'refilter))
      (when (eq notdeft-pending-updates 'refilter)
	      (notdeft-filter-update)
	      (setq notdeft-pending-updates 'redraw))
      (when (eq notdeft-pending-updates 'redraw)
	      (notdeft-buffer-setup))
      (setq notdeft-pending-updates nil))))

(defun notdeft-reset (&optional all-buffers)
  "Reset NotDeft state without making change notifications.
Clear some of the state. The cleared state includes the file
information cache, the pending state of all buffers, and the
search query and filter string for any current NotDeft buffer, or
optionally for ALL-BUFFERS."
  (notdeft-cache-clear)
  (if all-buffers
      (notdeft-with-each-buffer
	      (setq notdeft-xapian-query nil)
	      (setq notdeft-filter-string nil)
	      (setq notdeft-pending-updates 'requery))
    (when (notdeft-buffer-p)
      (setq notdeft-xapian-query nil)
      (setq notdeft-filter-string nil))
    (notdeft-with-each-buffer
      (setq notdeft-pending-updates 'requery))))

(defun notdeft-mode-reset-hook ()
  "Handle explicit reset requests for `notdeft-mode'.
If a state reset is to be done, clear queries and filters. Ask
interactively if it should be done for all buffers, or just any
current `notdeft-mode' buffer."
  (notdeft-reset (y-or-n-p (concat "Reset all " notdeft-buffer " buffers? "))))

(add-hook 'notdeft-reset-hook 'notdeft-mode-reset-hook)

(defun notdeft-mode-after-index-change-hook ()
  "Invalidate NotDeft buffers after a search index change."
  (notdeft-with-each-buffer
    (setq notdeft-pending-updates 'requery)))

(add-hook 'notdeft-after-index-change-hook 'notdeft-mode-after-index-change-hook)

(defun notdeft-mode-after-fs-change-hook ()
  "Execute pending operations on NotDeft buffers."
  (notdeft-buffers-mapc #'notdeft-do-pending)
  (when (and notdeft-xapian-program notdeft-cache-compaction-factor)
    (let ((count (hash-table-count notdeft-hash-entries)))
      (when (> count (* notdeft-cache-compaction-factor
			                  notdeft-xapian-max-results))
	      (let ((remain (hash-table-count (notdeft-cache-compact))))
	        (message "Cache compacted: size %d -> %d" count remain))))))

(add-hook 'notdeft-after-fs-change-hook 'notdeft-mode-after-fs-change-hook)

(defun notdeft-query-edit ()
  "Enter a Xapian query string, and make it current."
  (interactive nil notdeft-mode)
  (when notdeft-xapian-program
    (notdeft-xapian-query-set (notdeft-xapian-read-query))))

(defun notdeft-query-clear ()
  "Clear current Xapian query string."
  (interactive nil notdeft-mode)
  (when notdeft-xapian-program
    (notdeft-xapian-query-set nil)))

(defun notdeft-xapian-query-set (new-query)
  "Set NEW-QUERY string as the current Xapian query.
Refresh `notdeft-all-files' and other state accordingly, as
`notdeft-changed--query' does it. Additionally, display a message
summarizing some statistics about the results shown."
  (setq notdeft-xapian-query new-query)
  (notdeft-changed--query)
  (let* ((n (length notdeft-all-files))
	       (is-none (= n 0))
	       (is-max (and (> notdeft-xapian-max-results 0)
		                  (= n notdeft-xapian-max-results)))
	       (found (cond
		             (is-max (format "Found maximum of %d notes" n))
		             (is-none "Found no notes")
		             (t (format "Found %d notes" n))))
	       (shown (cond
		             (is-none "")
		             (notdeft-filter-string
		              (format ", showing %d of them"
			                    (length notdeft-current-files)))
		             (t ", showing all of them"))))
    (message (concat found shown))))

;;;###autoload
(defun notdeft-switch-to-buffer ()
  "Switch to an existing NotDeft buffer.
Where multiple buffers exist, query for the desired buffer
interactively."
  (interactive)
  (let ((buffers (notdeft-buffer-list)))
    (cond
     ((not buffers)
      (message "No NotDeft buffers"))
     ((null (cdr buffers))
      (let ((buf (car buffers)))
	      (if (eq (current-buffer) buf)
	          (message "No other NotDeft buffers")
	        (switch-to-buffer buf))))
     (t
      (let* ((choices
	            (mapcar
	             (lambda (buf)
		             (let (query filter)
		               (with-current-buffer buf
		                 (setq query notdeft-xapian-query
			                     filter notdeft-filter-string))
		               (format "%s: %s: %s"
			                     (buffer-name buf)
			                     (or query "-")
			                     (or filter "-"))))
	             buffers))
	           (chosen (ido-completing-read "Buffer: " choices nil t))
	           (ix (cl-position chosen choices))
	           (buffer (nth ix buffers)))
	      (switch-to-buffer buffer))))))

(defun notdeft-buffer-list ()
  "Return a list of NotDeft buffers.
That is, behave like `buffer-list', but exclude all non-NotDeft
buffers."
  (cl-loop for buf in (buffer-list)
	         if (notdeft-buffer-p buf)
	         collect buf))

;; File list filtering

(defun notdeft-file-newer-p (file1 file2)
  "Whether FILE1 is more recently modified than FILE2."
  (let ((time1 (notdeft-file-mtime file1))
	      (time2 (notdeft-file-mtime file2)))
    (time-less-p time2 time1)))

(defun notdeft-sort-files (files)
  "Sort FILES in reverse order by modification time.
The argument list is modified."
  (sort files (lambda (f1 f2) (notdeft-file-newer-p f1 f2))))

(defun notdeft-filter-update ()
  "Update the filtered files list using the current filter string.
Refer to `notdeft-filter-string' for the string.
Modify the variable `notdeft-current-files' to set the result."
  (if (not notdeft-filter-string)
      (setq notdeft-current-files notdeft-all-files)
    (setq notdeft-current-files
	        (mapcar #'notdeft-filter-match-file notdeft-all-files))
    (setq notdeft-current-files (delq nil notdeft-current-files))))

(defun notdeft-filter-match-file (file)
  "Return FILE if it is a match against the current filter string.
Treat `notdeft-filter-string' as a list of whitespace-separated
strings and require all elements to match."
  (let ((contents (notdeft-file-contents file))
	      (filter-lst
	       (mapcar #'regexp-quote (split-string notdeft-filter-string)))
	      (case-fold-search t))
    (when (cl-every (lambda (filter)
		                  (string-match-p filter contents))
		                filter-lst)
      file)))

(defun notdeft-grep-for-filter ()
  "Open a Grep view to show filter substrings.
Show each individual match of the `notdeft-filter-string' words,
as they appear in `notdeft-current-files'. Where there is no
filter string, use any `notdeft-xapian-query' instead, treating
it as a plain string (without query operators). Use
`grep-program' when set, and otherwise \"grep\"."
  (interactive nil notdeft-mode)
  (let ((s (or notdeft-filter-string notdeft-xapian-query)))
    (when s
      (let ((grep-args
	           (mapconcat
	            #'shell-quote-argument
	            `(,(or (bound-and-true-p grep-program) "grep")
		            "--color" "-nH" "-F" "-i"
		            ,(mapconcat #'identity (split-string s) "\n")
		            ,@notdeft-current-files)
	            " ")))
	      (grep grep-args)))))

;; Filters that cause a refresh

(defun notdeft-filter-clear (&optional pfx)
  "Clear the current filter string and refresh the file browser.
With a prefix argument PFX, also clear any Xapian query."
  (interactive "P" notdeft-mode)
  (if (and pfx notdeft-xapian-query)
      (progn
	      (setq notdeft-xapian-query nil)
	      (setq notdeft-filter-string nil)
	      (notdeft-changed--query))
    (notdeft-filter nil)))

(defun notdeft-filter (str)
  "Set the filter string to STR and update the file browser.
If STR is nil, clear the filter."
  (interactive "sFilter: ")
  (let ((old-filter notdeft-filter-string))
    (setq notdeft-filter-string (and (not (equal "" str)) str))
    (unless (equal old-filter notdeft-filter-string)
      (notdeft-changed--filter))))

(defun notdeft-filter-increment ()
  "Append character to the filter string and update state.
In particular, update `notdeft-current-files'. Get the character
from the variable `last-command-event', possibly as modified by
`input-method-function', which could also produce multiple
characters."
  (interactive nil notdeft-mode)
  (let* ((events (if input-method-function
                     (let ((buffer-read-only nil))
                       (funcall input-method-function last-command-event))
                   (list last-command-event)))
         (str (mapconcat
               (lambda (char)
                 (cond
                  ((= char ?\S-\ ) " ")
                  ((characterp char) (char-to-string char))
                  (t "")))
               events
               "")))
    (unless (string= "" str)
      (setq notdeft-filter-string (concat notdeft-filter-string str))
      (notdeft-changed--filter))))

(defun notdeft-filter-decrement ()
  "Remove last character from the filter string and update state.
In particular, update `notdeft-current-files'."
  (interactive nil notdeft-mode)
  (notdeft-filter
   (and (> (length notdeft-filter-string) 1)
	      (substring notdeft-filter-string 0 -1))))

(defun notdeft-filter-decrement-word ()
  "Remove last word from the filter, if possible, and update.
This is like `backward-kill-word' on the filter string, but the
kill ring is not affected."
  (interactive nil notdeft-mode)
  (when notdeft-filter-string
    (let* ((str notdeft-filter-string) ;; store buffer local value
	         (new-filter
	          (with-temp-buffer
	            (insert str)
	            (goto-char (point-max))
	            (backward-word)
	            (buffer-substring-no-properties (point-min) (point)))))
      (notdeft-filter (and (not (equal "" new-filter)) new-filter)))))

(defun notdeft-filter-yank ()
  "Append the most recently killed or yanked text to the filter."
  (interactive nil notdeft-mode)
  (let ((s (current-kill 0 t)))
    (notdeft-filter
     (if notdeft-filter-string
	       (concat notdeft-filter-string s)
       s))))

(defun notdeft-complete ()
  "Complete the current action.
If there is a widget at point, press it. Otherwise, open the
first listed file. If none are listed, but there is an active
filter, quickly create a new file using the
`notdeft-filter-string' as the title. Otherwise, quickly create a
new file."
  (interactive nil notdeft-mode)
  (cond
   ((widget-at)
    (widget-button-press (point)))
   (notdeft-current-files
    (notdeft-find-file (car notdeft-current-files)))
   (t
    (notdeft-sub--new-file nil nil notdeft-filter-string))))

(defun notdeft-show-file-info (file)
  "Show information about the note FILE.
Show filename, title, summary, etc. When called interactively
show use the selected note FILE, if any."
  (interactive
   (list (notdeft-filename-at-point t))
   notdeft-mode)
  (when file
    (let* ((title (notdeft-file-title file))
	         (summary (notdeft-file-summary file)))
      (message "name=%S file=%S title=%S summary=%S"
	             (file-name-nondirectory file)
	             file title
	             (and summary
		                (substring summary 0 (min 50 (length summary))))))))

(defun notdeft-mode-open-file ()
  "Open the selected file, if any."
  (interactive nil notdeft-mode)
  (let ((old-file (notdeft-filename-at-point t)))
    (when old-file
      (notdeft-find-file old-file))))

(defun notdeft-mode-open-file-other-window (&optional switch)
  "Open the selected file in another window.
Optionally SWITCH to the other window, also if called
interactively with a \\[universal-argument] prefix argument."
  (interactive "P" notdeft-mode)
  (let ((file (notdeft-filename-at-point)))
    (when file
      (notdeft-find-file file t switch))))

;;;###autoload
(defun notdeft-mode-open-query (&rest arguments)
  "Open NotDeft with a Xapian search query.
Accept `notdeft-open-query-function' compatible keyword
ARGUMENTS, including any QUERY. Order search results as specified
by any ORDER-BY argument. Optionally open the results in a
NEW-BUFFER instead of replacing the query in an existing
`notdeft-mode' one. When called interactively, prompt the user
for a QUERY, and have the results ordered by time, except when
called with the \\[universal-argument] 1 prefix, in which case
order by relevance. When called interactively open the query in a
new buffer as specified by the `notdeft-open-query-in-new-buffer'
configuration option, but have \\[universal-argument] negate that
setting."
  (interactive
   (let ((prefix current-prefix-arg))
     (list :query (notdeft-xapian-read-query)
	         :order-by (and (not (equal prefix 1)) 'time)
           :new-buffer (funcall (if (equal prefix '(4)) #'not #'identity)
                                notdeft-open-query-in-new-buffer))))
  (when notdeft-xapian-program
    (let* ((query (plist-get arguments :query))
           (order-by (plist-get arguments :order-by))
           (!order (pcase order-by
                     ('time "!time ")
                     ('name "!file ")
                     ('relevance "!rank ")
                     ;; By default we specify nothing so that the `notdeft-xapian' default applies.
                     (_ "")))
           (new-buffer (plist-get arguments :new-buffer))
           (query (concat !order (or query ""))))
      (notdeft nil new-buffer)
      (notdeft-xapian-query-set query))))

(defun notdeft-quit (&optional kill all)
  "Quit NotDeft mode.
Optionally KILL the buffer instead of burying it. Optionally kill
ALL NotDeft mode buffers. When called interactively, KILL if
called with prefix argument(s), and kill ALL if called with two
\\[universal-argument] prefixes."
  (interactive
   (list current-prefix-arg
         (equal current-prefix-arg '(16))))
  (when (notdeft-buffer-p)
    (quit-window kill))
  (when all
    (dolist (buf (notdeft-buffer-list))
      (kill-buffer buf))))

;;; `notdeft-mode' major mode definition

(defvar notdeft-mode-map
  (let ((i 0)
        (map (make-keymap)))
    ;; Make multibyte characters extend the filter string.
    (set-char-table-range (nth 1 map) (cons #x100 (max-char))
                          #'notdeft-filter-increment)
    ;; Extend the filter string by default.
    (setq i ?\s)
    (while (< i 256)
      (define-key map (vector i) #'notdeft-filter-increment)
      (setq i (1+ i)))
    ;; Filtering
    (define-key map (kbd "DEL") #'notdeft-filter-decrement)
    (define-key map (kbd "M-DEL") #'notdeft-filter-decrement-word)
    (define-key map (kbd "C-c C-l") #'notdeft-filter)
    (define-key map (kbd "C-c C-c") #'notdeft-filter-clear)
    (define-key map (kbd "C-y") #'notdeft-filter-yank)
    (define-key map (kbd "<C-S-backspace>") #'notdeft-filter-clear)
    ;; File opening
    (define-key map (kbd "RET") #'notdeft-complete)
    (define-key map (kbd "C-c o") #'notdeft-mode-open-file-other-window)
    ;; File creation
    (define-key map (kbd "C-c C-n") #'notdeft-new-file)
    (define-key map (kbd "C-c C-m") #'notdeft-new-file-named)
    ;; File management
    (define-key map (kbd "C-c C-d") #'notdeft-delete-file)
    (define-key map (kbd "C-c C-r") #'notdeft-rename-file)
    (define-key map (kbd "C-c m") #'notdeft-move-file)
    (define-key map (kbd "C-c C-a") #'notdeft-archive-file)
    (define-key map (kbd "C-c x e") #'notdeft-change-file-extension)
    (define-key map (kbd "C-c x s") #'notdeft-move-into-subdir)
    ;; File information
    (define-key map (kbd "C-c i") #'notdeft-show-file-directory)
    (define-key map (kbd "C-c I") #'notdeft-show-file-info)
    (define-key map (kbd "C-c P") #'notdeft-show-file-parse)
    ;; Miscellaneous
    (define-key map (kbd "C-c g") #'notdeft-grep-for-filter)
    (define-key map (kbd "C-c e") #'notdeft-refresh)
    (define-key map (kbd "C-c C-q") #'notdeft-quit)
    ;; Xapian
    (define-key map (kbd "C-c C-o") #'notdeft-query-edit)
    (define-key map (kbd "<tab>") #'notdeft-query-edit)
    (define-key map (kbd "<backtab>") #'notdeft-query-clear)
    (define-key map (kbd "<S-tab>") #'notdeft-query-clear)
    map)
  "Keymap for NotDeft mode.

\\{notdeft-mode-map}")

(defun notdeft-mode ()
  "Major mode for quickly listing and managing plain text notes.
Turn the current buffer into a `notdeft-mode' buffer, and run the
hook `notdeft-mode-hook'.

\\{notdeft-mode-map}"
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (use-local-map notdeft-mode-map)
  (setq major-mode 'notdeft-mode)
  (setq mode-name "NotDeft")
  (make-local-variable 'notdeft-directory)
  (make-local-variable 'notdeft-all-files)
  (make-local-variable 'notdeft-current-files)
  (make-local-variable 'notdeft-xapian-query)
  (make-local-variable 'notdeft-filter-string)
  (make-local-variable 'notdeft-pending-updates)
  (make-local-variable 'notdeft-buffer-width)
  (make-local-variable 'notdeft-previous-target)
  (add-hook 'window-configuration-change-hook ;; buffer locally
	          #'notdeft-changed--window nil t)
  (run-mode-hooks 'notdeft-mode-hook))

(put 'notdeft-mode 'mode-class 'special)

(defun notdeft-create-buffer (&optional new)
  "Create and switch to a `notdeft-mode' buffer.
Name it `notdeft-buffer'. If a NotDeft buffer by that name
already exists, reuse it, resetting its state. If NEW is non-nil,
then always create a new buffer."
  (switch-to-buffer (if new
			                  (generate-new-buffer notdeft-buffer)
		                  notdeft-buffer))
  (notdeft-mode))

;;;###autoload
(defun notdeft (&optional reset new)
  "Switch to a `notdeft-buffer', creating one if not yet created.
With a non-nil argument RESET, switch to any existing NotDeft
buffer with fresh state. With a non-nil argument NEW, always
create a new buffer, even when a `notdeft-buffer' already exists.
When called interactively, one prefix argument means NEW, whereas
two prefix arguments means RESET."
  (interactive (list (equal current-prefix-arg '(16))
		                 (equal current-prefix-arg '(4))))
  (let ((buf (and (not new) (get-buffer notdeft-buffer))))
    (if buf
	      (progn
	        (switch-to-buffer buf)
	        (when reset
	          (notdeft-reset)))
      (notdeft-create-buffer t))
    (notdeft-global-do-pending)
    (when (and notdeft-directory (or (not buf) reset))
      (message "Using NotDeft data directory %S" notdeft-directory))))

;;; Deft

(eval-when-compile
  (defvar deft-directory))
(declare-function deft-refresh "deft")

(defun notdeft-open-in-deft ()
  "Open the selected note's Deft directory in Deft.
Do that only when the command `deft' is available. This
implementation makes assumptions about Deft."
  (interactive nil notdeft-mode)
  (when (fboundp 'deft)
    (let ((old-file (notdeft-filename-at-point t)))
      (when old-file
	      (let ((old-dir (notdeft-dcache--strict-managed-file-root
			                  (expand-file-name old-file)
			                  (notdeft-dcache))))
	        (if (not old-dir)
	            (message "Not a NotDeft file: %S" old-file)
	          (let ((re-init
		               (and (boundp 'deft-buffer)
			                  (get-buffer deft-buffer)
			                  (not (equal deft-directory old-dir)))))
	            (setq deft-directory old-dir)
	            (deft)
	            (when re-init
		            (deft-refresh)))))))))

(provide 'notdeft-mode)

;;; notdeft-mode.el ends here

;; NotDeft, a note manager for Emacs
;; Copyright (C) 2011-2025  Tero Hasu
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
