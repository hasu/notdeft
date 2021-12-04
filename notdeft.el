;;; notdeft.el --- Note manager and search engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2011 Jason R. Blevins <jrblevin@sdf.org>
;; Copyright (C) 2011-2020 Tero Hasu <tero@hasu.is>
;; All rights reserved.

;; Author: Tero Hasu <tero@hasu.is>
;;	Jason R. Blevins <jrblevin@sdf.org>
;; Maintainer: Tero Hasu <tero@hasu.is>
;; Homepage: https://tero.hasu.is/notdeft/
;; Keywords: files text notes search
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

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

;;; Commentary:

;; NotDeft is an Emacs mode for quickly browsing, filtering, and
;; editing directories of plain text notes. It was designed for
;; increased productivity when writing and taking notes by making it
;; fast to find the right file at the right time.

;; NotDeft is open source software and may be freely distributed and
;; modified under the BSD license.  This version is a fork of
;; Deft version 0.3, which was released on September 11, 2011.

;; File Browser

;; The NotDeft buffer is simply a local search engine result browser
;; which lists the titles of all text files matching a search query
;; (entered by first pressing TAB or `C-c C-o`), followed by short
;; summaries and last modified times. The title is taken to be the
;; first line of the file (or as specified by an Org "TITLE" file
;; property) and the summary is extracted from the text that follows.
;; By default, files are sorted in terms of the last modified date,
;; from newest to oldest.

;; All NotDeft files or notes are simple plain text files (e.g., Org
;; markup files). As an example, the following directory structure
;; generated the screenshot above.
;;
;;     % ls ~/.deft
;;     about.org    browser.org     directory.org   operations.org
;;     ack.org      completion.org  extensions.org  text-mode.org
;;     binding.org  creation.org    filtering.org
;;
;;     % cat ~/.deft/about.org
;;     About
;;
;;     An Emacs mode for slicing and dicing plain text files.

;; Searching and Filtering

;; NotDeft's primary operations are searching and filtering. The list
;; of files matching a search query can be further narrowed down using
;; a filter string, which will match both the title and the body text.
;; To initiate a filter, simply start typing. Filtering happens on the
;; fly. As you type, the file browser is updated to include only files
;; that match the current string.

;; To open the first matching file, simply press `RET`.  If no files
;; match your filter string, pressing `RET` will create a new file
;; using the string as the title.  This is a very fast way to start
;; writing new notes.  The filename will be generated automatically.

;; To open files other than the first match, navigate up and down
;; using `C-p` and `C-n` and press `RET` on the file you want to open.

;; Press `C-c C-c` to clear the filter string and display all files
;; and `C-c C-x g` to refresh the file browser using the current
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

;; Instead of the above mode of operation, it is also possible to use
;; NotDeft's search functionality without a NotDeft buffer, by
;; invoking NotDeft's variants of the `find-file' command from any
;; major mode. The `notdeft-lucky-find-file' opens the "best" search
;; query match directly, whereas `notdeft-query-select-find-file'
;; presents the matches for selection in the minibuffer.

;; Getting Started

;; To start using NotDeft, place it somewhere in your Emacs
;; `load-path' and add the line
;;
;;     (require 'notdeft-autoloads)
;;
;; in your `.emacs` file. Then run `M-x notdeft` to start.
;; Alternatively, you may find it convenient to execute `M-x
;; notdeft-open-query` to enter a search query from anywhere, which
;; then also opens a `notdeft-mode' buffer for displaying the results.

;; To actually use NotDeft's search engine to get search results, you
;; must first compile the `notdeft-xapian` program, which is
;; responsible for accessing the search index(es). The variable
;; `notdeft-xapian-program' must specify the location of the compiled
;; executable in order for NotDeft to use it.

;; You should preferably also have the `notdeft-note-mode' minor mode
;; enabled for all of your note file buffers, in order to get NotDeft
;; to automatically update the search index according to changes made,
;; no matter how the buffers were opened. The minor mode is best
;; enabled for the relevant file formats and directories only, which
;; can be arranged by enabling it only when a certain directory-local
;; variable has been set to indicate note-containing directories. For
;; example, the `add-dir-local-variable' command can be used to set
;; such variables for the relevant modes and directories, and the
;; minor mode can then be enabled based on their values:
;;
;;     (defvar-local notdeft-note-mode-auto-enable nil)
;;
;;     (add-hook
;;      'hack-local-variables-hook
;;      (lambda ()
;;        (when notdeft-note-mode-auto-enable
;;          (notdeft-note-mode 1))))

;; One useful way to use NotDeft is to keep a directory of notes in a
;; synchronized folder.  This can be used with other applications and
;; mobile devices, for example, Notational Velocity or Simplenote
;; on OS X, Elements on iOS, or Epistle on Android.

;; Customization

;; Customize the `notdeft` group to change the functionality.
;;
;;     (customize-group "notdeft")

;; By default, NotDeft looks for notes by searching for files with the
;; extension `.org` in the `~/.deft` directory.  You can customize
;; both the file extension and the NotDeft note search path by running
;; `M-x customize-group` and typing `notdeft`.  Alternatively, you can
;; configure them in your `.emacs` file:
;;
;;     (setq notdeft-directories '("~/.deft/" "~/Dropbox/notes/"))
;;     (setq notdeft-extension "txt")
;;     (setq notdeft-secondary-extensions '("md" "scrbl"))
;;
;; The variable `notdeft-extension' specifies the default extension
;; for new notes. There can be `notdeft-secondary-extensions' for
;; files that are also considered to be NotDeft notes.

;; While you can choose a `notdeft-extension' that is not ".org",
;; NotDeft is somewhat optimized to working with files in Org format.
;; Refer to the `notdeft-org` feature for NotDeft's Org-specific
;; commands.

;; To enable the `notdeft-xapian` program to be compiled from within
;; Emacs, you may specify a suitable shell command by setting the
;; variable `notdeft-xapian-program-compile-command-format'. After
;; that you can use the command `notdeft-xapian-compile-program' to
;; build the program. It even possible to instruct the compilation to
;; happen transparently, by having your configuration include
;;
;;    (add-hook 'notdeft-load-hook
;;              'notdeft-xapian-make-program-when-uncurrent)

;; It can be useful to create a global keybinding for the `notdeft'
;; function (e.g., a function key) to start it quickly. You can easily
;; set up such a binding. For example, to bind `notdeft' to F8, add
;; the following code to your `.emacs` file:
;;
;;     (global-set-key [f8] 'notdeft)

;; NotDeft also comes with a predefined `notdeft-global-map' keymap of
;; commands, and that keymap can also be given a global keybinding to
;; make its commands accessible quickly. Both `notdeft' and
;; `notdeft-open-query' are included in the keymap, among other
;; commands that may be useful outside a NotDeft buffer.

;; The faces used for highlighting various parts of the screen can
;; also be customized.  By default, these faces inherit their
;; properties from the standard font-lock faces defined by your current
;; color theme.

;;; History:

;; NotDeft:

;; * Most notably, add a Xapian-based query engine.
;; * Add support for multiple notes directories.

;; Deft version 0.3 (2011-09-11):

;; * Internationalization: support filtering with multibyte characters.

;; Deft version 0.2 (2011-08-22):

;; * Match filenames when filtering.
;; * Automatically save opened files (optional).
;; * Address some byte-compilation warnings.

;; Deft was originally written by Jason Blevins.
;; The initial version, 0.1, was released on August 6, 2011.

;;; Code:

(require 'cl-lib)
(require 'widget)
(require 'wid-edit)

;; Customization

;;;###autoload
(defgroup notdeft nil
  "Emacs NotDeft mode."
  :group 'local)

(defcustom notdeft-directories '("~/.deft/")
  "NotDeft directories.
Each element must be a directory path string.
Each named directory may or may not exist."
  :type '(repeat string)
  :safe (lambda (lst) (cl-every 'stringp lst))
  :group 'notdeft)

(defcustom notdeft-directory nil
  "Default or previously selected NotDeft data directory.
One of the `notdeft-directories', or nil if none. The value may
be modified locally for each NotDeft mode buffer. The global
default is customizable."
  :type '(choice (string :tag "Default directory")
		 (const :tag "None" nil))
  :safe #'string-or-null-p
  :group 'notdeft)

(defcustom notdeft-extension "org"
  "Default NotDeft file extension."
  :type 'string
  :safe #'stringp
  :group 'notdeft)

(defcustom notdeft-secondary-extensions nil
  "Additional NotDeft file extensions."
  :type '(repeat string)
  :safe (lambda (lst) (cl-every 'stringp lst))
  :group 'notdeft)

(defcustom notdeft-sparse-directories nil
  "Directories indexed only for specified files.
Complements `notdeft-directories', with the difference that
sparse directory contents are not managed, other than being
searchable and tracked. The elements of the directory list are of
the form (DIR . (FILE ...)) where each FILE is a path string
relative to DIR."
  :type '(repeat (cons file (repeat string)))
  :group 'notdeft)

(defcustom notdeft-notename-function 'notdeft-default-title-to-notename
  "Function for deriving a note name from a title.
Returns nil if no name can be derived from the argument."
  :type 'function
  :group 'notdeft)

(defcustom notdeft-select-note-file-by-search nil
  "Whether to do a search when selecting a note file.
Ignored if `notdeft-select-note-file-function' is non-nil, in
which case that function defines how selection is done."
  :type 'boolean
  :safe #'booleanp
  :group 'notdeft)

(defcustom notdeft-archive-directory "_archive"
  "Sub-directory name for archived notes.
Should begin with '.', '_', or '#' to be excluded from
indexing for Xapian searches."
  :type 'string
  :safe #'stringp
  :group 'notdeft)

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

(defcustom notdeft-allow-org-property-drawers t
  "Whether to recognize Org property drawers.
If non-nil, then buffer-level Org \"PROPERTIES\" drawers are
treated as being part of the header of the note, which in
practice means that they are treated the same as comments."
  :type 'boolean
  :safe #'booleanp
  :group 'notdeft)

(defcustom notdeft-open-query-in-new-buffer nil
  "Whether to open query results in a new buffer.
More specifically, when this variable is non-nil, the
`notdeft-open-query' command shows its matches in a freshly
created NotDeft buffer."
  :type 'boolean
  :safe #'booleanp
  :group 'notdeft)

(defcustom notdeft-cache-compaction-factor 20
  "Indicates file cache compaction frequency.
If nil, then no compaction takes place. If it is 0, then
compaction happens after every query. Otherwise the value should
be an integer specifying a limit for the cache size as a factor
of the maximum result set size. This value is ignored if the
Xapian backend is not in use, as in that case filtering requires
information about all files at all times."
  :type '(choice (integer :tag "Times maximum")
		 (const :tag "Unlimited" nil))
  :safe (lambda (v) (or (not v) (numberp v)))
  :group 'notdeft)

;; Faces

(defgroup notdeft-faces nil
  "Faces used in NotDeft mode"
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

;; Internal requires

(require 'notdeft-global)
(require 'notdeft-xapian)

;; Constants

(defconst notdeft-buffer "*NotDeft*"
  "NotDeft buffer name.")

(defconst notdeft-separator " --- "
  "Text used to separate file titles and summaries.")

;; Global variables

(defvar notdeft-new-file-data-function 'notdeft-new-file-data
  "Function for computing a new note's name and content.
Will be called for all new notes, but not ones that are renamed
or moved. Must return the note data as a (file-name .
content-data) pair, where the data part can be nil for an empty
note. The function must accept the parameters (DIR NOTENAME EXT
DATA TITLE). DIR is a non-nil directory path for the name.
NOTENAME may be nil if one has not been given for the new note.
EXT is a non-nil file name extension for the note. Note content
text DATA may be given for the new note, possibly for further
manipulation, but will be nil for an empty note. TITLE may be nil
if one has not been provided. Uniqueness of the constructed file
name should be ensured if desired, as otherwise note creation
will fail due to a naming conflict. See `notdeft-new-file-data'
for an example implementation.")

(defvar notdeft-completing-read-history nil
  "History of selected NotDeft note files.
May be used by `notdeft-completing-read-function' as the history
variable.")

(defvar notdeft-completing-read-function
  'notdeft-ido-completing-read
  "Function to use for note file selection.
The function is used in the sense of `completing-read' to pick a
file from a list. The function must take a list of file paths,
and an optional prompt. See `notdeft-ido-completing-read' for an
example implementation.")

(defvar notdeft-select-note-file-query nil
  "A note file selection option.
Some implementations of `notdeft-select-note-file-function' may
check the value of this variable. If it is non-nil it should be a
search query string.")

(defvar notdeft-select-note-file-all nil
  "Whether to select a note from all matches.
Setting this variable to a non-nil indicates that
`notdeft-select-note-file-function' should preferably offer all
the relevant notes for selection rather than just the most highly
ranked ones.")

(defvar notdeft-select-note-file-function nil
  "Function for selecting a note file.
Used generally when another operation needs a note file to be
selected, probably interactively. The function is called without
arguments. For example implementations, see
`notdeft-ido-select-note-file' and
`notdeft-search-select-note-file'.")

(defvar notdeft-load-hook nil
  "Hook run immediately after `notdeft' feature load.")

(defvar notdeft-mode-hook nil
  "Hook run when entering NotDeft mode.")

(defvar notdeft-pre-refresh-hook nil
  "Hook run before each `notdeft-refresh'.")

(defvar notdeft-post-refresh-hook nil
  "Hook run after each `notdeft-refresh'.")

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

(defvar notdeft-dirlist-cache (make-hash-table :test 'equal)
  "A cache of lists of notes in NotDeft directories.
NotDeft directory names as keys, in their
`notdeft-canonicalize-root' form. Lists of full note file names
as values. Only used with the dirlist backend, in which case this
data structure gets built instead of a search index.")

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

(defvar notdeft-pending-reindex t
  "Whether to do initial, one-off search indexing.
This is a global flag referenced by `notdeft-global-do-pending'.
For the search index to stay current for subsequent queries, use
only NotDeft mode, NotDeft note mode, and NotDeft commands for
making changes to a note collection.")

(defvar notdeft-pending-updates 'requery
  "Whether there are pending updates for a NotDeft buffer.
Either nil for no pending updates, the symbol `redraw' for a
pending redrawing of the buffer, the symbol `refilter' for a
pending recomputation of `notdeft-current-files', or the symbol
`requery' for a pending querying of `notdeft-all-files'. Local to
a NotDeft mode buffer.")

;;; NotDeft directory information cache

(defvar notdeft-dcache--cache nil
  "A cache of directory information.
When set, contains a vector of form [MDIRS SDIRS ADIRS
SDIRS-FILES DIR-MAP], where all pathnames are canonicalized and
absolute, and where directory names are such also syntactically.
SDIRS-FILES is of the form ((SDIR . FILES) ...).")

(defun notdeft-canonicalize-root (path)
  "Canonicalize NotDeft directory PATH.
Converts the NotDeft directore PATH into the internal
representation used in `notdeft-dcache--cache'."
  (file-name-as-directory (expand-file-name path)))

(defun notdeft-dcache (&optional refresh)
  "Get the value of the variable `notdeft-dcache--cache'.
Compute it if not yet done, or if REFRESH is true."
  (when (or (not notdeft-dcache--cache) refresh)
    (let* ((mdirs (mapcar #'notdeft-canonicalize-root notdeft-directories))
	   (sfiles (mapcar (lambda (x)
			     (let* ((sdir (notdeft-canonicalize-root (car x)))
				    (files (mapcar
					    (lambda (file)
					      (expand-file-name file sdir))
					    (cdr x))))
			       (cons sdir files)))
			   notdeft-sparse-directories))
	   (sdirs (mapcar #'car sfiles))
	   (adirs (append mdirs sdirs))
	   (dirmap (append
		    (mapcar (lambda (dir)
			      (cons (notdeft-canonicalize-root dir) dir))
			    notdeft-directories)
		    (mapcar (lambda (dir)
			      (let ((dir (car dir)))
				(cons (notdeft-canonicalize-root dir) dir)))
			    notdeft-sparse-directories))))
      (setq notdeft-dcache--cache (vector mdirs sdirs adirs sfiles dirmap))))
  notdeft-dcache--cache)

(defun notdeft-dcache--root-to-original (root cache)
  "Translate NotDeft ROOT to configured form.
Use information in CACHE to do that. That is, given a NotDeft
directory path in any form, return the form that it has in either
`notdeft-directories' or `notdeft-sparse-directories', or nil if
it does not."
  (cdr (assoc (notdeft-canonicalize-root root)
	      (aref cache 4))))

(defun notdeft-dcache--roots (cache)
  "Return all NotDeft roots in the CACHE.
The result includes both managed and sparse directory paths in
their canonical form."
  (aref cache 2))

(defun notdeft-dcache--filter-roots (dirs cache)
  "Filter NotDeft roots in DIRS.
Use information in CACHE. That is, functionally drop all DIRS
that are not NotDeft root directories. Return a filtered list
of directory paths in canonical form."
  (let ((roots (aref cache 2)))
    (delete nil
	    (mapcar (lambda (dir)
		      (let ((dir (notdeft-canonicalize-root dir)))
			(when (member dir roots)
			  dir)))
		    dirs))))

(defun notdeft-dcache--expand-sparse-root (dir cache)
  "Expand NotDeft root path DIR.
Use information in CACHE. Expand the DIR path into a
specification for the sparse directory. Return nil if it is not a
sparse root."
  (assoc (notdeft-canonicalize-root dir)
	 (aref cache 3)))

(defun notdeft-dcache--sparse-file-root (file cache)
  "Resolve sparse FILE root directory.
More specifically, if FILE is a sparse NotDeft directory note
file, return its NotDeft directory in an absolute and canonical
form. Otherwise return nil. Assume FILE to be in an absolute,
canonical form. Use CACHE information for resolution."
  (let ((sdirs-files (aref cache 3)))
    (cl-some
     (lambda (sdir-files)
       (when (member file (cdr sdir-files))
	 (car sdir-files)))
     sdirs-files)))

(defun notdeft-dcache--managed-file-root (file cache)
  "Resolve managed FILE root directory.
Do this syntactically, using information in CACHE. More
specifically, if FILE names a managed NotDeft directory note
file, return its NotDeft directory in an absolute and canonical
form. Otherwise return nil. FILE must be in an absolute,
canonical form. The FILE name extension is not checked against
`notdeft-extension' and `notdeft-secondary-extensions', which may
be done separately on the argument if required. Also, it is not
checked that FILE is strictly under the returned root, rather
than the root itself, and that may also be done separately."
  (let ((mdirs (aref cache 0)))
    (cl-some
     (lambda (dir)
       (when (string-prefix-p dir file)
	 dir))
     mdirs)))

(defun notdeft-dcache--strict-managed-file-root (file cache)
  "Resolve managed FILE root, strictly, syntactically.
Return nil if FILE has no root, or if it itself names the root.
Otherwise return the root. Assume FILE to be in an absolute,
canonical form. Use CACHE information for resolution."
  (let ((root (notdeft-dcache--managed-file-root file cache)))
    (when (and root
	       (not (string= root (file-name-as-directory file))))
      root)))

(defun notdeft-dcache--managed-file-subdir (file cache)
  "Resolve managed FILE subdirectory.
That is, if FILE is syntactically in a subdirectory of a managed
NotDeft root, return the absolute and canonical directory path of
that subdirectory. Otherwise return nil. The result need not be
an immediate subdirectory of a NotDeft root. Assume FILE to be in
an absolute, canonical form. Use CACHE information for
resolution."
  (let ((root (notdeft-dcache--strict-managed-file-root file cache)))
    (when root
      (let ((dir (file-name-as-directory
		  (file-name-directory file))))
	(unless (string= root dir)
	  dir)))))

(defun notdeft-dcache--file-root (file cache)
  "Resolve note FILE root, syntactically.
Return the NotDeft root directory, or nil if FILE is neither
under a managed or sparse NotDeft directory. Assume FILE to be in
an absolute, canonical form. Use CACHE information for
resolution."
  (or (notdeft-dcache--strict-managed-file-root file cache)
      (notdeft-dcache--sparse-file-root file cache)))

(defun notdeft-dcache--sparse-file-by-basename (name cache)
  "Resolve sparse note file by NAME.
Return the file's absolute, canonical pathname. If multiple such
files exist, return one of them. If none exist, return nil. NAME
is assumed to be without leading directory components, but with
any extension. Use CACHE information for resolution."
  (let ((sdirs-files (aref cache 3)))
    (cl-some
     (lambda (sdir-files)
       (let ((files (cdr sdir-files)))
	 (cl-some
	  (lambda (file)
	    (when (string= name (file-name-nondirectory file))
	      file))
	  files)))
     sdirs-files)))

;; File processing

(defun notdeft-title-to-notename (str)
  "Call `notdeft-notename-function' on STR."
  (funcall notdeft-notename-function str))

(defun notdeft-default-title-to-notename (str)
  "Turn a title string STR to a note name string.
Return that string, or nil if no usable name can be derived."
  (save-match-data
    (when (string-match "^[^a-zA-Z0-9-]+" str)
      (setq str (replace-match "" t t str)))
    (when (string-match "[^a-zA-Z0-9-]+$" str)
      (setq str (replace-match "" t t str)))
    (while (string-match "[`'“”\"]" str)
      (setq str (replace-match "" t t str)))
    (while (string-match "[^a-zA-Z0-9-]+" str)
      (setq str (replace-match "-" t t str)))
    (setq str (downcase str))
    (and (not (string= "" str)) str)))

(defun notdeft-format-time-for-filename (tm)
  "Format time TM suitably for filenames."
  (format-time-string "%Y-%m-%d-%H-%M-%S" tm t)) ; UTC

(defun notdeft-generate-notename ()
  "Generate a notename, and return it.
The generated name is not guaranteed to be unique. Format with
the format string \"Deft--%s\", whose placeholder is filled in
with a current time string, as formatted with
`notdeft-format-time-for-filename'. This is the NotDeft detault
naming for notes that are created without a title."
  (let* ((ctime (current-time))
	 (ctime-s (notdeft-format-time-for-filename ctime))
	 (base-filename (format "Deft--%s" ctime-s)))
    base-filename))

(defun notdeft-make-filename (notename &optional ext dir in-subdir)
  "Derive a filename from NotDeft note name NOTENAME.
The filename shall have the extension EXT, defaulting to
`notdeft-extension'. The file shall reside in the directory
DIR (or a default directory computed by `notdeft-get-directory'),
except that IN-SUBDIR indicates that the file should be given its
own subdirectory."
  (let ((root (or dir (notdeft-get-directory))))
    (concat (file-name-as-directory root)
	    (if in-subdir (file-name-as-directory notename) "")
	    notename "." (or ext notdeft-extension))))

(defun notdeft-generate-filename (&optional ext dir)
  "Generate a new unique filename.
Do so without being given any information about note title or
content. Have the file have the extension EXT, and be in
directory DIR \(their defaults are as for
`notdeft-make-filename')."
  (let (filename)
    (while (or (not filename)
	       (file-exists-p filename))
      (let ((base-filename (notdeft-generate-notename)))
	(setq filename (notdeft-make-filename base-filename ext dir))))
    filename))

(defun notdeft-new-file-data (dir notename ext data title)
  "Generate a file name and data for a new note.
Use the directory path DIR, a note basename NOTENAME, and file
name extension EXT to construct a complete file name. Use DATA as
the note content, or just the TITLE if there is no other content.
Use NOTENAME as specified, or derive it from any TITLE with
`notdeft-title-to-notename'. Without either NOTENAME or TITLE,
use the current date and time to derive a name for a note,
attempting to construct a unique name."
  (let* ((notename (or notename
		       (when title
			 (notdeft-title-to-notename title))))
	 (file (if notename
		   (notdeft-make-filename notename ext dir)
		 (notdeft-generate-filename ext dir))))
    (cons file (or data title))))

(defun notdeft-make-file-re ()
  "Return a regexp matching strings with a NotDeft extension."
  (let ((exts (cons notdeft-extension notdeft-secondary-extensions)))
    (concat "\\.\\(?:"
	    (mapconcat #'regexp-quote exts "\\|")
	    "\\)$")))

(defun notdeft-strip-extension (file)
  "Strip any NotDeft filename extension from FILE."
  (replace-regexp-in-string (notdeft-make-file-re) "" file))
  
(defun notdeft-base-filename (file)
  "Strip the leading path and NotDeft extension from filename FILE.
Use `file-name-directory' to get the directory component.
Strip any extension with `notdeft-strip-extension'."
  (let* ((file (file-name-nondirectory file))
	 (file (notdeft-strip-extension file)))
    file))

(defun notdeft-file-equal-p (x y)
  "Whether X and Y are the same file.
Compare based on path names only, without consulting the
filesystem, unlike `file-equal-p'. Disregard directory syntax, so
that \"x\" is equal to \"x/\"."
  (string= (file-name-as-directory (expand-file-name x))
	   (file-name-as-directory (expand-file-name y))))

(defun notdeft-file-in-directory-p (file dir)
  "Whether FILE is in DIR, syntactically.
A directory is considered to be in itself.
Compare based on path names only, without consulting the
filesystem, unlike `file-in-directory-p'."
  (let ((dir (file-name-as-directory (expand-file-name dir)))
	(file (file-name-as-directory (expand-file-name file))))
    (string-prefix-p dir file)))

(defun notdeft-file-strictly-in-directory-p (file dir)
  "Whether FILE is strictly in DIR, syntactically.
Like `notdeft-file-in-directory-p', but a directory is not
considered to be in itself."
  (let ((dir (file-name-as-directory (expand-file-name dir)))
	(file (file-name-as-directory (expand-file-name file))))
    (and (string-prefix-p dir file)
	 (not (string= dir file)))))

(defun notdeft-file-member (file list)
  "Whether FILE is a member of LIST.
Comparisons are syntactic only.
Return the matching member of the list, or nil."
  (cl-some (lambda (elem)
	     (when (notdeft-file-equal-p file elem)
	       elem))
	   list))

(defun notdeft-dir-of-file (file)
  "Return the NotDeft directory for FILE, or nil.
FILE may not itself be one of the NotDeft roots.
Compare syntactically, without consulting the file system."
  (notdeft-dcache--file-root
   (expand-file-name file) (notdeft-dcache)))

(defun notdeft-file-sparse-p (file)
  "Whether FILE is in a sparse NotDeft directory."
  (notdeft-dcache--sparse-file-root
   (expand-file-name file) (notdeft-dcache)))

(defun notdeft-file-in-subdir-p (file)
  "Whether FILE is in a NotDeft sub-directory.
More accurately, whether FILE syntactically names a file or
directory that is not an immediate child of one of the
`notdeft-directories'. FILE need not actually exist for this
predicate to hold, nor does the containing NotDeft directory."
  (notdeft-dcache--managed-file-subdir
   (expand-file-name file) (notdeft-dcache)))

(defun notdeft-file-readable-p (file)
  "Whether FILE is a readable non-directory."
  (and (file-readable-p file)
       (not (file-directory-p file))))

(defun notdeft-read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun notdeft-title-from-file-content (file)
  "Extract a title from FILE content.
Return nil on failure."
  (when (notdeft-file-readable-p file)
    (let* ((contents (notdeft-read-file file))
	   (title (notdeft-parse-title contents)))
      title)))

(defun notdeft-chomp (str)
  "Trim leading and trailing whitespace from STR."
  (replace-regexp-in-string
   "\\(\\`[[:space:]\n\r]+\\|[[:space:]\n\r]+\\'\\)"
   "" str))

;;;###autoload
(defun notdeft-chomp-nullify (str &optional trim)
  "Return string STR if non-empty, otherwise return nil.
Optionally, use function TRIM to trim any result string."
  (when str
    (let ((str (notdeft-chomp str)))
      (unless (string= "" str)
	(if trim (funcall trim str) str)))))

(defvar notdeft-directory-files-regexp "^[^._#/][^/]*$"
  "A match regexp for `directory-files'.
The regular expression to use as the third argument when calling
`directory-files' to look for notes and note subdirectories from
the file system. This should be specified to that it is
consistent with the Xapian program's filtering of readdir
results.")

(defun notdeft-root-find-file (file-p root)
  "Find a file matching predicate FILE-P under ROOT.
FILE-P is called with the file path name \(including the ROOT
component) as its sole argument. ROOT is assumed to be a NotDeft
root, which need not exist. Return nil if no matching file is
found."
  (and
   (file-readable-p root)
   (file-directory-p root)
   (let ((root (file-name-as-directory root))
	 (files (directory-files root nil
				 notdeft-directory-files-regexp t))
	 result)
     (while (and files (not result))
       (let* ((abs-file (concat root (car files))))
	 (setq files (cdr files))
	 (cond
	  ((file-directory-p abs-file)
	   (setq result (notdeft-root-find-file file-p abs-file)))
	  ((funcall file-p abs-file)
	   (setq result abs-file)))))
     result)))

(defun notdeft-file-by-basename (name)
  "Resolve a NotDeft note NAME to a full pathname.
NAME is a non-directory filename, with extension. Resolve it to
the path of a file under `notdeft-directories' or
`notdeft-sparse-directories', if such a note file does exist. If
multiple such files exist, return one of them. If none exist,
return nil."
  (or (notdeft-managed-file-by-basename name)
      (notdeft-dcache--sparse-file-by-basename
       name (notdeft-dcache))))

(defun notdeft-managed-file-by-basename (name)
  "Resolve managed note file by basename NAME."
  (let* ((file-p (lambda (pn)
		   (string= name (file-name-nondirectory pn))))
	 (cand-roots notdeft-directories)
	 result)
    (while (and cand-roots (not result))
      (let ((abs-root (expand-file-name (car cand-roots))))
	(setq cand-roots (cdr cand-roots))
	(setq result (notdeft-root-find-file file-p abs-root))))
    result))

(defun notdeft-glob (root &optional dir result file-re)
  "Return a list of all NotDeft files in a directory tree.
List the NotDeft files under the specified NotDeft ROOT and its
directory DIR, with DIR given as a path relative to the directory
ROOT. If DIR is nil, then list NotDeft files under ROOT. Add to
the RESULT list in undefined order, and return the resulting
value. Only include files whose non-directory names match the
regexp FILE-RE, defaulting to the result of
`notdeft-make-file-re'. If ROOT does not exist, return nil."
  (let* ((root (file-name-as-directory (expand-file-name root)))
	 (dir (file-name-as-directory (or dir ".")))
	 (abs-dir (expand-file-name dir root)))
    (and
     (file-readable-p abs-dir)
     (file-directory-p abs-dir)
     (let* ((files (directory-files abs-dir nil
				    notdeft-directory-files-regexp t))
	    (file-re (or file-re (notdeft-make-file-re))))
       (dolist (file files result)
	 (let* ((rel-file (file-relative-name
			   (expand-file-name file abs-dir)
			   root))
		(abs-file (concat root rel-file)))
	   (cond
	    ((file-directory-p abs-file)
	     (setq result (notdeft-glob root rel-file result file-re)))
	    ((string-match-p file-re file)
	     (setq result (cons rel-file result))))))))))

(defun notdeft-glob--absolute (root &optional dir result file-re)
  "Like `notdeft-glob', but return the results as absolute paths.
The arguments ROOT, DIR, RESULT, and FILE-RE are the same."
  (mapcar
   (lambda (rel)
     (expand-file-name rel root))
   (notdeft-glob root dir result file-re)))

(defun notdeft-find-all-files-in-dir (dir full)
  "Return a list of all NotDeft files under DIR.
The specified directory must be a NotDeft root.
Return an empty list if there is no readable directory.
Return the files' absolute paths if FULL is true."
  (if full
      (notdeft-glob--absolute dir)
    (notdeft-glob dir)))

(defun notdeft-make-basename-list ()
  "Return the names of all NotDeft notes.
Search all existing `notdeft-directories', and include all
existing `notdeft-sparse-directories' files. The result list is
sorted by the `string-lessp' relation, and it may contain
duplicates."
  (let ((fn-lst '()))
    (dolist (dir notdeft-directories)
      (setq fn-lst
	    (append fn-lst
		    (notdeft-find-all-files-in-dir dir t))))
    (dolist (sdir-files notdeft-sparse-directories)
      (let ((dir (car sdir-files))
	    (files (cdr sdir-files)))
	(dolist (file files)
	  (let ((file (expand-file-name file dir)))
	    (when (file-exists-p file)
	      (setq fn-lst (cons file fn-lst)))))))
    ;; `sort` may modify `name-lst`
    (let ((name-lst (mapcar #'file-name-nondirectory fn-lst)))
      (sort name-lst 'string-lessp))))

(defun notdeft-parse-title (contents)
  "Parse the given file CONTENTS and determine the title.
The title is taken to be the first non-empty line of a file.
Org comments are skipped, and \"#+TITLE\" syntax is recognized,
and may also be used to define the title.
Returns nil if there is no non-empty, not-just-whitespace
title in CONTENTS."
  (let* ((res (with-temp-buffer
		(insert contents)
		(notdeft-parse-buffer)))
	 (title (car res)))
    title))

(defun notdeft-condense-whitespace (str)
  "Condense whitespace in STR into a single space."
  (replace-regexp-in-string "[[:space:]\n\r]+" " " str))

(defun notdeft-parse-buffer ()
  "Parse the file contents in the current buffer.
Extract a title and summary.
The summary is a string extracted from the contents following the
title. The result is a list (TITLE SUMMARY KEYWORDS) where any
component may be nil. The result list may include additional,
undefined components."
  (let (title summary keywords dbg (end (point-max)))
    (save-match-data
      (save-excursion
	(goto-char (point-min))
	(while (and (< (point) end) (not (and title summary)))
	  ;;(message "%S" (list (point) title summary))
	  (cond
	   ((looking-at "^\\(?:%\\|@;\\|<!--\\)?#\\+TITLE:[[:blank:]]*\\(.*\\)$") ;; Org title
	    (setq dbg (cons `(TITLE . ,(match-string 1)) dbg))
	    (setq title (match-string 1))
	    (goto-char (match-end 0)))
	   ((looking-at "^\\(?:%\\|@;\\|<!--\\)?#\\+\\(?:KEYWORDS\\|FILETAGS\\):[[:blank:]]*\\(.*\\)$")
	    (setq dbg (cons `(KEYWORDS . ,(match-string 1)) dbg))
	    (setq keywords (match-string 1))
	    (goto-char (match-end 0)))
	   ((looking-at "^\\(?:%\\|@;\\|<!--\\)?#.*$") ;; line comment
	    (setq dbg (cons `(COMMENT . ,(match-string 0)) dbg))
	    (goto-char (match-end 0)))
	   ((and notdeft-allow-org-property-drawers
		 (looking-at "^[ \t]*:PROPERTIES:[ \t]*\\(\n\\|$\\)"))
	    (let ((drawer-beg (point)) done)
	      (goto-char (match-end 0))
	      (while (and (not done) (< (point) end))
		(cond
		 ((looking-at "^[ \t]*:END:.*$") ;; recognize loosely for error recovery
		  (goto-char (match-end 0))
		  (setq done t))
		 ((looking-at "^[ \t]*:\\S-+:.*\\(\n\\|$\\)") ;; property line
		  (goto-char (match-end 0)))
		 (t ;; unclosed drawer
		  (setq done t))))
	      (setq dbg (cons
			 `(DRAWER . ,(buffer-substring
				      drawer-beg (point)))
			 dbg))))
	   ((looking-at "[[:graph:]].*$") ;; non-whitespace
	    (setq dbg (cons `(REST . ,(match-string 0)) dbg))
	    (unless title
	      (setq title (match-string 0))
	      (goto-char (match-end 0)))
	    (setq summary (buffer-substring (point) end))
	    (goto-char end))
	   (t
	    (let* ((b (point)) (e (1+ b)))
	      (setq dbg (cons `(SKIP . ,(buffer-substring b e)) dbg))
	      (goto-char e)))))))
    (list
     (notdeft-chomp-nullify title)
     (notdeft-chomp-nullify summary 'notdeft-condense-whitespace)
     (notdeft-chomp-nullify keywords)
     dbg)))

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

(defun notdeft-file-newer-p (file1 file2)
  "Whether FILE1 is more recently modified than FILE2."
  (let ((time1 (notdeft-file-mtime file1))
	(time2 (notdeft-file-mtime file2)))
    (time-less-p time2 time1)))

;; Cache access

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
	 (line-width (- notdeft-buffer-width (notdeft-string-width mtime)))
	 (path (when notdeft-file-display-function
		 (funcall notdeft-file-display-function file line-width)))
	 (path-width (notdeft-string-width path))
	 (up-to-path-width (- line-width path-width))
	 (title-width (min up-to-path-width
			   (notdeft-string-width title)))
	 (summary-width (min (notdeft-string-width summary)
			     (- up-to-path-width
				title-width
				(length notdeft-separator)))))
    (widget-create 'link
		   :button-prefix ""
		   :button-suffix ""
		   :button-face 'notdeft-title-face
		   :format "%[%v%]"
		   :tag file
		   :help-echo "Edit this file"
		   :notify (lambda (widget &rest _ignore)
			     (notdeft-find-file (widget-get widget :tag)))
		   (if title
		       (truncate-string-to-width title title-width)
		     "[Empty file]"))
    (when (> summary-width 0)
      (widget-insert
       (propertize notdeft-separator 'face 'notdeft-separator-face))
      (widget-insert
       (propertize (truncate-string-to-width summary summary-width)
		   'face 'notdeft-summary-face)))
    (when (or path mtime)
      (while (< (current-column) up-to-path-width)
	(widget-insert " ")))
    (when path
      (widget-insert (propertize path 'face 'notdeft-time-face)))
    (when mtime
      (widget-insert (propertize mtime 'face 'notdeft-time-face)))
    (widget-insert "\n")))

(defun notdeft-dirlist-scan-entries (dirs)
  "Scan DIRS and return entries for dirlist cache."
  (let ((file-re (notdeft-make-file-re))
	(cache (notdeft-dcache)))
     (mapcar
      (lambda (dir)
	(or (notdeft-dcache--expand-sparse-root dir cache)
	    (let ((dir (notdeft-canonicalize-root dir)))
	      (cons dir (notdeft-glob--absolute dir nil nil file-re)))))
      dirs)))

(defmacro notdeft-setq-cons (x v)
  "Prepend into list X the value V."
  (declare (indent 1))
  `(setq ,x (cons ,v ,x)))

(defun notdeft-hash-keys (hash)
  "Return a list of the keys of HASH.
Implemented in terms of `maphash'."
  (let (keys)
    (maphash
     (lambda (k _v)
       (notdeft-setq-cons keys k))
     hash)
    keys))

(defun notdeft-dirlist-gc ()
  "Garbage collect `notdeft-dirlist-cache'.
That is, remove cached information for NotDeft root directories
that are no longer current."
  (let ((keys (notdeft-hash-keys notdeft-dirlist-cache))
	(current-dirs (notdeft-dcache--roots (notdeft-dcache))))
    (dolist (dir keys)
      (unless (member dir current-dirs)
	(remhash dir notdeft-dirlist-cache)))))

(defun notdeft-dirlist-cache-rebuild ()
  "Rebuild the value for `notdeft-dirlist-cache'."
  (clrhash notdeft-dirlist-cache)
  (notdeft-dirlist-cache-update-dirs
   (notdeft-dcache--roots (notdeft-dcache))))

(defun notdeft-dirlist-cache-update-dirs (dirs)
  "Scan the specified NotDeft DIRS.
Update the dirlist cache."
  (cl-assert (and (listp dirs) (cl-every 'stringp dirs)))
  (let ((entries (notdeft-dirlist-scan-entries dirs)))
    (dolist (entry entries)
      (let ((dir (car entry))
	    (files (cdr entry)))
	(cl-assert (cl-every (lambda (file)
			       (notdeft-file-in-directory-p file dir))
			     files))
	(puthash dir files notdeft-dirlist-cache)))))

(defun notdeft-dirlist-cache-size ()
  "Return directory and file count for dirlist cache."
  (let ((d 0) (f 0))
    (maphash
     (lambda (_k v)
       (setq d (+ d 1)
	     f (+ f (length v))))
     notdeft-dirlist-cache)
    (cons d f)))

(defun notdeft-append-copy (&rest sequences)
  "Like `append', but copy all of the SEQUENCES.
That is, do not use the last sequence object as the tail of the
result."
  (apply #'append (append sequences (list nil))))

(defun notdeft-dirlist-get-all-files ()
  "Return a file list collected from `notdeft-dirlist-cache'.
Only include files for current NotDeft directories."
  (let ((lst (mapcar
	      (lambda (dir)
		;; nil if no `dir' key
		(gethash dir notdeft-dirlist-cache))
	      (notdeft-dcache--roots (notdeft-dcache)))))
    (apply #'notdeft-append-copy lst)))

(defmacro notdeft-if2 (cnd thn els)
  "Two-armed `if'.
Equivalent to (if CND THN ELS)."
  (declare (indent defun))
  `(if ,cnd ,thn ,els))

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

(defun notdeft-compute-changes (what things)
  "Compute optimized file system change lists.
Optimize the WHAT and THINGS change specification to some extent,
and return a result of the form (DIRS . FILES), or nil if no
changes remain."
  (let (dirs files) ;; filtered to NotDeft ones
    (cl-case what
      (dirs
       (setq dirs (notdeft-dcache--filter-roots
		   things (notdeft-dcache))))
      (files
       (dolist (file things)
	 (let ((dir (notdeft-dir-of-file file)))
	   (when dir
	     (notdeft-setq-cons files file)
	     (notdeft-setq-cons dirs dir)))))
      (anything
       (setq dirs (notdeft-dcache--roots (notdeft-dcache)))))
    (if (or (and (eq what 'files) (not files))
	    (and (eq what 'dirs) (not dirs)))
	nil
      (cons (cl-remove-duplicates dirs :test 'equal)
	    files))))

(defun notdeft-expand-sparse-dirs (dirs)
  "Expand sparse directory names in DIRS.
That is, replace each of them with a (DIR . FILES) tuple, where
DIR is canonical, and FILES are relative to DIR, or leave
elements intact for DIRS that are not sparse directories."
  (let ((cache (notdeft-dcache)))
    (mapcar (lambda (dir)
	      (let ((entry (notdeft-dcache--expand-sparse-root dir cache)))
		(if (not entry)
		    dir
		  (let ((dir (car entry))
			(files (cdr entry)))
		    (cons dir (mapcar (lambda (file)
					(file-relative-name file dir))
				      files))))))
	    dirs)))

(defun notdeft-changed--fs (what &optional things)
  "Refresh NotDeft file list, cache, and search index state.
The arguments hint at what may need refreshing.

WHAT is a symbolic hint for purposes of optimization.
It is one of:
- symbol `dirs' to assume changes in THINGS NotDeft directories;
- symbol `files' to assume changes in THINGS NotDeft files; or
- symbol `anything' to make no assumptions about filesystem changes.

Ignore THINGS outside NotDeft directory trees.

Refresh both file information cache and any Xapian indexes to
reflect the file system changes. Refresh also
`notdeft-dirlist-cache' where it is being used.

For further work call `notdeft-global-do-pending'."
  (let ((changes (notdeft-compute-changes what things)))
    (when changes
      (let ((dirs (car changes)))
	;;(message "CHANGES: %S" dirs)
	(notdeft-global-do-pending
	 (notdeft-if2 notdeft-xapian-program
	   (lambda () (notdeft-xapian-index-dirs
		       (notdeft-expand-sparse-dirs dirs)))
	   (lambda () (notdeft-dirlist-cache-update-dirs dirs))))))))

(defun notdeft-xapian-index-all-dirs (&optional recreate)
  "Refresh Xapian indexes for all configured directories.
The RECREATE argument is as for `notdeft-xapian-index-dirs'."
  (notdeft-xapian-index-dirs
   (append notdeft-sparse-directories notdeft-directories)
   recreate))

(defun notdeft-xapian-search-all-dirs (query)
  "Execute Xapian QUERY on all configured directories."
  (notdeft-xapian-search
   (notdeft-dcache--roots (notdeft-dcache))
   query))

(defun notdeft-set-all-files ()
  "Recompute `notdeft-all-files' for the current buffer.
Set its value for the buffer. Do that without any hints about
what has changed. Also update file information cache to ensure it
has information for those files. Do nothing else."
  (notdeft-if2 notdeft-xapian-program
    (let ((files (notdeft-xapian-search-all-dirs notdeft-xapian-query)))
      (notdeft-cache-update files)
      (setq notdeft-all-files files))
    (let ((files (notdeft-dirlist-get-all-files)))
      (notdeft-cache-update files)
      (setq notdeft-all-files (notdeft-sort-files files)))))

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
Update `notdeft-pending-updates' to indicate the operations \(if
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

(defun notdeft-global-do-pending (&optional reindex rebuild)
  "Do any pending NotDeft operations.
Unlike `notdeft-do-pending', this function takes care of pending
work globally, for all NotDeft buffers. For cases where there is
no `notdeft-pending-reindex', the caller may specify a REINDEX
function to be used instead for a partial index update. If
REBUILD is non-nil, always rebuild the entire index."
  (when (or reindex rebuild notdeft-pending-reindex)
    (if (or rebuild notdeft-pending-reindex)
	(progn
	  (notdeft-if2 notdeft-xapian-program
	    (notdeft-xapian-index-all-dirs rebuild)
	    (notdeft-dirlist-cache-rebuild))
	  (setq notdeft-pending-reindex nil))
      (funcall reindex))
    (notdeft-with-each-buffer
      (setq notdeft-pending-updates 'requery)))
  (notdeft-buffers-mapc #'notdeft-do-pending)
  (when (and notdeft-xapian-program notdeft-cache-compaction-factor)
    (let ((count (hash-table-count notdeft-hash-entries)))
      (when (> count (* notdeft-cache-compaction-factor
			notdeft-xapian-max-results))
	(let ((remain (hash-table-count (notdeft-cache-compact))))
	  (message "Cache compacted: size %d -> %d" count remain))))))

(defun notdeft-query-edit ()
  "Enter a Xapian query string, and make it current."
  (interactive)
  (when notdeft-xapian-program
    (notdeft-xapian-query-set (notdeft-xapian-read-query))))

(defun notdeft-query-clear ()
  "Clear current Xapian query string."
  (interactive)
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

;; File list file management actions

;;;###autoload
(define-minor-mode notdeft-note-mode
  "Manage NotDeft state for a note buffer.
A minor mode that should be enabled for NotDeft notes. Does
nothing but manage calls to `notdeft-register-buffer' and
`notdeft-deregister-buffer', which allow NotDeft to keep track of
changes to its note buffers."
  :lighter " ¬D"
  (if notdeft-note-mode
      (notdeft-register-buffer)
    (notdeft-deregister-buffer)))
  
(defun notdeft-refresh-after-save ()
  "Refresh global NotDeft state after saving a NotDeft note."
  (let ((file (buffer-file-name)))
    (when file
      (notdeft-changed--fs 'files (list file)))))

(defun notdeft-register-buffer (&optional buffer)
  "Register BUFFER for saving as a NotDeft note.
Use `current-buffer' as the default buffer.
Ensure that global NotDeft state gets refreshed on save."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (add-hook 'after-save-hook 'notdeft-refresh-after-save nil t))))

(defun notdeft-deregister-buffer (&optional buffer)
  "Deregister a NotDeft BUFFER.
Use `current-buffer' as the default buffer."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (remove-hook 'after-save-hook 'notdeft-refresh-after-save t))))

;;;###autoload
(defun notdeft-register-file (file)
  "Enable NotDeft note mode for any buffer of FILE."
  (let ((buf (get-file-buffer file)))
    (when buf
      (with-current-buffer buf
	(notdeft-note-mode 1)))))

;;;###autoload
(defun notdeft-save-buffer (prefix)
  "Save the current buffer as a NotDeft note.
Enable NotDeft note minor mode before saving.
The PREFIX argument is passed to `save-buffer'."
  (interactive "P")
  (notdeft-note-mode 1)
  (save-buffer prefix))

(defun notdeft-note-buffer-list ()
  "Return a list of NotDeft note buffers.
The list contains references to buffers with for which the
NotDeft note minor mode has been enabled, and thus the variable
`notdeft-note-mode' is bound and set."
  (cl-remove-if-not #'notdeft-note-buffer-p (buffer-list)))

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

;;;###autoload
(defun notdeft-switch-to-note-buffer ()
  "Switch to an existing NotDeft note buffer.
The list of choices is determined by the function
`notdeft-note-buffer-list'."
  (interactive)
  (let ((buffers (notdeft-note-buffer-list)))
    (cond
     ((not buffers)
      (message "No NotDeft notes open"))
     ((null (cdr buffers))
      (switch-to-buffer (car buffers)))
     (t
      (let* ((names (mapcar #'buffer-name buffers))
	     (name (ido-completing-read "Buffer: " names nil t)))
	(switch-to-buffer name))))))
		     
;;;###autoload
(defun notdeft-find-file (file)
  "Edit NotDeft note FILE.
Enable NotDeft note mode for the buffer for editing the file.
Called interactively, query for the FILE using the minibuffer."
  (interactive "FFind NotDeft file: ")
  (prog1 (find-file file)
    (notdeft-note-mode 1)))

;;;###autoload
(defun notdeft-create-file (&optional dir notename ext data title)
  "Create a new NotDeft note file.
Create it into the directory DIR with basename NOTENAME and
filename extension EXT, and write any DATA into the file. If any
of those values are nil, then use a default value. If DIR or EXT
is the symbol `ask', then query the user for a directory or
extension. If DIR is a non-empty list, then offer the user that
choice list of directories. If NOTENAME is of the form (title
STR), then use STR as the note title. Alternatively and
preferably any TITLE may be specified as its own argument. Use
`notdeft-new-file-data-function' to derive a note file name and
content for the note. Return the filename of the created file."
  (let* ((dir (pcase dir
	       ((pred stringp)
		dir)
	       ((pred consp)
		(notdeft-select-directory-from dir "Directory for new file: "))
	       (`ask
		(notdeft-select-directory "Directory for new file: "))
	       (_
		(notdeft-get-directory))))
	 (ext (pcase ext
	       ((pred stringp)
		ext)
	       (`ask
		(notdeft-read-extension))
	       (_
		notdeft-extension)))
	 (title (or title
		    (pcase notename
		      (`(title ,(and (pred stringp) str))
		       str)
		      (_ nil))))
	 (notename (when (stringp notename)
		     notename)))
    (pcase (funcall notdeft-new-file-data-function
		    dir notename ext data title)
      (`(,(and (pred stringp) file) .
	 ,(and (pred string-or-null-p) data))
       (notdeft-ensure-root file)
       (if (not data)
	   (notdeft-find-file file)
	 (write-region data nil file nil nil nil 'excl)
	 (notdeft-changed--fs 'files (list file))
	 (notdeft-find-file file)
	 (with-current-buffer (get-file-buffer file)
	   (goto-char (point-max))))
       file))))

(defun notdeft-sub-new-file (&optional notename data title pfx)
  "Create a new note file as specified.
Save into a file with the specified NOTENAME
\(if NOTENAME is nil, generate a name).
Save DATA as the note content.
Use the specified note TITLE,
possibly affecting note naming or content.
With a PFX >= '(4), query for a target directory;
otherwise default to the result of `notdeft-get-directory'.
With a PFX >= '(16), query for a filename extension;
otherwise default to `notdeft-extension'.
Return the name of the new file."
  (let ((pfx (prefix-numeric-value pfx)))
    (notdeft-create-file
      (and (>= pfx 4) 'ask)
      notename
      (and (>= pfx 16) 'ask)
      data
      title)))

;;;###autoload
(defun notdeft-switch-to-file-named (title &optional data)
  "Switch to a NotDeft note with the specified TITLE.
Derive a note name from the title with
`notdeft-title-to-notename', or fail that cannot be done. If no
note of the derived name and default `notdeft-extension' exists,
create one. Initialize any newly created file with DATA, possibly
as modified by `notdeft-new-file-data-function'. Return the full
file name of the file, whether created or existing. (Note that
unless `notdeft-new-file-data-function' derives its filenames in
terms of `notdeft-title-to-notename', then this command may not
behave in a useful way.)"
  (let ((notename (notdeft-title-to-notename title)))
    (unless notename
      (error "Aborting, unsuitable title: %S" title))
    (let* ((ext notdeft-extension)
	   (basename (concat notename "." ext))
	   (file (notdeft-file-by-basename basename)))
      (if (not file)
	  (notdeft-create-file nil notename ext data title)
	(notdeft-find-file file)
	file))))

;;;###autoload
(defun notdeft-new-file-named (pfx title)
  "Create a new file, prompting for a title.
The prefix argument PFX is as for `notdeft-new-file'.
Query for a TITLE when invoked as a command.
Return the filename of the created file."
  (interactive "P\nsNew title: ")
  (notdeft-sub-new-file nil nil title pfx))

;;;###autoload
(defun notdeft-new-file (pfx)
  "Create a new file quickly.
Create it with an automatically generated name, one based
on the `notdeft-filter-string' filter string if it is non-nil.
With a prefix argument PFX, offer a choice of NotDeft
directories, when there is more than one of them.
With two prefix arguments, also offer a choice of filename
extensions when `notdeft-secondary-extensions' is non-empty.
Return the filename of the created file."
  (interactive "P")
  (notdeft-sub-new-file nil nil notdeft-filter-string pfx))

(defun notdeft-note-buffer-p (&optional buffer)
  "Whether BUFFER is a NotDeft Note mode buffer.
Default to `current-buffer' if BUFFER is nil.
Return the buffer, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (when notdeft-note-mode
	buffer))))

(defun notdeft-buffer-p (&optional buffer)
  "Whether BUFFER is a `notdeft-mode' buffer.
Default to `current-buffer' if BUFFER is nil.
Return the buffer, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (when (eq major-mode 'notdeft-mode)
	buffer))))

(defun notdeft-buffer-list ()
  "Return a list of NotDeft buffers.
That is, behave like `buffer-list', but exclude all non-NotDeft
buffers."
  (cl-loop for buf in (buffer-list)
	   if (notdeft-buffer-p buf)
	   collect buf))

(defun notdeft-get-directory ()
  "Select a NotDeft directory for an operation.
If in a NotDeft note buffer, and `default-directory' or one of
its parents is a NotDeft directory, then use that directory.
Otherwise, use any previously selected `notdeft-directory'. All
else failing, query using `notdeft-select-directory', and cache
the result into `notdeft-directory'."
  (or (when (notdeft-note-buffer-p)
	(cl-some
	 (lambda (root)
	   (when (file-in-directory-p default-directory root)
	     root))
	 notdeft-directories))
      notdeft-directory
      (let ((dir (notdeft-select-directory)))
	(setq notdeft-directory (file-name-as-directory dir))
	dir)))

(defun notdeft-fail (fail)
  "Translate FAIL into a failure function.
If it is a function, return it as is. If it is the symbol
`error', return a function that accepts a message and raises it
as an error. If it is non-nil, return a function that displays
the message argument and returns nil. Otherwise return a function
that does nothing and returns nil. If FAIL is a function, it
should likewise return nil, if anything."
  (cond
   ((functionp fail)
    fail)
   ((eq fail 'error)
    (lambda (msg)
      (error "%s" msg)))
   (fail
    (lambda (msg)
      (message "%s" msg)
      nil))
   (t
    (lambda (_msg) nil))))

(defun notdeft-current-filename (&optional note-only fail)
  "Return the current NotDeft note filename.
In a `notdeft-mode' buffer, return the currently selected file's
name. Otherwise return the current buffer's file name, if any,
requiring it to name a NotDeft note if NOTE-ONLY is non-nil.
Otherwise FAIL as specified for `notdeft-fail'."
  (let ((fail (notdeft-fail fail)))
    (cond
     ((notdeft-buffer-p)
      (let ((file (widget-get (widget-at) :tag)))
	(if (not file)
	    (funcall fail "No NotDeft note selected")
	  file)))
     (note-only
      (let ((file (and (notdeft-note-buffer-p)
		       (buffer-file-name))))
	(if (not file)
	    (funcall fail "Not in a NotDeft note buffer")
	  file)))
     (t
      (let ((file (buffer-file-name)))
	(if (not file)
	    (funcall fail "Not in a file buffer")
	  file))))))

(defun notdeft-current-title (&optional note-only fail)
  "Return the current NotDeft note title.
In a `notdeft-mode' buffer, return the currently selected file's
title. Otherwise return the current buffer's title, requiring it
to be a NotDeft note buffer if NOTE-ONLY is non-nil. If the
current file or buffer has no title or the title is whitespace
only, return nil. If not on a note file or buffer, FAIL as
specified for `notdeft-fail'."
  (if (notdeft-buffer-p)
      (let ((file (notdeft-current-filename note-only fail)))
	(when file
	  (notdeft-file-title file)))
    (if (and note-only (not (notdeft-note-buffer-p)))
	(funcall (notdeft-fail fail) "Not in a NotDeft note buffer")
      (car (notdeft-parse-buffer)))))

(defun notdeft-select-file ()
  "Open the selected file, if any."
  (interactive)
  (let ((old-file (notdeft-current-filename nil t)))
    (when old-file
      (notdeft-find-file old-file))))

;;;###autoload
(defun notdeft-delete-file (file &optional trash kill-buffer
			    interactively)
  "Delete a NotDeft note FILE (or TRASH it).
When called interactively, delete the selected or current note,
and prompt before proceeding. With one \\[universal-argument]
prefix also KILL-BUFFER the deleted file's buffer, if any. Unless
two \\[universal-argument] prefixes are given, ask `delete-file'
to TRASH the file; that should result in the file being moved to
the system's trash can instead of being deleted, provided that
`delete-by-moving-to-trash' is non-nil. Print messages
accordingly when called INTERACTIVELY. Return the file name of
any deleted file, or nil if `delete-file' was not executed."
  (interactive
   (let ((prefix current-prefix-arg))
     (list (notdeft-current-filename nil t)
	   (not (equal prefix '(16)))
	   (equal prefix '(4))
	   t)))
  (let ((old-file file))
    (cond
     ((not old-file)
      nil)
     ((notdeft-file-sparse-p old-file)
      (when interactively
	(message "Cannot delete fixed-path file")))
     (t
      (let ((actually-trash
	     (and trash delete-by-moving-to-trash))
	    (old-file-nd
	     (file-name-nondirectory old-file)))
	(when (or (not interactively)
		  (y-or-n-p
		   (concat (if actually-trash "Trash" "Delete")
			   " file " old-file-nd "? ")))
	  (prog1
	      (when (file-exists-p old-file)
		;; This may result in the file being trashed rather than
		;; deleted, and we assume that any trash can is not one of
		;; the `notdeft-directories' (or under them), which would
		;; be weird.
		(delete-file old-file trash)
		old-file)
	    (delq old-file notdeft-current-files)
	    (delq old-file notdeft-all-files)
	    (notdeft-changed--fs 'files (list old-file))
	    (when kill-buffer
	      (let ((buf (get-file-buffer old-file)))
		(when buf
		  (kill-buffer buf))))
	    (when interactively
	      (message "%s %S"
		       (if actually-trash "Trashed" "Deleted")
		       old-file-nd)))))))))

;;;###autoload
(defun notdeft-move-into-subdir (prefix)
  "Move the file at point into a subdirectory of the same name.
To nest more than one level (which is allowed but perhaps atypical),
invoke with a PREFIX argument to force the issue."
  (interactive "P")
  (let ((old-file (notdeft-current-filename t t)))
    (cond
     ((not old-file)
      nil)
     ((notdeft-file-sparse-p old-file)
      (message "Cannot move fixed-path file"))
     ((and (not prefix) (notdeft-file-in-subdir-p old-file))
      (message "Already in a NotDeft sub-directory (%S)"
	       (file-name-directory old-file)))
     (t
      (let ((new-file
	     (concat
	      (file-name-directory old-file)
	      (file-name-as-directory (notdeft-base-filename old-file))
	      (file-name-nondirectory old-file))))
	(notdeft-rename-file+buffer old-file new-file nil t)
	(notdeft-changed--fs 'files (list old-file new-file))
	(message "Renamed as %S" new-file))))))

;;;###autoload
(defun notdeft-change-file-extension ()
  "Change the filename extension of a NotDeft note.
Operate on the selected or current NotDeft note file."
  (interactive)
  (let ((old-file (notdeft-current-filename nil t)))
    (cond
     ((not old-file)
      nil)
     ((not notdeft-secondary-extensions)
      (message "Only one configured extension"))
     ((notdeft-file-sparse-p old-file)
      (message "Cannot rename fixed-path file"))
     (t
      (let* ((old-ext (file-name-extension old-file))
	     (new-ext (notdeft-read-extension old-ext)))
	(unless (string= old-ext new-ext)
	  (let ((new-file
		 (concat (file-name-sans-extension old-file) "." new-ext)))
	    (notdeft-rename-file+buffer old-file new-file)
	    (notdeft-changed--fs 'files (list old-file new-file))
	    (message "Renamed as %S" new-file))))))))

;;;###autoload
(defun notdeft-rename-file (pfx)
  "Rename the selected or current NotDeft note file.
Defaults to a content-derived file name (rather than the old one)
if called with a prefix argument PFX."
  (interactive "P")
  (let ((old-file (notdeft-current-filename nil t)))
    (cond
     ((not old-file)
      nil)
     ((notdeft-file-sparse-p old-file)
      (message "Cannot rename fixed-path file"))
     (t
      (let* ((old-name (notdeft-base-filename old-file))
	     (def-name
	       (or (when pfx
		     (let ((title
			    (if (notdeft-buffer-p)
				(notdeft-title-from-file-content old-file)
			      (notdeft-parse-title (buffer-string)))))
		       (and title (notdeft-title-to-notename title))))
		   old-name))
	     (new-file (notdeft-sub-rename-file old-file old-name def-name)))
	(when new-file
	  (message "Renamed as %S" new-file)))))))

(defun notdeft-sub-rename-file (old-file old-name def-name)
  "Rename OLD-FILE with the OLD-NAME NotDeft name.
Query for a new name, defaulting to DEF-NAME. Use OLD-FILE's
filename extension in the new name. If the file was renamed,
return the new filename, and otherwise return nil."
  (let* ((history (list def-name))
	 (new-name
	  (read-string
	   (concat "Rename " old-name " to (without extension): ")
	   (car history) ;; INITIAL-INPUT
	   '(history . 1) ;; HISTORY
	   nil ;; DEFAULT-VALUE
	   ))
	 (new-file
	  (notdeft-make-filename new-name
	    (file-name-extension old-file)
	    (file-name-directory old-file))))
  (unless (string= old-file new-file)
    (notdeft-rename-file+buffer old-file new-file)
    (notdeft-changed--fs 'files (list old-file new-file))
    new-file)))

(defun notdeft-rename-file+buffer (old-file new-file &optional exist-ok mkdir)
  "Like `rename-file', rename OLD-FILE as NEW-FILE.
Additionally, rename any OLD-FILE buffer as NEW-FILE, and also
set its visited file as NEW-FILE. EXIST-OK is as the third
argument of `rename-file'. If MKDIR is non-nil, also create any
missing target directory, but do not create its parent
directories."
  (when mkdir
    (ignore-errors
      (make-directory (file-name-directory new-file) nil)))
  (rename-file old-file new-file exist-ok)
  (let ((buf (get-file-buffer old-file)))
    (when buf
      (save-current-buffer
        (set-buffer buf)
        (set-visited-file-name new-file nil t)))))

(defun notdeft-rename-directory+buffer (old-dir new-dir &optional mkdir)
  "Like `rename-file', rename OLD-DIR as NEW-DIR.
If MKDIR is non-nil, also create any missing target directory,
but do not create its parent directories. Error out if NEW-DIR
already exists. After renaming the directory, also rename any
affected NotDeft note buffers, and also set their visited files
to be the ones under NEW-DIR."
  (when mkdir
    (ignore-errors
      (make-directory (file-name-directory new-dir) nil)))
  (rename-file old-dir new-dir)
  (let ((old-dir (file-name-as-directory old-dir)))
    (dolist (buf (notdeft-note-buffer-list))
      (let ((old-file (buffer-file-name buf)))
	(when (and old-file (string-prefix-p old-dir old-file))
	  (let ((new-file (concat
			   (file-name-as-directory new-dir)
			   (substring old-file (length old-dir)))))
	    (save-current-buffer
	      (set-buffer buf)
	      (set-visited-file-name new-file nil t))))))))

(defun notdeft-sub-move-file (old-file dest-dir &optional whole-dir mkdir)
  "Move the OLD-FILE note file into the DEST-DIR directory.
If OLD-FILE has its own subdirectory, then move the entire
subdirectory, but only if WHOLE-DIR is true. If WHOLE-DIR is the
symbol `ask', then ask for confirmation first. With a non-nil
argument MKDIR, create any missing target directory \(one level
only). Return the old pathname of the file or directory that was
moved, or nil if nothing was moved."
  (let ((moving-sub (notdeft-file-in-subdir-p old-file)))
    (if (not moving-sub)
	(let ((new-file (concat (file-name-as-directory dest-dir)
				(file-name-nondirectory old-file))))
	  (notdeft-rename-file+buffer old-file new-file nil mkdir)
	  old-file)
      (unless whole-dir
	(error "Attempt to move file in a sub-directory: %S" old-file))
      (let ((old-dir (directory-file-name
		      (file-name-directory old-file))))
	(unless (and (eq whole-dir 'ask)
		     (not (y-or-n-p
			   (concat "Move entire directory "
				   (file-name-nondirectory old-dir) "? "))))
	  (let ((new-dir (concat (file-name-as-directory dest-dir)
				 (file-name-nondirectory old-dir))))
	    (notdeft-rename-directory+buffer old-dir new-dir mkdir)
	    old-dir))))))

(defvar notdeft-previous-target nil
  "Previous file move target NotDeft directory.
Local to a NotDeft mode buffer. Set to nil if `notdeft-move-file'
has not been used to move a file.")

;;;###autoload
(defun notdeft-move-file (&optional pfx)
  "Move the selected file under selected NotDeft root.
Query the user for a target from among `notdeft-directories'.
Offer to create the chosen NotDeft root directory if it does not
already exist. If the file resides in a subdirectory, move the
entire subdirectory, but require confirmation as a non-nil PFX
argument, or by asking. Moving an external \(non-Deft) file under
a NotDeft root is also allowed."
  (interactive "p")
  (let ((old-file (notdeft-current-filename nil t)))
    (cond
     ((not old-file)
      nil)
     ((notdeft-file-sparse-p old-file)
      (message "Cannot move fixed-path file"))
     (t
      (let* ((old-root (notdeft-dir-of-file old-file))
	     (choices ;; exclude any `old-root'
	      (if (not old-root)
		  notdeft-directories
		(cl-remove-if (lambda (dir)
				(file-equal-p dir old-root))
			      notdeft-directories)))
	     (choices ;; default to any `notdeft-previous-target'
	      (if (not notdeft-previous-target)
		  choices
		(notdeft-list-prefer
		 choices
		 (lambda (dir)
		   (notdeft-file-equal-p dir notdeft-previous-target)))))
	     (chosen-root
	      (notdeft-select-directory-from choices nil t t))
	     (new-root
	      (notdeft-canonicalize-root chosen-root)))
	(if (and old-root
		 (file-exists-p new-root)
		 (file-equal-p old-root new-root))
	    (message "File %S already under root %S" old-file chosen-root)
	  (notdeft-ensure-root new-root)
	  (let ((moved-file
		 (notdeft-sub-move-file old-file new-root
					(and pfx (if (> pfx 1) t 'ask)))))
	    (if (not moved-file)
		(message "Did not move %S" old-file)
	      (setq notdeft-previous-target new-root)
	      (notdeft-changed--fs
	       'dirs (delete nil (list old-root new-root)))
	      (message "Moved %S under root %S" moved-file chosen-root)))))))))

;;;###autoload
(defun notdeft-archive-file (&optional pfx)
  "Archive the selected NotDeft note file.
Archive it under `notdeft-archive-directory', under its NotDeft
root directory. If it resides in a subdirectory, archive the
entire directory, but require confirmation as a non-nil PFX
argument, or by asking the user when called interactively."
  (interactive "p")
  (let ((old-file (notdeft-current-filename t t)))
    (cond
     ((not old-file)
      nil)
     ((notdeft-file-sparse-p old-file)
      (message "Cannot archive fixed-path file"))
     (t
      (let ((old-root (notdeft-dir-of-file old-file)))
	(if (not old-root)
	    (message "Cannot archive non-Deft file")
	  (let* ((new-dir
		  (concat old-root
			  (file-name-as-directory notdeft-archive-directory)))
		 (moved-file
		  (notdeft-sub-move-file old-file new-dir
					 (and pfx (if (> pfx 1) t 'ask)) t)))
	    (if (not moved-file)
		(message "Did not archive %S" old-file)
	      (notdeft-changed--fs 'dirs (list old-root))
	      (message "Archived %S into %S" moved-file new-dir)))))))))

(eval-when-compile
  (defvar deft-directory))
(declare-function deft-refresh "deft")

;;;###autoload
(defun notdeft-open-in-deft ()
  "Open the selected note's Deft directory in Deft.
Do that only when the command `deft' is available. This
implementation makes assumptions about Deft."
  (interactive)
  (when (fboundp 'deft)
    (let ((old-file (notdeft-current-filename t t)))
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

;;;###autoload
(defun notdeft-show-file-directory ()
  "Show NotDeft directory of the selected note."
  (interactive)
  (let ((old-file (notdeft-current-filename t t)))
    (when old-file
      (let ((dir (notdeft-dir-of-file old-file)))
	(if (not dir)
	    (message "Not on a NotDeft file")
	  (message "%s" dir))))))

(defun notdeft-show-file-info ()
  "Show information about the selected note.
Show filename, title, summary, etc."
  (interactive)
  (let ((file (widget-get (widget-at) :tag)))
    (if (not file)
	(message "Not on a file")
      (let* ((title (notdeft-file-title file))
	     (summary (notdeft-file-summary file)))
	(message "name=%S file=%S title=%S summary=%S"
		 (file-name-nondirectory file)
		 file title
		 (and summary
		      (substring summary 0 (min 50 (length summary)))))))))

(defun notdeft-show-find-file-parse (file)
  "Query for a FILE, and show its parse information."
  (interactive "F")
  (let ((res (with-temp-buffer
	       (insert-file-contents file)
	       (notdeft-parse-buffer))))
    (message "name=%S file=%S parse=%S"
	     (file-name-nondirectory file)
	     file res)))

(defun notdeft-show-file-parse ()
  "Show parse information for the file at point."
  (interactive)
  (let ((file (widget-get (widget-at) :tag)))
    (if (not file)
	(message "Not on a file")
      (notdeft-show-find-file-parse file))))

;; File list filtering

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
  (interactive)
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
  (interactive "P")
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
In particular, update `notdeft-current-files'.
Get the character from the variable `last-command-event'."
  (interactive)
  (let ((char (if input-method-function
                  (let ((buffer-read-only nil))
                    (car (funcall input-method-function last-command-event)))
                last-command-event
                )))
    (when (= char ?\S-\ )
      (setq char ?\s))
    (setq char (char-to-string char))
    (setq notdeft-filter-string (concat notdeft-filter-string char))
    (notdeft-changed--filter)))

(defun notdeft-filter-decrement ()
  "Remove last character from the filter string and update state.
In particular, update `notdeft-current-files'."
  (interactive)
  (notdeft-filter
   (and (> (length notdeft-filter-string) 1)
	(substring notdeft-filter-string 0 -1))))

(defun notdeft-filter-decrement-word ()
  "Remove last word from the filter, if possible, and update.
This is like `backward-kill-word' on the filter string, but the
kill ring is not affected."
  (interactive)
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
  (interactive)
  (let ((s (current-kill 0 t)))
    (notdeft-filter
     (if notdeft-filter-string
	 (concat notdeft-filter-string s)
       s))))

(defun notdeft-complete ()
  "Complete the current action.
If there is a widget at point, press it. Otherwise, open the
first listed file. If none are listed, but there is an active
filter, quickly create a new file using the filter string as the
title. Otherwise, quickly create a new file."
  (interactive)
  (cond
   ((widget-at)
    (widget-button-press (point)))
   (notdeft-current-files
    (notdeft-find-file (car notdeft-current-files)))
   (t
    (notdeft-new-file 1))))

;;;###autoload
(defun notdeft-gc ()
  "Garbage collect to remove uncurrent NotDeft state.
More specifically, delete obsolete cached file and directory
information."
  (interactive)
  (notdeft-cache-gc)
  (notdeft-dirlist-gc))

(defun notdeft-quit (prefix)
  "Quit NotDeft mode.
With one PREFIX argument, kill the buffer. With two prefix
arguments, kill all NotDeft mode buffers."
  (interactive "P")
  (quit-window prefix)
  (when (equal prefix '(16))
    (dolist (buf (notdeft-buffer-list))
      (kill-buffer buf))))

;;; Mode definition

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
    ;; Handle return via completion or opening file
    (define-key map (kbd "RET") #'notdeft-complete)
    ;; Filtering
    (define-key map (kbd "DEL") #'notdeft-filter-decrement)
    (define-key map (kbd "C-c C-l") #'notdeft-filter)
    (define-key map (kbd "C-c C-c") #'notdeft-filter-clear)
    (define-key map (kbd "C-y") #'notdeft-filter-yank)
    (define-key map (kbd "M-DEL") #'notdeft-filter-decrement-word)
    (define-key map (kbd "<C-S-backspace>") #'notdeft-filter-clear)
    ;; File management
    (define-key map (kbd "C-c C-x i") #'notdeft-show-file-info)
    (define-key map (kbd "C-c C-x p") #'notdeft-show-file-parse)
    (define-key map (kbd "C-c C-x P") #'notdeft-show-find-file-parse)
    ;; Miscellaneous
    (define-key map (kbd "C-c C-x f") #'notdeft-grep-for-filter)
    (define-key map (kbd "C-c C-q") #'notdeft-quit)
    ;; Widgets
    (define-key map [down-mouse-1] #'widget-button-click)
    (define-key map [down-mouse-2] #'widget-button-click)
    ;; Xapian
    (define-key map (kbd "C-c C-o") #'notdeft-query-edit)
    (define-key map (kbd "<tab>") #'notdeft-query-edit)
    (define-key map (kbd "<backtab>") #'notdeft-query-clear)
    (define-key map (kbd "<S-tab>") #'notdeft-query-clear)
    (let ((parent-map (make-sparse-keymap)))
      (define-key parent-map (kbd "C-c") 'notdeft-global-map)
      (set-keymap-parent map parent-map)
      map))
  "Keymap for NotDeft mode.

\\{notdeft-mode-map}")

;;;###autoload
(defun notdeft-reindex ()
  "Recreate all indexes for `notdeft-directories'.
A `notdeft-refresh' is normally sufficient, but this command
should help if the Xapian search index becomes corrupted for some
reason, as indexes are re-built from scratch."
  (interactive)
  (notdeft-global-do-pending nil t))

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

;;;###autoload
(defun notdeft-refresh (&optional reset)
  "Refresh or reset NotDeft state.
Refresh NotDeft state so that outside filesystem changes get
noticed. Also reset state to clear caches and queries and such if
RESET is non-nil, or if the command prefix \\[universal-argument]
is given interactively. With two \\[universal-argument] prefixes,
clear queries and filters for all NotDeft mode buffers. Invoke
this command manually if NotDeft files change outside of NotDeft
mode and NotDeft note minor mode \(as toggled by the command
`notdeft-mode' and the command `notdeft-note-mode'), as such
changes are not detected automatically. Also invoke this if you
change `notdeft-directories' or `notdeft-sparse-directories'."
  (interactive "P")
  (run-hooks 'notdeft-pre-refresh-hook)
  (notdeft-dcache t)
  (when reset
    (notdeft-reset (equal reset '(16))))
  (notdeft-changed--fs 'anything)
  (run-hooks 'notdeft-post-refresh-hook))

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

(defun notdeft-ensure-root (file)
  "Maybe offer to create a NotDeft directory for FILE.
If FILE is one of the `notdeft-directories' or a file or
directory under it, offer to create that root directory if it
does not exist. Create directories recursively if necessary.
Always return FILE."
  (when notdeft-directories
    (let ((root (notdeft-dir-of-file file)))
      (when (and root (not (file-directory-p root)))
	(when (file-exists-p root)
	  (error "Data \"directory\" is a non-directory: %S" root))
	(when (y-or-n-p (concat "Create directory " root "? "))
	  (make-directory root t)))))
  file)

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

(defun notdeft-drop-nth-cons (n lst)
  "Make list element at position N the first one of LST.
That is, functionally move that element to position 0."
  (let* ((len (length lst))
	 (rst (- len n)))
    (cons (nth n lst) (append (butlast lst rst) (last lst (- rst 1))))))

(defun notdeft-read-extension (&optional prefer)
  "Read a NotDeft filename extension, interactively.
The default choice is `notdeft-extension', but any of the
`notdeft-secondary-extensions' are also available as choices.
With a PREFER argument, use that extension as the first choice."
  (if (not notdeft-secondary-extensions)
      notdeft-extension
    (let* ((choices (cons notdeft-extension notdeft-secondary-extensions))
	   (choices (if prefer
			(notdeft-list-prefer choices
			  `(lambda (ext) (string= ,prefer ext)))
		      choices)))
      (ido-completing-read "Extension: " choices nil t))))

(defun notdeft-list-prefer (choices prefer)
  "Re-order the CHOICES list to make preferred element first.
PREFER is a predicate for identifying such an element.
Move only the first matching element, if any.
Return CHOICES as is if there are no matching elements."
  (let ((ix (cl-position-if prefer choices)))
    (if ix (notdeft-drop-nth-cons ix choices) choices)))

(defun notdeft-select-directory-from (dirs &optional prompt confirm preserve)
  "Like `notdeft-select-directory', but select from DIRS.
The PROMPT, CONFIRM, and PRESERVE arguments are as for
`notdeft-select-directory'."
  (cond
   ((not dirs)
    (error "No data directory choices"))
   ((and (not confirm) (= (length dirs) 1))
    (car dirs))
   (t
    (when (and notdeft-directory (not preserve))
      (setq dirs (notdeft-list-prefer
		  dirs
		  (lambda (file)
		    (notdeft-file-equal-p notdeft-directory file)))))
    (ido-completing-read (or prompt "Data directory: ")
			 dirs nil t))))

(defun notdeft-select-directory (&optional prompt confirm preserve)
  "Select a NotDeft directory, possibly interactively.
If DIRS is non-nil, select from among those directories;
otherwise select from `notdeft-directories'. Use the specified
PROMPT in querying, if given. Return the selected directory, or
error out. If CONFIRM is non-nil, query even if there is only a
single choice. Present any `notdeft-directory' as the first
choice, except with a true PRESERVE argument, which preserves
DIRS order."
  (notdeft-select-directory-from notdeft-directories
				 prompt confirm preserve))

;;;###autoload
(defun notdeft-chdir ()
  "Change `notdeft-directory' according to interactive selection.
Query for a directory with `notdeft-select-directory'."
  (interactive)
  (let ((dir (notdeft-select-directory)))
    (setq notdeft-directory (file-name-as-directory dir))
    (message "Data directory set to %S" notdeft-directory)))

(defun notdeft-open-file-by-basename (filename)
  "Open a NotDeft file named FILENAME.
FILENAME is a non-directory filename, with an extension \(it is
not necessarily unique). Returns the resolved path, or nil if
none was found."
  (let ((fn (notdeft-file-by-basename filename)))
    (if (not fn)
	(message "No NotDeft note %S" filename)
      (notdeft-find-file fn))
    fn))

;;;###autoload
(defun notdeft-open-query (&optional query rank negate)
  "Open NotDeft with an Xapian search QUERY.
When called interactively, read the QUERY interactively. With
non-nil RANK, have results ranked by relevance; when called
interactively, the command prefix \\[universal-argument] 1 will
set this option. Open the query in a new buffer as specified by
the `notdeft-open-query-in-new-buffer' configuration option; a
non-nil NEGATE argument reverses that setting, as does the prefix
\\[universal-argument] when called interactively."
  (interactive (let ((prefix current-prefix-arg))
		 (list (notdeft-xapian-read-query)
		       (equal prefix 1)
		       (equal prefix '(4)))))
  (when notdeft-xapian-program
    (let* ((query (if rank (concat "!rank " (or query "")) query))
	   (new notdeft-open-query-in-new-buffer)
	   (new (if negate (not new) new)))
      (notdeft nil new)
      (notdeft-xapian-query-set query))))

(defun notdeft-ido-completing-read (files &optional prompt)
  "Present a choice of FILES with `ido-completing-read'.
Only present the non-directory component of each file. There may
be duplicates of the same non-directory name. If non-nil, use the
specified PROMPT. Return the path of the selected note file."
  (let ((choices
	 (mapcar
	  (lambda (file)
	    (propertize (file-name-nondirectory file) 'path file))
	  files)))
    (let ((file
	   (get-text-property
	    0 'path
	    (ido-completing-read (or prompt "File: ") choices nil t))))
      file)))

(defun notdeft-ido-select-note-file ()
  "Offer an Ido choice list of all notes.
Return a file name for the selected note. Return nil if there are
no notes from which to select."
  (let* ((name-lst (notdeft-make-basename-list))
	 (name (when name-lst
		 (ido-completing-read "NotDeft note: " name-lst)))
	 (file (when name
		 (notdeft-file-by-basename name))))
    file))

(defun notdeft-search-select-note-file (&optional query)
  "Search for a file matching a query.
If a QUERY is provided, then use it as is. Otherwise offer
`notdeft-select-note-file-query' for interactive editing,
accounting for `notdeft-xapian-query-history'. If there are any
matches in the query search results, present a choice list of
non-directory filenames with `notdeft-completing-read-function'.
Return the path of the chosen file, or nil if nothing was found."
  (when notdeft-xapian-program
    (let ((query (or query
		     (notdeft-xapian-read-query
		      notdeft-select-note-file-query))))
      (when query
	(let* ((notdeft-xapian-max-results
		(if notdeft-select-note-file-all
		    0
		  notdeft-xapian-max-results))
	       (files (notdeft-xapian-search-all-dirs query)))
	  (when files
	    (funcall notdeft-completing-read-function files)))))))

(defun notdeft-select-note-file ()
  "Let the user choose a note file.
Return the file name of the chosen note, or nil. Some selection
options may or may not be honored, depending on the method of
selection."
  (funcall (or notdeft-select-note-file-function
	       (if (and notdeft-xapian-program
			notdeft-select-note-file-by-search)
		   #'notdeft-search-select-note-file
		 #'notdeft-ido-select-note-file))))

;;;###autoload
(defun notdeft-query-select-find-file (&optional query by-time)
  "Open one of the files matching Xapian search QUERY.
If called interactively, read a search query interactively,
accounting for `notdeft-xapian-query-history' and any selected
region. If there is more than one match, present a choice list of
non-directory filenames with `notdeft-completing-read-function'.
Order the choices by relevance, or BY-TIME if requested."
  (interactive
   (let ((by-time (equal current-prefix-arg '(4)))
	 (no-region (equal current-prefix-arg '(16))))
     (list
      (notdeft-xapian-read-query
       (when (and mark-active (not no-region))
	 (buffer-substring-no-properties
	  (region-beginning) (region-end))))
      by-time)))
  (when notdeft-xapian-program
    (let* ((notdeft-xapian-order-by-time by-time)
	   (file (notdeft-search-select-note-file query)))
      (if (not file)
	  (message "No matching notes found")
	(notdeft-find-file file)))))

;;;###autoload
(defun notdeft-query-ido-find-file (&optional query by-time)
  "Deprecated. Use `notdeft-query-select-find-file'.
QUERY and BY-TIME are as for that function."
  (interactive (list (notdeft-xapian-read-query) current-prefix-arg))
  (when notdeft-xapian-program
    (let ((notdeft-completing-read-function
	   'notdeft-ido-completing-read))
      (notdeft-query-select-find-file query by-time))))

;;;###autoload
(defun notdeft-lucky-find-file (&optional query by-time)
  "Open the highest-ranked note matching a search QUERY.
If BY-TIME is non-nil, then choose the most recent matching note
instead of the highest-ranked one. If called interactively, read
a search query interactively, accounting for
`notdeft-xapian-query-history' and any selected region. Open the
file directly, without switching to any `notdeft-buffer'."
  (interactive
   (let ((by-time (equal current-prefix-arg '(4)))
	 (no-region (equal current-prefix-arg '(16))))
     (list
      (notdeft-xapian-read-query
       (when (and mark-active (not no-region))
	 (buffer-substring-no-properties
	  (region-beginning) (region-end))))
      by-time)))
  (when notdeft-xapian-program
    (let* ((notdeft-xapian-order-by-time by-time)
	   (notdeft-xapian-max-results 1)
	   (files (notdeft-xapian-search-all-dirs query)))
      (if (not files)
	  (message "No matching notes found")
	(notdeft-find-file (car files))))))

;;;###autoload
(defun notdeft-list-files-by-query (query)
  "Return a list of files matching Xapian QUERY."
  (when notdeft-xapian-program
    (notdeft-xapian-search-all-dirs query)))

(defun notdeft-string-as-phrase-query (str)
  "Turn STR into a phrase query."
  (let* ((str (downcase str))
	 (str (replace-regexp-in-string "\"" "" str))
	 (str (concat "\"" str "\"")))
    str))

(defun notdeft-open-phrase-as-query (phrase &optional rank negate)
  "Query for PHRASE.
Treat the phrase string as a phrase whose words must all appear
in the specified order. The RANK and NEGATE arguments are as for
`notdeft-open-query'."
  (let ((phrase (notdeft-string-as-phrase-query phrase)))
    (notdeft-open-query phrase rank negate)))

;;;###autoload
(defun notdeft-open-title-as-query (&optional rank negate)
  "Query for the title of the current note.
The RANK and NEGATE arguments are as for `notdeft-open-query'. In
the unranked case do a phrase search for the title. When called
interactively, any prefix arguments are also interpreted in the
`notdeft-open-query' sense."
  (interactive
   (let ((prefix current-prefix-arg))
     (list (equal prefix 1)
	   (equal prefix '(4)))))
  (let ((title (notdeft-current-title nil t)))
    (when title
      (notdeft-open-query
       (if rank title (notdeft-string-as-phrase-query title))
       rank negate))))

(provide 'notdeft)

(run-hooks 'notdeft-load-hook)

;;; notdeft.el ends here
