;;; notdeft.el --- Note manager and search engine  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;;	Jason R. Blevins <jrblevin@sdf.org>
;; Maintainer: Tero Hasu <tero@hasu.is>
;; Homepage: https://tero.hasu.is/notdeft/
;; Keywords: files text notes search
;; Package-Requires: ((emacs "28.1") (org "9.3") (transient "0.5.0"))

;; This file is not part of GNU Emacs.

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
;;
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

;; NotDeft is an Emacs package for managing and navigating plain text
;; note collections. For note storage it uses the filesystem with
;; (mostly flat) directory hierarchies, with each directory (tree)
;; containing a "bucket" of notes. As NotDeft does not have much
;; support for tagging of notes or organizing them hierarchically, it
;; mostly aims to support finding notes based on free-form text search
;; queries, without much advance organization. It also offers some
;; support for navigating note collections based on custom kinds of
;; Org mode links, but that naturally only applies to Org documents.

;; NotDeft is open source software.  This version is a fork of
;; Deft version 0.3, which was released on September 11, 2011.

;; Note File Directories

;; All NotDeft files or notes are simple plain text files (e.g., Org
;; markup files). As an example, we might have the following directory
;; structure:
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

;; NotDeft comes with a dedicated `notdeft-mode' major mode for
;; entering searches and viewing and filtering search results.

;; Instead of the above mode of operation, it is also possible to use
;; NotDeft's search functionality without a NotDeft buffer, by
;; invoking NotDeft's variants of the `find-file' command from any
;; major mode. For example, `notdeft-lucky-find-file' opens the "best"
;; search query match directly, whereas
;; `notdeft-xapian-ido-search-find-file' presents the matches for
;; selection in the minibuffer, while `notdeft-search-find-file' is a
;; more general and configurable command for finding and opening a
;; note file.

;; Getting Started

;; To actually use NotDeft's search engine to get search results, you
;; must first compile the `notdeft-xapian` program, which is
;; responsible for creating and accessing the search index(es). The
;; variable `notdeft-xapian-program' must specify the location of the
;; compiled executable in order for NotDeft to use it.

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

;; It can be useful to create a global keybinding for the `notdeft' UI
;; launching command (e.g., a function key) to start it quickly. You
;; can easily set up such a binding. For example, to bind `notdeft' to
;; F8, add the following code to your `.emacs` file:
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

;; NotDeft (2017-12-05):

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
(require 'notdeft-base)
(require 'notdeft-util)
(require 'notdeft-xapian)
(require 'subr-x)

;;; Customization

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

(defcustom notdeft-select-note-file-by-search t
  "Whether to do a search when selecting a note file.
Affects the default behavior of `notdeft-select-note-file', which
is used generally for selecting notes. This setting may make
sense for small note collections, for which it is not too
expensive to present a full note file choice list without first
narrowing down the selection."
  :type 'boolean
  :safe #'booleanp
  :group 'notdeft)

(defcustom notdeft-select-note-file-max-choices 0
  "Maximum number of choices to present in note selection.
Affects the default behavior of `notdeft-select-note-file', which
is used generally for selecting notes. This setting, if non-nil,
overrides the `notdeft-xapian-max-results' setting used for the
search. Setting the value to 0 will disable any limit, meaning
that all matches will be presented for selection, which may not
be appropriate for all searches on very large note collections."
  :type 'integer
  :safe #'integerp
  :group 'notdeft)

(defcustom notdeft-archive-directory "_archive"
  "Sub-directory name for archived notes.
Should begin with '.', '_', or '#' to be excluded from
indexing for Xapian searches."
  :type 'string
  :safe #'stringp
  :group 'notdeft)

;;; Global variables

(defvar notdeft-new-file-data-function 'notdeft-new-file-data
  "Function for computing a new note's name and content.
Will be called for all new notes, but not ones that are renamed
or copied or moved (as an existing file). Must return the note
data as a (file-name . content-data) pair, where the data part
can be nil for an empty note. The function must accept the
parameters (DIR NOTENAME EXT DATA TITLE). DIR is a non-nil
directory path for the name. NOTENAME may be nil if one has not
been given for the new note. EXT is a non-nil file name extension
for the note. Note content text DATA may be given for the new
note, possibly for further manipulation, but will be nil for an
empty note. TITLE may be nil if one has not been provided.
Uniqueness of the constructed file name should be ensured if
desired, as otherwise note creation will fail due to a naming
conflict. See `notdeft-new-file-data' for an example
implementation.")

(defvar notdeft-compread-file-history nil
  "History of selected NotDeft note files.
Should be used by `notdeft-compread-file-function' as the history
variable, if supported.")

(defvar notdeft-compread-file-function
  'notdeft-ido-compread-file
  "Function to use for note file selection.
The function is used in the sense of `completing-read' to pick a
file from a list. The function must take a list of file paths,
and an optional prompt, in that order. It may require a
selection, or it may return nil for non-selection. See
`notdeft-ido-compread-file' for an example implementation.")

(defvar notdeft-load-hook nil
  "Hook run immediately after `notdeft' feature load.")

(defvar notdeft-pre-refresh-hook nil
  "Hook run before each `notdeft-refresh'.")

(defvar notdeft-post-refresh-hook nil
  "Hook run after each `notdeft-refresh'.")

(defvar notdeft-reset-hook nil
  "Hook run to reset application state.")

(defvar notdeft-after-index-change-hook nil
  "Hook run after Xapian search index changes.")

(defvar notdeft-after-fs-change-hook nil
  "Hook run after changes to note files.")

(defvar notdeft-pending-reindex t
  "Whether to do initial, one-off search indexing.
This is a global flag referenced by `notdeft-global-do-pending'.
For the search index to stay current for subsequent queries, use
only NotDeft mode, NotDeft note mode, and NotDeft commands for
making changes to a note collection.")

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

;;; `notdeft-mode' status predicates

(defun notdeft-mode-loaded-p ()
  "Whether the `notdeft-mode' feature has been loaded."
  (fboundp 'notdeft))

(defun notdeft-buffer-p (&optional buffer)
  "Whether BUFFER is a `notdeft-mode' buffer.
Default to `current-buffer' if BUFFER is nil.
Return the buffer, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (when (eq major-mode 'notdeft-mode)
	buffer))))

;;; File processing

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
directory DIR (their defaults are as for
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

(defvar notdeft-directory-files-regexp "^[^._#/][^/]*$"
  "A match regexp for `directory-files'.
The regular expression to use as the third argument when calling
`directory-files' to look for notes and note subdirectories from
the file system. This should be specified to that it is
consistent with the Xapian program's filtering of readdir
results.")

(defun notdeft-root-find-file (file-p root)
  "Find a file matching predicate FILE-P under ROOT.
FILE-P is called with the file path name (including the ROOT
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
      (notdeft-dcache--sparse-file-by-basename name (notdeft-dcache))))

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

(defun notdeft-make-note-file-list (&optional only-basenames)
  "Return the names of all NotDeft notes.
Return full path names, or optionally ONLY-BASENAMES. Search all
existing `notdeft-directories', and include all existing
`notdeft-sparse-directories' files. The result list is sorted by
the `string-lessp' relation, and if it contains ONLY-BASENAMES it
may contain duplicates."
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
    (when only-basenames
      (setq fn-lst (mapcar #'file-name-nondirectory fn-lst)))
    ;; `sort` may modify `fn-lst`
    (sort fn-lst 'string-lessp)))

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

(defmacro notdeft-with-xapian (&rest then-forms)
  "Check for `notdeft-xapian-program'.
Evaluate THEN-FORMS if the variable is set to a non-nil value,
and raise an error otherwise."
  (declare (indent defun))
  `(if notdeft-xapian-program
       (progn ,@then-forms)
     (error "Cannot execute operation without `notdeft-xapian-program'")))

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

(defun notdeft-xapian-index-all-dirs (&optional recreate)
  "Refresh Xapian indexes for all configured directories.
The RECREATE argument is as for `notdeft-xapian-index-dirs'."
  (notdeft-with-xapian
    (notdeft-xapian-index-dirs
     (append notdeft-sparse-directories notdeft-directories)
     recreate)))

(defun notdeft-xapian-search-all-dirs (query)
  "Execute Xapian QUERY on all configured directories."
  (notdeft-with-xapian
    (notdeft-xapian-search
     (notdeft-dcache--roots (notdeft-dcache))
     query)))

(defun notdeft-xapian-list-all-keywords ()
  "List keywords from all configured search indexes.
That is, use the indexes in all configured directories."
  (notdeft-with-xapian
    (notdeft-xapian-list
     (notdeft-dcache--roots (notdeft-dcache))
     'keywords)))

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
	  (notdeft-xapian-index-all-dirs rebuild)
	  (setq notdeft-pending-reindex nil))
      (funcall reindex))
    (run-hooks 'notdeft-after-index-change-hook))
  (run-hooks 'notdeft-after-fs-change-hook))

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
reflect the file system changes.

For further work call `notdeft-global-do-pending'."
  (let ((changes (notdeft-compute-changes what things)))
    (when changes
      (let ((dirs (car changes)))
	;;(message "CHANGES: %S" dirs)
	(notdeft-global-do-pending
	 (lambda () (notdeft-xapian-index-dirs
		     (notdeft-expand-sparse-dirs dirs))))))))

;;; `notdeft-note-mode'

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
(defun notdeft-find-file (file &optional other switch)
  "Edit NotDeft note FILE.
Enable NotDeft note mode for the buffer for editing the FILE, if
it is non-nil, and return the file buffer. When OTHER is non-nil,
open the file in another window. When OTHER and SWITCH are both
non-nil, switch to the other window. Called interactively, query
for the FILE using the minibuffer."
  (interactive "FFind NotDeft file: ")
  (when file
    (let ((buffer (find-file-noselect file)))
      (prog1
          buffer
        (with-current-buffer buffer
          (notdeft-note-mode 1))
        (if other
            (if switch
                (switch-to-buffer-other-window buffer)
              (display-buffer buffer other))
          (switch-to-buffer buffer))))))

;;; File management commands

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

(defun notdeft-sub--new-file (&optional notename data title pfx)
  "Create a new note file as specified.
Save into a file with the specified NOTENAME, generating a name
if NOTENAME is nil. Save DATA as the note content. Use the
specified note TITLE, possibly affecting note naming or content.
With a PFX >= 4, query for a target directory; otherwise default
to the result of `notdeft-get-directory'. With a PFX >= 16, query
for a filename extension; otherwise default to
`notdeft-extension'. Return the name of the new file."
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
  (notdeft-sub--new-file nil nil title pfx))

;;;###autoload
(defun notdeft-new-file (pfx)
  "Create a new file quickly.
Create it with an automatically generated name.
With a prefix argument PFX, offer a choice of NotDeft
directories, when there is more than one of them.
With two prefix arguments, also offer a choice of filename
extensions when `notdeft-secondary-extensions' is non-empty.
Return the filename of the created file."
  (interactive "P")
  (notdeft-sub--new-file nil nil nil pfx))

(defun notdeft-note-buffer-p (&optional buffer)
  "Whether BUFFER is a NotDeft Note mode buffer.
Default to `current-buffer' if BUFFER is nil.
Return the buffer, or nil."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (when notdeft-note-mode
	buffer))))

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

(defvar notdeft-current-note-filename nil
  "A variable to define a note file as current.
If set to non-nil for the dynamic extent of the call, the value
of this variable will be the result of the function
`notdeft-current-filename'. Any value is assumed to name a note
file without further checking.")

(defun notdeft-current-filename (&optional note-only fail)
  "Return the current filename for file commands.
Return the value of `notdeft-current-note-filename' if it is
non-nil. In `notdeft-mode' use `notdeft-filename-at-point' to
determine note name. Otherwise return the current buffer's file
name, if any, requiring it to name a NotDeft note if NOTE-ONLY is
non-nil. Otherwise FAIL as specified for `notdeft-fail'."
  (cond
   (notdeft-current-note-filename)
   ((notdeft-buffer-p)
    (when (fboundp 'notdeft-filename-at-point)
      (funcall 'notdeft-filename-at-point fail)))
   ((when (or (not note-only) (notdeft-note-buffer-p))
      (buffer-file-name)))
   (t
    (funcall (notdeft-fail fail)
             (if note-only
                 "Not in a NotDeft note buffer"
               "Not in a file buffer")))))

(defun notdeft-buffer-title (&optional buffer note-only fail)
  "Return the NotDeft note title of BUFFER.
Default to the `current-buffer'. Return nil if the buffer has
no (non-whitespace) title. Require a NotDeft note BUFFER if
NOTE-ONLY is non-nil. If the buffer is not a note buffer then
FAIL as specified for `notdeft-fail'."
  (if (and note-only (not (notdeft-note-buffer-p)))
      (funcall (notdeft-fail fail) "Not in a NotDeft note buffer")
    (with-current-buffer (or buffer (current-buffer))
      (car (notdeft-parse-buffer)))))

;;;###autoload
(defun notdeft-delete-file (file &optional trash kill-buffer interactively)
  "Delete a NotDeft note FILE (or TRASH it).
When called interactively, delete the current buffer's file, and
prompt before proceeding. With one \\[universal-argument] prefix
also KILL-BUFFER the deleted file's buffer, if any. Unless two
\\[universal-argument] prefixes are given, ask `delete-file' to
TRASH the file; that should result in the file being moved to the
system's trashcan instead of being deleted, provided that
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
		;; This may result in the file being trashed rather
		;; than deleted, and we assume that any trashcan is
		;; not one of the `notdeft-directories' (or under
		;; them), which would be weird.
		(delete-file old-file trash)
		old-file)
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
(defun notdeft-move-into-subdir (old-file &optional force)
  "Move OLD-FILE into a subdirectory of the same name.
To nest more than one level (which is allowed but perhaps atypical),
invoke with a non-nil FORCE argument."
  (interactive
   (list (notdeft-current-filename t t)))
  (cond
   ((not old-file)
    nil)
   ((notdeft-file-sparse-p old-file)
    (message "Cannot move fixed-path file"))
   ((and (not force) (notdeft-file-in-subdir-p old-file))
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
      (message "Renamed as %S" new-file)))))

;;;###autoload
(defun notdeft-change-file-extension (old-file)
  "Change the filename extension of a NotDeft note.
Operate on the specified NotDeft note with the name OLD-FILE.
When called interactively use current note file, if any."
  (interactive
   (list (notdeft-current-filename t t)))
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
	  (message "Renamed as %S" new-file)))))))

;;;###autoload
(defun notdeft-rename-file (old-file &optional derive)
  "Rename the file OLD-FILE.
When called interactively, rename the current buffer file.
Default the new name to a content-derived file name (instead of
the old one) if DERIVE is non-nil, or if called interactively
with a \\[universal-argument] prefix argument."
  (interactive
   (list (notdeft-current-filename nil t)
         current-prefix-arg))
  (when old-file
    (if (notdeft-file-sparse-p old-file)
        (message "Cannot rename fixed-path file"))
    (let* ((old-name (notdeft-base-filename old-file))
	   (def-name
	     (or (when derive
		   (let ((title
			  (if (notdeft-buffer-p)
			      (notdeft-title-from-file-content old-file)
			    (notdeft-parse-title (buffer-string)))))
		     (and title (notdeft-title-to-notename title))))
		 old-name))
	   (new-file (notdeft-sub--rename-file old-file old-name def-name)))
      (when new-file
	(message "Renamed as %S" new-file)))))

(defun notdeft-plist-get (plist property default)
  "From PLIST get PROPERTY value, or DEFAULT value."
  (declare (pure t))
  (let ((x (plist-member plist property)))
    (if x (cadr x) default)))

(defun notdeft-plist-get-some (plist &rest properties)
  "From PLIST the first non-nil value of PROPERTIES.
Return nil if there is no such value."
  (declare (pure t))
  (cl-some (lambda (property)
             (plist-get plist property))
           properties))

(defun notdeft-plist-put (plist property value)
  "To PLIST put PROPERTY VALUE, with functional update.
Avoid duplication of the property entry."
  (declare (pure t))
  (let ((tail (plist-member plist property)))
    (when tail
      (let ((t-len (length tail)))
        (setq plist (append (butlast plist t-len) (cddr tail)))))
    (cons property (cons value plist))))

(defun notdeft-plist-put-multi (plist &rest entries)
  "To PLIST put property ENTRIES, with functional update.
For each entry pass property key and value as separate
arguments."
  (declare (pure t))
  (while entries
    (let ((k (car entries)))
      (setq entries (cdr entries))
      (let ((v (car entries)))
        (setq entries (cdr entries))
        (setq plist (notdeft-plist-put plist k v)))))
  plist)

(defvar notdeft-notename-history nil
  "History of `notdeft-read-notename' notenames.
These are without directory path or extension.")

(defvar notdeft-notename-filename-history nil
  "History of `notdeft-read-notename' notenames or filenames.
Thse are without directory path, but possibly with a file name
extension.")

(defun notdeft-read-notename (&rest options)
  "Prompt the user for a new notename.
OPTIONS may be given as a plist, including a PROMPT and an
INITIAL value. If INITIAL is nil then try using any filename of
OLD-FILE or OLD-BUF, or otherwise possibly USE-TITLE of OLD-BUF
to derive a notename. For recording history use the variable
`notdeft-notename-history', or with a non-nil ALLOW-EXT instead
use `notdeft-notename-filename-history'. Return the entered name,
or nil if no or empty input was given. ALLOW-EMPTY input only if
requested, otherwise insist on non-blank input."
  (let* ((allow-ext (plist-get options :allow-ext))
         (prompt
          (or (plist-get options :prompt)
              (format "Note name (%s): "
                      (if allow-ext
                          "optionally with extension"
                        "without extension"))))
         (initial
          (or
           (plist-get options :initial)
           (when-let ((pn (plist-get options :old-file))
                      (fn (file-name-nondirectory pn)))
             (if allow-ext fn (file-name-sans-extension fn)))
           (when-let ((buf (plist-get options :old-buf)))
             (if-let ((pn (buffer-file-name buf))
                      (fn (file-name-nondirectory pn)))
                 (if allow-ext fn (file-name-sans-extension fn))
               (when-let (((plist-get options :use-title))
                          (content
                           (with-current-buffer buf
                             (buffer-string)))
                          (title (notdeft-parse-title content)))
                 (notdeft-title-to-notename title))))))
         (history (if allow-ext
                      'notdeft-notename-filename-history
                    'notdeft-notename-history))
         (read (lambda ()
                 (notdeft-chomp-nullify
	          (read-string
                   prompt ;; PROMPT
                   initial ;; INITIAL-INPUT
	           (if initial
                       (cons history 1)
                     history) ;; HISTORY
	           nil ;; DEFAULT-VALUE
	           t ;; INHERIT-INPUT-METHOD
                   )))))
    (when initial
      (set history (cons initial (symbol-value history))))
    (let ((new-name
           (if (plist-get options :allow-empty)
               (funcall read)
             (let (name)
               (while (not name)
                 (setq name (funcall read)))
               name))))
      new-name)))

(defun notdeft-sub--rename-file (old-file old-name def-name)
  "Rename OLD-FILE with the OLD-NAME NotDeft name.
Query for a new name, defaulting to DEF-NAME. Use OLD-FILE's
filename extension in the new name. If the file was renamed,
return the new filename, and otherwise return nil."
  (let* ((new-name
          (notdeft-read-notename
           :prompt (concat "Rename " old-name " to (without extension): ")
           :initial def-name))
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

(defun notdeft-sub--move-file (old-file dest-dir &optional whole-dir mkdir)
  "Move the OLD-FILE note file into the DEST-DIR directory.
If OLD-FILE has its own subdirectory, then move the entire
subdirectory, but only if WHOLE-DIR is true. If WHOLE-DIR is the
symbol `ask', then ask for confirmation first. With a non-nil
argument MKDIR, create any missing target directory (one level
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
  "Previous file move or copy target NotDeft directory.
Local to a NotDeft mode buffer. Set to nil if `notdeft-move-file'
or `notdeft-import-buffer' has not been used to move or copy a
note.")

;;;###autoload
(defun notdeft-move-file (old-file &optional whole-dir)
  "Move the OLD-FILE under selected NotDeft root.
When called interactively move current buffer file, if any. Query
the user for a target from among `notdeft-directories'. Offer to
create the chosen NotDeft root directory if it does not already
exist. If the file resides in a subdirectory, move the entire
subdirectory, but require confirmation as a non-nil WHOLE-DIR
argument, a \\[universal-argument] prefix, or by asking. Moving
an external (non-Deft) file under a NotDeft root is also
allowed."
  (interactive
   (list (notdeft-current-filename nil t) current-prefix-arg))
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
	       (notdeft-sub--move-file old-file new-root (or whole-dir 'ask) t)))
	  (if (not moved-file)
	      (message "Did not move %S" old-file)
	    (setq notdeft-previous-target new-root)
	    (notdeft-changed--fs
	     'dirs (delete nil (list old-root new-root)))
	    (message "Moved %S under root %S" moved-file chosen-root))))))))

;;;###autoload
(defun notdeft-import-buffer (&optional old-buf other-window)
  "Import OLD-BUF content as a note under selected NotDeft root.
OLD-BUF must not be an existing NotDeft note buffer. If OLD-BUF
is nil then import `current-buffer' content. If the buffer is a
file buffer with a note file extension, then offer to use that
filename for the imported note, and in any case ask the user to
confirm the destination notename and extension. Query the user
for a target directory from among `notdeft-directories'. Offer to
create the chosen NotDeft root directory if it does not already
exist. Open any imported note for editing as a NotDeft note,
optionally in OTHER-WINDOW, and return its buffer object.
Otherwise return an error message. A \\[universal-argument]
prefix argument enables the OTHER-WINDOW option."
  (interactive
   (list nil current-prefix-arg))
  (let ((old-buf (or old-buf (current-buffer))))
    (if (not (and old-buf notdeft-directories (not (zerop (buffer-size old-buf)))))
        (message "Nothing or nowhere to import")
      (if (notdeft-note-buffer-p old-buf)
          (message "Buffer %S already a note buffer" (buffer-name old-buf))
        (let* ((old-file (buffer-file-name old-buf))
               (old-root (and old-file (notdeft-dir-of-file old-file))))
          (if old-root
              (message "File %S already under root %S" old-file old-root)
            (let* ((ext-re (notdeft-make-file-re))
                   (old-has-ext (when old-file
                                  (string-match-p ext-re old-file)))
                   (prompt (if old-has-ext
                               "Destination note name (optionally with file name extension): "
                             "Destination note name (without extension): "))
                   (read-name
                    (notdeft-read-notename
                     :prompt prompt
                     :old-file old-file
                     :old-buf old-buf
                     :allow-ext old-has-ext
                     :use-title t))
                   (new-name
                    (if (and old-has-ext
                             (string-match-p ext-re read-name))
                        read-name
                      (concat read-name "." (notdeft-read-extension))))
                   (root-choices
                    (if (not notdeft-previous-target)
		        notdeft-directories
	              (notdeft-list-prefer
	               notdeft-directories
	               (lambda (dir)
		         (notdeft-file-equal-p dir notdeft-previous-target)))))
	           (chosen-root
	            (notdeft-select-directory-from root-choices "Destination directory: " t t))
	           (new-root
	            (notdeft-canonicalize-root chosen-root))
                   (new-file (concat new-root new-name)))
	      (notdeft-ensure-root new-root)
              (with-current-buffer old-buf
	        (write-region nil nil new-file nil nil nil 'excl))
	      (setq notdeft-previous-target new-root)
	      (notdeft-changed--fs 'files (list new-file))
              (prog1
	          (notdeft-find-file new-file other-window other-window)
	        (message "Imported %S under root %S" new-file chosen-root)))))))))

;;;###autoload
(defun notdeft-archive-file (old-file &optional whole-dir)
  "Move OLD-FILE to a note archive.
When called interactively, archive the current buffer file, but
only if it is a note file. Archive the file under
`notdeft-archive-directory', under its NotDeft root directory. If
it resides in a subdirectory, archive the entire directory, but
require interactive confirmation unless WHOLE-DIR is non-nil or a
\\[universal-argument] prefix argument was given."
  (interactive
   (list (notdeft-current-filename t t)))
  (when old-file
    (if (notdeft-file-sparse-p old-file)
        (message "Cannot archive fixed-path file")
      (let ((old-root (notdeft-dir-of-file old-file)))
	(if (not old-root)
	    (message "Cannot archive non-Deft file")
	  (let* ((new-dir
		  (concat old-root
			  (file-name-as-directory notdeft-archive-directory)))
		 (moved-file
		  (notdeft-sub--move-file old-file new-dir (or whole-dir 'ask) t)))
	    (if (not moved-file)
		(message "Did not archive %S" old-file)
	      (notdeft-changed--fs 'dirs (list old-root))
	      (message "Archived %S into %S" moved-file new-dir))))))))

;;;###autoload
(defun notdeft-show-file-directory (file)
  "Show NotDeft directory of the note FILE."
  (interactive
   (list (notdeft-current-filename t t)))
  (when file
    (let ((dir (notdeft-dir-of-file file)))
      (if (not dir)
	  (message "Not on a NotDeft file")
	(message "%s" dir)))))

(defun notdeft-show-file-parse (file)
  "Show parse information for FILE.
When called interactively show it for the current note file."
  (interactive
   (list (notdeft-current-filename t t)))
  (when file
    (let ((res (with-temp-buffer
	         (insert-file-contents file)
	         (notdeft-parse-buffer))))
      (message "name=%S file=%S parse=%S"
	       (file-name-nondirectory file)
	       file res))))

;;;###autoload
(defun notdeft-reindex ()
  "Recreate all indexes for `notdeft-directories'.
A `notdeft-refresh' is normally sufficient, but this command
should help if the Xapian search index becomes corrupted for some
reason, as indexes are re-built from scratch."
  (interactive)
  (notdeft-global-do-pending nil t))

;;;###autoload
(defun notdeft-refresh (&optional reset)
  "Refresh or reset NotDeft state.
Refresh NotDeft state so that outside filesystem changes get
noticed. Also reset state to some extent if RESET is non-nil, or
if the command prefix \\[universal-argument] is given
interactively. Resetting may involve further interactive queries
regarding what state to clear. Invoke this command manually if
NotDeft files change outside of NotDeft mode and NotDeft note
minor mode (as toggled by the command `notdeft-mode' and the
command `notdeft-note-mode'), as such changes are not detected
automatically. Also invoke this if you change
`notdeft-directories' or `notdeft-sparse-directories'."
  (interactive "P")
  (run-hooks 'notdeft-pre-refresh-hook)
  (notdeft-dcache t)
  (when reset
    (run-hooks 'notdeft-reset-hook))
  (notdeft-changed--fs 'anything)
  (run-hooks 'notdeft-post-refresh-hook))

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
FILENAME is a non-directory filename, with an extension (it is
not necessarily unique). Returns the resolved path, or nil if
none was found."
  (let ((fn (notdeft-file-by-basename filename)))
    (if (not fn)
	(message "No NotDeft note %S" filename)
      (notdeft-find-file fn))
    fn))

(defun notdeft-ido-compread-file (files &optional prompt)
  "Present a choice of FILES with `ido-completing-read'.
Only present the non-directory component of each file. There may
be duplicates of the same non-directory name. If non-nil, use the
specified PROMPT. Return the path of the selected note file."
  ;; Ido has been a part of Emacs since version 22.
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

(defun notdeft-compread-file (files &optional prompt)
  "Present a choice of note FILES for selection.
If non-nil, use the specified PROMPT. Use
`notdeft-compread-file-function' for the selection. Return the
path of the selected note file, if any, or nil otherwise."
  (funcall notdeft-compread-file-function files prompt))

(defun notdeft-xapian-compread-search (&rest arguments)
  "Search for a file matching a Xapian query.
If a QUERY is provided in the keyword ARGUMENTS plist then use
it, and otherwise ask the user for a query, accounting for
`notdeft-xapian-query-history'. If EDIT is non-nil then offer
QUERY for interactive editing in any case. If there are any
matches in the query search results, present a choice list of
files with `notdeft-compread-file'. Return the path of the chosen
file, or nil if nothing was found. Optionally OPEN any selected
file with `notdeft-find-file'."
  (notdeft-with-xapian
    (let ((query (let ((query (plist-get arguments :query)))
                   (if (and query (not (notdeft-plist-get-some arguments :edit :rich)))
                       query
	             (notdeft-xapian-read-query (or query (plist-get arguments :initial-query)))))))
      (when query
	(let* ((notdeft-xapian-order-by (plist-get arguments :order-by))
               (notdeft-xapian-max-results
                (or notdeft-select-note-file-max-choices notdeft-xapian-max-results))
               (files (notdeft-xapian-search-all-dirs query)))
	  (when files
            (let ((file (notdeft-compread-file files)))
              (prog1
                  file
                (when (and file (plist-get arguments :open))
                  (notdeft-find-file file))))))))))

(defun notdeft-xapian-compread-search-find-file (&rest arguments)
  "Perform a Xapian search to find and open a file.
The ARGUMENTS are as for `notdeft-xapian-compread-search'."
  (apply #'notdeft-xapian-compread-search
         (notdeft-plist-put arguments :open t)))

(defvar notdeft-open-search-function #'notdeft-xapian-compread-search-find-file
  "Default function to use for searching and presenting results.
The function must accept any number of keyword arguments, one of
which may be a QUERY search string. The return value is
undefined. If a QUERY is not provided, then the function must ask
for what to search. Other arguments may be provided, and
implementations shall choose which of them to honor. Notably,
RICH is a boolean flag that is a hint about whether the function
may interactively offer non-default query or result presentation
options. This function only defines the default way of querying
for and presenting results, and some commands (e.g.,
`notdeft-open-query' and `notdeft-search-find-file') always
present their results in a specific way without consulting this
setting.")

(defvar notdeft-search-find-file-function #'notdeft-xapian-compread-search-find-file
  "Function to use by default for search and `find-file'.
The function is required to call `notdeft-find-file' for any
selected file. Its arguments and return value are the same as
described for `notdeft-open-search-function'.")

(defvar notdeft-select-note-file-function #'notdeft-xapian-compread-search
  "Function for selecting a note file.
This variable defines the behavior of `notdeft-select-note-file',
which is used generally when another operation needs a note file
to be selected, probably interactively. The function must accept
at least the keyword arguments QUERY:string-or-null-p and
RICH:booleanp, where QUERY is an optional and possibly applicable
search string, and RICH consents to offers of non-default
behavior. The function must return the full pathname of the
selected file, or nil if none. For example implementations, see
`notdeft-compread-select-note-file' and
`notdeft-xapian-compread-search'.")

(defun notdeft-compread-select-note-file (&rest arguments)
  "Offer a choice list of all notes.
Return a file name for the selected note. Return nil if there are
no notes from which to select. Any ARGUMENTS are ignored."
  (ignore arguments)
  (let ((files (notdeft-make-note-file-list))
        (prompt "NotDeft note: "))
    (notdeft-compread-file files prompt)))

(defun notdeft-select-note-file (&rest arguments)
  "Let the user choose a note file.
Allow choosing from all notes in the collection. Return the file
name of the chosen note, or nil for none. Optionally offer a more
RICH mode of selection, if the keyword ARGUMENTS include that
option. Optionally honor related options such as INITIAL-QUERY."
  (let ((rich (plist-get arguments :rich)))
    (if (and notdeft-select-note-file-function
	     (or rich notdeft-select-note-file-by-search))
        (apply notdeft-select-note-file-function arguments)
      (notdeft-compread-select-note-file))))

(defun notdeft-string-from-region (&optional no-region)
  "Get a string from any active region.
If NO-REGION is non-nil then return nil regardless of whether
there is a region."
  (when (and mark-active (not no-region))
    (buffer-substring-no-properties
     (region-beginning) (region-end))))

;;;###autoload
(defun notdeft-open-search (&rest arguments)
  "Execute a search and present results in the default way.
The default way is defined by `notdeft-open-search-function'.
Pass keyword ARGUMENTS to that function and return whatever it
returns. When called interactively have \\[universal-argument]
prefix argument enable the RICH option, and possibly use any
active region as INITIAL-QUERY input if prompting for a search
query."
  (interactive
   (list :rich current-prefix-arg :initial-query (notdeft-string-from-region)))
  (apply notdeft-open-search-function arguments))

;;;###autoload
(defun notdeft-search-find-file (&rest arguments)
  "Execute a search in order to `find-file' to open.
The default way is defined by
`notdeft-search-find-file-function'. Pass ARGUMENTS to that
function and return whatever it returns. If called interactively
with a \\[universal-argument] prefix include a non-nil RICH value
among the ARGUMENTS, and possibly use any active region as
INITIAL-QUERY input if prompting for a search query."
  (interactive
   (list :rich current-prefix-arg :initial-query (notdeft-string-from-region)))
  (apply notdeft-search-find-file-function arguments))

(defalias 'notdeft-query-select-find-file
  #'notdeft-search-find-file
  "Deprecated. Use `notdeft-search-find-file'.")

;;;###autoload
(defun notdeft-xapian-ido-search-find-file (&rest arguments)
  "Search and interactively select and open file.
Implement `notdeft-search-find-file-function' in terms of Xapian
and Ido, accepting ARGUMENTS as documented. When called
interactively have \\[universal-argument] imply RICH mode, and
possibly use any active region as INITIAL-QUERY if prompting for
a search query."
  (interactive
   (list :rich current-prefix-arg :initial-query (notdeft-string-from-region)))
  (let ((notdeft-compread-file-function #'notdeft-ido-compread-file))
    (apply #'notdeft-xapian-compread-search-find-file arguments)))

;;;###autoload
(defun notdeft-lucky-find-file (&rest arguments)
  "Open the highest-ranked note matching a search query.
Accept ARGUMENTS as documented for
`notdeft-search-find-file-function', but only consider QUERY and
ORDER-BY parameters. If called interactively, read a search query
interactively, accounting for `notdeft-xapian-query-history' and
any selected region. With a \\[universal-argument] prefix
argument choose the most recent matching note instead of the
highest-ranked one. Open the file directly, without switching to
any `notdeft-buffer'."
  (interactive
   (let ((by-time (equal current-prefix-arg '(4)))
	 (no-region (equal current-prefix-arg '(16))))
     (list :query (notdeft-xapian-read-query
                   (notdeft-string-from-region no-region))
           :order-by (and by-time 'time))))
  (let* ((query (plist-get arguments :query))
         (notdeft-xapian-order-by (plist-get arguments :order-by))
	 (notdeft-xapian-max-results 1)
	 (files (notdeft-xapian-search-all-dirs query)))
    (if (not files)
	(message "No matching notes found")
      (notdeft-find-file (car files)))))

;;;###autoload
(defun notdeft-list-files-by-query (query)
  "Return a list of files matching Xapian QUERY.
Like `notdeft-xapian-search-all-dirs' but autoloadable, and
returns no results if there is no `notdeft-xapian-program'."
  (when notdeft-xapian-program
    (notdeft-xapian-search-all-dirs query)))

(defun notdeft-string-as-phrase-query (str)
  "Turn STR into a phrase query."
  (let* ((str (downcase str))
	 (str (replace-regexp-in-string "\"" "" str))
	 (str (concat "\"" str "\"")))
    str))

(defun notdeft-search-for-title (title &optional as-phrase)
  "Query for the specified TITLE.
Optionally execute the search as a phrase search if AS-PHRASE is
non-nil."
  (when title
    (let ((title (notdeft-chomp title)))
      (unless (string-equal title "")
        (notdeft-open-search
         :query (if as-phrase
                    (notdeft-string-as-phrase-query title)
                  title))))))

;;;###autoload
(defun notdeft-search-for-note-title (buffer &optional as-phrase)
  "Query for the title of the note in BUFFER.
Use the title of the `current-buffer' if called interactively. Do
nothing if there is no (non-empty) title. Optionally execute
AS-PHRASE search, also when called interactively with a
\\[universal-argument] prefix argument."
  (interactive
   (list (current-buffer) current-prefix-arg))
  (let ((title (notdeft-buffer-title buffer nil t)))
    (notdeft-search-for-title title as-phrase)))

(provide 'notdeft)

(run-hooks 'notdeft-load-hook)

;;; notdeft.el ends here
