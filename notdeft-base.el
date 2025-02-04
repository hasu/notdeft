;;; notdeft-base.el --- Common definitions for NotDeft  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; See end of file for licensing information.

;;; Commentary:
;; Commonly required definitions for NotDeft. Should be kept small as
;; most NotDeft features load it eagerly. Also defines the root
;; customization group for NotDeft.

(require 'cl-lib)

;;; Code:

(defgroup notdeft nil
  "Emacs NotDeft mode."
  :group 'local)

(defcustom notdeft-directories '("~/.deft/")
  "NotDeft directories.
Each element must be a directory path string.
Each named directory may or may not exist."
  :type '(repeat string)
  :safe (lambda (lst) (cl-every #'stringp lst))
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
  "Default NotDeft file extension.
May not be nil, and is without the preceding dot character. This
file name extension is given to new note files by default."
  :type 'string
  :safe #'stringp
  :group 'notdeft)

(defcustom notdeft-secondary-extensions nil
  "Additional NotDeft file extensions.
NotDeft note files may have these extensions even though
`notdeft-extension' is the default one for new notes."
  :type '(repeat string)
  :safe (lambda (lst) (cl-every #'stringp lst))
  :group 'notdeft)

(defcustom notdeft-allow-org-property-drawers t
  "Whether to recognize Org property drawers.
If non-nil, then buffer-level Org \"PROPERTIES\" drawers are
treated as being part of the header of the note, which in
practice means that they are treated the same as comments."
  :type 'boolean
  :safe #'booleanp
  :group 'notdeft)

(provide 'notdeft-base)

;;; notdeft-base.el ends here

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
