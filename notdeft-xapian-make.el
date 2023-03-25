;;; notdeft-xapian-make.el --- Xapian backend auto-installer  -*- lexical-binding: t; -*-

;; Author: MaxSt <max@stoerchle.at>
;; Author: Tero Hasu <tero@hasu.is>
;; See end of file for licensing information.

;;; Commentary:
;; Functionality for compiling the C++ code for the NotDeft Xapian
;; backend, automatically or otherwise.
;;
;; Instead of setting the `notdeft-xapian-program' variable yourself,
;; you may instead load this `notdeft-xapian-make' feature to have the
;; program built and configured automatically. You may additionally
;; need to set the `notdeft-xapian-program-compile-command-format' to
;; something that produces a suitable compiler invocation for your
;; platform.
;;
;; Suggested use:
;;  (add-hook 'notdeft-load-hook 'notdeft-xapian-make-program-when-uncurrent)

(require 'notdeft-base)

;;; Code:

(defcustom notdeft-xapian-program-compile-command-format
  "c++ -o %s %s -std=c++11 -Wall `pkg-config --cflags --libs tclap` `xapian-config --cxxflags --libs`"
  "Compilation shell command.
Can be a `format' string with two \"%s\" directives, or a
function of two arguments, with the first directive or argument
specifying the executable path for the program, and the second
specifying the C++ source file for the notdeft-xapian program. If
it is a function, it should do any necessary shell escaping of
the arguments."
  :type '(choice
	  (string :tag "Format string")
	  (function :tag "Function"))
  :group 'notdeft)

(defcustom notdeft-xapian-program-install-path
  "notdeft-xapian"
  "Path for the notdeft-xapian executable to build.
If the path is not absolute, it is considered relative to
`notdeft-xapian-home'."
  :type 'string
  :safe #'stringp
  :group 'notdeft)

(defvar notdeft-xapian-home
  (expand-file-name "xapian/"
		    (file-name-directory
		     (file-truename (locate-library "notdeft"))))
  "Directory path for notdeft-xapian sources.
Must specify an absolute path.")

(defvar notdeft-xapian-compile-buffer-name "*Compile notdeft-xapian*"
  "Name of the buffer used for compiling notdeft-xapian.")

(defun notdeft-xapian-program-current-p (&optional program)
  "Whether the notdeft-xapian PROGRAM is current.
It is uncurrent if it does not exist as an executable, or if its
source file is newer. PROGRAM defaults to
`notdeft-xapian-program-install-path'."
  (let ((exe-file (expand-file-name
		   (or program notdeft-xapian-program-install-path)
		   notdeft-xapian-home)))
    (when (file-executable-p exe-file)
      (let ((cxx-file (expand-file-name
		       "notdeft-xapian.cc"
		       notdeft-xapian-home)))
	(when (file-exists-p cxx-file)
	  (not (time-less-p
		(nth 5 (file-attributes exe-file))
		(nth 5 (file-attributes cxx-file)))))))))

;;;###autoload
(defun notdeft-xapian-compile-program (&optional program)
  "Compile the notdeft-xapian program.
Use notdeft-xapian sources in `notdeft-xapian-home', and build
the PROGRAM, which defaults to `notdeft-xapian-program-install-path'.
On success, return the path of the built executable."
  (interactive)
  (unless (file-directory-p notdeft-xapian-home)
    (error "Cannot locate notdeft-xapian sources"))
  (let* ((exe-file (expand-file-name
		    (or program notdeft-xapian-program-install-path)
		    notdeft-xapian-home))
	 (cxx-file (expand-file-name
		    "notdeft-xapian.cc"
		    notdeft-xapian-home))
	 (compile-command
	  (if (functionp notdeft-xapian-program-compile-command-format)
	      (funcall notdeft-xapian-program-compile-command-format
		       exe-file cxx-file)
	    (format notdeft-xapian-program-compile-command-format
		    (shell-quote-argument exe-file)
		    (shell-quote-argument cxx-file))))
	 (buffer (get-buffer-create notdeft-xapian-compile-buffer-name)))
    (pop-to-buffer notdeft-xapian-compile-buffer-name)
    (let ((exit-code
	   (call-process
	    "sh" nil buffer t "-c" compile-command)))
      (unless (zerop exit-code)
	(error "Compilation of notdeft-xapian failed: %s (%d)"
	       compile-command exit-code))
      (unless (file-executable-p exe-file)
	(error (concat "Compilation of notdeft-xapian failed: "
		       "Executable %S not created")
	       exe-file))
      (message "Compilation of notdeft-xapian succeeded: %S" exe-file)
      exe-file)))

(defun notdeft-xapian-make-program (&optional force)
  "Compile notdeft-xapian program.
Only do that if the source directory `notdeft-xapian-home'
exists, and the target path `notdeft-xapian-program-install-path'
is non-nil. In that case generate the executable with the target
path, but only if any existing executable appears to be
uncurrent, or if the FORCE flag is non-nil. Return the absolute
target path if it is known, even if the program could not be
compiled."
  (when notdeft-xapian-program-install-path
    (when (and notdeft-xapian-home
	       (file-directory-p notdeft-xapian-home))
      (let ((exe-file (expand-file-name
		       notdeft-xapian-program-install-path
		       notdeft-xapian-home)))
	(when (or force (not (notdeft-xapian-program-current-p exe-file)))
	  (notdeft-xapian-compile-program exe-file))
	exe-file))))

(eval-when-compile
  (defvar notdeft-xapian-program))

;;;###autoload
(defun notdeft-xapian-make-program-when-uncurrent ()
  "Compile notdeft-xapian program when it is uncurrent.
Do that as for `notdeft-xapian-make-program', but fail silently
if compilation fails. Set `notdeft-xapian-program' to the
program's absolute path, or to nil if the program does not exist
even after any compilation attempt."
  (setq notdeft-xapian-program
	(let ((exe-file
	       (ignore-errors
		 (notdeft-xapian-make-program nil))))
	  (when (and exe-file (file-executable-p exe-file))
	    exe-file))))

(provide 'notdeft-xapian-make)

;;; notdeft-xapian-make.el ends here

;; NotDeft, a note manager for Emacs
;; Copyright (C) 2020  Tero Hasu
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
;; Copyright (C) 2020  MaxSt <max@stoerchle.at>
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
