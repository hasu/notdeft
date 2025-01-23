;;; notdeft-org9.el --- Org link support for NotDeft notes  -*- lexical-binding: t; -*-

;; Author: Tero Hasu <tero@hasu.is>
;; See end of file for licensing information.

;;; Commentary:
;; Support for "deft:" and "notdeft:" links for `org-mode' version 9.
;; The `org-link-set-parameters' API is available since Org version 9,
;; in the `org' feature.
;;
;; Suggested use:
;;  (eval-after-load 'org (lambda () (require 'notdeft-org9)))

;;; Code:

(require 'org)

;; These are autoloadable, but declare them for byte-compilation as we
;; are avoiding requiring `notdeft-org' (and `notdeft') here.
(declare-function notdeft-org-open-deft-link "notdeft-org")
(declare-function notdeft-org-complete-deft-link "notdeft-org")
(declare-function notdeft-org-open-notdeft-link "notdeft-org")
(declare-function notdeft-org-store-notdeft-link "notdeft-org")

(org-link-set-parameters
 "deft"
 :follow #'notdeft-org-open-deft-link
 :complete #'notdeft-org-complete-deft-link)

(org-link-set-parameters
 "notdeft"
 :follow #'notdeft-org-open-notdeft-link
 :store #'notdeft-org-store-notdeft-link)

(provide 'notdeft-org9)

;;; notdeft-org9.el ends here

;; NotDeft, a note manager for Emacs
;; Copyright (C) 2017-2025  Tero Hasu
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
