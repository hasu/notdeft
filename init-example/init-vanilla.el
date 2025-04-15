;;
;; A test configuration for NotDeft. This one is relatively "vanilla"
;; in that it doesn't use straight.el or `use-package', and that it
;; doesn't incorporate installation of NotDeft itself. We assume that
;; NotDeft sources have already been unpacked. For installing
;; dependencies and generating autoloads and byte compilation and
;; Xapian executable compilation the "config-notdeft-package.el" file
;; is used by default, but an alternative "user-local-init.el" file
;; can be created to do those things if desired.
;;
;; Example usage:
;;
;;   PROFILE=vanilla ACTIONS=all emacs -Q -L . -l main
;;

;; We try loading "user-local-init" for any alternative initial setup.
(unless (load "user-local-init" t)
  (load "config-notdeft-package"))

(require 'notdeft-autoloads)

(load "config-notdeft-xapian")

(load "config-notdeft-note-mode")

(eval-after-load 'org
  (lambda ()
    (load "config-notdeft-org")))

(load "config-notdeft-global-map")

;; The `notdeft-transient' package is especially picky about its
;; dependencies, but if it is installed then bind a command key.
(when (require 'notdeft-transient-autoloads nil t)
  (define-key notdeft-global-map (kbd "a") #'notdeft-search))

;; Do minibuffer note selection by search and then Ivy choice list.
(when (require 'notdeft-ivy-autoloads nil t)
  (require 'ivy) ;; for `ivy-re-builders-alist'
  (add-to-list 'ivy-re-builders-alist
	       '(notdeft-ivy-compread-file . ivy--regex-ignore-order))
  (setq notdeft-compread-file-function #'notdeft-ivy-compread-file))

(setq notdeft-open-query-function #'notdeft-mode-open-query)
