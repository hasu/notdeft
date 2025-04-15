;;
;; A test configuration for NotDeft. This one makes use of
;; `use-package' forms for organizing the configuration. It is
;; otherwise much like "init-vanilla.el".
;;
;; Example usage:
;;
;;   PROFILE=use-package ACTIONS=all emacs -Q -L . -l main
;;

;; We try loading "user-local-init" for any alternative initial setup.
(unless (load "user-local-init" t)
  (load "config-notdeft-package"))

(use-package notdeft
  :defer t
  :init
  ;; Since in this case `notdeft' has not been installed as a package
  ;; we need to explicitly `require' or declare autoloads here in
  ;; order to have any, since no package manager does it for us.
  (require 'notdeft-autoloads)
  (load "config-notdeft-xapian")
  (load "config-notdeft-note-mode")
  (load "config-notdeft-global-map")
  (setq notdeft-open-query-function #'notdeft-mode-open-query))

(use-package notdeft-ivy
  :if (require 'notdeft-ivy-autoloads nil t)
  :defer t
  :init
  (setq notdeft-compread-file-function #'notdeft-ivy-compread-file)
  :config
  ;; The `ivy-re-builders-alist' variable is defined since `ivy' is required by `notdeft-ivy'.
  (add-to-list 'ivy-re-builders-alist '(notdeft-ivy-compread-file . ivy--regex-ignore-order)))

(use-package notdeft-transient
  :if (require 'notdeft-transient-autoloads nil t)
  :defer t
  :init
  (define-key notdeft-global-map (kbd "a") #'notdeft-search))

(use-package org
  :defer t
  :config
  (load "config-notdeft-org"))
