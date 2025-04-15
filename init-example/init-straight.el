;;
;; A test configuration for NotDeft. This one makes use of straight.el
;; for NotDeft installation and `use-package' forms for organizing the
;; configuration. The bootstrapping of straight.el is beyond the scope
;; of this file, and that is left for a "user-local-init.el" file.
;;
;; Example usage:
;;
;;   PROFILE=straight emacs -Q -L . -l main
;;

;; This is now required, since something must make the straight.el
;; package available for use. Bootstrapping code for the package can
;; be found from https://github.com/radian-software/straight.el
(load "user-local-init")

(setq straight-use-package-by-default t
      use-package-always-ensure t
      use-package-always-defer t)

(straight-use-package 'use-package)

;; Do not clone Org as it is very large.
(straight-use-package '(org :type built-in))

(defconst notdeft-repo (expand-file-name ".."))

(straight-use-package
 `(notdeft
   :local-repo ,notdeft-repo
   :files ("notdeft*.el" "xapian")))

(straight-use-package
 `(notdeft-ivy
   :local-repo ,notdeft-repo
   :files ("ivy/notdeft-*.el")))

(straight-use-package
 `(notdeft-transient
   :local-repo ,notdeft-repo
   :files ("transient/notdeft-*.el")))

(use-package notdeft
  :init
  (load "config-notdeft-note-mode")
  (load "config-notdeft-global-map")
  (setq notdeft-open-query-function #'notdeft-mode-open-query)
  :config
  ;; In this configuration we build the C++ program on demand instead
  ;; of assuming that it has been done ahead of time. We also do not
  ;; need to `load' "config-notdeft-xapian" for this reason.
  (notdeft-xapian-make-program-when-uncurrent))

(use-package notdeft-ivy
  :init
  (setq notdeft-compread-file-function #'notdeft-ivy-compread-file)
  :config
  (add-to-list 'ivy-re-builders-alist '(notdeft-ivy-compread-file . ivy--regex-ignore-order)))

;; Since we are using straight.el and its repositories we assume that
;; a recent enough version of transient can be installed and used
;; instead of any built-in version.
(use-package notdeft-transient
  :init
  (define-key notdeft-global-map (kbd "a") #'notdeft-search))

(use-package org
  :config
  (load "config-notdeft-org"))
