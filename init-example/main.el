;;
;; A program for testing NotDeft installation.
;;
;; Usage:
;;
;;   PROFILE=<profile> ACTIONS=<action0,action1,...> emacs -Q -L . -l main
;;

(defconst profile (or (getenv "PROFILE")
                      (error "No installation PROFILE specified"))
  "Installation PROFILE.")

(defvar actions (when-let ((s (getenv "ACTIONS")))
                  (split-string s ","))
  "List of optional installation ACTIONS to perform.")

;; Just set `user-emacs-directory', `user-init-file', and
;; `native-comp-eln-load-path' directly, similar to the way
;; with-emacs.sh does it, so that there is no need to rely on the
;; rather new --init-directory option.
(setq user-emacs-directory
      (file-name-as-directory (expand-file-name (concat "emacs-run-" profile))))

;; Use a dedicated "init.el" file for customizations (e.g.,
;; `custom-set-variables') so that source files do not get cluttered
;; by those.
(setq user-init-file (expand-file-name "init.el" user-emacs-directory))

(when (boundp 'native-comp-eln-load-path)
  (push (expand-file-name "eln-cache" user-emacs-directory) native-comp-eln-load-path))

(defun add-load-path (x)
  (add-to-list 'load-path (expand-file-name x)))

(pcase-exhaustive profile
  ("vanilla"
   (add-load-path "..")
   (add-load-path "../ivy")
   (add-load-path "../transient")
   (load "init-vanilla"))
  ("use-package"
   (add-load-path "..")
   (add-load-path "../ivy")
   (add-load-path "../transient")
   (load "init-use-package"))
  ("straight"
   (load "init-straight")))
