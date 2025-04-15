;;
;; Configures the Emacs package system, and optionally installs
;; NotDeft and its dependencies.
;;

(require 'package)

(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(when (member "melpa" actions)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(when (member "all" actions)
  (push "packages" actions)
  (push "notdeft" actions)
  (unless (fboundp 'use-package)
    (push "use-package" actions)))

(when (member "packages" actions)
  (package-refresh-contents)
  (package-install 'ivy)
  ;; If the package is built-in then this will not tend to do
  ;; anything, and it may be necessary to do the upgrade manually from
  ;; "Manage Emacs Packages".
  (package-install 'transient)
  (when (member "use-package" actions)
    (package-install 'use-package)))

(when (member "notdeft" actions)
  (require 'notdeft-install)
  (notdeft-install (member "force" actions)))
