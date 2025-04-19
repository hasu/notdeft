;; Full path of "notdeft-xapian" executable.
(let ((x
       (let ((default-directory
	       (file-name-directory
		(file-truename (locate-library "notdeft")))))
	 (file-truename "xapian/notdeft-xapian"))))
  (setq notdeft-xapian-program x))

;; As an alternative to the above, you can try building and
;; configuring "notdeft-xapian" on demand, but on most systems this
;; will not succeed out of the box.
;(add-hook 'notdeft-load-hook 'notdeft-xapian-make-program-when-uncurrent)

;; Potentially enable `notdeft-note-mode' for note editing major
;; modes. You may want to register with different hooks here if you
;; configure different file name `notdeft-extension' or
;; `notdeft-secondary-extensions'.
(add-hook 'org-mode-hook 'notdeft-note-mode-enable)

;; Org mode "deft:" and "notdeft:" link support.
(eval-after-load 'org
  (lambda ()
    (let ((ver (ignore-errors
		 (car (version-to-list org-version)))))
      (when (and ver (>= ver 9))
        (require 'notdeft-org9)))))

;; Add global bindings for NotDeft. To do that, bind a custom keymap
;; that inherits from NotDeft's, one that we can use to override and
;; add to the predefined set of bindings.
(require 'notdeft-global)
(defvar my-notdeft-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(l)] #'notdeft-org-link-existing-note) ;; l for link
    (define-key map [(n)] #'notdeft-org-link-new-file) ;; n for new
    (define-key map [(s)] #'org-store-link) ;; s for store
    (define-key map [(S)] #'notdeft-org-store-deft-link) ;; s for store
    (set-keymap-parent map 'notdeft-global-map)
    map)
  "Custom keymap for accessing NotDeft functionality.

\\{my-notdeft-global-map}")
(fset 'my-notdeft-global-map my-notdeft-global-map)
(global-set-key [f6] 'my-notdeft-global-map)

;; Add Org-specific bindings that are also usable in a NotDeft buffer.
(add-hook 'notdeft-load-hook
  (lambda ()
    (define-key notdeft-mode-map (kbd "C-c S")
      #'notdeft-org-store-deft-link)))

(require 'hydra nil t)
(when (featurep 'hydra)
  ;; Augment `notdeft-mode' bindings with a hydra.
  (autoload 'notdeft-mode-hydra/body "notdeft-mode-hydra" nil t)
  (add-hook 'notdeft-load-hook
    (lambda ()
      (define-key notdeft-mode-map (kbd "C-c h")
	#'notdeft-mode-hydra/body)))

  ;; Augment the global NotDeft keymap with a hydra also.
  (autoload 'notdeft-global-hydra/body "notdeft-global-hydra" nil t)
  (define-key my-notdeft-global-map [(h)] #'notdeft-global-hydra/body))

(require 'ivy nil t)
(when (featurep 'ivy)
  ;; Do minibuffer note selection by search and then Ivy choice list.
  (require 'notdeft-ivy)
  (add-to-list 'ivy-re-builders-alist
	       '(notdeft-ivy-compread-file . ivy--regex-ignore-order))
  (setq notdeft-compread-file-function 'notdeft-ivy-compread-file))
