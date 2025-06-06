;; You can configure building and configuring the "notdeft-xapian"
;; executable on demand, but on most systems this will not succeed out
;; of the box, and so please check that
;; `notdeft-xapian-program-compile-command-format' is defined
;; correctly for your system.
(add-hook 'notdeft-load-hook 'notdeft-xapian-make-program-when-uncurrent)

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
    (define-key map [(S)] #'notdeft-org-store-note-deft-link) ;; S for store
    (set-keymap-parent map 'notdeft-global-map)
    map)
  "Custom keymap for accessing NotDeft functionality.

\\{my-notdeft-global-map}")
(fset 'my-notdeft-global-map my-notdeft-global-map)
(global-set-key [f6] 'my-notdeft-global-map)

(defvar notdeft-mode-load-hook nil
  "Hook run immediately after `notdeft-mode' feature load.")

(eval-after-load 'notdeft-mode
  (lambda ()
    (run-hooks 'notdeft-mode-load-hook)))

;; Add Org-specific bindings that are also usable in a NotDeft buffer.
(add-hook 'notdeft-mode-load-hook
          (lambda ()
            (define-key notdeft-mode-map (kbd "C-c S")
              #'notdeft-org-store-note-deft-link)))

(when (require 'hydra nil t)
  ;; Augment `notdeft-mode' bindings with a hydra.
  (autoload 'notdeft-mode-hydra/body "notdeft-mode-hydra" nil t)
  (add-hook 'notdeft-mode-load-hook
    (lambda ()
      (define-key notdeft-mode-map (kbd "C-c h")
	#'notdeft-mode-hydra/body)))

  ;; Augment the global NotDeft keymap with a hydra also.
  (autoload 'notdeft-global-hydra/body "notdeft-global-hydra" nil t)
  (define-key my-notdeft-global-map [(h)] #'notdeft-global-hydra/body))

(if (not (require 'ivy nil t))
    ;; Use Ido if Ivy is not available.
    (setq notdeft-compread-file-function 'notdeft-ido-compread-file)
  ;; Do minibuffer note selection by search and then Ivy choice list.
  (require 'notdeft-ivy)
  (add-to-list 'ivy-re-builders-alist
	       '(notdeft-ivy-compread-file . ivy--regex-ignore-order))
  (setq notdeft-compread-file-function 'notdeft-ivy-compread-file))
