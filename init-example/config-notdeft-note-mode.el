(if (member "alt-enable" actions)
    (add-hook 'org-mode-hook 'notdeft-note-mode-enable)

  (defun run-local-variables-mode-hooks ()
    "Run hooks for `major-mode' with locals set.
Like `run-mode-hooks', but run later, with any buffer and
directory local variables set."
    (run-hooks (intern (concat (symbol-name major-mode)
			       "-local-variables-hook"))))

  (add-hook 'hack-local-variables-hook 'run-local-variables-mode-hooks)

  (defcustom notdeft-note-mode-auto-enable nil
    "Whether to enable NotDeft Note minor mode for a buffer."
    :type 'boolean
    :safe 'booleanp)

  (make-variable-buffer-local 'notdeft-note-mode-auto-enable)

  (defun default-notdeft-hook ()
    "Conditionally enable `notdeft-note-mode'.
Enable when the buffer local variable
`notdeft-note-mode-auto-enable' is set to a non-nil value."
    (when notdeft-note-mode-auto-enable
      (notdeft-note-mode 1)))

  ;; Have Org mode files respect the flag. A hook like this should be
  ;; set for all NotDeft note file types, and no others.
  (add-hook 'org-mode-local-variables-hook 'default-notdeft-hook)

  (defun my-notdeft-add-directory-local-variables ()
    "Add `notdeft-note-mode-auto-enable' flag.
Add it for all `notdeft-directories'."
    (interactive)
    (require 'notdeft-base) ;; for `notdeft-directories'
    (dolist (dir notdeft-directories)
      (make-directory dir t)
      (let ((default-directory dir))
        (add-dir-local-variable nil 'notdeft-note-mode-auto-enable t)))))
