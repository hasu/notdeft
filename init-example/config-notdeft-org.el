;; Org mode "deft:" and "notdeft:" link support.
(require 'notdeft-org9)

(define-key org-mode-map (kbd "C-c h") #'notdeft-org-search-for-heading)
(define-key org-mode-map (kbd "C-c i") #'notdeft-org-insert-notdeft-link-from-history)
(define-key org-mode-map (kbd "C-c n") #'notdeft-org-link-new-file)
(define-key org-mode-map (kbd "C-c x") #'notdeft-org-link-existing-note)
