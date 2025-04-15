;; Set up global key bindings for NotDeft. To do that we load a
;; predefined `notdeft-global-map', bind it to some prefix key, and
;; add some bindings to it for commands that are not bound to a key by
;; default. In order to add to the keymap we must load the containing
;; `notdeft-global' feature, which just defines the keymap for
;; NotDeft, without immediately pulling in all of NotDeft.
(require 'notdeft-global)

(global-set-key [f6] 'notdeft-global-map)

(define-key notdeft-global-map (kbd "t") #'notdeft-search-for-note-title)
