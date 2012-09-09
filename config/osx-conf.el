(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

(if (window-system)
    (progn
      (add-to-list 'default-frame-alist '(font . "Inconsolata-16"))
      (set-frame-size (selected-frame) 120 52)
      (set-frame-position (selected-frame) 100 24)))
