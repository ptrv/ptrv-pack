(live-add-pack-lib "pandoc-mode")
(load "pandoc-mode")

(add-hook 'markdown-mode-hook 'turn-on-pandoc)
