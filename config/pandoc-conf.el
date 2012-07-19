(live-add-pack-lib "pandoc-mode")
(load "pandoc-mode")

(add-hook 'markdown-mode-hook 'turn-on-pandoc)

(custom-set-variables
 '(pandoc-binary "/usr/local/bin/pandoc"))

(add-to-list 'auto-mode-alist '("\\.text" .  markdown-mode))
