;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pandoc-conf.el

(autoload 'turn-on-pandoc "pandoc-mode" nil t)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(add-hook 'markdown-mode-hook 'turn-on-pandoc)

(custom-set-variables
 '(pandoc-binary "/usr/local/bin/pandoc"))

(add-to-list 'auto-mode-alist '("\\.text$" .  markdown-mode))
