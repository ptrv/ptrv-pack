(add-to-list 'auto-mode-alist '("\\.markdown$" .  markdown-mode))

(setq markdown-css-path (expand-file-name
                         (concat
                          (file-name-directory buffer-file-name)
                          "../etc/css/markdown.css")))
