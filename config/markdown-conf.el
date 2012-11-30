(require 'markdown-mode)

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

(setq markdown-css-path (expand-file-name
                         (concat
                          (file-name-directory
                           (or load-file-name (buffer-file-name)))
                          "../etc/css/markdown.css")))
