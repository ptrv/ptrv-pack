;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown-conf.el

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

;; (setq-default markdown-command
;;               (concat
;;                "pandoc -S -s --self-contained -f markdown -t html5 --css="
;;                markdown-css-path
;;                " "))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

(setq markdown-css-path (expand-file-name
                         (concat
                          (file-name-directory
                           (or load-file-name (buffer-file-name)))
                          "../etc/css/markdown.css")))
