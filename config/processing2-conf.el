(live-add-pack-lib "processing2-emacs")

(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(setq processing-location "~/applications/processing-2.0/processing-java")

(yas-load-directory (concat (live-pack-lib-dir) "processing2-emacs/snippets"))

(add-hook 'processing-mode-hook
          (lambda ()
            (setq ac-sources
                  '(ac-source-yasnippet
                    ;;ac-source-dictionary
                    ac-source-words-in-buffer
                    ac-source-words-in-same-mode-buffers
                    ;;ac-source-words-in-all-buffer
                    ;;ac-source-semantic
                    ))))

(add-to-list 'ac-modes 'processing-mode)
