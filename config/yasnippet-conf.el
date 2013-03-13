;; yasnippet-conf.el

;; add snippets
(setq ptrv-yasnippet-dir (concat (live-pack-lib-dir) "snippets"))
(yas-load-directory ptrv-yasnippet-dir)

;; (setq yas-snippet-dirs (append 'yas-snippet-dirs 'ptrv-yasnippet-dirs))

(defun ptrv-reload-snippets ()
  (interactive)
  (yas-load-directory live-yasnippet-dir)
  (yas-load-directory ptrv-yasnippet-dir))
