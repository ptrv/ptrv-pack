;; (add-hook 'html-mode-hook 'flymake-mode)

;; (when (load "flymake" t)
;;   (defun flymake-html-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "tidy" (list local-file))))

;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.html$\\|\\.ctp" flymake-html-init))

;;   (add-to-list 'flymake-err-line-patterns
;;                '("line \\([0-9]+\\) column \\([0-9]+\\) - \\(Warning\\|Error\\): \\(.*\\)"
;;                  nil 1 2 4))
;; )

;; nXhtml
(live-add-pack-lib "nxhtml")

(autoload 'nxhtml-mumamo-mode "autostart" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(html\\|ejs\\|jsp\\)$" . nxhtml-mumamo-mode))
(eval-after-load "nxhtml-mode"
  '(setq mumamo-chunk-coloring 1
     rng-nxml-auto-validate-flag nil
     nxhtml-skip-welcome t))

;; Patch a mumamo bug which keeps giving annoying warnings
(eval-after-load "mumamo"
  '(setq mumamo-per-buffer-local-vars (delq 'buffer-file-name mumamo-per-buffer-local-vars)))
