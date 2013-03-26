;; yasnippet-conf.el

;; add snippets
(setq ptrv-yasnippet-dir (concat (live-pack-lib-dir) "snippets"))
(yas-load-directory ptrv-yasnippet-dir)

;; (setq yas-snippet-dirs (append 'yas-snippet-dirs 'ptrv-yasnippet-dirs))

(defun ptrv-reload-snippets ()
  (interactive)
  (yas-load-directory live-yasnippet-dir)
  (yas-load-directory ptrv-yasnippet-dir))

(eval-after-load 'yasnippet
  '(progn
     (setq yas-keymap  (let ((map (make-sparse-keymap)))
                         (define-key map [(control tab)] 'yas-next-field-or-maybe-expand)
                         (define-key map (kbd "C-TAB")   'yas-next-field-or-maybe-expand)
                         (define-key map [(shift tab)]   'yas-prev-field)
                         (define-key map [backtab]       'yas-prev-field)
                         (define-key map (kbd "C-g")     'yas-abort-snippet)
                         (define-key map (kbd "C-d")     'yas-skip-and-clear-or-delete-char)
                         map))
     (setq yas-prompt-functions '(yas-dropdown-prompt
                                  yas-ido-prompt
                                  yas-x-prompt
                                  yas-completing-prompt))))
