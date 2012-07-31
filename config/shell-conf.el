;; (setenv "PATH" (shell-command-to-string "echo $PATH"))
(setenv "PATH" (replace-regexp-in-string "[[:space:]\n]*$" ""
                                         (shell-command-to-string "$SHELL -l -c 'echo $PATH'")))

;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell
;;          (replace-regexp-in-string "[[:space:]\n]*$" ""
;;                                    (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))
;;     ))
;; (when (or
;;        (equal system-type 'darwin)
;;        (equal system-type 'gnu/linux))
;;   (set-exec-path-from-shell-PATH))

(setq eshell-aliases-file "~/.live-packs/ptrv-pack/etc/eshell_aliases")

(setq term-default-bg-color "black")
(setq term-default-fg-color "white")

;; Configure multi-term
(require 'multi-term)
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)
;; (global-set-key (kbd "C-x M") 'multi-term-next)
;; (global-set-key (kbd "C-x m") 'multi-term-dedicated-toggle)

(setq multi-term-dedicated-select-after-open-p t)
(setq multi-term-dedicated-window-height 20)