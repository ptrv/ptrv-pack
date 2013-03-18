;; (setenv "PATH" (shell-command-to-string "echo $PATH"))
;; (setenv "PATH" (replace-regexp-in-string "[[:space:]\n]*$" ""
;;                                          (shell-command-to-string "$SHELL -l -c 'echo $PATH'")))

;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell
;;          (replace-regexp-in-string "[[:space:]\n]*$" ""
;;                                    (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))
;;     ))
;; (when (or
;;        (equal system-type 'darwin)
;;        (equal system-type 'gnu/linux))
;;   (set-exec-path-from-shell-PATH))

(exec-path-from-shell-initialize)

;;; Eshell
;; (eval-when-compile (require 'eshell nil t))
(setq eshell-aliases-file (concat ptrv-pack-root-dir "etc/eshell_aliases"))

(defun eshell/clear ()
  "04Dec2001 - sailor, to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell/e (file)
  (find-file-other-window file))

(add-hook 'eshell-prompt-load-hook
          (lambda ()
            (set-face-attribute 'eshell-prompt-face nil :foreground "dark green")))

(live-add-pack-lib "emacs-pcomplete-plugins")
(require 'pcmpl-git)

;;; Term
(setq term-default-bg-color "black")
(setq term-default-fg-color "white")

;; ;; Configure multi-term
;; (require 'multi-term)
;; (autoload 'multi-term "multi-term" nil t)
;; (autoload 'multi-term-next "multi-term" nil t)
;; ;; (global-set-key (kbd "C-x M") 'multi-term-next)
;; ;; (global-set-key (kbd "C-x m") 'multi-term-dedicated-toggle)

;; (setq multi-term-dedicated-select-after-open-p t)
;; (setq multi-term-dedicated-window-height 20)
