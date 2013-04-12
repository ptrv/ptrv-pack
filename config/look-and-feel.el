;; look-and-feel.el

;; Show column numbers in modeline
(setq column-number-mode t)

;; (global-linum-mode t)
;; (setq linum-format "%4d")

;; ;; Redefine linum-on to ignore terminal buffers, because just turning
;; ;; it off in term-mode-hook doesn't work.
;; (setq linum-disabled-modes
;;       '(term-mode slime-repl-mode magit-status-mode help-mode
;;                   eshell-mode erc-mode ibuffer-mode magit-log-edit-mode))
;; (defun linum-on ()
;;   (unless (or (minibufferp) (member major-mode linum-disabled-modes))
;;     (linum-mode 1)))

(global-hl-line-mode 1)

(defvar ptrv-themes-dir (concat (live-pack-lib-dir) "themes"))
(add-to-list 'load-path ptrv-themes-dir)
;; (load-file (concat (file-name-as-directory ptrv-themes-dir) "gandalf-ptrv.el"))
(autoload 'color-theme-gandalf-ptrv "gandalf-ptrv" nil nil)
(color-theme-gandalf-ptrv)

;; taken from colour-pack
(load-file (concat (live-pack-dir "dev/colour-pack") "lib/live-fontify-hex.el"))
(require 'live-fontify-hex)

(font-lock-add-keywords 'lisp-mode
                        '((live-fontify-hex-colors)))

(font-lock-add-keywords 'emacs-lisp-mode
                        '((live-fontify-hex-colors)))

(font-lock-add-keywords 'lisp-interaction-mode
                        '((live-fontify-hex-colors)))

(font-lock-add-keywords 'css-mode
                        '((live-fontify-hex-colors)))

;; nyan-mode
(live-add-pack-lib "nyan-mode")
(setq nyan-bar-length 16)
(autoload 'nyan-mode "nyan-mode" nil t)
(nyan-mode 1)
