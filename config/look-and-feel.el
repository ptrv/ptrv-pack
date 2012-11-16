;; Let's see what we're running on
(setq on-console (null window-system))

(global-linum-mode t)
(setq linum-format (if on-console "%4d " "%4d"))

;; Show column numbers in modeline
(setq column-number-mode t)

;; Redefine linum-on to ignore terminal buffers, because just turning
;; it off in term-mode-hook doesn't work.
(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode
                  eshell-mode erc-mode ibuffer-mode magit-log-edit-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes))
    (linum-mode 1)))

(global-hl-line-mode 1)

(require 'color-theme)

(defvar ptrv-themes-dir (concat (live-pack-lib-dir) "themes"))
(load-file (concat  (file-name-as-directory ptrv-themes-dir) "gandalf-ptrv.el"))
(color-theme-gandalf-ptrv)
