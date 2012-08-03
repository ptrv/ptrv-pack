(cond
 ((and (window-system) (eq system-type 'gnu/linux))
  (add-to-list 'default-frame-alist '(font . "Inconsolata-14"))))

(global-linum-mode t)
;; (setq linum-format (if on-console "%4d " "%4d"))

;; Show column numbers in modeline
;; (setq column-number-mode t)

;; Redefine linum-on to ignore terminal buffers, because just turning
;; it off in term-mode-hook doesn't work.
(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode
                  eshell-mode erc-mode ibuffer-mode magit-log-edit-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes))
    (linum-mode 1)))
