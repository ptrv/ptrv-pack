;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck-conf.el

(unless (and (>= emacs-major-version 24)
             (>= emacs-minor-version 3))
  (add-to-list 'debug-ignored-errors "\\`No more Flycheck errors\\'")
  (add-to-list 'debug-ignored-errors "\\`Flycheck mode disabled\\'")
  (add-to-list 'debug-ignored-errors "\\`Configured syntax checker .* cannot be used\\'"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))
(add-hook 'lisp-interaction-mode (lambda () (flycheck-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "flycheck"
  '(progn
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

     (setq flycheck-highlighting-mode 'lines)

     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
