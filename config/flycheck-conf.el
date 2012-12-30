(live-add-pack-lib "flycheck")
(require 'flycheck)

(setq flycheck-ignore-columns t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; TODO: make this work
;; (defvar flycheck-checker-go
;;   '(:command
;;     ("go" "build" "-o" "/dev/null" source)
;;     :modes go-mode))

;; (add-to-list 'flycheck-checkers 'flycheck-checker-go)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; modes
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'nxml-mode-hook 'flycheck-mode)
(add-hook 'sgml-mode-hook 'flycheck-mode)
(add-hook 'nxhtml-mode-hook 'flycheck-mode)
(add-hook 'LaTeX-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'lua-mode-hook 'flycheck-mode)
;; (add-hook 'go-mode-hook 'flycheck-mode)
