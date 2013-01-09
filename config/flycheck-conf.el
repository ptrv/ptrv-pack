(live-add-pack-lib "flycheck")
(require 'flycheck)

(setq flycheck-highlighting-mode 'lines)

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

(defun flycheck-enable-except-on-temp-buffers ()
  (if (and buffer-file-name (file-exists-p buffer-file-name))
      (flycheck-mode)))

(add-hook 'nxml-mode-hook 'flycheck-enable-except-on-temp-buffers)
(add-hook 'sgml-mode-hook 'flycheck-enable-except-on-temp-buffers)
(add-hook 'nxhtml-mode-hook 'flycheck-enable-except-on-temp-buffers)
;; (add-hook 'sgml-mode-hook 'flycheck-mode)
;; (add-hook 'nxhtml-mode-hook 'flycheck-mode)
(add-hook 'LaTeX-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'lua-mode-hook 'flycheck-mode)
;; (add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'shell-script-mode 'flycheck-mode)
