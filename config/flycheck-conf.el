(require 'flycheck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar flycheck-checker-xml-xmlstarlet
  '(:command
    ("xmlstarlet" "val" "-e" "-q" source-inplace)
    :modes nxml-mode))

(add-to-list 'flycheck-checkers 'flycheck-checker-xml-xmlstarlet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar flycheck-checker-go
  '(:command
    ("go" "build" "-o" "/dev/null" source)
    :modes go-mode))

(add-to-list 'flycheck-checkers 'flycheck-checker-go)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar flycheck-checker-lua
;;   '(:command
;;     ("luac" "-p" source)
;;     :error-patterns
;;     (("^.*luac[0-9.]*\\(.exe\\)?: *\\(.*\\):\\([0-9]+\\): \\(.*\\)$"
;;       2 3 nil 4))
;;     :modes lua-mode))

;; (add-to-list 'flycheck-checkers 'flycheck-checker-lua)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun flycheck-enable-except-on-temp-buffers ()
  (if (and buffer-file-name (file-exists-p buffer-file-name))
      (flycheck-mode)))

(add-hook 'nxml-mode-hook 'flycheck-enable-except-on-temp-buffers)
(add-hook 'sgml-mode-hook 'flycheck-enable-except-on-temp-buffers)
(add-hook 'LaTeX-mode-hook 'flycheck-enable-except-on-temp-buffers)
(add-hook 'python-mode-hook 'flycheck-enable-except-on-temp-buffers)
(add-hook 'go-mode-hook 'flycheck-enable-except-on-temp-buffers)
;; (add-hook 'lua-mode-hook 'flycheck-enable-except-on-temp-buffers)
