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

(add-hook 'nxml-mode-hook 'flycheck-mode)
(add-hook 'html-mode-hook 'flycheck-mode)
(add-hook 'LaTeX-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
;; (add-hook 'lua-mode-hook 'flycheck-mode)
