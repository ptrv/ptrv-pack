(require 'flycheck)
(defvar flycheck-checker-xml-xmlstarlet
  '(:command
    ("xmlstarlet" "val" "-e" "-q" source-inplace)
    :modes nxml-mode))

(add-to-list 'flycheck-checkers 'flycheck-checker-xml-xmlstarlet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'nxml-mode-hook 'flycheck-mode)
(add-hook 'html-mode-hook 'flycheck-mode)
(add-hook 'LaTeX-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'flycheck-mode)
