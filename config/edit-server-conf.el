(require 'edit-server)

(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer)

(edit-server-start)

(setq edit-server-url-major-mode-alist
      '(("github\\.com" . gfm-mode)))
