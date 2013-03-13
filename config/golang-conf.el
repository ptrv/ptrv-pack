;; go-lang completion
(require 'go-autocomplete)

(add-hook 'go-mode-hook '(lambda ()
                           (set (make-local-variable 'before-save-hook)
                                'gofmt-before-save)))

(add-hook 'go-mode-hook
          (lambda ()
            (setq skeleton-pair t)
            (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
            ))

;;(define-key go-mode-map (kbd "C-c h") 'godoc)

(eval-after-load "go-mode"
  '(if (executable-find "goflymake")
       (progn
         (flycheck-declare-checker go-goflymake
           "A Go syntax and style checker using the go utility.

See URL `https://github.com/dougm/goflymake'."
           :command '("goflymake" source)
           :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
           :modes 'go-mode)
         ;;(add-to-list 'flycheck-checkers 'go-goflymake)
         (setq flycheck-checkers (append flycheck-checkers '(go-goflymake)))
         )
     (warn "goflymake not found")))
