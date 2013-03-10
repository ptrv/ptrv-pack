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
