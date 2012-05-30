;; Place your bindings here.

;; For example:
;;(define-key global-map (kbd "C-+") 'text-scale-increase)
;;(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-x f") 'ido-recentf-open)

(windmove-default-keybindings) ;; Shift+direction

(global-set-key [(control shift left)] 'previous-buffer)
(global-set-key [(control shift right)] 'next-buffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
