;; Place your bindings here.
(require 'key-chord)
(key-chord-mode 1)

;; For example:
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

(windmove-default-keybindings) ;; Shift+direction

(global-set-key [(control shift left)] 'previous-buffer)
(global-set-key [(control shift right)] 'next-buffer)

(global-set-key [f5] 'refresh-file)

(global-set-key (kbd "C-S-d") 'duplicate-line)

(global-set-key (kbd "C-x g") 'magit-status)

;;mark current function
(global-set-key (kbd "C-x C-p")     'mark-defun)

;;C-c handy shortcuts
;;l - lispy shortcuts (i.e. paredit and clojure specific fns)
;;m - emacs eval shortcuts
;;s - slime eval shortcuts
;;t - text manipulation shortcuts
;;i - utf8 char shortcuts
;;j - quick-jump shortcuts
;;d - diff shortcuts
;;p - project shortcuts
;;o - overtone shortcuts

;;text manipulation shortcuts
(global-set-key (kbd "C-c t b")     'untabify-buffer)
(global-set-key (kbd "C-c t r")     'untabify)

;;C-c C-? bindings
(global-set-key (kbd "C-c C-e")   'slime-eval-last-expression)

;;emacs-lisp shortcuts
(global-set-key (kbd "C-c m s")     'eval-and-replace) ;swap
(global-set-key (kbd "C-c m b")     'eval-buffer)
(global-set-key (kbd "C-c m e")     'eval-last-sexp)
(global-set-key (kbd "C-c m i")     'eval-expression)
(global-set-key (kbd "C-c m d")     'eval-defun)
(global-set-key (kbd "C-c m n")     'eval-print-last-sexp)
(global-set-key (kbd "C-c m r")     'eval-region)

;;slime mode shortcuts
(global-set-key (kbd "C-c s c")     'slime-connect)
(global-set-key (kbd "C-c s e e")   'slime-eval-last-expression)
(global-set-key (kbd "C-c s e b")   'slime-eval-buffer)
(global-set-key (kbd "C-c s e i")   'slime-eval-describe)
(global-set-key (kbd "C-c s e a")   'slime-eval-async)
(global-set-key (kbd "C-c s e d")   'slime-eval-defun)
(global-set-key (kbd "C-c s e f")   'slime-eval-feature-expression)
(global-set-key (kbd "C-c s e l")   'slime-eval-for-lisp)
(global-set-key (kbd "C-c s e m")   'slime-eval-macroexpand-inplace)
(global-set-key (kbd "C-c s e n")   'slime-eval-print-last-expression)
(global-set-key (kbd "C-c s e r")   'slime-eval-region)
(global-set-key (kbd "C-c s e t")   'slime-eval-with-transcript)

;;funky characters
(global-set-key (kbd "C-c i l") (lambda () (interactive) (insert "λ")))
(global-set-key (kbd "C-c i n") (lambda () (interactive) (insert "ℕ")))
(global-set-key (kbd "C-c i i") (lambda () (interactive) (insert "∞")))
(global-set-key (kbd "C-c i .") (lambda () (interactive) (insert "×")))
(global-set-key (kbd "C-c i 0") (lambda () (interactive) (insert "∅")))
(global-set-key (kbd "C-c i u") (lambda () (interactive) (insert "∪")))
(global-set-key (kbd "C-c i s") (lambda () (interactive) (insert "♯")))
(global-set-key (kbd "C-c i p") (lambda () (interactive) (insert "£")))

(global-set-key (kbd "C-c j p") 'quick-jump-go-back)
(global-set-key (kbd "C-c j b") 'quick-jump-go-back)
(global-set-key (kbd "C-c j m") 'quick-jump-push-marker)
(global-set-key (kbd "C-c j n") 'quick-jump-go-forward)
(global-set-key (kbd "C-c j f") 'quick-jump-go-forward)
(global-set-key (kbd "C-c j c") 'quick-jump-clear-all-marker)
(key-chord-define-global "öj"  'quick-jump-push-marker)

;;diff shortcuts
(global-set-key (kbd "C-c d f") 'diff-buffer-with-file)

;;mk-project shortcuts
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p a") 'project-ack)
(global-set-key (kbd "C-c p o") 'project-multi-occur)
(global-set-key (kbd "C-c p f") 'project-find-file-ido)
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p t") 'project-tags)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p s") 'project-status)

;;overtone shortcuts
(define-key clojure-mode-map (kbd "C-c o s") 'overtone-stop)

;;requires buffer-move
(global-set-key (kbd "C-c w u")  'buf-move-up)
(global-set-key (kbd "C-c w d")  'buf-move-down)
(global-set-key (kbd "C-c w l")  'buf-move-left)
(global-set-key (kbd "C-c w r")  'buf-move-right)

;;fast vertical naviation
(global-set-key  (kbd "M-P") (lambda () (interactive) (previous-line 10)))
(global-set-key  (kbd "M-N") (lambda () (interactive) (next-line 10)))
(global-set-key  (kbd "M-p") 'outline-previous-visible-heading)
(global-set-key  (kbd "M-n") 'outline-next-visible-heading)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Ace jump mode
(global-set-key (kbd "C-o") 'ace-jump-mode)
