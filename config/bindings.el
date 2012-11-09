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

;; Split Windows
(global-set-key [f6] 'split-window-horizontally)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [f8] 'delete-window)

(global-set-key (kbd "C-S-d") 'duplicate-line)

(global-set-key (kbd "C-x g") 'magit-status)

;;mark current function
(global-set-key (kbd "C-x C-p")     'mark-defun)

;; C-c Groups
;;
;;e - eval and replace
;;b - winner undo
;;f - winner redo
;;l - lispy shortcuts (i.e. paredit and clojure specific fns)
;;m - emacs eval shortcuts
;;t - text shortcuts
;;i - utf8 char shortcuts
;;j - quick-jump shortcuts
;;d - diff shortcuts
;;p - project shortcuts
;;s - show popupwindows
;;w - window and buffer shortcuts

(global-set-key (kbd "C-c C-e")   'slime-eval-last-expression)

;; winner undo and redo
(global-set-key (kbd "C-c b") 'winner-undo)
(global-set-key (kbd "C-c f") 'winner-redo)

;;text manipulation shortcuts
(global-set-key (kbd "C-c t b")     'untabify-buffer)
(global-set-key (kbd "C-c t r")     'untabify)

;;emacs-lisp shortcuts
(global-set-key (kbd "C-c m s")     'eval-and-replace) ;swap
(global-set-key (kbd "C-c m b")     'eval-buffer)
(global-set-key (kbd "C-c m e")     'eval-last-sexp)
(global-set-key (kbd "C-c m i")     'eval-expression)
(global-set-key (kbd "C-c m d")     'eval-defun)
(global-set-key (kbd "C-c m n")     'eval-print-last-sexp)
(global-set-key (kbd "C-c m r")     'eval-region)

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
(global-set-key (kbd "C-ä") 'quick-jump-push-marker)

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

;; (global-set-key (kbd "C-c s t") 'live-show-ansi-terminal)
;; (global-set-key (kbd "C-c s n") 'live-new-ansi-terminal)
(global-set-key (kbd "C-c s m") 'live-show-messages)

;;overtone shortcuts
(define-key clojure-mode-map (kbd "C-c o s") 'live-overtone-stop)

(global-set-key (kbd "C-c w s") 'swap-windows)
(global-set-key (kbd "C-c w r") 'rotate-windows)

;;requires buffer-move
(global-set-key (kbd "C-c w u")  'buf-move-up)
(global-set-key (kbd "C-c w d")  'buf-move-down)
(global-set-key (kbd "C-c w l")  'buf-move-left)
(global-set-key (kbd "C-c w r")  'buf-move-right)

(global-set-key (kbd "C-c w .") 'shrink-window-horizontally)
(global-set-key (kbd "C-c w ,") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c w /") (lambda () (interactive) (enlarge-window -1)))
(global-set-key (kbd "C-c w '") (lambda () (interactive) (enlarge-window 1)))

;; ;; paredit
;; (define-key paredit-mode-map (kbd "C-c l k") 'paredit-splice-sexp-killing-forward)
;; (define-key paredit-mode-map (kbd "C-c l w") 'paredit-splice-sexp-killing-backward)
;; (define-key paredit-mode-map (kbd "C-c l l") 'align-cljlet)
;; (define-key paredit-mode-map (kbd "C-c l t") 'fill-paragraph)
;; (define-key paredit-mode-map (kbd "C-M-e") 'paredit-backward-barf-sexp)
;; (define-key paredit-mode-map (kbd "C-M-s") 'paredit-backward-slurp-sexp)
;; (define-key paredit-mode-map (kbd "C-M-j") 'live-paredit-forward-slurp-sexp-neatly)
;; (define-key paredit-mode-map (kbd "C-M-y") 'paredit-forward-barf-sexp)
;; (define-key paredit-mode-map (kbd "C-M-z") 'align-cljlet)
;; (define-key paredit-mode-map (kbd "M-s") 'paredit-split-sexp)
;; (define-key paredit-mode-map (kbd "M-j") 'paredit-join-sexps)
;; (define-key paredit-mode-map (kbd "M-P") 'live-paredit-previous-top-level-form)
;; (define-key paredit-mode-map (kbd "M-N") 'live-paredit-next-top-level-form)
;; (define-key paredit-mode-map (kbd "C-M-f") 'live-paredit-forward)
;; (define-key paredit-mode-map (kbd "M-q") 'live-paredit-reindent-defun)
;; (define-key paredit-mode-map (kbd "M-d") 'live-paredit-forward-kill-sexp)
;; (define-key paredit-mode-map (kbd "M-w") 'live-paredit-backward-kill-sexp)
;; (define-key paredit-mode-map (kbd "M-k") 'live-paredit-backward-kill)
;; (define-key paredit-mode-map (kbd "M-\\") 'live-paredit-delete-horizontal-space)

;;scroll other window
(global-set-key (kbd "C-M-ä") 'scroll-other-window)
(global-set-key (kbd "C-M-ö") 'scroll-other-window-down)

;;fast vertical naviation
(global-set-key  (kbd "M-U") (lambda () (interactive) (previous-line 10)))
(global-set-key  (kbd "M-D") (lambda () (interactive) (next-line 10)))
(global-set-key  (kbd "M-p") 'outline-previous-visible-heading)
(global-set-key  (kbd "M-n") 'outline-next-visible-heading)

;; (global-set-key (kbd "C-s") 'isearch-forward)
;; (global-set-key (kbd "C-r") 'isearch-backward)
;; (global-set-key (kbd "C-M-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-M-r") 'isearch-backward-regexp)

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; Ace jump mode
(global-set-key (kbd "C-o") 'ace-jump-mode)

;; Show documentation/information with M-RET
(define-key lisp-mode-shared-map (kbd "M-RET") 'live-lisp-describe-thing-at-point)
(define-key nrepl-mode-map (kbd "M-RET") 'nrepl-doc)
(define-key nrepl-interaction-mode-map (kbd "M-RET") 'nrepl-doc)

;; Make Emacs use "newline-and-indent" when you hit the Enter key so
;; that you don't need to keep using TAB to align yourself when coding.
(global-set-key "\C-m" 'newline-and-indent)

;; Start eshell or switch to it if it's active.
(global-set-key (kbd "C-x m") 'eshell)
;; Start a new eshell even if one is active.
(global-set-key (kbd "C-x M") (lambda () (interactive) (eshell t)))

(define-key markdown-mode-map (kbd "<tab>") nil)

(global-set-key [C-tab] [M-tab])

(global-set-key (kbd " C-x p") popwin:keymap)

(require 'iflipb)
(global-set-key (kbd "C-<next>") 'iflipb-next-buffer)
(global-set-key (kbd "C-<prior>") 'iflipb-previous-buffer)
(global-set-key (kbd "<XF86Forward>") 'iflipb-next-buffer)
(global-set-key (kbd "<XF86Back>") 'iflipb-previous-buffer)

(define-key ac-completing-map "\r" 'ac-complete)

(browse-kill-ring-default-keybindings)

;;kill regions
(global-set-key (kbd "C-x C-k") 'kill-region)
