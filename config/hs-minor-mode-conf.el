;; hs-minor-mode-conf.el

(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
(add-hook 'sh-mode-hook 'hs-minor-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)

(require 'hideshow)
;; https://github.com/Hawstein/my-emacs/blob/master/_emacs/hs-minor-mode-settings.el
(setq hs-isearch-open t)

(defvar hs-hide-all nil "Current state of hideshow for toggling all.")
(make-local-variable 'hs-hide-all)

(defun hs-toggle-hiding-all ()
  "Toggle hideshow all."
  (interactive)
  (setq hs-hide-all (not hs-hide-all))
  (if hs-hide-all
      (hs-hide-all)
    (hs-show-all)))

(defvar fold-all-fun nil "Function to fold all.")
(make-variable-buffer-local 'fold-all-fun)
(defvar fold-fun nil "Function to fold.")
(make-variable-buffer-local 'fold-fun)

(defun toggle-fold-all ()
  "Toggle fold all."
  (interactive)
  (if fold-all-fun
      (call-interactively fold-all-fun)
    (hs-toggle-hiding-all)))

(defun toggle-fold ()
  "Toggle fold."
  (interactive)
  (if fold-fun
      (call-interactively fold-fun)
    (hs-toggle-hiding)))

(define-key hs-minor-mode-map (kbd "C-c @ h") 'hs-hide-block)
(define-key hs-minor-mode-map (kbd "C-c @ H") 'hs-hide-all)
(define-key hs-minor-mode-map (kbd "C-c @ s") 'hs-show-block)
(define-key hs-minor-mode-map (kbd "C-c @ S") 'hs-show-all)
(define-key hs-minor-mode-map (kbd "C-c @ l") 'hs-hide-level)
(define-key hs-minor-mode-map (kbd "C-c @ c") 'hs-toggle-hiding)

(defadvice goto-line (after expand-after-goto-line
                            activate compile)
  "hideshow-expand affected block when using goto-line in a collapsed buffer"
  (save-excursion
    (hs-show-block)))