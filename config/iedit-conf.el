(live-add-pack-lib "emacs-iedit")

(require 'iedit)

;; (defun iedit-dwim (arg)
;;   "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
;;   (interactive "P")
;;   (if arg
;;       (iedit-mode)
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         ;; this function determines the scope of `iedit-start'.
;;         (narrow-to-defun)
;;         (if iedit-mode
;;             (iedit-done)
;;           ;; `current-word' can of course be replaced by other
;;           ;; functions.
;;           (iedit-start (current-word)))))))
;;
;; (global-set-key (kbd "C-;") 'iedit-dwim)

(global-set-key (kbd "C-;") 'iedit-mode)
(define-key isearch-mode-map (kbd "C-;") 'iedit-mode)
