;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure-conf.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(live-add-pack-lib "kibit-mode")

(require 'kibit-mode)
(add-hook 'clojure-mode-hook 'kibit-mode)

(define-key kibit-mode-keymap (kbd "C-c C-n") 'nil)
(define-key kibit-mode-keymap (kbd "C-c k c") 'kibit-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (flycheck-declare-checker clojure-kibit
;;   "A Clojure code analyzer using the kibit utility."
;;   :command `(,(concat kibit-mode-path "bin/kibit-flymake.sh") source)
;;   :error-patterns '(("\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:ERROR: .* CORRECTION: .*\\)" error))
;;   :modes 'clojure-mode)

;; (add-to-list 'flycheck-checkers 'clojure-kibit)

;;(add-hook 'clojure-mode-hook 'flycheck-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; push-mark when switching to nrepl via C-c C-z
(defadvice nrepl-switch-to-repl-buffer (around
                                        nrepl-switch-to-repl-buffer-with-mark
                                        activate)
  (with-current-buffer (current-buffer)
    (push-mark)
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
