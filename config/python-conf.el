(live-add-pack-lib "python-mode")
(setq py-install-directory (concat (live-pack-lib-dir) "python-mode"))

(require 'python-mode)

(setq py-load-pymacs-p t)
(setq py-start-run-py-shell nil)

;; (require 'smart-operator)

;; ;; (live-add-pack-lib "pymacs-0.24-beta2")
;; ;; (require 'pymacs)
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (pymacs-load "ropemacs" "rope-")
;; (setq ropemacs-enable-autoimport t)

(add-hook 'python-mode-hook
          (lambda ()
            (setq ac-sources
                  '(ac-source-dictionary
                    ac-source-words-in-buffer
                    ac-source-words-in-same-mode-buffers
                    ac-source-words-in-all-buffer
                    ;; ac-source-yasnippet
                    ac-source-semantic))))

(add-to-list 'ac-modes 'python-mode)

;; (add-hook 'python-mode-hook 'flymake-mode)
;; (when (load "flymake" t)
;;   (defun flymake-pyflakes-init ()
;;     (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;            (local-file (file-relative-name
;;                         temp-file
;;                         (file-name-directory buffer-file-name))))
;;       (list "pycheckers"  (list local-file))))
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py\\'" flymake-pyflakes-init)))
;; (load-library "flymake-cursor")
