;; (live-add-pack-lib "Pymacs")

;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")

;; ;; ropemacs
;; (setq ropemacs-enable-autoimport t)
;; ;; Automatically save project python buffers before refactorings
;; (setq ropemacs-confirm-saving nil)
;; ;; (setq ropemacs-enable-shortcuts nil)
;; (setq ropemacs-local-prefix "C-c C-p")
;; (setq ropemacs-guess-project t)

;; (ac-ropemacs-initialize)

;; (defun load-ropemacs ()
;;   (interactive)
;;   (require 'pymacs)
;;   (pymacs-load "ropemacs" "rope-")
;;   (ac-ropemacs-setup)
;;   (add-to-list 'ac-sources 'ac-source-filename)
;;   (ropemacs-mode t)
;;   )
;; (add-hook 'python-mode-hook 'load-ropemacs)

(live-add-pack-lib "emacs-jedi")
(autoload 'jedi:setup "jedi" nil t)
(setq jedi:setup-keys t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq ac-sources
;;                   '(ac-source-dictionary
;;                     ac-source-words-in-buffer
;;                     ac-source-words-in-same-mode-buffers
;;                     ac-source-words-in-all-buffer
;;                     ;; ac-source-yasnippet
;;                     ac-source-semantic))))

(require 'pytest)
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-a") 'pytest-all)
            (local-set-key (kbd "C-c C-,") 'pytest-module)
            (local-set-key (kbd "C-c C-.") 'pytest-one)
            (local-set-key (kbd "C-c C-d") 'pytest-directory)
            (local-set-key (kbd "C-c t p a") 'pytest-pdb-all)
            (local-set-key (kbd "C-c t p m") 'pytest-pdb-module)
            (local-set-key (kbd "C-c t p .") 'pytest-pdb-one)
            ))
