(live-add-pack-lib "python-mode")

(setq py-install-directory
      (expand-file-name
       (file-name-as-directory
        (concat (live-pack-lib-dir) "python-mode"))))

(setq py-load-pymacs-p nil)
;; Do not open python shell on start
(setq py-start-run-py-shell nil)


(require 'python-mode)

;; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
;; use the wx backend, for both mayavi and matplotlib
;; (setq py-python-command-args
;;   '("--gui=wx" "--pylab=wx" "--colors=Linux"))
(setq py-python-command-args
  '("--gui=wx" "--pylab=wx"))
(setq py-force-py-shell-name-p t)

;; ;; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p t)
;; (setq py-switch-buffers-on-execute-p t)
;; ;; don't split windows
;; (setq py-split-windows-on-execute-p nil)
;; ;; try to automagically figure out indentation
;; (setq py-smart-indentation t)

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

;; ropemacs
(setq ropemacs-enable-autoimport t)
;; Automatically save project python buffers before refactorings
(setq ropemacs-confirm-saving nil)
;; (setq ropemacs-enable-shortcuts nil)
(setq ropemacs-local-prefix "C-c C-p")
(setq ropemacs-guess-project t)

(ac-ropemacs-initialize)

(defun load-ropemacs ()
  (interactive)
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  (ac-ropemacs-setup)
  (add-to-list 'ac-sources 'ac-source-filename)
  (ropemacs-mode t)
  )
(add-hook 'python-mode-hook 'load-ropemacs)

;; (require 'smart-operator)

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (setq ac-sources
;;                   '(ac-source-dictionary
;;                     ac-source-words-in-buffer
;;                     ac-source-words-in-same-mode-buffers
;;                     ac-source-words-in-all-buffer
;;                     ;; ac-source-yasnippet
;;                     ac-source-semantic))))

;; (add-to-list 'ac-modes 'python-mode)

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
