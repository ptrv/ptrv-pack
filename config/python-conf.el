;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; python-mode.el

;; (live-add-pack-lib "python-mode")

;; (setq py-install-directory (concat ptrv-pack-root-dir "lib/python-mode/"))
;; (setq py-load-pymacs-p nil)
;; ;; Do not open python shell on start
;; (setq py-start-run-py-shell nil)

;; (require 'python-mode)

;; ;; ;; use IPython
;; ;; (setq-default py-shell-name "ipython")
;; ;; (setq-default py-which-bufname "IPython")
;; ;; ;; use the wx backend, for both mayavi and matplotlib
;; ;; ;; (setq py-python-command-args
;; ;; ;;   '("--gui=wx" "--pylab=wx" "--colors=Linux"))
;; ;; (setq py-python-command-args
;; ;;   '("--gui=wx" "--pylab=wx"))
;; ;; (setq py-force-py-shell-name-p t)

;; ;; ;; switch to the interpreter after executing code
;; ;; (setq py-shell-switch-buffers-on-execute-p t)
;; ;; (setq py-switch-buffers-on-execute-p t)
;; ;; ;; don't split windows
;; ;; (setq py-split-windows-on-execute-p nil)
;; ;; ;; try to automagically figure out indentation
;; ;; (setq py-smart-indentation t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Pymacs and Rope

;;(live-add-pack-lib "Pymacs")

;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)
;; (autoload 'pymacs-autoload "pymacs")

;; ;; ropemacs
;; (ac-ropemacs-initialize)

;; (setq ropemacs-enable-autoimport t)
;; ;; Automatically save project python buffers before refactorings
;; (setq ropemacs-confirm-saving nil)
;; (setq ropemacs-enable-shortcuts t)
;; ;;(setq ropemacs-local-prefix "C-c C-p")
;; (setq ropemacs-guess-project t)

;; (defun load-ropemacs ()
;;   (interactive)
;;   (require 'pymacs)
;;   (pymacs-load "ropemacs" "rope-")
;;   (ac-ropemacs-setup)
;;   (add-to-list 'ac-sources 'ac-source-filename)
;;   (ropemacs-mode t)
;;   )
;; (add-hook 'python-mode-hook 'load-ropemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-jedi
(live-add-pack-lib "emacs-jedi")
(eval-when-compile (require 'jedi nil t))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pytest
(require 'pytest)
(setq pytest-global-name "py.test")
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-a") 'pytest-all)
            (local-set-key (kbd "C-c C-,") 'pytest-module)
            (local-set-key (kbd "C-c C-.") 'pytest-one)
            (local-set-key (kbd "C-c C-d") 'pytest-directory)
            ;; (local-set-key (kbd "C-c t p a") 'pytest-pdb-all)
            ;; (local-set-key (kbd "C-c t p m") 'pytest-pdb-module)
            ;; (local-set-key (kbd "C-c t p .") 'pytest-pdb-one)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; info
(require 'info-look)
(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(python)Index" nil "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PythonTidy
(defun pytidy-whole-buffer ()
  (interactive)
  (let ((a (point)))
    (shell-command-on-region (point-min) (point-max) "pythontidy" t)
    (goto-char a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pylookup
(live-add-pack-lib "pylookup")
(eval-when-compile (require 'pylookup))
(setq pylookup-dir (concat ptrv-pack-root-dir "lib/pylookup"))
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; set search option if you want
;; (setq pylookup-search-options '("--insensitive" "0" "--desc" "0"))

;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)

(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

(require 'python)
(define-key python-mode-map (kbd "C-c L") 'pylookup-lookup)

(autoload 'pylint "pylint")
(add-hook 'python-mode-hook 'pylint-add-menu-items)
(add-hook 'python-mode-hook 'pylint-add-key-bindings)

;;(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))

(add-hook 'python-mode-hook #'lambda-mode 1)
