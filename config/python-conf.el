(require 'smart-operator)

;; (live-add-pack-lib "pymacs-0.24-beta2")
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport t)

(add-hook 'python-mode-hook
          (lambda ()
            (setq ac-sources
                  '(ac-source-dictionary
                    ac-source-words-in-buffer
                    ac-source-words-in-same-mode-buffers
                    ac-source-words-in-all-buffer
                    ;;ac-source-yasnippet
                    ac-source-semantic))))

(add-to-list 'ac-modes 'python-mode)


(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "pycheckers"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(load-library "flymake-cursor")
