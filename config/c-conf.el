(live-add-pack-lib "clang-complete-async")

;; Hook auto-complete into clang
(eval-after-load "cc-mode"
  '(progn
     (require 'auto-complete-clang-async)
     (setq clang-complete-executable
           ;; "~/.live-packs/ptrv-pack/lib/clang-complete-async/clang-complete"
           (concat (live-pack-lib-dir) "clang-complete-async/clang-complete")
           )
     (when (not (file-exists-p clang-complete-executable))
       (warn (concat "The clang-complete executable doesn't exist")))
     ;; (when (not (file-exists-p clang-complete-executable))
     ;;   (warn (concat "The clang-complete executable doesn't exist - please run "
     ;;                 dotfiles-dir "setup.sh to compile it.")))
     ;; Add Qt4 includes to load path if installed
     (when (file-exists-p "/usr/include/qt4")
       (setq ac-clang-flags
             (mapcar (lambda (f) (concat "-I" f))
                     (directory-files "/usr/include/qt4" t "Qt\\w+"))))
     ))

(add-hook 'c++-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-clang-async)
            (launch-completion-proc)))

;; (require 'auto-complete-clang-async)

;; (defun ac-cc-mode-setup ()
;;   (setq clang-complete-executable "~/.live-packs/ptrv-pack/lib/clang-complete-async/clang-complete")
;;   (when (not (file-exists-p clang-complete-executable))
;;     (warn (concat "The clang-complete executable doesn't exist")))
;;   (setq ac-sources '(ac-source-clang-async))
;;   (launch-completion-proc)
;; )

;; (defun my-ac-config ()
;;   ;;(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'c-mode-hook 'ac-cc-mode-setup)
;;   (add-hook 'c++-mode-hook 'ac-cc-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))

;; (my-ac-config)
