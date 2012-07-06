;; (let ((path))
;;   (setq path (concat "~/bin:"
;;                      "/usr/local/bin:"
;;                      "/usr/bin:"
;;                      "/bin"))
;;   (setenv "PATH" path))

(setenv "PATH" (shell-command-to-string "echo $PATH"))

;;organise ibuffer into handy groups
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("IRC"      (mode . erc-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*Completions\\*$")
                         (filename . ".emacs.d")
                         (filename . ".live-packs")))

               ;; ("improcess apps"  (filename . "Development/improcess/apps"))
               ;; ("improcess lib"   (filename . "Development/improcess/lib"))
               ("dired" (mode . dired-mode))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ido-max-directory-size 100000)

(global-auto-revert-mode t)

(setq-default indent-tabs-mode nil) ; And force use of spaces
(setq c-basic-offset 4)     ; indents 4 chars
(setq-default tab-width 4)          ; and 4 char wide for TAB

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))))

; (add-hook 'write-file-hooks
;           (lambda () (if (not indent-tabs-mode)
;                          (untabify (point-min) (point-max)))))

;; gist.el - open gist on browser
(setq gist-view-gist t)

(setq eshell-aliases-file "~/.live-packs/ptrv-pack/eshell/alias")
