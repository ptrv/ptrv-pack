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

(setq c-basic-offset 4)     ; indents 4 chars
(setq tab-width 4)          ; and 4 char wide for TAB
(setq indent-tabs-mode nil) ; And force use of spaces

; (add-hook 'write-file-hooks
;           (lambda () (if (not indent-tabs-mode)
;                          (untabify (point-min) (point-max)))))

;; gist.el - open gist on browser
(setq gist-view-gist t)

(yas/load-directory "~/.emacs.d/packs/dev/power-pack/vendor/checkouts/yasnippet-0.6.1c/snippets/text-mode")
