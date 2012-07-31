;; session.el
;; save a list of open files in $HOME/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save (quote if-exists))
(desktop-save-mode 1)

;; Flymake
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; ido recent file
(setq ido-max-directory-size 100000)

;; gist.el - open gist on browser
(setq gist-view-gist t)

;; activate wrap-region mode
(wrap-region-global-mode t)

;; add snippets
(yas/load-directory (concat (live-pack-lib-dir) "snippets"))
;; (yas/load-directory (concat (live-pack-lib-dir) "yasnippet-snippets"))

;;organise ibuffer into handy groups
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ;; ("IRC"      (mode . erc-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")
                         (name . "^\\*Completions\\*$")
                         (filename . ".emacs.d")
                         (filename . ".live-packs")))

               ;; ("improcess apps"  (filename . "Development/improcess/apps"))
               ;; ("improcess lib"   (filename . "Development/improcess/lib"))
               ("magit" (name . "\\*magit"))
               ("dired" (mode . dired-mode))
               ("sclang" (mode . sclang-mode))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; popwin settings
(setq popwin:special-display-config
      '(("*Help*"  :height 30 :stick t)
        ("*Completions*" :noselect t)
        ("*compilation*" :noselect t)
        ("*Messages*" :height 30)
        ("*Occur*" :noselect t)
        ("\\*Slime Description.*" :noselect t :regexp t :height 30)
        ("*magit-commit*" :noselect t :height 30 :width 80)
        ("*magit-diff*" :noselect t :height 30 :width 80)
        ("*magit-edit-log*" :noselect t :height 15 :width 80)
        ("\\*Slime Inspector.*" :regexp t :height 30)
        ("*Ido Completions*" :noselect t :height 30)
        ("*eshell*" :height 30)
        ("\\*ansi-term\\*.*" :regexp t :height 30)
        ("*shell*" :height 30)
        (".*overtone.log" :regexp t :height 30)
        ("*gists*" :height 30)
        ("*sldb.*":regexp t :height 30)
        ("*Gofmt Errors*" :noselect t)
        ("*Shell Command Output*" :noselect t)))
