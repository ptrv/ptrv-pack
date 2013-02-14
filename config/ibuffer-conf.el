;; ibuffer-conf.el

;;organise ibuffer into handy groups
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("IRC"      (mode . erc-mode))
               ("emacs" (or (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$")
                            (name . "^\\*Completions\\*$")
                            (filename . ".emacs.d")
                            (filename . ".live-packs")))
               ("magit" (name . "\\*magit"))
               ("dired" (mode . dired-mode))
               ("sclang" (mode . sclang-mode))
               ("Org" (mode . org-mode))
               ("Help" (or (name . "\\*Help\\*")
                           (name . "\\*Apropos\\*")
                           (name . "\\*info\\*")))
               ("#!-config" (filename . ".cb-config"))
               ))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)
