;; popwin-conf.el

;; popwin settings
(setq popwin:special-display-config
      '(("*Help*" :height 30 :stick t)
        ("*Completions*" :noselect t)
        ("*compilation*" :noselect t)
        ("*Messages*")
        ("*Occur*" :noselect t)
        ("\\*Slime Description.*" :noselect t :regexp t :height 30)
        ("*magit-commit*" :noselect t :height 30 :width 80 :stick t)
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
        ;; ("*Gofmt Errors*" :noselect t)
        ("\\*godoc*" :regexp t)
        ("*Shell Command Output*" :noselect t)
        ("*nREPL error*" :height 30)
        ("*nREPL doc*" :height 30)
        ("*Kill Ring*" :height 30)
        ("*project-status*" :noselect t)
        ("*Compile-Log" :height 20)
        ("*pytest*" :noselect t)
        ))
