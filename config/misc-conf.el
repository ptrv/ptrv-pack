;; misc-conf.el

;; session.el
;; save a list of open files in $HOME/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save (quote if-exists))
(desktop-save-mode 1)

;; ;; Flymake
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; ido recent file
(setq ido-max-directory-size 100000)

(setq sentence-end-double-space nil)

;; gist.el - open gist on browser
(setq gist-view-gist t)

;; activate wrap-region mode
(wrap-region-global-mode t)

;; follow version controlled symlinks automatically
(setq vc-follow-symlinks t)

;; add snippets
(yas-load-directory (concat (live-pack-lib-dir) "snippets"))

(setq live-disable-zone t)

;; debug messages
(setq debug-on-error nil)

;; scroll compilation buffer
(setq compilation-scroll-output 'first-error)

;; fix tramp backups
(add-to-list 'bkup-backup-directory-info
             (list tramp-file-name-regexp ""))
(setq tramp-bkup-backup-directory-info bkup-backup-directory-info)

;; ;;disable backups of files edited with tramp
;;(setq tramp-bkup-backup-directory-info nil)

;; hippie-expand functions list
;; http://trey-jackson.blogspot.de/2007/12/emacs-tip-5-hippie-expand.html
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        ))
