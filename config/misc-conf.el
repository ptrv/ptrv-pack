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
