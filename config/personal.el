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
			   ("magit" (name . "\\*magit"))
			   ("dired" (mode . dired-mode))
			   ("sclang" (mode . sclang-mode))
			   ("Org" (mode . org-mode))
			   ("Help" (or (name . "\\*Help\\*")
						   (name . "\\*Apropos\\*")
						   (name . "\\*info\\*")))
			   ))))

(add-hook 'ibuffer-mode-hook
		  (lambda ()
			(ibuffer-auto-mode 1)
			(ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)

;; popwin settings
(setq popwin:special-display-config
	  '(("*Help*" :height 30 :stick t)
		("*Completions*" :noselect t)
		("*compilation*" :noselect t)
		("*Messages*")
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
		("\\*godoc*" :regexp t)
		("*Shell Command Output*" :noselect t)
		("*nREPL error*" :height 30)
		("*nREPL doc*" :height 30)
		("*Kill Ring*" :height 30)
		("*project-status*" :noselect t)
		("*Compile-Log" :height 20)))

(setq live-disable-zone t)

;; debug messages
(setq debug-on-error nil)

;; scroll compilation buffer
(setq compilation-scroll-output 'first-error)

;; Do not allow to kill the *scratch* buffer
(defvar unkillable-scratch-buffer-erase)
(setq unkillable-scratch-buffer-erase nil)
(defun toggle-unkillable-scratch-buffer-erase ()
  (interactive)
  (if unkillable-scratch-buffer-erase
	  (progn
		(setq unkillable-scratch-buffer-erase nil)
		(message "Disable scratch-buffer erase on kill!"))
	(progn
	  (setq unkillable-scratch-buffer-erase t)
	  (message "Enable scratch-buffer erase on kill!"))))

(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
	  (progn
		(if unkillable-scratch-buffer-erase
			(delete-region (point-min) (point-max)))
		nil
		)
	t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)
