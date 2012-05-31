(let ((path))
  (setq path (concat "~/bin:"
                     "/usr/local/bin:"
                     "/usr/bin:"
                     "/bin"))
  (setenv "PATH" path))

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

(global-auto-revert-mode t)

(cond
 ((and (window-system) (eq system-type 'gnu/linux))
  (add-to-list 'default-frame-alist '(font . "Inconsolata-12"))))

(add-to-list 'package-archives '
             ("marmalade" . "http://marmalade-repo.org/packages/"))
