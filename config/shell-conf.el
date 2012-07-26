;; (let ((path))
;;   (setq path (concat "~/bin:"
;;                      "/usr/local/bin:"
;;                      "/usr/bin:"
;;                      "/bin"))
;;   (setenv "PATH" path))

;; (setenv "PATH" (shell-command-to-string "echo $PATH"))
(setenv "PATH" (replace-regexp-in-string "[[:space:]\n]*$" ""
                                         (shell-command-to-string "$SHELL -l -c 'echo $PATH'")))

;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell
;;          (replace-regexp-in-string "[[:space:]\n]*$" ""
;;                                    (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))
;;     ))
;; (when (or
;;        (equal system-type 'darwin)
;;        (equal system-type 'gnu/linux))
;;   (set-exec-path-from-shell-PATH))

(setq eshell-aliases-file "~/.live-packs/ptrv-pack/etc/eshell_aliases")
