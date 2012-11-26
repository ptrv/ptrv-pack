;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ptrv's personal live pack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(live-load-config-file "package-conf.el")
(live-load-config-file "util-fns.el")
(live-load-config-file "look-and-feel.el")
(live-load-config-file "shell-conf.el")
(live-load-config-file "personal.el")
(live-load-config-file "buffer-conf.el")
(live-load-config-file "bindings.el")
(live-load-config-file "org-mode-conf.el")
(live-load-config-file "latex-conf.el")
(live-load-config-file "filetype-conf.el")
(live-load-config-file "abbrev-conf.el")
(live-load-config-file "pandoc-conf.el")
(live-load-config-file "golang-conf.el")
(live-load-config-file "ack-and-a-half-conf.el")
(live-load-config-file "html-conf.el")
(live-load-config-file "erc-conf.el")
(live-load-config-file "faust-conf.el")
(live-load-config-file "iedit-conf.el")
(live-load-config-file "edit-server-conf.el")
(live-load-config-file "editing-conf.el")
(live-load-config-file "markdown-conf.el")
(live-load-config-file "projects-conf.el")
(cond
 ((eq system-type 'darwin)
  (live-load-config-file "osx-conf.el"))
 ((eq system-type 'gnu/linux)
  (live-load-config-file "supercollider.el")
  (live-load-config-file "python-conf.el")
  (live-load-config-file "c-conf.el")
  (live-load-config-file "linux-conf.el")
  (live-load-config-file "processing2-conf.el")))
(live-load-config-file "flycheck-conf.el")

(defmacro Xlaunch (&rest x)
  (list 'if (eq window-system 'x) (cons 'progn x)))

(Xlaunch (live-load-config-file "x11-config.el" ))

(load-file "~/.emacs.d/packs/dev/power-pack/config/quick-jump-conf.el")

;; local settings
(defvar ptrv-locals-file "~/.emacs-locals.el")
(if (file-exists-p ptrv-locals-file)
     (load-file ptrv-locals-file))


(server-start)
