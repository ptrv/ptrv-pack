;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ptrv's personal live pack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(live-add-pack-lib "magit")
(require 'magit)

(live-add-pack-lib "mk-project")
(require 'mk-project)

(live-load-config-file "util-fns.el")
(live-load-config-file "look-and-feel.el")
(live-load-config-file "package-conf.el")
(live-load-config-file "personal.el")
(live-load-config-file "bindings.el")
(live-load-config-file "org-mode-conf.el")
(live-load-config-file "supercollider.el")
(live-load-config-file "latex-conf.el")
(live-load-config-file "filetype-conf.el")
(live-load-config-file "abbrev-conf.el")
(live-load-config-file "pandoc-conf.el")

(load-file "~/.emacs.d/packs/dev/power-pack/config/quick-jump-conf.el")

(server-start)
