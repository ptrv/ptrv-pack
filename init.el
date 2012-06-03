;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ptrv's personal live pack

(live-add-pack-lib "egg")
(require 'egg)

(live-add-pack-lib "mk-project")
(require 'mk-project)

(live-load-config-file "util-fns.el")
(live-load-config-file "personal.el")
(live-load-config-file "bindings.el")
(live-load-config-file "org-mode-conf.el")
(live-load-config-file "supercollider.el")

(setq ido-max-directory-size 100000)

(server-start)
