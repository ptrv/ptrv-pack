;; User pack init file
;;
;; User this file to initiate the pack configuration.
;; See README for more information.
(live-add-pack-lib "mk-project")
(require 'mk-project)

(live-load-config-file "util-fns.el")
(live-load-config-file "personal.el")
(live-load-config-file "bindings.el")
(live-load-config-file "org-mode-conf.el")
(live-load-config-file "supercollider.el")

(setq ido-max-directory-size 100000)

(server-start)
