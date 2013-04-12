;; typewriter-conf.el

(autoload 'typewriter-mode "typewriter-mode" nil t)
(setq typewriter-sound-default (concat
                                ptrv-pack-root-dir
                                "etc/sounds/9744__horn__typewriter.wav"))
(setq typewriter-sound-end (concat
                            ptrv-pack-root-dir
                            "etc/sounds/eol-bell.wav"))
(setq typewriter-sound-return (concat
                               ptrv-pack-root-dir
                               "etc/sounds/carriage-return.wav"))
