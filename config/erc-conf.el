;;;;;;;;;;;;;;;;;;;;
;; erc-conf.el
;;;;;;;;;;;;;;;;;;;;

(setq freenode-credentials (netrc-machine (netrc-parse "~/.authinfo.gpg") "freenode" t))
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      `((freenode ((,(netrc-get freenode-credentials "login") .
                    ,(netrc-get freenode-credentials "password"))))))

;;IRC
(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#ubuntu" "#supercollider")))

(defun erc-connect ()
  "Start up erc and connect to freedonde"
  (interactive)
  (erc :server "irc.freenode.net"
       :full-name "Peter V."
       :port 6667
       :nick (netrc-get freenode-credentials "login")
       ))

(require 'erc-match)
(setq erc-keywords '("ptrv"))
(erc-match-mode)

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;;change wrap width when window is resized
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook
           '(lambda ()
              (save-excursion
                (walk-windows
                 (lambda (w)
                   (let ((buffer (window-buffer w)))
                     (set-buffer buffer)
                     (when (eq major-mode 'erc-mode)
                       (setq erc-fill-column (- (window-width w) 2)))))))))
