;;;;;;;;;;;;;;;;;;;;
;; erc-conf.el
;;;;;;;;;;;;;;;;;;;;

(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      `((freenode ((,freenode-user . ,freenode-pass)))))

;;IRC
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#clojure" "#overtone" "#go-nuts")))

(cond ((string= system-name "alderaan")
       (setq erc-autojoin-channels-alist
             (list (append (car erc-autojoin-channels-alist)
                           '("#supercollider" "#crunchbang" "#debian")))))
      ((string= system-name "anoth")
       (setq erc-autojoin-channels-alist
             (list (append (car erc-autojoin-channels-alist)
                           '("#supercollider" "#ubuntu")))))
      ;; (t (setq erc-autojoin-channels-alist
      ;;          '(("freenode.net" "#emacs" "#clojure" "overtone"))))
      )


(defun erc-connect ()
  "Start up erc and connect to freedonde"
  (interactive)
  (erc :server "irc.freenode.net"
       :full-name "Peter V."
       :port 6667
       :nick freenode-user
       ))

(setq erc-keywords `(,freenode-user))
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
