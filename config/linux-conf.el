;; (if (display-graphic-p)
;;     (progn
;;       (add-to-list 'default-frame-alist '(font . "Inconsolata-12"))))
(add-to-list 'default-frame-alist '(font . "Inconsolata-12"))
(autoload 'pcomplete/apt-get "pcmpl-apt" nil nil)

(defun setup-frame-hook (frame)
  ;; (run-with-idle-timer 0.2 nil 'toggle-frame-maximized)
  (run-with-idle-timer 0.2 nil 'toggle-fullscreen)
  )
(add-hook 'after-make-frame-functions 'setup-frame-hook)

;; Define a function for making desktop notifications
(autoload 'dbus-call-method "dbus" nil nil)
(defun dbus-send-desktop-notification (summary body icon timeout)
  "call notification-daemon method METHOD with ARGS over dbus"
  (dbus-call-method
    :session                        ; use the session (not system) bus
    "org.freedesktop.Notifications" ; service name
    "/org/freedesktop/Notifications"   ; path name
    "org.freedesktop.Notifications" "Notify" ; Method
    "emacs"
    0 icon summary body
    '(:array)
    '(:array (:dict-entry "x-canonical-append" (:variant "allowed")))
    ':int32 timeout))

;; ERC stuff

;; (require 'notifications)
;; (defun erc-global-notify (match-type nick message)
;;   "Notify when a message is recieved."
;;   (unless (posix-string-match "^\\** *Users on #" message)
;;     (notifications-notify
;;      :title (format "%s in %s"
;;                     ;; Username of sender
;;                     (car (split-string nick "!"))
;;                     ;; Channel
;;                     (or (erc-default-target) "#unknown"))
;;      :body message
;;      )))

;; (add-hook 'erc-text-matched-hook 'erc-global-notify)

(defun my-notify-erc (match-type nickuserhost message)
  "Notify when a message is received."
  (unless (posix-string-match "^\\** *Users on #" message)
    (notify (format "%s in %s"
                    ;; Username of sender
                    (car (split-string nickuserhost "!"))
                    ;; Channel
                    (or (erc-default-target) "#unknown"))
            ;; Remove duplicate spaces
            (replace-regexp-in-string " +" " " message)
            ;; :icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
            :timeout -1)))

(add-hook 'erc-text-matched-hook 'my-notify-erc)
