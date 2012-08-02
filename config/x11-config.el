;; x11.el -- X-specific stuff, only loaded if X is available

;; Maximise the Emacs window
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;; set fullscreen if we are on a small display
(if (and
     (<= (x-display-pixel-width) 1280)
     (<= (x-display-pixel-height) 800))
    (toggle-fullscreen))

;; Define a function for making desktop notifications
(require 'dbus)
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

(require 'notify)
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
