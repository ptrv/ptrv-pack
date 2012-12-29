;; x11.el -- X-specific stuff, only loaded if X is available

;; Maximise the Emacs window
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;; set fullscreen if we are on a small display
;; (if (and
;;      (<= (x-display-pixel-width) 1280)
;;      (<= (x-display-pixel-height) 800))
;;     (toggle-fullscreen))

(cond ((and
         (<= (x-display-pixel-width) 1280)
         (<= (x-display-pixel-height) 800))
       (toggle-fullscreen))
      (t
       ;; (set-frame-size (selected-frame) 110 60)
       (set-frame-size (selected-frame) 130 60)
       (set-frame-position (selected-frame) 200 20)))
