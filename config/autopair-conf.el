;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair-conf.el
(autopair-global-mode)

(defadvice enable-paredit-mode (before disable-autopair activate)
  (setq autopair-dont-activate t)
  (autopair-mode -1))

(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (push '(?< . ?>)
                    (getf autopair-extra-pairs :code))))
