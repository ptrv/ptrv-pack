;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autopair-conf.el
(autopair-global-mode)

(add-hook 'lisp-mode-hook #'(lambda () (autopair-mode -1)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (autopair-mode -1)))
(add-hook 'clojure-mode-hook #'(lambda () (autopair-mode -1)))
(add-hook 'nrepl-mode-hook #'(lambda () (autopair-mode -1)))

(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(add-hook 'c++-mode-hook
          #'(lambda ()
              (push '(?< . ?>)
                    (getf autopair-extra-pairs :code))))
