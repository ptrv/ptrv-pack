(live-add-pack-lib "processing2-mode")

(setq processing-location "~/applications/processing-2.0/processing-java")
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(autoload 'processing-mode "processing-mode" "Processing mode" t)

;; (add-hook 'processing-mode-hook '(lambda ()
;;                                    (setq c-basic-offset 2)
;;                                    (set (make-local-variable 'tab-width) 2)
;;                                    (set (make-local-variable 'tab-stop-list)
;;                                         '(2 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84))))
(add-hook 'processing-mode-hook '(lambda ()
                                   (setq c-basic-offset 2)
                                   (set (make-local-variable 'tab-width) 2)))