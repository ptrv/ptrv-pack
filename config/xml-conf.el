;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xml-conf.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.gpx$" . nxml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load 'nxml-mode
  '(progn
     ;; make nxml outline work with gpx files
     (defun gpx-setup ()
       (when (and (stringp buffer-file-name)
                  (string-match "\\.gpx\\'" buffer-file-name))
         (make-local-variable 'nxml-section-element-name-regexp)
         (setq nxml-section-element-name-regexp "trk\\|trkpt\\|wpt")
         (make-local-variable 'nxml-heading-element-name-regexp)
         (setq nxml-heading-element-name-regexp "name\\|time")
         ))
     (add-hook 'nxml-mode-hook 'gpx-setup)

     ;; typing a slash automatically completes the end-tag
     (setq nxml-slash-auto-complete-flag t)

     ;; treat an element as a single expression instead of only tag
     (setq nxml-sexp-element-flag t)
     ))
