;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck-conf.el

(eval-after-load "flycheck"
  '(progn
     (setq flycheck-highlighting-mode 'lines)))

(unless (and (>= emacs-major-version 24)
             (>= emacs-minor-version 3))
  (add-to-list 'debug-ignored-errors "\\`No more Flycheck errors\\'")
  (add-to-list 'debug-ignored-errors "\\`Flycheck mode disabled\\'")
  (add-to-list 'debug-ignored-errors "\\`Configured syntax checker .* cannot be used\\'"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))
(add-hook 'lisp-interaction-mode (lambda () (flycheck-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load "flycheck"
  '(progn
     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; alternative Go checkers
     (defun go-flycheck-check-command ()
       "Build go checker command depending on the type of source file to check."
       (let ((src-tmp-file (file-name-nondirectory
                            (flycheck-substitute-argument 'source-inplace)))
             (src-orig (file-name-nondirectory (buffer-file-name)))
             (go-list-format (concat "{{range .GoFiles}}{{.}},{{end}}"
                                     "{{range .CgoFiles}}{{.}},{{end}}"))
             args files-list go-list-result)
         (if (s-ends-with? "_test.go" src-tmp-file)
             (progn
               (setq args (list "test" "-c"))
               (setq go-list-format (concat go-list-format
                                            "{{range .TestGoFiles}}{{.}},{{end}}")))
           (setq args (list "build" "-o" "/dev/null")))
         (setq args (append args `(,src-tmp-file)))
         (setq go-list-result (process-lines "go" "list" "-f" go-list-format))
         (setq files-list (s-split "," (car go-list-result) t))
         (setq files-list (remove src-orig files-list))
         (setq files-list (remove src-tmp-file files-list))
         (setq args (append args files-list))))

     (flycheck-declare-checker go-goflycheck
       "A Go syntax and style checker using the go compiler.

       See URL `https://golang.org/cmd/go'."
       :command (append (list "go") `((eval (go-flycheck-check-command))))
       :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
       :modes 'go-mode
       :predicate '(not (buffer-modified-p)))

     (add-to-list 'flycheck-checkers 'go-goflycheck)

     (flycheck-declare-checker go
       "A Go syntax and style checker using the gofmt utility go build tools.

       See URL `http://golang.org/cmd/gofmt/'."
       :command '("gofmt" source)
       :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\):\\(?3:[0-9]+\\): \\(?4:.*\\)$" error))
       :modes 'go-mode
       :next-checkers '((no-errors . go-goflycheck)))

     (add-to-list 'flycheck-checkers 'go)

     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ;; cpplint.py checker
     (flycheck-declare-checker c++-cpplint
       "A cpp syntax checker using the cpplin.py tool.

       See URL `https://code.google.com/p/google-styleguide/'."
       :command '("cpplint.py" "--verbose=3" source-original)
       :error-patterns '(("^\\(?1:.*\\):\\(?2:.*\\):  \\(?4:.*\\)$" warning))
       :modes 'c++-mode)

     (add-to-list 'flycheck-checkers 'c++-cpplint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
