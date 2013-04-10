;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; golang-conf.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(exec-path-from-shell-copy-env "GOROOT")
(exec-path-from-shell-copy-env "GOPATH")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go-lang completion
(add-to-list 'load-path (concat (getenv "GOPATH")
                                "/src/github.com/nsf/gocode/emacs"))
(require 'go-autocomplete)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile fucntions
(defun go-build ()
  "compile project"
  (interactive)
  (compile "go build"))

(defun go-test ()
  "test project"
  (interactive)
    (compile "go test -v"))

(defun go-chk ()
  "gocheck project"
  (interactive)
    (compile "go test -gocheck.vv"))

(defun go-run ()
  "go run current buffer"
  (interactive)
    (compile (concat "go run " buffer-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks
(defun go-mode-init ()
  (make-local-variable 'before-save-hook)
  (setq before-save-hook 'gofmt-before-save)
  (hs-minor-mode 1)
  (flycheck-mode 1)
  (local-set-key (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "C-c C-c r") 'go-run)
  (define-key go-mode-map (kbd "C-c C-c b") 'go-build)
  (define-key go-mode-map (kbd "C-c C-c t") 'go-test)
  (define-key go-mode-map (kbd "C-c C-c c") 'go-chk)
  (define-key go-mode-map (kbd "C-c i") 'go-goto-imports)
  (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)
  )

(add-hook 'go-mode-hook 'go-mode-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck support
(add-to-list 'load-path (concat (getenv "GOPATH")
                                "/src/github.com/ptrv/goflymake"))
(require 'go-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Fix *Gofmt errors* window when using popwin
;; (eval-after-load 'go-mode
;;   '(progn
;;      (defadvice gofmt--process-errors (around gofmt--process-errors-new
;;                                               (filename tmpfile errbuf)
;;                                               activate)
;;        (with-current-buffer errbuf
;;          (goto-char (point-min))
;;          (insert "gofmt errors:\n")
;;          (while (search-forward-regexp (concat "^\\(" (regexp-quote tmpfile) "\\):") nil t)
;;            (replace-match (file-name-nondirectory filename) t t nil 1))
;;          (compilation-mode)
;;          (display-buffer errbuf)))))

(eval-after-load "flycheck"
  '(progn
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
       :modes 'go-mode)

     (add-to-list 'flycheck-checkers 'go-goflycheck)))
