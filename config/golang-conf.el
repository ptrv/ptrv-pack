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
;; hooks
(add-hook 'go-mode-hook (lambda ()
                          (make-local-variable 'before-save-hook)
                          (setq before-save-hook 'gofmt-before-save)))
(add-to-list 'go-mode-hook 'hs-minor-mode)
(add-to-list 'go-mode-hook 'flycheck-mode)

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

(eval-after-load "go-mode"
  '(progn
     (define-key go-mode-map (kbd "C-c C-r") 'go-run)
     (define-key go-mode-map (kbd "C-c C-b") 'go-build)
     (define-key go-mode-map (kbd "C-c C-t") 'go-test)
     (define-key go-mode-map (kbd "C-c C-u") 'go-chk)

     ;; flycheck goflymake checker
     (flycheck-declare-checker go-goflymake
             "A Go syntax and style checker using the go utility.

See URL `https://github.com/ptrv/goflymake'."
             :command '("goflymake" "flycheck-" source-inplace)
             :error-patterns '(("^\\(?1:.*\\):\\(?2:[0-9]+\\): \\(?4:.*\\)$" error))
             :modes 'go-mode)
     (add-to-list 'flycheck-checkers 'go-goflymake)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
