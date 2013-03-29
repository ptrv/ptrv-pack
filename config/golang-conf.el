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
(add-hook 'go-mode-hook 'hs-minor-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda ()
                          (local-set-key (kbd "M-.") 'godef-jump)))
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
     (define-key go-mode-map (kbd "C-c C-p") 'go-goto-imports)
     (define-key go-mode-map (kbd "C-c C-z") 'go-remove-unused-imports)
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck support
(add-to-list 'load-path (concat (getenv "GOPATH")
                                "/src/github.com/ptrv/goflymake"))
(require 'go-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fix *Gofmt errors* window when using popwin
(eval-after-load 'go-mode
  '(progn
     (defadvice gofmt--process-errors (around gofmt--process-errors-new
                                              (filename tmpfile errbuf)
                                              activate)
       (with-current-buffer errbuf
         (goto-char (point-min))
         (insert "gofmt errors:\n")
         (while (search-forward-regexp (concat "^\\(" (regexp-quote tmpfile) "\\):") nil t)
           (replace-match (file-name-nondirectory filename) t t nil 1))
         (compilation-mode)
         (display-buffer errbuf)))))
