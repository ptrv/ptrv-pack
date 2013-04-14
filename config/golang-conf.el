;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; golang-conf.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(exec-path-from-shell-copy-env "GOROOT")
(exec-path-from-shell-copy-env "GOPATH")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; go-lang completion
(add-to-list 'load-path (concat
                         (car (split-string (getenv "GOPATH") ":"))
                         "/src/github.com/nsf/gocode/emacs"))
(require 'go-autocomplete)

(defun go-dot-complete ()
  "Insert dot and complete code at point."
  (interactive)
  (insert ".")
  (unless (ac-cursor-on-diable-face-p)
    (auto-complete)))

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
  "go run current package"
  (interactive)
  (let (files-list
        go-list-result
        go-list-result-list)
    ;; get package files as list
    (setq go-list-result-list
          (s-split ","
                   (car (process-lines
                         "go" "list" "-f"
                         "{{range .GoFiles}}{{.}},{{end}}"))
                   t))
    ;; escape space in file names
    (setq go-list-result
          (loop for i in go-list-result-list
                collect (s-replace " " "\\ " i)))
    (setq files-list (s-join " " go-list-result))
    (compile (concat "go run " files-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hooks
(defun go-mode-init ()
  (make-local-variable 'before-save-hook)
  (setq before-save-hook 'gofmt-before-save)
  (hs-minor-mode 1)
  ;;(flycheck-mode-on-safe)
  (local-set-key (kbd "M-.") 'godef-jump)
  (define-key go-mode-map (kbd "C-c C-c r") 'go-run)
  (define-key go-mode-map (kbd "C-c C-c b") 'go-build)
  (define-key go-mode-map (kbd "C-c C-c t") 'go-test)
  (define-key go-mode-map (kbd "C-c C-c c") 'go-chk)
  (define-key go-mode-map (kbd "C-c i") 'go-goto-imports)
  (define-key go-mode-map (kbd "C-c C-r") 'go-remove-unused-imports)
  (define-key go-mode-map "." 'go-dot-complete))

(add-hook 'go-mode-hook 'go-mode-init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck support
(add-to-list 'load-path (concat
                         (car (split-string (getenv "GOPATH") ":"))
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
