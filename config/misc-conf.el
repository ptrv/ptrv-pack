;; misc-conf.el

;; session.el
;; save a list of open files in $HOME/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save (quote if-exists))
(desktop-save-mode 1)

;; ;; Flymake
;; (add-hook 'find-file-hook 'flymake-find-file-hook)

;; ido recent file
(setq ido-max-directory-size 100000)

(setq sentence-end-double-space nil)

;; gist.el
(setq gist-view-gist t)
(add-to-list 'gist-supported-modes-alist '(processing-mode . "pde"))
(add-to-list 'gist-supported-modes-alist '(conf-mode . "desktop"))


;; follow version controlled symlinks automatically
(setq vc-follow-symlinks t)

;; add snippets
(yas-load-directory (concat (live-pack-lib-dir) "snippets"))

(setq live-disable-zone t)

;; debug messages
(setq debug-on-error nil)

;; scroll compilation buffer
;;(setq compilation-scroll-output 'first-error)
(setq compilation-scroll-output t)

;; fix tramp backups
(add-to-list 'bkup-backup-directory-info
             (list tramp-file-name-regexp ""))
(setq tramp-bkup-backup-directory-info bkup-backup-directory-info)

;; ;;disable backups of files edited with tramp
;;(setq tramp-bkup-backup-directory-info nil)

;; hippie-expand functions list
;; http://trey-jackson.blogspot.de/2007/12/emacs-tip-5-hippie-expand.html
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        ))

;; newline after 72 chars in magit-log-edit-mode
(add-hook 'magit-log-edit-mode-hook
          (lambda ()
             (set-fill-column 72)
             (auto-fill-mode 1)))

;; ;; Soft word wrap
;; (require 'longlines)
;; (setq longlines-show-hard-newlines t)
;; (setq longlines-auto-wrap t)
;; ;; (add-hook 'LaTeX-mode-hook 'longlines-mode)
;; ;; (add-hook 'markdown-mode-hook 'longlines-mode)

(require 'webjump)
(setq webjump-sites
       (append '(("Urban Dictionary" .
                  [simple-query
                   "www.urbandictionary.com"
                   "http://www.urbandictionary.com/define.php?term="
                   ""])
                 ("stackoverflow" .
                  [simple-query
                   "www.stackoverflow.com"
                   "http://stackoverflow.com/search?q="
                   ""])
                 ("askubuntu" .
                  [simple-query
                   "www.askubuntu.com"
                   "http://askubuntu.com/search?q="
                   ""])
                 ("superuser" .
                  [simple-query
                   "www.superuser.com"
                   "http://superuser.com/search?q="
                   ""])
                 ("tex.stackexchange" .
                  [simple-query
                   "tex.stackexchange.com"
                   "http://tex.stackexchange.com/search?q="
                   ""])
                 ("math.stackexchange" .
                  [simple-query
                   "math.stackexchange.com"
                   "http://math.stackexchange.com/search?q="
                   ""])
                 ("leo" .
                  [simple-query
                   "dict.leo.org"
                   "http://dict.leo.org/ende?search="
                   ""])
                 ("Java API" .
                  [simple-query
                   "www.google.com"
                   "http://www.google.ca/search?hl=en&as_sitesearch=http://java.sun.com/javase/6/docs/api/&q="
                   ""]))
               webjump-sample-sites))

(live-add-pack-lib "arduino-mode")
(require 'arduino-mode)

(setq browse-url-generic-program (executable-find "google-chrome")
      browse-url-browser-function 'browse-url-generic)

;; lua-mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)
(ido-ubiquitous-disable-in erc-iswitchb)

(add-to-list 'ido-ubiquitous-command-exceptions 'sh-set-shell)
(add-to-list 'ido-ubiquitous-command-exceptions 'ispell-change-dictionary)
(add-to-list 'ido-ubiquitous-command-exceptions 'add-dir-local-variable)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
;; (ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)

;; iflipb
(require 'iflipb)
(setq iflipb-ignore-buffers '("*Ack-and-a-half*"
                              "*Help*"
                              "*Compile-Log*"
                              "*Ibuffer*"
                              "*Messages*"
                              "*scratch*"
                              "*Completions*"
                              "*magit"
                              "*Pymacs*"
                              "*clang-complete*"
                              "*compilation*"
                              "*Packages*"
                              "TAGS"
                              "*file-index*"
                              " output*"
                              "*tramp/"
                              "*project-status*"
                              ))
(setq iflipb-wrap-around t)

;; pure-mode
(when (require 'pure-mode nil 'noerror)
  (require 'pure-mode)
  (add-to-list 'auto-mode-alist '("\\.pure\\(rc\\)?$" . pure-mode))
  (require 'hideshow)
  (add-hook 'pure-mode-hook 'hs-minor-mode)
  (add-hook 'pure-eval-mode-hook
            (lambda ()
              (define-key pure-eval-mode-map [up] 'comint-previous-input)
              (define-key pure-eval-mode-map [down] 'comint-next-input)))

  (define-key pure-mode-map (kbd "C-c M-p") 'run-pure)
  (define-key pure-mode-map (kbd "C-x M-p") 'pure-scratchpad))

;; refheap
(require 'secrets)
(require 'refheap)

;; disabled commands
(put 'downcase-region 'disabled nil)
(put 'updacase-region 'disabled nil)

(live-add-pack-lib "insert-time.el")
(require 'insert-time)
(setq insert-date-format "%Y-%m-%d")
(setq insert-time-format "%H:%M:%S")
