;; misc-conf.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; session.el
;; save a list of open files in $HOME/.emacs.desktop
;; save the desktop file automatically if it already exists
(setq desktop-save (quote if-exists))
(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc

;; default browser
(setq browse-url-generic-program (executable-find "google-chrome")
      browse-url-browser-function 'browse-url-generic)

;; ido recent file
(setq ido-max-directory-size 100000)

(setq sentence-end-double-space nil)

;; follow version controlled symlinks automatically
(setq vc-follow-symlinks t)

(setq live-disable-zone t)

;; debug messages
(setq debug-on-error nil)

;; scroll compilation buffer
;;(setq compilation-scroll-output 'first-error)
(setq compilation-scroll-output t)

;; disabled commands
(put 'downcase-region 'disabled nil)
(put 'updacase-region 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gist.el
(setq gist-view-gist t)
(add-to-list 'gist-supported-modes-alist '(processing-mode . "pde"))
(add-to-list 'gist-supported-modes-alist '(conf-mode . "desktop"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
(setq ac-dwim t)
(setq ac-ignore-case nil)
;;(setq ac-auto-start 3)
(setq ac-auto-show-menu t)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fix tramp backups
(add-to-list 'bkup-backup-directory-info
             (list tramp-file-name-regexp ""))
(setq tramp-bkup-backup-directory-info bkup-backup-directory-info)

;; ;;disable backups of files edited with tramp
;;(setq tramp-bkup-backup-directory-info nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;; newline after 72 chars in magit-log-edit-mode
(add-hook 'magit-log-edit-mode-hook
          (lambda ()
             (set-fill-column 72)
             (auto-fill-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Soft word wrap
;; (require 'longlines)
;; (setq longlines-show-hard-newlines t)
;; (setq longlines-auto-wrap t)
;; ;; (add-hook 'LaTeX-mode-hook 'longlines-mode)
;; ;; (add-hook 'markdown-mode-hook 'longlines-mode)


(live-add-pack-lib "arduino-mode")
(require 'arduino-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; refheap
(require 'my-secrets)
(require 'refheap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert-time
(live-add-pack-lib "insert-time")
(require 'insert-time)
(setq insert-date-format "%Y-%m-%d")
(setq insert-time-format "%H:%M:%S")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pomodoro.el
(require 'pomodoro)
(pomodoro-add-to-mode-line)
(setq pomodoro-sound-player "/usr/bin/aplay")
(setq pomodoro-break-start-sound (concat ptrv-pack-root-dir "etc/sounds/alarm.wav"))
(setq pomodoro-work-start-sound (concat ptrv-pack-root-dir "etc/sounds/alarm.wav"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ChucK
(live-add-pack-lib "chuck-mode")
(require 'chuck-mode)
;;(yas-load-directory (concat (live-pack-lib-dir) "chuck-mode/snippets/text-mode"))

;; fix whitespace-cleanup
;; http://stackoverflow.com/a/12958498/464831
(defadvice whitespace-cleanup (around whitespace-cleanup-indent-tab
                                      activate)
  "Fix whitespace-cleanup indent-tabs-mode bug"
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
        (whitespace-tab-width tab-width))
    ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; the silver searcher
(require 'ag)
(setq ag-highlight-search t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; switch between sqlite3 and spatialite excutable
(defun sql-switch-spatialite-sqlite ()
  (interactive)
  (let* ((sqlprog sql-sqlite-program)
         (change (if (string-match "sqlite" sqlprog)
                     (executable-find "spatialite")
                   (executable-find "sqlite3"))))
    (setq sql-sqlite-program change)
    (message "sql-sqlite-program changed to %s" change)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(live-add-pack-lib "misc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pure-mode
(autoload 'pure-mode "pure-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.pure\\(rc\\)?$" . pure-mode))
(eval-after-load "pure-mode"
  '(progn
     ;; (require 'hideshow)
     (add-hook 'pure-mode-hook 'hs-minor-mode)
     (add-hook 'pure-eval-mode-hook
               (lambda ()
                 (define-key pure-eval-mode-map [up] 'comint-previous-input)
                 (define-key pure-eval-mode-map [down] 'comint-next-input)))
     (define-key pure-mode-map (kbd "C-c M-p") 'run-pure)
     (define-key pure-mode-map (kbd "C-x M-p") 'pure-scratchpad)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mercurial
(require 'ahg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lambda-mode
(require 'lambda-mode)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

(add-hook 'emacs-lisp-mode-hook #'lambda-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'smart-operator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tea-time
(require 'tea-time)
(setq tea-time-sound (concat ptrv-pack-root-dir "etc/sounds/alarm.wav"))
(cond
 ((eq system-type 'darwin)
  (setq tea-time-sound-command "afplay %s"))
 ((eq system-type 'gnu/linux)
  (setq tea-time-sound-command "aplay %s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; xah lee modes
(autoload 'xmsi-mode "xmsi-math-symbols-input"
  "Load xmsi minor mode for inputting math/Unicode symbols." t)
(eval-after-load "xmsi-math-symbols-input"
  '(progn
     (define-key xmsi-keymap (kbd "S-SPC") 'nil)
     (define-key xmsi-keymap (kbd "C-c C-8") 'xmsi-change-to-symbol)))


;; xub-mode
(autoload 'xub-mode "xub-mode" "Load xub-mode for browsing Unicode." t)
(defalias 'unicode-browser 'xub-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display visited file's path as frame title
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
