;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar org-mode-dir (concat ptrv-pack-root-dir "lib/org/"))
(if (file-directory-p org-mode-dir)
    (progn
      (add-to-list 'load-path (concat org-mode-dir "lisp"))
      (add-to-list 'load-path (concat org-mode-dir "contrib/lisp"))
      (add-to-list 'Info-directory-list
                   (expand-file-name (concat org-mode-dir "doc")))))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-outline-path-complete-in-steps nil)
(setq org-completion-use-iswitchb nil)
(setq org-completion-use-ido t)

(setq org-log-done t)
(setq org-replace-disputed-keys nil)
(setq org-clock-into-drawer t)
(setq org-clock-idle-time 10)
(setq org-src-fontify-natively nil)
;; Override
(add-hook 'org-mode-hook
          (lambda()
            (local-set-key [(control meta return)] 'org-insert-heading)
            (local-set-key [(control shift left)] 'previous-buffer)
            (local-set-key [(control shift right)] 'next-buffer)
            ;; (local-set-key [(meta shift right)] 'ido-switch-buffer)
            ;; (local-set-key [(meta shift left)] 'magit-status)
            (auto-complete-mode -1)
            )
          )

(add-hook 'org-mode-hook 'turn-off-flyspell)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (make-variable-buffer-local 'yas/trigger-key)
;;             (org-set-local 'yas/trigger-key [tab])
;;             (define-key yas/keymap [tab] 'yas/next-field-group)))

;; (defun yas/org-very-safe-expand ()
;;   (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             ;; yasnippet (using the new org-cycle hooks)
;;             (make-variable-buffer-local 'yas/trigger-key)
;;             (setq yas/trigger-key [tab])
;;             (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
;;             (define-key yas/keymap [tab] 'yas/next-field)))

;; org-mode
(setq org-default-notes-file "~/Dropbox/org/captures.org")

;; Set to the location of your Org files on your local system
(setq org-directory "~/Dropbox/org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/MobileOrg")
;; Set files to push to org-mobile-directory
(setq org-mobile-files (quote ("~/Dropbox/org/newgtd.org"
                               "~/Dropbox/org/uni.org"
                               "~/Dropbox/org/notes.org"
                               "~/Dropbox/org/journal.org"
                               "~/Dropbox/org/master_thesis.org")))
;; Set to the files (or directory of files) you want sync'd
(setq org-agenda-files (quote ("~/Dropbox/org/newgtd.org"
                               "~/Dropbox/org/uni.org"
                               "~/Dropbox/org/master_thesis.org")))
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Dropbox/org/from-mobile.org")

(setq org-agenda-custom-commands
      '(("P" "Projects"
         ((tags "PROJECT")))
        ("H" "Home Lists"
         ((tags "HOME")
          (tags "COMPUTER")
          (tags "DVD")
          (tags "READING")))
        ("U" "Uni"
         ((tags "UNI")))
        ;; ("W" "Work Lists"
        ;;  ((tags "WORK")))
        ("D" "Daily Action List"
         ((agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)
                      ))))
        ))

(defun gtd ()
  (interactive)
  (find-file "~/Dropbox/org/newgtd.org"))
(global-set-key (kbd "C-c g") 'gtd)

;; org publish projects file
(load "~/.org-publish-projects.el" 'noerror)

;; ;; Remember.el
;; (add-to-list 'load-path (concat dotfiles-dir "/vendor/remember.el"))
;; (require 'remember)
;; (setq remember-annotation-functions '(org-remember-annotation))
;; (setq remember-handler-functions '(org-remember-handler))
;; (add-hook 'remember-mode-hook 'org-remember-apply-template)
(require 'org-latex)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
  (python . t)
  (C . t)
  (octave . t)
  (emacs-lisp . t)
  (java . t)
  (latex . t)
  ))

;; Open mailto links in gmail
(setq org-link-mailto-program
      '(browse-url "https://mail.google.com/mail/?view=cm&to=%a&su=%s"))

;; org2blog
(live-add-pack-lib "org2blog")
(require 'org2blog-autoloads)
(load "~/.org-blogs.el" 'noerror)
