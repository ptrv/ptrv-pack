(require 'package)

(custom-set-variables
 '(package-user-dir (concat ptrv-pack-root-dir "elpa")))

(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ;;("org" . "http://orgmode.org/elpa/")
                  ))
  (add-to-list 'package-archives source t))

(package-initialize)

(defadvice package-compute-transaction (before package-compute-transaction-reverse
                                               (package-list requirements)
                                               activate compile)
  "reverse the requirements"
  (setq requirements (reverse requirements))
  (print requirements))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar ptrv-packages '(magit
                        ;;auctex
                        wrap-region
                        go-mode
                        starter-kit-eshell
                        ack-and-a-half
                        session
                        notify
                        edit-server
                        color-theme
                        ;;org-plus-contrib
                        markdown-mode
                        js2-mode
                        apache-mode
                        yaml-mode
                        xml-rpc
                        pytest
                        git-commit-mode
                        gitignore-mode
                        gitconfig-mode
                        gnuplot
                        dtrt-indent
                        exec-path-from-shell
                        s
                        dash
                        ido-ubiquitous
                        refheap
                        epc
                        iflipb
                        find-file-in-project
                        pomodoro
                        flycheck
                        pandoc-mode
                        iedit
                        glsl-mode
                        ag
                        cmake-mode
                        pylint
                        lua-mode
                        ahg
                        autopair
                        tea-time
                        json-mode
                        iy-go-to-char
                        ;;projectile
                        popwin
                        edit-server-htmlize
                        move-text
                        pcmpl-git
                        multiple-cursors
                        expand-region
                        w3m
                        flymake-cursor
                        )
  "A list of packages to ensure are installed at launch.")

(dolist (p ptrv-packages)
  (when (not (package-installed-p p))
    (package-install p)))
