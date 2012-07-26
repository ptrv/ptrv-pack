(require 'package)

(custom-set-variables
 '(package-user-dir "~/.live-packs/ptrv-pack/elpa"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(magit
                      auctex
                      wrap-region
                      go-mode
                      starter-kit-eshell
                      ack-and-a-half)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
