(live-add-pack-lib "mk-project")
(require 'mk-project)

(make-directory (file-name-as-directory (concat live-tmp-dir "mk-project-cache")) t)
(load "~/.emacs-projects.el" 'noerror)
