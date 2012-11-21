(live-add-pack-lib "mk-project")
(require 'mk-project)

(setq projects-file "~/.emacs-projects.el")
(make-directory (file-name-as-directory (concat live-tmp-dir "mk-project-cache")) t)
(if (file-exists-p projects-file)
     (load-file projects-file))
