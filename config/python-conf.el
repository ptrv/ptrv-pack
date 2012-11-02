(live-add-pack-lib "python-mode")

(setq py-install-directory
      (expand-file-name
       (file-name-as-directory
        (concat (live-pack-lib-dir) "python-mode"))))

(autoload 'python-mode (concat (live-pack-config-dir) "python-setup") "" t)
