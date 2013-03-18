;; typewriter-conf.el

(defvar typewriter-mode nil)
;;(setq typewriter-mode t)

;; (defun play-typewriter-sound ()
;;   (if (equal (this-command-keys) (kbd "RET"))
;;       nil
;;     (let ((data-directory "~/.live-packs/ptrv-pack/etc/sounds"))
;;       (play-sound `(sound :file "9744__horn__typewriter.wav")))))

;; (defun play-typewriter-end ()
;;   (let ((data-directory "~/.live-packs/ptrv-pack/etc/sounds"))
;;     (if (eq (current-column) 80)
;;         (play-sound `(sound :file "eol-bell.wav")))))

;; (defun play-typewriter-return ()
;;   (if (equal (this-command-keys) (kbd "RET"))
;;       (let ((data-directory "~/.live-packs/ptrv-pack/etc/sounds"))
;;         (play-sound `(sound :file "carriage-return.wav")))))

;; Play typing sound
(defun play-typewriter-sound ()
  (if (equal (this-command-keys) (kbd "RET"))
      nil
    (start-process-shell-command "typewriter" nil
                                 (concat
                                  "aplay "
                                  ptrv-pack-root-dir
                                  "etc/sounds/9744__horn__typewriter.wav"))))

;; Play bell when cursor is at column 80
(defun play-typewriter-end ()
  (if (eq (current-column) 80)
      (start-process-shell-command "typewriter" nil
                                   (concat
                                    "aplay "
                                    ptrv-pack-root-dir
                                    "etc/sounds/eol-bell.wav"))))

;; Return sound
(defun play-typewriter-return ()
  (if (equal (this-command-keys) (kbd "RET"))
      (start-process-shell-command "typewriter" nil
                                   (concat
                                    "aplay "
                                    ptrv-pack-root-dir
                                    "etc/sounds/carriage-return.wav"))))

(defun toggle-typewriter ()
  (interactive)
  (if (eq typewriter-mode t)
      (progn
        (remove-hook 'post-self-insert-hook 'play-typewriter-sound)
        (remove-hook 'post-command-hook 'play-typewriter-end)
        (remove-hook 'post-command-hook 'play-typewriter-return)
        (setq typewriter-mode nil))
    (progn
      (add-hook 'post-self-insert-hook 'play-typewriter-sound)
      (add-hook 'post-command-hook 'play-typewriter-end)
      (add-hook 'post-command-hook 'play-typewriter-return)
      (setq typewriter-mode t))))

(defun typewriter-mode-on ()
  (if (eq typewriter-mode nil)
      (toggle-typewriter)))

(if (not (eq typewriter-mode nil))
    (typewriter-mode-on))
