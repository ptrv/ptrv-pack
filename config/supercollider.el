(live-add-pack-lib "SuperCollider")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SuperCollider
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sclang)
(require 'w3m)

(eval-after-load "w3m"
  '(progn
     (define-key w3m-mode-map [left] 'backward-char)
     (define-key w3m-mode-map [right] 'forward-char)
     (define-key w3m-mode-map [up] 'previous-line)
     (define-key w3m-mode-map [down] 'next-line)
     (setq w3m-auto-show 1)
     ))

;; (setq w3m-key-binding 'info)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(color-theme-selection "Oswald" nil (color-theme))
 '(emacs-goodies-el-defaults t)
 '(pop-up-frames nil)
 '(pop-up-windows t)
 '(sclang-auto-scroll-post-buffer nil)
 '(sclang-eval-line-forward nil)
 '(sclang-help-path (quote ("/usr/local/share/SuperCollider/Help" "~/.local/share/SuperCollider/Extensions")))
 ;; '(sclang-help-path (quote ("~/compiledir/supercollider/build/HelpBase" "~/share/SuperCollider/Extensions/Help")))
 '(sclang-library-configuration-file "~/.sclang.cfg")
 ;; '(sclang-runtime-directory "~/share/SuperCollider/")
 '(sclang-runtime-directory "~/scwork/")
 ;; '(sclang-server-panel "Server.default.makeGui")
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(w3m-pop-up-frames t)
 '(w3m-pop-up-windows nil)
 '(inhibit-startup-screen t)
 (custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  ))

;; ##### extension for block error messages ####
;;(load-file (concat (live-pack-lib-dir) "ext-scel.el"))

(add-hook 'sclang-mode-hook
          (lambda ()
            (setq skeleton-pair t)
            (local-set-key (kbd "(") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "{") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "[") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
            (local-set-key (kbd "'") 'skeleton-pair-insert-maybe)
            ))

(add-hook 'sclang-mode-hook
          (lambda ()
            (setq ac-sources
                  '(ac-source-dictionary
                    ac-source-words-in-buffer
                    ac-source-words-in-same-mode-buffers
                    ac-source-words-in-all-buffer
                    ;;ac-source-yasnippet
                    ac-source-semantic))))

(add-to-list 'ac-modes 'sclang-mode)
(add-to-list 'ac-user-dictionary-files "~/.local/share/SuperCollider/sclang_completion_dict")

(add-hook 'sclang-mode-hook 'yas/minor-mode)
(yas/load-directory (concat (live-pack-lib-dir) "snippets"))

;; (defun sclang-mode-untabify ()
;;   (save-excursion
;;     (goto-char (point-min))
;;     (while (re-search-forward "[ \t]+$" nil t)
;;       (delete-region (match-beginning 0) (match-end 0)))
;;     (goto-char (point-min))
;;     (if (search-forward "\t" nil t)
;;         (untabify (1- (point)) (point-max))))
;;   nil)

;; (add-hook 'sclang-mode-hook
;;           '(lambda ()
;;              (make-local-variable 'write-contents-hooks)
;;              (add-hook 'write-contents-hooks 'sclang-mode-untabify)))

(require 'ext-scel)
