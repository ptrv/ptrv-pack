;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SuperCollider
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(live-add-pack-lib "SuperCollider")

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
 '(pop-up-frames nil)
 '(pop-up-windows t)
 '(sclang-auto-scroll-post-buffer nil)
 '(sclang-eval-line-forward nil)
 '(sclang-help-path (quote ("/usr/local/share/SuperCollider/Help" "~/.local/share/SuperCollider/Extensions")))
 ;; '(sclang-help-path (quote ("~/compiledir/supercollider/build/HelpBase" "~/share/SuperCollider/Extensions/Help")))
 '(sclang-library-configuration-file "~/.sclang.cfg")
 '(sclang-runtime-directory "~/scwork/")
 '(sclang-server-panel "Server.local.makeGui.window.bounds = Rect(5,5,288,98)")
 '(show-paren-mode t nil (paren))
 '(transient-mark-mode t)
 '(w3m-pop-up-frames t)
 '(w3m-pop-up-windows nil)
 '(inhibit-startup-screen t)
 (custom-set-faces
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

(defun sclang-mode-untabify ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
  nil)

(add-hook 'sclang-mode-hook
          '(lambda ()
             (make-local-variable 'write-contents-hooks)
             (add-hook 'write-contents-hooks 'sclang-mode-untabify)))

(require 'ext-scel)
(setq sclang-minibuf-results nil)

;; Raise all supercollider windows.
(define-key sclang-mode-map (kbd "C-c f")
  (lambda ()
    (interactive)
    (sclang-eval-string "Window.allWindows.do(_.front);")))
