;; Latex
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
(add-hook 'LaTeX-mode-hook 'reftex-mode)

(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)

(eval-after-load "tex"
   '(add-to-list 'TeX-command-list
                 '("latexmk"
                    "latexmk %s"
                    TeX-run-TeX nil t) t))

(defun okular-make-url () (concat
                           "file://"
                           (expand-file-name (funcall file "pdf" t)
                                             (file-name-directory (TeX-master-file)))
                           "#src:"
                           (TeX-current-line) (buffer-file-name)))

(cond ((eq system-type 'gnu/linux)
       (setq TeX-view-program-list '(("Okular" "okular --unique %u")))
       (setq TeX-view-program-selection '((output-pdf "Okular") (output-dvi "Okular")))
       (add-hook 'LaTeX-mode-hook
                 (lambda ()
                   (add-to-list 'TeX-expand-list
                                '("%u" okular-make-url)))))
      ((eq system-type 'darwin)
       (setq TeX-view-program-selection '((output-pdf "Skim")))
       (setq TeX-view-program-list
             '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b")))
       ))


(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(live-add-pack-lib "auto-complete-auctex")
(require 'auto-complete-auctex)
