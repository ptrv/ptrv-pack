;; ido-ubiquitous-conf.el

;; Use ido everywhere
(ido-ubiquitous-mode 1)
(ido-ubiquitous-disable-in erc-iswitchb)

(add-to-list 'ido-ubiquitous-command-exceptions 'sh-set-shell)
(add-to-list 'ido-ubiquitous-command-exceptions 'ispell-change-dictionary)
(add-to-list 'ido-ubiquitous-command-exceptions 'add-dir-local-variable)
(add-to-list 'ido-ubiquitous-command-exceptions 'ahg-do-command)
;;(add-to-list 'ido-ubiquitous-command-exceptions 'godoc)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
;; (ido-ubiquitous-use-new-completing-read yas-expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas-visit-snippet-file 'yasnippet)
