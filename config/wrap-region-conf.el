;; activate wrap-region mode
(wrap-region-global-mode t)

(add-to-list 'wrap-region-except-modes 'magit-status-mode)
(add-to-list 'wrap-region-except-modes 'ibuffer-mode)

;; Custom wrappers

(wrap-region-add-wrapper "*" "*" "*" 'markdown-mode)

(wrap-region-add-wrapper "*" "*" "*" 'org-mode)
(wrap-region-add-wrapper "/" "/" "/" 'org-mode)
(wrap-region-add-wrapper "_" "_" "_" 'org-mode)
(wrap-region-add-wrapper "=" "=" "=" 'org-mode)
(wrap-region-add-wrapper "~" "~" "~" 'org-mode)
(wrap-region-add-wrapper "+" "+" "+" 'org-mode)
