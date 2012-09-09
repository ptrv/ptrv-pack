(live-add-pack-lib "emacs-faust-mode")

(setq auto-mode-alist (cons '("\\.dsp$" . faust-mode) auto-mode-alist))
(autoload 'faust-mode "faust-mode" "FAUST editing mode." t)

;; Synth-A-Modeler mode
(setq auto-mode-alist (cons '("\\.mdl$" . sam-mode) auto-mode-alist))
(autoload 'sam-mode "sam-mode" "Synth-A-Modeler editing mode." t)
