;;; pure-mode.el --- as simple Pure mode -*- Emacs-Lisp -*-

;; Author/Maintainer: Albert Graef <Dr.Graef@t-online.de>

;; Distributed under GPL V3 (or later; see the accompanying COPYING file).

;; INSTALLATION: If necessary, edit the values of the `pure-prog' and
;; `pure-libdir' variables below. This usually isn't necessary if the PURELIB
;; environment variable is set.

(defvar pure-prog "/usr/local/bin/pure")
(defvar pure-libdir "/usr/local/lib/pure")

;; Then copy this file to your site-lisp directory. In addition, to make Pure
;; mode available and enable it for *.pure and *.purerc files, you'll have to
;; add the following to your emacs startup file:

;; (require 'pure-mode)
;; (setq auto-mode-alist
;;       (cons '("\\.pure\\(rc\\)?$" . pure-mode) auto-mode-alist))

;; Furthermore, you can enable font lock (syntax highlighting) as follows:

;; (add-hook 'pure-mode-hook 'turn-on-font-lock)
;; (add-hook 'pure-eval-mode-hook 'turn-on-font-lock)

;; Pure mode has support for code folding using the hideshow minor mode. To
;; enable this, load hideshow before Pure mode in your .emacs:

;; (require 'hideshow)

;; You can then invoke hideshow manually using M-x hs-minor-mode. To have
;; hs-minor-mode enabled in all your Pure mode buffers, add the following to
;; your .emacs:

;; (add-hook 'pure-mode-hook 'hs-minor-mode)

;; Using the Pure-Eval hook you can also rebind the cursor up and down keys to
;; the history cycling commands:

;; (add-hook 'pure-eval-mode-hook
;;	   (lambda ()
;;	     (define-key pure-eval-mode-map [up] 'comint-previous-input)
;;	     (define-key pure-eval-mode-map [down] 'comint-next-input)))

;; Finally, you might wish to add some global key bindings, such as:

;; (global-set-key "\C-c\M-p" 'run-pure)
;; (global-set-key "\C-x\M-p" 'pure-scratchpad)

;; Note that Pure and Pure-Eval mode provide these bindings anyway and the
;; commands are also available from the Pure menu, but the global bindings
;; above may be convenient to enter Pure or Pure-Eval mode from other buffers.
;; This is useful, in particular, if Emacs is invoked without a Pure script.

;; The run-pure command invokes the Pure interpreter without a script in a
;; comint buffer named *pure-eval*. Another method to invoke the interpreter
;; is to open a script file and use the command pure-run-script (C-c C-k in
;; Pure mode).

;; The pure-scratchpad command opens a scratch buffer named *pure-scratch* and
;; puts it in Pure mode. This buffer can be used like any other Pure mode
;; buffer but isn't associated with a file (similar to the Emacs Lisp buffer
;; *scratch*). Thus you can't "run" the *pure-scratch* buffer with C-c C-k
;; (unless you save it to a file first), but you can use it to feed Pure code
;; into a running interpreter instance with the pure-send-line (C-c C-c) and
;; pure-send-region (C-c C-r) commands.

;; Another useful command to always remember is the pure-help command (C-c h
;; in Pure and Pure-Eval mode) which lets you read the online manual inside
;; emacs if emacs-w3m is installed (see below). Always use this command
;; instead of the Pure interpreter's 'help' command. (The latter will try to
;; run the command line version of w3m which doesn't work all that well in an
;; Emacs buffer.)

;; The online help facility uses emacs-w3m to display help files in html
;; format, if it is installed and loaded. To these ends, put the following
;; line into your .emacs (make sure that this comes before Pure mode):

;; (require 'w3m-load)

;; Otherwise Emacs' generic browse-url function will be used, which usually
;; invokes some external browser by default.

;; --------------------------------------------------------------------------

;; Try to figure out where interpreter and standard library are located. This
;; relies on the PURELIB environment variable being set correctly.

(let ((libpath (getenv "PURELIB")) (path (getenv "PATH")))
  (if libpath
      (let* ((prefix1 (file-name-directory libpath))
	     (prefix2 (file-name-directory (directory-file-name prefix1)))
	     ;; On most systems the interpreter is located in ../../bin.
	     (exepath1 (concat prefix2 "bin/pure"))
	     ;; Windows/Msys:
	     (exepath2 (concat prefix2 "bin/pure.exe"))
	     ;; Windows (Pure MSI package):
	     (exepath3 (concat prefix1 "pure.exe"))
	     (exepath
	      (cond
	       ((file-exists-p exepath1) exepath1)
	       ((file-exists-p exepath2) exepath2)
	       ((file-exists-p exepath3) exepath3)
	       (t exepath1))))
	(setq pure-prog exepath)
	(setq pure-libdir libpath)
	))
  ;; If we haven't found the executable yet, try to locate it on PATH.
  (unless (or (file-exists-p pure-prog) (null path))
    (let ((exe1 (locate-library "pure" t (parse-colon-path path)))
	  (exe2 (locate-library "pure.exe" t (parse-colon-path path))))
      (cond
       (exe1 (setq pure-prog exe1))
       (exe2 (setq pure-prog exe2))
       )))
  )

;; customizable variables

(defgroup pure nil "Major mode for editing and running Pure scripts."
  :group 'languages)

;; indentation and editing style

(defcustom pure-tab-always-indent t
  "Handling of TAB commands in `pure-mode'.
Non-nil means TAB in `pure-mode' should always reindent the
current line.  If this is nil, TAB inserts a tab if it is at the
end of the line and follows non-whitespace text."
  :type 'boolean
  :group 'pure)

(defcustom pure-electric-keys t
  "Enable or disable electric keys in `pure-mode'.
Non-nil means that keys like `=', `;', `(', `)' etc. do automatic
indentation and alignment. Note that if this is nil then
`pure-align-equals' doesn't have any effect either."
  :type 'boolean
  :group 'pure)

(defcustom pure-indent-equals t
  "Enable or disable indentation of right-hand sides of equations.
Non-nil means that `=' at the beginning of a line is indented so
that it aligns with a previous equation at the same block level,
and the right-hand sides of equations are indented relative to
`='. This is also known as ``hanging indent''. Setting this
value to nil does normal block indentation of equations instead."
  :type 'boolean
  :group 'pure)

(defcustom pure-align-equals t
  "Enable or disable alignment of `=' in equations.
Non-nil means that typing `=' at the end of a line aligns it with
a previous equation at the same block level. Note: This setting
is independent of `pure-indent-equals'."
  :type 'boolean
  :group 'pure)

(defcustom pure-block-indent 2
  "Indentation of Pure code with respect to containing block."
  :type 'integer
  :group 'pure)

(defcustom pure-extra-indent 0
  "Extra block indentation at the beginning of a line."
  :type 'integer
  :group 'pure)

(defcustom pure-hanging-comment-starter-p t
  "Handling of Pure block comment starters with \\[fill-paragraph].
When set to nil, Pure block comment starters are left on their own line.
When set to t, text that follows a block comment starter will be
placed on the same line as the block comment starter (i.e. the text
`hangs' on that line)."
  :type 'boolean
  :group 'pure)

(defcustom pure-hanging-comment-ender-p t
  "Handling of Pure block comment enders with \\[fill-paragraph].
When set to nil, Pure block comment enders are left on their own line.
When set to t, block comment enders will be placed at the end of the
previous line (i.e. they `hang' on that line)."
  :type 'boolean
  :group 'pure)

;; special fontification for %<...%> code sections

(defcustom pure-code-section-face 'default
  "Face with which to fontify inline code sections.
Normally this is set to `default' to avoid fontification of
inline code, but you can change this to any face you prefer. Note
that in order for the new face to take effect in existing
buffers, you'll have to refontify them using
\\[font-lock-fontify-buffer]."
  :type 'face
  :group 'pure)

;; mode hooks

(defcustom pure-mode-hook nil
  "Hook for customising `pure-mode'.
For instance, add `turn-on-font-lock' to enable syntax highlighting."
  :type 'hook
  :group 'pure)

(defcustom pure-eval-mode-hook nil
  "Hook for customising `pure-eval-mode'.
For instance, add `turn-on-font-lock' to enable syntax highlighting."
  :type 'hook
  :group 'pure)

;; running the Pure interpreter

(defcustom pure-prog-name pure-prog
  "Name of the interpreter executable."
  :type 'string
  :group 'pure)

(defcustom pure-prog-opts nil
  "List of extra command line options the interpreter is invoked with.
See the Pure manual for a list of available options."
  :type '(repeat string)
  :group 'pure)

(defcustom pure-library-dir pure-libdir
  "Location of the Pure library directory."
  :type 'string
  :group 'pure)

(defcustom pure-docs-dir (concat pure-libdir "/docs")
  "Location of the Pure documentation directory."
  :type 'string
  :group 'pure)

(defcustom pure-histfile "~/.emacs-pure-history"
  "Name of the command history file."
  :type 'string
  :group 'pure)

(defcustom pure-histsize 500
  "Size of the command history."
  :type 'integer
  :group 'pure)

(defcustom pure-stacksize 0
  "Stack size available to Pure programs in kilobytes.
If nonzero, this determines the value of the PURE_STACK
environment variable."
  :type 'integer
  :group 'pure)

(defcustom pure-query-before-kill nil
  "Enable or disable confirmation before killing an interpreter process.
If non-nil, this indicates that the user should be prompted
before zapping an existing interpreter process when starting a
new one."
  :type 'boolean
  :group 'pure)

(defcustom pure-process-timeout 3
  "Timeout for responses from the interpreter.
This denotes the time interval in seconds after which
`pure-send-region' and other operations which read and digest
output from the interpreter time out. Can be set to nil to
disable the timeout, but this isn't recommended."
  :type 'integer
  :group 'pure)

(defcustom pure-prompt-regexp "^> \\|^: "
  "Regexp to match prompts in the pure-eval buffer.
If you customize the interpreter's default prompt, you will have
to change this value accordingly."
  :type 'regexp
  :group 'pure)

(defcustom pure-msg-regexp
  "^\\(\\([^:,\n]+\\), line \\([0-9]+\\)\\): "
"Regexp to match error and warning messages in the pure-eval buffer.
Expression 1 denotes the whole source line info, expression 2 the
file name and expression 3 the corresponding line number."
  :type 'regexp
  :group 'pure)

(defcustom pure-send-show-buffer t
  "Enable or disable automatic pure-eval buffer display.
Non-nil means display the pure-eval buffer after sending to it."
  :type 'boolean
  :group 'pure)

(defcustom pure-send-line-auto-forward t
  "Enable or disable auto-forward after sending to the Pure interpreter.
Non-nil means always go to the next Pure code line after sending."
  :type 'boolean
  :group 'pure)

(defcustom pure-send-echo-input t
  "Enable or disable echoing of input sent to the Pure interpreter.
Non-nil means that input sent to the Pure interpreter is echoed
at the command prompt."
  :type 'boolean
  :group 'pure)

(defcustom pure-send-skip-prompts t
  "Enable or disable preprocessing of input sent to the Pure interpreter.
Non-nil means that input starting with `pure-prompt-regexp' is
preprocessed to remove the command prompts and skip lines not
beginning with a command prompt. This is convenient, in
particular, with the `pure-send-yank' command. It lets you send
transcripts of interpreter interactions containing both command
input and results printed by the inpreter, as can be found, e.g.,
in the online help."
  :type 'boolean
  :group 'pure)

;; pd-pure support

(defcustom pure-pd-pure-support nil
  "pd-pure support in `pure-mode'.
Non-nil adds some keybindings as well as an additional menu for
remote operation of Pd patches, as described in the pd-pure manual."
  :type 'boolean
  :group 'pure)

;; List of additional directories to search for imported scripts.

(defvar pure-includes
  (let ((path (getenv "PURE_INCLUDE")))
    (if path
	(setq pure-includes (parse-colon-path path))
      nil))
  "List of additional directories to search for imported scripts.")

;; the following are used internally

(defvar pure-output-list nil)
(defvar pure-output-string nil)
(defvar pure-receive-in-progress nil)
(defvar pure-last-dir nil)
(defvar pure-last-script nil)
(defvar pure-debug-mode nil)
(defvar pure-submode-indicators nil)
(defvar pure-eval-submode-indicators nil)

;; menu-related utilities

(defsubst pure-region-is-active-p ()
  ;; Return t when the region is active.
  (and (boundp 'mark-active) mark-active))

(defsubst pure-yank-is-active-p ()
  ;; Return t when the kill ring has some contents.
  (not (null kill-ring)))

(defmacro pure-menu-toggle (item cmd var)
  `[,item ,cmd :style toggle :selected ,var])

;; handle toggles

(defun pure-calculate-state (arg prevstate)
  ;; Calculate the new state of PREVSTATE, t or nil, based on arg. If
  ;; arg is nil or zero, toggle the state. If arg is negative, turn
  ;; the state off, and if arg is positive, turn the state on
  (if (or (not arg)
	  (zerop (setq arg (prefix-numeric-value arg))))
      (not prevstate)
    (> arg 0)))

;; Pure mode

(defvar pure-font-lock-keywords
  (list
   (list "^#!.*" 0 'font-lock-preprocessor-face t)
   (list "\\_<\\(bigint\\|bool\\|char\\|float\\|double\\|expr\\|short\\|int\\(8\\|16\\|32\\|64\\)?\\|long\\|string\\|pointer\\|void\\|[dci]?matrix\\)\\_>" 1 'font-lock-type-face)
   (list "\\_<\\(catch\\|throw\\|__break__\\|__trace__\\)\\_>" 0 'font-lock-builtin-face)
   (list
    (concat "\\_<\\("
	    "case\\|const\\|def\\|e\\(lse\\|nd\\|xtern\\)\\|i\\(f\\|nfix[lr]?\\|nterface\\)\\|"
	    "let\\|n\\(amespace\\|onfix\\)\\|o\\(f\\|therwise\\|utfix\\)\\|p\\(r\\(efix\\|ivate\\)\\|ostfix\\|ublic\\)\\|"
	    "then\\|type\\|using\\|w\\(hen\\|ith\\)"
	    "\\)\\_>")
    0 'font-lock-keyword-face))
  "Rules for fontifying Pure scripts.")

(defvar pure-font-lock-syntactic-keywords
  '(;; %< ... %> code sections are treated as comments
    ("\\(%\\)<" 1 "!" t) ("%\\(>\\)" 1 "!" t)
    ;; Uncomment this if you'd prefer to have #! (shebang) treated as comments,
    ;; too. This isn't recommended, though, since they can also be pragmas.
    ;;("^\\(#!\\)" 1 "<") ("^#!.*\\([\n]\\)" 1 ">")
    ;; backslash followed by paren (lambda syntax) is treated as normal
    ;; punctuation, to prevent confusion in the sexp parser
    ("\\([\\]\\)[([{]" 1 "." t)))

(defun pure-syntactic-face-function (state)
  (cond
   ((nth 3 state) font-lock-string-face)		; as normal
   ;; Look for code sections.
   ((or (eq 'syntax-table (nth 7 state))
	(eq (char-after (nth 8 state)) ?\%))
    pure-code-section-face)
   (t font-lock-comment-face)))

(defvar pure-mode-map nil)
(cond ((not pure-mode-map)
       (setq pure-mode-map (make-sparse-keymap))
       (define-key pure-mode-map "("        'pure-electric-char)
       (define-key pure-mode-map ")"        'pure-electric-char)
       (define-key pure-mode-map "["        'pure-electric-char)
       (define-key pure-mode-map "]"        'pure-electric-char)
       (define-key pure-mode-map "{"        'pure-electric-char)
       (define-key pure-mode-map "}"        'pure-electric-char)
       (define-key pure-mode-map ";"        'pure-electric-char)
       (define-key pure-mode-map "="        'pure-electric-equal)
       (define-key pure-mode-map "\t"       'pure-electric-tab)
       (define-key pure-mode-map "\C-ch"    'pure-help)
       (define-key pure-mode-map "\C-c\M-p" 'pure-run)
       (define-key pure-mode-map "\C-c\M-r" 'pure-rerun)
       (define-key pure-mode-map "\C-c\M-q" 'pure-quit)
       (define-key pure-mode-map "\C-x\M-p" 'pure-scratchpad)
       (define-key pure-mode-map "\C-c\C-k" 'pure-run-script)
       (define-key pure-mode-map "\C-c\M-d" 'pure-toggle-debug-mode)
       (define-key pure-mode-map "\C-ct"    'pure-make-tags)
       (define-key pure-mode-map "\C-c\C-u" 'pure-current-msg)
       (define-key pure-mode-map "\C-c\C-n" 'pure-next-msg)
       (define-key pure-mode-map "\C-c\C-p" 'pure-prev-msg)
       (define-key pure-mode-map "\C-c\C-e" 'pure-last-msg)
       (define-key pure-mode-map "\C-c\C-a" 'pure-first-msg)
       (define-key pure-mode-map "\C-x\M-f" 'pure-find-script)
       (define-key pure-mode-map "\C-x\M-v" 'pure-goto-input-line)
       (define-key pure-mode-map "\C-c\C-c" 'pure-send-line)
       (define-key pure-mode-map "\C-c\C-f" 'pure-send-defun)
       (define-key pure-mode-map "\C-c\C-r" 'pure-send-region)
       (define-key pure-mode-map "\C-c\C-y" 'pure-send-yank)
       (define-key pure-mode-map "\C-c:"    'pure-eval)
       (define-key pure-mode-map "\C-c\M-h" 'pure-show)
       (define-key pure-mode-map "\C-c\M-c" 'pure-clear)
       (define-key pure-mode-map "\C-c\M-b" 'pure-break)
       (define-key pure-mode-map "\C-c\M-t" 'pure-trace)
       (define-key pure-mode-map "\C-c\M-i" 'pure-toggle-indent-mode)
       (define-key pure-mode-map "\C-c\M-a" 'pure-toggle-align-mode)
       (define-key pure-mode-map "\C-c\M-e" 'pure-toggle-electric-mode)
       (define-key pure-mode-map [M-left]   'pure-prev-defun)
       (define-key pure-mode-map [M-right]  'pure-next-defun)
       (define-key pure-mode-map [M-up]     'pure-backward-defun)
       (define-key pure-mode-map [M-down]   'pure-forward-defun)
       (define-key pure-mode-map "\M-p"     'pure-prev-defun)
       (define-key pure-mode-map "\M-n"     'pure-next-defun)
       (define-key pure-mode-map "\M-P"     'pure-backward-defun)
       (define-key pure-mode-map "\M-N"     'pure-forward-defun)
       (define-key pure-mode-map "\M-\C-h"  'pure-mark-defun)
       (define-key pure-mode-map "\e\C-q"   'pure-indent-line-or-region)
       (define-key pure-mode-map "\e\t"     'pure-complete-symbol)
       (when pure-pd-pure-support
	 (define-key pure-mode-map "\C-c\C-m" 'pd-pure-send-message)
	 (define-key pure-mode-map "\C-c\C-x" 'pd-pure-send-bang)
	 (define-key pure-mode-map "\C-c\M-x" 'pd-pure-send-reload)
	 (define-key pure-mode-map "\C-c\C-s" 'pd-pure-send-start)
	 (define-key pure-mode-map "\C-c\C-t" 'pd-pure-send-stop)
	 (define-key pure-mode-map "\C-c\C-g" 'pd-pure-send-restart)
	 (define-key pure-mode-map [(control ?\/)] 'pd-pure-send-dsp-on)
	 (define-key pure-mode-map [(control ?\.)] 'pd-pure-send-dsp-off))
    ))

(defvar pure-mode-menu
  (append
   (list "Pure"
	 ["Pure Help..."		pure-help t]
	 ["Describe Pure Mode"		describe-mode t]
	 ["Customize"			(customize-group 'pure) t]
	 "-"
	 ["Comment Out Region"		comment-region
					(pure-region-is-active-p)]
	 ["Uncomment Region"		uncomment-region
					(pure-region-is-active-p)]
	 (list "Definitions"
	       ["Previous Definition"	pure-prev-defun t]
	       ["Next Definition"	pure-next-defun t]
	       ["Previous Sibling"	pure-backward-defun t]
	       ["Next Sibling"		pure-forward-defun t]
	       ["Mark Definition"	pure-mark-defun t])
	 ["Fill Comment Paragraph"	fill-paragraph t]
	 ["Indent Line or Region"	pure-indent-line-or-region t]
	 (list "Indentation Modes"
	       (pure-menu-toggle "Indent Equals" pure-toggle-indent-mode
				 pure-indent-equals)
	       (pure-menu-toggle "Align Equals" pure-toggle-align-mode
				 pure-align-equals)
	       (pure-menu-toggle "Electric Keys" pure-toggle-electric-mode
				 pure-electric-keys))
	 "-"
	 ["Make Tags..."		pure-make-tags t]
	 "-"
	 ["Start Interpreter"		pure-run t]
	 ["Restart Interpreter"		pure-rerun
					(get-process "pure-eval")]
	 ["Quit Interpreter"		pure-quit
					(get-process "pure-eval")])
   (when pure-pd-pure-support
     (list (list "Pd"
		 ["Reload"		pd-pure-send-bang t]
		 ["Rerun"		pd-pure-send-reload t]
		 "-"
		 ["Start"		pd-pure-send-start t]
		 ["Stop"		pd-pure-send-stop t]
		 ["Restart"		pd-pure-send-restart t]
		 "-"
		 ["Dsp on"		pd-pure-send-dsp-on t]
		 ["Dsp off"		pd-pure-send-dsp-off t]
		 "-"
		 ["Send message..."	pd-pure-send-message t])))
   (list "-"
	 (list "Execute"
	       ["Run Script"		pure-run-script t]
	       ["Send Current Line"	pure-send-line t]
	       ["Send Current Definition" pure-send-defun t]
	       ["Send Region"		pure-send-region
					(pure-region-is-active-p)]
	       ["Send Yank"		pure-send-yank
					(pure-yank-is-active-p)]
	       ["Evaluate Expression..." pure-eval t]
	       ["Show Symbol..."	pure-show t]
	       ["Clear Symbol..."	pure-clear t])
	 "-"
	 (list "Debugger"
	       (pure-menu-toggle "Debugging Mode" pure-toggle-debug-mode
				 pure-debug-mode)
	       "-"
	       ["Breakpoint..."		pure-break pure-debug-mode]
	       ["Tracepoint..."		pure-trace pure-debug-mode])
	 "-"
	 ["Find Main Script"		pure-find-script pure-last-script]
	 ["Goto Input Line"		pure-goto-input-line
					(get-process "pure-eval")]
	 ["Pure Scratchpad"		pure-scratchpad t]
	 "-"
	 (list "Locate"
	       ["Current Message"	pure-current-msg
					(get-buffer "*pure-eval*")]
	       ["First Message"		pure-first-msg
					(get-buffer "*pure-eval*")]
	       ["Next Message"		pure-next-msg
					(get-buffer "*pure-eval*")]
	       ["Previous Message"	pure-prev-msg
					(get-buffer "*pure-eval*")]
	       ["Last Message"		pure-last-msg
					(get-buffer "*pure-eval*")])
	 "-"
	 ["Complete Symbol"		pure-complete-symbol t]))
  "Menu for `pure-mode'.")

;; Submode indicators in the pure-mode modeline. "i", "a" and "e" indicate the
;; current (buffer-local) state of pure-indent-equals, pure-align-equals and
;; pure-electric-keys, respectively.

(defun pure-update-modeline ()
  (let ((fmt (format " %s%s%s"
		     (if pure-align-equals "a" "")
		     (if pure-electric-keys "e" "")
		     (if pure-indent-equals "i" ""))))
    (setq pure-submode-indicators
	  (if (> (length fmt) 1)
 	      fmt))
    (force-mode-line-update)))
  
;;;###autoload
(define-derived-mode pure-mode fundamental-mode "Pure"
  "Major mode for editing Pure scripts.

Provides syntax highlighting (using `font-lock-mode') and
automatic indentation of Pure code, completion of Pure symbols
and integrated help, etags support, as well as facilities to run
scripts and evaluate code with the Pure interpreter.

The major operations of this mode are also accessible from the
menu bar (Pure menu).

Completion and Help
-------------------

\\<pure-mode-map>\\[pure-complete-symbol] completes the symbol at point, looking up known Pure
symbols in the online documentation index.

\\[pure-help] invokes the online help. You can optionally specify a
Pure symbol or topic to look up in the index.

Etags Support
-------------

Use the `pure-make-tags' (\\[pure-make-tags]) command to create an Emacs
TAGS file for the script in the current buffer or a given
collection of scripts (shell glob patterns are permitted).
You can then employ the usual Emacs commands such as
`find-tag' \(\\[find-tag]) to locate the definitions of Pure symbols
in the given scripts (and all their imports). See info node
`(emacs)Tags' for details.

Indentation and Filling
-----------------------

Two distinct basic indentation styles are provided: ``hanging
indent'' which indents right-hand sides of equations so that they
are aligned at `=' symbols (this is the default), and ``block
indent'' which does normal block indentation of equations. The
default indentation style can be set with the `pure-indent-equals'
variable (see below) and also changed quickly in a buffer-local
fashion with the `pure-toggle-indent-mode' command.

\\[pure-electric-tab] indents the current line. \\[pure-indent-line-or-region] indents an entire region
of Pure code (if the region is currently active, otherwise it
indents the current line). Other electric characters such as `;'
and `=' also perform automatic indentation, see below.

\\[pure-electric-tab] takes into account the surrounding context. When inside
a comment, it uses relative indentation (see `indent-relative').
When inside a string, it just inserts a literal tab character.

\\[fill-paragraph] fills paragraphs in comments and strings. You can also use
this command with a prefix argument to justify the text between
margins. (Please note that at present this command doesn't deal
with escaped newlines, thus you'll have to add those manually
when using this command with Pure strings.)

Some important variables controlling indentation/edit style:

`pure-block-indent': Indentation of Pure code with respect to
    containing block. Default: 2.

`pure-extra-indent': Extra indentation for Pure code blocks at
    the beginning of a line. Default: 0.

`pure-tab-always-indent': Non-nil means \\[pure-electric-tab] should always
    reindent the current line, regardless of where in the line
    point is when the \\[pure-electric-tab] command is used. Default: t.

`pure-electric-keys': Non-nil means that keys like `=', `;', `(',
    `)' etc. do automatic indentation and alignment. Note that if
    this is nil then `pure-align-equals' doesn't have any effect
    either. Default: t.

`pure-indent-equals': Non-nil means that the right-hand sides of
    equations are indented so that they are aligned at `='
    symbols (``hanging indent''). Otherwise normal block
    indentation of equations is done instead. Default: t.

`pure-align-equals': Non-nil means that `=' at the end of a line
    is aligned with a previous equation at the same block level.
    Note: This setting is independent of `pure-indent-equals'.
    Default: t.

There are key bindings for quickly changing the indentation
modes on a per-buffer basis (see the command list below), and the
current status of the `pure-electric-keys', `pure-indent-equals'
and `pure-align-equals' flags is displayed in the mode line
\(`aei' indicators).

Other Editing Commands
----------------------

Emacs provides some movement and editing commands which deal with
balanced parentheses, see info node `(emacs)Parentheses' for
details. These are also quite handy when editing expressions in
Pure mode.

Like other programming language modes, Pure mode also offers some
special commands which operate on ``defuns'', i.e., toplevel
items in Pure: definitions, declarations and expressions. (In the
following, we call these just ``definitions''.)

The following commands traverse the tree of blocks and
definitions in a Pure script. They provide a quick way to
navigate the block hierarchy of a Pure program. Each of these
commands also accepts a repeat count as a prefix arg. (Please
note that the alternative shifted cursor key bindings are only
available if you're running a GUI version of emacs.)

`pure-prev-defun' (\\[pure-prev-defun], <M-left>) moves backward to the
beginning of the previous definition at the same or a lower block
level.

`pure-next-defun' (\\[pure-next-defun], <M-right>) moves forward to the
beginning of the next definition at the same or a lower block
level.

`pure-backward-defun' (\\[pure-backward-defun], <M-up>) moves backward to the
beginning of the previous definition at the same block level,
skipping over definitions in lower levels.

`pure-forward-defun' (\\[pure-forward-defun], <M-down>) moves forward to the
beginning of the next definition at the same block level,
skipping over definitions in lower levels.

Note that `pure-prev-defun' and `pure-next-defun' can be used to
visit all definitions in a script, traversing the block structure
in preorder or postorder, respectively. In contrast,
`pure-backward-defun' and `pure-forward-defun' skip over any
local blocks of definitions contained in the current block
\(pretty much like `backward-sexp' and `forward-sexp' do for
balanced parentheses), so they can be used to move in larger
increments.

These commands always stop when reaching the beginning or end of
the current block, respectively.  However, you can then repeat
the command to move on to outer block levels.

*Note:* Only the first definition on each line (or behind the
beginning of each block) will be located by these commands, so it
is generally a good idea to start each definition on a line of
its own. Also, try to avoid definitions looking like line
noise (punctuation only), as these cannot be recognized in the
current implementation.

There's also a command to mark a Pure definition, which is handy
if you want to operate on an entire definition.

`pure-mark-defun' (\\[pure-mark-defun]) marks the current definition around
point. It puts the mark at the end of the definition and point at
its beginning.

Folding
-------

Pure mode has support for code folding using the hideshow minor
mode, which usually comes with Emacs. To enable this, load
hideshow before Pure mode in your .emacs:

  (require 'hideshow)

This makes sure that Pure's block and comment syntax are known to
hideshow. You can then invoke hideshow manually using
\\[hs-minor-mode]. To have hideshow enabled in all your Pure mode
buffers, add the following to your .emacs:

  (add-hook 'pure-mode-hook 'hs-minor-mode)

See `hs-minor-mode' for details on how to use this mode.

Running the Pure Interpreter
----------------------------

Pure mode also provides tight integration with the Pure
interpreter. First, the interpreter can be started in a
buffer (see `pure-eval-mode') using \\[run-pure]. Second, the
`pure-run-script' (\\[pure-run-script]) command lets you run the
interpreter on the script in the current buffer. It will be
verified that the buffer has a file associated with it, and you
will be prompted to save edited buffers when invoking this
command.

Third, you can also feed lines, definitions or arbitrary regions
of Pure code into a running interpreter with the
`pure-send-line' (\\[pure-send-line]), `pure-send-defun' (\\[pure-send-defun]),
`pure-send-region' (\\[pure-send-region]) and `pure-send-yank' (\\[pure-send-yank])
commands. (This starts a new interpreter instance without a
script if necessary.)

If `pure-send-skip-prompts' is set (the default), the send
commands will recognize transcripts of an interactive session
which contain both command lines starting with a prompt and
results printed by the interpreter. In this case the command
prompts are removed and all lines not starting with a command
prompt are skipped, which is convenient, in particular, to send
examples from the online documentation which show sample
interpreter interactions.

When working with these commands, it is often convenient to have
a scratchpad for entering Pure definitions and expressions; this
is provided by the `pure-scratchpad' (\\[pure-scratchpad]) command which
creates a *pure-scratch* buffer very much like the standard
*scratch* buffer but operated in Pure mode.

The `pure-eval' (\\[pure-eval]) command evaluates a Pure expression
read from the minibuffer and prints its result in the echo area,
or, if invoked with a prefix arg, inserts the result into the
current buffer.

`pure-clear' (\\[pure-clear]) clears the definition of a Pure symbol using
the interpreter's `clear' command. Similarly, `pure-show' (\\[pure-show])
shows the definition of a Pure symbol, using the `show' command
of the interpreter. This command can also be invoked with a
prefix arg to insert the output of `show' into the current buffer.

`pure-break' (\\[pure-break]) and `pure-trace' (\\[pure-trace]) set (or, if
invoked with a prefix arg, delete) debugger breakpoints and
tracepoints. (You may first have to enable debugging mode with
`pure-toggle-debug-mode' (\\[pure-toggle-debug-mode]) when using these.)

Other special commands are provided to quickly locate the main
script and the input line of the pure-eval buffer, and to visit
the source lines shown in error messages. These are the same as
in `pure-eval-mode' (which see).

Customization
-------------

The mode can be customized with the Customize menu option which
is a shortcut for \\[customize-group] pure.

Entry to this mode calls the value of `pure-mode-hook' if that
value is non-nil.

Commands
--------

\\{pure-mode-map}"
  (set-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\_  "_")
  (modify-syntax-entry ?\:  "_")
  (modify-syntax-entry ?\.  ".")
  (modify-syntax-entry ?\+  ".")
  (modify-syntax-entry ?\-  ".")
  (modify-syntax-entry ?\=  ".")
  (modify-syntax-entry ?\<  ".")
  (modify-syntax-entry ?\>  ".")
  (modify-syntax-entry ?\$  ".")
  (modify-syntax-entry ?\|  ".")
;  (modify-syntax-entry ?\\  ".")
  ;; comment syntax a la C++ mode
  (cond
   ((string-match "XEmacs\\|Lucid" emacs-version)
    (modify-syntax-entry ?/  ". 1456")
    (modify-syntax-entry ?*  ". 23"))
   (t
    (modify-syntax-entry ?/  ". 124b")
    (modify-syntax-entry ?*  ". 23")))
  (modify-syntax-entry ?\n "> b")
  (modify-syntax-entry ?\^m "> b")
  (use-local-map pure-mode-map)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)
  (setq comment-column 48
	comment-start "// "
	comment-end ""
	comment-start-skip "/\\*+ *\\|%< *\\|// *\\|^#! *"
	comment-multi-line nil)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'pure-indent-line)
  (if (boundp 'fill-paragraph-function)
      (progn
	(make-local-variable 'fill-paragraph-function)
	(setq fill-paragraph-function 'pure-fill-paragraph)))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(pure-font-lock-keywords
	  nil nil ((?_ . "w")) nil
	  (font-lock-syntactic-keywords . pure-font-lock-syntactic-keywords)
	  (font-lock-syntactic-face-function . pure-syntactic-face-function)
	  (parse-sexp-lookup-properties . t)
	  ))
  (require 'easymenu)  
  (easy-menu-define pure-mode-menu-map pure-mode-map
		    "Menu keymap for `pure-mode'." pure-mode-menu)
  (easy-menu-add pure-mode-menu-map pure-mode-map)
  ;; make these local so that they can be changed on a per-buffer basis
  (make-local-variable 'pure-indent-equals)
  (make-local-variable 'pure-align-equals)
  (make-local-variable 'pure-electric-keys)
  ;; submode indicators
  (make-local-variable 'pure-submode-indicators)
  ;; Put submode indicators onto minor-mode-alist, but only once.
  (or (assq 'pure-submode-indicators minor-mode-alist)
      (setq minor-mode-alist
 	    (cons '(pure-submode-indicators pure-submode-indicators)
 		  minor-mode-alist)))
  (pure-update-modeline))

;; pd-pure support

(defun pd-pure-send-start-process ()
  "Starts a pdsend process to communicate with Pd via UDP port 4711."
  (interactive)
  (start-process "pdsend" nil "pdsend" "4711" "localhost" "udp")
  (process-kill-without-query (get-process "pdsend")))

(defun pd-pure-send-stop-process ()
  "Stops a previously started pdsend process."
  (interactive)
  (delete-process "pdsend"))

(defun pd-pure-send-message (message)
  "Send a given message to Pd. Start the pdsend process if needed."
  (interactive "sMessage: ")
  (unless (get-process "pdsend") (pd-pure-send-start-process))
  (process-send-string "pdsend" (concat message "\n")))

(defun pd-pure-send-dsp-on ()
  "Send a 'dsp 1' message to Pd."
  (interactive)
  (pd-pure-send-message "pd dsp 1"))

(defun pd-pure-send-dsp-off ()
  "Send a 'dsp 0' message to Pd."
  (interactive)
  (pd-pure-send-message "pd dsp 0"))

(defun pd-pure-send-bang ()
  "Send a 'bang' message to Pd."
  (interactive)
  (pd-pure-send-message "bang"))

(defun pd-pure-send-reload ()
  "Send a 'reload' message to Pd."
  (interactive)
  (pd-pure-send-message "reload"))

(defun pd-pure-send-start ()
  "Send a 'play 1' message to Pd."
  (interactive)
  (pd-pure-send-message "play 1"))

(defun pd-pure-send-stop ()
  "Send a 'play 0' message to Pd."
  (interactive)
  (pd-pure-send-message "play 0"))

(defun pd-pure-send-restart ()
  "Send 'play 0, play 1' messages to Pd."
  (interactive)
  (pd-pure-send-message "play 0")
  (pd-pure-send-message "play 1"))

;; comint support

(require 'comint)

(defvar pure-eval-font-lock-keywords
  (list
;    (list pure-prompt-regexp 0 'font-lock-preprocessor-face t)
   (list "^#!.*" 0 'font-lock-preprocessor-face t)
   (list pure-msg-regexp 0 'font-lock-warning-face t)
   (list "\\_<\\(bigint\\|bool\\|char\\|float\\|double\\|expr\\|short\\|int\\(8\\|16\\|32\\|64\\)?\\|long\\|string\\|pointer\\|void\\|[dci]?matrix\\)\\_>" 1 'font-lock-type-face)
;   (list "\\_<\\(catch\\|throw\\|__break__\\|__trace__\\)\\_>" 0 'font-lock-builtin-face)
   (list
    (concat "\\_<\\("
	    "const\\|def\\|extern\\|i\\(nfix[lr]?\\|nterface\\)\\|"
	    "let\\|n\\(amespace\\|onfix\\)\\|outfix\\|p\\(r\\(efix\\|ivate\\)\\|ostfix\\|ublic\\)\\|type\\|"
	    "using"
	    "\\)\\_>")
    0 'font-lock-keyword-face))
  "Rules for fontifying in `pure-eval-mode'.")

;; some helper functions for pure-eval-mode: check that we're on the command
;; resp. debugger prompt

(defun pure-at-pmark-p ()
  (and (get-buffer "*pure-eval*")
       (get-process "pure-eval")
       (progn (set-buffer "*pure-eval*") (comint-after-pmark-p))))

(defun pure-at-command-prompt-p ()
  (and
   (pure-at-pmark-p)
   (save-excursion
     (forward-line 0)
     (looking-at pure-prompt-regexp))))
		   
(defun pure-at-debug-prompt-p ()
  (and
   (pure-at-pmark-p)
   (save-excursion
     (forward-line 0)
     (looking-at ":"))))

(defvar pure-eval-mode-map nil)
(cond ((not pure-eval-mode-map)
       (setq pure-eval-mode-map (copy-keymap comint-mode-map))
       (define-key pure-eval-mode-map "\C-ch"    'pure-help)
       (define-key pure-eval-mode-map "\C-c\M-p" 'pure-run)
       (define-key pure-eval-mode-map "\C-c\M-r" 'pure-rerun)
       (define-key pure-eval-mode-map "\C-c\M-q" 'pure-quit)
       (define-key pure-eval-mode-map "\C-c\M-d" 'pure-toggle-debug-mode)
       (define-key pure-eval-mode-map "\C-x\M-p" 'pure-scratchpad)
       (define-key pure-eval-mode-map "\C-c\M-h" 'pure-show)
       (define-key pure-eval-mode-map "\C-c\M-c" 'pure-clear)
       (define-key pure-eval-mode-map "\C-c\M-b" 'pure-break)
       (define-key pure-eval-mode-map "\C-c\M-t" 'pure-trace)
       (define-key pure-eval-mode-map "\C-c\C-y" 'pure-send-yank)
       (define-key pure-eval-mode-map "\C-c:"    'pure-eval)
       (define-key pure-eval-mode-map "\t" 'comint-dynamic-complete)
       (define-key pure-eval-mode-map "\C-a" 'comint-bol)
       (define-key pure-eval-mode-map [home] 'comint-bol)
;;       (define-key pure-eval-mode-map [up] 'comint-previous-input)
;;       (define-key pure-eval-mode-map [down] 'comint-next-input)
       (define-key pure-eval-mode-map [return] 'pure-current-msg-or-send)
       (define-key pure-eval-mode-map [mouse-2] 'pure-mouse-msg)
       (define-key pure-eval-mode-map "\C-c\C-u" 'pure-current-msg)
       (define-key pure-eval-mode-map "\C-c\C-n" 'pure-next-msg)
       (define-key pure-eval-mode-map "\C-c\C-p" 'pure-prev-msg)
       (define-key pure-eval-mode-map "\C-c\C-e" 'pure-last-msg)
       (define-key pure-eval-mode-map "\C-c\C-a" 'pure-first-msg)
       (define-key pure-eval-mode-map "\C-x\M-f" 'pure-find-script)
       (define-key pure-eval-mode-map "\C-x\M-v" 'pure-goto-input-line)))

(defvar pure-eval-mode-menu
  (list "Pure"
	["Pure Help..."			pure-help t]
	["Describe Pure-Eval Mode"	describe-mode t]
	["Customize"			(customize-group 'pure) t]
	"-"
	["Start Interpreter"		pure-run t]
	["Restart Interpreter"		pure-rerun
					(get-process "pure-eval")]
	["Quit Interpreter"		pure-quit
					(get-process "pure-eval")]
	"-"
	(list "Execute"
	       ["Send Yank"		pure-send-yank
					(pure-yank-is-active-p)]
	       ["Evaluate Expression..." pure-eval t]
	       ["Show Symbol..."	pure-show t]
	       ["Clear Symbol..."	pure-clear t])
	"-"
	(list "Debugger"
	      (pure-menu-toggle "Debugging Mode" pure-toggle-debug-mode
				pure-debug-mode)
	      ["Breakpoint..."		pure-break pure-debug-mode]
	      ["Tracepoint..."		pure-trace pure-debug-mode])
	"-"
	["Find Main Script"		pure-find-script pure-last-script]
	["Goto Input Line"		pure-goto-input-line
					(get-process "pure-eval")]
	["Pure Scratchpad"		pure-scratchpad t]
	"-"
	(list "Locate"
	      ["Current Message"	pure-current-msg
					(get-buffer "*pure-eval*")]
	      ["First Message"		pure-first-msg
					(get-buffer "*pure-eval*")]
	      ["Next Message"		pure-next-msg
					(get-buffer "*pure-eval*")]
	      ["Previous Message"	pure-prev-msg
					(get-buffer "*pure-eval*")]
	      ["Last Message"		pure-last-msg
					(get-buffer "*pure-eval*")])
	"-"
	["Complete Symbol"		comint-dynamic-complete
					(pure-at-command-prompt-p)])
  "Menu for `pure-eval-mode'.")

;; Debug submode indicator in the pure-eval-mode modeline.

(defun pure-eval-update-modeline ()
  (let ((fmt (format " %s"
		     (if pure-debug-mode "debug" ""))))
    (setq pure-eval-submode-indicators
	  (if (> (length fmt) 1)
 	      fmt))
    (force-mode-line-update)))

(defun pure-eval-mode ()

  "Major mode for interacting with the Pure interpreter.

This is a major mode based on `comint-mode' which lets you run
the Pure interpreter in an emacs buffer. It also provides tight
integration with `pure-mode' so that you can run Pure scripts or
feed script lines into the interpreter.

The `pure-current-msg-or-send' (\\<pure-eval-mode-map>\\[pure-current-msg-or-send]) command
uses `comint-send-input' to submit a command line to the
interpreter (or to copy it to the command prompt when point is
not at the current command line). If point is at an error message
describing a source reference, the command visits the given line
in the corresponding source file in another window instead.

Error messages are indicated with a special font. Pressing the
middle mouse button (button2) over such a message visits the
corresponding source line in another window (`pure-mouse-msg'
command).

The following commands can be used to scan through error messages
found in the buffer:

`pure-first-msg' (\\[pure-first-msg])
`pure-next-msg' (\\[pure-next-msg])
`pure-prev-msg' (\\[pure-prev-msg])
`pure-last-msg' (\\[pure-last-msg])

The following commands let you visit the script that is currently
running and quickly go to the prompt at the current input line in
the pure-eval buffer:

`pure-find-script' (\\[pure-find-script])
`pure-goto-input-line' (\\[pure-goto-input-line])

\(The above commands are also provided in `pure-mode'. If you like,
you can bind them globally, so that you can invoke them from
other kinds of buffers as well.)

Besides this, you can use the usual comint commands, see the
description of `comint-mode' for details. Some important commands
are listed below:

\\[comint-previous-input] and \\[comint-next-input] cycle through the command history.
\\[comint-previous-matching-input] and \\[comint-next-matching-input] search the command history.
\\[comint-interrupt-subjob] sends a Ctl-C to the interpreter.
\\[comint-send-eof] sends a Ctl-D to the interpreter.
\\[comint-dynamic-list-input-ring] lists the command history.
\\[comint-dynamic-complete] performs symbol and filename completion.

Note that in difference to standard comint mode, the C-a/Home
keys are rebound to `comint-bol', to mimic the behaviour of the
default binding of these keys in the interpreter.

Most of these operations can also be selected from the Comint and
Pure menus accessible from the menu bar. The interpreter's prompt
and lines containing error messages are described by the
variables `pure-prompt-regexp' and `pure-msg-regexp'. (Note that
if you have set the PURE_PS environment variable then you'll have
to modify `pure-prompt-regexp' accordingly.) The history file and
size is given by the `pure-histfile' and `pure-histsize'
variables.

The mode can be customized with the Customize menu option which
is a shortcut for \\[customize-group] pure.

Entry to this mode runs the hooks on `comint-mode-hook' and
`pure-eval-mode-hook' (in that order).

Commands
--------

\\{pure-eval-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (comint-mode)
  (set-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\_  "_")
  (modify-syntax-entry ?\:  "_")
  (modify-syntax-entry ?\.  ".")
  (modify-syntax-entry ?\+  ".")
  (modify-syntax-entry ?\-  ".")
  (modify-syntax-entry ?\=  ".")
  (modify-syntax-entry ?\<  ".")
  (modify-syntax-entry ?\>  ".")
  (modify-syntax-entry ?\|  ".")
  (modify-syntax-entry ?\$  ".")
  (modify-syntax-entry ?\/  ". 12")
  (modify-syntax-entry ?\*  ".")
;  (modify-syntax-entry ?\\  ".")
  (modify-syntax-entry ?\n  ">")
  (modify-syntax-entry ?\^m ">")
  (setq major-mode 'pure-eval-mode)
  (setq mode-name "Pure-Eval")
  (use-local-map pure-eval-mode-map)
  (setq comint-prompt-regexp pure-prompt-regexp)
  (setq comint-use-prompt-regexp t)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start comint-prompt-regexp)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-multi-line)
  (setq comment-column 48
	comment-start-skip "// *\\|^#! *"
	comment-multi-line nil)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults
	'(pure-eval-font-lock-keywords nil nil ((?_ . "w"))))
  (setq comint-input-ring-file-name pure-histfile
	comint-input-ring-size pure-histsize
	comint-dynamic-complete-functions
	'(pure-complete comint-dynamic-complete-filename))
  (comint-read-input-ring t)
  (require 'easymenu)  
  (easy-menu-define pure-eval-mode-menu-map pure-eval-mode-map
		    "Menu keymap for `pure-eval-mode'." pure-eval-mode-menu)
  (easy-menu-add pure-eval-mode-menu-map pure-eval-mode-map)
  ;; submode indicators
  (make-local-variable 'pure-eval-submode-indicators)
  ;; Put submode indicators onto minor-mode-alist, but only once.
  (or (assq 'pure-eval-submode-indicators minor-mode-alist)
      (setq minor-mode-alist
 	    (cons '(pure-eval-submode-indicators pure-eval-submode-indicators)
 		  minor-mode-alist)))
  (pure-eval-update-modeline)
  (run-hooks 'pure-eval-mode-hook))

;; completion

(defun pure-complete ()
  "Perform completion for defined Pure symbols on the token
preceding point."
  (interactive)
  (if (pure-at-command-prompt-p)
      (let* ((end (point))
	     (command
	      (save-excursion
		;; skip back one word/identifier or operator (punctuation)
		(skip-syntax-backward "w_")
		(and (eq (point) end)
		     (skip-syntax-backward "."))
		(and (looking-at pure-prompt-regexp)
		     (goto-char (match-end 0)))
		(buffer-substring-no-properties (point) end))))
	(pure-send-list-and-digest
	 (list (concat "completion_matches " command "\n")))
	;; Sort the list
	(setq pure-output-list
	      (sort pure-output-list 'string-lessp))
	;; Remove duplicates
	(let* ((x pure-output-list)
	       (y (cdr x)))
	  (while y
	    (if (string-equal (car x) (car y))
		(setcdr x (setq y (cdr y)))
	      (setq x y
		    y (cdr y)))))
	;; And let comint handle the rest
	(comint-dynamic-simple-complete command pure-output-list))))

;; online help

(require 'thingatpt)

;; This is rather simplistic, it just looks for a sequence of symbol
;; constituents including a possible namespace prefix.
(put 'puresym 'end-op (lambda () (skip-syntax-forward "w_")))
(put 'puresym 'beginning-op (lambda () (skip-syntax-backward "w_")))

(defun puresym-at-point ()
  (let ((sym (thing-at-point 'puresym)))
    (if (and (stringp sym) (> (length sym) 0))
	sym)))

(defun pure-help-complete (symbol)
  (pure-send-list-and-digest
   (list (concat "help_matches " symbol "\n")))
  ;; Sort the list
  (setq pure-output-list
	(sort pure-output-list 'string-lessp))
  ;; Remove duplicates
  (let* ((x pure-output-list)
	 (y (cdr x)))
    (while y
      (if (string-equal (car x) (car y))
	  (setcdr x (setq y (cdr y)))
	(setq x y
	      y (cdr y)))))
  (mapcar 'list pure-output-list))

;; This is from Emacs 22.3. (Emacs 23.1 and later have
;; completion-table-dynamic.)
(defmacro pure-dynamic-completion-table (fun)
  (declare (debug (lambda-expr)))
  (let ((win (make-symbol "window"))
        (string (make-symbol "string"))
        (predicate (make-symbol "predicate"))
        (mode (make-symbol "mode")))
    `(lambda (,string ,predicate ,mode)
       (with-current-buffer (let ((,win (minibuffer-selected-window)))
                              (if (window-live-p ,win) (window-buffer ,win)
                                (current-buffer)))
         (complete-with-action ,mode (,fun ,string) ,string ,predicate)))))

;;;###autoload
(defun pure-help (&optional topic)
  "Read online help in html format.
This works pretty much like the interpreter's help command. Uses
emacs-w3m if it is available. The command searches for help files
in the current directory, the `pure-docs-dir', and all
directories specified in `pure-includes'. The given TOPIC may
specify a search term or a help file and section/index entry, see
topic pure#online-help for details. When run interactively, tab
completion is provided for search terms in the index."
;;  (interactive "sPure help topic: ")
  (interactive
   (let ((default (puresym-at-point)))
     (pure-start t)
     (list (completing-read
	    (format "Pure help topic%s"
		    (if (null default) ": "
		      (format " (default %s): " default)))
	    (pure-dynamic-completion-table pure-help-complete)
	    nil nil nil nil default))))
  (let* ((args (split-string (if topic topic "") "#"))
	 (args (if (or (null args) (not (zerop (length (car args))))) args
		 (list topic)))
	 (base
	  (cond
	   ((null args) (cons "index" ""))
	   ((null (cdr args))
	    (if (zerop (length (car args))) (cons "index" "")
;; Keyword search. This looks up the given search term in the global index
;; using the interpreter's help_index command.  If the search term isn't
;; found, we fall back to a link target in pure.html. This mimics the
;; behaviour of the interpreter's help command.
	      (pure-send-list-and-digest
	       (list (concat "help_index " (car args) "\n")))
	      (if (null pure-output-list)
		  (cons "pure" (car args))
		(let* ((target (split-string (car pure-output-list) "#")))
		  (if (null (cdr target)) target
		    (cons (car target)
			  (mapconcat 'identity (cdr target) "#")))))))
	   (t
	    (cons (if (zerop (length (car args))) "pure" (car args))
	     (mapconcat 'identity (cdr args) "#")))))
	 (file (car base))
	 (file (if (null (file-name-extension file)) (concat file ".html")
		 file))
	 (anchor (if (zerop (length (cdr base))) "" (concat "#" (cdr base))))
	 (helpfile
	  (locate-library file t (append (list "." pure-docs-dir)
					 pure-includes))))
    (cond
     ((null helpfile) (error (concat "Couldn't locate help file " file)))
     ;; use emacs-w3m if it is available
     ((featurep 'w3m-load)
      (if (get-buffer "*w3m*")
	  (switch-to-buffer-other-window "*w3m*")
	(switch-to-buffer-other-window (current-buffer)))
      (w3m-browse-url (concat "file:" helpfile anchor)))
     ;; otherwise use Emacs' generic browse-url facility
     (t (browse-url (concat "file:" helpfile anchor))))))

;; create the Pure scratchpad

(defvar pure-scratchpad-msg
"// This buffer is a scratchpad for Pure code and other notes you don't want to
// save. It can also be used to evaluate Pure code (see \\[describe-mode] for help).

")

;;;###autoload
(defun pure-scratchpad ()
  "Create the *pure-scratch* buffer if necessary and put it in
`pure-mode'."
  (interactive)
  (let* ((pure-scratch-active (not (null (get-buffer "*pure-scratch*"))))
	 (pure-scratch-buffer (get-buffer-create "*pure-scratch*")))
    (set-buffer pure-scratch-buffer)
    (unless pure-scratch-active
      (pure-mode)
      (insert (substitute-command-keys pure-scratchpad-msg)))
    (pop-to-buffer pure-scratch-buffer)))

;; run a Pure script in a Pure Eval buffer

;;;###autoload
(defalias 'run-pure 'pure-run)

(defun pure-run (&rest args)

  "Run the interpreter with given arguments, in buffer *pure-eval*.

Debugging mode of the interpreter (-g) is enabled depending on
the current value of the `pure-debug-mode' variable. The current
status of this variable is also indicated in the mode
line (`debug' mode indicator).

There are a number of other customizable variables which can be
used to control how the interpreter is invoked. In particular,
`pure-stacksize' sets the stack size available to Pure programs
in kilobytes. (If nonzero, this determines the value of the
PURE_STACK environment variable.)

The interpreter is invoked in the directory of the current
buffer (current default directory if no file is associated with
the current buffer).  If buffer exists but process is not
running, make new process.  If buffer exists and process is
running, kill it and start a new one.

Program used comes from variable `pure-prog-name'. The buffer is
put in `pure-eval-mode', giving commands for visiting source files,
sending input, manipulating the command history, etc."

  (interactive)
  (let* ((dir (if buffer-file-name
		  (file-name-directory (buffer-file-name))
		default-directory))
	 (pure-eval-active (not (null (get-buffer "*pure-eval*"))))
	 (pure-eval-running (comint-check-proc "*pure-eval*"))
	 (pure-eval-buffer (get-buffer-create "*pure-eval*")))
    (if (and pure-eval-running
	     pure-query-before-kill
	     (not
	      (y-or-n-p
	       "An interpreter process is still running. Start a new one? ")))
	(message "Aborted")
      (set-buffer pure-eval-buffer)
      (goto-char (point-max))
      (let ((proc (get-buffer-process pure-eval-buffer)))
	(if proc (delete-process proc))) ; Blast any old process.
      (cd dir)
      (if (not pure-eval-active)
	  (pure-eval-mode)
	(if (and pure-eval-running
		 (or (not (string-equal
			   comint-input-ring-file-name pure-histfile))
		     (not (= comint-input-ring-size pure-histsize))))
	    ;; reset history in case any of the options have changed
	    (progn
	      (comint-write-input-ring)
	      (setq comint-input-ring-file-name pure-histfile
		    comint-input-ring-size pure-histsize)
	      (comint-read-input-ring t))))
      ;; disable paging in the interpreter
      (setenv "PURE_MORE" nil)
      (setenv "PURE_LESS" nil)
      ;; set PURE_STACK
      (setenv "PURE_STACK"
	      (if (zerop pure-stacksize) nil
		(format "%d" pure-stacksize)))
      ;; invoke the interpreter
      (comint-exec pure-eval-buffer "pure-eval" pure-prog-name nil
		   (append (list "-q" "-i" "--noediting")
			   (if pure-debug-mode (list "-g") nil)
			   pure-prog-opts args))
      ;; set up process parameters
      (setq pure-output-list nil
	    pure-output-string nil
	    pure-receive-in-progress nil
	    pure-last-script nil
	    pure-last-dir dir)
      (set-process-sentinel (get-process "pure-eval") 'pure-eval-sentinel)
      (if (not pure-query-before-kill)
	  (process-kill-without-query (get-process "pure-eval")))
      ;; switch to and go to the end of the eval buffer
      (pop-to-buffer "*pure-eval*")
      (goto-char (point-max))
      (pure-eval-update-modeline)))
  )

(defun pure-start (&optional arg)
  "Start a new interpreter instance in buffer *pure-eval*, unless
an interpreter is already running. Hides the buffer window if ARG
is non-nil. See `run-pure' for details."
  (interactive "P")
  (if (not (comint-check-proc "*pure-eval*"))
      (save-current-buffer
	(message "starting interpreter...")
	(pure-run)
	(if arg
	    ;; hide the *pure-eval* window
	    (delete-window))
	;; give the interpreter some time to start
	(sleep-for 2)))
  )

(defun pure-quit ()
  "Exit the Pure interpreter and kill its buffer."
  (interactive)
  (let* ((pure-eval-buffer (get-buffer "*pure-eval*"))
	 (pure-eval-process (and pure-eval-buffer
				 (get-buffer-process pure-eval-buffer)))
	 (pure-eval-window (and pure-eval-buffer
				 (get-buffer-window pure-eval-buffer))))
    (if pure-eval-process
	(save-excursion
	  (set-buffer pure-eval-buffer)
	  (comint-write-input-ring)
	  (process-send-eof pure-eval-process)
	  (accept-process-output pure-eval-process)))
    (if pure-eval-window
	(delete-window pure-eval-window))
    (if pure-eval-buffer
	(kill-buffer pure-eval-buffer))))

(defun pure-run-script ()
  "Run the interpreter with the script in the current buffer, in buffer
*pure-eval*. See `run-pure' for details."
  (interactive)
  (let ((script-file
	 (if (buffer-file-name)
	     (file-name-nondirectory (buffer-file-name))
	   (error "Buffer is not associated with any file"))))
    (save-some-buffers)
    (pure-run script-file)
    (setq pure-last-script script-file)))

(defun pure-rerun ()
  "If the interpreter is currently running, rerun it with the
same script (if any). Otherwise, start a new interpreter
instance. See `run-pure' for details."
  (interactive)
  (save-some-buffers)
  (let ((script-file pure-last-script))
    (if (not script-file)
	(pure-run)
      (pure-goto-input-line)
      (pure-run script-file)
      (setq pure-last-script script-file))))

;; debugging mode

(defun pure-toggle-debug-mode (&optional arg)
  "Toggle debugging mode (-g).
Optional numeric ARG, if supplied, turns on debugging when
positive, turns it off when negative, and just toggles it when
zero or left out.

If the value of the flag is changed and the interpreter is
currently running then it is restarted automatically."
  (interactive "P")
  (let ((oldstate pure-debug-mode))
    (when (and
	   (not
	    (eq oldstate
		(setq pure-debug-mode
		      (pure-calculate-state arg pure-debug-mode))))
	   (get-process "pure-eval"))
      (pure-rerun))))

;; run the interpreter to create a TAGS file

(defun pure-make-tags (&optional script-files)
  "Run the interpreter on the given SCRIPT-FILES (the script in
the current buffer by default) to create a TAGS file."
  (interactive
   (let ((default (if (buffer-file-name)
		      (file-name-nondirectory (buffer-file-name)))))
     (list (read-string
	    (format "Make tags from files%s"
		    (if (null default) ": "
		      (format " (default %s): " default)))
	    nil nil default))))
  (when script-files
    (save-some-buffers)
    ;; call-process-shell-command is needed here to expand glob patterns
    (apply 'call-process-shell-command pure-prog-name nil 0 nil "--etags"
	   (split-string-and-unquote script-files))))

;; find a script in the current directory or on the Pure library path

(defun pure-locate-script (file)
  (let ((script
	 (locate-library file t (append (list "." pure-library-dir)
					pure-includes))))
    (if script
	script
      (error (concat "File " file " not found")))))

;; visit source lines of error and debugging messages

(defun pure-current-msg ()
  "Show the source line referenced by an error message on the current line
in the pure-eval buffer."
  (interactive)
  (let ((actwindow (selected-window)))
    (if (get-buffer "*pure-eval*")
	(pop-to-buffer "*pure-eval*")
      (error "No script is running"))
    (cond
     ((save-excursion (forward-line 0) (looking-at pure-msg-regexp))
      (forward-line 0) (recenter 0)
      (let (visit-buffer
	    visit-line
	    (file (match-string 2)) (line (match-string 3)))
	(setq visit-buffer (find-file-noselect (pure-locate-script file)))
	(setq visit-line (string-to-number line))
	(message "%s, line %s" file line)
	(switch-to-buffer-other-window visit-buffer)
	(goto-line visit-line)))
     (t
      (select-window actwindow)
      (error "No message found")))))

(defun pure-current-msg-or-send ()
  "Depending on whether point is at an error message, either execute a
`pure-current-msg' or a `comint-send-input' command. This must be invoked
from the pure-eval buffer."
  (interactive)
  (if (save-excursion (forward-line 0) (looking-at pure-msg-regexp))
      (pure-current-msg)
    (comint-send-input)))

(defun pure-next-msg (&optional count)
  "Advance to the next Pure error message below the current line in the Pure
eval buffer, and show the referenced source line in another window. When used
with a numeric argument n, advance to the nth message below the current line
\(move backwards if numeric argument is negative).

Note that this command can easily be fooled if the running script produces
some output, or you insert some text, which looks like an error message, so
you should take care what you're doing."
  (interactive "P")
  (if (and (numberp count) (< count 0))
      (pure-prev-msg (- count))
    (if (null count) (setq count 1))
    (let ((actwindow (selected-window)))
      (if (get-buffer "*pure-eval*")
	  (pop-to-buffer "*pure-eval*")
	(error "No script is running"))
      (forward-line 0)
      (if (looking-at pure-msg-regexp)
	  (if (save-excursion (end-of-line) (not (eobp)))
	      (forward-line 1)
	    (error "No more messages")))
      (let ((pos (re-search-forward pure-msg-regexp nil t count)))
	(if pos
	    (let ((file (match-string 2)) (line (match-string 3)))
	      (goto-char pos)
	      (recenter 0)
	      (find-file-other-window (pure-locate-script file))
	      (goto-line (string-to-number line))
	      (message "%s, line %s" file line))
	  (select-window actwindow)
	  (error "No more messages"))))))

(defun pure-prev-msg (&optional count)
  "Advance to previous Pure error messages above the current line in the Pure
eval buffer, and show the referenced source line in another window. Like
`pure-next-msg', but moves backward."
  (interactive "P")
  (if (and (numberp count) (< count 0))
      (pure-next-msg (- count))
    (if (null count) (setq count 1))
    (let ((actwindow (selected-window)))
      (if (get-buffer "*pure-eval*")
	  (pop-to-buffer "*pure-eval*")
	(error "No script is running"))
      (forward-line 0)
      (let ((pos (re-search-backward pure-msg-regexp nil t count)))
	(if pos
	    (let ((file (match-string 2)) (line (match-string 3)))
	      (goto-char pos)
	      (recenter 0)
	      (find-file-other-window (pure-locate-script file))
	      (goto-line (string-to-number line))
	      (message "%s, line %s" file line))
	  (select-window actwindow)
	  (error "No more messages"))))))

(defun pure-last-msg ()
  "Advance to the last message in a contiguous sequence of error messages at
or below the current line, and show the referenced source line in another
window."
  (interactive)
  (let ((actwindow (selected-window)))
    (if (get-buffer "*pure-eval*")
	(pop-to-buffer "*pure-eval*")
      (error "No script is running"))
    (forward-line 0)
    (let ((pos
	   (if (looking-at pure-msg-regexp)
	       (point)
	     (re-search-forward pure-msg-regexp nil t))))
      (if pos
	  (progn
	    (goto-char pos)
	    (while (and (save-excursion (end-of-line) (not (eobp)))
			(save-excursion (forward-line 1)
					(looking-at pure-msg-regexp)))
	      (forward-line 1))
	    (let ((file (match-string 2)) (line (match-string 3)))
	      (recenter 0)
	      (find-file-other-window (pure-locate-script file))
	      (goto-line (string-to-number line))
	      (message "%s, line %s" file line)))
	(select-window actwindow)
	(error "No more messages")))))

(defun pure-first-msg ()
  "Advance to the first message in a contiguous sequence of error messages at
or above the current line, and show the referenced source line in another
window."
  (interactive)
  (let ((actwindow (selected-window)))
    (if (get-buffer "*pure-eval*")
	(pop-to-buffer "*pure-eval*")
      (error "No script is running"))
    (forward-line 0)
    (let ((pos
	   (if (looking-at pure-msg-regexp)
	       (point)
	     (re-search-backward pure-msg-regexp nil t))))
      (if pos
	  (progn
	    (goto-char pos)
	    (while (and (not (bobp))
			(save-excursion (forward-line -1)
					(looking-at pure-msg-regexp)))
	      (forward-line -1))
	    (let ((file (match-string 2)) (line (match-string 3)))
	      (recenter 0)
	      (find-file-other-window (pure-locate-script file))
	      (goto-line (string-to-number line))
	      (message "%s, line %s" file line)))
	(select-window actwindow)
	(error "No more messages")))))

(defun pure-mouse-msg (event)
  "Show the source line referenced by an error message under the mouse."
  (interactive "e")
  (mouse-set-point event)
  (if (save-excursion (forward-line 0) (looking-at pure-msg-regexp))
      (progn (forward-line 0) (pure-current-msg))
    (mouse-yank-at-click event 0)))

;; visit main script and the eval buffer

(defun pure-find-script ()
  "Visit the script currently running in the pure-eval buffer."
  (interactive)
  (if (and pure-last-dir pure-last-script)
      (if (not (string-equal (concat pure-last-dir pure-last-script)
			     (buffer-file-name)))
	  (find-file-other-window (concat pure-last-dir pure-last-script)))
    (error "No script is running")))

(defun pure-goto-input-line ()
  "Move to the prompt in the pure-eval buffer."
  (interactive)
  (if (get-buffer "*pure-eval*")
      (progn (pop-to-buffer "*pure-eval*") (goto-char (point-max)))
    (error "No script is running")))

;; Some nice stuff borrowed from Octave mode.

;; Symbol completion for Pure buffers. This differs from the completion
;; function pure-complete used in comint mode in that symbols are looked up in
;; the documentation, so it works the same no matter which scripts are
;; currently loaded in the interpreter. OTOH, this means that only documented
;; symbols will be recognized.

(defun pure-index-filter (symbol)
  (let ((res (replace-regexp-in-string "^\\([^ ]+\\) .*$" "\\1" symbol)))
    (replace-regexp-in-string "^\\(.+\\)\\[[0-9]+\\]$" "\\1" res)))

(defun pure-index-complete (symbol)
  (pure-send-list-and-digest
   (list (concat "help_matches " symbol "\n")))
  ;; Remove all trailing garbage that doesn't belong to the symbols.
  (setq pure-output-list
	(mapcar 'pure-index-filter pure-output-list))
  ;; Sort the list
  (setq pure-output-list
	(sort pure-output-list 'string-lessp))
  ;; Remove duplicates
  (let* ((x pure-output-list)
	 (y (cdr x)))
    (while y
      (if (string-equal (car x) (car y))
	  (setcdr x (setq y (cdr y)))
	(setq x y
	      y (cdr y)))))
  (mapcar 'list pure-output-list))

(defun pure-complete-symbol ()
  "Perform completion for documented Pure symbols on the token
preceding point."
  ;; This code taken from lisp-complete-symbol
  (interactive)
  (pure-start t)
  (let* ((end (point))
	 (beg (save-excursion
		;; skip back one word/identifier or operator (punctuation)
		(skip-syntax-backward "w_")
		(and (eq (point) end)
		     (skip-syntax-backward "."))
		;; skip leading :: in absolute qualid
		(and (looking-at "::")
		     (goto-char (match-end 0)))
		(point)))
	 (string (buffer-substring-no-properties beg end))
	 (completion-alist (pure-dynamic-completion-table pure-index-complete))
	 (completion (try-completion string completion-alist)))
    (cond ((eq completion t))		; ???
	  ((null completion)
	   (message "Can't find completion for `%s'" string)
	   (ding))
	  ((not (string= string completion))
           (delete-region beg end)
           (insert completion))
	  (t
	   (let ((list (all-completions string completion-alist))
		 (conf (current-window-configuration)))
	     ;; Taken from comint.el
	     (message "Making completion list...")
	     (with-output-to-temp-buffer "*Completions*"
	       (display-completion-list list string))
	     (message "Hit space to flush")
	     (let (key first)
	       (if (save-excursion
		     (set-buffer (get-buffer "*Completions*"))
		     (setq key (read-key-sequence nil)
			   first (aref key 0))
		     (and (consp first) (consp (event-start first))
			  (eq (window-buffer (posn-window (event-start
							   first)))
			      (get-buffer "*Completions*"))
			  (eq (key-binding key) 'mouse-choose-completion)))
		   (progn
		     (mouse-choose-completion first)
		     (set-window-configuration conf))
		 (if (eq first ?\ )
		     (set-window-configuration conf)
		   (setq unread-command-events
			 (listify-key-sequence key))))))))))

;; Commands to move to next/previous code line in a Pure buffer. These aren't
;; bound by default, but pure-next-code-line is used to feed a code line into
;; the Pure interpreter in pure-send-line below.

(defun pure-next-code-line (&optional arg)
  "Move ARG lines of Pure code forward (backward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (beginning-of-line)
  (let ((n 0)
	(inc (if (> arg 0) 1 -1)))
    (while (and (/= arg 0) (= n 0))
      (setq n (forward-line inc))
      (while (and (= n 0)
;		  (looking-at "\\s-*\\($\\|//\\)"))
		  (pure-at-empty-or-comment-line))
	(setq n (forward-line inc)))
      (setq arg (- arg inc)))
    n))

(defun pure-previous-code-line (&optional arg)
  "Move ARG lines of Pure code backward (forward if ARG is negative).
Skips past all empty and comment lines.  Default for ARG is 1.

On success, return 0.  Otherwise, go as far as possible and return -1."
  (interactive "p")
  (or arg (setq arg 1))
  (pure-next-code-line (- arg)))

;; Communication with the Pure interpreter. These feed a given string
;; (obtained from the kill ring, a region or the current line in a Pure
;; buffer) into the interpreter. A new interpreter is started if none is
;; currently running.

(defun pure-trim-whitespace (string)
  (if (string-match "^[ \t]+" string)
      (substring string (match-end 0))
    string))

(defun pure-send-string (string)
  "Send a string to the Pure interpreter."
  (interactive "sPure Code: ")
  (pure-start t)
  (let* ((pure-eval-buffer (get-buffer "*pure-eval*"))
	 (proc (get-buffer-process pure-eval-buffer))
	 (skip-prompts (and pure-send-skip-prompts
			    (string-match pure-prompt-regexp
					  (pure-trim-whitespace string))))
	 line)
    (save-excursion
      (set-buffer pure-eval-buffer)
      (goto-char (point-max))
      (setq pure-output-list nil)
      (while (not (string-equal string ""))
	(if (string-match "\n" string)
	    (setq line (substring string 0 (match-beginning 0))
		  string (substring string (match-end 0)))
	  (setq line string string ""))
	(if skip-prompts
	    (let ((str (pure-trim-whitespace line)))
	      (if (string-match pure-prompt-regexp str)
		  (setq line (substring str (match-end 0)))
		(setq line nil))))
	(if line
	    (progn
	     (pure-send-list-and-digest (list (concat line "\n")))
	     (insert-before-markers
	      (mapconcat 'identity
			 (append
			  (if pure-send-echo-input (list line) (list ""))
			  pure-output-list
			  (list pure-output-string))
			 "\n"))))))
    (if pure-send-show-buffer
	(display-buffer pure-eval-buffer))))

(defun pure-send-yank ()
  "Send current contents of the kill buffer to the Pure interpreter."
  (interactive)
  (pure-send-string (substring-no-properties (current-kill 0 t))))

(defun pure-send-region (beg end)
  "Send current region to the Pure interpreter."
  (interactive "r")
  (pure-send-string (buffer-substring-no-properties beg end)))

(defun pure-send-line (&optional arg)
  "Send current Pure code line to the Pure interpreter.
With positive prefix ARG, send that many lines.
If `pure-send-line-auto-forward' is non-nil, go to the next unsent
code line."
  (interactive "P")
  (or arg (setq arg 1))
  (if (> arg 0)
      (let (beg end)
	(beginning-of-line)
	(setq beg (point))
	(pure-next-code-line (- arg 1))
	(end-of-line)
	(setq end (point))
	(if pure-send-line-auto-forward
	    (pure-next-code-line 1))
	(pure-send-region beg end))))

(defun pure-send-defun ()
  "Send the current Pure definition to the Pure interpreter."
  (interactive)
  (and (pure-mark-defun) (pure-send-region (point) (mark))))

(defun pure-eval (expr &optional arg)
  "Evaluate a Pure expression EXPR. Result is printed in the echo
area if prefix ARG is nil, otherwise it is inserted at point into
the current buffer."
  (interactive
   (list (read-string "Pure Eval: ") current-prefix-arg))
  (pure-start t)
  (let* ((pure-eval-buffer (get-buffer "*pure-eval*"))
	 (proc (get-buffer-process pure-eval-buffer))
	 ;; The ";;" here is intentional. Since this is only permitted at the
	 ;; toplevel, it guarantees that we bail out with a syntax error if
	 ;; the expression is incomplete or the interpreter happens to be in
	 ;; the middle of an expression parse.
	 (line (concat expr ";;")))
    (save-excursion
      (set-buffer pure-eval-buffer)
      (setq pure-output-list nil)
      (pure-send-list-and-digest (list (concat line "\n")))))
  (let ((res (mapconcat 'identity pure-output-list "\n")))
    (if arg (insert res) (message res))))

(defun pure-symbol-complete (symbol)
  (pure-send-list-and-digest
   (list (concat "completion_matches " symbol "\n")))
  ;; Sort the list
  (setq pure-output-list
	(sort pure-output-list 'string-lessp))
  ;; Remove duplicates
  (let* ((x pure-output-list)
	 (y (cdr x)))
    (while y
      (if (string-equal (car x) (car y))
	  (setcdr x (setq y (cdr y)))
	(setq x y
	      y (cdr y)))))
  (mapcar 'list pure-output-list))

(defun pure-show (symbol &optional arg)
  "Show the definition of SYMBOL in the Pure interpreter. If
prefix ARG is not nil, insert the output into the current buffer
instead."
  (interactive
   (list
    (let ((default (puresym-at-point)))
      (pure-start t)
      (completing-read
       (format "Show symbol%s"
	       (if (null default) ": "
		 (format " (default %s): " default)))
       (pure-dynamic-completion-table pure-symbol-complete)
       nil nil nil nil default))
    current-prefix-arg))
  (let* ((pure-eval-buffer (get-buffer "*pure-eval*"))
	 (proc (get-buffer-process pure-eval-buffer))
	 line)
    (save-excursion
      (set-buffer pure-eval-buffer)
      (setq pure-output-list nil)
      (setq line (concat "show " symbol))
      (pure-send-list-and-digest (list (concat line "\n")))
      (unless arg
	(insert-before-markers
	 (mapconcat 'identity
		    (append
		     (if pure-send-echo-input (list line) (list ""))
		     pure-output-list
		     (list pure-output-string))
		    "\n"))))
    (cond
     (arg
      (insert (mapconcat 'identity pure-output-list "\n") "\n"))
     (pure-send-show-buffer
      (display-buffer pure-eval-buffer)))))

(defun pure-clear (symbol)
  "Clear the definition of SYMBOL in the Pure interpreter."
  (interactive
   (list
    (let ((default (puresym-at-point)))
      (pure-start t)
      (completing-read
       (format "Clear symbol%s"
	       (if (null default) ": "
		 (format " (default %s): " default)))
       (pure-dynamic-completion-table pure-symbol-complete)
       nil nil nil nil default))))
  (let* ((pure-eval-buffer (get-buffer "*pure-eval*"))
	 (proc (get-buffer-process pure-eval-buffer))
	 line)
    (save-excursion
      (set-buffer pure-eval-buffer)
      (setq pure-output-list nil)
      (setq line (concat "clear " symbol))
      (pure-send-list-and-digest (list (concat line "\n")))
      (insert-before-markers
       (mapconcat 'identity
		  (append
		   (if pure-send-echo-input (list line) (list ""))
		   pure-output-list
		   (list pure-output-string))
		  "\n")))
    (if pure-send-show-buffer
	(display-buffer pure-eval-buffer))))

(defun pure-break (symbol &optional arg)
  "If prefix ARG is nil, set a breakpoint on SYMBOL in the Pure
interpreter, or show all current breakpoints if SYMBOL is empty.
If prefix ARG is not nil, remove an existing breakpoint instead."
  (interactive
   (list
    (let ((default (puresym-at-point)))
      (unless pure-debug-mode
	(error
	 (substitute-command-keys
	  "Debugging is disabled, use \\[pure-toggle-debug-mode] to enable")))
      (pure-start t)
      (completing-read
       (format "%s breakpoint%s"
	       (if current-prefix-arg "Delete" "Set")
	       (if (null default) ": "
		 (format " (default %s): " default)))
       (pure-dynamic-completion-table pure-symbol-complete)
       nil nil nil nil default))
    current-prefix-arg))
  (when (string-match "[ \t]*$" symbol)
    (setq symbol (replace-match "" nil nil symbol)))
  (unless (and arg (zerop (length symbol)))
    (let* ((pure-eval-buffer (get-buffer "*pure-eval*"))
	   (proc (get-buffer-process pure-eval-buffer))
	   line)
      (save-excursion
	(set-buffer pure-eval-buffer)
	(setq pure-output-list nil)
	(setq line (format "%s %s" (if arg "del -b" "break") symbol))
	(pure-send-list-and-digest (list (concat line "\n")))
	(insert-before-markers
	 (mapconcat 'identity
		    (append
		     (if pure-send-echo-input (list line) (list ""))
		     pure-output-list
		     (list pure-output-string))
		    "\n")))
      (if pure-send-show-buffer
	  (display-buffer pure-eval-buffer)))))

(defun pure-trace (symbol &optional arg)
  "If prefix ARG is nil, set a tracepoint on SYMBOL in the Pure
interpreter, or show all current tracepoints if SYMBOL is empty.
If prefix ARG is not nil, remove an existing tracepoint instead."
  (interactive
   (list
    (let ((default (puresym-at-point)))
      (unless pure-debug-mode
	(error
	 (substitute-command-keys
	  "Debugging is disabled, use \\[pure-toggle-debug-mode] to enable")))
      (pure-start t)
      (completing-read
       (format "%s tracepoint%s"
	       (if current-prefix-arg "Delete" "Set")
	       (if (null default) ": "
		 (format " (default %s): " default)))
       (pure-dynamic-completion-table pure-symbol-complete)
       nil nil nil nil default))
    current-prefix-arg))
  (when (string-match "[ \t]*$" symbol)
    (setq symbol (replace-match "" nil nil symbol)))
  (unless (and arg (zerop (length symbol)))
    (let* ((pure-eval-buffer (get-buffer "*pure-eval*"))
	   (proc (get-buffer-process pure-eval-buffer))
	   line)
      (save-excursion
	(set-buffer pure-eval-buffer)
	(setq pure-output-list nil)
	(setq line (format "%s %s" (if arg "del -t" "trace") symbol))
	(pure-send-list-and-digest (list (concat line "\n")))
	(insert-before-markers
	 (mapconcat 'identity
		    (append
		     (if pure-send-echo-input (list line) (list ""))
		     pure-output-list
		     (list pure-output-string))
		    "\n")))
      (if pure-send-show-buffer
	  (display-buffer pure-eval-buffer)))))

;; send commands to the Pure interpreter and digest their results

(defun pure-output-digest (proc string)
  (setq string (concat pure-output-string string))
  (while (string-match "\n" string)
    (setq pure-output-list
	  (append pure-output-list
		  (list (substring string 0 (match-beginning 0))))
	  string (substring string (match-end 0))))
  (if (string-match pure-prompt-regexp string)
      (setq pure-receive-in-progress nil))
  (setq pure-output-string string))

(defun pure-send-list-and-digest (list)
  (let* ((pure-eval-buffer (get-buffer "*pure-eval*"))
	 (proc (get-buffer-process pure-eval-buffer))
	 (filter (process-filter proc))
	 string)
    (set-process-filter proc 'pure-output-digest)
    (setq pure-output-list nil)
    (unwind-protect
	(while (setq string (car list))
	  (setq pure-output-string nil
		pure-receive-in-progress t)
	  (comint-send-string proc string)
	  (while pure-receive-in-progress
	    ;; Make sure to use a reasonable timeout here, otherwise an
	    ;; unresponsive interpreter can easily lock up emacs.
	    (unless (or (accept-process-output proc pure-process-timeout)
			(if (get-buffer-process pure-eval-buffer) nil
			  (error "Pure interpreter has exited"))
			(y-or-n-p
			 "Pure interpreter seems unresponsive, continue? "))
	      (error "Aborted")))
	  (setq list (cdr list)))
      (set-process-filter proc filter)
      (setq pure-receive-in-progress nil))))

;; perform cleanup when the interpreter process is killed

(defun pure-eval-sentinel (proc msg)
  (if (null (buffer-name (process-buffer proc)))
      ;; buffer has been killed
      (set-process-buffer proc nil)
    (set-buffer (process-buffer proc))
    (comint-write-input-ring)
    (setq pure-last-dir nil
	  pure-last-script nil)
    (goto-char (point-max))
    (insert "\n*** Process pure-eval finished ***\n")))

;; make sure that the history is written when exiting emacs
(add-hook 'kill-emacs-hook
	  (lambda ()
	    (let ((pure-eval-buffer (get-buffer "*pure-eval*")))
	      (cond
	       (pure-eval-buffer
		(set-buffer pure-eval-buffer)
		(comint-write-input-ring))))))

;; some helpers used in the indentation and filling code below

(defmacro pure-safe (&rest body)
  ;; safely execute BODY, return nil if an error occurred
  `(condition-case nil
       (progn ,@body)
       (error nil)))

(defsubst pure-within-string ()
  ;; Check whether point is within a Pure string.
  (save-excursion
    (nth 3 (parse-partial-sexp (point-min) (point)))))

(defsubst pure-within-literal ()
  ;; Check whether point is within a Pure literal (string or comment).
  (let ((state
	 (save-excursion
	   (parse-partial-sexp (point-min) (point)))))
    (or (nth 3 state) (nth 4 state))))

(defsubst pure-within-literal-or-sexp ()
  ;; Check whether point is within a Pure literal or sexp.
  (let ((state
	 (save-excursion
	   (parse-partial-sexp (point-min) (point)))))
    (or (> (car state) 0) (nth 3 state) (nth 4 state))))

(defsubst pure-within-literal-or-list ()
  ;; Check whether point is within a Pure literal or Pure list (bracket sexp).
  (let ((state
	 (save-excursion
	   (parse-partial-sexp (point-min) (point)))))
    (or (and (> (car state) 0)
	     (equal ?\[ (char-after (nth 1 state))))
	(nth 3 state) (nth 4 state))))

(defmacro pure-forward-sexp (&optional arg)
  ;; stripped-down version of forward-sexp
  (or arg (setq arg 1))
  `(goto-char (or (scan-sexps (point) ,arg)
		  ,(if (numberp arg)
		       (if (> arg 0) `(point-max) `(point-min))
		     `(if (> ,arg 0) (point-max) (point-min))))))

(defmacro pure-backward-sexp (&optional arg)
  ;; stripped-down version of backward-sexp
  (or arg (setq arg 1))
  `(pure-forward-sexp ,(if (numberp arg) (- arg) `(- ,arg))))

(defun pure-backward-sexp-bol ()
  ;; special version of backward-sexp which always stops at bol
  (let ((oldpos (point))
	(bol (save-excursion (beginning-of-line)
			     (skip-chars-forward " \t")
			     (and (not (pure-within-literal-or-sexp))
				  (point))))
	(newpos (or (scan-sexps (point) -1) (point-min))))
    (goto-char (if (and bol (< bol oldpos)) (max bol newpos) newpos))))

(defun pure-backward-sexp-bol2 ()
  (pure-backward-sexp-bol)
  (let ((oldpos (point))
	(bol (save-excursion (beginning-of-line)
			     (skip-chars-forward " \t")
			     (and (not (pure-within-literal-or-sexp))
				  (point))))
	(newpos (or (scan-sexps (point) -1) (point-min))))
    (if (and bol (< bol oldpos) (< newpos bol))
	(goto-char bol)
      (point))))

;; indentation (experimental)

;; Based on Emacs' Pascal mode by Espen Skoglund <esk@gnu.org>, adjusted for
;; Pure's block constructs which can also occur inside expressions.

(defconst pure-beg-block-re     "\\_<\\(case\\|when\\|with\\)\\_>")
(defconst pure-beg-block-re2    "\\_<\\(of\\|when\\|with\\)\\_>")
(defconst pure-end-block-re     "\\_<end\\_>")
;(defconst pure-declaration-re   "\\_<\\(const\\|extern\\|infix[lr]?\\|interface\\|let\\|namespace\\|\\(non\\|out\\|post\\|pre\\)fix\\|private\\|public\\|using\\)\\_>")
(defconst pure-declaration-re   "\\_<\\(const\\|extern\\|infix[lr]?\\|interface\\|let\\|namespace\\|\\(non\\|out\\|post\\|pre\\)fix\\|private\\|public\\|using\\)\\_>\\|^#!")
(defconst pure-ifelse-re        "\\_<\\(if\\|else\\)\\_>")
(defconst pure-noindent-re      "\\_<\\(end\\|then\\|else\\)\\_>")
;(defconst pure-comment-start-re "\\(/[/*]\\|%<\\|^#!\\)")
(defconst pure-comment-start-re "\\(/[/*]\\|%<\\)")

(defconst pure-indent-alist
  '((block . (+ ind pure-block-indent))
    (declaration . (+ ind pure-block-indent))
;;     (comment . (pure-indent-comment))
    (contexp . ind)
    (string . 0) (toplevel . 0)))

(defun pure-indent-line ()
  "Indent current line as a Pure code."
  (let* ((indent-str (pure-calculate-indent))
	 (type (car indent-str))
	 (ind (car (cdr indent-str)))
	 (indent-fun (assoc type pure-indent-alist)))
    (delete-horizontal-space)
    (cond (; Anything unkown gets relative indent
	   (not indent-fun)
	   (indent-relative))
	  (; Some things should not be indented
	   (and (eq type 'declaration) (looking-at pure-declaration-re))
	   ())
	  (; Other things should have no extra indent
	   (and (or (eq type 'toplevel) (eq type 'block))
		(looking-at pure-noindent-re))
	   (indent-to ind))
	  (; But most lines are treated this way
	   (indent-to (eval (cdr indent-fun)))
	   ))))

(defun pure-alignment-column (&optional lim &optional flag)
  "Look for an alignment column (`=') in the vicinity of
point. LIM, if given, limits the search to the region between
point and LIM. Return a list of two elements: (EQUALS-COL
INDENT-COL)."
  (unless lim
    (setq lim (point-at-eol)))
  (save-excursion
   (when (re-search-forward "[ \t]\\(=\\)\\([ \t]\\|[ \t]*$\\)" lim t)
     (goto-char (match-beginning 1))
     ;; Note that we generally exclude a '=' directly inside a list here, to
     ;; avoid improper alignments relative to a '=' in a list comprehension.
     (and (not (pure-within-literal-or-list))
	  (if flag
	      ;; do only basic block indent
	      (let ((col (pure-basic-indent-level)))
		(list col (+ col pure-block-indent)))
	    ;; return the proper alignment column
	    (let* ((col1 (current-column))
		   (col2 (progn
			   (goto-char (match-end 1))
			   (skip-chars-forward " \t")
			   (max (current-column) (+ col1 2)))))
	      (list col1 col2)))))))

(defun pure-calculate-indent ()
  "Calculate the indent of the current Pure line.
Return a list of two elements: (INDENT-TYPE INDENT-LEVEL)."
  (save-excursion
    (let* ((parse-sexp-ignore-comments t)
	   (oldpos (point)) (lim oldpos)
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (level (car state)) (nest 0) (pos 0) (complete nil)
	   (at-block-beg (looking-at
			  (concat "[ \t]*\\(" pure-beg-block-re "\\)")))
	   (at-equal (and (zerop level) (looking-at "[ \t]*=\\([ \t]\\|$\\)")))
	   (extra-indent (if at-block-beg pure-extra-indent 0))
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((nth 3 state) (throw 'nesting 'string))
			 ((nth 4 state) (throw 'nesting 'comment))
			 ((> level 0)
			  (save-excursion
			    (goto-char (nth 1 state))
			    (setq pos (1+ (current-column))))))
		   ;; Loop until correct indent is found
		   (while t
		     ;; The backward-sexp may fail if we reach the beginning
		     ;; of an enclosing sexp, assume toplevel in this case.
		     (unless (pure-safe (pure-backward-sexp))
		       (throw 'nesting 'toplevel))
		     ;; Check for a trailing semicolon which completes a
		     ;; definition.
		     (if (save-excursion (pure-forward-sexp 1)
					 (looking-at "[ \t]*;"))
			 (setq complete t))
		     ;; Check for alignment (`=') with a preceding rule. This
		     ;; is taken into account if point was either at a lone
		     ;; '=' or in the middle of an incomplete definition.
		     (let* ((flag (not pure-indent-equals))
			    (col (and (or at-equal
					  (and (not complete)
					       (not at-block-beg)))
				      (= nest 0)
				      (pure-alignment-column lim flag))))
		       (when col
			 (setq pos (if (and at-equal complete) (car col)
				     (car (cdr col))))
			 (throw 'nesting 'toplevel)))
		     (cond (;--Nest block outwards
			    (looking-at pure-beg-block-re)
			    (if (> nest 0)
				(if (zerop (setq nest (1- nest)))
				    (setq lim (point)))
			      (throw 'nesting 'block)))
			   (;--Nest block inwards
			    (looking-at pure-end-block-re)
			    (setq nest (1+ nest)))
			   (;--Declaration part
			    (looking-at pure-declaration-re)
			    (if (and
				 (looking-at "\\_<namespace\\_>")
				 (looking-back "\\_<using\\_>\\s *"
					       (point-at-bol)))
				(pure-backward-sexp 1))
			    (unless complete
			      (throw 'nesting 'declaration)))
			   (;--If-then-else
			    (and (zerop nest)
				 (not complete)
				 (looking-at pure-ifelse-re))
			    (throw 'nesting 'block))
			   (;--Beginning of program
			    (bobp)
			    (throw 'nesting 'toplevel)))
		     (if (bobp) (throw 'nesting 'toplevel))
		     ))))

      ;; Return type of block and indent level.
      (cond (;; Toplevel, indent comes from pos.
	     (eq type 'toplevel)
	     (list 'contexp (+ extra-indent pos)))
	    (;; Indentation inside if-then-else gets special treatment: block
	     ;; starters never get an extra indent, and other elements are
	     ;; always indented to the column of the if/else keyword, not the
	     ;; indentation of the line containing the if-then-else.
	     (and (or (eq type 'toplevel) (eq type 'block))
		  (looking-at pure-ifelse-re))
	     ;; look for else-if
	     (if (looking-back "\\_<else\\_>\\s *" (point-at-bol))
	       (pure-backward-sexp 1))
	     (if at-block-beg
		 (list
		  type
		  (+ extra-indent
		     ;; compensate for extra if-then-else indent
		     (- (pure-indent-level) pure-block-indent)))
	       (list type (current-column))))
	    (;; Anything else gets indent from pure-indent-level.
	     t
	     (list type (+ extra-indent (pure-indent-level))))))))

(defun pure-calculate-alignment ()
  "Similar to `pure-calculate-indent', but this function
specifically determines the alignment of equations (`=' symbol).
The return value is just the alignment column, or zero if none."
  (save-excursion
    (let* ((parse-sexp-ignore-comments t)
	   (oldpos (point)) (lim oldpos)
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (level (car state)) (nest 0) (pos 0) (complete nil)
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((nth 3 state) (throw 'nesting 'string))
			 ((nth 4 state) (throw 'nesting 'comment))
			 ((> level 0) (throw 'nesting 'toplevel)))
		   ;; Loop until correct indent is found
		   (while t
		     (pure-backward-sexp 1)
		     (if (save-excursion (pure-forward-sexp 1)
					 (looking-at "[ \t]*;"))
			 (setq complete t))
		     (let ((col (and (= nest 0)
				     (pure-alignment-column lim))))
		       (when col
			 (setq pos (car col))
			 (throw 'nesting 'toplevel)))
		     (cond (;--Nest block outwards
			    (looking-at pure-beg-block-re)
			    (if (> nest 0)
				(if (zerop (setq nest (1- nest)))
				    (setq lim (point)))
			      (throw 'nesting 'block)))
			   (;--Nest block inwards
			    (looking-at pure-end-block-re)
			    (setq nest (1+ nest)))
			   (;--Declaration part
			    (looking-at pure-declaration-re)
			    (if (and
				 (looking-at "\\_<namespace\\_>")
				 (looking-back "\\_<using\\_>\\s *"
					       (point-at-bol)))
				(pure-backward-sexp 1))
			    (unless complete
			      (throw 'nesting 'declaration)))
			   (;--If-then-else
			    (and (zerop nest)
				 (not complete)
				 (looking-at pure-ifelse-re))
			    (throw 'nesting 'block))
			   (;--Beginning of program
			    (bobp)
			    (throw 'nesting 'toplevel)))
		     (if (bobp) (throw 'nesting 'toplevel))
		     ))))

      (if (eq type 'toplevel) pos 0))))

(defun pure-basic-indent-level ()
  "Return the basic indent level of the current line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))

(defun pure-indent-level ()
  "Return the indent level of the current line."
  (let* ((state (save-excursion (parse-partial-sexp (point-min) (point))))
	 (level (car state))
	 (at-block-beg (looking-at pure-beg-block-re))
	 (lim (point))
	 (pos (save-excursion
		(beginning-of-line)
		(skip-chars-forward " \t")
		(point)))
	 (pos (cond
	       ((zerop level) pos)
	       (at-block-beg (max pos (1+ (nth 1 state))))
	       (t pos))))
    (save-excursion
      (goto-char pos)
      (if (and pure-indent-equals at-block-beg)
	  ;; Special "hanging" indent for blocks at the end of an equation.
	  (cond
	   ((looking-at "=[ \t]")
	    (forward-char)
	    (skip-chars-forward " \t"))
	   ((save-excursion
	      (and (re-search-forward "[ \t]\\(=\\)\\([ \t]\\|[ \t]*$\\)"
				      (point-at-eol) t)
		   (> lim (match-end 1))))
	    (goto-char (match-end 1))
	    (skip-chars-forward " \t"))))
      (current-column))))

(defun pure-indent-comment ()
  "Return indent for current comment."
  (save-excursion
    (re-search-backward "/\\*\\|%<" nil t)
    (+ (current-column) 3)))

;; Electric characters doing auto-indentation and/or alignment.

(defun pure-electric-char ()
  "Insert a character and automatically reindent the line."
  (interactive)
  (insert last-command-char)
  (if (and pure-electric-keys (not (pure-within-literal)))
      (save-excursion
	(beginning-of-line)
	(pure-indent-line))))

(defun pure-electric-equal ()
  "Insert a `=' character and reindent the line if at the
beginning of the line. Otherwise, if point is at the end of the
line and `pure-align-equals' is t, align the `=' symbol to the
current `=' indentation column, as given by
`pure-calculate-alignment'."
  (interactive)
  (cond ((or (not pure-electric-keys) (pure-within-literal))
	 (insert last-command-char))
	((save-excursion
	   (skip-chars-backward " \t")
	   (bolp))
	 (insert last-command-char)
	 (save-excursion
	   (beginning-of-line)
	   (pure-indent-line)))
	((eq (char-before) ?=)
	 ;; We may have a `==' operator here, so undo any previous alignment
	 ;; or indentation.
	 (progn
	   (save-excursion
	     (backward-char)
	     (when (or (eq (char-before) ?\s) (eq (char-before) ?\t))
	       (delete-horizontal-space)
	       (insert " ")))
	   (insert last-command-char))
	 (save-excursion
	   (beginning-of-line)
	   (if (looking-at "[ \t]*==")
	       (pure-indent-line))))
	((and pure-align-equals
	      ;; require bol or whitespace, so that we don't tear apart
	      ;; operators with a trailing `=', such as `~='
	      (or (bolp) (eq (char-before) ?\s) (eq (char-before) ?\t))
	      (not (pure-within-literal-or-sexp))
	      (save-excursion
		(skip-chars-forward " \t")
		(eolp)))
	 (save-excursion
	   (beginning-of-line)
	   (pure-indent-line))
	 (let ((col (current-column))
	       (icol (pure-calculate-alignment)))
	   (if (> icol col)
	       (move-to-column icol t))
	   (insert last-command-char)))
	(t (insert last-command-char))))

(defun pure-electric-tab ()
  "Function called when TAB is pressed in `pure-mode'."
  (interactive)
  ;; Do nothing if within a string.
  (if (pure-within-string)
      (insert "\t")
    ;; If pure-tab-always-indent, indent the beginning of the line.
    (if pure-tab-always-indent
	(save-excursion
	  (beginning-of-line)
	  (pure-indent-line))
      (if (save-excursion
	    (skip-chars-backward " \t")
	    (bolp))
	  (pure-indent-line)
	(insert "\t")))
    (skip-chars-forward " \t")))

(defun pure-indent-region (start end)
  ;; Indent every line whose first char is between START and END inclusive.
  (let (p)
    (save-excursion
      (goto-char start)
      (setq p (copy-marker end))
      (while (and (bolp)
		  (not (eobp))
		  (< (point) p))
	(pure-indent-line)
	(forward-line 1)))))

(defun pure-indent-line-or-region ()
  "When the region is active, indent it.  Otherwise indent the current line."
  (interactive)
  (if (pure-region-is-active-p)
      (pure-indent-region (region-beginning) (region-end))
    (pure-indent-line)))

;; Some convenience functions to quickly change between different indentation
;; and editing styles. These are set on a per-buffer basis.

(defun pure-toggle-indent-mode (&optional arg)
  "Toggle the `pure-indent-equals' flag.
Optional numeric ARG, if supplied, turns on this flag when
positive, turns it off when negative, and just toggles it when
zero or left out."
  (interactive "P")
  (setq pure-indent-equals (pure-calculate-state arg pure-indent-equals))
  (pure-update-modeline))

(defun pure-toggle-align-mode (&optional arg)
  "Toggle the `pure-align-equals' flag.
Optional numeric ARG, if supplied, turns on this flag when
positive, turns it off when negative, and just toggles it when
zero or left out."
  (interactive "P")
  (setq pure-align-equals (pure-calculate-state arg pure-align-equals))
  (pure-update-modeline))

(defun pure-toggle-electric-mode (&optional arg)
  "Toggle the `pure-electric-keys' flag.
Optional numeric ARG, if supplied, turns on this flag when
positive, turns it off when negative, and just toggles it when
zero or left out."
  (interactive "P")
  (setq pure-electric-keys (pure-calculate-state arg pure-electric-keys))
  (pure-update-modeline))

;; movement by defuns

;; This is still a bit experimental. It's somewhat difficult to recognize the
;; beginning of a definition reliably in Pure since ordinary equations and
;; singleton expressions don't start with a keyword, so we have to resort to a
;; heuristic approach here. At present, we recognize a defun only when it
;; starts at the beginning of a line and it is either a declaration or macro
;; definition, or it has a `;' or a block start keyword before it.

(defun pure-at-empty-or-comment-line ()
  ;; Check whether we're at an empty or comment line.
  (save-excursion
    (beginning-of-line)
    (or (let* ((parse-sexp-ignore-comments t)
	       (state
		(save-excursion (parse-partial-sexp (point-min) (point)))))
	  (nth 4 state))
	(looking-at
	 (concat "[ \t]*\\($\\|" pure-comment-start-re "\\)")))))

(defun pure-check-defun ()
  (save-excursion
    (let ((chk (< (skip-chars-backward " \t") 0)))
      (when (or (and (bolp) (not (pure-at-empty-or-comment-line)))
		(and chk (looking-back pure-beg-block-re2 (point-at-bol))))
	(or (looking-at
	     (concat "[ \t]*\\(" pure-declaration-re
		     "\\|\\_<def\\_>\\|\\_<type\\_>\\)"))
	    (let ((parse-sexp-ignore-comments t))
	      (pure-backward-sexp 1)
	      (or (bobp)
		  (looking-at pure-beg-block-re2)
		  (progn
		    (pure-forward-sexp 1)
		    (looking-at "[ \t]*;")))))))))

(defun pure-at-defun ()
  (and
   ;; Make sure that we're not inside a comment, string or parenthesized
   ;; expression.
   (let* ((parse-sexp-ignore-comments t)
	  (state
	   (save-excursion (parse-partial-sexp (point-min) (point)))))
     (not (or (> (car state) 0) (nth 3 state) (nth 4 state))))
   (pure-check-defun)))

(defun pure-find-head ()
  (let ((pos (point)))
    (while (not (pure-check-defun))
      (pure-backward-sexp-bol))
    (skip-chars-forward " \t\n")
    (if (> (point) pos) (goto-char pos) (point))))

(defun pure-find-eqn-head (&optional lim)
  (unless lim
    (setq lim (point-at-eol)))
  (let ((pos
	 (save-excursion
	   (when (re-search-forward "[ \t]\\(=\\)\\([ \t]\\|[ \t]*$\\)" lim t)
	     (goto-char (match-beginning 1))
	     (if (not (pure-within-literal-or-sexp))
		 (pure-find-head))))))
    (if pos
	(goto-char pos))))

;; FIXME: These don't recognize blocks inside sexps right now.

(defun pure-prev-defun (&optional arg)
  "Move backward to the beginning of a Pure definition.
If point is already at the beginning of a definition, move
backward to the previous definition at the current or a lower
block level, if any. Otherwise move up one block level past the
current definition. Prefix arg may be used to specify a repeat
count."
  (interactive "p")
  (unless (integerp arg) (setq arg 1))
  (cond
   ((zerop arg) (point))
   ((< arg 0) (pure-next-defun (- arg)))
   ((> arg 1)
    (while (> arg 0)
      (pure-prev-defun 1)
      (setq arg (1- arg))))
   (t
    (let* ((parse-sexp-ignore-comments t)
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (level (car state)) (nest 0)
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((or (nth 3 state) (nth 4 state))
			  (goto-char (nth 8 state))
			  (throw 'nesting 'string-or-comment))
			 ((> level 0)
			  (goto-char (nth 1 state))
			  (throw 'nesting 'toplevel)))
		   (if (and (not (pure-at-defun))
			    (looking-back pure-beg-block-re2 (point-at-bol)))
		       (pure-backward-sexp 1))
		   ;; Loop until beginning of next defun is found
		   (while t
		     (pure-backward-sexp-bol)
		     (if (bobp) (throw 'nesting 'toplevel))
		     (cond (;--Nest block outwards
			    (looking-at pure-beg-block-re2)
			    (if (> nest 0)
				(setq nest (1- nest))
			      (pure-forward-sexp 1)
			      (throw 'nesting 'block)))
			   (;--Nest block inwards
			    (looking-at pure-end-block-re)
			    (setq nest (1+ nest)))
			   (;--Declaration part
			    (looking-at pure-declaration-re)
			    (if (and
				 (looking-at "\\_<namespace\\_>")
				 (looking-back "\\_<using\\_>\\s *"
					       (point-at-bol)))
				(pure-backward-sexp 1))
			    (throw 'nesting 'declaration))
			   (;--Beginning of program
			    (bobp)
			    (throw 'nesting 'toplevel))
			   (;--Beginning of expression
			    (pure-check-defun)
			    (throw 'nesting 'toplevel)))
		     ))))

      (if (bobp)
	  (point)
	(if (and (not (eq type 'declaration))
		 (not (eq type 'block))
		 (or (> level 0)
		     (not (eq type 'toplevel))
		     (looking-at "=\\([ \t]\\|$\\)")))
	    (pure-prev-defun)
	  (point)))))))

(defun pure-next-defun (&optional arg)
  "Move forward to the beginning of a Pure definition.
If point is already at the beginning of a definition, move
forward to the next definition at the current or a lower block
level, if any. Otherwise move up one block level past the current
definition. Prefix arg may be used to specify a repeat count."
  (interactive "p")
  (unless (integerp arg) (setq arg 1))
  (cond
   ((zerop arg) (point))
   ((< arg 0) (pure-prev-defun (- arg)))
   ((> arg 1)
    (while (> arg 0)
      (pure-next-defun 1)
      (setq arg (1- arg))))
   (t
    (let* ((parse-sexp-ignore-comments t)
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (level (car state)) (nest 0)
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((or (nth 3 state) (nth 4 state))
			  (goto-char (nth 8 state))
			  (throw 'nesting 'string-or-comment))
			 ((> level 0)
			  (goto-char (nth 1 state))
			  (throw 'nesting 'toplevel)))
		   (if (or (pure-at-defun)
			   (looking-at pure-declaration-re)
			   (looking-at pure-end-block-re))
		       (pure-forward-sexp 1))
		   ;; Loop until beginning of next defun is found
		   (while t
		     (pure-forward-sexp 1)
		     (if (eobp) (throw 'nesting 'toplevel))
		     (cond (;--Nest block outwards
			    (looking-back pure-end-block-re (point-at-bol))
			    (if (> nest 0)
				(setq nest (1- nest))
			      (pure-backward-sexp 1)
			      (throw 'nesting 'block)))
			   (;--Nest block inwards
			    (looking-back pure-beg-block-re (point-at-bol))
			    (setq nest (1+ nest)))
			   (;--Declaration part
			    (looking-back pure-declaration-re (point-at-bol))
			    (when (and (zerop nest)
				       (not
					(looking-back
					 "\\_<using\\_>\\s *\\_<namespace\\_>\\s *"
					 (point-at-bol))))
			      (pure-backward-sexp 1)
			      (throw 'nesting 'declaration)))
			   (;--End of program
			    (eobp)
			    (throw 'nesting 'toplevel))
			   (;--Beginning of expression
			    (save-excursion (pure-backward-sexp-bol2)
					    (pure-check-defun))
			    (when (>= nest 0)
			      (pure-backward-sexp-bol2)
			      (throw 'nesting 'toplevel))))
		     ))))

      (if (eobp)
	  (point)
	(if (and (not (eq type 'declaration))
		 (not (eq type 'block))
		 (or (> level 0)
		     (not (eq type 'toplevel))
		     (looking-at "=\\([ \t]\\|$\\)")))
	    (pure-next-defun)
	  (point)))))))

(defun pure-backward-defun (&optional arg)
  "Move backward to the beginning of a Pure definition.
If point is already at the beginning of a definition, move
backward to the previous sibling at the same block level, if
any. Otherwise move up one block level, like `pure-prev-defun'.
Prefix arg may be used to specify a repeat count."
  (interactive "p")
  (unless (integerp arg) (setq arg 1))
  (cond
   ((zerop arg) (point))
   ((< arg 0) (pure-forward-defun (- arg)))
   ((> arg 1)
    (while (> arg 0)
      (pure-backward-defun 1)
      (setq arg (1- arg))))
   (t
    (let* ((parse-sexp-ignore-comments t)
	   (oldpos (point)) (lim oldpos)
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (level (car state)) (nest 0)
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((or (nth 3 state) (nth 4 state))
			  (goto-char (nth 8 state))
			  (throw 'nesting 'string-or-comment))
			 ((> level 0)
			  (goto-char (nth 1 state))
			  (throw 'nesting 'toplevel)))
		   (if (and (not (pure-at-defun))
			    (looking-back pure-beg-block-re2 (point-at-bol)))
		       (pure-backward-sexp 1))
		   ;; Loop until beginning of previous defun is found
		   (while t
		     (pure-backward-sexp-bol)
		     (if (and (= nest 0)
			      (pure-find-eqn-head lim))
			 (throw 'nesting 'toplevel))
		     (cond (;--Nest block outwards
			    (looking-at pure-beg-block-re2)
			    (if (> nest 0)
				(if (zerop (setq nest (1- nest)))
				    (setq lim (point)))
			      (pure-forward-sexp 1)
			      (throw 'nesting 'block)))
			   (;--Nest block inwards
			    (looking-at pure-end-block-re)
			    (setq nest (1+ nest)))
			   (;--Declaration part
			    (looking-at pure-declaration-re)
			    (if (and
				 (looking-at "\\_<namespace\\_>")
				 (looking-back "\\_<using\\_>\\s *"
					       (point-at-bol)))
				(pure-backward-sexp 1))
			    (if (zerop nest)
				(throw 'nesting 'declaration)))
			   (;--Beginning of program
			    (bobp)
			    (throw 'nesting 'toplevel))
			   (;--Beginning of expression
			    (pure-check-defun)
			    (if (zerop nest)
				(throw 'nesting 'toplevel))))
		     ))))

      (if (and (not (eq type 'declaration))
	       (not (eq type 'block))
	       (or (> level 0)
		   (not (eq type 'toplevel))
		   (looking-at "=\\([ \t]\\|$\\)")))
	  (pure-backward-defun)
	(point))))))

(defun pure-forward-defun (&optional arg)
  "Move forward to the beginning of a Pure definition.
If point is already at the beginning of a definition, move
forward to the next sibling at the same block level, if
any. Otherwise move up one block level past the current
definition, like `pure-next-defun'. Prefix arg may be used to
specify a repeat count."
  (interactive "p")
  (unless (integerp arg) (setq arg 1))
  (cond
   ((zerop arg) (point))
   ((< arg 0) (pure-backward-defun (- arg)))
   ((> arg 1)
    (while (> arg 0)
      (pure-forward-defun 1)
      (setq arg (1- arg))))
   (t
    (let* ((parse-sexp-ignore-comments t)
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (level (car state)) (nest 0)
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((or (nth 3 state) (nth 4 state))
			  (goto-char (nth 8 state))
			  (throw 'nesting 'string-or-comment))
			 ((> level 0)
			  (goto-char (nth 1 state))
			  (throw 'nesting 'toplevel)))
		   (if (or (pure-at-defun)
			   (looking-at pure-declaration-re)
			   (looking-at pure-end-block-re))
		       (pure-forward-sexp 1))
		   ;; Loop until beginning of next defun is found
		   (while t
		     (pure-forward-sexp 1)
		     (if (eobp) (throw 'nesting 'toplevel))
		     (cond (;--Nest block outwards
			    (looking-back pure-end-block-re (point-at-bol))
			    (if (> nest 0)
				(setq nest (1- nest))
			      (pure-backward-sexp 1)
			      (throw 'nesting 'block)))
			   (;--Nest block inwards
			    (looking-back pure-beg-block-re (point-at-bol))
			    (setq nest (1+ nest)))
			   (;--Declaration part
			    (looking-back pure-declaration-re (point-at-bol))
			    (when (and (zerop nest)
				       (not
					(looking-back
					 "\\_<using\\_>\\s *\\_<namespace\\_>\\s *"
					 (point-at-bol))))
			      (pure-backward-sexp 1)
			      (throw 'nesting 'declaration)))
			   (;--End of program
			    (eobp)
			    (throw 'nesting 'toplevel))
			   (;--Beginning of expression
			    (save-excursion (pure-backward-sexp-bol2)
					    (pure-check-defun))
			    (when (zerop nest)
			      (pure-backward-sexp-bol2)
			      (throw 'nesting 'toplevel))))
		     ))))

      (if (eobp)
	  (point)
	(if (and (not (eq type 'declaration))
		 (not (eq type 'block))
		 (or (> level 0)
		     (not (eq type 'toplevel))
		     (looking-at "=\\([ \t]\\|$\\)")))
	    (pure-forward-defun)
	  (point)))))))

(defun pure-mark-section ()
  (let* ((state
	  (save-excursion
	    (parse-partial-sexp (point-min) (point))))
	 (state
	  ;; check to see whether we might be looking at an inline code section
	  (if (nth 4 state)
	      state
	    (if (and (not (nth 3 state)) (looking-at "[ \t]*\\(%<\\|/\\*\\)"))
		(save-excursion
		  ;; skip over the start marker
		  (skip-chars-forward " \t") (forward-char 2)
		  ;; reparse from here
		  (parse-partial-sexp (point-min) (point)))
	      ;; not an inline section, give up
	      state)))
	 ;; determine the start position of the comment or section, if any
	 (start (if (nth 4 state) (nth 8 state))))
    (when start
      (goto-char start)
      (forward-comment 1)
      (set-mark (point))
      (goto-char start)
      (point))))

(defun pure-mark-defun ()
  "Mark the current Pure definition at or around point.
Put mark at the end of the definition and point at the
beginning. Return point if successful, nil otherwise."
  (interactive)
  (if (pure-at-empty-or-comment-line)
      (pure-mark-section)
    (or (pure-at-defun) (pure-backward-defun))
    (pure-forward-defun)
    ;; skip over any trailing whitespace and comments
    (let ((parse-sexp-ignore-comments t)) (pure-backward-sexp 1))
    (let ((parse-sexp-ignore-comments nil)) (pure-forward-sexp 1))
    (skip-chars-backward " \t\n")
    ;; skip over any trailing garbage if we can
    (or (and (looking-at "[ \t]*;?\\(//.*\\|[ \t]*\\)?$") (forward-line 1))
	(and (looking-at "[ \t]*;") (skip-chars-forward " \t;")))
    (set-mark (point))
    (pure-backward-defun)
    (point)))

;; Code-folding support, using hideshow (experimental).
;; FIXME: This doesn't recognize blocks inside sexps right now.

(defconst pure-hs-beg-block-re "\\_<\\(case\\|when\\|with\\)\\_>")
(defconst pure-hs-end-block-re "\\_<end\\_>")

(defun pure-hs-forward-sexp (&optional arg)
  "Move forward to the end of the current block. Point must be
positioned at the beginning of the block, otherwise do a
`forward-sexp'. This is useful as a replacement for forward-sexp
in hs-minor-mode."
  (interactive "p")
  (unless (integerp arg) (setq arg 1))
  (cond
   ((<= arg 0) (point)) ;; we don't support negative repeat counts for now
   ((> arg 1)
    (while (> arg 0)
      (pure-hs-forward-block 1)
      (setq arg (1- arg))))
   ((looking-at pure-hs-beg-block-re)
    (let* ((parse-sexp-ignore-comments t)
	   (state (save-excursion (parse-partial-sexp (point-min) (point))))
	   (level (car state)) (nest 0)
	   (type (catch 'nesting
		   ;; Check if inside a string, comment or parenthesis
		   (cond ((or (nth 3 state) (nth 4 state))
			  (throw 'nesting 'string-or-comment))
			 ((> level 0)
			  (throw 'nesting 'toplevel)))
		   ;; Loop until end of block is found
		   (while t
		     (pure-forward-sexp 1)
		     (if (eobp) (throw 'nesting 'toplevel))
		     (cond (;--Nest block outwards
			    (looking-back pure-hs-end-block-re (point-at-bol))
			    (setq nest (1- nest))
			    (if (= nest 0)
				(throw 'nesting 'block)))
			   (;--Nest block inwards
			    (looking-back pure-hs-beg-block-re (point-at-bol))
			    (setq nest (1+ nest))))
		     ))))
      (point)))
   (t (forward-sexp 1))))

;; If hideshow mode is already loaded, set up some convenient defaults for
;; Pure mode.
(if (featurep 'hideshow)
    (setq hs-special-modes-alist
	  (cons
	   (list 'pure-mode pure-hs-beg-block-re pure-hs-end-block-re
		 "/[*/]\\|%<" 'pure-hs-forward-sexp nil)
	   hs-special-modes-alist)))

;; Filling. This is the paragraph-fill from q-mode which in turn was based on
;; (XEmacs) cc-mode.

(defun pure-literal-limits (&optional lim near)
  ;; Returns a cons of the beginning and end positions of the comment
  ;; or string surrounding point (including both delimiters), or nil
  ;; if point isn't in one.  If LIM is non-nil, it's used as the
  ;; "safe" position to start parsing from.  If NEAR is non-nil, then
  ;; the limits of any literal next to point is returned.  "Next to"
  ;; means there's only [ \t] between point and the literal.  The
  ;; search for such a literal is done first in forward direction.
  ;;
  ;; This is the Emacs 19 version.
  (save-excursion
    (let* ((pos (point))
	   (lim (or lim (point-min)))
	   (state (parse-partial-sexp lim (point))))
      (cond ((nth 3 state)
	     ;; String.  Search backward for the start.
	     (while (nth 3 state)
	       (search-backward (make-string 1 (nth 3 state)))
	       (setq state (parse-partial-sexp lim (point))))
	     (cons (point) (or (pure-safe (pure-forward-sexp 1) (point))
			       (point-max))))
	    ((nth 7 state)
	     ;; Line comment.  Search from bol for the comment starter.
	     (beginning-of-line)
	     (setq state (parse-partial-sexp lim (point))
		   lim (point))
	     (while (not (nth 7 state))
	       (search-forward "//")	; Should never fail.
	       (setq state (parse-partial-sexp
			    lim (point) nil nil state)
		     lim (point)))
	     (backward-char 2)
	     (cons (point) (progn (forward-comment 1) (point))))
	    ((nth 4 state)
	     ;; Block comment.  Search backward for the comment starter.
	     (while (nth 4 state)
	       (search-backward "/*")	; Should never fail.
	       (setq state (parse-partial-sexp lim (point))))
	     (cons (point) (progn (forward-comment 1) (point))))
	    ((pure-safe (nth 4 (parse-partial-sexp ; Can't use prev state due
			     lim (1+ (point))))) ; to bug in Emacs 19.34.
	     ;; We're standing in a comment starter.
	     (backward-char 2)
	     (cons (point) (progn (forward-comment 1) (point))))
	    (near
	     (goto-char pos)
	     ;; Search forward for a literal.
	     (skip-chars-forward " \t")
	     (cond
	      ((eq (char-syntax (or (char-after) ?\ )) ?\") ; String.
	       (cons (point) (or (pure-safe (pure-forward-sexp 1) (point))
				 (point-max))))
	      ((looking-at pure-comment-start-re) ; Line or block comment.
	       (cons (point) (progn (forward-comment 1) (point))))
	      (t
	       ;; Search backward.
	       (skip-chars-backward " \t")
	       (let ((end (point)) beg)
		 (cond
		  ((eq (char-syntax (or (char-before) ?\ )) ?\") ; String.
		   (setq beg (pure-safe (pure-backward-sexp 1) (point))))
		  ((and (pure-safe (forward-char -2) t)
			(looking-at "*/"))
		   ;; Block comment.  Due to the nature of line
		   ;; comments, they will always be covered by the
		   ;; normal case above.
		   (goto-char end)
		   (forward-comment -1)
		   ;; If LIM is bogus, beg will be bogus.
		   (setq beg (point))))
		 (if beg (cons beg end))))))
	    ))))

(defun pure-fill-paragraph (&optional arg)
  "Like \\[fill-paragraph] but handles Pure (i.e., C/C++) style
comments. If any of the current line is a comment or within a comment,
fill the comment or the paragraph of it that point is in,
preserving the comment indentation or line-starting decorations.

If point is inside multiline string literal, fill it.  This currently
does not respect escaped newlines, except for the special case when it
is the very first thing in the string.  The intended use for this rule
is in situations like the following:

description = \"\\
A very long description of something that you want to fill to make
nicely formatted output.\"\;

If point is in any other situation, i.e. in normal code, do nothing.

Optional prefix ARG means justify paragraph as well."
  (interactive "*P")
  (let* ((point-save (point-marker))
	 limits
	 comment-start-place
	 (first-line
	  ;; Check for obvious entry to comment.
	  (save-excursion
	    (beginning-of-line)
	    (skip-chars-forward " \t")
	    (and (looking-at comment-start-skip)
		 (setq comment-start-place (point)))))
	 (re1 "\\|\\([ \t]*/\\*[ \t]*\\|[ \t]*\\*/[ \t]*\\|[ \t/*]*\\)"))
    (if (save-excursion
	  (beginning-of-line)
	  (looking-at "#!\\|.*//"))
	(let ((fill-prefix fill-prefix)
	       ;; Lines containing just a comment start or just an end
	       ;; should not be filled into paragraphs they are next
	       ;; to.
	      (paragraph-start (concat paragraph-start re1 "$"))
	      (paragraph-separate (concat paragraph-separate re1 "$")))
	  (save-excursion
	    (beginning-of-line)
	    ;; Move up to first line of this comment.
	    (while (and (not (bobp))
 			(looking-at "[ \t]*//[ \t]*[^ \t\n]"))
	      (forward-line -1))
 	    (if (not (looking-at ".*//[ \t]*[^ \t\n]"))
		(forward-line 1))
	    ;; Find the comment start in this line.
	    (re-search-forward "[ \t]*//[ \t]*")
	    ;; Set the fill-prefix to be what all lines except the first
	    ;; should start with.  But do not alter a user set fill-prefix.
	    (if (null fill-prefix)
		(setq fill-prefix (buffer-substring (match-beginning 0)
						    (match-end 0))))
	    (save-restriction
	      ;; Narrow down to just the lines of this comment.
	      (narrow-to-region (point-at-bol)
				(save-excursion
				  (forward-line 1)
				  (while
				      (looking-at (regexp-quote fill-prefix))
				    (forward-line 1))
				  (point)))
	      (or (pure-safe
		   ;; fill-paragraph sometimes fails to detect when we
		   ;; are between paragraphs.
		   (beginning-of-line)
		   (search-forward fill-prefix (point-at-eol))
		   (looking-at paragraph-separate))
		  ;; Avoids recursion
		  (let (fill-paragraph-function)
		    (fill-paragraph arg))))))
      ;; else C style comments
      (if (or first-line
	      ;; t if we enter a comment between start of function and
	      ;; this line.
	      (save-excursion
		(setq limits (pure-literal-limits))
		(and (consp limits)
		     (save-excursion
		       (goto-char (car limits))
		       (looking-at pure-comment-start-re))))
	      ;; t if this line contains a comment starter.
	      (setq first-line
		    (save-excursion
		      (beginning-of-line)
		      (prog1
			  (re-search-forward comment-start-skip
					     (save-excursion (end-of-line)
							     (point))
					     t)
			(setq comment-start-place (point)))))
	      ;; t if we're in the whitespace after a comment ender
	      ;; which ends its line.
	      (and (not limits)
		   (when (and (looking-at "[ \t]*$")
			      (save-excursion
				(beginning-of-line)
				(looking-at ".*\\*/[ \t]*$")))
		     (save-excursion
		       (forward-comment -1)
		       (setq comment-start-place (point)))
		     t)))
	  ;; Inside a comment: fill one comment paragraph.
	  (let ((fill-prefix
		 (or
		  ;; Keep user set fill prefix if any.
		  fill-prefix
		  ;; The prefix for each line of this paragraph
		  ;; is the appropriate part of the start of this line,
		  ;; up to the column at which text should be indented.
		  (save-excursion
		    (beginning-of-line)
		    (if (looking-at ".*/\\*.*\\*/")
			(progn (re-search-forward comment-start-skip)
			       (make-string (current-column) ?\ ))
		      (if first-line
			  (forward-line 1)
			(if (and (looking-at "[ \t]*\\*/")
				 (not (save-excursion
					(forward-line -1)
					(looking-at ".*/\\*"))))
			    (forward-line -1)))

		      (let ((line-width (progn (end-of-line)
					       (current-column))))
			(beginning-of-line)
			(prog1
			    (buffer-substring
			     (point)

			     ;; How shall we decide where the end of the
			     ;; fill-prefix is?
			     (progn
			       (skip-chars-forward " \t*" (point-at-eol))
			       ;; kludge alert, watch out for */, in
			       ;; which case fill-prefix should *not*
			       ;; be "*"!
			       (if (and (eq (char-after) ?/)
					(eq (char-before) ?*))
				   (forward-char -1))
			       (point)))

			  ;; If the comment is only one line followed
			  ;; by a blank line, calling move-to-column
			  ;; above may have added some spaces and tabs
			  ;; to the end of the line; the fill-paragraph
			  ;; function will then delete it and the
			  ;; newline following it, so we'll lose a
			  ;; blank line when we shouldn't.  So delete
			  ;; anything move-to-column added to the end
			  ;; of the line.  We record the line width
			  ;; instead of the position of the old line
			  ;; end because move-to-column might break a
			  ;; tab into spaces, and the new characters
			  ;; introduced there shouldn't be deleted.

			  ;; If you can see a better way to do this,
			  ;; please make the change.  This seems very
			  ;; messy to me.
			  (delete-region (progn (move-to-column line-width)
						(point))
					 (progn (end-of-line) (point)))))))))

		;; Lines containing just a comment start or just an end
		;; should not be filled into paragraphs they are next
		;; to.
		(paragraph-start (concat paragraph-start re1 "$"))
		(paragraph-separate (concat paragraph-separate re1 "$"))
		(chars-to-delete 0)
		)
	    (save-restriction
	      ;; Don't fill the comment together with the code
	      ;; following it.  So temporarily exclude everything
	      ;; before the comment start, and everything after the
	      ;; line where the comment ends.  If comment-start-place
	      ;; is non-nil, the comment starter is there.  Otherwise,
	      ;; point is inside the comment.
	      (narrow-to-region (save-excursion
				  (if comment-start-place
				      (goto-char comment-start-place)
				    (search-backward "/*"))
				  (if (and (not pure-hanging-comment-starter-p)
					   (looking-at
					    (concat pure-comment-start-re
						    "[ \t]*$")))
				      (forward-line 1))
				  ;; Protect text before the comment
				  ;; start by excluding it.  Add
				  ;; spaces to bring back proper
				  ;; indentation of that point.
				  (let ((column (current-column)))
				    (prog1 (point)
				      (setq chars-to-delete column)
				      (insert-char ?\  column))))
				(save-excursion
				  (if comment-start-place
				      (goto-char (+ comment-start-place 2)))
				  (search-forward "*/" nil 'move)
				  (if (and (not pure-hanging-comment-ender-p)
					   (save-excursion
					     (beginning-of-line)
					     (looking-at "[ \t]*\\*/")))
				      (beginning-of-line)
				    (forward-line 1))
				  (point)))
	      (or (pure-safe
		   ;; fill-paragraph sometimes fails to detect when we
		   ;; are between paragraphs.
		   (beginning-of-line)
		   (search-forward fill-prefix (point-at-eol))
		   (looking-at paragraph-separate))
		  ;; Avoids recursion
		  (let (fill-paragraph-function)
		    (fill-paragraph arg)))
	      (save-excursion
		;; Delete the chars we inserted to avoid clobbering
		;; the stuff before the comment start.
		(goto-char (point-min))
		(if (> chars-to-delete 0)
		    (delete-region (point) (+ (point) chars-to-delete)))
		;; Find the comment ender (should be on last line of
		;; buffer, given the narrowing) and don't leave it on
		;; its own line, unless that's the style that's desired.
		(goto-char (point-max))
		(forward-line -1)
		(search-forward "*/" nil 'move)
		(beginning-of-line)
		(if (and pure-hanging-comment-ender-p
			 (looking-at "[ \t]*\\*/"))
		    ;(delete-indentation)))))
		    (let ((fill-column (+ fill-column 9999)))
		      (forward-line -1)
		      (fill-region-as-paragraph (point) (point-max))
		      ;; If fill-prefix ended with a `*', it may be
		      ;; taken away from the comment ender.  We got to
		      ;; check this and put it back if that is the
		      ;; case.
		      (goto-char (- (point-max) 2))
		      (if (not (= (char-before) ?*))
			  (insert ?*))
		      )))))
	;; Else maybe a string.  Fill it if it's a multiline string.
	;; FIXME: This currently doesn't handle escaped newlines.
	;; Doing that correctly is a bit tricky.
	(if (and limits
		 (eq (char-syntax (char-after (car limits))) ?\")
		 (save-excursion
		   (goto-char (car limits))
		   (end-of-line)
		   (< (point) (cdr limits))))
	    (let (fill-paragraph-function)
	      (save-restriction
		(narrow-to-region (save-excursion
				    (goto-char (1+ (car limits)))
				    (if (looking-at "\\\\$")
					;; Some DWIM: Leave the start
					;; line if it's nothing but an
					;; escaped newline.
					(1+ (match-end 0))
				      (point)))
				  (save-excursion
				    (goto-char (1- (cdr limits)))
				    ;; Inserting a newline and
				    ;; removing it again after
				    ;; fill-paragraph makes it more
				    ;; predictable.
				    (insert ?\n)
				    (point)))
		;; Do not compensate for the narrowed column.  This
		;; way the literal will always be filled at the same
		;; column internally.
		(fill-paragraph arg)
		(goto-char (1- (point-max)))
		(delete-char 1)))
	  )))
    (goto-char (marker-position point-save))
    (set-marker point-save nil)
    ;; Always return t.  This has the effect that if filling isn't
    ;; done above, it isn't done at all, and it's therefore
    ;; effectively disabled in normal code.
    t))

(provide 'pure-mode)
