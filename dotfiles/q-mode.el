(require 'comint)

(defgroup q nil "Major mode for editing Q code" :group 'languages)

(defcustom q-server ""
"If non-nil, Q-Shell will ssh to remote box before executing."
  :type 'string
  :group 'q)

(defgroup q-init nil "Q initialization variables" :group 'q)

(defcustom q-init-port 0
  "If non-zero, Q-Shell will start with the specified server port."
  :type 'integer
  :group 'q-init)

(defcustom q-init-user ""
  "If non-nil, Q-Shell will user this value to log in."
  :type 'string
  :group 'q-init)

(defcustom q-init-password ""
  "If non-nil, Q-Shell will user this value to log in."
  :type 'string
  :group 'q-init)

(defcustom q-init-slaves 0
  "If non-zero, Q-Shell will start with the specified number of slaves."
  :type 'integer
  :group 'q-init)

(defcustom q-init-workspace 0
  "If non-zero, Q-Shell will start with the specified workspace limit."
  :type 'integer
  :group 'q-init)

(defcustom q-init-digits 7
  "If non-zero, Q-Shell will start with the specified print resolution."
  :type 'integer
  :group 'q-init)

(defcustom q-init-file ""
  "If non-empty, Q-Shell will load the specified file."
  :type 'file
  :group 'q-init)

(defcustom q-init-envfile "/u/kdb/kdb.env"
  "Q-Shell will souce the specified file before executing q on a remote machine."
  :type 'file
  :group 'q-init)

(defcustom q-qsm-path "qsm"
  "Path to the qsm binary to run q"
  :type 'file
  :group 'q-init)

(defgroup q-faces nil "Q custom faces" :group 'q :group 'faces)

(defface q-space-face '((((background light)) (:background "lightgray"))
			(((background dark))  (:background "darkgray")))
  "Face for highliting first space of new line."
  :group `q-faces)

(defun q-default-args ()
  "Build a list of default args out of the q-init customizable variables."
  (setq args "")
  (unless (equal q-init-file "") (setq args (format "%s " q-init-file)))
  (unless (equal q-init-port 0) (setq args (concat args (format "-p %s " q-init-port))))
  (unless (equal q-init-slaves 0) (setq args (concat args (format "-s %s " q-init-slaves))))
  (unless (equal q-init-workspace 0) (setq args (concat args (format "-w %s " q-init-workspace))))
  (unless (equal q-init-user "") (setq args (concat args (format "-u %s:%s " q-init-user q-init-password))))
  (setq args (concat args (format "-P %s " q-init-digits)))
  args)

(defun q-shell (&optional args)
  "Invoke q executable."
  (interactive (let* ((args (q-default-args)))
		 (list (if current-prefix-arg 
			   (read-string "q command line args: " args)
			 args))))
  (setq buf (get-buffer-create "*q*"))
  (switch-to-buffer-other-window buf)
  (set-buffer buf)
  (inferior-q-mode)
  (comint-exec buf "q" q-qsm-path nil nil)
  (setq comint-input-ring-file-name (concat (getenv "HOME") "/.q_history"))
  (comint-read-input-ring)
  (set-process-sentinel (get-process "q") 'q-process-sentinel)
  (goto-char (point-max))
  )

(defun q-process-sentinel (proc message)
  "Sentinel for use with q processes.
   This marks the process with a message, at a particular time point."
  (save-excursion
    (setq message (substring message 0 -1)) ; strip newline
    (set-buffer (process-buffer proc))
    (comint-write-input-ring)
    (goto-char (point-max))
    (insert-before-markers
     (format "\nProcess %s %s at %s\n"
	     (process-name proc) message (current-time-string)))))

(defun q-font-mode ()
  (interactive)
  (set-syntax-table q-mode-syntax-table)
  (set (make-local-variable 'font-lock-defaults) '((q-font-lock-keywords 
						    q-font-lock-keywords-1
						    q-font-lock-keywords-2
						    q-font-lock-keywords-3))))

(defun q-strip-tramp (str)
  "Removes the /ssh:... string prepending the location of a file via TRAMP"
  (replace-regexp-in-string "/ssh:[a-zA-Z0-9_-]+@[a-zA-Z0-9_-]+:" "" str)
  )

(defun q-show (var)
  (interactive "svariable: ")
  (q-send-string (format "%s" var)))

;; These functions have been edited for use with qsm - they will fail on local q instances
;; to revert, just remove the `:r' prepended to each string
(defun q-load (file)
  (interactive "file: ")
  (q-send-string (format "system \"l %s\"" (q-strip-tramp file))))

(defun q-directory (dir)
  (interactive "sdirectory: ")
  (q-send-string (format ":r \\d %s" dir)))

(defun q-tables (dir)
  (interactive "sdirectory: ")
  (q-send-string (format ":r \\a %s" dir)))

(defun q-views (dir)
  (interactive "sdirectory: ")
  (q-send-string (format ":r \\b %s" dir)))

(defun q-functions (dir)
  (interactive "sdirectory: ")
  (q-send-string (format ":r \\f %s" dir)))

(defun q-variables (dir)
  (interactive "sdirectory: ")
  (q-send-string (format ":r \\v %s" dir)))

(defun q-port (p)
  (interactive "nport: ")
  (q-send-string (format ":r \\p %d" p)))

(defun q-timer (s)
  (interactive "nmilliseconds: ")
  (q-send-string (format ":r \\t %d" s)))

(defun q-console (height width)
  (interactive "nheight: \nnwidth: ")
  (q-send-string (format ":r \\c %d %d" height width)))

(defun q-send-raw (text)
  (q-send-string (concat ":r " text)))

(defun q-workspace ()
  (interactive)
  (q-send-string ":r \\w"))

(defun q-strip (text); order matters, don't rearrange
  (while (string-match "/ .*" text) (setq text (replace-match "" t t text))) ; / comments
  (while (string-match "\\([\n;)]\\)[ \t]*/.*" text) (setq text (replace-match "\\1" t nil text))) ; /comments
  ;(while (string-match "\n[ \t]+" text) (setq text (replace-match " " t t text))) ; fold functions
  (while (string-match "\\([:;)]\\)[ \t]+" text) (setq text (replace-match "\\1" t nil text))) ; excess white space
  text)

(defun q-send-string (string)
;  (if (equal q-server "") 
;      (save-excursion 
;	(set-buffer (process-buffer (get-process "q")))
;	(goto-char (point-max))
;	(insert (concat string "\n"))))
  (comint-simple-send (get-process "q") string)
  (process-send-eof (get-process "q")))

(defun q-eval-region (start end)
  "Send the current region to the inferior q process."
  (interactive "r")
  (q-send-string (q-strip (buffer-substring start end))))

(defun q-eval-line ()
  "Send the current line to the inferior q process."
  (interactive)
  (q-eval-region (point-at-bol) (point-at-eol)))

(defun q-eval-buffer ()
  "Load current buffer into the inferior q process."
  (interactive)
  (q-eval-region (point-min) (point-max)))

(defun q-eval-function ()
  "Send the current function to the inferior q process."
  (interactive)
  (save-excursion 
    (goto-char (point-at-eol)) ; go to end of line
    (let ((start (re-search-backward "^\\\w+[ \t]*:[ \t]*{")) ; find beinning of function
	  (end   (re-search-forward ":")) ; find end of function name
	  (fun   (thing-at-point `sexp))) ; find function body
      (q-send-string (q-strip (concat (buffer-substring start end) fun))))))

(defun q-load-buffer()
  "Load current buffer's file into the inferior q process after saving."
  (interactive)
  (save-buffer)
  (q-load (buffer-file-name)))

(defun q-fix-quote()
  "Fix the fontifying that results from partial printing of string."
  (interactive)
  (q-show "\""))

(defun q-show-symbol ()
  "Show contents of symbol under curser."
  (interactive)
  (q-show (thing-at-point `symbol)))

(defun q-show-region (start end)
  "Show result of selected expression."
  (interactive "r")
  (q-show (q-strip (buffer-substring start end))))

(defun q-type (string)
  (q-send-string (format "type %s" string)))

(defun q-type-symbol ()
  "Show type of symbol under curser."
  (interactive)
  ;(q-type (thing-at-point `symbol)))
  (q-type (symbol-at-point)))

(defun q-type-region (start end)
  "Show type of selected expression."
  (interactive "r")
  (q-type (q-strip (buffer-substring start end))))

(defun q-plot (table)
  "Plot a table with gnuplot, and show in the buffer")

(defun remove-windows-carriage-return ()
  "remove carriage return created by people using evil PCs."
  (interactive)
  (format-replace-strings '(("
" . ""))))

; keymaps
(defvar inferior-q-mode-map 
  (let ((inferior-q-mode-map (make-keymap)))
    (define-key inferior-q-mode-map "\C-c\M-s"	'q-show)
    (define-key inferior-q-mode-map "\C-c\M-l"	'q-load)
    (define-key inferior-q-mode-map "\C-c\M-d"	'q-directory)
    (define-key inferior-q-mode-map "\C-c\M-a"	'q-tables)
    (define-key inferior-q-mode-map "\C-c\M-b"	'q-views)
    (define-key inferior-q-mode-map "\C-c\M-f"	'q-functions)
    (define-key inferior-q-mode-map "\C-c\M-v"	'q-variables)
    (define-key inferior-q-mode-map "\C-c\M-p"	'q-port)
    (define-key inferior-q-mode-map "\C-c\M-t"	'q-timer)
    (define-key inferior-q-mode-map "\C-c\M-w"	'q-workspace) 
    (define-key inferior-q-mode-map "\C-c\M-c"	'q-console)
    inferior-q-mode-map)
  "Keymap for inferior q mode")

(set-keymap-parent inferior-q-mode-map comint-mode-map)

(defvar q-mode-map
  (let ((q-mode-map (make-keymap))) ;;; maybe use make-sparse-keymap
    (define-key q-mode-map "\C-c\C-l"	'q-eval-line)
    (define-key q-mode-map "\C-c\C-f"	'q-eval-function)
    (define-key q-mode-map "\C-c\C-r"	'q-eval-region)
    (define-key q-mode-map "\C-c\C-b"	'q-eval-buffer)
    (define-key q-mode-map "\C-c\C-s"	'q-show-symbol)
    (define-key q-mode-map "\C-c\M-s"	'q-show-region)
    (define-key q-mode-map "\C-c\C-t"	'q-type-symbol)
    (define-key q-mode-map "\C-c\M-t"	'q-type-region)
    (define-key q-mode-map "\C-c\M-l"	'q-load-buffer)
    (define-key q-mode-map "\C-c\M-f"	'q-fix-quote)
    (define-key q-mode-map "\C-c\C-c"   'comment-region)
    (define-key q-mode-map "\C-c\M-m"   'remove-windows-carriage-return)
    q-mode-map)
  "Keymap for q major mode")

; pulldowns
(defvar q-menubar-menu nil)
(defconst q-menubar-menu-1
  (purecopy
   '("Q"
     ["Eval Line"      q-eval-line t]
     ["Eval Function"  q-eval-function t]
     ["Eval Region"    q-eval-region t]
     ["Eval Buffer"    q-eval-buffer t]
     "---"
     ["Show Symbol"    q-show-symbol t]
     ["Show Region"    q-show-region t]
     ["Type Symbol"    q-type-symbol t]
     ["Type Region"    q-type-region t]
     "---"
     ["Comment Region" comment-region t]
     ["Load File"      q-load-buffer t]
     ["Remove ^M"      remove-windows-carriage-return t]
     "---"
     ["Q Shell"        q t]
     )))

(defvar inferior-q-menubar-menu nil)
(defconst inferior-q-menubar-menu-1
  (purecopy
   '("Q-Shell"
     ["Show"           q-show t]
     ["Load"           q-load t]
     ["Directory"      q-directory t]
     ["Tables"         q-tables t]
     ["Views"          q-views t]
     ["Variables"      q-variables t]
     ["Port"           q-port t]
     ["Timer"          q-timer t]
     ["Workspace"      q-workspace t]
     ["Console"        q-console t]
     "---"
     ["Fix Quote"      q-fix-quote t]
     )))

; faces 
; font-lock-comment-face font-lock-string-face
; font-lock-doc-string-face font-lock-keyword-face
; font-lock-builtin-face font-lock-function-name-face
; font-lock-variable-name-face font-lock-type-face
; font-lock-constant-face font-lock-reference-face
; font-lock-preprocessor-face font-lock-warning-face
; this is needed for emacs yuck!
(defvar font-lock-preprocessor-face 'font-lock-preprocessor-face)
(defvar font-lock-reference-face 'font-lock-reference-face)
(defvar q-preprocessor-words
  (eval-when-compile
    (concat "\\b" (regexp-opt 
		   '("acos" "asin" "atan" "avg" "avgs" "bin" "by" "cos" "delete" "do"
		     "exec" "exit" "exp" "from" "getenv" "if" "in" "insert" "last" "like"
		     "log" "max" "min" "prd" "select" "sin" "sqrt" "ss" "sum" "tan"
		     "update" "wavg" "where" "while" "within" "wsum" "xbar") 
		   t) "\\b"))
  "Preprocessor words for q mode")

(defvar q-function-words
  (eval-when-compile
    (concat "\\b" (regexp-opt '("abs" "aj" "all" "and" "any" "asc" "asof" "attr" "ceiling"	
				"cols" "cor" "count" "cov" "cross" "csv" "cut" "deltas" "desc" "dev"
				"differ" "distinct" "each" "enlist" "eval" "except" "fby" "fills" "first" "flip" "floor"
				"get" "group" "gtime" "hclose" "hcount" "hdel" "hopen" "hsym" "iasc" "idesc"
				"inter" "inv" "key" "keys" "lj" "load" "lower" "lsq" "ltime" "ltrim"
				"mavg" "maxs" "mcount" "md5" "mdev" "med" "meta" "mins" "mmax" "mmin"
				"mmu" "mod" "msum" "neg" "next" "not" "null" "or" "parse" "peach" "pj"
				"plist" "prds" "prev" "rand" "rank" "ratios" "raze" "read0" "read1" "reciprocal"
				"reverse" "rload" "rotate" "rsave" "rtrim" "save" "set" "show" "signum" "ssr"
				"string" "sublist" "sums" "sv" "system" "tables" "til" "trim" "txf" "type"
				"uj" "ungroup" "union" "upper" "upsert" "value" "var" "view" "views" "vs"
				"where" "xasc" "xcol" "xcols" "xdesc" "xexp" "xgroup" "xkey" "xlog" "xprev"
				"xrank") 
			      t) "\\b"))
  "Functions defined in q.k")

(defvar q-builtin-words
  (eval-when-compile
    (concat (regexp-opt '(".z.K" ".z.Z" ".z.a" ".z.b" ".z.f" ".z.h" ".z.k" ".z.l" ".z.o" ".z.pc" ".z.pg"
			  ".z.ph" ".z.pi" ".z.po" ".z.pp" ".z.ps" ".z.pw" ".z.s" ".z.ts" ".z.u" ".z.vs"
			  ".z.w" ".z.x" ".z.z" ".z.zd") 
		t) "\\b"))
  "Builtin Functions for q mode")

(defvar q-font-lock-keywords		; order matters, don't rearrange
  (list '("^\/\n\\(?:.\\|\n\\)*?\n\\\\\n" 0 font-lock-comment-face t) ; lines between singleton '/' and '\' are coments
	'("^\/.*" 0 font-lock-comment-face t) ; lines starting with a '/' are also comments
	'("^\\\\[a-z] +\\(?:\\w\\|[/._]\\)*" 0 font-lock-reference-face t) ; lines starting with a '\' are compile time
	)
  "Minimal highlighting expressions for q mode")

(defvar q-font-lock-keywords-1
  (append q-font-lock-keywords
	  (list
	   '("`:\\(?:\\w\\|[/:._]\\)*" . font-lock-preprocessor-face) ; files
	   '("`\\(?:\\(?:\\w\\|[.]\\)\\(?:\\w\\|[/:._]\\)*\\)?" . font-lock-constant-face) ; symbols
	   '("\\b[a-zA-Z_]\\(?:\\w\\|[._]\\)*?[_ \t]*,?::?" . font-lock-variable-name-face) ; variables
	   (cons q-preprocessor-words  'font-lock-preprocessor-face) ; select from
	   '("\\b[0-2]:" . font-lock-preprocessor-face) ; IO/IPC
	  ))
  "More highlighting expressions for q mode")

(defvar q-font-lock-keywords-2
  (append q-font-lock-keywords-1
	  (list
	   (cons q-function-words 'font-lock-function-name-face) ; q.k
	   (cons q-builtin-words 'font-lock-builtin-face) ; .z.*
	   ))
  "Even More highlighting expressions for q mode")

(defvar q-font-lock-keywords-3
  (append q-font-lock-keywords-2
	  (list
	   '("^'.*" 0 font-lock-warning-face t) ; error
	   '("'`\\w*" 0 font-lock-warning-face t) ; signal
	   '("\\b[0-9][0-9][0-9][0-9]\\.[0-9][0-9]\\(?:m\\|\\.[0-9][0-9]\\)" . font-lock-constant-face) ; date
	   '("\\(?:T\\|\\b\\)[0-9][0-9]:[0-9][0-9]\\(?:[:][0-9][0-9]\\(?:[.][0-9][0-9][0-9]\\)?\\)?" . font-lock-constant-face) ; time
	   '("\\b[0-9]*\\.[0-9]*\\([eE]-?[0-9][0-9]*\\)?[ef]?\\b" . font-lock-constant-face) ; floats/reals
	   '("\\b[0-9]+[hjef]?\\b" . font-lock-constant-face) ; int/shorts/longs
	   '("\\b[01]+b\\b" . font-lock-constant-face) ; bool
	   '("\\b0x[0-9a-fA-F]+\\b" . font-lock-constant-face) ; bytes
	   '("\\b0[nNwW][efhijmdzuvt]?\\b" . font-lock-constant-face) ; null/infinty
	   '("TODO\\|NOTE" 0 font-lock-warning-face t)  ; TODO
	   '("^[ \t]+" 0 q-space-face t)  ; leading spaces
	   ))
  "Most highlighting expressions for q mode")

; syntax table
(defvar q-mode-syntax-table
  (let ((q-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?\  ". 1" q-mode-syntax-table)  ; first char of a comment can be ' '
    (modify-syntax-entry ?\t ". 1" q-mode-syntax-table)  ; first char of a comment can be '\t'
    (modify-syntax-entry ?\; ". 1" q-mode-syntax-table)  ; first char of a comment can be ';'
    (modify-syntax-entry ?/  ". 2" q-mode-syntax-table)  ; second char of a comment is '/'
    (modify-syntax-entry ?\n "> " q-mode-syntax-table)   ; comments are ended by a new line
    (modify-syntax-entry ?\^M "> " q-mode-syntax-table)  ; comments are ended by a new line
    (modify-syntax-entry ?.  "_  " q-mode-syntax-table)  ; treat . as a symbol but not word character
    (modify-syntax-entry ?\\ ".  " q-mode-syntax-table)  ; treat \ as a normal character
    (modify-syntax-entry ?$  ".  " q-mode-syntax-table)  ; treat $ as a normal character
    (modify-syntax-entry ?%  ".  " q-mode-syntax-table)  ; treat % as a normal character
    (modify-syntax-entry ?`  "'  " q-mode-syntax-table)  ; treat ` as a normal character
    (modify-syntax-entry ?:  ".  " q-mode-syntax-table)  ; treat : as a normal character
    (modify-syntax-entry ?\( "() " q-mode-syntax-table)
    (modify-syntax-entry ?\) ")( " q-mode-syntax-table)
    (modify-syntax-entry ?\[ "(] " q-mode-syntax-table)
    (modify-syntax-entry ?\] ")[ " q-mode-syntax-table)
    (modify-syntax-entry ?\{ "(} " q-mode-syntax-table)
    (modify-syntax-entry ?\} "){ " q-mode-syntax-table)
    q-mode-syntax-table)
  "Syntax table for q-mode")

(defun q-to-comment-or-eol ()
  "Go to position before comment on the current line, or to end of line.
Returns true if comment is found."
  (let (state stop-in point (lim (progn (end-of-line) (point))))
    (beginning-of-line)
    (if (or
	 (eq (get-text-property (point) 'syntax-type) 'pod)
	 (re-search-forward "[ \t]*\\(\\/\\|$\\)" lim t))
	(if (eq (preceding-char) ?\/) (progn (backward-char 1) t))
      ;; Else
      (while (not stop-in)
	(setq state (parse-partial-sexp (point) lim nil nil nil t))
					; stop at comment
	;; If fails (beginning-of-line inside sexp), then contains not-comment
	(if (nth 4 state)		; After `/';
	  (setq stop-in t)))		; Finish 
      (nth 4 state))))

;; Stolen from cperl-mode and changed to use '/' char instead of '#'
(defun q-fill-paragraph (&optional justify iteration)
  "Like \\[fill-paragraph], but handle q comments.
If any of the current line is a comment, fill the comment or the
block of it that point is in, preserving the comment's initial
indentation and initial slashes.  Behaves usually outside of comment."
  (interactive "P")
  (let (;; Non-nil if the current line contains a comment.
	has-comment
	;; If has-comment, the appropriate fill-prefix for the comment.
	comment-fill-prefix
	;; Line that contains code and comment (or nil)
	start
	c spaces len dc (comment-column comment-column))
    ;; Figure out what kind of comment we are looking at.
    (save-excursion
      (beginning-of-line)
      (cond
       ;; A line with nothing but a comment on it?
       ((looking-at "[ \t]*\\/[\\/ \t]*")
	(setq has-comment t
	      comment-fill-prefix (buffer-substring (match-beginning 0)
						    (match-end 0))))
       ;; A line with some code, followed by a comment?  Remember that the
       ;; semi which starts the comment shouldn't be part of a string or
       ;; character.
       ((q-to-comment-or-eol)
	(setq has-comment t)
	(looking-at "\\/+[ \t]*")
	(setq start (point) c (current-column)
	      comment-fill-prefix
	      (concat (make-string (current-column) ?\ )
		      (buffer-substring (match-beginning 0) (match-end 0)))
	      spaces (progn (skip-chars-backward " \t")
			    (buffer-substring (point) start))
	      dc (- c (current-column)) len (- start (point))
	      start (point-marker))
	(delete-char len)
	(insert (make-string dc ?-)))))
    (if (not has-comment)
	(fill-paragraph justify)       ; Do the usual thing outside of comment
      ;; Narrow to include only the comment, and then fill the region.
      (save-restriction
	(narrow-to-region
	 ;; Find the first line we should include in the region to fill.
	 (if start (progn (beginning-of-line) (point))
	   (save-excursion
	     (while (and (zerop (forward-line -1))
			 (looking-at "^[ \t]*\\/+[ \t]*[^ \t\n\\/]")))
	     ;; We may have gone to far.  Go forward again.
	     (or (looking-at "^[ \t]*\\/+[ \t]*[^ \t\n\\/]")
		 (forward-line 1))
	     (point)))
	 ;; Find the beginning of the first line past the region to fill.
	 (save-excursion
	   (while (progn (forward-line 1)
			 (looking-at "^[ \t]*\\/+[ \t]*[^ \t\n\\/]")))
	   (point)))
	;; Remove existing slashes
	(goto-char (point-min))
	(while (progn (forward-line 1) (< (point) (point-max)))
	  (skip-chars-forward " \t")
	  (and (looking-at "\\/+")
	       (delete-char (- (match-end 0) (match-beginning 0)))))
	;; Lines with only hashes on them can be paragraph boundaries.
	(let ((paragraph-start (concat paragraph-start "\\|^[ \t\\/*$"))
	      (paragraph-separate (concat paragraph-start "\\|^[ \t\\/]*$"))
	      (fill-prefix comment-fill-prefix))
	  (fill-paragraph justify)))
      (if (and start)
	  (progn
	    (goto-char start)
	    (if (> dc 0)
		(progn (delete-char dc) (insert spaces)))
	    (if (or (= (current-column) c) iteration) nil
	      (setq comment-column c)
	      (indent-for-comment)
	      ;; Repeat once more, flagging as iteration
	      (q-fill-paragraph justify t))))))
  t)

; modes
(defvar inferior-q-mode-hook nil)
(defvar q-mode-hook nil)

(defun inferior-q-mode ()
  "Major mode for editing q Language files"
  (interactive)
  (make-local-variable `comint-output-filter-functions)
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
  (comint-mode)
  (setq comint-prompt-regexp "^\\(q)+\\|[^:]*:[0-9]+>\\)")
  (setq major-mode 'inferior-q-mode)
  (setq mode-name "Q-Shell")
  (use-local-map inferior-q-mode-map)
  (q-font-mode)
  (font-lock-mode t)
  (make-local-variable 'comint-process-echoes)
  (setq comint-process-echoes (not (equal q-server "")))
  (unless inferior-q-menubar-menu
    (easy-menu-define inferior-q-menubar-menu nil ""
		      inferior-q-menubar-menu-1))
  (easy-menu-add inferior-q-menubar-menu)
  (run-hooks 'inferior-q-mode-hook)
  (local-set-key (kbd "<return>") (lambda () "" (interactive) (comint-send-input) (comint-send-eof))))

(defun q-mode ()
  "Major mode for editing q Language files"
  (interactive)
  (kill-all-local-variables)
  (q-font-mode)
  (use-local-map q-mode-map)
  (set (make-local-variable 'comment-column) 60)
  (set (make-local-variable 'comment-start) "/")
  (set (make-local-variable 'comment-start-skip) "/[ \t]*")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-multi-line) nil)
  ;; Keep  out of the code.
  (set (make-local-variable 'indent-tabs-mode) nil)
; (set (make-local-variable 'indent-line-function) 'q-indent-line)  
  (set (make-local-variable 'fill-paragraph-function) 'q-fill-paragraph)
  (setq major-mode 'q-mode)
  (setq mode-name "Q-Script")
  (unless q-menubar-menu
    (easy-menu-define q-menubar-menu nil ""
		      q-menubar-menu-1))
  (easy-menu-add q-menubar-menu)
  (run-hooks 'q-mode-hook))

(add-to-list 'auto-mode-alist '("\\.[qk]\\'" . q-mode))

(provide 'q-mode)
