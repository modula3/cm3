;;; Last modified on Fri Oct  7 10:55:23 PDT 1994 by detlefs
;;;      modified on Fri May 15 17:13:12 PDT 1992 by heydon

;;;      modified on Thu Apr 23 17:45:03 PDT 1992 by muller
;;;      modified on Fri Feb  2 13:04:24 1990 by discolo
;;;      modified on Tue May  2 21:59:35 1989 by ellis
;;;      modified                             by Trevor Morris
;;;      modified                             by Tom Perrine
;;;      modified                             by Michael Schmidt
;;;      modified                             by Peter Robinson
;;;      modified                             by mjordan

;; LCD Archive Entry:
;; modula3|Eric Muller|muller@src.dec.com|
;; Modula-3 mode.|
;; 92-04-17||~/modes/modula3.el.Z|

(require 'cl)

(provide 'modula3)

;;; ---------- Syntax Table and Keymap (Added by TEP) ----------

(defvar m3::mode-abbrev-table nil
  "Abbrev table in use in C++-mode buffers.")
(define-abbrev-table 'm3::mode-abbrev-table ())

(defvar m3::mode-syntax-table nil
  "Syntax table in use in Modula 3 mode buffers.")

(if m3::mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\( "()1" table)
    (modify-syntax-entry ?\) ")(4" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} ")}" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?% "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq m3::mode-syntax-table table)))

(defvar m3::mode-map nil
  "Keymap used in Modula 3 mode.")

(defvar m3::screen-pool nil
  "Pool of sceens for m3")

(defvar m3::mouse-map nil
  "mouse map for m3-mode.")

(defun m3::setup-mode-map ()
  "Sets up Modula 3 mode map; this must be called after the sequence for the
keypad key \"?\\C-@\" has been setup - it uses \"function-key-sequence\" on
that key in order to bind the Modula 3 specific functions"
  (if (not m3::mode-map)
      (progn
	(setq m3::mode-map (make-sparse-keymap))
	(define-key m3::mode-map "\t" 'm3::abbrev-and-or-indent)
	(define-key m3::mode-map "\M-\t" 'm3::ident-complete)
	(define-key m3::mode-map "\C-ca" 'm3::toggle-abbrev)

	(define-key m3::mode-map "\C-ci" 'm3::show-interface)
	(define-key m3::mode-map "\C-cm" 'm3::show-implementation)
	(define-key m3::mode-map "\C-cs" 'm3::show-spec)
	(define-key m3::mode-map "\C-ct" 'm3::show-trait)

	(define-key m3::mode-map "\C-cb" 'm3::pp-buffer)
	(define-key m3::mode-map "\C-cu" 'm3::pp-unit)
	(define-key m3::mode-map "\C-cr" 'm3::pp-region)

	(define-key m3::mode-map "\M-\C-a" 'm3::beginning-of-defun)
	(define-key m3::mode-map "\M-\C-e" 'm3::end-of-defun)
	(define-key m3::mode-map "\C-c\C-f" 'm3::forward-sexp)
	(define-key m3::mode-map "\C-c\C-b" 'm3::backward-sexp)
	))
  )


;;; --------------- Constant and global variable declarations --------------

(defconst m3::identifier-char-re "[a-zA-Z0-9_]")
(defconst m3::alpha-char-re "[a-zA-Z_]")
(defconst m3::not-identifier-char-re "[^a-zA-Z0-9_]")

(defconst m3::identifier-re
  (concat "\\b" m3::alpha-char-re m3::identifier-char-re "*\\b"))

(defconst m3::intlit-re "\\(0\\|[1-9][0-9]*\\)")

(defconst m3::poss-qual-ident-re
  (concat "\\(" "\\(" m3::identifier-re "\\.\\)?" m3::identifier-re "\\.\\)?"
	  m3::identifier-re))

(defconst m3::com-start-re "\\((\\*\\|<\\*\\)")
(defconst m3::com-end-re "\\(\\*)\\|\\*>\\)")
(defconst m3::com-start-or-end-re
  (concat "\\\(" m3::com-start-re "\\|" m3::com-end-re "\\)"))

(defconst m3::whitespace-char-re "[ \t]")
(defconst m3::whitespace-re "[ \t]+")
(defconst m3::poss-whitespace-re "[ \t]*")
(defconst m3::poss-whitespace-nl-re "[ \t\n]*")
(defconst m3::whitespace-line-re "^[ \t]*$")


(defconst m3::char-lit-re "'\\([^\\]\\|\\\\..?.?\\)'")

(defconst m3::range-end-re
  (concat "\\(" m3::poss-qual-ident-re "\\|" m3::intlit-re "\\|"
	  m3::char-lit-re "\\)"))

(defconst m3::range-re
  (concat m3::range-end-re m3::poss-whitespace-re "\\.\\."
	  m3::poss-whitespace-re m3::range-end-re))
  
  
(defconst m3::case-label-re
  (concat "\\(" m3::poss-qual-ident-re "\\|"
	  m3::char-lit-re "\\|"
	  m3::intlit-re "\\|"
	  m3::range-re
	  "\\)"))

(defconst m3::handler-start-re
  (concat "\\(|[ \t]*\\)?\\("
	  (concat "\\b" m3::poss-qual-ident-re m3::poss-whitespace-re
		  "(" m3::poss-whitespace-re m3::identifier-re
		  m3::poss-whitespace-re ")" )
	  "\\|"
	  (concat "REF" m3::whitespace-re ".*"
		  "(" m3::poss-whitespace-re m3::identifier-re
		  m3::poss-whitespace-re ")" )
	  "\\|"
	  (concat m3::case-label-re
		  (concat "\\(" m3::poss-whitespace-re ","
			  m3::poss-whitespace-nl-re m3::case-label-re "\\)*"))
	  
	  "\\)" m3::poss-whitespace-re "=>"))


(defconst m3::object-re
  (concat "\\(" m3::identifier-re "[ \t]+\\)?\\(BRANDED[ \t]+"
	  "\\(\"[^\"]+\"\\)?[ \t]+\\)?OBJECT"))


(defconst m3::part-starters
  (concat
   "\\bINTERFACE\\b\\|\\bMODULE\\b\\|\\bIMPORT\\b\\|\\bFROM\\b\\|"
   "\\bTYPE\\b\\|\\bEXCEPTION\\b\\|\\bVAR\\b\\|"
   "\\bPROCEDURE\\b\\|\\bREVEAL\\b\\|\\bCONST\\b\\|\\bBEGIN\\b")
  "These are the patterns that can start lines and change the indentation
of the following line.")


(defconst m3::keyword-endable-ssl-introducers
  (concat
   "\\bTYPE\\b\\|\\bVAR\\b\\|"
   "\\bRECORD\\b\\|\\bOBJECT\\b\\|\\bMETHODS\\b\\|\\bOVERRIDES\\b\\|"
   "\\bBEGIN\\b\\|\\bTRY\\b\\|\\bEXCEPT\\b\\|"
   m3::handler-start-re "\\|"
   "\\bFINALLY\\b\\|\\bLOOP\\b\\|\\bTHEN\\b\\|\\bELSE\\b\\|\\bREPEAT\\b\\|"
   "\\bDO\\b\\|\\bOF\\b\\|\\bREVEAL\\b\\|\\bCONST\\b"))

(defconst m3::statement-list-starter
  (concat
   "\\bBEGIN\\b\\|\\bTRY\\b\\|\\bEXCEPT\\b\\|"
   m3::handler-start-re "\\|"
   "\\bFINALLY\\b\\|\\bLOOP\\b\\|\\bTHEN\\b\\|\\bELSE\\b\\|\\bREPEAT\\b\\|"
   "\\bDO\\b"))

;;; These keywords have the property that they affect the indentation if they
;;; occur at the beginning of a line.
(defconst m3::keyword-line-starters
  (concat
   "\\bTYPE\\b\\|\\bPROCEDURE\\b\\|\\bEXCEPTION\\b\\|"
   "\\bVAR\\b\\|\\bBEGIN\\b\\|\\bTRY\\b\\|\\bEXCEPT\\b\\|"
   m3::handler-start-re "\\|"
   "|\\|\\bFINALLY\\b\\|\\bLOOP\\b\\|\\bTHEN\\b\\|\\bELSIF\\b\\|"
   "\\bIF\\|ELSE\\|WHILE\\|REPEAT\\|"
   "WITH\\|FOR\\b\\|DO\\|CASE\\|\\bOF\\b\\|TYPECASE\\|LOCK\\|CONST\\|FROM\\|"
   "REVEAL\\|METHODS\\|OVERRIDES"))


(defconst m3::multi-keyword-line-prefix
  (concat
   "\\("
   ;; ...a PROCEDURE at the start of a line that ends
   ;; with an equals
   "^PROCEDURE[^\n]*=" "\\|"
   ;; ... or an IF or ELSEIF that ends with a THEN
   "\\(IF\\|ELSIF\\)[^\n]*THEN" "\\|"
   ;; ... or a WHILE, WITH, FOR, or LOCK that ends with a DO
   "\\(WHILE\\|WITH\\|FOR\\b\\|LOCK\\)[^\n]*DO" "\\|"
   ;; ... or a FOR that ends with a TO or BY
   "FOR[^\n]*\\(DO\\|BY\\)" "\\|"		  
   ;; ... or a CASE or TYPECASE that ends with a OF
   "\\(CASE\\|TYPECASE\\)[^\n]*OF" "\\|"
   ;; ... or at a handler-start that ends with a "=>"
   "\\(|\\|\\)[ \t]*" m3::handler-start-re
   "\\)"
   ))

(defconst m3::multi-keyword-lines
  (concat m3::multi-keyword-line-prefix 
	  "[ \t]*\\($\\|(\\*\\)"))


(defconst m3::statement-starters
  (concat
   "BEGIN\\b\\|TRY\\b\\|LOOP\\b\\|IF\\b\\|WHILE\\b\\|REPEAT\\b\\|"
   "WITH\\b\\|FOR\\b\\|CASE\\b\\|TYPECASE\\b\\|LOCK\\b")
  "These are the patterns that can start lines and change the indentation
of the following line.")



(defconst m3::keyword-ssl-enders
  (concat "\\(|\\|\\bEXCEPT\\b\\|\\bFINALLY\\b\\|\\bELSIF\\b\\|"
	  "\\bELSE\\b\\|\\bUNTIL\\b\\|\\bEND\\b\\)"))

(defconst m3::left-parens
  "\\((\\|\\[\\|{\\)")
(defconst m3::right-parens
  "\\()\\|\\]\\|}\\)")

(defconst m3::statement-keywords
  "RETURN\\|RAISE\\|EXCEPTION\\|IMPORT\\|WITH")

(defconst m3::end-matchers
  (concat
   "\\bRECORD\\b\\|\\bOBJECT\\b\\|\\bBEGIN\\b\\|\\bTRY\\b\\|\\bLOOP\\b\\|"
   "\\bIF\\b\\|\\bWHILE\\b\\|\\bWITH\\b\\|\\bFOR\\b\\|\\bCASE\\b\\|"
   "\\bTYPECASE\\b\\|\\bLOCK\\b\\|\\bINTERFACE\\b\\|\\bMODULE\\b\\|"
   "\\bGENERIC\\b"))


(defconst m3::same-line-ssl-keywords
  "\\bVAR\\b\\|\\bTYPE\\b\\|\\bCONST\\b\\|\\bEXCEPTION\\b\\|\\bREVEAL\\b"
  "These are the keywords that can be followed by an SSL that begins on
the same line -- if so, indent to the level of the first elem.")

(defconst m3::case-starters
  "TRY\\|CASE\\|TYPECASE")

;;; ------ Variables that control indentation behavior ------------

(defvar m3::standard-offset 2)
(defvar m3::continued-line-offset 2)
(defvar m3::case-offset 0)
(defvar m3::open-paren-offset 4)
(defvar m3::open-paren-sep 0)
(defvar m3::proc-param-from-proc-keyword t)
(defvar m3::assign-offset 4)
(defvar m3::RAISES-offset 4)

(defvar m3::follow-continued-indent t)

(defvar m3::END-undent 2)
(defvar m3::METHODS-undent 2)
(defvar m3::OVERRIDES-undent 2)
(defvar m3::EXCEPT-undent 2)
(defvar m3::VERT-undent 2)
(defvar m3::handler-start-undent 0)
(defvar m3::EXCEPT-undent 2)
(defvar m3::UNTIL-undent 2)
(defvar m3::FINALLY-undent 2)
(defvar m3::ELSIF-undent 2)
(defvar m3::ELSE-undent 2)

(defvar m3::DO-undent 1)
(defvar m3::OF-undent 1)
(defvar m3::THEN-undent 1)

(defvar m3::OBJECT-undent 1)
(defvar m3::RECORD-undent 1)

;;;  --------  Variables controlling keyword-completion and end-matching.

(defvar m3::abbrev-enabled t
  "*Non-nil indicates TAB should complete keywords.")

(defvar m3::electric-end 'proc-mod
  "*If the value of this variable is 'proc-mod or 'all, a TAB that
completes an END, or occurs after a complete END, may cause some text to
be inserted after the END.  If the value is 'proc-mod and the END
completes the main block of a procedure or a module or interface,
fills in the name of the procedure, module, or interface, and a
semi-colon or period as appropriate.  If the value is 'all, works as
for 'proc-mod, but also adds a comment containing the keyword starting
the construct completed by other END's.  If the value is nil, no text
is added.")


(defvar m3::blink-end-matchers t
  "*Non-nil causes a TAB that completes an END or occurs after a
completed END to momentarily move the cursor to the beginning of the
keyword that starts the construct completed by the END.")

;;; ----------------------------------------------------------------------
;;; NOT USER SETTABLE.
;;; These variables enable us to "cycle through" a list of possible
;;; completions when the prefix the user typed was ambiguous.  If the
;;; point is at (m3::cur-keyword-completion-start +
;;; m3::cur-keyword-completion-len nil) and
;;; m3::cur-keyword-completions is non-nil, we substitute the first
;;; element of the list for the current completion, and rotate the list.

(defvar m3::cur-keyword-completion-start (make-marker)
  "A marker indicating the start of the last word that was keyword-completed.")

(defvar m3::cur-keyword-completion-len nil
  "The length of the completed keyword at the time of completion, to allow
us to determine if the user has entered more text.")

(defvar m3::cur-keyword-completions nil
  "A list of the strings that matched the originally input keyword text.")


;;; ------ THE MAIN ROUTINE - SETS UP MODULA-3 MODE --------------

  
(defun modula-3-mode ()
  "This is a mode intended to support program development in Modula 3.

You can avoid tedious entry of constructs involving long uppercase
keywords by using 'abbrev-mode.'  When m3::abbrev-enabled is non-nil,
TAB typed at the end of a word completes just that current word as a
keyword.  This mode analyzes the context to restrict the choices
admitted by partial prefixes to as small a set as possible.  If more
than 1 choice remain after this winnowing, they are ordered according
to their popularity (assigned in an ad hoc manner by me, dld, and
easily changed), and the first completion is performed, with a message
that other completions are possible.  If the choice is wrong, hitting
TAB immediately will cycle through the other choices.

There are two independent mechanism for indenting/prettyprinting
text.  The main addition that I (dld) have made is adding the style of
'electric' indentation normally associated with gnuemacs language
modes.  Basically, all you need to know is that TAB, in addition to
completing keywords, also indents the current line properly.

The other mechanism uses a pretty printer (m3pp) that runs as a
separate process.  The command m3pp-region and m3pp-unit, and the
variable m3pp-options are used to apply m3pp to a portion of the
buffer.

Another new feature is END-matching and completion.  Various non-nil
values of the variable 'm3::electric-end' cause hitting TAB on a line
containing just an END to do things like fill in the name of the
procedure, module, or interface, or the keyword that starts the
construct that the END completes.  Another, independent, variable,
'm3::blink-end-matchers', temporarily blinks the curser at the
beginning of the construct that the END matches.

Another convenient feature is that beginning-of-defun, end-of-defun,
forward-sexp, and backward-sexp have been given appropriate Modula-3
definitions, and these functions have been bound to the standard keys.

The following list gives the key bindings.
\\{m3::mode-map}"

  (interactive)
  (kill-all-local-variables)
  (m3::setup-mode-map)
  (use-local-map m3::mode-map)
  (setq major-mode 'modula-3-mode)
  (setq mode-name "Modula 3")
  (setq local-abbrev-table m3::mode-abbrev-table)
  (set-syntax-table m3::mode-syntax-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'm3::indent-line)
  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)
  (make-local-variable 'comment-start)
  (setq comment-start "(* ")
  (make-local-variable 'comment-end)
  (setq comment-end " *)")
  (make-local-variable 'comment-column)
  (setq comment-column 41)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+[ \t]*")
  (make-local-variable 'comment-multi-line)
  (setq comment-multi-line t)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'c-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  (run-hooks 'm3::mode-hook))


;;; -------------------- Electric indentation --------------------


(defun m3::indent-line ()
  "Indent the current-line."
  (interactive)
  (m3::indent-line-work t))

(defun m3::indent-line-work (electric)
  ;; If in unterminated string, give an error.  If in comment and
  ;; electric, indent like previous line.
;;;  (message "indent-line-work") (sit-for 2)
  (let ((string-comment-state (m3::in-comment-or-string)))
;;;    (message "string-comment-state = %s" string-comment-state) (sit-for 2)
    (cond
     ((eq string-comment-state 'string)
      (beep)
      (message "Unterminated Text literal..."))
     ((eq string-comment-state 'comment)
      (if electric
	  (let ((cur-point (point)))
	    (beginning-of-line)
	    (m3::skip-whitespace-in-line)
	    (cond
	     ;; If the current line begines with a close comment,
	     ;; indent it to the level of the matching start comment.
	     ((save-excursion
		(beginning-of-line)
		(m3::skip-whitespace-in-line)
		(looking-at "*)"))
	      (m3::indent-to
	       cur-point
	       (save-excursion
		 (beginning-of-line)
		 (m3::skip-whitespace-in-line)
		 (forward-char 2)
		 (m3::skip-comment-backward (point-min) t)
		 (current-column))))

	     ;;; If the current line begins with an open-comment, and
	     ;;; the opened comment is not nested, indent like a code line.
	     ((save-excursion
		(beginning-of-line)
		(m3::skip-whitespace-in-line)
		(and (looking-at "(*")
		     (not (m3::in-comment-or-string))))
	      (m3::indent-to cur-point (m3::indent-for-line)))

	     ;;; Otherwise, indent to same level as previous
	     ;;; non-whitespace line.
	     (t
	      (m3::indent-to
	       cur-point
	       (save-excursion
		 (forward-line -1)
		 (while (looking-at m3::whitespace-line-re)
		   (forward-line -1))
		 (m3::skip-whitespace-in-line)
		 (if (looking-at "(\\*")
		     (progn (forward-char 2)
			    (m3::skip-whitespace-in-line)))
		 (current-column))))))))

     ;; We're not in a comment or a string.  Indent the current line.
     (t
      (m3::indent-to (point) (m3::indent-for-line))
      ;; Do the appropriate thing for electric end's.
      (m3::do-electric-end)))))


(defun m3::indent-for-line ()
  (save-excursion
    (beginning-of-line)
    (let* ((cur-point (point))
	   (part-start (save-excursion
			 (m3::backward-to-last-part-begin)
			 (point)))
	   (first-code
	    (save-excursion
	      (re-search-forward "[ \t]*"
				 (save-excursion (end-of-line) (point))
				 t)
	      (goto-char (match-end 0))
;;;	      (message "first-code 2") (sit-for 2)
	      (point)))
	   (need-keyword
	    (or
	     (save-excursion
	       (goto-char first-code)
	       (looking-at m3::keyword-ssl-enders))
	     (and
	      (save-excursion
		(goto-char part-start)
		(looking-at m3::same-line-ssl-keywords))
	      (save-excursion
		(goto-char first-code)
		(looking-at m3::part-starters)))))
	   (after-keyword nil)		; non-nil indicates that the line
					; used for indentation was not
					; complete, but rather was
					; chose because it started
					; with a keyword.
	   ;; Must do this because Modula is case-sensitive
	   (case-fold-search nil))

      ;; Now figure out if there is an intervening "incomplete
      ;; line" between here and the original line:
      ;; (A complete statement forms a complete line.  Also, a line
      ;; ending in an ssl-introducer forms a complete line.  All other
      ;; lines are incomplete...)
      (let ((prev-statement-start (point)))
;;;	(message "Checking completeness") (sit-for 2)
	(cond
	 ;; Is it incomplete?
	 ((m3::prev-line-incomplete-p cur-point first-code part-start)

	  ;; ...OK, the previous line *was* incomplete.
	  (goto-char cur-point)
;;;	  (message "m3::indent-for-line: incomplete") (sit-for 2)
	  (m3::incomplete-indent cur-point first-code part-start))

	 (t
	  ;; Find beginning of statement upon which to base the
	  ;; indentation of the current line.
;;;	    (message "m3::complete-line-start before") (sit-for 2)
	  (setq after-keyword 
		(m3::backward-to-complete-line-start
		 part-start first-code (if need-keyword 'need)))
;;;	    (message "m3::complete-line-start after") (sit-for 2)

;;;	  (message "m3::indent-for-line: indent, ak = %s" after-keyword)
;;;	  (sit-for 2)
	  (cond
	   (after-keyword
	    (m3::after-keyword-adjust-indent (current-column)
					     first-code part-start))
	   (t
	    (m3::complete-adjust-indent (current-column) first-code
					part-start)))))))))

(defun m3::backward-to-complete-line-start
  (part-start first-code ssl-state)
  "Find beginning of statement upon which to base the indentation of
the line whose first non-blank character is at FIRST-CODE.  If
SSL-STATE is 'need, the keyword at first-code is an ssl-ender,
so we want to find the line that ended with the ssl-introducer that
the ssl-ender ended (whew.)  If Returns t iff the line found starts
with a keyword-statement-starter."

  ;; We must find the first previous line upon which we can base
  ;; the indentation.  Here is the algorithm:
  
  ;; We have set first-code to the character position of the first
  ;;   character in the line to be indented.
  ;;   Search backward for the first end-of-statement or
  ;;     keyword-line-starter.  (An end-of-statement is a
  ;;     semicolon or END; a keyword-line-starter is a keyword
  ;;     that changes the indentation when it starts a line.)
  ;;   end-of-statement was found =>
  ;;     Is there any code between the end-of-statement and
  ;;     first-code?
  ;;     Yes =>
  ;;       Go to the first character of that code and leave the
  ;;       point there.
  ;;     No ==>
  ;;       We don't know how far back this statement begins.
  ;;       If the statement-ender is an END =>
  ;;         Find the end-matcher.
  ;;         If that is a line starter =>
  ;;           Leave the point there.
  ;;         Otherwise =>
  ;;           Recursively go backward-to-complete-line-start.
  ;;       else =>
  ;;         Search backward again for the first end-of-statement,
  ;;         ssl-introducer, or keyword-line-starter.
  ;;         end-of-statement, ssl-introducer found =>
  ;;           forward-to-code; leave point there.
  ;;         keyword-line-starter =>
  ;;           Leave point at the start of the keyword.
  ;;   keyword-line-starter was found =>
  ;;     Leave point at the start of the keyword.

  (let ((after-keyword nil) (in-end-match nil))
;;;    (message "m3::cl-start(A) ps = %d, need = %s" part-start ssl-state)
;;;    (sit-for 2)
    (m3::backward-to-ender-starter-or-ssl-intro part-start)

;;;    (message "m3::indent-for-line(A10)") (sit-for 2)

    (when (looking-at m3::handler-start-re)
      (m3::backward-to-handler-start))

;;;    (message "m3::indent-for-line(A11)") (sit-for 2)


    (cond
     ((looking-at "\\(;\\|\\bEND\\b\\|\\bUNTIL\\b\\)")
      ;;   end-of-statement was found =>
;;;      (message "m3::indent-for-line(A-x1)") (sit-for 2)
      (cond
       ((and (eq ssl-state 'need) (not (looking-at "\\bEND\\b\\|\\bUNTIL\\b")))
;;;	(message "m3::indent-for-line(A-x1-0)") (sit-for 2)
	(setq after-keyword
	      (m3::backward-to-complete-line-start
	       part-start first-code ssl-state)))
       (t
	(let ((ender-start (point)))
;;;	  (message "m3::indent-for-line(A-x2-0)") (sit-for 2)
	  (m3::end-of-ender first-code)
;;;	  (message "m3::indent-for-line(A-x2-1)") (sit-for 2)
	  ;;     Is there any code between the end-of-statement and
	  ;;     first-code?
	  (m3::forward-to-code (point-max))
;;;	  (message "m3::indent-for-line(A-x2)") (sit-for 2)
	  (cond
	   ((< (point) first-code)
	    ;;     Yes =>
	    ;;       Go to the first character of that code and leave the
	    ;;       point there.
;;;	    (message "m3::indent-for-line(A-x2b)") (sit-for 2)
	    (setq after-keyword
		  (looking-at (concat "\\(" m3::keyword-line-starters
				      "\\)"))))
	   (t
;;;	    (message "m3::indent-for-line(A-x2a)") (sit-for 2)
	    ;;     No ==>
	    ;;       We don't know how far back this statement begins.
	    (goto-char ender-start)
;;;	    (message "m3::indent-for-line(A-x2a0)") (sit-for 2)
	    (cond
	     ;;       If the statement ender is an END =>
	     ;;         Find the end-matcher and see if it is a
	     ;;         keyword-line-starter.  If not, search again...
	     ((looking-at "\\(\\bEND\\b\\|\\bUNTIL\\b\\)")
	      (cond
	       ((looking-at "\\bEND\\b")
		(m3::backward-to-end-match part-start))
	       ((looking-at "UNTIL")
		(m3::backward-to-until-match part-start)))
	      (setq in-end-match t)
;;;	      (message "m3::complete-line-start END-match 0") (sit-for 2)
	      (let ((begin-start (point)) (not-begin nil))
		(when (looking-at "BEGIN")
		    ;; If this begin is the main body of a procedure or
		    ;; module, skip back further to the PROCEDURE or
		    ;; MODULE keywords.
		    (m3::backward-to-BEGIN-owner)
		    (if (not (= (point) begin-start)) (setq not-begin t)))
;;;		(message "m3::complete-line-start END-match") (sit-for 2)
		(if (and
		     (looking-at (concat "\\("
					 m3::keyword-line-starters
					 "\\)"))
		     (not (eq ssl-state 'need)))
		    (progn
		      (setq after-keyword not-begin)
;;;		      (message "&&&&") (sit-for 2)
		      )
		  (setq after-keyword
			(m3::backward-to-complete-line-start
			 part-start (point) ssl-state))
;;;		  (message "m3::cl-start END-match recurse returns %s"
;;;			   after-keyword)
;;;		  (sit-for 2)
		  )))

	   ;;       else =>
	     (t
	      ;;         Search backward again for the first end-of-statement,
	      ;;         ssl-introducer, or keyword-line-starter.
	      (setq after-keyword
		    (m3::backward-to-complete-line-start
		     part-start first-code ssl-state))))))))))
	      

     ;;   ssl-introducer was found =>
     ((looking-at
       (concat "\\(" m3::keyword-endable-ssl-introducers "\\)"))
      (let ((ssl-intro-start (point)))
	(cond
	 ((progn
;;;	    (message "m3::c-l-start X -- 1a") (sit-for 2)
	    (re-search-forward
	     (concat "\\(" m3::keyword-endable-ssl-introducers "\\)")
	     first-code)
	    (goto-char (match-end 0))
;;;	    (message "m3::c-l-start X -- 1a2") (sit-for 2)
	    (m3::forward-to-code (point-max))
	    (< (point) first-code))
	  (cond
	   ((eq ssl-state 'need)
;;;	    (message "m3::c-l-start X -- 1b") (sit-for 2)
	    (goto-char ssl-intro-start)
	    (setq after-keyword
		  (save-excursion
		    (goto-char first-code)
		    (not (looking-at m3::part-starters))))
			       
	    (when (not (looking-at
			(concat "\\(" m3::keyword-line-starters "\\)")))
	      (m3::backward-to-complete-line-start
	       part-start (point) nil)))
	   (t
;;;	    (message "m3::c-l-start X -- 1b2") (sit-for 2)
	    (setq after-keyword
		  (looking-at (concat "\\(" m3::keyword-line-starters
				      "\\)"))))))

	 ((progn
	    (goto-char ssl-intro-start)
	    (not (looking-at (concat "\\(" m3::keyword-line-starters "\\)"))))
;;;	  (message "m3::c-l-start Y 1") (sit-for 2)
	  (setq after-keyword t) ;; The ssl-introducer suffices...
	  (m3::backward-to-complete-line-start
	   part-start (point) nil))
	  
	  ;;; must be a keyword-line-starter
	 (t
;;;	  (message "m3::c-l-start Z") (sit-for 2)
	  (setq after-keyword t)))))
      
     ;;   keyword-line-starter was found, or at part-start =>
     ;;     Leave point at the start of the keyword.
     (t
;;;      (message "after keyword.") (sit-for 2)
      (setq after-keyword
	    (looking-at (concat "\\(" m3::keyword-line-starters "\\)")))))

    ;; One final little thing to do:
    ;;  Regular expression search backward matches the shortest match.
    ;;  For the handler-start-re, we can't make sure we got the whole thing,
    ;;  because of the poss-qual-id-re.  So now extend it if necessary.
    (when (looking-at m3::handler-start-re)
      (m3::backward-to-handler-start))

    ;; Now: we should be at the first code of the line, or else we
    ;; have to look again...
    (when (> (save-excursion (m3::backward-to-code part-start) (point))
	     (save-excursion (beginning-of-line 1) (point)))
;;;      (message "not first-code, in-end-match = %s" in-end-match) (sit-for 2)
      ;; If we're currently looking at an ssl-introducer, that cancels the
      ;; need for one...
      (when (and (eq ssl-state 'need)
		 (looking-at (concat "\\(" m3::keyword-endable-ssl-introducers
				     "\\)")))
	(setq ssl-state nil))
      (setq after-keyword
	    (m3::backward-to-complete-line-start part-start (point) ssl-state))
      (if (and in-end-match (not (eq ssl-state 'need)))
	  (setq after-keyword nil)))

;;;    (message "returning after-keyword = %s" after-keyword) (sit-for 2)
    after-keyword))

(defun m3::backward-to-handler-start ()
  ;; Assumes we are looking-at a handler-start; Ensures that point is
  ;; at the start of that handler.
  (let ((new-point (point)))
    ;; First of all, we might not be at the start of the id...
    (while (save-excursion
	     (forward-char -1)
	     (looking-at m3::identifier-char-re))
      (forward-char -1))
    (setq new-point (point))
      
    (save-excursion
      (forward-char -1)
      (cond
       ((looking-at "\\b.")
;;;	(message "m3::backward-to-handler-start A") (sit-for 2)
	(forward-word -1)
	(when (looking-at m3::handler-start-re)
	  (m3::backward-to-handler-start)
	  (setq new-point (point))))

       ((looking-at "[ \t,|]")
;;;	(message "m3::backward-to-handler-start B") (sit-for 2)
	(let ((last-point (point)))
	  (when (and (re-search-backward "[|,][ \t]*" (point-min) t)
		     (equal (match-end 0) last-point))
	    (cond
	     ((looking-at "|")
	      (setq new-point (match-beginning 0)))
	     ((looking-at ",")
	      (forward-word -1)
	      (m3::backward-to-handler-start)
	      (setq new-point (point)))))))))
	    
    (goto-char new-point)))
	      

	  
(defun m3::backward-to-ender-starter-or-ssl-intro (min-point)
  "Moves backwards to the beginning of the first statement ender, that is
semi-colon or END or UNTIL, or starter (m3::keyword-line-starters) or
ssl-introducer, or, if none are found before min-point, to min-point."
;;;  (message "m3::backward-to...0")
  (m3::re-search-backward
   (concat "\\(;\\|\\bEND\\b\\|\\bUNTIL\\b\\|"
	   "^[ \t]*\\(" m3::keyword-line-starters "\\)\\|"
	   m3::keyword-endable-ssl-introducers "\\)")
   min-point 'move-to-point)
;;;    (message "m3::backward-to...0.1") (sit-for 1)
  (while (m3::in-arg-list min-point)
;;;    (message "m3::backward-to...0.5") (sit-for 1)
;;;    (message "m3::backward-to...0.51") (sit-for 1)
    (m3::re-search-backward
     (concat "\\(;\\|\\bEND\\b\\|\\bUNTIL\\b\\|"
	     "^[ \t]*\\(" m3::keyword-line-starters "\\)\\|"
	   m3::keyword-endable-ssl-introducers "\\)")
     min-point t))
;;;  (message "m3::backward-to...1") (sit-for 2)
  (cond
   ((looking-at ";")
    (let ((p (point)))
      (m3::backward-to-code min-point)
      (forward-word -1)
      (if (and (not (looking-at "\\bEND\\b"))
	       (progn (forward-word -1)
		      (not (looking-at "\\bEND\\b"))))
	  (goto-char p))))
   ((looking-at 
     (concat "^[ \t]*\\(" m3::keyword-line-starters "\\)"))
    ;; Must be a keyword-line-starter or ssl-introducer...
;;;    (message "m3::backward-to...2") (sit-for 2)
    (re-search-forward "[ \t]*" (point-max) t))))

(defun m3::end-of-ender (max-point)
  "Assumes point is looking-at END or UNTIL or semi-colon"
  (cond
   ((looking-at "\\bEND\\b")
    (forward-word 1)
    (let ((p (point)))
      (m3::forward-to-code max-point)
      (if (looking-at ";")
	  (forward-char 1)
	(forward-word 1)
	(if (looking-at "[;.]")
	    (forward-char 1)
	  (goto-char p)))))
   ((looking-at "UNTIL")
    (forward-word 1)
    (m3::re-search-forward "\\(;\\|\\bEND\\b\\|\\bUNTIL\\b\\|$\\)"
			   (point-max) 'move-to-limit)
    (cond
     ((looking-at ";") (forward-char 1))
     (t (m3::backward-to-code (point-min)))))
   (t ; semi-colon
    (forward-char 1))))

(defun m3::in-arg-list (part-start)
  "Returns non-NIL iff the point is in a procedure or method argument
list."
;;;  (message "m3::in-arg-list(1)") (sit-for 2)
  (save-excursion
    (let ((cur-point (point)))
      (m3::re-search-backward "PROCEDURE\\|METHODS" part-start t)
      (cond
       ((looking-at "PROCEDURE")
	(forward-word 1)
	(m3::re-search-forward "([^*]" (point-max) t)
;;;	(message "m3::in-arg-list(3)") (sit-for 2)
	(and (< (point) cur-point)
	     (condition-case err
		 (progn
		   (forward-sexp 1)
;;;		   (message "m3::in-arg-list(4)") (sit-for 2)
		   (> (point) cur-point))
	       (error t))))

       ((looking-at "METHODS")
	(let ((continue t) (res nil))
	  (while (and continue (< (point) cur-point))
	    (m3::re-search-forward "([^*]\\|\\bEND\\b" (point-max) t)
;;;	    (message "m3::in-arg-list(101)") (sit-for 2)
	    (cond
	     ((and (looking-at "([^*]") (< (point) cur-point))
;;;	      (message "m3::in-arg-list(101.5)") (sit-for 2)
	      (condition-case err
		  (progn
		    (forward-sexp 1)
;;;		    (message "m3::in-arg-list(102)") (sit-for 2)
		    (if (> (point) cur-point) (setq res t)))
		(error
		 ;; No matching right paren, so must still be in arg list.
;;;		 (message "m3::in-arg-list(103)") (sit-for 2)
		 (setq continue nil)
		 (setq res t))))
	     (t
;;;	      (message "m3::in-arg-list(104)") (sit-for 2)
	      (setq continue nil))))
	  res))

       (t nil)))))
	      

(defun m3::prev-line-incomplete-p (cur-point first-code part-start)
;;;  (message "incomplete?") (sit-for 2)
  (and
   ;; If the last word of the previous line is ";", "END", or an
   ;; ssl-introducer, the previous line is complete.
   (save-excursion
     (goto-char cur-point)
;;;     (message "incomplete: at-cur-point") (sit-for 2)
     (m3::backward-to-code part-start)
;;;     (message "incomplete: at prev-code") (sit-for 2)
     (not (or (and (eq (point) part-start) (looking-at "(*"))
	      (save-excursion
		(and (> (point) 1)
		     (progn (forward-char -1) (looking-at ";"))))
	      (progn (forward-word -1)
		     (looking-at
		      (concat "\\(\\bEND\\b\\|"
			      m3::keyword-endable-ssl-introducers
			      "\\)"))))))
			      
   (or
    ;; Does the previous non-blank line end with an operator?
    (save-excursion
;;;      (message "incomplete-1") (sit-for 2)
      (goto-char cur-point)
      (m3::backward-to-code part-start)
      (or (looking-at "[+\\-*&#<,(]")
	  (and (looking-at ">")
	       (save-excursion
		 (beginning-of-line)
;;;		(message "incomplete-1.1") (sit-for 2)
		 (not (looking-at
		       (concat "[ \t]*"
			       m3::handler-start-re
			       "[ \t]*\\($\\|(\\*\\)")))))
	  (and (looking-at "=")
	       (save-excursion
;;;		(message "incomplete-1.2") (sit-for 2)
		 (beginning-of-line)
;;;		(message "incomplete-1.21") (sit-for 2)
		 (and (not (looking-at
			    (concat "PROCEDURE.*=[ \t]*\\($\\|(\\*\\)")))
		      (not (m3::in-arg-list part-start)))))

	  (and (looking-at ";")
	       (m3::in-arg-list part-start))
		     
	  (and (> (point) 2)
	       (progn
		 (forward-char -2)
		 (or (looking-at
		      (concat m3::not-identifier-char-re "OR"))
		     (and
		      (> (point) 1)
		      (progn
			(forward-char -1)
			(looking-at
			 (concat m3::not-identifier-char-re
				 "\(DIV\\|MOD\\|AND\\|NOT")))))))))

    (save-excursion
      (goto-char cur-point)
      (m3::backward-to-code part-start)
      (and (> (point) part-start)
	   (progn
	     (forward-char 1)
;;;      (message "incomplete-1B1") (sit-for 2)
	     (let ((last-char (point)))
	       (beginning-of-line 1)
	       (and (re-search-forward
		     (concat "^[ \t]*\\(" m3::statement-keywords "\\)")
		     cur-point t)
		    (= last-char (match-end 0)))))))

    (save-excursion
;;;     (message "incomplete-2") (sit-for 2)
      (cond
       ((looking-at "\\bEND;\\b")
;;;       (message "incomplete-2.01") (sit-for 2)
	(forward-char 4))
       ((looking-at
	 (concat "\\bEND[ \t]*" m3::identifier-re "[ \t]*\\(;\\|\\.\\)"))
;;;       (message "incomplete-2.02") (sit-for 2)
	(re-search-forward
	 (concat "\\bEND[ \t]*" m3::identifier-re "[ \t]*\\(;\\|\\.\\)")
	 (point-max) t)
	(goto-char (match-end 0)))
       ((looking-at m3::multi-keyword-line-prefix)
;;;       (message "incomplete-2.1") (sit-for 2)
	(re-search-forward m3::multi-keyword-line-prefix (point-max) t)
	(goto-char (match-end 0)))

       ((looking-at "PROCEDURE")
;;;       (message "incomplete-2.15") (sit-for 2)
	(forward-word 1)
	(m3::re-search-forward "([^*]" (point-max) t)
	(let ((new-point (point)))
	  (save-excursion
	    (condition-case err
		(forward-sexp 1)
	      (error (goto-char (point-max))))
;;;	  (message "incomplete-2.15-2") (sit-for 2)
	    (and (< (point) cur-point)
		 (m3::re-search-forward "=" (point-max) t)
		 (progn
		   (forward-char 1)
		   (and (< (point) cur-point)
;;;		      (message "incomplete-2.15-3") (sit-for 2)
			(setq new-point (point))))))
	  (goto-char new-point)))

       ((looking-at "WITH")
;;;       (message "incomplete-2.191") (sit-for 2)
	(forward-word 1)
	(let ((new-point (point)))
	  (m3::re-search-forward "DO" first-code t)
;;;	 (message "incomplete-2.192") (sit-for 2)
	  (cond
	   ((looking-at "DO")
	    (forward-word 1)
;;;	   (message "incomplete-2.193") (sit-for 2)
	    (setq new-point (point))))
	  (goto-char new-point)))

       ((looking-at "\\bEND\\b")
	(forward-word 1)
	(cond
	 ((save-excursion
	    (m3::forward-to-code (point-max))
	    (looking-at ";"))
	  (m3::forward-to-code (point-max))
	  (forward-char 1))))

       ;; If looking-at keyword-line-starter or part-starter
       ((looking-at (concat m3::keyword-line-starters "\\|" m3::part-starters))
;;;	(message "incomplete-2.2") (sit-for 2)
	(re-search-forward
	 (concat m3::keyword-line-starters "\\|" m3::part-starters)
	 (point-max) t)
	(goto-char (match-end 0)))

       ((looking-at ";")
	(forward-char 1)))

      ;; Go forward to code.
;;;      (message "m3::IFL: before codepoint") (sit-for 2)
      (m3::forward-to-code (point-max))
      ;; Is there something between the last ';' and the current
      ;; line?
;;;     (message "m3::IFL: codepoint") (sit-for 2)
      (and
       (< (point) cur-point)
       ;; Yes -- means that the previous statement was incomplete...

       ;; ...unless the current line is an ssl-ender, in which
       ;; case it is assumed complete...
;;;      (message "incomplete-3") (sit-for 2)
       (or (not
	    (save-excursion
	      (goto-char first-code)
;;;	     (message "incomplete-3.1") (sit-for 2)
	      (looking-at m3::keyword-ssl-enders)))
	   (save-excursion
;;;	    (message "incomplete-3.2") (sit-for 2)
	     (goto-char first-code)
	     (m3::backward-to-code part-start)
	     (forward-char 1)
;;;	    (message "incomplete-3.21") (sit-for 2)
	     (let ((after (point)))
	       (m3::re-search-backward m3::keyword-endable-ssl-introducers
				      part-start t)
	       (re-search-forward m3::keyword-endable-ssl-introducers
				  cur-point t)
	       (goto-char (match-end 0))
;;;	      (message "incomplete-3.22") (sit-for 2)
	       (= (point) after))))

       ;; ... or there is a an ssl-ender between here and first-code
       ;; that is not a semi in an argument list...
       (not (save-excursion
;;;	     (message "incomplete-3.3-0") (sit-for 2)
	      (and (m3::re-search-forward
		    (concat ";\\|" m3::keyword-ssl-enders)
		    first-code 't)
		   (let ((continue t))
		     (while (and continue (m3::in-arg-list part-start))
;;;		      (message "incomplete-3.3-1") (sit-for 2)
		       (re-search-forward
			(concat ";\\|" m3::keyword-ssl-enders)
			first-code 't)
		       (goto-char (match-end 0))
;;;		      (message "incomplete-3.3-2") (sit-for 2)
		       (setq continue
			     (m3::re-search-forward
			      (concat ";\\|" m3::keyword-ssl-enders)
			      first-code 't)))
		     continue)
;;;		  (message "incomplete-3.3") (sit-for 2)
		   (< (point) first-code))))

       ;; ... or the previous statement is a multi-keyword statement
       ;; and the current line is completed by a subsequent keyword...
       (not
	(save-excursion
	  (goto-char cur-point)
	  (m3::backward-to-non-comment-line-start part-start)
;;;	 (message "m3::indent-for-line: multi-keyword") (sit-for 2)
	  (looking-at m3::multi-keyword-lines)))
       )))))



(defun m3::after-keyword-adjust-indent (indent first-code part-start)
  "Point is looking at a keyword at column INDENT; if the current line has
any code it starts at FIRST-CODE.  Return the proper indentation for the
current line."
;;;  (message "m3::after-keyword: indent = %d" indent) (sit-for 2)
  (let ((call-adjust-indent t))
    (cond
     ((looking-at "\\bEND\\b")
;;;      (message "m3::after-keyword(END): i: %d, m3::END: %d, m3::stand: %d"
;;;	       indent m3::END-undent m3::standard-offset)
;;;      (sit-for 2)
      (setq indent (- (+ indent m3::END-undent) m3::standard-offset)))

     ((looking-at "ELSE")
      (setq indent (+ indent m3::ELSE-undent))
      (if (m3::in-case part-start)
	  (setq indent (+ indent m3::case-offset))))
    

     ((looking-at "METHODS")
      (setq indent (+ indent m3::METHODS-undent)))
     ((looking-at "OVERRIDES")
      (setq indent (+ indent m3::OVERRIDES-undent)))
     ((looking-at "EXCEPT\\b")
;;;    (message "m3::after-keyword: EXCEPT" indent) (sit-for 2)
      (setq indent (+ indent m3::EXCEPT-undent)))
     ((looking-at "|")
;;;    (message "m3::after-keyword: vert" indent) (sit-for 2)
      (setq indent (+ indent m3::VERT-undent m3::case-offset)))
     ((looking-at m3::handler-start-re)
;;;      (message "m3::after-keyword: handler-start" indent) (sit-for 2)
      (setq indent (+ indent m3::handler-start-undent m3::case-offset)))
     ((looking-at "FINALLY")
      (setq indent (+ indent m3::FINALLY-undent)))
     ((looking-at "THEN")
      (setq indent (+ indent m3::THEN-undent)))
     ((looking-at "ELSIF")
      (setq indent (+ indent m3::ELSIF-undent)))
     ((looking-at "ELSE")
      (setq indent (+ indent m3::ELSE-undent)))
     ((looking-at "DO")
      (setq indent (+ indent m3::DO-undent)))
     ((looking-at "OF")
      (setq indent (+ indent m3::OF-undent)))
     ((looking-at m3::object-re)
      (setq indent (+ indent m3::OBJECT-undent)))
     ((looking-at "RECORD")
      (setq indent (+ indent m3::RECORD-undent)))

     ;; These are the keywords that can be followed by an SSL that begins on
     ;; the same line -- if so, indent to the level of the first elem.
     ((looking-at m3::same-line-ssl-keywords)
;;;      (message "m3::after-keyword: same-line-ssl") (sit-for 2)
      (let ((eol (save-excursion (end-of-line 1) (point))))
	(save-excursion
	  (forward-word 1)
	  (m3::forward-to-code (point-max))
;;;	  (message "m3::after-keyword: SlSSL(2)") (sit-for 2)
	  (cond
	   ((and
	     m3::follow-continued-indent
;;;	     (progn (message "m3::after-keyword: SlSSL(2.1)") (sit-for 2) t)
	     (<= (point) eol)
;;;	     (progn (message "m3::after-keyword: SlSSL(2.2)") (sit-for 2) t)
	     (save-excursion
	       (goto-char first-code)
	       (not (looking-at (concat m3::part-starters
					"\\|BEGIN\\|\\bEND\\b"))))
;;;	     (progn (message "m3::after-keyword: SlSSL(2.3)") (sit-for 2) t)
	     (save-excursion
	       (goto-char first-code)
	       (m3::backward-to-code part-start)
	       (looking-at ";"))
;;;	     (progn (message "m3::after-keyword: SlSSL(2.4)") (sit-for 2) t)
	     )
;;;	    (message "m3::after-keyword: SLSSL (3)") (sit-for 2)
	    (setq indent (current-column))
	    (setq call-adjust-indent nil))
	   (t
	    (setq indent (+ indent m3::standard-offset)))))))

     ;; These are all the keywords that don't affect the indentation
     ;; when they start complete lines.
     ((looking-at
       (concat "INTERFACE\\|MODULE\\|IMPORT\\|FROM\\|EXCEPTION"))
;;;    (message "m3::after-keyword: no extra") (sit-for 2)
      indent)

     ;; Otherwise, give the standard indentation.
     (t
;;;      (message "m3::after-keyword: standard") (sit-for 2)
      (setq indent (+ indent m3::standard-offset))))
	
    (cond
     (call-adjust-indent
      (save-excursion
	(goto-char first-code)
;;;	(message "m3::after-keyword: calling complete-adjust") (sit-for 2)
	(m3::complete-adjust-indent indent first-code part-start)))
     (t
;;;      (message "m3::after-keyword: not calling complete-adjust") (sit-for 2)
      indent))))


(defun m3::in-case (part-start)
;;;  (message "M3::in-case") (sit-for 2)
  (save-excursion
    (let ((cur-point (point)))
      (m3::backward-to-end-match part-start)
;;;      (message "M3::in-case(2)") (sit-for 2)
      (and
       (looking-at m3::case-starters)
       (progn
	 (cond
	  ((looking-at "TRY")
	   (forward-word 1)
	   ;; Is it a TRY-FINALLY or a TRY-EXCEPT?
	   (let (res (continue t))
	     (while continue
	       (setq res (m3::re-search-forward "TRY\\|EXCEPT\\|FINALLY"
					     cur-point t))
;;;	       (message "M3::in-case(3)") (sit-for 2)
	       (cond
		((looking-at "EXCEPT")
		 (setq continue nil))
		((looking-at "TRY")
		 ;; Go to matching END and try again
		 (m3::forward-to-end-match cur-point))
		(t;; FINALLY or not found
		 (setq res nil)
		 (setq continue nil))))
	     res))
	  (t t)))
       ;;; We are now looking at a case starter.  Make sure there is
       ;;; at least one case arm starter.
       (progn
	 (cond
	  ((looking-at "EXCEPT") (forward-word 1))
	  ((looking-at "CASE\\|TYPECASE")
	   (forward-word 1)
	   (m3::re-search-forward "OF" cur-point 'move-to-limit)
	   (forward-word 1)))
	 (m3::forward-to-code cur-point)
;;;	 (message "M3::in-case: about to test handler") (sit-for 2)   
	 (and (< (point) cur-point)
	      (looking-at m3::handler-start-re)))

;;;       (message "M3::in-case: returning t") (sit-for 2)
       ))))

	 
(defun m3::in-continued-record-def (part-start)
  (if (not (looking-at "\\bEND\\b"))
      (error "m3::in-continued-record-def assumes looking-at END"))
  (save-excursion
    (m3::backward-to-end-match part-start)
    (let ((end-match (point)) (eol (save-excursion (end-of-line) (point))))
      (beginning-of-line)
      (or (save-excursion
	    (re-search-forward "[ \t]*" eol t)
	    (= (point) end-match))
	  (save-excursion
	    (and
	     (re-search-forward "[ \t]*BRANDED[ \t]+" eol t)
	     (= (point) end-match)
	     (save-excursion
	       (goto-char end-match)
	       (looking-at "OBJECT"))))))))

	 
(defun m3::correct-for-trailing-ends (indent part-start)
  ;; If the previous line ends in a (series of) END(s) that does
  ;; (do) not start the line, and are unmatched by the start of the line,
  ;; subtract the END-undent(s) from indent (the Eric Muller convention.)
;;;  (message "correct-for-trailing-ends in: %d" indent) (sit-for 2)
  (let ((prev-line-start
	 (save-excursion
	   (m3::backward-to-code part-start)
	   (beginning-of-line)
	   (m3::forward-to-code (point-max))
;;;	   (message "correct-for-trailing-ends (0)") (sit-for 2)
	   (point))))
    (save-excursion
      (if (save-excursion
	    (m3::backward-to-code part-start)
	    (beginning-of-line)
	    (not (looking-at "[ \t]*END\\b")))
	  (save-excursion
	    (let ((continue t))
	      (while continue
		;; Move back to just after the last "real" (non-";") code
		(m3::backward-to-code part-start)
;;;		(message "correct-for-trailing-ends (1)") (sit-for 2)
		(if (looking-at ";") (m3::backward-to-code part-start))
		(forward-char 1)
;;;		(message "correct-for-trailing-ends (2)") (sit-for 2)
		
		;; Now, what are we looking at?
		(cond
		 ;; Is it an END?
		 ((or (save-excursion
			(forward-word -1) (looking-at "\\bEND\\b"))
		      (save-excursion
			(forward-word -2)
			(looking-at
			 (concat "\\bEND\\b" m3::poss-whitespace-re
				 m3::identifier-re m3::poss-whitespace-re
				 ";"))))
		  ;; Move back to the beginning of the end...
		  (re-search-backward "\\bEND\\b" part-start t)
		  (goto-char (match-beginning 0))
;;;		  (message "correct-for-trailing-ends (3)") (sit-for 2)
		  (if (not (looking-at "\\bEND\\b"))
		      (error "m3::complete-adjust-indent(A)"))
		  ;; Find the end matcher.
		  (let ((em-point
			 (save-excursion
			   (m3::backward-to-end-match part-start)
;;;			   (message "correct-for-trailing-ends EM") (sit-for 2)
			   (point))))
;;;		    (message "xxx") (sit-for 2)
		    (cond
		      ((< em-point prev-line-start)
		       (goto-char prev-line-start)
;;;		       (message "xxx<") (sit-for 2)
		       (setq indent
			     (save-excursion (goto-char em-point)
					     (current-column)))
		       (setq continue nil))
		      ((= em-point prev-line-start)
;;;		       (message "xxx=") (sit-for 2)
		       (setq indent (- indent m3::END-undent))
		       (setq continue nil))
		      ((> em-point prev-line-start)
		       (goto-char em-point)))))
		 (t
		  (setq continue nil))))))))
;;;    (message "m3::trailing-end returns %d" indent) (sit-for 2)
    indent))
     

(defun m3::complete-adjust-indent (indent first-code part-start)
  "Previous statement is complete and starts at column INDENT;
if the current line has any code it starts at FIRST-CODE.  Returns the
proper indentation for the current line."
;;;  (message "m3::complete-adjust(A): indent = %d, first-code = %d"
;;;	   indent first-code)
;;;  (sit-for 2)
  (save-excursion
    (goto-char first-code)
;;;    (message "m3::complete-adjust(B)") (sit-for 2)

    ;; If the previous line ends in a (series of) END(s) that does
    ;; (do) not start the line, and are unmatched before the start of the line,
    ;; add the END-undent(s) (the Eric Muller convention.)
;;;    (setq indent (m3::correct-for-trailing-ends indent part-start))
		  
;;;    (message "yyy2: indent = %d" indent) (sit-for 2)
    (cond
     ;; Some things can only start parts, and must be on the left margin.
     ((looking-at (concat "REVEAL\\b\\|EXCEPTION\\b\\|"
			  "FROM\\b\\|IMPORT\\b"))
      0)
      
     ;; These can start parts, but can also appear in the procedures.
     ((looking-at
       (concat "\\(PROCEDURE\\b\\|CONST\\b\\|VAR\\b\\|TYPE\\b\\|BEGIN\\b\\)"))
      ;; Look backwards for line-beginning-keywords that increase the
      ;; indentation, start an SSL, but don't require an END (i.e.,
      ;; TYPE, VAR, or CONST); or END's.  If the former is found first,
      ;; decrease the indentation to the same as the keyword line's.
      ;; If an END is found whose matcher is not something that can
      ;; occur in a TYPE, VAR, or CONST (i.e. RECORD or OBJECT),
      ;; indent normally.  If neither is found, indent normally.
;;;      (message "yyy7") (sit-for 2)
      (let ((new-indent indent) (continue t))
	(while continue
;;;	  (message "xxx1") (sit-for 2)
	  (m3::re-search-backward
	   (concat "\\(^[ \t]*\\(" m3::same-line-ssl-keywords "\\)\\|"
		   "\\bEND\\b\\|" m3::statement-starters "\\)")
	   part-start 'move-to-limit)
;;;	  (message "xxx2") (sit-for 2)
	  (cond
	   ;; If we reached the part-start because of the move-to-limit,
	   ;; indent to here...
	   ((looking-at (concat "^\\(" m3::part-starters "\\)"))
;;;	    (message "xxx2.5") (sit-for 2)
	    (goto-char first-code)
	    ;; If its the start of a procedure def, indent normally.
	    ;; Otherwise, indent to left margin.
	    (if (not (m3::after-procedure-introducer part-start))
		(setq new-indent 0))
	    (setq continue nil))
	      
	   ((and
	     (looking-at
	      (concat "^[ \t]*\\(" m3::same-line-ssl-keywords "\\)"))
	     (not (m3::in-arg-list part-start)))
	    (setq continue nil)

	    ;;; To accomodate part-starters that establish new indentations,
	    ;;; indent to the level of the previous part-starter, unless
	    ;;; that was a BEGIN.
	    (goto-char first-code)
	    (m3::re-search-backward
	     (concat m3::part-starters "\\|BEGIN") part-start t)
	    (while (m3::in-arg-list part-start)
	      (m3::re-search-backward
	       (concat m3::part-starters "\\|BEGIN") part-start t))
;;;	    (message "xxx3") (sit-for 2)
	    (cond
	     ((looking-at "BEGIN")
	      (setq new-indent (- new-indent m3::standard-offset)))
	     (t
	      (setq new-indent (current-column)))))
	     
	   ((looking-at
	     (concat "\\bEND[ \t]*" m3::identifier-re "[ \t]*;"))
	    (setq continue nil)
	    (setq new-indent (- new-indent m3::standard-offset)))


	   ((looking-at "\\bEND\\b")
	    (m3::backward-to-end-match part-start)
;;;	    (message "xxxEND-match") (sit-for 2)
	    (cond
	     ((looking-at "\\(RECORD\\|OBJECT\\)")
	      nil)
	     (t
	      (setq continue nil))))

	   (t
	    (setq continue nil))))
	new-indent))

     ;; If the current line is an END, add the END-undent.
     ((looking-at "\\bEND\\b")
;;;      (message "zzz1") (sit-for 2)
      (cond
       ((m3::in-case part-start)
	(- indent m3::END-undent m3::case-offset))
       ((save-excursion
	  (m3::backward-to-end-match (point-min))
	  (looking-at "^INTERFACE\\|^MODULE\\|^UNSAFE\\|^GENERIC"))
	0)
       (t
;;;	(message "Subtracting %d from indent %d." m3::END-undent indent)
	(- indent m3::END-undent))))


     ((looking-at "ELSE")
      (- indent m3::ELSE-undent
	 (if (m3::in-case part-start) m3::case-offset 0)))

     ((looking-at "METHODS")
      (- indent m3::METHODS-undent))
     ((looking-at "OVERRIDES")
      (- indent m3::OVERRIDES-undent))
     ((looking-at "EXCEPT")
      (- indent m3::EXCEPT-undent))
     ((looking-at "UNTIL")
      (- indent m3::UNTIL-undent))
     ((looking-at "|")
      (cond
       ((save-excursion
	  (m3::backward-to-code part-start)
;;;	  (message "zzz2") (sit-for 2)
	  (or
	   (save-excursion
	     (and (> (point) 1)
		  (progn (forward-char -1) (looking-at "OF"))))
	   (save-excursion
	     (and (> (point) 5)
		  (progn (forward-char -5) (looking-at "EXCEPT"))))))
	(- indent m3::VERT-undent))
       (t
	(- indent m3::VERT-undent m3::case-offset))))

     ((looking-at "FINALLY")
      (- indent m3::FINALLY-undent))
     ((looking-at "THEN")
      (- indent m3::THEN-undent))
     ((looking-at "ELSIF")
      (- indent m3::ELSIF-undent))
     ((looking-at "ELSE")
      (- indent m3::ELSE-undent))
     ((looking-at "DO")
      (- indent m3::DO-undent))
     ((looking-at "OF")
      (- indent m3::OF-undent))
     ((looking-at "RECORD")
;;;      (message "zzz-record") (sit-for 2)
      (- indent m3::RECORD-undent))
     ((looking-at m3::object-re)
;;;      (message "zzz-object") (sit-for 2)
      (- indent m3::OBJECT-undent))
     (t
;;;      (message "zzz-t: indent = %d" indent) (sit-for 2)
      indent))))
  

(defun m3::incomplete-indent (cur-point first-code part-start)
  (let* (list-indent
	 (prev-line-start
	  (save-excursion
	    (m3::backward-to-non-comment-line-start part-start)
	    (point)))
	 (last-char-prev-line
	  (save-excursion
	    (m3::backward-to-non-comment-line-start part-start)
	    (end-of-line)
	    (m3::backward-to-code
	     (save-excursion (beginning-of-line) (point)))
	    (point)))
	 (prev-line-indent
	  (save-excursion
	    (m3::backward-to-non-comment-line-start part-start)
	    (let ((pli (current-column)))
	      (cond
	       ((looking-at m3::statement-keywords)
		(forward-word 1)
		(m3::forward-to-code first-code)
		(cond
		 ((<= (point) last-char-prev-line)
		  (current-column))
		 (t pli)))
	       (t pli))))))
;;;    (message "m3::incomplete-indent(A)") (sit-for 2)
    (cond
     ;; Did the previous non-blank line end with a paren?
     ((save-excursion
	(goto-char last-char-prev-line)
	(looking-at m3::left-parens))

;;;      (message "m3::incomplete-indent(PAREN)") (sit-for 2)
      ;;   Find the indentation of the previous line,
      ;;     either add open-paren-offset, or indent of paren +
      ;;     open-paren-sep
      (goto-char last-char-prev-line)
      (cond
       (m3::open-paren-offset
;;;	(message "m3::incomplete-indent(PAREN offset)") (sit-for 2)
	(re-search-backward
	 (concat m3::identifier-re m3::poss-whitespace-re)
	 part-start t)
	(goto-char (match-beginning 0))
	;; Account for qualified names.
	(cond
	 ((save-excursion
	    (and (> (point) 1)
		 (progn
		   (forward-char -1)
		   (looking-at "\\."))))
	  (re-search-backward
	   (concat m3::identifier-re m3::poss-whitespace-re)
	   part-start t)
	  (goto-char (match-beginning 0))))

;;;	(message "m3::incomplete-indent(PAREN offset 2)") (sit-for 2)

	(if (and m3::proc-param-from-proc-keyword
		 (save-excursion
		   (forward-word -1)
		   (looking-at "PROCEDURE")))
	    (forward-word -1))

;;;	(message "m3::incomplete-indent(PAREN offset 3)") (sit-for 2)
	(+ (current-column) m3::open-paren-offset))

       (t
	(+ (current-column) m3::open-paren-sep))))
		
     ;; Did the previous line end with a ',' or ';'?:
     ((save-excursion
	(goto-char last-char-prev-line)
	(looking-at ",\\|;"))

;;;      (message "m3::incomplete-indent(COMMA)") (sit-for 2)
      ;; Skip over any matched parens; if this puts us at a line
      ;; containing an unmatched left paren, indent to that +
      ;; paren-sep.  Otherwise, indent same as beginning of that line.
      (save-excursion
	(goto-char last-char-prev-line)
	(let ((continue t) res)
	  (while continue
;;;	    (message "m3::incomplete-indent(COMMA) 0") (sit-for 2)
	    (m3::re-search-backward
	     (concat m3::left-parens "\\|" m3::right-parens)
	     (save-excursion (beginning-of-line)
			     (point)) 'move-to-limit)
;;;	    (message "m3::incomplete-indent(COMMA) 1") (sit-for 2)
	    (cond
	     ((looking-at m3::left-parens)
;;;	      (message "m3::incomplete-indent(COMMA) lp") (sit-for 2)
	      (setq continue nil)
	      (forward-char 1)
	      (re-search-forward "[ \t]*") (goto-char (match-end 0))
	      (setq list-indent (current-column)))
	     ((looking-at m3::right-parens)
;;;	      (message "m3::incomplete-indent(COMMA) rp") (sit-for 2)
	      (forward-char 1)
	      (backward-sexp 1))
	     (t
;;;	      (message "m3::incomplete-indent(COMMA) none") (sit-for 2)
	      (beginning-of-line)
	      (skip-chars-forward "[ \t]") 
	      (setq continue nil)
	      (setq list-indent (current-column)))))
;;;	  (message "m3::incomplete-indent(COMMA) end") (sit-for 2)
	  (cond
	   ((looking-at (concat "|[ \t]*" m3::identifier-char-re))
	    (forward-word 1) (forward-word -1)
	    (setq list-indent (current-column)))
	   ((looking-at m3::statement-keywords)
	    (forward-word 1)
	    (re-search-forward "[ \t]*" last-char-prev-line t)
	    (setq list-indent (current-column))))))
      list-indent)
	      
     ;; Did the previous non-blank line end a procedure header?
     ((m3::after-procedure-introducer part-start)
;;;      (message "m3::incomplete-indent(PROCEDURE)") (sit-for 2)
      (goto-char last-char-prev-line)
      (m3::re-search-backward "PROCEDURE" part-start t)
      (+ (current-column) m3::standard-offset))

     ;; Does the current line start a RAISES clause?
     ((looking-at "^[ \t]*RAISES")
;;;      (message "m3::incomplete-indent(RAISES)") (sit-for 2)
      (goto-char last-char-prev-line)
      (m3::re-search-backward "\\(PROCEDURE\\|METHODS\\)"
			     part-start t)
      (if (looking-at "METHODS")
	  (progn (forward-word 1) (m3::forward-to-code (point-max))))
      (+ (current-column) m3::RAISES-offset))

     ;; Did the previous line end with an assignment?
     ((save-excursion
	(goto-char last-char-prev-line)
	(beginning-of-line)
;;;	(message "m3::incomplete-indent(:= 1)") (sit-for 2)
	(and (m3::re-search-forward ":=" (1+ last-char-prev-line) t)
	     (re-search-forward "[^ \t]" last-char-prev-line t)))
;;;      (message "m3::incomplete-indent(:=)") (sit-for 2)
      (goto-char last-char-prev-line)
      (beginning-of-line)
      (m3::re-search-forward ":=" last-char-prev-line t)
      (forward-char 2)
      (re-search-forward "[ \t]*[^ \t]")
      (+ (- (current-column) 1) m3::assign-offset))

     ;; Otherwise:
     (t
;;;      (message "m3::incomplete-indent(OTHER)") (sit-for 2)
      ;; Find out if the previous line begins the statement.
      (goto-char prev-line-start)
      (m3::re-search-backward
       (concat ";\\|" m3::keyword-line-starters "\\|" m3::part-starters
	       "\\|" m3::statement-keywords)
       part-start t)
      (while (m3::in-arg-list part-start)
	(m3::re-search-backward
	 (concat ";\\|" m3::keyword-line-starters "\\|" m3::part-starters
		 "\\|" m3::statement-keywords)
	 part-start t))
;;;      (message "m3::incomplete-indent(OTHER1)") (sit-for 2)
      (if (or (> (point) part-start)
	      (and (= (point) part-start)
		   (looking-at m3::keyword-endable-ssl-introducers)))
	  (progn
	    (re-search-forward
	     (concat ";\\|" m3::keyword-line-starters "\\|" m3::part-starters
		     "\\|" m3::statement-keywords)
	     cur-point t)
	    (goto-char (match-end 0))))
;;;      (message "m3::incomplete-indent(OTHER1.5)") (sit-for 2)
      (m3::forward-to-code (point-max))
;;;      (message "m3::incomplete-indent(OTHER2), prev-line-start = %d"
;;;	       prev-line-start)
;;;      (sit-for 2)
      (cond
       ;; If the previous line begins the statement, add
       ;; m3::standard-offset to indentation, unless the prev-line-indent
       ;; has already skipped over a keyword.
       ((= (point) prev-line-start)
;;;	(message "m3::incomplete-indent(START): prev-line-indent = %d"
;;;		 prev-line-indent)
;;;	(sit-for 2)
	(m3::complete-adjust-indent
	 ;; Indent further if we haven't indented already.
	 (cond
	  ((= prev-line-indent
	      (save-excursion (goto-char prev-line-start) (current-column)))
	   (+ prev-line-indent m3::continued-line-offset))
	  (t prev-line-indent))
	 first-code part-start))
       (t
;;;	(message "m3::incomplete-indent(CONT)") (sit-for 2)
	;; Otherwise, same indentation as previous, modulo adjustment
	;; for current line
	prev-line-indent))))))


(defun m3::after-procedure-introducer (part-start)
  "Returns t iff first non-blank non-comment character before point is the '='
of a procedure definition."
  (save-excursion
    (m3::backward-to-code part-start)
    (and
     (looking-at "=")
;;;     (message "m3::API(0)") (sit-for 2)
     (let ((eq-point (point)))
       (and
	;; Not that this does not allow any comments in
	;;   PROCEDURE Foo <left-paren>
	;; and all must occur on the same line.
	(m3::re-search-backward
	 (concat "PROCEDURE[ \t]*" m3::identifier-re "[ \t]*(")
	 part-start t)
;;;	(message "m3::API(1)") (sit-for 2)
	(progn
	  (re-search-forward
	   (concat "PROCEDURE[ \t]*" m3::identifier-re "[ \t]*(")
	   eq-point t)
	  (goto-char (match-end 0))
;;;	  (message "m3::API(2)") (sit-for 2)
	  (forward-char -1)
	  (and
	   (condition-case err
	       (progn (forward-sexp 1) t)
	     (error nil))
;;;	   (message "m3::API(3)") (sit-for 2)
	   ;; We should now be at the right paren of the arg-list.
	   ;; Check for a return type.
	   (progn
	     (m3::forward-to-code eq-point)
	     (and
;;;	      (message "m3::API(4)") (sit-for 2)
	      (cond
	       ((looking-at ":")
		(forward-char 1)
		(m3::forward-to-code eq-point)
		(and
		 (looking-at m3::poss-qual-ident-re)
		 (progn
		   (re-search-forward m3::poss-qual-ident-re eq-point t)
		   (goto-char (match-end 0))
		   (m3::forward-to-code eq-point)
		   t)))
	       (t t))
	      ;; Now check for RAISES clause.
;;;	      (message "m3::API(5)") (sit-for 2)
	      (cond
	       ((looking-at "RAISES")
		(forward-word 1)
		(m3::forward-to-code eq-point)
		(cond
		 ((looking-at "ANY")
		  (forward-word 1)
		  (m3::forward-to-code eq-point)
		  t)
		 ((looking-at "{")
;;;		  (message "m3::API(5.5)") (sit-for 2)
		  (and
		   (condition-case err
		       (progn (forward-sexp 1) t)
		     (error nil))
		   (progn (m3::forward-to-code eq-point) t)))
		 (t t)))
	       (t t))

	      ;; Now, we better be back to the original =!
	      (= (point) eq-point))))))))))


(defun m3::backward-to-end-match (part-start &optional depth)
  (if (not depth) (setq depth 0))
  (let (res
	(case-fold-search nil)
	(continue t))
    (while continue
;;;      (message "m3::backward-to-end-match(1) [%d]" depth) (sit-for 1)
      (setq res (m3::re-search-backward
		 (concat "\\(" m3::end-matchers "\\|\\bEND\\b\\)") 
		 part-start t))
      (cond
       ((and res (looking-at "\\bEND\\b"))
	(m3::backward-to-end-match part-start (1+ depth)))
       (t
	(setq continue nil))))
    res))

(defun m3::forward-to-end-match (max-point &optional depth)
  (if (not depth) (setq depth 0))
  (if (looking-at (concat "\\(" m3::statement-starters "\\)")) (forward-word 1))
  (let (res
	(case-fold-search nil)
	(continue t))
    (while continue
;;;      (message "m3::backward-to-end-match(1) [%d]" depth) (sit-for 1)
      (setq res (m3::re-search-forward
		 (concat "\\(" m3::statement-starters "\\|\\bEND\\b\\)")
		 max-point t))
      (cond
       ((looking-at m3::statement-starters)
	(m3::forward-to-end-match max-point (1+ depth)))
       (t   ;; looking at END or reached max-point
	(forward-word 1)
	(setq continue nil))))
    res))

(defun m3::backward-to-until-match (part-start &optional depth)
  (if (not depth) (setq depth 0))
  (let (res
	(case-fold-search nil)
	(continue t))
    (while continue
;;;      (message "m3::backward-to-end-match(1) [%d]" depth) (sit-for 1)
      (setq res (m3::re-search-backward
		 (concat "\\(\\bREPEAT\\b\\|\\bUNTIL\\b\\)") part-start t))
      (cond
       ((and res (looking-at "UNTIL"))
	(m3::backward-to-until-match part-start (1+ depth)))
       (t
	(setq continue nil))))
    res))

(defun m3::forward-sexp (n)
  "Moves forward to the (end of the) END that terminates the current innermost
syntactic unit.  With a prefix argument, does that N times."
  (interactive "p")
  (while (and (> n 0) (< (point) (point-max)))
    (m3::forward-to-end-match (point-max))
    (setq n (- n 1))))

(defun m3::backward-sexp (n)
  "Moves backward to the (start of) the keyword that starts the current
innermost syntactic unit.  With a prefix argument, does that N times."
  (interactive "p")
  (while (and (> n 0) (> (point) (point-min)))
    ;; Make forward and backward sexp inverses...
    (forward-word -1)
    (m3::backward-to-end-match (point-min))
    (setq n (- n 1))))

(defun m3::end-of-defun (n)
  "Moves forward to the line after the end of the current 'defun', or top-level
syntactic unit.  With a prefix argument, does that N times."
  (interactive "p")
  (while (and (> n 0) (< (point) (point-max)))
    (m3::end-of-defun-work)
    (setq n (- n 1))))

(defun m3::end-of-defun-work ()
  (skip-chars-forward " \t\n")
  (if (not (looking-at 
	    (concat "^\\(" m3::com-start-re "\\|" m3::part-starters
		    "\\|\\bEND\\b\\)")))
      (m3::backward-to-last-part-begin))
  (cond
   ((looking-at m3::com-start-re)
    (m3::skip-comment-forward (point-max) t)
    (beginning-of-line 2))
   ((looking-at m3::part-starters)
    (forward-char 1)
    (let ((start (point)))
      (if (re-search-forward
	   (concat "^\\(" m3::com-start-re "\\|"
		   m3::part-starters "\\|\\bEND\\b\\)")
	   (point-max) 'move-to-limit)
	  (goto-char (match-beginning 0)))
      (if (looking-at m3::com-start-re) (forward-char -2))
      (m3::backward-to-code start)
      (beginning-of-line 2)))
   (t (beep))))



(defun m3::backward-to-non-comment-line-start (part-start)
  "Sets the point at the first non-whitespace character in a line that
contains something other than comments and/or whitespace."
  (m3::backward-to-code part-start)
  (beginning-of-line)
  (m3::skip-whitespace-in-line))


(defun m3::skip-whitespace-in-line ()
  (re-search-forward "[ \t]*"))


(defun m3::indent-to (cur-point new-column)
  "Make current line indentation NEW-COLUMN.  If the point is to the
left of the first non-blank character, move it to NEW-COLUMN.
Otherwise, maintain its relative position.  Has the side effect
of converting tabs to spaces."
  (goto-char cur-point)
  (untabify (save-excursion (beginning-of-line) (point))
	    (save-excursion (end-of-line) (point)))
  (let ((cur-column (current-column))
	(cur-point (point))
	(first-column
	 (save-excursion
	   (beginning-of-line)
	   (re-search-forward " *")
	   (current-column))))
    (let ((diff (- new-column first-column)))
      (cond
       ((> diff 0)
	(beginning-of-line)
	;; Must do this to make sure the keyword completion marker moves
	;; correctly.
	(let ((d diff))
	  (while (> d 0)
	    (insert-before-markers " ") (setq d (1- d))))
	)
       ((< diff 0)
	(save-excursion
	  (forward-char (- first-column cur-column))
	  (backward-delete-char-untabify (- diff)))))
      (cond
       ((> first-column cur-column)
	(beginning-of-line)
	(forward-char new-column))
       (t
	(goto-char (+ cur-point diff)))))))


(defun m3::in-comment-or-string ()
  "Returns 'string if point is in an unterminated string, 'comment if in
an unterminated comment, otherwise, nil."
  (save-excursion
    (beginning-of-line)
    (let ((cur-point (point))
	  (state nil))
      (save-excursion
	;; We assume the lisp-like convention that "top-level defuns,"
	;; or "parts", are the only things that occur on the left
	;; margin (we make an exception for end-comments.)
	(m3::backward-to-last-part-begin)
	(while (and (not state)
		    (re-search-forward
		     (concat "\\(" m3::com-start-re "\\|\"\\|'\\)")
		     cur-point t))
	  (goto-char (match-beginning 0))
	  (cond
	   ((looking-at m3::com-start-re)
	    (setq state 'comment)
	    (if (m3::skip-comment-forward cur-point t) (setq state nil)))
	   ((looking-at "\"")
	    (setq state 'string)
	    (if (re-search-forward "[^\\\\]\"" cur-point t)
		(setq state nil)))
	   ((looking-at "'")
	    (setq state 'string)
	    (if (re-search-forward "[^\\\\]'" cur-point t)
		(setq state nil)))))
	state))))

(defun m3::backward-to-last-part-begin ()
;;;  (beginning-of-line nil)
;;;  (message "search-start") (sit-for 2)
  (let ((search-start (point)))
    (if (re-search-backward
	 (concat "^\\(" m3::com-start-re "\\|" m3::part-starters
		 "\\|\\bEND\\b\\)")
	 (point-min) t)
	(progn
	  (goto-char (match-beginning 0))
	  (when (looking-at "\\bEND\\b")
	    (m3::end-of-ender (point-max))
	      (forward-line 1) (beginning-of-line 1))
;;;	  (message "prev") (sit-for 2)
	  )
      (goto-char (point-min)))
    (let ((last-found (point)))
      (forward-char 1)
      (if (re-search-forward
	   (concat "^\\(" m3::com-start-re "\\|" m3::part-starters
		   "\\|\\bEND\\b\\)")
	   (point-max) t)
	  (progn
	    (goto-char (match-beginning 0))
	    (when (looking-at "\\bEND\\b")
	      (m3::end-of-ender (point-max))
	      (forward-line 1) (beginning-of-line 1))
;;;	    (message "after-prev") (sit-for 2)
	    )
	(goto-char (point-max)))
      (if (<= search-start (point)) (goto-char last-found))))
;;;  (message "part-start") (sit-for 2)
  )

(defun m3::beginning-of-defun (n)
  "Moves backward to the start of the current 'defun', or top-level
syntactic unit.  With a prefix argument, does that N times."
  (interactive "p")
  (while (and (> n 0) (> (point) (point-min)))
    (forward-char -1)
    (m3::backward-to-last-part-begin)
    (setq n (- n 1))))

  

(defun m3::forward-to-code (max-point)
  "Sets the point at the first non-comment, non-whitespace character
following the current point, else at max-point."
;;;  (message "m3::forward-to-code (1)") (sit-for 2)
  (let ((continue t))
    (while continue
;;;      (message "m3::forward-to-code (1.5)") (sit-for 2)
      (setq continue
	    (and (re-search-forward "[^ \t\n]" max-point 'move-to-limit)
		 (progn (goto-char (match-beginning 0))
;;;			(message "m3::forward-to-code (2)") (sit-for 2)
			(and (looking-at m3::com-start-re)
			     (m3::skip-comment-forward max-point t))))))))


(defun m3::backward-to-code (min-point)
  "Sets the point at the first non-comment, non-whitespace character
before the current point, else at end-of-file"
  (let ((continue t))
    (while continue
      (if (re-search-backward "[^ \t\n][ \t\n]*" min-point t)
	  (goto-char (match-beginning 0))
	(goto-char min-point))
      (setq continue (and (save-excursion
			    (and (> (point) 1)
				 (progn
				   (forward-char -1)
				   (looking-at m3::com-end-re))))
			  (progn
			    (forward-char 1)
			    (m3::skip-comment-backward min-point t)))))

    t))

(defun m3::re-search-forward (re max-point fail)
  "Assumes we're not in a comment or a string.  Puts point at the start of the
first occurence of RE that is not in a comment or string, if such an occurence
occurs before MAX-POINT, and returns non-nil.  Otherwise, returns nil
and leaves point unaffected.  Results are undefined if RE matches any
comment starter."
  (let ((continue t)
	(save-point (point))
	(res nil))
    (while continue
      (setq res (re-search-forward
		  (concat "\\(" m3::com-start-re "\\|\"\\|" re "\\)")
		  max-point fail))
      (goto-char (match-beginning 0))
      (cond
       (res
	(cond
	 ((looking-at m3::com-start-re)
	  (m3::skip-comment-forward max-point fail))
	 ((looking-at "\"")
	  (forward-char -1)
	  (re-search-forward "[^\\]\"" max-point 'move-to-point)
	  (goto-char (match-end 0)))
	 (t
	  (setq continue nil))))
       (t
	(setq continue nil)
	(if (and (eq fail t) (not res))
	    (goto-char save-point)))))
    res))
	

(defun m3::re-search-backward (re min-point fail)
  "Assumes we're not in a comment.  Puts point the start of the
first previous occurence of RE that is not in a comment, if such an occurence
occurs before MIN-POINT, and returns non-nil.  FAIL is interpreted as is third
argument to re-search.  Results are undefined if RE matches any comment
starter." 
  (let ((continue t)
	(save-point (point))
	(res nil))
    (while continue
      (setq res (re-search-backward
		 (concat "\\(" m3::com-end-re "\\|\"\\|" re "\\)")
		 min-point fail))
      (cond
       (res
	(cond
	 ((looking-at m3::com-end-re)
	  (forward-char 2)
	  (m3::skip-comment-backward min-point fail))
	 ((looking-at "\"")
	  (let ((quote-continue t))
	    (while quote-continue
;;;	      (message "m3::re-search-backward (1)") (sit-for 2)
	      (if (re-search-backward "\"" min-point 'move-to-point)
		  (goto-char (match-beginning 0)))
;;;	      (message "m3::re-search-backward (2)") (sit-for 2)
	      (cond
	       ((or (= (point) min-point)
		    (save-excursion
		      (forward-char -1)
		      (not (looking-at "\\\\"))))
		(setq quote-continue nil)))
;;;	      (message "m3::re-search-backward (3)") (sit-for 2)
	      )))
	 (t
	  (setq continue nil))))
       (t
	(setq continue nil)
	(if (and (eq fail t) (not res))
	    (goto-char save-point)))))
    res))

(defun m3::skip-comment-forward (max-point fail)
  "Requires that point is at the start of a comment.  If that comment
is terminated before MAX-POINT, return t and leaves point after end of
the comment.  Otherwise, if fail is 't, returns returns nil and leaves
the point unchanged; if fail is nil raises an errer; if fail is not t or nil,
returns nil and leaves the point at max-point or (point-max), whichever is
smaller."
  (if (not (looking-at m3::com-start-re))
      (error
       "m3::skip-comment-forward should only be called when looking at
comment-starter"))
  (forward-char 2)
  (let ((save-point (point)) (continue t) res)
    (while continue
;;;      (message "m3::comment-forward (0.5)") (sit-for 2)
      (setq res (re-search-forward m3::com-start-or-end-re max-point fail))
      (cond
       (res
;;;	(message "m3::comment-forward (1)") (sit-for 2)
	(goto-char (match-beginning 0))
;;;	(message "m3::comment-forward (2)") (sit-for 2)
	(cond
	 ((looking-at m3::com-start-re)
	  (if (not (m3::skip-comment-forward max-point fail))
	      (progn (setq res nil)
		     (setq continue nil))))
	 ((looking-at m3::com-end-re)
	  (goto-char (match-end 0))
	  (setq continue nil))
	 (t
;;;	  (message "m3::comment-forward (4)") (sit-for 2)
	  (goto-char save-point)
	  (setq res nil)
	  (setq continue nil))))
       (t 
;;;	(message "m3::comment-forward (5)") (sit-for 2)
	(goto-char save-point)
	(setq res nil)
	(setq continue nil))))
    res))


(defun m3::skip-comment-backward (min-point fail)
  "Requires that point is at the end of a comment.  If that comment
is terminated before MIN-POINT, return t and leaves point at the start
the comment.  Otherwise returns nil and leaves the point in an
unspecified position."
  (forward-char -2)
  (if (not (looking-at m3::com-end-re))
      (error
       "m3::skip-comment-backward should only be called when looking at
comment-ender"))
  (let ((save-point (point)) (continue t) res)
    (while continue
      (setq res (re-search-backward m3::com-start-or-end-re min-point fail))
      (cond
       (res
	(cond
	 ((looking-at m3::com-end-re)
	  (forward-char 2)
	  (if (not (m3::skip-comment-backward min-point fail))
	      (progn
		(setq res nil)
		(setq continue nil))))
	 ((looking-at m3::com-start-re)
	  (setq continue nil))
	 (t
	  (goto-char save-point)
	  (setq res nil)
	  (setq continue nil))))
       (t
	(goto-char save-point)
	(setq res nil)
	(setq continue nil))))
    res))
     

;;; -------- Electric END completion --------

(defun m3::do-electric-end ()
;;;  (message "m3::do-electric-end") (sit-for 2)
  (let ((start-point (point))
	(case-fold-search nil))
    (cond
     ((and (save-excursion
	     (end-of-line)
	     (forward-word -1)
;;;	     (progn (message "m3::do-electric-end 1.2") (sit-for 2) t)
	     (and
	      (looking-at "\\bEND\\b")
	      (or (save-excursion (beginning-of-line)
				  (looking-at "[ \t]*\\bEND\\b[ \t]*$"))
		  (progn (forward-word 1)
			 (= (point) start-point)))))
	   (or m3::electric-end m3::blink-end-matchers))
;;;      (progn (message "m3::do-electric-end 1.5") (sit-for 2) t)
      (let ((insert-point
	     (save-excursion (end-of-line)
			     (forward-word -1)
			     (forward-word 1)
			     (point)))
	    (insert-string))
;;;	(progn (message "m3::do-electric-end 2") (sit-for 2) t)
	(end-of-line) (forward-word -1)
	(save-excursion
	  (and
	   (m3::backward-to-end-match (point-min))
	   (if m3::blink-end-matchers (sit-for 1) t)
;;;	   (progn (message "m3::do-electric-end 3") (sit-for 1) t)
	   (progn
	     (cond
	      ;; Do nothing if we're not supposed to...
	      ((not m3::electric-end))
	      ;; If it's a begin, what is it the begin of?
	      ((looking-at "BEGIN")
	       (setq insert-string
		     (save-excursion (m3::backward-to-BEGIN-owner)))
	       )

	      ((looking-at "INTERFACE\\|MODULE")
	       (forward-word 2)
	       (setq insert-string
		     (concat
		      (buffer-substring
		       (save-excursion (forward-word -1) (point))
		       (point))
		      ".")))

	      ;; Otherwise, m3::electric-end must be 'all.
	      ((eq m3::electric-end 'all)
;;;	       (progn (message "m3::do-electric-end non-BEGIN") (sit-for 2) t)
	       (setq insert-string
		     (concat "(* "
			     (buffer-substring
			      (point)
			      (save-excursion (forward-word 1) (point)))
			     " *)")))))))

	(cond
	 (insert-string
	  (progn
	    (goto-char insert-point)
	    ;; If we completed an END and then added something, include
	    ;; the something in the completion...
	    (if (and (marker-position m3::cur-keyword-completion-start)
		     (= insert-point
			(+ m3::cur-keyword-completion-start
			   m3::cur-keyword-completion-len)))
		(setq m3::cur-keyword-completion-len
		      (+ m3::cur-keyword-completion-len 1
			 (length insert-string))))
	    (insert " " insert-string)))
	 (t
	  (goto-char start-point))))))))

(defun m3::backward-to-BEGIN-owner ()
  "Assumes looking-at BEGIN.  If this begin is a module main body or
the body of a procedure, moves backward to the MODULE or PROCEDURE
keyword of that module or procedure, and returns the name of the
MODULE or procedure.  If neither of these are true, does not move
point, and returns the string BEGIN if m3::electric-end is 'all, and
nil otherwise."
;;;  (message "begin-owner") (sit-for 2)
  (let ((insert-string nil) (orig-point (point)) (new-point (point)))
    (save-excursion
      (cond
       ;; If it's on the left margin, it must be a module.
       ((looking-at "^BEGIN")
	(goto-char (point-min))
	(and
	 (re-search-forward "MODULE\\|INTERFACE" (point-max) t)
	 (progn
	   (goto-char (match-beginning 0))
	   (setq new-point (point))
	   (forward-word 2)
	   (setq insert-string
		 (concat
		  (buffer-substring
		   (save-excursion (forward-word -1) (point))
		   (point))
		  ".")))))
       ;; Is it the body of a procedure?
       ((let ((continue t))
	  (while continue
	    (m3::re-search-backward
	     "BEGIN\\|PROCEDURE\\|\\bEND\\b" (point-min) t)
	    (cond
	     ((looking-at "\\bEND\\b")
	      (m3::backward-to-end-match (point-min))
	      (cond
	       ((looking-at "BEGIN")
		(m3::re-search-backward
		 "BEGIN\\|PROCEDURE" (point-min) t)
		(if (looking-at "BEGIN") (forward-word 1)))))
	     (t
	      (setq continue nil))))
	  (and (looking-at "PROCEDURE")
	       (progn
;;;		 (message "m3::BEGIN-owner PROC 2") (sit-for 2)
		 (setq new-point (point))
		 (forward-word 2)
		 (setq insert-string
		       (concat
			(buffer-substring
			 (save-excursion (forward-word -1) (point))
			 (point))
			";"))))))
       ;; Otherwise, it is just a random BEGIN, so
       ;; m3::electric-end must be 'all.
       ((eq m3::electric-end 'all)
	(setq insert-string "(* BEGIN *)"))))
    (goto-char new-point)
    insert-string))
	  

;;;  --------  PSEUDO ABBREV MODE --------

(defun m3::toggle-abbrev ()
  "Toggle the flag enabling/disabling Modula 3 pseudo abbrev mode."
  (interactive)
  (setq m3::abbrev-enabled (not  m3::abbrev-enabled))
  (message "M3 abbrev-enabled is now %s." m3::abbrev-enabled))


(defun m3::prev-word ()
  "returns last word in buffer."
  (buffer-substring (point) (save-excursion (backward-word 1) (point))))

(defun m3::is-abbrev (keyword word)
  "Returns non-nil if WORD is abbreviation of given KEYWORD."
  (if (> (length word) (length keyword)) ()
    (string-equal (substring keyword 0 (length word)) (upcase word))))


(defun m3::is-prefix (word prefix &optional no-upper)
  "returns non-nil if PREFIX is a (non-proper) prefix of WORD."
  (let ((uword (if no-upper word (upcase word)))
	(uprefix (if no-upper prefix (upcase prefix))))
    (if (> (length prefix) (length word)) nil
      (string-equal (substring uword 0 (length prefix)) uprefix))))


(defun m3::if-abbrev-kill-prev (keyword word)
  "checks if word is abbreviation of keyword; if so deletes last word
in buffer." 
  (if (not (m3::is-abbrev keyword word)) ()
    (forward-word -1)
    (delete-region (point) (save-excursion (forward-word 1) (point)))
    t))
		 

(defun m3::complete-abbrev ()
  "call appropriate m3::function depending on value of last word in buffer."
  (let ((pw (m3::prev-word)))
    ;; Must split this in two because it's so big (or else elisp
    ;; can't handle it.)
    (if m3::abbrev-enabled
	(m3::complete-abbrev-work pw))))
      

;;; Here is the data structure we use to decide what keywords are
;;; appropriate completions of a prefix in the current context, and
;;; how we should order them.
;;;
;;; This alist associates with each keyword:
;;; (<score> <left-margin> <pred>)
;;;
;;; <score> is a score for breaking ties.  Smaller numbers are
;;;    preferred to higher.
;;; <props> is a list of properties of the keyword.
;;;    Properties include:
;;;      left-margin status:  It is assumed that a keyword cannot
;;;        appear at the left-margin unless it has one of the
;;;        properties 'lm-ok or 'lm-only, which indicate that it can
;;;        or must appear at the left margin, respectively.
;;;      line-starter status:  It is assumed that a keyword cannot
;;;        appear after an ssl-introducer unless it has one of the
;;;        properties 'ls-ok or 'ls-only, which indicate that it can
;;;        or must appear after an ssl-introducer, respectively.
;;; <pred>, if non-nil, is a function that must return non-nil for the
;;;    completion to be legal

(defconst m3::keyword-completions
  '(("ABS" . (3 ()))
    ("ADDRESS" . (5 ()))
    ("ADR" . (6 ()))
    ("ADRSIZE" . (7 ()))
    ("AND" . (2 ()))
    ("ANY" . (1 () (lambda (on-lm starts-ssl)
		     (m3::keyword-before-ssl-introducer-p "RAISES"))))
    ("ARRAY" . (4 (ls-ok) (lambda (on-lm starts-ssl)
			    (or (not starts-ssl)
				(save-excursion
				  (forward-word -2)
				  (looking-at "OF"))))))

    ("BEGIN" . (1 (lm-ok ls-ok) (lambda (on-lm starts-ssl)
				    (save-excursion
				      (forward-word -1)
				      (if (not starts-ssl)
					  (m3::after-procedure-introducer
					   (point-min))
					t)))))
    ("BITS" . (6 ()))
    ("BITSIZE" . (7 ()))
    ("BOOLEAN" . (3 ()))
    ("BRANDED" . (4 ()))
    ("BY" . (2 () (lambda (on-lm starts-ssl)
		    (m3::keyword-before-ssl-introducer-p "FOR"))))
    ("BYTESIZE" . (5 ()))

    ("CARDINAL" . (4 (ls-of)))
    ("CASE" . (3 (ls-only)))
    ("CEILING" . (5 ()))
    ("CHAR" . (2 (ls-of)))
    ("CONST" . (1 (lm-ok ls-ok)))

    ("DEC" . (2 (ls-only)))
    ("DISPOSE" . (4 (ls-only)))
    ("DIV" . (3 ()))
    ("DO" . (1 () (lambda (on-lm starts-ssl)
		    (save-excursion
		      (forward-word -1)
		      (or
		       (m3::keyword-before-ssl-introducer-p "WHILE")
		       (m3::keyword-before-ssl-introducer-p "WITH")
		       (m3::keyword-before-ssl-introducer-p "FOR")
		       (m3::keyword-before-ssl-introducer-p "LOCK"))))))

    ("ELSE" . (2 (ls-ok) (lambda (on-lm starts-ssl)
			   (or (m3::end-matcher-is-p "IF")
			       (m3::end-matcher-is-p "TRY")
			       (m3::end-matcher-is-p "\\bCASE")
			       (m3::end-matcher-is-p "\\bTYPECASE")))))
    ("ELSIF" . (3 (ls-ok) (lambda (on-lm starts-ssl)
			    (m3::end-matcher-is-p "IF"))))
    ("END" . (1 (lm-ok ls-ok)))
    ("EVAL" . (7 (ls-only)))
    ("EXCEPT" . (6 (ls-ok) (lambda (on-lm starts-ssl)
			     (m3::end-matcher-is-p "TRY"))))
    ("EXCEPTION" . (5 (lm-only ls-ok)))
    ("EXIT" . (8 (ls-only)))
    ("EXPORTS"  . (4 () (lambda (on-lm starts-ssl)
			  (save-excursion
			    ;; One for prefix of EXPORTS one for module name,
			    ;; one for MODULE.
			    (forward-word -3)
			    (looking-at "MODULE")))))

    ("FALSE" . (4 ()))
    ("FINALLY" . (3 (ls-ok) (lambda (on-lm starts-ssl)
			      (m3::end-matcher-is-p "TRY"))))
    ("FIRST" . (5 ()))
    ("FLOAT" . (6 ()))
    ("FLOOR" . (7 ()))
    ("FOR" . (2 (ls-ok)))
    ("FROM" . (1 (lm-only ls-ok)))

    ("GENERIC" . (1 (lm-only)))

    ("IMPORT"  . (2 (lm-ok ls-ok)
		    (lambda (on-lm starts-ssl)
		      (or on-lm
			  (save-excursion
			    (forward-word -3)
			    (looking-at "\\bFROM\\b"))))))
    ("IF" . (3 (ls-only)
	       (lambda (on-lm starts-ssl)
		 (or (not starts-ssl)
		     (save-excursion
		       (forward-word -3)
		       (not (looking-at "\\(\\bARRAY\\|\bSET\\)[ \t]+OF")))))))
    ("IN" . (7 ()))
    ("INC" . (4 (ls-only)
		(lambda (on-lm starts-ssl)
		  (or (not starts-ssl)
		      (save-excursion
			(forward-word -3)
			(not (looking-at
			      "\\(\\bARRAY\\|\bSET\\)[ \t]+OF")))))))
    ("INTEGER" . (5 (ls-ok)
		    (lambda (on-lm starts-ssl)
		      (or (not starts-ssl)
			  (save-excursion
			    (forward-word -2)
			    (looking-at "OF"))))))
    ("INTERFACE" . (1 (lm-ok) (lambda (on-lm starts-ssl)
				(save-excursion
				  (or on-lm
				      (progn
					(forward-word -2)
					(and
					 (m3::at-left-margin-p)
					 (looking-at "GENERIC\\|UNSAFE"))))))))
    ("ISTYPE" . (7 ()))

    ("LAST" . (3 ()))
    ("LOCK" . (1 (ls-only)
		 (lambda (on-lm starts-ssl)
		   (save-excursion (forward-word -2)
				   (not (looking-at "OF"))))))
    ("LOOP" . (2 (ls-only)
		 (lambda (on-lm starts-ssl)
		   (save-excursion (forward-word -2)
				   (not (looking-at "OF"))))))
    ("LONGFLOAT" . (4 ()))
    ("LONGREAL" . (5 (ls-of)))
    ("LOOPHOLE" . (6 ()))

    ("MAX" . (5 ()))
    ("METHODS" . (2 (ls-only)))
    ("MIN" . (4 ()))
    ("MOD" . (3 ()))
    ("MODULE" . (1 (lm-ok)
		   (lambda (on-lm starts-ssl)
		     (save-excursion
		       (forward-word -1)
		       (or (m3::at-left-margin-p)
			   (progn
			     (forward-word -1)
			     (and (m3::at-left-margin-p)
				  (looking-at "GENERIC\\|UNSAFE"))))))))

    ("NARROW" . (1 ()))
    ("NEW" . (2 ()))
    ("NIL" . (3 ()))
    ("NULL" . (6 ()))
    ("NUMBER" . (5 ()))
    ("NOT" . (4 ()))

    ("OBJECT" . (2 ()
		   (lambda (on-lm starts-ssl)
		     (save-excursion
		       (m3::re-search-backward m3::part-starters (point-min) t)
		       (looking-at "TYPE\\|REVEAL")))))
    ("OF" . (1 () (lambda (on-lm starts-ssl)
		    (or (m3::keyword-before-ssl-introducer-p
			 "\\bCASE\\|\\bTYPECASE")
			(m3::keyword-before-ssl-introducer-p
			 "\\bARRAY\\|SET\\b")))))
    ("OR" . (4 ()))
    ("ORD" . (5 ()))
    ("OVERRIDES" . (3 (ls-only)))

    ("PROCEDURE" . (1 (lm-ok ls-ok)))

    ("RAISE" . (5 (ls-only)))
    ("RAISES" . (3 () m3::raises-ok))
    ("READONLY" . (4 (ls-ok) (lambda (on-lm starts-ssl)
			  (m3::in-arg-list 0))))
    ("REAL" . (9 (ls-of)))
    ("RECORD" . (6 ()))
    ("REF" . (7 ()))
    ("REFANY" . (8 ()))
    ("REPEAT" . (10 (ls-only)))
    ("RETURN" . (2 (ls-only)))
    ("REVEAL" . (1 (lm-only ls-ok)))
    ("ROOT" . (11 ()))
    ("ROUND" . (12 ()))

    ("SET" . (1 ()))
    ("SUBARRAY" . (2 (ls-ok)))

    ("TEXT" . (6 (ls-of)))
    ("THEN" . (1 () (lambda (on-lm starts-ssl)
		      (or (m3::keyword-before-ssl-introducer-p "\\bIF")
			  (m3::keyword-before-ssl-introducer-p "\\bELSIF")))))
    ("TO" . (2 () (lambda (on-lm starts-ssl)
		    (m3::keyword-before-ssl-introducer-p "\\bFOR"))))
    ("TRUE" . (8 ()))
    ("TRUNC" . (9 ()))
    ("TRY" . (3 (ls-only)))
    ("TYPE" . (4 (lm-ok ls-ok)))
    ("TYPECASE" . (5 (ls-only)))
    ("TYPECODE" . (7 ()))

    ("UNSAFE" . (1 (lm-only)))
    ("UNTIL" . (2 (ls-ok)))
    ("UNTRACED" . (3 ()))

    ("VAL" . (2 () (lambda (on-lm starts-ssl)
		     (and (not (save-excursion
				 (forward-word -1)
				 (m3::after-procedure-introducer 0)))
			  (not (m3::in-arg-list 0))))))

    ("VALUE" . (3 ()
		  (lambda (on-lm starts-ssl)
		    (not (save-excursion
			   (forward-word -1)
			   (m3::after-procedure-introducer 0))))))

    ("VAR" . (1 (lm-ok ls-ok)
		(lambda (on-lm starts-ssl)
		  (or on-lm starts-ssl
		      (save-excursion
			(forward-word -1)
			(m3::after-procedure-introducer 0))
		      (m3::in-arg-list 0)))))

    ("WHILE" . (1 (ls-only)))
    ("WITH" . (2 (ls-only)))))



(defun m3::at-left-margin-p () (eq (current-column) 0))

(defun m3::keyword-before-ssl-introducer-p (keyword)
  "Returns non-nil if KEYWORD occurs before an ssl-introducer (other than
KEYWORD), looking backward."
  (save-excursion
    (m3::re-search-backward
     (concat "\\(;\\|\\bEND\\b\\|" m3::keyword-endable-ssl-introducers "\\|"
	     keyword "\\)")
     (point-min) 't)
    (looking-at keyword)))
      
(defun m3::end-matcher-is-p (keyword)
  "Returns non-nil if the keyword that would match an END inserted at
point is KEYWORD."
  (save-excursion
    (m3::backward-to-end-match (point-min))
    (looking-at keyword)))

(defun m3::raises-ok (on-lm starts-ssl)
  (save-excursion
    (forward-word -1)
    (let ((save-point (point)))
      (and
       (m3::re-search-backward "[^*])" 0 t)
       (progn
	 (forward-char 1)
	 (and
	  (m3::in-arg-list 0)
	  (progn
	    (forward-char 1)
	    (let ((retval-pat
		   (concat "[ \t\n]*:[ \t\n]*" m3::poss-qual-ident-re)))
	      (if (looking-at retval-pat)
		  (progn
		    (re-search-forward retval-pat)
		    (goto-char (match-end 0))))
	      (m3::forward-to-code (point-max))
	      (= (point) save-point)))))))))
	    

(defun m3::complete-abbrev-work (pw)
;;;  (message "In m3::polite-abbrev") (sit-for 2)
  (let ((case-fold-search nil))
    (cond
     ;; First, if the start of the current keyword is the same as the
     ;; start of the last keyword we completed, and the user hasn't
     ;; appended any characters, and m3::cur-keyword-completions is non-nil,
     ;; try the next completion in the list.
     ((and
;;;     (progn (message "In m3::polite-abbrev (x1)") (sit-for 2) t)
       (marker-position m3::cur-keyword-completion-start)
;;;     (progn (message "In m3::polite-abbrev (x2)") (sit-for 2) t)
       (> (point) m3::cur-keyword-completion-len)
       (= m3::cur-keyword-completion-start
	  (save-excursion
	    (forward-char (- m3::cur-keyword-completion-len))
	    (point)))
;;;     (progn (message "In m3::polite-abbrev (x3)") (sit-for 2) t)
       m3::cur-keyword-completions
       (string-equal (buffer-substring
		      (marker-position m3::cur-keyword-completion-start)
		      (point))
		     (car m3::cur-keyword-completions)))
      (let ((cur-completion (car m3::cur-keyword-completions)))
	(setq m3::cur-keyword-completions
	      (append (cdr m3::cur-keyword-completions) (list cur-completion)))
;;;      (progn (message "In m3::polite-abbrev (xx1)") (sit-for 2) t)
	(forward-word -1)
	(delete-region m3::cur-keyword-completion-start
		       (+ m3::cur-keyword-completion-start
			  m3::cur-keyword-completion-len))
;;;      (progn (message "In m3::polite-abbrev (xx2)") (sit-for 2) t)
	(insert (car m3::cur-keyword-completions))
	(setq m3::cur-keyword-completion-len
	      (- (point) m3::cur-keyword-completion-start))
	(if (> (length m3::cur-keyword-completions) 1)
	    (message "Other matches: %s"
		     (mapconcat '(lambda (x) x)
				(cdr m3::cur-keyword-completions) ", ")))))

     ;; Otherwise, form the list of (<keyword> . <score>) pairs such
     ;; that pw is a prefix of <keyword>, <score> is the score
     ;; associated with <keyword> in m3::keyword-completions, and the
     ;; conditions in m3::keyword-completions are met.
     (t
;;;    (message "In m3::polite-abbrev (t)") (sit-for 2)
      (let ((keyword-list m3::keyword-completions)
	    matches
	    (on-lm
	     (and
	      (= (save-excursion (forward-word -1) (current-column))
		 0)
	      (let ((continue t) (res nil))
		(save-excursion
;;;		  (message "Checking on-lm, about to enter loop") (sit-for 2)
		  (while continue
		    (setq continue nil)
;;;		    (message "Checking on-lm, before search") (sit-for 2)
		    (m3::re-search-backward
		     (concat m3::part-starters "\\|" m3::end-matchers "\\|"
			     "\\bEND\\b")
		     (point-min) 'move-to-limit)
;;;		    (message "Checking on-lm, after search") (sit-for 2)
		    (cond
		     ((looking-at "\\bEND\\b")
		      (m3::backward-to-end-match (point-min))
		      (if (and (looking-at "BEGIN")
			       (not (looking-at "^BEGIN")))
			  (progn
;;;			    (message "Checking doing BEGIN adjustment")
;;;			    (sit-for 2)
			    (m3::re-search-backward
			     "\\(^PROCEDURE\\|^[ \t]+BEGIN\\)"
			     (point-min) 'move-to-limit)
			    (goto-char (match-end 0))))
		      (setq continue t))
		     ((looking-at (concat "^\\(" m3::part-starters "\\)"))
		      (setq res t))
		     ((looking-at "IMPORT")
		      (save-excursion
			(forward-word -2)
;;;			(message "Doing FROM ... IMPORT special") (sit-for 2)
			(if (looking-at "^FROM")
			    (setq res t))))
		     ((= (point) (point-min))
		      (setq res t)))))
;;;		(message "After loop, res is %s" res) (sit-for 2)
		(and res
		     (save-excursion
		       (forward-word -1)
		       (m3::backward-to-code (point-min))
		       (or (= (point) (point-min))
;;;			   (progn (message "xxx") (sit-for 2) nil)
			   (looking-at ";")))))))
	    (starts-ssl
	     (let ((first-char (save-excursion (forward-word -1) (point))))
	       (save-excursion
		 (forward-word -1)
		 (m3::re-search-backward
		  (concat
		   "\\(;\\|\\bEND\\b\\|"
		   m3::keyword-endable-ssl-introducers "\\)")
		  (point-min) 'move-to-limit)
		 (re-search-forward
		  (concat
		   "\\(;\\|\\bEND\\b\\|"
		   m3::keyword-endable-ssl-introducers "\\)")
		  first-char t)
		 (goto-char (match-end 0))
;;;	       (message "In m3::polite-abbrev (zz1)") (sit-for 2)
		 (m3::forward-to-code (point-max))
		 (= (point) first-char))))
	    (after-of
	     (save-excursion (forward-word -2) (looking-at "OF"))))
;;;	(message
;;;	 "In m3::polite-abbrev, on-lm = %s, starts-ssl = %s, after-of = %s."
;;;	 on-lm starts-ssl after-of)
;;;	(sit-for 2)

	(while keyword-list
	  (let* ((entry (car keyword-list))
		 (kw (car entry)))
;;;	  (message "In m3::polite-abbrev kw = %s" kw) (sit-for 2)
;;;	  (message "Foo") (sit-for 2)
	    (if (m3::is-prefix kw pw)
		(let* ((rest (cdr entry))
		       (score (car rest))
		       (props (car (cdr rest)))
		       (pred (car (cdr (cdr rest)))))
;;;		  (message "In m3::polite-abbrev, found kw = %s" kw) (sit-for 1)
		  (let ((lm-status
			 (cond
			  ((and (memq 'lm-ok props) (memq 'lm-only props))
			   (error "Bad prop-list in m3::keyword-completions."))
			  ((memq 'lm-ok props) 'lm-ok)
			  ((memq 'lm-only props) 'lm-only)
			  (t 'lm-not)))
			(ls-status
			 (cond
			  ((let ((n 0))
			     (if (memq 'ls-ok props) (setq n (+ n 1)))
			     (if (memq 'ls-only props) (setq n (+ n 1)))
			     (if (memq 'ls-of props) (setq n (+ n 1)))
			     (> n 1))
			   (error "Bad prop-list in m3::keyword-completions."))
			  ((memq 'ls-ok props) 'ls-ok)
			  ((memq 'ls-only props) 'ls-only)
			  ((memq 'ls-of props) 'ls-of)
			  (t 'ls-not))))
;;;		    (message
;;;		     "In m3::polite-abbrev, (2) lm-status = %s ls-status = %s"
;;;		     lm-status ls-status)
;;;		    (sit-for 2)
		    (and
		     (or (eq lm-status 'lm-ok)
			 (cond
			  ((eq lm-status 'lm-only) on-lm)
			  ((eq lm-status 'lm-not) (not on-lm))))
		     (or
;;;		    (progn (message "In m3::polite-abbrev, (3.2)")
;;;			   (sit-for 2) nil)
		      (and (eq ls-status 'ls-ok) (not after-of))
		      (cond
		       ((eq ls-status 'ls-only) (and starts-ssl (not after-of)))
		       ((eq ls-status 'ls-not) (not starts-ssl))
		       ((eq ls-status 'ls-of) (or (not starts-ssl) after-of))))

		     (or 
;;;		    (progn (message "In m3::polite-abbrev, (5), pred = %s" pred)
;;;			   (sit-for 2) nil)
		      (not pred)
;;;		    (progn (message "In m3::polite-abbrev, (5)")
;;;			   (sit-for 2) nil)
		      (funcall pred on-lm starts-ssl))
;;;		   (message "In m3::polite abbrev, adding %s to matches" kw)
;;;		   (sit-for 2)
		     (setq matches (cons (cons kw score) matches)))))))
	  (setq keyword-list (cdr keyword-list)))

;;;   (message "In m3::polite-abbrev (after matches): %s" matches) (sit-for 4)
	;; If there are any matches, do a completion
	(and matches
	     (progn
	       ;; Now sort matches according to score.
;;;	     (message "In m3::polite-abbrev, (10)") (sit-for 2)
	       (setq matches
		     (sort matches '(lambda (e1 e2) (< (cdr e1) (cdr e2)))))
	       ;; And strip off the scores from the result.
;;;	     (message "In m3::polite-abbrev, (11)") (sit-for 2)
	       (setq matches (mapcar'(lambda (e) (car e)) matches))
;;;	     (message "In m3::polite-abbrev, (12)") (sit-for 2)
	       (setq m3::cur-keyword-completions matches)
	       (let ((first-match (car matches)))
		 (forward-word -1)
		 (delete-region (point)
				(save-excursion (forward-word 1) (point)))
;;;	       (message "In m3::polite-abbrev, (13)") (sit-for 2)
		 (set-marker m3::cur-keyword-completion-start (point))
		 (insert first-match)
		 (setq m3::cur-keyword-completion-len
		       (- (point) m3::cur-keyword-completion-start))
		 (if (> (length matches) 1)
		     (message
		      "Other matches: %s"
		      (mapconcat '(lambda (x) x) (cdr matches) ", ")))))
	     ))))))


;;;======================================================================

(defun m3::is-letter (ch)
  "checks if argument is a letter."
  (and (>= (upcase ch) ?A) (<= (upcase ch) ?Z)))

(defun m3::abbrev-and-or-indent ()
  "If preceding char in buffer is letter, tries to expand abbrev.
Otherwise, indents the current line."
  (interactive)
;;;  (message "Foo1") (sit-for 2)
  (if (and m3::abbrev-enabled
	   (or (m3::is-letter (preceding-char))
	       (save-excursion
		 (and
		  (> (point) 2)
		  (progn
		    (forward-char -2)
		    (and
		     (looking-at "*)")
		     (progn (forward-word -1) (forward-char -3)
			    (looking-at "(*"))
		     (progn (forward-word -1) (looking-at "\\bEND\\b"))))))
	       (save-excursion
		 (and
		  (> (point) 2)
		  (progn
		    (forward-char -1)
		    (and
		     (looking-at ";\\|.")
		     (progn (forward-word -2) (looking-at "\\bEND\\b")))))))
	   (or (eq (point) (point-max))
	       (eq (following-char) ?\ )
	       (eq (following-char) ?\t)
	       (eq (following-char) ?\n)))
      (progn (m3::complete-abbrev)
	     (m3::indent-line))
    (m3::indent-line)))


;;; ----------------- M3PP pretty printing ------------------

(defvar m3::pp-options '("-ZZ")
  "Command line options that should be passed to m3pp when it is started up.")

(defvar m3::pp-modunit "\002")
(defvar m3::pp-defunit "\005")
(defvar m3::pp-endunit "\001")

(defvar m3::pp-process nil)
(defvar m3::pp-in-progress nil)

(defvar m3::pp-unit-boundary
      (concat "^[ \t]*\nCONST\\|" 
              "^[ \t]*\nTYPE\\|"
              "^[ \t]*\nVAR\\|"
              "^[ \t]*\nPROCEDURE\\|"
              "^[ \t]*\nEXCEPTION\\|"
	      "^[ \t]*\n<\*EXTERNAL\*>|"
	      "^[ \t]*\n<\*INLINE\*>|"
              "^[ \t]*\nMODULE\\|"
	      "^[ \t]*\nINTERFACE\\|"
	      "^[ \t]*\nIMPORT\\|"
              "^[ \t]*\nBEGIN"))

(defun m3::pp-startup ()
  (if (not (and m3::pp-process
		(process-status (process-name m3::pp-process))))
      (save-excursion 
	(get-buffer-create "m3::pp")
	(set-buffer "m3::pp")
	(erase-buffer)
	(setq m3::pp-process 
	      (apply 'start-process "m3::pp" nil "m3pp" m3::pp-options))
	(process-kill-without-query m3::pp-process)
	(set-process-filter m3::pp-process 'm3::pp-filter)
	(process-send-string m3::pp-process 
			     (concat m3::pp-modunit m3::pp-endunit "\n"))
	(accept-process-output m3::pp-process))))

(defun m3::pp-buffer ()
  (interactive)
  (m3::pp-region-safe (point-min) (point-max) (point)))

(defun m3::pp-unit ()
  "Pretty prints the 'unit' containing the cursor. 
   A unit starts with a blank line followed by CONST, TYPE, VAR, 
   PROCEDURE, EXCEPTION, IMPORT, FROM, MODULE, or BEGIN, and it extends 
   to the start of the next unit.  If there is no such unit around the
   cursor, the entire file is pretty printed."
  (interactive)
  (let ((return-point (point)) start end)
    (save-excursion
      (if (not (looking-at
		(concat "^\\(" m3::part-starters "\\)")))
	  (m3::beginning-of-defun 1))
      (setq start (point))
;;;      (message "Unit start...") (sit-for 2)
      (m3::end-of-defun 1)
;;;      (message "Unit end.") (sit-for 2)
      (setq end (point)))
    (m3::pp-region-safe start end return-point)))

(defun m3::pp-region ()
  "Pretty prints the region. 
   The region should consist of zero or more declarations, definitions, 
   import statements, or modules."
  (interactive)
  (m3::pp-region-safe (min (point) (mark)) (max (point) (mark)) (point)))


(defun m3::pp-region-safe (start end return-point)
;;;  (message "m3::pp-region-safe (1) rt = %d" return-point) (sit-for 2)
  (let ((m3pp-type nil)
	(m3pp-start nil))
    (m3::pp-startup)
;;;    (message "m3::pp-region-safe (2)") (sit-for 2)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward m3::pp-endunit (point-max) t)
	  (error "m3pp: file mustn't contain ^A"))
      (get-buffer-create "m3::pp-output")
      (set-buffer "m3::pp-output")
      (erase-buffer))
;;;    (message "m3::pp-region-safe (3)") (sit-for 2)
    (if (buffer-file-name)
	(let* ((len (length (buffer-file-name)))
	       (tail (substring (buffer-file-name) (- len 3) len)))
	  (cond
	   ((or (string-equal tail ".m3") (string-equal tail ".mg"))
	    (setq m3pp-type m3::pp-modunit))
	   ((or (string-equal tail ".i3") (string-equal tail ".ig"))
	    (setq m3pp-type m3::pp-defunit))
	   (t
	    (error "m3pp: pretty-print only .m3, .mg, .i3, or .ig files"))))
      (save-excursion
	(goto-char (point-min))
	(m3::forward-to-code (point-max))
	(cond
	 ((looking-at "INTERFACE")
	  (setq m3pp-type m3::pp-defunit))
	 ((looking-at "MODULE")
	  (setq m3pp-type m3::pp-modunit))
	 ((looking-at "\\(GENERIC\\|UNSAFE\\)")
	  (forward-word 1)
	  (m3::forward-to-code (point-max))
	  (cond
	   ((looking-at "INTERFACE")
	    (setq m3pp-type m3::pp-defunit))
	   ((looking-at "MODULE")
	    (setq m3pp-type m3::pp-modunit))
	   (t
	    (error "m3pp: buffer is not an interface or module.")))))))

    (message "m3pp: working ...")
    (setq m3::pp-in-progress t)
    (cond
     ;; Empirically, this number seems to work; lengths over 8000 seem
     ;; to get hung up somewhere when using process-send-string.
     ((> (- end start) 4000)
      (let* ((num (mod (random) 1000))
	     (fn-in (concat "/tmp/" (getenv "USER") "-m3pp-in-"
			    (format "%d" num)))
	     (fn-out (concat "/tmp/" (getenv "USER") "-m3pp-out-"
			     (format "%d" num))))
;;;	(message "random-filename is %s" fn) (sit-for 1)
	(goto-char end) (insert "")
	(goto-char start) (insert "")
	(write-region start end fn-in nil 'no-msg)
	(goto-char start) (delete-char 1)
	(goto-char end) (delete-char 1)
	(let ((cur-buffer (current-buffer)))
	  (get-buffer-create "m3::pp-output")
	  (set-buffer "m3::pp-output")
	  (shell-command (concat "m3pp -ZZG < " fn-in " > " fn-out))
	  (insert-file fn-out)
	  (set-buffer cur-buffer))))
     (t
      (process-send-string 
       m3::pp-process
       (concat m3pp-type (buffer-substring start end) m3::pp-endunit "\n"))
      (while m3::pp-in-progress
	(accept-process-output m3::pp-process))))
;;;    (setq m3::pp-start (point-marker))
    (kill-region start end)
    (insert-buffer "m3::pp-output")
    (save-excursion
      (set-buffer "m3::pp-output")
      (if (re-search-backward "(\\* SYNTAX ERROR " (point-min) t)
	  (progn
	    (beep)
	    (message "m3pp: syntax error"))
	(progn ;else
	  (message "m3pp: done"))))
    (goto-char return-point)))

;;    (if (not (pos-visible-in-window-p))
;;	(let ((dotval (+ (point-marker))))
;;	  (line-to-bottom-of-window)
;;	  (goto-char dotval)))))

(defun m3::pp-filter (&process &str)
  (save-excursion
    (get-buffer-create "m3::pp-output")
    (set-buffer "m3::pp-output")
    (goto-char (point-max))
    (insert &str)
    (if (search-backward m3::pp-endunit (point-min) t) 
	(progn
	  (delete-char 2)
	  (setq m3::pp-in-progress nil)))))

(defun m3::pp-find-format-unit ()
;;;  (message "Beginning of format region") (sit-for 2)
  (set-mark (point))
  (m3::end-of-defun 1)
;;;  (message "End of format region") (sit-for 2)
  (exchange-point-and-mark)
  nil)

;;;----------- SRC-specific (but adaptable) stuff ----

(defvar m3::path-alist nil
  "Alist with entries of the form:
   <DIRECTORY, M3MAKEFILE-MOD-DATE, PATH-AS-DIR-LIST>
   If DIRECTORY has an entry on this list, its m3makefile has been processed
to yield the path PATH-AS-DIR-LIST at a time in the past when the modification
time of the m3makefile was M3MAKEFILE-MOD-DATE.  If the m3makefile has not
been modified since then, it is safe to use the cached path.")

(defvar m3::path-default nil
  "The search path corresponding to the 'be' directory.")

(defvar m3::path-default-time nil
  "The modification date of the default m3path file when it was last read.")

(defvar m3::derived-dir "DS"
  "Subdirectories into which emacs assumes m3build will put derived files.")

(defun m3::read-path ()
  "Assumes that the current directory is a (possibly non-proper) subdirectory
of the src directory of the current package, that that src directory
contains the m3makefile for the package, and that the package contains
one subdirectory named src, and it is an immediate subdirectory
of the package directory.  Constructs and returns the search path associated
with that m3makefile, if this m3makefile exists; otherwise returns NIL.  May
do caching based on the modification time of the m3makefile."
  ;; First, find the src directory.
  (let ((old-dd default-directory))
    (when (not (m3::find-main-src-dir))
      (setq default-directory old-dd)
      (error "Unable to find main src directory."))
    (let ((entry (assoc default-directory m3::path-alist)))
      (cond
       (entry
	(let ((imp-tab-name (concat "../" m3::derived-dir "/.M3IMPTAB")))
	  (cond
	   ((file-exists-p imp-tab-name)
	    (let ((mod-time (m3::get-mod-time imp-tab-name)))
	      ;; Do we have this cached?
	      (cond
	       ((let ((cached-date (cadr entry)))
		  (m3::time-le mod-time cached-date))
		;; we got a cache hit that is still valid.
		(setq default-directory old-dd)
		(nth 2 entry))
	       (t
		;; Cache entry was invalid.  Update it.
		(m3::update-m3-alist)
		(setq entry (assoc default-directory m3::path-alist))
		(setq default-directory old-dd)
		(nth 2 entry)))))
	   (t
	    (message "%s file is no longer-present.")))))
       (t nil)))))

(defun m3::find-main-src-dir ()
  "Moves the current directory to the main 'src' directory of the current
package, if it can find it.  Returns non-nil iff it finds one."
  (while 
      (and (not (string=
		 (file-name-nondirectory (substring default-directory 0 -1))
		 "src"))
	   (not (string= default-directory "/")))
    (cd ".."))
  (and (not (string= default-directory "/"))
       ;; We found the src directory.  Make sure.
       (if (not (string= (file-name-nondirectory
			  (substring default-directory 0 -1))
			 "src"))
	   (error "INTERNAL")
	 t)))

(defun m3::search-for-pkg (off)
  "Asserts that the current directory has special imports not covered by the
default, so we should use parse and use a directory-specific search-path
for it.  With a prefix-argument, removes search-path entry for current package."
  (interactive "P")
  (let ((old-dd default-directory))
    (when (not (m3::find-main-src-dir))
      (setq default-directory old-dd)
      (error "Unable to find main src directory."))
    (cond
     (off
      (delete-if '(lambda (elem) (string= (car elem) default-directory))
		 m3::path-alist))
     (t
      (when (not (assoc default-directory m3::path-alist))
	(setq m3::path-alist (cons (list default-directory '(0 0) nil)
				   m3::path-alist)))))))

(defun m3::update-m3-alist ()
  "m3::path-alist has an out-of-date entry for the current directory.
Make that entry valid again.  Requires that we're in the main 'src'
directory of the current package, and that the appropriate .M3IMPTAB
exists."
  (setq m3::path-alist
	(delete-if '(lambda (elem) (string= (car elem) default-directory))
		   m3::path-alist))
  ;; Find the .M3IMPTAB for the current directory, if it exists.
  (let ((imp-tab-name (concat "../" m3::derived-dir "/.M3IMPTAB")))
    (if (not (file-exists-p imp-tab-name)) (error "INTERNAL"))
    (message "Reading %s for search path..." imp-tab-name)
    (let ((sav-buffer (current-buffer))
	  (m3path-buffer (create-file-buffer "m3path"))
	  (path nil))
      (set-buffer m3path-buffer)
      (delete-region (point-min) (point-max))
      (shell-command-on-region
       (point-min) (point-max)
       (concat "grep '^@.*$' " imp-tab-name " | sed -e 's/^@//'")
       t)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(let ((dir (buffer-substring 
		    (point) (progn (end-of-line nil) (point)))))
	  (if (> (length dir) 0)
	      (setq path (cons dir path)))
	  (forward-line 1)))
      (set-buffer sav-buffer)
      (setq m3::path-alist
	    (cons (list default-directory (m3::get-mod-time imp-tab-name) path)
		  m3::path-alist)))))

(defun m3::get-mod-time (fn)
  "Assumes fn exists; returns modification date as two-element-list."
  (let ((attrs (file-attributes fn))) (nth 5 attrs)))

(defun m3::time-le (t1 t2)
  "t1 and t2 are times represented as two-element lists of (16 bit) integers.
Returns non-NIL iff t1 <= t2."
  (or (< (car t1) (car t2))
      (and (= (car t1) (car t2))
	   (<= (cadr t1) (cadr t2)))))
    
(defconst m3::be-pkg "/proj/m3/pkg/be/")

(defun m3::read-default-path ()
  "Ensures that m3::path-default has an up-to-date value."
  (let ((fn (concat m3::be-pkg m3::derived-dir "/m3path")))
    (cond
     ((file-exists-p fn)
      (let ((tm (m3::get-mod-time fn)))
	(when (or (not m3::path-default-time)
		  (not (m3::time-le tm m3::path-default-time)))
	  (message "Reading default m3path...")
	  (save-excursion
	    (let ((sav-buffer (current-buffer))
		  (path nil))
	      (find-file-read-only fn)
	      (goto-char (point-min))
	      (while (< (point) (point-max))
		(let ((dir (buffer-substring 
			    (point) (progn (end-of-line nil) (point)))))
		(if (> (length dir) 0)
		    (setq path (cons dir path)))
		(forward-line 1)))
	      (let ((m3path-buffer (current-buffer)))
		(set-buffer sav-buffer)
		(kill-buffer m3path-buffer)
		(setq m3::path-default (cons "." path))
		(setq m3::path-default-time tm)))))))
     (t
      (message "Default m3path file '%s' does not exist..." fn)
      ))))

;;; stolen from lib-complete, 
;;; Author          : Mike Williams <mike-w@cs.aukuni.ac.nz>
;;; Created On      : Sat Apr 20 17:47:21 1991
;;; Last Modified By: Mike Williams
;;; Last Modified On: Tue Jun 18 12:53:08 1991

(defun m3::locate-file (FILE SEARCH-PATH &optional SUFFIX-LIST PRED)
  "Search for FILE on SEARCH-PATH (list).  If optional SUFFIX-LIST is
provided, allow file to be followed by one of the suffixes.
Optional second argument PRED restricts the number of files which
may match.  The default is file-exists-p."
  (if (not SUFFIX-LIST) (setq SUFFIX-LIST '("")))
  (if (not PRED) (setq PRED 'file-exists-p))
  (if (file-name-absolute-p FILE) (setq SEARCH-PATH '(nil)))
  (if (equal FILE "") (error "Empty filename"))
  (let ((filelist 
	 (mapcar 
	  (function (lambda (ext) (concat FILE ext)))
	  SUFFIX-LIST)))
    ;; Search SEARCH-PATH for a readable file in filelist
    (catch 'found
      (while SEARCH-PATH
	(let ((filelist filelist))
	  (while filelist
	    (let* ((expanded (expand-file-name (car filelist)
					       (car SEARCH-PATH)))
		   (filepath (substitute-in-file-name expanded)))
	      (if (funcall PRED filepath)
		  (throw 'found filepath)))
	    (setq filelist (cdr filelist))))
	(setq SEARCH-PATH (cdr SEARCH-PATH))))
    ))

(defvar m3::show-file-other-frame t
  "If non-nil and using emacs19, files found using
m3::show-interface or m3::show-implementation will be displayed on
new screens.") 

(defun m3::show-interface (&optional arg)
  "Find a Modula-3 interface. 
If ARG is a string, it is the name of the interface.  If ARG is nil,
get the name from the text around the point.  Otherwise, ARG should be 
an epoch mouse position and the name is found around that position.
If the current directory has an 'm3path' file, reads that to get a
search path; otherwise, uses m3::path.  Then find the file that
contains that interface.  Under gnuemacs, or if using epoch and
m3::show-interface-other-frame is nil, show the interface in another
window of the current screen.  If using epoch and
m3::show-interface-other-frame is non-nil, show the interface in a
new screen of the Modula-3 pool; the screens in that pool are in the
class m3::poolclass. The Modula-3 pool is of size m3::poolsize."
  (interactive)
  (let ((interface (if (stringp arg) arg (m3::ident-around-point))))
    (m3::show-file-work interface 'interface)))

(defun m3::show-spec (&optional arg)
  "Find a Modula-3 spec file."
  (interactive)
  (let ((interface (if (stringp arg) arg (m3::ident-around-point))))
    (m3::show-file-work interface 'specification)))

(defvar m3::trait-path
  '("." "/udir/horning/TheBook/handbook" "/proj/m3/pkg/lm3-traits/traits"
    "/udir/kjones/larch/LM3/pkg/TEST")
  "The list of directories to search for lsl files...")
  
(defun m3::show-trait (&optional arg)
  "Find an LSL trait file."
  (interactive)
  (let* ((trait (if (stringp arg) arg (m3::ident-around-point)))
	 (file (m3::locate-file (concat trait ".lsl") m3::trait-path)))
    (if file
	(m3::show-file file)
      (error "Unable to locate trait %s." trait))))

(defun m3::ident-around-point ()
  (save-excursion
    (let (end)
      (re-search-forward "[^A-Za-z0-9_]" nil t)
      (backward-char)
      (setq end (point))
      (re-search-backward "[^A-Za-z0-9_]" nil t)
      (forward-char)
      (buffer-substring (point) end))))


(defun m3::show-file-work (interface kind)
  (m3::read-default-path)
  (let ((path (m3::read-path)))
    (message "Searching for file...")
    (cond
     ((eq kind 'interface)
      (setq filename
	    (or (m3::locate-file interface path '(".i3" ".ig"))
		(m3::locate-file interface m3::path-default '(".i3" ".ig"))
		)))
     ((eq kind 'specification)
      (setq filename
	    (or (m3::locate-file (concat interface ".lm3")  path)
		(m3::locate-file (concat interface ".lm3")  m3::path-default)
		))))
    (if (not filename)
	(message "Unable to locate %s '%s'" kind interface)
      (message "found.")
      (m3::show-file filename))))


(defun m3::show-file (filename)
  (cond 
   ((and m3::show-file-other-frame (fboundp 'find-file-other-frame))
    (find-file-other-frame filename))
   (t
    (find-file-other-window filename))))

(defun m3::show-implementation ()
  "If the current buffer contains an interface file, attempts to find
the implementation of that interface in the same directory as the
interface, and displays that file if it is found.  If using epoch
and m3::show-file-other-frame is non-nil, displays the file in a new
screen."
  (interactive)
  (let* ((bfn (buffer-file-name))
	 (ext (m3::get-extension (file-name-nondirectory bfn))))
    (if (and bfn
	     (or (equal ext ".i3") (equal ext ".ig")))
	(progn
	  ;; First, find the true directory of the file.
	  (let* ((true-name (m3::file-true-name bfn))
		 (true-dir (file-name-directory true-name))
		 (interface-name (m3::strip-extension
				  (file-name-nondirectory bfn)))
		 (save-buffer (current-buffer))
		 (new-buffer (get-buffer-create "*implementation*"))
		 (impl-name (concat true-dir interface-name
				    (if (equal ext ".i3")
					".m3"
				      ".mg"))))
	    (if (not (file-exists-p impl-name))
		(if (not (equal ext ".i3"))
		    (setq impl-name nil)
		  (save-excursion
		    (setq impl-name nil)
		    (set-buffer new-buffer)
		    (delete-region (point-min) (point-max))
		    (let ((grep-cmd
			   (concat "cd " true-dir ";"
				   "egrep -l "
				   "'(MODULE +" interface-name ")|"
				   "(MODULE +[a-zA-Z_][a-zA-Z_0-9]* +EXPORTS +"
				   "([a-zA-Z_][a-zA-Z_0-9]*, *)*"
				   interface-name ")' *.m3")))
		      (message "Searching for exporter of %s..." interface-name)
		      (shell-command-on-region (point-min) (point-min)
					       grep-cmd t)
		      (message "done."))
		    (goto-char (point-min))
		    (if (> (point-max) (point-min))
			(progn
			  (setq impl-name (buffer-substring
					   (point-min)
					   (save-excursion
					     (end-of-line nil) (point))))
			  (message "Implementation is %s." impl-name))))))
	    (if (not impl-name)
		(message "Implementation of %s not found in directory %s."
			 interface-name true-dir)
	      (m3::show-file (concat true-dir impl-name)))))
      (message "Current file does not appear to be an interface."))))

		       
(defun m3::file-true-name (fn)
  (let ((continue t))
    (while continue
      (let* ((fa (file-attributes fn))
	     (fa1 (car fa)))
	(cond
	 ((or (eq fa1 t) (not fa1))
	  (setq continue nil))
	 (t
	  ;; Otherwise, fa is a symbolic link; follow it.
	  (setq fn fa1)))))
    (expand-file-name fn)))
	  

(defun m3::get-extension (name)
  "Gets .ext from the given string (where ext is any extension)"
  (let ((dot-pos (string-match "\\." name)))
    (if dot-pos 
	(let ((ext (substring name dot-pos nil)) ext-pos)
	  (setq ext-pos (string-match "<" ext))
	  (if ext-pos (substring ext 0 ext-pos) ext)))))

(defun m3::strip-extension (name)
"Strips .ext from the given string (where ext is any extension)"
  (let ((dot-pos (string-match "\\." name)))
    (if dot-pos (substring name 0 dot-pos) name)))
