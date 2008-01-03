;Here's a version of ispell.el which works with the old ispell v3
;rather than the new unimproved version 4 distributed with emacs v19.

;Some big holes still are highlighting in version 19 emacs, and
;apparently ispell-choose-help doesn't work on all emacs versions.
;Hopefully I'll look at the v19 highlighting, but if you have
;suggestions, let me know.  The code in there now has been contributed
;by various authors, and I haven't tested it.
;
;Please send all comments, suggestions, improvements, and bug reports to me
;at stevens@cpsc.ucalgary.ca  -- or --  stevens@hplkss.hpl.hp.com
;
;Thanks		-Ken
;
;______________________________cut here_________________________________
;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: emacs-lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spelling correction interface for GNU EMACS "ispell"
;;; 
;;; 
;;; Authors         : Ken Stevens and a cast of thousands.
;;;                 : Original by Walt Buehring
;;; Last Modified By: Ken Stevens <stevens@cpsc.ucalgary.ca>
;;; Last Modified On: Wed Jun 02 12:32:52 MDT 1993
;;; Update Revision : 2.20
;;; Syntax          : emacs-lisp
;;; Status	    : Beta test
;;; Version	    : International Ispell Version 3.0 by Geoff Kuenning.
;;; 
;;; 

;;; Not yet released....  still testing!

(defvar ispell-version "2.20 Wed Jun 02 12:32:52 MDT 1993 ==Kenny7 was here==")

;;; INSTRUCTIONS
;;;
;;;  This code contains a section of user-settable variables that you should
;;; inspect prior to installation.  Look past the end of the history list.
;;; Set them up for your locale and the preferences of the majority of the
;;; users.  Otherwise the users may need to set a number of variables
;;; themselves.
;;;  You particularly may want to change the default dictionary for your
;;; country and language.
;;;
;;; 
;;; To fully install this, add this file to your GNU lisp directory and 
;;; compile it with M-X byte-compile-file.  Then add the following to the
;;; appropriate init file:
;;;
;;;  (autoload 'ispell-word "ispell"
;;;    "Check the spelling of word in buffer." t)
;;;  (global-set-key "\e$" 'ispell-word)
;;;  (autoload 'ispell-region "ispell"
;;;    "Check the spelling of region." t)
;;;  (autoload 'ispell-buffer "ispell"
;;;    "Check the spelling of buffer." t)
;;;  (autoload 'ispell-complete-word "ispell"
;;;    "Look up current word in dictionary and try to complete it." t)
;;;  (autoload 'ispell-change-dictionary "ispell"
;;;    "Change ispell dictionary." t)
;;;
;;; 
;;; TABLE OF CONTENTS
;;;
;;;   ispell-word
;;;   ispell-region
;;;   ispell-buffer
;;;   ispell-complete-word
;;;   ispell-change-dictionary
;;;   ispell-kill-ispell
;;;   ispell-pdict-save
;;;   
;;;
;;; TYPE IN A QUICK TUTORIAL OF THE COMMANDS HERE!
;;;
;;; Commands in ispell-region:
;;; Character replacement: Replace word with choice.  May query-replace.
;;; ' ': Accept word this time.
;;; 'i': Accept word and insert into private dictionary.
;;; 'a': Accept word for this session.
;;; 'A': Accept word and place in buffer-local dictionary.
;;; 'r': Replace word with typed-in value.  Rechecked.
;;; 'R': Replace word with typed-in value. Query-replaced in buffer. Rechecked.
;;; '?': Show these commands
;;; 'x': Exit spelling buffer.  Move cursor to original point.
;;; 'X': Exit spelling buffer.  Leave cursor at the current point.
;;; 'q': Quit spelling session (Kills ispell process).
;;; 'l': Look up typed-in replacement in alternate dictionary.  Wildcards okay.
;;;
;;;
;;; BUGS:
;;;
;;;   ispell-choose-help doesn't seem to work for all emacs versions.
;;;   highlighting for version 19 emacs not fully tested or implemented.
;;;
;;;
;;; HISTORY
;;;
;;;
;;; Revision 2.20  1993/06/01 16:47:24  stevens
;;; Debugging: Boris Aronov, Rik Faith, ....
;;; Major update including many tweaks.
;;; Many changes were integrations of suggestions.
;;; ispell-complete-word originally ported by Ashwin Ram.
;;; Particular thanks to Michael Lipp, Jamie Zawinski, Phil Queinnec
;;;  and John Heidemann for suggestions and code.
;;; lookup-words rehacked to use call-process.
;;; ispell-complete-word rehacked to be compatible with the rest of the
;;; system for word searching and to include multiple wildcards,
;;; and it's own dictionary.
;;; query-replace capability added.  New options 'X' and 'R'.
;;; buffer-local modes for dictionary, word-spelling, and formatter-parsing.
;;; Many random bugs, like commented comments being skipped, fix to
;;; keep-choices-win, fix for math mode, added pipe mode choice,
;;; fixed 'q' command, ispell-word checks previous word and leave cursor
;;; in same location.  Fixed tib code which could drop spelling regions.
;;; Cleaned up setq calls for efficiency. Gave more context on window overlays.
;;; Assure context on ispell-choose.  Window lossage in look command fixed.
;;; Due to pervasive opinion, common-lisp package syntax removed.
;;;
;;; Revision 2.19  1992/01/10  10:54:08  geoff
;;; Make another attempt at fixing the "Bogus, dude" problem.  This one is
;;; less elegant, but has the advantage of working.
;;;
;;; Revision 2.18  1992/01/07  10:04:52  geoff
;;; Fix the "Bogus, Dude" problem in ispell-word.
;;;
;;; Revision 2.17  1991/09/12  00:01:42  geoff
;;; Add some changes to make ispell-complete-word work better, though
;;; still not perfectly.
;;; 
;;; Revision 2.16  91/09/04  18:00:52  geoff
;;; More updates from Sebastian, to make the multiple-dictionary support
;;; more flexible.
;;; 
;;; Revision 2.15  91/09/04  17:30:02  geoff
;;; Sebastian Kremer's tib support
;;; 
;;; Revision 2.14  91/09/04  16:19:37  geoff
;;; Don't do set-window-start if the move-to-window-line moved us
;;; downward, rather than upward.  This prevents getting the buffer all
;;; confused.  Also, don't use the "not-modified" function to clear the
;;; modification flag;  instead use set-buffer-modified-p.  This prevents
;;; extra messages from flashing.
;;; 
;;; Revision 2.13  91/09/04  14:35:41  geoff
;;; Fix a spelling error in a comment.  Add code to handshake with the
;;; ispell process before sending anything to it.
;;; 
;;; Revision 2.12  91/09/03  20:14:21  geoff
;;; Add Sebastian Kremer's multiple-language support.
;;; 
;;;
;;; Walt Buehring
;;; Texas Instruments - Computer Science Center
;;; ARPA:  Buehring%TI-CSL@CSNet-Relay
;;; UUCP:  {smu, texsun, im4u, rice} ! ti-csl ! buehring
;;;
;;; ispell-region and associated routines added by
;;; Perry Smith
;;; pedz@bobkat
;;; Tue Jan 13 20:18:02 CST 1987
;;; 
;;; extensively modified by Mark Davies and Andrew Vignaux
;;; {mark,andrew}@vuwcomp
;;; Sun May 10 11:45:04 NZST 1987
;;; 
;;; Ken Stevens  ARPA: stevens@cpsc.ucalgary.ca or stevens@hplkss.hpl.hp.com
;;; Tue Jan  3 16:59:07 PST 1989
;;; This file has overgone a major overhaul to be compatible with ispell
;;; version 2.1.  Most of the functions have been totally rewritten, and
;;; many user-accessible variables have been added.  The syntax table has
;;; been removed since it didn't work properly anyway, and a filter is
;;; used rather than a buffer.  Regular expressions are used based on
;;; ispell's internal definition of characters (see ispell(4)).
;;; Some new updates:
;;; - Updated to version 3.0 to include terse processing.
;;; - Added a variable for the look command.
;;; - Fixed a bug in ispell-word when cursor is far away from the word
;;;   that is to be checked.
;;; - Ispell places the incorrect word or guess in the minibuffer now.
;;; - fixed a bug with 'l' option when multiple windows are on the screen.
;;; - lookup-words just didn't work with the process filter.  Fixed.
;;; - Rewrote the process filter to make it cleaner and more robust
;;;   in the event of a continued line not being completed.
;;; - Made ispell-init-process more robust in handling errors.
;;; - Fixed bug in continuation location after a region has been modified by
;;;   correcting a misspelling.
;;; Mon 17 Sept 1990
;;; 
;;; Sebastian Kremer <sk@thp.uni-koeln.de>
;;; Wed Aug  7 14:02:17 MET DST 1991
;;; - Ported ispell-complete-word from Ispell 2 to Ispell 3.
;;; - Added ispell-kill-ispell command.
;;; - Added ispell-dictionary and ispell-dictionary-alist variables to
;;;   support other than default language.  See their docstrings and
;;;   command ispell-change-dictionary.
;;; - (ispelled it :-)
;;; - Added ispell-check-tib variable to support the tib bibliography
;;;   program.
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; **********************************************************************
;;; The following variables should be set according to personal preference
;;; and location of binaries:
;;; **********************************************************************


;;;  ******* THIS FILE IS WRITTEN FOR ISPELL VERSION 3.0


(provide 'ispell)

;;; Highlighting can slow down display at slow baud and emacs in
;;; X11 windows cannot take advantage of highlighting (yet).
(defvar ispell-highlight-p nil
  "*When not nil, spelling errors will be highlighted.")

(defvar ispell-check-comments nil
  "*When true, the spelling of comments in region is checked.")

(defvar ispell-query-replace-choices t
  "*When true and spell checking a region, the correction will be made
throughout the buffer using \\[query-replace].")

(defvar ispell-check-tib nil
  "*If non-nil, the spelling of references for the tib(1) bibliography
program is checked.  Else any text between strings matching the regexps
ispell-tib-ref-beginning and ispell-tib-ref-end is ignored, usually what
you want.")

(defvar ispell-tib-ref-beginning "[[<]\\."
  "Regexp matching the beginning of a Tib reference.")

(defvar ispell-tib-ref-end "\\.[]>]"
  "Regexp matching the end of a Tib reference.")

(defvar ispell-keep-choices-win nil
  "*When true, the *Choices* window remains for spelling session.")

(defvar ispell-choices-win-default-height 2
  "*The default size of the *Choices*, including status line.
  Must be greater than 1.")

(defvar ispell-program-name "ispell"
  "Program invoked by \\[ispell-word] and \\[ispell-region] commands.")

(defvar ispell-alternate-dictionary "/usr/dict/words"
  "*Alternate dictionary for spelling help.")

(defvar ispell-complete-word-dict ispell-alternate-dictionary
  "*Dictionary used for word completion.")

(defvar ispell-grep-command "/usr/bin/egrep"
  "Name of the grep command for search processes.")

(defvar ispell-grep-options "-i"
  "Options for ispell-grep-command.
Should probably be \"-i\" or \"-e\".")

(defvar ispell-look-p t
  "*Use look.  Should be nil if your UNIX doesn't have this program.
Attempts to automatically reset if look not available")

(defvar ispell-look-command "/usr/bin/look"
  "Name of the look command for search processes.
Must contain complete path!")

(defvar ispell-look-options "-df"
  "Options for ispell-look-command")

(defvar ispell-use-ptys-p nil
  "When t, emacs will use pty's to communicate with ispell.
When nil, emacs will use pipes.")

(defvar ispell-following-word nil
  "*If non-nil the \\[ispell-word] command will check the spelling
of the word under or following \(rather than preceding\) the cursor
when called interactively.")

(defvar ispell-quietly nil
  "*If non-nil, the \\[ispell-word] command will suppress all
non-corrective messages when called interactively.")

(defvar ispell-format-word (function upcase)
  "*The function called to format the ...
The function must take one string argument and return a string.")

(defvar ispell-personal-dictionary nil
  "*A string or nil.  If nil, the default directory, ~/.ispell-words is used.")

(defvar ispell-tex-major-modes '(plain-TeX-mode plain-tex-mode TeX-mode
						tex-mode LaTeX-mode latex-mode)
  "The major modes which put ispell into TeX processing mode.")


(defvar ispell-dictionary nil
  "If non-nil, a dictionary to use instead of the default one.
This is passed to the ispell process using the \"-d\" switch and is
used as key in ispell-dictionary-alist (which see).

You should set this variable before your first call to ispell (e.g. in
your .emacs), or use the \\[ispell-change-dictionary] command to
change it, as changing this variable only takes effect in a newly
started ispell process.")

(defvar ispell-dictionary-alist		; sk  9-Aug-1991 18:28
  '((nil				; default (english.aff) 
     "[A-Za-z]" "[^A-Za-z]" "[---']" nil ("-B") nil)
    ("english"				; make english explicitly selectable
     "[A-Za-z]" "[^A-Za-z]" "[---']" nil ("-B") nil)
    ("german"				; german.aff
     "[A-Za-z]" "[^A-Za-z]" "[---'\"]" t ("-C") nil)
    ("swedish"				;7 bit swedish mode
     "[A-Za-z}{|\\133\\135\\\\]" "[^A-Za-z}{|\\133\\135\\\\]"
     "[---']" nil ("-C") nil)
    ("swedish8"				;8 bit swedish mode
     "[A-Za-z\345\344\366\305\304\366]"  "[^A-Za-z\345\344\366\305\304\366]"
     "[---']" nil ("-C") "~list")	; Add `"-T" "list"' to args instead?
    ("french"
     "[A-Za-z]" "[^A-Za-z]" "[---`'\^]" nil nil nil)
    )
  "An alist of dictionaries and their associated parameters.

Each element of this list is also a list:

    \(DICTIONARY-NAME
        CASECHARS NOT-CASECHARS OTHERCHARS MANY-OTHERCHARS-P
        ISPELL-ARGS EXTENDED-CHARACTER-MODE\)

DICTIONARY-NAME is a possible value of variable ispell-dictionary, nil
means the default dictionary.

CASECHARS is a regular expression of valid characters that comprise a
word.

NOT-CASECHARS is the opposite regexp of CASECHARS.

OTHERCHARS is a regular expression of other characters that are valid
in word constructs.  Otherchars cannot be adjacent to each other in a
word, nor can they begin or end a word.  This implies we can't check
\"Stevens'\" as a correct possessive and other correct formations.

Hint: regexp syntax requires the hyphen to be declared first here.

MANY-OTHERCHARS-P is non-nil if many otherchars are to be allowed in a
word instead of only one.

ISPELL-ARGS is a list of additional arguments passed to the ispell
subprocess.

EXTENDED-CHARACTER-MODE should be used when dictionaries are used which
have been configured in ispell's parse.y.  (For example, umlauts
can be encoded as \\\"a, a\\\", \"a, ...)  Defaults are ~tex and ~nroff
in english.  This has the same effect as the command-line `-T' option.
The buffer Major Mode controls ispell's parsing in tex or nroff mode,
but the dictionary can control the extended character mode.
Both defaults can be overruled in a buffer-local fashion. See
ispell-parsing-keyword for details on this.

Note that the CASECHARS and OTHERCHARS slots of the alist should
contain the same character set as casechars and otherchars in the
language.aff file (e.g., english.aff).")



;;; **********************************************************************
;;; The following are used by ispell, and should not be changed.
;;; **********************************************************************


(defun ispell-get-casechars ()
  (nth 1 (assoc ispell-dictionary ispell-dictionary-alist)))
(defun ispell-get-not-casechars ()
  (nth 2 (assoc ispell-dictionary ispell-dictionary-alist)))
(defun ispell-get-otherchars ()
  (nth 3 (assoc ispell-dictionary ispell-dictionary-alist)))
(defun ispell-get-many-otherchars-p ()
  (nth 4 (assoc ispell-dictionary ispell-dictionary-alist)))
(defun ispell-get-ispell-args ()
  (nth 5 (assoc ispell-dictionary ispell-dictionary-alist)))
(defun ispell-get-extended-character-mode ()
  (nth 6 (assoc ispell-dictionary ispell-dictionary-alist)))


(defvar ispell-process nil
  "Holds the process object for 'ispell'")

(defvar ispell-pdict-modified-p nil
  "T when the personal dictionary has modifications that need to be written.")

(defvar ispell-quit nil
  "Set to t or point when user want to abort ispell session.")

(defvar ispell-filter nil
  "Output filter from piped calls to ispell.")

(defvar ispell-filter-continue nil
  "Control variable for ispell filter function.")

(defvar ispell-process-directory nil
  "The directory where ispell-process was started.")

(defvar ispell-saved-selection nil
  "New ver-19 highlighting?")

(defvar ispell-query-replace-marker (make-marker)
  "Marker for query-replace processing.")


;;; *** Buffer Local Definitions ***

(defvar ispell-local-dictionary nil
  "A buffer local variable. If non-nil, a dictionary to be used when running
an ispell-command in this buffer. Setting ispell-local-dictionary to a value
has the same effect as calling \\[ispell-change-dictionary] with that value.
This variable is automatically set when defined in the file with either
ispell-dictionary-keyword or the Local Variable syntax.
If using Local Variable syntax, the dictionary must be a string.")

(make-variable-buffer-local 'ispell-local-dictionary)

;; use default directory.  For now it is nil, and unnecessary.
;; (set-default 'ispell-local-dictionary nil)

(defvar ispell-words-keyword "Local IspellWords: "				
  "The keyword for local oddly-spelled words to accept.
The keyword will be followed by any number of local word spellings.
There can be multiple of these keywords in the file.")

(defvar ispell-dictionary-keyword "Local IspellDict: "
  "The keyword for local dictionary definitions.
There should be only one dictionary keyword definition per file, and it
should be followed by a correct dictionary name in ispell-dictionary-alist.")

(defvar ispell-parsing-keyword "Local IspellParsing: "
  "The keyword for overriding default ispell parsing as determined by
the buffer's major mode and extended-character mode as determined by the
default dictionary.
  The above keyword string should be followed by `latex-mode' or
`nroff-mode' to put the current buffer into the desired parsing mode.
  Extended character mode can be changed for this buffer by placing
a `~' followed by an extended-character mode -- such as `~tex'.")

(defvar ispell-buffer-local-name nil
  "Contains the buffer name if local definitions were used.")


;;; **********************************************************************
;;; **********************************************************************



(defun ispell-word (&optional following quietly)
  "Check spelling of word under or before the cursor.
If word not found in dictionary, display possible corrections in a window
and let user select.
  If optional argument FOLLOWING is non-nil or if ispell-following-word
is non-nil when called interactively, then the following word
\(rather than preceding\) will be checked when the cursor is not over a word.
  When the optional argument QUIETLY is non-nil or ispell-quietly is non-nil
when called interactively, non-corrective messages are suppressed.

  Word syntax described by ispell-dictionary-alist (which see)."
  (interactive)
  (if (interactive-p)
      (setq following ispell-following-word
	    quietly ispell-quietly))
  (ispell-buffer-local-dict)		; use the correct dictionary
  (let ((cursor-location (point))	; retain cursor location
	ispell-keep-choices-win		; override global to force creation
	(word (ispell-get-word following))
	start end poss replace)
    ;; destructure return word info list.
    (setq start (car (cdr word))
	  end (car (cdr (cdr word)))
	  word (car word))

    ;; now check spelling of word.
    (or quietly
	(message "Checking spelling of %s..."
		 (funcall ispell-format-word word)))
    (ispell-init-process)		; erases ispell output buffer
    (process-send-string ispell-process "%\n") ;put in verbose mode
    (process-send-string ispell-process (concat "^" word "\n"))
    ;; wait until ispell has processed word
    (while (progn
	     (accept-process-output ispell-process)
	     (not (string= "" (car ispell-filter)))))
    ;;(process-send-string ispell-process "!\n") ;back to terse mode.
    (setq ispell-filter (cdr ispell-filter))
    (if (listp ispell-filter)
	(setq poss (ispell-parse-output (car ispell-filter))))
    (cond ((eq poss t)
	   (or quietly
	       (message "%s is correct." (funcall ispell-format-word word))))
	  ((stringp poss)
	   (or quietly
	       (message "%s is correct because of root %s"
			(funcall ispell-format-word word)
			(funcall ispell-format-word poss))))
	  ((null poss) (message "Error in ispell process"))
	  (t				; prompt for correct word.
	   (unwind-protect
	       (progn
		 (if ispell-highlight-p
		     (highlight-spelling-error start end t)) ; highlight word
		 (setq replace (ispell-choose (car (cdr (cdr poss)))
					      (car (cdr (cdr (cdr poss))))
					      (car poss))))
	     ;; protected
	     (if ispell-highlight-p	; clear highlight
		 (highlight-spelling-error start end)))
	   (cond ((equal 0 replace)
		  (ispell-add-per-file-word-list (car poss)))
		 (replace
		  (delete-region start end)
		  (setq word (if (atom replace) replace (car replace))
			cursor-location (+ (- (length word) (- end start))
					   cursor-location))
		  (insert-string word)
		  (if (not (atom replace)) ; recheck spelling of replacement
		      (progn
			(goto-char cursor-location)
			(ispell-word following quietly)))))
	   (if (get-buffer "*Choices*")
	       (kill-buffer "*Choices*"))))
    (goto-char cursor-location)		; return to original location
    (ispell-pdict-save)
    (if ispell-quit (setq ispell-quit nil))))


(defun ispell-get-word (following &optional extra-otherchars)
  "Return the word for spell-checking according to ispell syntax.
  If optional argument FOLLOWING is non-nil or if ispell-following-word
is non-nil when called interactively, then the following word
\(rather than preceeding\) will be checked when the cursor is not over a word.
  Optional second argument contains otherchars that can be included in word
many times.

  Word syntax described by ispell-dictionary-alist (which see)."
  (let* ((ispell-casechars (ispell-get-casechars))
	 (ispell-not-casechars (ispell-get-not-casechars))
	 (ispell-otherchars (ispell-get-otherchars))
	 (ispell-many-otherchars-p (ispell-get-many-otherchars-p))
	 (word-regexp (concat ispell-casechars
			      "+\\("
			      ispell-otherchars
			      "?"
			      (if extra-otherchars
				  (concat extra-otherchars "?"))
			      ispell-casechars
			      "+\\)"
			      (if (or ispell-many-otherchars-p
				      extra-otherchars)
				  "*" "?")))
	 did-it-once
	 start end word)
    ;; find the word
    (if (not (looking-at ispell-casechars))
	(if following
	    (re-search-forward ispell-casechars (point-max) t)
	  (re-search-backward ispell-casechars (point-min) t)))
    ;; move to front of word
    (re-search-backward ispell-not-casechars (point-min) 'start)
    (while (and (or (looking-at ispell-otherchars)
		    (and extra-otherchars (looking-at extra-otherchars)))
		(not (bobp))
		(or (not did-it-once)
		    ispell-many-otherchars-p))
      (if (and extra-otherchars (looking-at extra-otherchars))
	  (progn
	    (backward-char 1)
	    (if (looking-at ispell-casechars)
		(re-search-backward ispell-not-casechars (point-min) t)))
	(setq did-it-once t)
	(backward-char 1)
	(if (looking-at ispell-casechars)
	    (re-search-backward ispell-not-casechars (point-min) t)
	  (backward-char -1))))
    ;; Now mark the word and save to string.
    (or (re-search-forward word-regexp (point-max) t)
	(error "No word found to check!"))
    (setq start (match-beginning 0)
	  end (point)
	  word (buffer-substring start end))
    (list word start end)))


;;; Global ispell-pdict-modified-p is set by ispell-choose and
;;; tracks changes in the dictionary.  The global may either be
;;; a value or a list, whose value is the state of whether the
;;; dictionary needs to be saved.

(defun ispell-pdict-save (&optional force-save)
  "Check to see if the personal dictionary has been modified.
  If so, ask if it needs to be saved."
  (interactive)
  (if (interactive-p) (setq force-save t))
  (if (and ispell-pdict-modified-p (listp ispell-pdict-modified-p))
      (setq ispell-pdict-modified-p (car ispell-pdict-modified-p)))
  (if (or ispell-pdict-modified-p force-save)
      (if (y-or-n-p "Personal dictionary modified.  Save? ")
	  (process-send-string ispell-process "#\n")))
  ;; unassert variable, even if not saved to avoid questioning.
  (setq ispell-pdict-modified-p nil))


(defun ispell-choose (miss guess word)
  "Display possible corrections from list MISS.
GUESS lists possibly valid affix construction of WORD.
Returns nil to keep word.
	0 to insert locally into buffer-local dictionary.
        string for new chosen word.
        list for new replacement word (will be rechecked).
	  Optional second argument means replace misspelling in
	  the rest of the region.
Global ispell-pdict-modified-p becomes a list where the only value
indicates whether the dictionary has been modified when option a or i is
used."
  (unwind-protect
  (save-window-excursion
  (let ((count ?0)
	(line 2)
	(max-lines (- (window-height) 4)) ; assure 4 context lines.
	(choices miss)
	(window-min-height (min window-min-height
				ispell-choices-win-default-height))
	(command-characters '( ?  ?i ?a ?A ?r ?R ?? ?x ?X ?q ?l ))
	(skipped 0)
	char num result)
    (save-excursion
    (if ispell-keep-choices-win
	(select-window (previous-window))
      (set-buffer (get-buffer-create "*Choices*"))
      (setq mode-line-format "--  %b  --"))
    (if (equal (get-buffer "*Choices*") (current-buffer))
	(erase-buffer)
      (error "Bogus, dude! I should be in the *Choices* buffer, but I'm not!"))
    (if guess
	(progn
	  (insert
	   "\tAffix rules generate and capitalize this word as shown below:\n")
	  (while guess
	    (if (> (+ 4 (current-column) (length (car guess)))
		   (window-width))
		(progn
		  (insert "\n")
		  (setq line (1+ line))))
	    (insert (car guess) "    ")
	    (setq guess (cdr guess)))
	  (insert "\nUse option \"i\" if this is a correct composition from the derivative root.\n\n")
	  (setq line (+ line 4))))
    (while (and choices
		(< (if (> (+ 7 (current-column) (length (car choices))
			     (if (> count ?~) 3 0))
			  (window-width))
		       (progn
			 (insert "\n")
			 (setq line (1+ line)))
		     line)
		   max-lines))
      ;; not so good if there are over 20 or 30 options, but then, if
      ;; there are that many you don't want to have to scan them all anyway...
      (while (memq count command-characters) ; skip command characters.
	(setq count (1+ count)
	      skipped (1+ skipped)))
      (insert "(" count ") " (car choices) "  ")
      (setq choices (cdr choices)
	    count (1+ count)))
    (setq count (- count ?0 skipped)))

    (if ispell-keep-choices-win
	(if (> line ispell-keep-choices-win)
	    (progn
	      (switch-to-buffer "*Choices*")
	      (select-window (next-window))
	      (save-excursion
		(let ((cur-point (point)))
		  (move-to-window-line (- line ispell-keep-choices-win))
		  (if (<= (point) cur-point)
		      (set-window-start (selected-window) (point)))))
	      (select-window (previous-window))
	      (enlarge-window (- line ispell-keep-choices-win))
	      (goto-char (point-min))))
      (overlay-window (max line ispell-choices-win-default-height)))
    (switch-to-buffer "*Choices*")
    (goto-char (point-min))
    (select-window (next-window))
    (while
	(eq
	 t
	 (setq
	  result
	  (progn
	    (message "C-h or ? for more options; SPC to leave unchanged, Character to replace word")
	    (setq char (read-char)
		  skipped 0)
	    ;; Adjust num to array offset skipping command characters.
	    (let ((com-chars command-characters))
	      (while com-chars
		(if (and (> (car com-chars) ?0) (< (car com-chars) char))
		    (setq skipped (1+ skipped)))
		(setq com-chars (cdr com-chars)))
	      (setq num (- char ?0 skipped)))

	    (cond
	     ((= char ? ) nil)		; accept word this time only
	     ((= char ?i)		; accept and insert word into pers dict
	      (process-send-string ispell-process (concat "*" word "\n"))
	      (setq ispell-pdict-modified-p '(t)) ; dictionary was modified!
	      nil)
	     ((or (= char ?a) (= char ?A)) ; accept word, don't insert in dict
	      (process-send-string ispell-process (concat "@" word "\n"))
	      (if (null ispell-pdict-modified-p)
		  (setq ispell-pdict-modified-p
			(list ispell-pdict-modified-p)))
	      (if (= char ?A) 0))	; return 0 for ispell-add buffer-local
	     ((or (= char ?r) (= char ?R)) ; type in replacement
	      (if (or (= char ?R) ispell-query-replace-choices)
		  (list (read-string "Query-replacement for: " word) t)
		(cons (read-string "Replacement for: " word) nil)))
	     ((or (= char ??) (= char help-char) (= char ?\C-h))
	      (ispell-choose-help)
	      t)
	     ((= char ?x)		; quit.
	      (setq ispell-quit t) nil)
	     ((= char ?X)		; quit but stay at this point.
	      (setq ispell-quit (point)) nil)
	     ((= char ?q)
	      (if (y-or-n-p "Really quit ignoring changes? ")
		  (progn
		    (ispell-kill-ispell t) ; terminate process.
		    (setq ispell-quit t
			  ispell-pdict-modified-p nil))
		t))			; continue if they don't quit.
	     ((= char ?l)
	      (let ((new-word (read-string "Lookup string ('*' is wildcard): "
					   word))
		    (new-line 2))
		(if new-word
		    (progn
		      (save-excursion
			(set-buffer (get-buffer-create "*Choices*"))
			(erase-buffer)
			(setq count ?0
			      skipped 0
			      mode-line-format "--  %b  --"
			      miss (lookup-words new-word)
			      choices miss)
			(while (and choices ; adjust choices window.
				    (< (if (> (+ 7 (current-column)
						 (length (car choices))
						 (if (> count ?~) 3 0))
					      (window-width))
					   (progn
					     (insert "\n")
					     (setq new-line (1+ new-line)))
					 new-line)
				       max-lines))
			  (while (memq count command-characters)
			    (setq count (1+ count)
				  skipped (1+ skipped)))
			  (insert "(" count ") " (car choices) "  ")
			  (setq choices (cdr choices)
				count (1+ count)))
			(setq count (- count ?0 skipped)))
		      (select-window (previous-window))
		      (if (/= new-line line)
			  (progn
			    (if (> new-line line)
				(enlarge-window (- new-line line))
			      (shrink-window (- line new-line)))
			    (setq line new-line)))
		      (select-window (next-window)))))
	      t)			; reselect from new choices
	     ((and (>= num 0) (< num count))
	      (if ispell-query-replace-choices ; Query replace when flag set.
		  (list (nth num miss) 'query-replace)
		(nth num miss)))
	     ((= char ?\C-l)
	      (redraw-display) t)
	     ((= char ?\C-r)
	      (save-window-excursion (recursive-edit)) t)
	     ((= char ?\C-z)
	      (suspend-emacs) t)
	     (t (ding) t))))))
    result))
  (if (not ispell-keep-choices-win) (bury-buffer "*Choices*"))))


(defun ispell-choose-help ()
  (let ((help-1 "[r/R]eplace word; [a/A]ccept for this session; [i]nsert into private dictionary")
	(help-2 "[l]ook a word up in alternate dictionary;  e[x/X]it;  [q]uit session."))
    (if (and (boundp 'epoch::version)
	     (equal epoch::version "Epoch 3.1"))
	;; Enlarging the minibuffer crashes Epoch 3.1
	(with-output-to-temp-buffer "*Ispell Help*"
	  (princ help-1)
	  (princ "\n")
	  (princ help-2))
      (save-window-excursion
	(select-window (minibuffer-window))
	(message help-2)
	(enlarge-window 1)
	(message help-1)
	(sit-for 5)
	(erase-buffer)))))


(defun lookup-words (word &optional lookup-dict)
  "Look up word in word-list dictionary.
A '*' is used for wild cards.  If no wild cards, 'look' is used if it exists.
 Otherwise the variable ispell-grep-command contains the command used to
 search for the words (usually egrep).
Optional second argument contains the dictionary to use, the default is
 ispell-alternate-dictionary."
  ;; We don't use the filter for this function, rather the result is written
  ;; into a buffer.  Hence there is no need to save the filter values.
  (if (null lookup-dict)
      (setq lookup-dict ispell-alternate-dictionary))
  (let ((process-connection-type ispell-use-ptys-p)
	(do-look (and ispell-look-p	; Only use look for an exact match.
		      (not (string-match "\\*" word)) 
		      (setq ispell-look-p
			    (file-exists-p ispell-look-command))))
	(ispell-grep-buffer (get-buffer-create "*Ispell-Temp*")) ; result buf
	retval results loc)
    (unwind-protect
    (save-window-excursion
      (message "Starting \"%s\" process..." (if do-look "look" "grep"))
      (if (not do-look)			; Format correctly for grep search.
	  (let ((start 0)
		new-word end)
	    (while (progn		; change "*"'s to ".*"'s.
		     (if (setq end (string-match "\\*" word start))
			 (setq new-word (concat new-word
						(substring word start end)
						".*")
			       start (1+ end))
		       (setq new-word (concat new-word (substring word start)))
		       nil)))
	    (setq word (concat "^" new-word "$"))))

      (set-buffer ispell-grep-buffer)
      (setq retval
	    (call-process (if do-look ispell-look-command ispell-grep-command)
			  nil t nil
			  (if do-look ispell-look-options ispell-grep-options)
			  word lookup-dict))
      (if (and retval (not (eq retval 0)))
	  (setq results (cons (concat "error: exited with signal " retval)
			      results))
	;; Collect words into `results' in FIFO order
	(goto-char (point-max))
	;; assure we've ended with \n
	(or (bobp) (= (preceding-char) ?\n) (insert ?\n))
	(while (not (bobp))
	  (setq loc (point))
	  (forward-line -1)
	  (setq results (cons (buffer-substring (point) (1- loc)) results)))))
    ;; protected
    (kill-buffer ispell-grep-buffer)
    (if (and results (string-match ".+: " (car results)))
	(progn				; Error occured.  Display error message
	  (message "%s" (car results))
	  (setq results nil)
	  (sit-for 3))))
    results))


;;; "ispell-filter" is a list of output lines from the generating function.
;;;   Each full line (ending with \n) is a separate item on the list.
;;; "output" can contain multiple lines, part of a line, or both.
;;; "start" and "end" are used to keep bounds on lines when "output" contains
;;;   multiple lines.
;;; "ispell-filter-continue" is true when we have received only part of a
;;;   line as output from a generating function ("output" did not end with \n)
;;; NOTE THAT THIS FUNCTION WILL FAIL IF THE PROCESS OUTPUT DOESNT END WITH \n!
;;;   This is the case when a process dies or fails. The default behavior
;;;   in this case treats the next input received as fresh input.

(defun ispell-filter (process output)
  "Output filter function for ispell, grep, and look."
  (let ((start 0)
	(continue t)
	end)
    (while continue
      (setq end (string-match "\n" output start)) ; get text up to the newline.
      ;; If we get out of sync and ispell-filter-continue is asserted when we
      ;; are not continuing, treat the next item as a separate list.  When
      ;; ispell-filter-continue is asserted, ispell-filter *should* always be a
      ;; list!

      ;; Continue with same line (item)?
      (if (and ispell-filter-continue ispell-filter (listp ispell-filter))
	  ;; Yes.  Add it to the prev item
	  (setcar ispell-filter
		  (concat (car ispell-filter) (substring output start end)))
	;; No. This is a new line and item.
	(setq ispell-filter
	      (cons (substring output start end) ispell-filter)))
      (if (null end)
	  ;; We've completed reading the output, but didn't finish the line.
	  (setq ispell-filter-continue t continue nil)
	;; skip over newline, this line complete.
	(setq ispell-filter-continue nil end (1+ end))
	(if (= end (length output))	; No more lines in output
	    (setq continue nil)		;  so we can exit the filter.
	  (setq start end))))))		; else move start to next line of input


;;; For versions less than 19 this function destroys the mark location
;;; if it is in the word being highlighted.

(defun highlight-spelling-error (start end &optional highlight)
  "Highlight a word by toggling inverse-video.
  highlights word from START to END.
  When the optional third arg HIGHLIGHT is set, the word is drawn in inverse
  video, otherwise the word is drawn in normal video mode."
  (if (string-match "^19\\." emacs-version)
      (if (string-match "Lucid" emacs-version)
	  (highlight-spelling-error-v19-Lucid start end highlight)
	(highlight-spelling-error-v19 start end highlight))
    (let ((modified (buffer-modified-p)) ; don't allow this fn to modify buffer
	  (text (buffer-substring start end)) ; Save highlight region
	  (inhibit-quit t)		; inhibit interrupt processing here.
	  (buffer-undo-list nil))	; don't clutter the undo list.
      (delete-region start end)
      (insert-char ?  (- end start))	; mimimize amount of redisplay
      (sit-for 0)			; update display
      (if highlight (setq inverse-video (not inverse-video))) ; toggle video
      (delete-region start end)		; delete whitespace
      (insert text)			; insert text in inverse video.
      (sit-for 0)			; update display showing inverse video.
      (if highlight (setq inverse-video (not inverse-video))) ; toggle video
      (set-buffer-modified-p modified)))) ; don't modify if flag not set.


;;; debug debug debug debug
;;; The next two functions are not complete!

(defun highlight-spelling-error-v19-Lucid (start end &optional highlight)
  (if highlight
      (isearch-highlight start end)
    (isearch-dehighlight t))
  (sit-for 0))

(defun highlight-spelling-error-v19 (start end &optional highlight)
  (if highlight
      (setq ispell-saved-selection (cons selection-begin selection-end)
	    selection-begin (set-marker (make-marker) start)
	    selection-end (set-marker (make-marker) end))
    (setq selection-begin (car ispell-saved-selection)
	  selection-end (cdr ispell-saved-selection)
	  ispell-saved-selection nil))
  (sit-for 0))


(defun overlay-window (height)
  "Create a (usually small) window with HEIGHT lines and avoid recentering."
  (save-excursion
    (let ((oldot (save-excursion (forward-line -1) (point)))
	  (top (save-excursion (move-to-window-line height) (point))))
      (if (< oldot top) (setq top oldot))
      (split-window-vertically height)
      (set-window-start (next-window) top))))


(defun ispell-parse-output (output)
  "Parse the OUTPUT string of 'ispell' and return:
1: T for an exact match.
2: A string containing the root word for a match via suffix removal.
3: A list of possible correct spellings of the format:
   '(\"original-word\" offset miss-list guess-list)
   original-word is a string of the possibly misspelled word.
   offset is an integer giving the line offset of the word.
   miss-list and guess-list are possibly null lists of guesses and misses."
  (cond
   ((string= output "") t)		; for startup with pipes...
   ((string= output "*") t)		; exact match
   ((string= (substring output 0 1) "+") ; found cuz of root word
    (substring output 2))		; return root word
   (t					; need to process &, ?, and #'s
    (let ((type (substring output 0 1))	; &, ?, or #
	  (original-word (substring output 2 (string-match " " output 2)))
	  (cur-count 0)			; contains number of misses + guesses
	  count miss-list guess-list offset)
      (setq output (substring output (match-end 0))) ; skip over misspelling
      (if (string= type "#")
	  (setq count 0)		; no misses for type #
	(setq count (string-to-int output) ; get number of misses.
	      output (substring output (1+ (string-match " " output 1)))))
      (setq offset (string-to-int output))
      (if (string= type "#")		; No miss or guess list.
	  (setq output nil)
	(setq output (substring output (1+ (string-match " " output 1)))))
      (while output
	(let ((end (string-match ", \\|\\($\\)" output))) ; end of miss/guess.
	  (setq cur-count (1+ cur-count))
	  (if (> cur-count count)
	      (setq guess-list (cons (substring output 0 end) guess-list))
	    (setq miss-list (cons (substring output 0 end) miss-list)))
	  (if (match-end 1)		; True only when at end of line.
	      (setq output nil)		; no more misses or guesses
	    (setq output (substring output (+ end 2))))))
      (list original-word offset miss-list guess-list)))))


(defun ispell-init-process ()
  "Check status of 'ispell' process and start if necessary."
  (if (and ispell-process
	   (eq (process-status ispell-process) 'run)
	   (equal ispell-process-directory default-directory))
      (setq ispell-filter nil ispell-filter-continue nil)
    ;; may need to restart to select new dictionary.
    (ispell-kill-ispell t)
    (message "Starting new ispell process...")
    (sit-for 0)
    (setq ispell-process
	  (let ((process-connection-type ispell-use-ptys-p))
	    (apply 'start-process
		   "ispell" nil ispell-program-name
		   "-a"			; accept single input lines
		   "-m"			; make root/affix combos not in dict
		   (let ((args (ispell-get-ispell-args)))
		     (if ispell-dictionary ; use specified dictionary
			 (setq args
			       (append (list "-d" ispell-dictionary)
				       args)))
		     (if ispell-personal-dictionary ; use specified pers dict
			 (setq args
			       (append (list "-p" ispell-personal-dictionary)
				       args)))
		     args)))
	  ispell-filter nil
	  ispell-filter-continue nil
	  ispell-process-directory default-directory)
    (set-process-filter ispell-process 'ispell-filter)
    (while (progn			; Get version ID line
	     (accept-process-output ispell-process)
	     (not (eq (process-status ispell-process) 'run))))
    (setq ispell-filter nil)		; Discard version ID line
    (let ((extended-char-mode (ispell-get-extended-character-mode)))
      (if extended-char-mode
	  (process-send-string (concat extended-char-mode "\n"))))
    (process-kill-without-query ispell-process)))


(defun ispell-kill-ispell (&optional no-error)
  "Kill current ispell process (so that you may start a fresh one)."
  ;; With NO-ERROR, just return non-nil if there was no ispell running.
  (interactive)
  (if (not (and ispell-process
		(eq (process-status ispell-process) 'run)))
      (or no-error
	  (error "There is no ispell process running!"))
    (kill-process ispell-process)
    (setq ispell-process nil)
    (message "Killed ispell process.")
    nil))


(defun ispell-change-dictionary (dict)
  "Change ispell-dictionary (q.v.) and kill old ispell process.
A new one will be started as soon as necessary.

By just answering RET you can find out what the current dictionary is."
  (interactive
   (list (completing-read "Use new ispell dictionary (type SPC to complete): "
			  ispell-dictionary-alist nil t)))
  ;; Like info.el, we also rely on completing-read's bug of returning ""
  ;; even if this is not in the table:
  (if (or (equal dict "") (equal dict ispell-dictionary))
      (message "(No change, using %s dictionary)" ispell-dictionary)
    (if (assoc dict ispell-dictionary-alist)
	(setq ispell-dictionary dict)
      (error "Illegal dictionary: %s" dict))
    (ispell-kill-ispell t)
    (message "(Next ispell command will use %s dictionary)"
	     (or dict "default"))))



;;; Spelling of comments are checked when ispell-check-comments is non-nil.

(defun ispell-region (reg-start reg-end)
  "Interactively check a region for spelling errors."
  (interactive "*r")
  (ispell-accept-buffer-local-defs)	; set up dictionary, local words, etc.
  (unwind-protect
  (save-excursion
  (message "Spelling %s..."
	   (if (and (= reg-start (point-min)) (= reg-end (point-max)))
	       (buffer-name) "region"))
  (sit-for 0)
  ;; must be top level now, not inside ispell-choose for keeping window around.
  (save-window-excursion
  (if ispell-keep-choices-win
      (let ((window-min-height ispell-choices-win-default-height))
	;; This keeps the default window size when choices window saved.
	(setq ispell-keep-choices-win ispell-choices-win-default-height)
	(overlay-window ispell-choices-win-default-height)
	(switch-to-buffer (get-buffer-create "*Choices*"))
	(setq mode-line-format "--  %b  --")
	(erase-buffer)
	(select-window (next-window))
	(sit-for 0)))
  (goto-char reg-start)
  (while (and (not ispell-quit) (< (point) reg-end))
    (let ((start (point))
	  (offset-change 0)
	  (end (save-excursion (end-of-line) (min (point) reg-end)))
	  (ispell-casechars (ispell-get-casechars))
	  string)
      (cond				; LOOK AT THIS LINE AND SKIP OR PROCESS
       ((eolp)				; END OF LINE, just go to next line.
	(forward-char 1))
       ((and (null ispell-check-comments) ; SKIPING COMMENTS
	     comment-start		; skip comments that start on the line.
	     (search-forward comment-start end t)) ; a comment is on this line.
	(if (= (- (point) start) (length comment-start))
	    ;; comment starts the line.  We can skip the entire line or region
	    (if (string= "" comment-end) ; skip to next line over comment
		(beginning-of-line 2)
	      (search-forward comment-end reg-end 'limit)) ; jmp to comment end
	  ;; Comment starts later on line.  Check for spelling before comment.
	  (let ((limit (- (point) (length comment-start))))
	    (goto-char (1- limit))
	    (if (looking-at "\\\\")	; "quoted" comment, don't skip
		;; quoted comment.  Skip over comment-start and continue.
		(if (= start (1- limit))
		    (setq limit (+ limit (length comment-start)))
		  (setq limit (1- limit))))
	    (goto-char start)
	    ;; Only check if there are "casechars" or math chars before comment
	    (if (or (re-search-forward ispell-casechars limit t)
		    (re-search-forward "[][()$]" limit t))
		(setq string (concat "^" (buffer-substring start limit) "\n")))
	    (goto-char limit))))
       ((and (null ispell-check-tib)	; SKIP TIB REFERENCES!
	     (re-search-forward ispell-tib-ref-beginning end t))
	(if (= (- (point) 2) start)	; tib ref is 2 chars.
	    ;; Skip to end of tib ref, not necessarily on this line.
	    (re-search-forward ispell-tib-ref-end reg-end 'move)
	  ;; tib ref starts later on line.  Check spelling before tib.
	  (let ((limit (- (point) 2)))
	    (goto-char start)
	    (if (or (re-search-forward ispell-casechars limit t)
		    (re-search-forward "[][()$]" limit t))
		(setq string (concat "^" (buffer-substring start limit) "\n")))
	    (goto-char limit))))
       ((looking-at "[---#@*+!%~^]")	; SKIP SPECIAL ISPELL CHARACTERS
	(forward-char 1))
       ((or (re-search-forward ispell-casechars end t) ; TEXT EXISTS...
	    (re-search-forward "[][()$]" end t)) ; or MATH COMMANDS...
	(setq string (concat "^" (buffer-substring start end) "\n"))
	(goto-char end))
       (t (beginning-of-line 2)))	; EMPTY LINE, skip it.

      (setq end (point))		; "end" tracks end of region to check.

      (if string			; there is something to spell!
	  (let (poss)
	    ;; send string to spell process and get input.
	    (process-send-string ispell-process string)
	    (while (progn
		     (accept-process-output ispell-process)
		     ;; Last item of output contains a blank line.
		     (not (string= "" (car ispell-filter)))))
	    ;; parse all inputs from the stream one word at a time.
	    ;; Place in FIFO order and remove the blank item.
	    (setq ispell-filter (nreverse (cdr ispell-filter)))
	    (while (and (not ispell-quit) ispell-filter)
	      (setq poss (ispell-parse-output (car ispell-filter)))
	      (if (listp poss)		; spelling error occurred.
		  (let* ((word-start (+ start offset-change (car (cdr poss))))
			 (word-end (+ word-start (length (car poss))))
			 replace)
		    (goto-char word-start)
		    (if (/= word-end (progn
				       (search-forward (car poss) word-end t)
				       (point)))
			;; This usually occurs due to filter pipe problems
			(error "***ispell misalignment: word \"%s\" point %d; please retry."
			       (car poss) word-start))
		    (unwind-protect
		    (progn
		      (if ispell-highlight-p
			  (highlight-spelling-error word-start word-end t)
			(sit-for 0))	;DBH 15-Jul-1993
		      (setq replace (ispell-choose (car (cdr (cdr poss)))
						   (car (cdr (cdr (cdr poss))))
						   (car poss))))
		    ;; protected
		    (if ispell-highlight-p
			(highlight-spelling-error word-start word-end)))
		    (cond
		     ((and replace (listp replace))
		      ;; REPLACEMENT WORD entered.  Recheck line starting with
		      ;; the replacement word.
		      (setq ispell-filter nil
			    string (buffer-substring word-start word-end))
		      (let ((change (- (length (car replace)) ; adjust
				       (length (car poss))))) ;  regions
			(setq reg-end (+ reg-end change)
			      offset-change (+ offset-change change)))
		      (delete-region word-start word-end)
		      (insert (car replace))
		      ;; I only need to recheck typed-in replacements.
		      (if (not (eq 'query-replace (car (cdr replace))))
			  (backward-char (length (car replace))))
		      (setq end (point)) ; reposition in region to recheck
		      ;; when second arg exists, query-replace, saving regions
		      (if (car (cdr replace))
			  (unwind-protect
			  (progn
			    (set-marker ispell-query-replace-marker reg-end)
			    ;; Assume case-replace & case-fold-search correct?
			    (query-replace string (car replace)))
			  ;; protected
			  (setq reg-end (marker-position
					 ispell-query-replace-marker))
			  (set-marker ispell-query-replace-marker nil))))
		     ((or (null replace) (equal 0 replace)) ; ACCEPT/INSERT
		      (if (equal 0 replace) ; BUFFER-LOCAL DICTIONARY ADD
			  (setq reg-end (ispell-add-per-file-word-list
					 (car poss) reg-end)))
		      ;; This prevents us from pointing out the word that was
		      ;; just accepted (via 'i' or 'a') if it follows on the
		      ;; same line. (The drawback of processing entire lines.)
		      ;; Redo check following the accepted word.
		      (if (and ispell-pdict-modified-p
			       (listp ispell-pdict-modified-p))
			  ;; We have accepted or inserted a word. Re-check line
			  (setq ispell-pdict-modified-p ; fix update flag
				(car ispell-pdict-modified-p)
				ispell-filter nil ; don't continue check.
				end word-end))) ; reposition continue location
		     (replace		; STRING REPLACEMENT for this word.
		      (delete-region word-start word-end)
		      (insert replace)
		      (let ((change (- (length replace) (length (car poss)))))
			(setq reg-end (+ reg-end change)
			      offset-change (+ offset-change change)
			      end (+ end change)))))
		    (message "continuing spelling check...")
		    (sit-for 0)))
	      (setq ispell-filter (cdr ispell-filter))))) ; finished with line
      (goto-char end))))
  (not ispell-quit))
  ;; protected
  (if (get-buffer "*Choices*")
      (kill-buffer "*Choices*"))
  (ispell-pdict-save)
  (if ispell-quit
      (progn
	(if (numberp ispell-quit) (goto-char ispell-quit))
	(set-mark reg-end)		; preserve the region, so we can
	(setq ispell-quit nil)))	; execute 'ispell-region' as next cmd.
  (message "Spell done.")))



(defun ispell-buffer () 
  "Check the current buffer for spelling errors interactively."
  (interactive)
  (ispell-region (point-min) (point-max)))


;;; Interactive word completion.
;;; Forces "previous-word" processing.  Do we want to make this selectable?

(defun ispell-complete-word ()
  "Look up word before or under point in dictionary (see lookup-words command)
and try to complete it.  Standard ispell choices are then available."
  (interactive)
  (let ((cursor-location (point))
	ispell-keep-choices-win
	(word (ispell-get-word nil "\\*")) ; force "previous-word" processing.
	start end possibilities replacement)
    (setq start (car (cdr word))
	  end (car (cdr (cdr word)))
	  word (car word)
	  possibilities
	  (or (string= word "")		; Will give you every word
	      (lookup-words (concat word "*") ispell-complete-word-dict)))
    (cond ((eq possibilities t)
	   (message "No word to complete"))
	  ((null possibilities)
	   (message "No match"))
	  (t				; There is a modification...
	   (unwind-protect
	   (progn
	     (if ispell-highlight-p
		 (highlight-spelling-error start end t)) ; highlight word
	     (setq replacement (ispell-choose possibilities nil word)))
	   ;; protected
	   (if ispell-highlight-p
	       (highlight-spelling-error start end))) ; un-highlight
	   (cond
	    ((equal 0 replacement)	; BUFFER-LOCAL ADDITION
	     (ispell-add-per-file-word-list word))
	    (replacement		; REPLACEMENT WORD
	     (delete-region start end)
	     (setq word (if (atom replacement) replacement (car replacement))
		   cursor-location (+ (- (length word) (- end start))
				      cursor-location))
	     (insert-string word)
	     (if (not (atom replacement)) ; recheck spelling of replacement.
		 (progn
		   (goto-char cursor-location)
		   (ispell-word nil t)))))
	   (if (get-buffer "*Choices*")
	       (kill-buffer "*Choices*"))))
    (ispell-pdict-save)
    (goto-char cursor-location)))


;;; **********************************************************************
;;; 			Buffer Local Functions
;;; **********************************************************************


(defun ispell-accept-buffer-local-defs ()
  "Loads all buffer-local information, restarting ispell when necessary."
  (ispell-buffer-local-dict)		; May kill ispell-process.
  (ispell-buffer-local-words)		; Will initialize ispell-process.
  (ispell-buffer-local-parsing))


;;; Currently ispell version 3.0.09 (beta) doesn't fully support the "~"
;;; pipe mode command.  Should be fixed in the next release.

(defun ispell-buffer-local-parsing ()
  "Places ispell into parsing mode for this buffer.
This overrides the default parsing mode.
This includes latex/nroff modes and extended character mode."
  ;; (ispell-init-process) must already be called.
  (process-send-string ispell-process "!\n") ; Put process in terse mode.
  (if (memq major-mode ispell-tex-major-modes)
      (process-send-string ispell-process "+\n") ; set ispell mode to tex
    (process-send-string ispell-process "-\n"))	; set mode to normal (nroff)
  (let ((extended-char-mode (ispell-get-extended-character-mode)))
    (if extended-char-mode
	(process-send-string (concat extended-char-mode "\n"))))
  (save-excursion
    (goto-char (point-min))
    ;; Uses last valid definition
    (while (search-forward ispell-parsing-keyword nil t)
      (let ((end (save-excursion (end-of-line) (point)))
	    (case-fold-search t)
	    string)
	(while (re-search-forward " *\\([^ \"]+\\)" end t)
	  ;; space separated definitions.
	  (setq string (buffer-substring (match-beginning 1) (match-end 1)))
	  (cond ((string-match "latex-mode" string)
		 (process-send-string ispell-process "+\n"))
		((string-match "nroff-mode" string)
		 (process-send-string ispell-process "-\n"))
		((string-match "~" string) ; Set extended character mode.
		 (process-send-string ispell-process (concat string "\n")))
		(t (message "Illegal Ispell Parsing argument!")
		   (sit-for 2))))))))


;;; Can kill the current ispell process

(defun ispell-buffer-local-dict ()
  "Does necessary local dictionary initialization.
This overrides a Local Variable definition.
Both should not be used to define a buffer-local dictionary."
  (save-excursion
    (goto-char (point-min))
    (let (end)
      ;; Override the local variable definition.
      ;; Uses last valid definition.
      (while (search-forward ispell-dictionary-keyword nil t)
	(setq end (save-excursion (end-of-line) (point)))
	(if (re-search-forward " *\\([^ \"]+\\)" end t)
	    (setq ispell-local-dictionary
		  (buffer-substring (match-beginning 1) (match-end 1)))))))
  (if (and ispell-local-dictionary
	   (not (equal ispell-local-dictionary ispell-dictionary)))
      (ispell-change-dictionary ispell-local-dictionary)))


(defun ispell-buffer-local-words ()
  "Loads the buffer-local \"dictionary\" in the current buffer."
  (if (and ispell-buffer-local-name
	   (not (equal ispell-buffer-local-name (buffer-name))))
      (progn
	(setq ispell-buffer-local-name nil)
	(ispell-kill-ispell t)))
  (ispell-init-process)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward ispell-words-keyword nil t)
      (or ispell-buffer-local-name
	  (setq ispell-buffer-local-name (buffer-name)))
      (let ((end (save-excursion (end-of-line) (point)))
	    string)
	(while (re-search-forward " *\\([^ \"]+\\)" end t)
	  (setq string (buffer-substring (match-beginning 1) (match-end 1)))
	  (process-send-string
	   ispell-process (concat "@" (buffer-substring (match-beginning 1)
							(match-end 1))
				  "\n")))))))


;;; returns optionally adjusted region-end-point.

(defun ispell-add-per-file-word-list (word &optional reg-end)
  "Adds new word to the per-file word list."
  (or ispell-buffer-local-name
      (setq ispell-buffer-local-name (buffer-name)))
  (if (null reg-end)
      (setq reg-end 0))
  (save-excursion
    (goto-char (point-min))
    (let (line-okay search done string)
      (while (not done)
	(setq search (search-forward ispell-words-keyword nil 'move)
	      line-okay (< (+ (length word) 1 ; 1 for space after word..
			      (progn (end-of-line) (current-column)))
			   80))
	(if (or (and search line-okay)
		(null search))
	    (progn
	      (setq done t)
	      (if (null search)
		  (progn
		    (open-line 1)
		    (setq string (concat comment-start " "
					 ispell-words-keyword))
		    ;; in case the keyword is in the middle of the file....
		    (if (> reg-end (point))
			(setq reg-end (+ reg-end (length string))))
		    (insert string)
		    (if (and comment-end (not (equal "" comment-end)))
			(save-excursion
			  (open-line 1)
			  (forward-line 1)
			  (insert comment-end)))))
	      (if (> reg-end (point))
		  (setq reg-end (+ 1 reg-end (length word))))
	      (insert (concat " " word)))))))
  reg-end)



;;; LOCAL VARIABLES AND BUFFER-LOCAL VALUE EXAMPLES.

;;; Local Variable options:
;;; mode: name(-mode)
;;; eval: expression
;;; local-variable: value

;;; Local Variables:
;;; mode: emacs-lisp
;;; comment-column: 40
;;; ispell-local-dictionary: "english"
;;; End:


;;; THE ISPELL BUFFER-LOCAL VALUES

;;; The following places this file in nroff parsing and extended char modes.
;;; Local IspellParsing: nroff-mode ~nroff
;;; Change IspellDict to IspellDict: to enable the following line.
;;; Local IspellDict german
;;; The following were automatically generated by ispell using the 'A' command:
; Local IspellWords:  ispell defvar ispell-highlight-p ispell-check-comments
; Local IspellWords:  ispell-query-replace-choices query-replace non-nil tib
; Local IspellWords:  ispell-check-tib regexps ispell-tib-ref-beginning Regexp
; Local IspellWords:  ispell-tib-ref-end ispell-keep-choices-win ispell-word
; Local IspellWords:  ispell-choices-win-default-height ispell-program-name
; Local IspellWords:  ispell-region ispell-alternate-dictionary
