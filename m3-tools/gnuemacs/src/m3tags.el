;;
;; Tags facility for Emacs.
;; Copyright (C) 1985, 1986, 1988 Free Software Foundation, Inc.
;;
;; Extended version of the GNU Emacs tags package:
;;  - Handles both ETAGS and FTAGS format.
;;    FTAGS format differs from ETAGS format in the following ways:
;;      In ETAGS, each tag line contains: 
;;          <search-string>\177<lineno>,<charno>
;;      In FTAGS, each tag line also contains the actual tag value as well:
;;          <key>\177<search-string>\177<lineno>,<charno>
;;      This enables exact matching for tag values.
;;      Also, the first line of an FTAGS file contains
;;          <<FTAGS-1.0>>^L
;;      wherease the first line of an ETAGS file contains only
;;          ^L
;;      Thus, FTAGS file format is very easy to detect.
;;  - Provides commands that show tags buffers in other window.
;;    (See find-tag-other-window, tags-search-other-window, and 
;;    tags-query-replace-other-window functions.)
;;  - Shows exact tag matches before showing inexact matches.
;;  - tag-search now offers to look for the expression at point by default.
;;  - New function that automatically does a tag-find on the expression 
;;    at point.  (See find-default-tag-other-window function.)
;;  - Maintains a mark-trail for returning to previous window configurations.
;;    (See mark-location-form.)
;;  - Provides optional buffer-local tags files.
;;    (Derived from the harris-tags package.)
;;    (See visit-tags-table-locally function.)
;;  - Provides "merged" tag tables.  That is, one may "add" additional
;;    tags files to a buffer's tag table.  The union of the tags files
;;    will be used as the tag table.
;;    (See add-tags-table function.)
;;
;; Derived by Marvin Theimer from the standard GNU emacs tags.el package.


;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.


(provide 'tags)

(defvar tag-table-files nil
  "List of file names covered by current tag table.
nil means it has not been computed yet; do (tag-table-files) to compute it.

This is buffer local on each tags table.")
(make-variable-buffer-local 'tag-table-files)

(defvar last-tag nil
  "Tag found by the last find-tag.")

(defvar tags-file-name nil
"*Current tag table filename.

This is buffer local, so you can have multiple tags files.  To set
the default value, use visit-tags-table.  To set the local value,
use visit-tags-table-locally.")
(make-variable-buffer-local 'tags-file-name)

(defvar tags-file-names nil
"*Default list of file names comprising a merged tag table.

This is buffer local, so you can have multiple tags files.  To set
the default value, use visit-tags-table.  To set the local value,
use visit-tags-table-locally.")
(make-variable-buffer-local 'tags-file-names)

(defvar default-tags-file-names nil
"List of file names comprising a merged tag table.")


(defvar mark-location-form nil
  "Form to eval to record point and buffer before doing something
that changes buffers such as find-tag, tags-search, or next-error.
For example: '(mmt-push-window-state).")


(defvar etags-format nil
  "True if tags file being used in the standard Gnu-Emacs format.
False implies the new, extended FTAGS format.

This is buffer local on each tags table.")
(make-variable-buffer-local 'etags-format)

(defvar ftags-version-string "<<FTAGS-1.0>>"
  "Version string at the beginning of FTAGS-style tags files.")
(defvar ftags-version-string-len 13
  "Length of FTAGS version string.")


(defun visit-tags-table (file)
  "Tell tags commands to use tag table file FILE.
FILE should be the name of a file created with the `ftags' or 'etags' program.
A directory name is ok too; it means file FTAGS in that directory."
  (interactive (list (read-file-name "Visit tags table: (default FTAGS) "
				     default-directory
				     (concat default-directory "FTAGS")
				     t)))
  (setq file (expand-file-name file))
  (if (file-directory-p file)
      (setq file (concat file "FTAGS")))
  (if (not (file-exists-p file))
      (error "File %s does not exist" file))
  (message "Default tags table now %s" file)
  (set-default 'tags-file-name file)
  (setq default-tags-file-names nil))

(defun visit-tags-table-locally (file)
  "Tell tags commands to use tag table file FILE, only for this buffer.
FILE should be the name of a file created with the `ftags' or 'etags' program.
A directory name is ok too; it means file FTAGS in that directory."
  (interactive (list (read-file-name "Visit tags table: (default FTAGS) "
				     default-directory
				     (concat default-directory "FTAGS")
				     t)))
  (setq file (expand-file-name file))
  (if (file-directory-p file)
      (setq file (concat file "FTAGS")))
  (if (not (file-exists-p file))
      (error "File %s does not exist" file))
  (message "Local tags table now %s" file)
  (setq tag-table-files nil
	tags-file-name file
	tags-file-names nil))

(defun add-tags-table (file)
  "Add file FILE to the default tags table file.
FILE should be the name of a file created with the `ftags' or 'etags' program.
A directory name is ok too; it means file FTAGS in that directory."
  (interactive (list (read-file-name "Add tags table: (default FTAGS) "
				     default-directory
				     (concat default-directory "FTAGS")
				     t)))
  (setq default-tags-file-names (cons file default-tags-file-names))
  (set-default 'tags-file-names default-tags-file-names))

(defun add-tags-table-locally (file)
  "Add file FILE to the buffer-local tags table file.
FILE should be the name of a file created with the `ftags' or 'etags' program.
A directory name is ok too; it means file FTAGS in that directory."
  (interactive (list (read-file-name "Add tags table: (default FTAGS) "
				     default-directory
				     (concat default-directory "FTAGS")
				     t)))
  (setq tags-file-names (cons file tags-file-names)))

(defun visit-tags-table-buffer ()
  "Select the buffer containing the current tag table.
This is a file whose name is in the variable tags-file-name."
  (or tags-file-name
      (call-interactively 'visit-tags-table))
  (set-buffer (or (get-file-buffer tags-file-name)
		  (progn
		    (setq tag-table-files nil)
		    (find-file-noselect tags-file-name))))
  (or (verify-visited-file-modtime (get-file-buffer tags-file-name))
      (cond ((yes-or-no-p "Tags file has changed, read new contents? ")
	     (revert-buffer t t)
	     (setq tag-table-files nil))))
  (cond ((eq (char-after 1) ?\^L)
	 (setq etags-format t))
	((string-equal ftags-version-string
		      (buffer-substring 1 (+ 1 ftags-version-string-len)))
	 (setq etags-format nil))
	(t (error "File %s not a valid tag table" tags-file-name)))
  (setq tags-file-name (buffer-file-name)))

(defun file-of-tag ()
  "Return the file name of the file whose tags point is within.
Assumes the tag table is the current buffer.
File name returned is relative to tag table file's directory."
  (let ((opoint (point))
	prev size)
    (save-excursion
     (goto-char (point-min))
     (while (< (point) opoint)
       (forward-line 1)
       (end-of-line)
       (skip-chars-backward "^,\n")
       (setq prev (point))
       (setq size (read (current-buffer)))
       (goto-char prev)
       (forward-line 1)
       (forward-char size))
     (goto-char (1- prev))
     (buffer-substring (point)
		       (progn (beginning-of-line) (point))))))

(defun tag-table-files ()
  "Return a list of files in the current tag table.
File names returned are absolute."
  (let ((tags-files tags-file-names))
    (condition-case err
	(tag-table-files-inner)
      (error
       (if tags-files
	   (progn
	     (setq tags-file-name (car tags-files))
	     (setq tags-files (cdr tags-files))
	     (tag-table-files-inner))
	 (progn
	   (message (car (cdr err)))
	   (beep)))))))

(defun tag-table-files-inner ()
  "Single tag table version of tag-table-files."
  (save-excursion
   (visit-tags-table-buffer)
   (or tag-table-files
       (let (files)
	(goto-char (point-min))
	(while (not (eobp))
	  (forward-line 1)
	  (end-of-line)
	  (skip-chars-backward "^,\n")
	  (setq prev (point))
	  (setq size (read (current-buffer)))
	  (goto-char prev)
	  (setq files (cons (expand-file-name
			     (buffer-substring (1- (point))
					       (save-excursion
						 (beginning-of-line)
						 (point)))
			     (file-name-directory tags-file-name))
			    files))
	  (forward-line 1)
	  (forward-char size))
	(setq tag-table-files (nreverse files))))))

(defun find-tag-default ()
  "Return a default tag to search for, based on the text at point."
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (re-search-backward "\\sw\\|\\s_" nil t)
	(progn (forward-char 1)
	       (buffer-substring (point)
				 (progn (forward-sexp -1)
					(while (looking-at "\\s'")
					  (forward-char 1))
					(point))))
      nil)))

(defun find-tag-tag (string)
  (let* ((default (find-tag-default))
	 (spec (read-string
		(if default
		    (format "%s(default %s) " string default)
		  string))))
    (list (if (equal spec "")
	      default
	    spec))))

(defvar tags-last-match-exact nil
  "Internal variable.
Indicate whether the most recent find-tag matched exactly.")

(defvar tags-last-find-tag nil
  "Internal variable. 
Last tagname that find-tag was called on.")

(defvar remember-tags-file-name nil
  "Used to remember the value of tags-file-name from the previous buffer we
were in.")

(defun find-tag-exact (tagname)
  "Find next tag (in current tag table) whose name exactly matches TAGNAME.
Returns whether or not an exact match was found."
  (if (re-search-forward (concat "^" tagname "\177") nil t)
      (setq tags-last-match-exact t)
    (setq tags-last-match-exact nil)))

(defun find-tag-prefix (tagname)
  "Find next tag (in current tag table) that contains TAGNAME as a 
proper prefix.
Place the buffer point just beyond the 0177 that terminates a tagname."
  (if (re-search-forward (concat "^" tagname "[^\177]") nil t)
      (search-forward "\177")
    (error "No %sentries containing %s"
	   (if next "more " "") tagname))
  (setq tags-last-match-exact nil))

(defun find-tag (tagname &optional next other-window)
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (find-tag-tag "Find tag: ")))
  (eval mark-location-form)
  (let ((tags-files tags-file-names))
    (condition-case err
	(find-tag-inner tagname next other-window)
      (error
       (if tags-files
	   (progn
	     (setq tags-file-name (car tags-files))
	     (setq tags-files (cdr tags-files))
	     (find-tag-inner tagname next other-window))
	 (progn
	   (message (car (cdr err)))
	   (beep)))))))

(defun find-tag-inner (tagname &optional next other-window)
  "Single tag table version of find-tag."
  (let (buffer file linebeg startpos)
    (save-excursion
     (visit-tags-table-buffer)
     (if (not next)
	 (goto-char (point-min))
       (setq tagname last-tag))
     (setq last-tag tagname)
     (if etags-format
	 (while (progn
		  (if (not (search-forward tagname nil t))
		      (error "No %sentries containing %s"
			     (if next "more " "") tagname))
		  (not (looking-at "[^\n\177]*\177"))))
       (while
	   (progn
	     ;; If we're dealing with a "new" tagname then reset variables
	     ;; and look for an exact match.  Look for a prefix match if
	     ;; the exact match fails.
	     (if (or (not next)
		     (not (string-equal tags-last-find-tag tagname)))
		 (progn
		   (setq tags-last-find-tag tagname)
		   (if (not (find-tag-exact tagname))
		       (find-tag-prefix tagname)))
	       ;; We're looking for the "same" tag as previously.
	       ;; If we're still looking for exact matches then keep doing that.
	       ;; If we can't find any then go to the beginning of the tags
	       ;; file and start looking for prefix matches.
	       (if tags-last-match-exact
		   (if (not (find-tag-exact tagname))
		       (progn
			 (goto-char (point-min))
			 (find-tag-prefix tagname)))
		 ;; We've already found all exact matches and are looking for
		 ;; prefix matches.  Don't find any exact matches that are
		 ;; encountered along the way.
		 (find-tag-prefix tagname)))
	     (not (looking-at "[^\n\177]*\177")))))
     (search-forward "\177")
     (setq file (expand-file-name (file-of-tag)
				  (file-name-directory tags-file-name)))
     (setq linebeg
	   (if etags-format
	       (buffer-substring (1- (point))
				 (save-excursion
				   (beginning-of-line)
				   (point)))
	     (buffer-substring (1- (point))
			       (save-excursion
				 (beginning-of-line)
				 (search-forward "\177")
				 (point)))))
     (search-forward ",")
     (setq startpos (read (current-buffer))))
    (setq remember-tags-file-name tags-file-name)
    (if other-window
	(find-file-other-window file)
      (find-file file))
    (visit-tags-table-locally remember-tags-file-name)
    (widen)
    (push-mark)
    (or (find-tagpat startpos (concat "^" (regexp-quote linebeg) "\\>"))
					; To avoid finding prefixes of the
					; search string, we first
					; look for the word-delimited version
					; of the pattern.
	(find-tagpat startpos (concat "^" (regexp-quote linebeg)))
					; If the tag string isn't the end of
					; a word (according to the syntax for
					; words in the FTAGS buffer) then
					; try finding the undelimited version
					; of the pattern.
	(error "%s not found in %s" linebeg file))
    (beginning-of-line))
  (setq tags-loop-form '(find-tag nil t))
  ;; Return t in case used as the tags-loop-form.
  t)

(defun find-tagpat (startpos pat)
  "Search for pat, using startpos as a location hint.
Return an indication of whether the search succeeded."
  (let* ((offset 1000)
	 found)
    (or startpos (setq startpos (point-min)))
    (while (and (not found)
		(progn
		  (goto-char (- startpos offset))
		  (not (bobp))))
      (setq found
	    (re-search-forward pat (+ startpos offset) t))
      (setq offset (* 3 offset)))
    (or found
	(re-search-forward pat nil t))))

(defun find-tag-other-window (tagname &optional next)
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in in another window
and puts point at its definition.
 If TAGNAME is a null string, the expression in the buffer
around or before point is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  (interactive (if current-prefix-arg
		   '(nil t)
		   (find-tag-tag "Find tag other window: ")))
  (find-tag tagname next t))

(defun find-default-tag-other-window (&optional next)
  "Find tag (in current tag table) whose name contains TAGNAME.
 Selects the buffer that the tag is contained in in another window
and puts point at its definition.
 The expression in the buffer around or before point is used as the tag name.
 If second arg NEXT is non-nil (interactively, with prefix arg),
searches for the next tag in the tag table
that matches the tagname used in the previous find-tag.

See documentation of variable tags-file-name."
  (interactive)
  (find-tag (find-tag-default) next t))


(defvar next-file-list nil
  "List of files for next-file to process.")

(defun next-file (&optional initialize)
  "Select next file among files in current tag table.
Non-nil argument (prefix arg, if interactive)
initializes to the beginning of the list of files in the tag table."
  (interactive "P")
  (if initialize
      (setq next-file-list (tag-table-files)))
  (or next-file-list
      (error "All files processed."))
  (setq remember-tags-file-name tags-file-name)
  (find-file (car next-file-list))
  (visit-tags-table-locally remember-tags-file-name)
  (setq next-file-list (cdr next-file-list)))

(defun next-file-other-window (&optional initialize)
  "Select next file into another window among files in current tag table.
Non-nil argument (prefix arg, if interactive)
initializes to the beginning of the list of files in the tag table."
  (interactive "P")
  (if initialize
      (setq next-file-list (tag-table-files)))
  (or next-file-list
      (error "All files processed."))
  (setq remember-tags-file-name tags-file-name)
  (find-file-other-window (car next-file-list))
  (visit-tags-table-locally remember-tags-file-name)
  (setq next-file-list (cdr next-file-list)))

(defvar tags-loop-form nil
  "Form for tags-loop-continue to eval to process one file.
If it returns nil, it is through with one file; move on to next.")

(defun tags-loop-continue (&optional first-time)
  "Continue last \\[tags-search] or \\[tags-query-replace] command.
Used noninteractively with non-nil argument
to begin such a command.  See variable tags-loop-form."
  (interactive)
  (if first-time
      (progn (next-file t)
	     (goto-char (point-min))))
  (while (not (eval tags-loop-form))
    (next-file)
    (message "Scanning file %s..." buffer-file-name)
    (goto-char (point-min))))

(defun tags-loop-continue-1 (&optional first-time)
  "Continue last \\[tags-search] or \\[tags-query-replace] command.
Used noninteractively with non-nil argument
to begin such a command.  See variable tags-loop-form.
Uses another window the first time (unlike tags-loop-continue)."
  (interactive)
  (if first-time
      (progn 
	(next-file-other-window t)
	(goto-char (point-min))))
  (while (not (eval tags-loop-form))
    (next-file)
    (message "Scanning file %s..." buffer-file-name)
    (goto-char (point-min))))

(defun tags-search (regexp)
  "Search through all files listed in tag table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable tags-file-name."
  (interactive "sTags search (regexp): ")
  (if (and (equal regexp "")
	   (eq (car tags-loop-form) 're-search-forward))
      (tags-loop-continue nil)
    (setq tags-loop-form
	  (list 're-search-forward regexp nil t))
    (tags-loop-continue t)))

(defun tags-search-other-window (regexp)
  "Search in other window through all files listed in tag table for match for REGEXP.
Stops when a match is found.
To continue searching for next match, use command \\[tags-loop-continue].

See documentation of variable tags-file-name."
;  (interactive "sTags search (regexp): ")
  (interactive (find-tag-tag "Tags search (regexp): "))
  (if (and (equal regexp "")
	   (eq (car tags-loop-form) 're-search-forward))
      (tags-loop-continue-1 nil)
    (setq tags-loop-form
	  (list 're-search-forward regexp nil t))
    (tags-loop-continue-1 t)))

(defun tags-query-replace (from to &optional delimited)
  "Query-replace-regexp FROM with TO through all files listed in tag table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (C-G or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable tags-file-name."
  (interactive "sTags query replace (regexp): \nsTags query replace %s by: \nP")
  (setq tags-loop-form
	(list 'and (list 'save-excursion
			 (list 're-search-forward from nil t))
	      (list 'not (list 'perform-replace from to t t 
			       (not (null delimited))))))
  (tags-loop-continue t))

(defun tags-query-replace-other-window (from to &optional delimited)
  "Query-replace-regexp FROM with TO in other window through all files listed in tag table.
Third arg DELIMITED (prefix arg) means replace only word-delimited matches.
If you exit (C-G or ESC), you can resume the query-replace
with the command \\[tags-loop-continue].

See documentation of variable tags-file-name."
  (interactive "sTags query replace (regexp): \nsTags query replace %s by: \nP")
  (setq tags-loop-form
	(list 'and (list 'save-excursion
			 (list 're-search-forward from nil t))
	      (list 'not (list 'perform-replace from to t t 
			       (not (null delimited))))))
  (tags-loop-continue-1 t))

(defun list-tags (string)
  "Display list of tags in file FILE.
FILE should not contain a directory spec
unless it has one in the tag table."
  (interactive "sList tags (in file): ")
  (with-output-to-temp-buffer "*Tags List*"
    (princ "Tags in file ")
    (princ string)
    (terpri)
    (save-excursion
      (let ((tags-files tags-file-names))
	(condition-case err
	    (list-tags-inner string)
	  (error
	   (if tags-files
	       (progn
		 (setq tags-file-name (car tags-files))
		 (setq tags-files (cdr tags-files))
		 (list-tags-inner string))
	     (progn
	       (message (car (cdr err)))
	       (beep)))))))))

(defun list-tags-inner (string)
  "Single tag table file version of list-tags."
  (visit-tags-table-buffer)
  (goto-char 1)
  (search-forward (concat string ","))
  (forward-line 1)
  (while (not (or (eobp) (looking-at "\f")))
    (princ (buffer-substring (point)
			     (progn (skip-chars-forward "^\177")
				    (point))))
    (terpri)
    (forward-line 1)))

(defun tags-apropos (string)
  "Display list of all tags in tag table REGEXP matches."
  (interactive "sTag apropos (regexp): ")
  (with-output-to-temp-buffer "*Tags List*"
    (princ "Tags matching regexp ")
    (prin1 string)
    (terpri)
    (save-excursion
      (let ((tags-files tags-file-names))
	(tags-apropos-inner string)
	(while tags-files
	  (setq tags-file-name (car tags-files))
	  (setq tags-files (cdr tags-files))
	  (tags-apropos-inner string))))))

(defun tags-apropos-inner (string)
  "Single tag table file version of tags-apropos."
  (visit-tags-table-buffer)
  (goto-char 1)
  (while (re-search-forward string nil t)
    (beginning-of-line)
    (princ (buffer-substring (point)
			     (progn (skip-chars-forward "^\177")
				    (point))))
    (terpri)
    (forward-line 1)))
