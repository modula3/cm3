;; Parsing compiler error messages in a shell buffer
;; Copyright (C) 1985, 1986 Free Software Foundation, Inc.

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


;;; Last modified on Mon Nov 30 16:27:01 PST 1992 by mjordan

;; This is a hack of the standard compile code to work in a
;; *shell* buffer. The function shell-next-error, bound to
;; C-cC-v, takes the next error message off the list held in
;; shell-compilation-error-list, and behaves exactly as the
;; standard next-error function. If the list is empty, new
;; errors are searched for starting at the buffer position given
;; by the variable shell-compilation-parsing-end, which 
;; initially has the value 1. You can reset the value with
;; C-cC-s, which sets the variable to the cursor position, 
;; and throws away the current list of errors. You can use
;; this to give up on a batch of errors by keying C-cC-s
;; with the cursor set after the last error message, or revisit
;; an earlier message by keying C-cC-s with the cursor set
;; just before the message and then keying C-cC-n.

;; It is left up to you to decide if you want to rebind
;; the standard key binding for next-error (C-x`) to
;; shell-next-error.

;; 16-Oct-92: Modified shell-compilation-grab-filename to
;; support Modula-3 generic instantiations. E.g.
;; "generic.ig[instance.i3], line n,m: ..."

(require 'shell)
(provide 'shell-parse-errors)

(defvar shell-compilation-error-list nil
  "List of error message descriptors for visiting erring functions.
Each error descriptor is a list of length two.
Its car is a marker pointing to an error message.
Its cadr is a marker pointing to the text of the line the message is about,
  or nil if that is not interesting.")

(defvar shell-compilation-parsing-end 1
  "Position of end of buffer when last error messages parsed.")

(defvar shell-compilation-error-message nil
  "Message to print when no more matches for shell-compilation-error-regexp are found")

;; The filename excludes colons to avoid confusion when error message
;; starts with digits.
(defvar shell-compilation-error-regexp
  "\\([^ :\n]+\\(: *\\|, line \\|(\\)[0-9]+\\(,\\([0-9]+\\)\\)?\\)\\|\\([0-9]+ *of *[^ \n]+\\)"
  "Regular expression for filename/linenumber in error in compilation log.")

(defun shell-next-error (&optional argp)
  "Visit next compilation error message and corresponding source code.
This operates on the output from the \\[shell-parse-errors] command.
If all preparsed error messages have been processed,
the error message buffer is checked for new ones."
  (interactive)
  (if shell-compilation-error-list
      nil
    (save-excursion
      (set-buffer "*shell*")
      (shell-compilation-parse-errors)))
  (let ((next-error (car shell-compilation-error-list)))
    (if (null next-error)
	(message "No more errors")
        (setq shell-compilation-error-list (cdr shell-compilation-error-list))
        (if (null (car (cdr next-error)))
          ;; file was not visitable
          (progn
             (beep)
             (message "Can't visit file"))
         ;; display file in buffer
         (if (eq (current-buffer) (marker-buffer (car next-error)))
           ;; first time avoid swapping windows
           (switch-to-buffer-other-window (marker-buffer (car (cdr next-error))))
           (switch-to-buffer (marker-buffer (car (cdr next-error)))))
         (goto-char (car (cdr next-error)))
         (set-marker (car (cdr next-error)) nil))
       (let* ((pop-up-windows t)
	      (w (display-buffer (marker-buffer (car next-error)))))
         (set-window-point w (car next-error))
         (set-window-start w (car next-error)))
       (set-marker (car next-error) nil))))

;; Set shell-compilation-error-list to nil, and
;; unchain the markers that point to the error messages and their text,
;; so that they no longer slow down gap motion.
;; This would happen anyway at the next garbage collection,
;; but it is better to do it right away.
(defun shell-compilation-forget-errors ()
  (if (eq shell-compilation-error-list t)
      (setq shell-compilation-error-list nil))
  (while shell-compilation-error-list
    (let ((next-error (car shell-compilation-error-list)))
      (set-marker (car next-error) nil)
      (if (car (cdr next-error))
	  (set-marker (car (cdr next-error)) nil)))
    (setq shell-compilation-error-list (cdr shell-compilation-error-list))))

(defun shell-compilation-parse-errors ()
  "Parse the current buffer as error messages.
This makes a list of error descriptors, shell-compilation-error-list.
For each source-file, line-number pair in the buffer,
the source file is read in, and the text location is saved in shell-compilation-error-list.
The function shell-next-error, assigned to \\[shell-next-error], takes the next error off the list
and visits its location."
  (setq shell-compilation-error-list nil)
  (message "Parsing error messages...")
  (let (text-buffer
	last-filename last-linenum last-linepos)
    ;; Don't reparse messages already seen at last parse.
    (goto-char shell-compilation-parsing-end)
    ;; Don't parse the first two lines as error messages.
    ;; This matters for grep.
    (if (bobp)
	(forward-line 2))
    (while (re-search-forward shell-compilation-error-regexp nil t)
      (let (linenum (linepos 0) filename
	    error-marker text-marker)
	;; Extract file name and line number from error message.
	(save-restriction
	  (narrow-to-region (match-beginning 0) (match-end 0))
	  (goto-char (point-max))
	  (skip-chars-backward "[0-9]")
	  ;; If it's a lint message, use the last file(linenum) on the line.
	  ;; Normally we use the first on the line.
	  (if (= (preceding-char) ?\()
	      (progn
		(narrow-to-region (point-min) (1+ (buffer-size)))
		(end-of-line)
		(re-search-backward shell-compilation-error-regexp)
		(skip-chars-backward "^ \t\n")
		(narrow-to-region (point) (match-end 0))
		(goto-char (point-max))
		(skip-chars-backward "[0-9]")))
	  ;; Are we looking at a "filename-first" or "line-number-first" form?
	  (if (looking-at "[0-9]")
	      (progn
                (if (= (preceding-char) ?,)
                  (progn
                    (save-excursion
                      (setq linepos (read (current-buffer))))
                    (backward-char 1)
                    (narrow-to-region (point-min) (point))
                    (skip-chars-backward "[0-9]")))
		(setq linenum (read (current-buffer)))
		(goto-char (point-min)))
	    ;; Line number at start, file name at end.
	    (progn
	      (goto-char (point-min))
	      (setq linenum (read (current-buffer)))
	      (goto-char (point-max))
	      (skip-chars-backward "^ \t\n")))
	  (setq filename (shell-compilation-grab-filename)))
	;; Locate the erring file and line.
	(if (and (equal filename last-filename)
		 (and (= linenum last-linenum) (= linepos last-linepos)))
	    nil
	  (beginning-of-line 1)
	  (setq error-marker (point-marker))
	  ;; text-buffer gets the buffer containing this error's file.
	  (if (not (equal filename last-filename))
	      (setq text-buffer
		    (and (file-exists-p (setq last-filename filename))
			 (find-file-noselect filename))
		    last-linenum 0))
	  (if text-buffer
	      ;; Go to that buffer and find the erring line.
	      (save-excursion
		(set-buffer text-buffer)
		(if (zerop last-linenum)
		    (progn
		      (goto-char 1)
		      (setq last-linenum 1)))
		(forward-line (- linenum last-linenum))
                (beginning-of-line 1)
                (forward-char linepos)
		(setq last-linenum linenum)
		(setq last-linepos linepos)
		(setq text-marker (point-marker)))
              ;; prepare to warn user that file is not visitable
              (setq text-marker nil))
	      (setq shell-compilation-error-list
		      (cons (list error-marker text-marker)
			    shell-compilation-error-list)))
	(forward-line 1)))
    (setq shell-compilation-parsing-end (point-max)))
  (message "Parsing error messages...done")
  (setq shell-compilation-error-list (nreverse shell-compilation-error-list)))

(defun shell-set-compilation-parsing-end ()
  "Set the search for error messages to the value of point"
  (interactive)
  (shell-compilation-forget-errors)
  (setq shell-compilation-parsing-end (point)))

(defun shell-parse-errors ()
  "Parse error messages in shell buffer"
 (interactive)
 (shell-compilation-parse-errors))

(defun shell-compilation-grab-filename ()
  "Return a string which is a filename, starting at point.
Ignore quotes and parentheses around it, as well as trailing colons."
  (if (eq (following-char) ?\")
    (save-excursion
      (forward-char)
      (buffer-substring (point)
		        (progn
			  (skip-chars-forward "^\"[")
			  (point))))
    (buffer-substring (point)
		      (progn
			(skip-chars-forward "^ :,\n\t(")
			(point)))))

(global-set-key "\C-c\C-v" 'shell-next-error)
(define-key shell-mode-map "\C-c\C-s" 'shell-set-compilation-parsing-end)
