;;; Last modified on Mon Aug 16 16:54:37 PDT 1993 by detlefs 
;;;      modified on Wed Dec  2 16:15:35 PST 1992 by wobber 


;;; Provides specials shell-mode's for running debuggers on Modula-3
;;; programs.

;;; In particular it provides the commands 'run-m3-gdb' and 'run-m3-dbx', which
;;; run gdb and dbx, respectively in inferior shell buffers.  These shells are
;;; in 'gdb-mode' and 'dbx-mode', respectively, which provide things like
;;; tracking the current line in the current source file with the overlay
;;; error, allowing the setting of breakpoints with easy commands in the
;;; source buffers, etc.  See the documentation on those modes for more
;;; details.

;;; The main advantage gained by invoking gdb-mode/dbx-mode through this
;;; interface is the use of Steve Glassman's hack to print the values of
;;; variables in code produced by the SRC compiler.  These commands bind
;;; M-p to a command that considers the word containing the point as a REF or
;;; OBJECT value (either a variable or an address), and print the type and
;;; value of the referent.  M-r does the same with record values.

;;; The normal usage of this file is to have an 'autoload' command in your
;;; .emacs:

;;; (autoload 'run-m3-gdb "m3-debug" "" t)
;;; (autoload 'run-m3-dbx "m3-debug" "" t)

;;; Then M-x run-m3-gdb will invoke gdb-mode.

(require 'modula3)

(cond
 ((emacs19-p)
  (load-library "cmushell"))
 (t
  (require 'shell)))

(if (emacs19-p)
    (load-library "gud")
  (load-library "gdb")
  (load-library "dbx"))

(defvar m3-debug-added-gdb-bindings nil)

(defun m3-debug-add-bindings (&optional map)
  (if (not map) (setq map (current-local-map)))
  (cond
   ((emacs19-p)
    (define-key map "\C-cp" 'm3-pref)
    (define-key map "\C-cr" 'm3-prec))
   (t
    (define-key map "\ep" 'm3-pref)
    (define-key map "\er" 'm3-prec))))

(defun m3-debug-gdb-shell ()
  (interactive)
  (m3-debug-add-bindings shell-mode-map))

(defun m3-debug-set-pstr-gdb ()
  (setq m3-debug-pstr-start m3-debug-pstr-start-gdb)
  (setq m3-debug-pstr-end m3-debug-pstr-end-gdb))
   

(defun m3-debug-add-gdb-bindings ()
  (or m3-debug-added-gdb-bindings
      (progn (m3-debug-add-bindings)
	     (setq m3-debug-added-gdb-bindings t))))


(defun run-m3-gdb (file)
  "Run gdb on m3 program FILE in buffer *gdb-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for GDB.  If you wish to change this, use
the GDB commands `cd DIR' and `directory'."
  (interactive "FRun gdb on file: ")
  (let ((gdb-mode-hook
	 (cond
	  ((not (boundp 'gdb-mode-hook))
	   (list 'm3-debug-add-gdb-bindings 'm3-debug-set-pstr-gdb))
	  ((listp gdb-mode-hook)
	   (cons 'm3-debug-add-gdb-bindings
		 (cons 'm3-debug-set-pstr-gdb gdb-mode-hook)))
	  (t
	   (list 'm3-debug-add-gdb-bindings 'm3-debug-set-pstr-gdb
		 gdb-mode-hook)))))
    (if (not (emacs19-p))
	(define-key ctl-x-map " " 'gdb-break))
    (gdb file)))

;;;--------------------------------------------------

(defvar m3-debug-added-dbx-bindings nil)

(defun m3-debug-set-pstr-dbx ()
  (setq m3-debug-pstr-start m3-debug-pstr-start-dbx)
  (setq m3-debug-pstr-end m3-debug-pstr-end-dbx))

(defun m3-debug-add-dbx-bindings ()
  (or m3-debug-added-dbx-bindings
      (progn (m3-debug-add-bindings)
	     (setq m3-debug-added-dbx-bindings t))))

(defun run-m3-dbx (file)
  "Run dbx on m3 program FILE in buffer *dbx*.
The directory containing FILE becomes the initial working directory
and source-file directory for DBX.  If you wish to change this, use
the DBX commands `cd DIR' and `directory'."
  (interactive "FRun dbx on file: ")
  (setq file (expand-file-name file))

  (let ((dbx-mode-hook
	 (cond
	  ((not (boundp 'dbx-mode-hook))
	   (list 'm3-debug-add-dbx-bindings 'm3-debug-set-pstr-dbx))
	  ((listp dbx-mode-hook)
	   (cons 'm3-debug-add-dbx-bindings
		 (cons 'm3-debug-set-pstr-dbx dbx-mode-hook)))
	  (t
	   (list 'm3-debug-add-dbx-bindings 'm3-debug-set-pstr-dbx
		 dbx-mode-hook)))))
    (cond
     ((emacs19-p)
      (dbx file))
     (t
      (define-key ctl-x-map " " 'dbx-stop-at))
     (run-dbx file))))


;;;======================================================================  
(defvar m3-debug-old-filter)
(defvar m3-debug-accum)
(defvar m3-debug-var)

(defun m3-debug-pref-filter (proc str)
 (condition-case nil
  (progn
  (set 'm3-debug-accum (concat m3-debug-accum str))
  (let ((end (string-match "\n\(g?dbx?\)" m3-debug-accum)))
     (if end
        (let* ((q1 (string-match "\"" m3-debug-accum))
               (dot (string-match "\\." m3-debug-accum q1))
               (q2 (string-match "\"" m3-debug-accum (1+ q1)))
               (cmd
                   (concat "print *("
                     (if dot 
                       (concat (substring m3-debug-accum (1+ q1) dot) "__"
                         (substring m3-debug-accum (1+ dot) q2))
                       (substring m3-debug-accum (1+ q1) q2))
                   ")(" m3-debug-var ")\n")))
          (set-process-filter proc m3-debug-old-filter)
          (insert (concat "\n" cmd))
          (process-send-string proc cmd)
          (set-marker (process-mark proc) (point-max))
       )))
  )
  (error 
    (progn
      (set-process-filter proc m3-debug-old-filter)
      (insert "\nBad input! (not a ref)\n")
      (process-send-string proc "\n")
      (set-marker (process-mark proc) (point-max))))))

(defun m3-debug-prec-filter (proc str)
 (condition-case nil
  (progn
  (set 'm3-debug-accum (concat m3-debug-accum str))
  (let ((end (string-match "\n\(g?dbx?\)" m3-debug-accum)))
     (if end
        (let* ((type (string-match "=" m3-debug-accum))
               (star (string-match "*" m3-debug-accum type))
               (nl (string-match "\n" m3-debug-accum type))
               (cmd
                   (concat "print "
                     (if star "*(" "(")
                     (substring m3-debug-accum (1+ type) nl)
                   ")(" m3-debug-var ")\n")))
          (set-process-filter proc m3-debug-old-filter)
          (insert (concat "\n" cmd))
          (process-send-string proc cmd)
          (set-marker (process-mark proc) (point-max))
       )))
  )
  (error 
    (progn
      (set-process-filter proc m3-debug-old-filter)
      (insert "\nBad input! (not a record)\n")
      (process-send-string proc "\n")
      (set-marker (process-mark proc) (point-max))))))

(defun m3-get-selection (pat)
   (save-excursion
     (let ((p1 (condition-case nil
                (progn (re-search-backward pat) (forward-char) (point))
                (error (point-min))))
           (p2 (condition-case nil
                (progn (re-search-forward pat) (backward-char) (point))
                (error (point-max))))
     )
     (buffer-substring p1 p2))))

(defun m3-get-selection-or-prev (p pat)
   (save-excursion (goto-char p) (m3-get-selection pat)))

(defun m3-prec ()
  "Prints the value of a record."
  (interactive nil)
  (let ((process (get-buffer-process (current-buffer)))
        (p (point)) (m (mark)) (max (point-max)))

    (goto-char max)
    (set 'm3-debug-old-filter (process-filter process))
    (set 'm3-debug-var (m3-get-selection-or-prev p "[^a-zA-Z0-9_.*]"))
    (set 'm3-debug-accum nil)
    (set-process-filter process 'm3-debug-prec-filter)

    (process-send-string process (concat "whatis " m3-debug-var "\n"))))


(defvar m3-debug-pstr-start-gdb "print ((_TYPE*)(_types[((*(int *)(")
(defvar m3-debug-pstr-start-dbx "print _types[((*(int *)(")

(defvar m3-debug-pstr-end-gdb "-4)) & 0x1fffff) / 2]))->name\n")
(defvar m3-debug-pstr-end-dbx "-4)) & 0x1fffff) div 2]->name\n")

(defvar m3-debug-pstr-start m3-debug-pstr-start-gdb)
(defvar m3-debug-pstr-end m3-debug-pstr-end-gdb)
(make-variable-buffer-local 'm3-debug-pstr-start)
(make-variable-buffer-local 'm3-debug-pstr-end)

(defun m3-pref ()
  "Prints the referent of a REF value."
  (interactive nil)
  (let ((process (get-buffer-process (current-buffer)))
        (p (point)) (m (if (emacs19-p) (mark t) (mark))) (max (point-max)))

    (goto-char max)
    (set 'm3-debug-old-filter (process-filter process))
    (set 'm3-debug-var (m3-get-selection-or-prev p "[^a-zA-Z0-9_.*]"))
    (set 'm3-debug-accum nil)
    (set-process-filter process 'm3-debug-pref-filter)

    (process-send-string process 
      (concat m3-debug-pstr-start m3-debug-var m3-debug-pstr-end))))

