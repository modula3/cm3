;;; esc-emit.scm -- Serialize formulas with labels for error localization
;;;
;;; Copyright (c) 2026, Mika Nystrom.  All rights reserved.
;;;
;;; Wraps assertion formulas with LBL (label) forms for error reporting.
;;; Labels follow the pattern:
;;;   |ERROR.kind.line.col|
;;;   |ERROR.kind.line.col.context.extra|
;;;
;;; Error kinds:
;;;   deref     -- null dereference
;;;   bounds    -- array bounds violation
;;;   pre       -- precondition violation
;;;   post      -- postcondition violation
;;;   loopinv   -- loop invariant violation
;;;   lock      -- locking discipline violation
;;;   assert    -- assertion violation
;;;   modifies  -- modifies clause violation

;; Emit a formula with labels wrapping assertions
;; Returns S-expression text suitable for Simplify input
(define (emit-vc vc wr)
  (write-sx vc wr))

;; Write an S-expression to a writer, with pretty-printing
(define (write-sx sx wr)
  (cond
   ((null? sx) (display "()" wr))
   ((pair? sx)
    (display "(" wr)
    (write-sx (car sx) wr)
    (let loop ((rest (cdr sx)))
      (cond
       ((null? rest) (display ")" wr))
       ((pair? rest)
        (display " " wr)
        (write-sx (car rest) wr)
        (loop (cdr rest)))
       (else
        (display " . " wr)
        (write-sx rest wr)
        (display ")" wr)))))
   ((symbol? sx)
    ;; Symbols with special chars need pipe quoting
    (let ((s (symbol->string sx)))
      (if (needs-pipe-quoting s)
          (begin (display "|" wr) (display s wr) (display "|" wr))
          (display s wr))))
   ((number? sx)
    (display sx wr))
   ((string? sx)
    (display "\"" wr) (display sx wr) (display "\"" wr))
   ((boolean? sx)
    (display (if sx "|@true|" "|@false|") wr))
   (else
    (display sx wr))))

;; Check if a symbol name needs pipe quoting
(define (needs-pipe-quoting s)
  (let ((len (string-length s)))
    (if (= len 0) #t
        (let loop ((i 0))
          (if (>= i len) #f
              (let ((c (string-ref s i)))
                (if (or (char-alphabetic? c) (char-numeric? c)
                        (char=? c '_') (char=? c '-'))
                    (loop (+ i 1))
                    #t)))))))

;; Create an error label atom
(define (make-error-label kind line col . extra)
  (string->symbol
   (apply string-append
     (append (list "ERROR." kind "." (number->string line)
                   "." (number->string col))
             (map (lambda (s) (string-append "." s)) extra)))))

;; Wrap an assertion with an error label
(define (labeled-assert kind line col formula . extra)
  (let ((label (apply make-error-label kind line col extra)))
    (sx-lbl label formula)))

;; Wrap a postcondition assertion
(define (labeled-post line col context formula)
  (labeled-assert "post" line col formula context))

;; Wrap a precondition assertion
(define (labeled-pre line col formula)
  (labeled-assert "pre" line col formula))

;; Wrap a null dereference check
(define (labeled-deref line col expr)
  (labeled-assert "deref" line col (sx-neq expr '|$NIL|)))

;; Wrap an array bounds check
(define (labeled-bounds line col index length)
  (labeled-assert "bounds" line col
    (sx-and (sx-le 0 index) (sx-lt index length))))

;; Wrap a loop invariant check
(define (labeled-loopinv line col formula)
  (labeled-assert "loopinv" line col formula))

;; Wrap a locking discipline check
(define (labeled-lock line col formula)
  (labeled-assert "lock" line col formula))
