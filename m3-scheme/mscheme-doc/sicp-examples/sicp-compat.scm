;;; sicp-compat.scm --- Compatibility prelude for SICP 1st edition code
;;;
;;; The first edition of SICP (Abelson & Sussman, 1985) uses MIT Scheme
;;; conventions that predate R4RS.  This file defines the missing
;;; primitives so the examples run under MScheme.
;;;
;;; Usage:  (load "sicp-compat.scm")   ; or (require-modules "sicp-compat")

;;; ---- Boolean constants ----
;;; MIT Scheme used t/nil; MScheme uses #t/#f.  Many SICP examples
;;; return nil for false.  We bind nil to the empty list (which is
;;; false in MIT Scheme but not in R4RS).  Code that tests (if nil ...)
;;; should work because MScheme treats '() as false.

(define nil '())
(define t   #t)

;;; ---- Sequencing ----
;;; First edition uses (sequence e1 e2 ...) where R4RS uses (begin ...).

(define sequence
  (macro args
    (cons 'begin args)))

;;; ---- Output ----
;;; MIT Scheme's print = write + newline; princ = display (no newline).

(define (princ x) (display x))

(define (print x) (write x) (newline))

;;; ---- Arithmetic helpers ----
;;; MIT Scheme had 1+ and -1+ as primitives.

(define (1+ x) (+ x 1))
(define (-1+ x) (- x 1))

;;; ---- Predicate ----
;;; MIT Scheme's atom? --- true for anything that is not a pair.

(define (atom? x) (not (pair? x)))

;;; ---- Timing ----
;;; MIT Scheme's (runtime) returns process time in seconds.
;;; Map to MScheme's timenow (wall-clock time).

(define runtime timenow)

;;; ---- Streams ----
;;; The first edition stream library uses cons-stream/head/tail/
;;; the-empty-stream/empty-stream?.  We implement lazy streams
;;; using MScheme's delay/force.

(define the-empty-stream '())

(define (empty-stream? s) (null? s))

(define cons-stream
  (macro (a b)
    (list 'cons a (list 'delay b))))

(define (head s) (car s))

(define (tail s) (force (cdr s)))

(define (singleton x) (cons-stream x the-empty-stream))

;;; ---- assq compatibility ----
;;; MIT Scheme's assq returns '() (nil) on failure; R4RS/MScheme
;;; returns #f.  SICP code tests (null? (assq ...)) which fails
;;; with #f.  We override assq to return '() on miss.

(define %mscheme-assq assq)
(define (assq key alist)
  (let ((result (%mscheme-assq key alist)))
    (if result result '())))

;;; ---- Misc ----
;;; error is already provided by MScheme.
;;; random is already provided by MScheme (returns [0,1) float).
;;; For SICP's (random n) which returns an integer 0..n-1:

(define (sicp-random n)
  (floor (* (random) n)))
