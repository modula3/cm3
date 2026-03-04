;;; esc-util.scm -- shared utilities for ESC/M3 VCG
;;;
;;; Copyright (c) 2026, Mika Nystrom.  All rights reserved.

;; S-expression constructors matching Simplify syntax

(define (sx-and . args)
  (cond ((null? args) '|@true|)
        ((null? (cdr args)) (car args))
        (else (cons 'AND args))))

(define (sx-or . args)
  (cond ((null? args) '|@false|)
        ((null? (cdr args)) (car args))
        (else (cons 'OR args))))

(define (sx-not e)
  (list 'NOT e))

(define (sx-implies a b)
  (list 'IMPLIES a b))

(define (sx-iff a b)
  (list 'IFF a b))

(define (sx-eq a b)
  (list 'EQ a b))

(define (sx-neq a b)
  (list 'NEQ a b))

(define (sx-lt a b)
  (list '< a b))

(define (sx-le a b)
  (list '<= a b))

(define (sx-gt a b)
  (list '> a b))

(define (sx-ge a b)
  (list '>= a b))

(define (sx-plus a b)
  (list '+ a b))

(define (sx-minus a b)
  (list '- a b))

(define (sx-times a b)
  (list '* a b))

(define (sx-select map index)
  (list 'select map index))

(define (sx-store map index val)
  (list 'store map index val))

(define (sx-number arr dim)
  (list 'NUMBER arr dim))

(define (sx-forall vars pats body)
  (if pats
      (list 'FORALL vars (cons 'PATS pats) body)
      (list 'FORALL vars body)))

(define (sx-exists vars body)
  (list 'EXISTS vars body))

(define (sx-lbl name body)
  (list 'LBL name body))

(define (sx-lblneg name body)
  (list 'LBLNEG name body))

(define (sx-lblpos name body)
  (list 'LBLPOS name body))

(define (sx-distinct . atoms)
  (cons 'DISTINCT atoms))

(define (sx-is-pred type-pred x)
  (sx-eq (list type-pred x) '|@true|))

(define (sx-subtype tc1 tc2)
  (sx-eq (list 'SUBTYPE tc1 tc2) '|@true|))

(define (sx-subtype1 tc1 tc2)
  (sx-eq (list 'SUBTYPE1 tc1 tc2) '|@true|))

(define (sx-typecode x)
  (list 'TYPECODE x))

(define (sx-addr x)
  (list 'ADDR x))

;; Naming conventions

(define (esc-field-name module type field)
  (string->symbol
   (string-append (symbol->string module) "."
                  (symbol->string type) "."
                  (symbol->string field))))

(define (esc-is-pred type-name)
  (string->symbol
   (string-append "Is$" (symbol->string type-name))))

(define (esc-func-name module var)
  (string->symbol
   (string-append "FUNC." (symbol->string module) "."
                  (symbol->string var))))

(define (esc-residue-name module var)
  (string->symbol
   (string-append "RESIDUE." (symbol->string module) "."
                  (symbol->string var))))

(define (esc-typecode-name type-name)
  (string->symbol
   (string-append (symbol->string type-name) ".TYPECODE")))

(define (esc-error-label kind line col . extra)
  (string->symbol
   (apply string-append
     "ERROR." kind "." (number->string line) "." (number->string col)
     (map (lambda (s) (string-append "." s)) extra))))

;; Fresh variable generation

(define *gensym-counter* 0)

(define (esc-gensym prefix)
  (set! *gensym-counter* (+ *gensym-counter* 1))
  (string->symbol
   (string-append prefix "$$" (number->string *gensym-counter*))))

(define (esc-reset-gensym!)
  (set! *gensym-counter* 0))

;; List utilities

(define (esc-flatmap f lst)
  (apply append (map f lst)))

(define (esc-filter pred lst)
  (cond ((null? lst) '())
        ((pred (car lst)) (cons (car lst) (esc-filter pred (cdr lst))))
        (else (esc-filter pred (cdr lst)))))
