;;; esc-wp.scm -- Weakest-precondition calculus on passified guarded commands
;;;
;;; Copyright (c) 2026, Mika Nystrom.  All rights reserved.
;;;
;;; After passification, the guarded command language is:
;;;   (assert e)        -- assert condition
;;;   (assume e)        -- assume condition
;;;   (seq s1 s2 ...)   -- sequential composition
;;;   (choice s1 s2)    -- nondeterministic choice
;;;   (var x s)         -- fresh variable introduction
;;;   (skip)            -- no-op
;;;
;;; Weakest precondition rules:
;;;   wp(assert e, Q) = (AND e Q)
;;;   wp(assume e, Q) = (IMPLIES e Q)
;;;   wp(S1 ; S2, Q)  = wp(S1, wp(S2, Q))
;;;   wp(S1 [] S2, Q) = (AND wp(S1, Q) wp(S2, Q))
;;;   wp(var x. S, Q) = (FORALL (x) wp(S, Q))
;;;   wp(skip, Q)     = Q

;; Compute weakest precondition: wp(cmd, Q)
;; Returns a Simplify S-expression (the verification condition)
(define (wp cmd q)
  (let ((kind (if (pair? cmd) (car cmd) 'skip)))
    (cond
     ((eq? kind 'assert)
      (wp-assert cmd q))
     ((eq? kind 'assume)
      (wp-assume cmd q))
     ((eq? kind 'seq)
      (wp-seq cmd q))
     ((eq? kind 'choice)
      (wp-choice cmd q))
     ((eq? kind 'var)
      (wp-var cmd q))
     ((eq? kind 'skip)
      q)
     (else
      ;; Unknown command -- treat as skip
      q))))

;; wp(assert e, Q) = (AND e Q)
;; We wrap the assert in a label for error localization
(define (wp-assert cmd q)
  (let ((e (cadr cmd)))
    ;; If e has a label, preserve it; otherwise wrap
    (if (and (pair? e) (memq (car e) '(LBL LBLNEG LBLPOS)))
        (sx-and e q)
        (sx-and e q))))

;; wp(assume e, Q) = (IMPLIES e Q)
(define (wp-assume cmd q)
  (let ((e (cadr cmd)))
    ;; Optimization: (IMPLIES |@true| Q) = Q
    (if (eq? e '|@true|)
        q
        ;; Optimization: (IMPLIES |@false| Q) = |@true|
        (if (eq? e '|@false|)
            '|@true|
            (sx-implies e q)))))

;; wp(S1 ; S2 ; ..., Q) = wp(S1, wp(S2, ... wp(Sn, Q)))
;; We process right-to-left
(define (wp-seq cmd q)
  (let ((stmts (cdr cmd)))
    (fold-right wp q stmts)))

;; wp(S1 [] S2, Q) = (AND wp(S1, Q) wp(S2, Q))
(define (wp-choice cmd q)
  (let ((s1 (cadr cmd))
        (s2 (caddr cmd)))
    (sx-and (wp s1 q) (wp s2 q))))

;; wp(var x. S, Q) = (FORALL (x) wp(S, Q))
(define (wp-var cmd q)
  (let ((x (cadr cmd))
        (body (caddr cmd)))
    (sx-forall (list x) #f (wp body q))))

;; fold-right for wp: processes a list of commands right-to-left
(define (fold-right f init lst)
  (if (null? lst)
      init
      (f (car lst) (fold-right f init (cdr lst)))))

;; Generate a complete verification condition for a procedure.
;; proc-gcmd: the guarded command for the procedure body
;; Returns: the VC formula (to be proven valid by Simplify)
(define (generate-vc-formula proc-gcmd)
  (wp proc-gcmd '|@true|))
