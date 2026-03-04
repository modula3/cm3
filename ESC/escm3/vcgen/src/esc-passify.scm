;;; esc-passify.scm -- Flanagan-Saxe passification transform
;;;
;;; Copyright (c) 2026, Mika Nystrom.  All rights reserved.
;;;
;;; Passification converts assignments into assumptions about fresh
;;; SSA variables. After passification, no variable is assigned more
;;; than once. This produces compact verification conditions
;;; (quadratic worst-case, near-linear typical).
;;;
;;; Input: guarded command with (assign x e)
;;; Output: guarded command with only (assume ...) and (assert ...)
;;;
;;; Transform rules:
;;;   (assign x e)  ->  (var x_new (assume (= x_new e[x->x_old])))
;;;   (havoc vars)  ->  (var x1_new (var x2_new ...))
;;;
;;; The sigma map tracks variable -> current SSA name.

;; Passify a guarded command.
;; Returns (values passified-cmd sigma')
;; where sigma is a mutable alist mapping variable names to SSA names.

(define (passify cmd sigma)
  (let ((kind (if (pair? cmd) (car cmd) 'skip)))
    (cond
     ((eq? kind 'assign)
      (passify-assign cmd sigma))
     ((eq? kind 'assume)
      (passify-assume cmd sigma))
     ((eq? kind 'assert)
      (passify-assert cmd sigma))
     ((eq? kind 'seq)
      (passify-seq cmd sigma))
     ((eq? kind 'choice)
      (passify-choice cmd sigma))
     ((eq? kind 'var)
      (passify-var cmd sigma))
     ((eq? kind 'havoc)
      (passify-havoc cmd sigma))
     ((eq? kind 'skip)
      (values '(skip) sigma))
     (else
      (values cmd sigma)))))

;; (assign x e) -> (assume (= x_new (subst e sigma)))
;; and update sigma: x -> x_new
(define (passify-assign cmd sigma)
  (let* ((x (cadr cmd))
         (e (caddr cmd))
         (e-subst (subst-expr e sigma))
         (x-new (esc-gensym (symbol->string x))))
    (set! sigma (cons (cons x x-new) sigma))
    (values `(assume ,(sx-eq x-new e-subst)) sigma)))

;; (assume e) -> (assume (subst e sigma))
(define (passify-assume cmd sigma)
  (values `(assume ,(subst-expr (cadr cmd) sigma)) sigma))

;; (assert e) -> (assert (subst e sigma))
(define (passify-assert cmd sigma)
  (values `(assert ,(subst-expr (cadr cmd) sigma)) sigma))

;; (seq s1 s2 ...) -> thread sigma through sequentially
(define (passify-seq cmd sigma)
  (let loop ((stmts (cdr cmd))
             (accum '())
             (sig sigma))
    (if (null? stmts)
        (values `(seq ,@(reverse accum)) sig)
        (let-values (((s' sig') (passify (car stmts) sig)))
          (loop (cdr stmts) (cons s' accum) sig')))))

;; (choice s1 s2) -> each branch gets a copy of sigma;
;; after choice, we merge by introducing fresh variables
;; for anything modified in either branch.
(define (passify-choice cmd sigma)
  (let* ((s1 (cadr cmd))
         (s2 (caddr cmd)))
    (let-values (((s1' sig1) (passify s1 (copy-sigma sigma)))
                 ((s2' sig2) (passify s2 (copy-sigma sigma))))
      ;; Find variables that differ between sig1 and sig2
      (let* ((changed (sigma-diff sig1 sig2 sigma))
             (merged-sigma sigma))
        ;; For each changed variable, introduce a fresh merge variable
        (for-each
         (lambda (v)
           (let ((v-merge (esc-gensym (symbol->string v))))
             (set! merged-sigma (cons (cons v v-merge) merged-sigma))
             ;; Add assume equalities to each branch
             (set! s1' `(seq ,s1' (assume ,(sx-eq v-merge
                                            (sigma-lookup v sig1)))))
             (set! s2' `(seq ,s2' (assume ,(sx-eq v-merge
                                            (sigma-lookup v sig2)))))))
         changed)
        (values `(choice ,s1' ,s2') merged-sigma)))))

;; (var x body) -> passify body with x mapped to fresh
(define (passify-var cmd sigma)
  (let* ((x (cadr cmd))
         (body (caddr cmd))
         (x-new (esc-gensym (symbol->string x))))
    (set! sigma (cons (cons x x-new) sigma))
    (let-values (((body' sig') (passify body sigma)))
      (values `(var ,x-new ,body') sig'))))

;; (havoc vars) -> introduce fresh names for each variable
(define (passify-havoc cmd sigma)
  (let loop ((vars (cadr cmd))
             (sig sigma))
    (if (null? vars)
        (values '(skip) sig)
        (let ((v-new (esc-gensym (symbol->string (car vars)))))
          (loop (cdr vars)
                (cons (cons (car vars) v-new) sig))))))

;; Substitute variables in an expression using sigma
(define (subst-expr expr sigma)
  (cond
   ((symbol? expr)
    (sigma-lookup expr sigma))
   ((pair? expr)
    (cons (subst-expr (car expr) sigma)
          (subst-expr (cdr expr) sigma)))
   (else expr)))

;; Sigma operations

(define (sigma-lookup var sigma)
  (let ((pair (assoc var sigma)))
    (if pair (cdr pair) var)))

(define (copy-sigma sigma)
  (map (lambda (p) (cons (car p) (cdr p))) sigma))

(define (sigma-diff sig1 sig2 base)
  ;; Return list of variables that have different values in sig1 vs sig2
  (let ((changed '()))
    (for-each
     (lambda (pair)
       (let ((v (car pair)))
         (unless (eq? (sigma-lookup v sig1) (sigma-lookup v sig2))
           (set! changed (cons v changed)))))
     (append sig1 sig2))
    ;; Deduplicate
    (let loop ((lst changed) (seen '()) (result '()))
      (cond ((null? lst) result)
            ((memq (car lst) seen) (loop (cdr lst) seen result))
            (else (loop (cdr lst) (cons (car lst) seen)
                        (cons (car lst) result)))))))
