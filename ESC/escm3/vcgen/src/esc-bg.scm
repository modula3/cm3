;;; esc-bg.scm -- Assemble background predicate (BG_PUSH)
;;;
;;; Copyright (c) 2026, Mika Nystrom.  All rights reserved.
;;;
;;; The background predicate is the conjunction of:
;;; 1. Type axioms (from esc-typegen)
;;; 2. Framing axioms (from esc-framegen)
;;; 3. Abstraction function axioms (from esc-absfunc)
;;; 4. User axioms (SPEC AXIOM)
;;; 5. Standard axioms from esc.ax

;; Assemble the complete BG_PUSH form
;; type-axioms: list of type axioms (from generate-type-axioms)
;; frame-axioms: list of framing axioms (from generate-all-frame-axioms)
;; abstract-axioms: list of abstraction axioms (from process-abstractions)
;; user-axioms: list of user SPEC AXIOM declarations
(define (assemble-bg type-axioms frame-axioms abstract-axioms user-axioms)
  (let ((all-axioms (append type-axioms
                            frame-axioms
                            abstract-axioms
                            user-axioms)))
    (list 'BG_PUSH (apply sx-and all-axioms))))

;; Generate the complete verification bundle for a procedure:
;; Returns a list: (defpredmaps bg-push vc)
(define (generate-vc proc specs type-env)
  (let* (;; Extract spec components
         (proc-spec (find-proc-spec (proc-name proc) specs))
         (ghost-vars (find-ghost-vars specs))
         (abstract-decls (find-abstracts specs))
         (depend-decls (find-depends specs))
         (func-decls (find-funcs specs))
         (axiom-decls (find-axioms specs))

         ;; Generate type axioms
         (type-axioms (generate-type-axioms type-env))

         ;; Process abstractions -> (defpredmaps abstract-axioms)
         (abs-result (process-abstractions abstract-decls depend-decls
                                           func-decls axiom-decls))
         (defpredmaps (car abs-result))
         (abstract-axioms (cadr abs-result))

         ;; Generate framing axioms from DEPEND declarations
         (frame-axioms (generate-all-frame-axioms
                        (build-abstraction-info abstract-decls depend-decls
                                                type-env)))

         ;; Assemble background predicate
         (bg (assemble-bg type-axioms frame-axioms abstract-axioms '()))

         ;; Generate guarded command from procedure body
         (gcmd (generate-gcmd proc-spec (proc-body proc) specs type-env))

         ;; Passify
         (passified (let-values (((cmd sigma) (passify gcmd '())))
                      cmd))

         ;; Weakest precondition
         (vc (generate-vc-formula passified)))

    ;; Return the three components
    (list defpredmaps bg vc)))

;; Spec filtering functions
(define (find-proc-spec name specs)
  (let loop ((s specs))
    (cond ((null? s) #f)
          ((and (pair? (car s)) (eq? (caar s) 'proc-spec)
                (eq? (cadar s) name))
           (car s))
          (else (loop (cdr s))))))

(define (find-ghost-vars specs)
  (esc-filter (lambda (s) (and (pair? s) (eq? (car s) 'ghost-var))) specs))

(define (find-abstracts specs)
  (esc-filter (lambda (s) (and (pair? s) (eq? (car s) 'abstract))) specs))

(define (find-depends specs)
  (esc-filter (lambda (s) (and (pair? s) (eq? (car s) 'depend))) specs))

(define (find-funcs specs)
  (esc-filter (lambda (s) (and (pair? s) (eq? (car s) 'func))) specs))

(define (find-axioms specs)
  (esc-filter (lambda (s) (and (pair? s) (eq? (car s) 'axiom))) specs))

;; Build abstraction info list for frame axiom generation
(define (build-abstraction-info abstract-decls depend-decls type-env)
  ;; Stub: returns empty list for now
  '())

;; Procedure accessors (stub)
(define (proc-name p) (if (pair? p) (cadr p) p))
(define (proc-body p) (if (and (pair? p) (> (length p) 2)) (cddr p) '()))
