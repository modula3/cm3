;;; esc-absfunc.scm -- Abstraction function axioms and DEFPREDMAP
;;;
;;; Copyright (c) 2026, Mika Nystrom.  All rights reserved.
;;;
;;; From SPEC ABSTRACT and SPEC DEPEND declarations, generates:
;;;
;;; 1. DEFPREDMAP forms:
;;;    (DEFPREDMAP (|FUNC.M.V| fields... |RESIDUE.M.V|) ind)
;;;    (DEFPREDMAP (|FUNC.M.V| fields... |RESIDUE.M.V|) ind body)
;;;
;;; 2. Abstraction function definition axioms:
;;;    (FORALL (fields... RESIDUE t)
;;;      (PATS (MPAT (select (FUNC fields... RESIDUE) t)))
;;;      (IMPLIES (Is$T t)
;;;        (= (select (FUNC fields... RESIDUE) t) <body>)))

;; Generate DEFPREDMAP for a predicate abstraction (like IntSeq.Valid)
;; func-name: |FUNC.IntSeq.Valid|
;; depend-fields: list of field map symbols
;; residue-name: |RESIDUE.IntSeq.Valid|
;; body: the predicate body, or #f for declaration-only
(define (generate-defpredmap func-name depend-fields residue-name body)
  (let ((header (append (list func-name)
                        depend-fields
                        (list residue-name))))
    (if body
        ;; With body: (DEFPREDMAP (FUNC fields RESIDUE) ind body)
        (list 'DEFPREDMAP header 'ind body)
        ;; Without body: (DEFPREDMAP (FUNC fields RESIDUE) ind)
        (list 'DEFPREDMAP header 'ind))))

;; Generate abstraction function definition axiom
;; For SPEC ABSTRACT M.V[t: T]: body
;; Maps to:
;;   (FORALL (fields... RESIDUE t)
;;     (PATS (MPAT (select (FUNC fields RESIDUE) t)))
;;     (IMPLIES (Is$T t)
;;       (EQ (select (FUNC fields RESIDUE) t) body)))
(define (generate-abstract-axiom func-name depend-fields residue-name
                                  type-pred body)
  (let* ((t (esc-gensym "t"))
         ;; Create parameter variables for each depend field
         (field-vars (map (lambda (f) (esc-gensym (symbol->string f)))
                          depend-fields))
         (res-var (esc-gensym (symbol->string residue-name)))
         ;; Build FUNC application
         (func-app (cons func-name (append field-vars (list res-var))))
         ;; All quantifier variables
         (all-vars (append field-vars (list res-var t)))
         ;; MPAT trigger
         (mpat `(MPAT ,(sx-select func-app t))))
    (sx-forall all-vars
      (list mpat)
      (sx-implies
       (sx-is-pred type-pred t)
       (sx-eq (sx-select func-app t) body)))))

;; Generate a DEFPRED form for a spec function
;; For SPEC FUNC name(formals): body
;; Maps to:
;;   (DEFPRED (name formals) body)
(define (generate-defpred name formals body)
  (list 'DEFPRED (cons name formals) body))

;; Process all abstraction declarations for a module.
;; Returns (list defpredmaps abstract-axioms)
(define (process-abstractions abs-decls depend-decls func-decls axiom-decls)
  (let ((defpredmaps '())
        (axioms '()))

    ;; Process SPEC ABSTRACT + SPEC DEPEND pairs
    (for-each
     (lambda (abs)
       (let* ((name (abstract-name abs))
              (body (abstract-body abs))
              (dep (find-depend name depend-decls))
              (fields (if dep (depend-fields dep) '()))
              (func-name (esc-func-name (abstract-module name)
                                        (abstract-var name)))
              (residue (esc-residue-name (abstract-module name)
                                         (abstract-var name)))
              (type-pred (esc-is-pred (abstract-type name))))
         ;; DEFPREDMAP
         (set! defpredmaps
           (cons (generate-defpredmap func-name fields residue body)
                 defpredmaps))
         ;; Definition axiom
         (when body
           (set! axioms
             (cons (generate-abstract-axiom func-name fields residue
                                            type-pred body)
                   axioms)))))
     abs-decls)

    ;; Process SPEC FUNC declarations
    (for-each
     (lambda (fd)
       (when (func-body fd)
         (set! axioms
           (cons (generate-defpred (func-name fd) (func-formals fd)
                                   (func-body fd))
                 axioms))))
     func-decls)

    ;; Process SPEC AXIOM declarations
    (for-each
     (lambda (ax)
       (set! axioms (cons (axiom-body ax) axioms)))
     axiom-decls)

    (list (reverse defpredmaps) (reverse axioms))))

;; Accessors for abstraction declarations (stubs)
(define (abstract-name a) (list-ref a 0))
(define (abstract-body a) (list-ref a 1))
(define (abstract-module a)
  ;; Extract module part from qualified name "Module.Var"
  (let ((s (symbol->string (abstract-name a))))
    (let ((dot (string-index s #\.)))
      (if dot (string->symbol (substring s 0 dot)) (abstract-name a)))))
(define (abstract-var a)
  (let ((s (symbol->string (abstract-name a))))
    (let ((dot (string-index s #\.)))
      (if dot (string->symbol (substring s (+ dot 1) (string-length s)))
          (abstract-name a)))))
(define (abstract-type a) (if (> (length a) 2) (list-ref a 2) 'T))

(define (find-depend name depend-decls)
  (let loop ((deps depend-decls))
    (cond ((null? deps) #f)
          ((eq? (depend-name (car deps)) name) (car deps))
          (else (loop (cdr deps))))))

(define (depend-name d) (list-ref d 0))
(define (depend-fields d) (list-ref d 1))

(define (func-name f) (list-ref f 0))
(define (func-formals f) (list-ref f 1))
(define (func-body f) (if (> (length f) 2) (list-ref f 2) #f))

(define (axiom-body a) (list-ref a 0))

;; Helper: find index of character in string
(define (string-index s ch)
  (let loop ((i 0))
    (cond ((>= i (string-length s)) #f)
          ((char=? (string-ref s i) ch) i)
          (else (loop (+ i 1))))))
