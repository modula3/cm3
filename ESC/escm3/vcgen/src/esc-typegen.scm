;;; esc-typegen.scm -- Generate type axioms for Simplify background
;;;
;;; Copyright (c) 2026, Mika Nystrom.  All rights reserved.
;;;
;;; From the type environment, generates:
;;; - (DISTINCT |T1.TYPECODE| |T2.TYPECODE| ...)
;;; - (FORALL (q) (PATS (|Is$T| q)) (IMPLIES ... subtype/ordinal axioms))
;;; - (EQ (SUBTYPE |NULL.TYPECODE| |T.TYPECODE|) |@true|) for ref types
;;; - (EQ (SUBTYPE1 |T1.TYPECODE| |T2.TYPECODE|) |@true|) for direct subtypes
;;; - Built-in type axioms (BOOLEAN, CHAR, INTEGER, CARDINAL)

;; Generate all type axioms as a list of S-expressions
(define (generate-type-axioms type-env)
  (let ((all-types (type-env-all-types type-env))
        (axioms '()))

    ;; 1. DISTINCT typecodes
    (let ((tcs (map type-info-typecode all-types)))
      (when (> (length tcs) 1)
        (set! axioms (cons (apply sx-distinct tcs) axioms))))

    ;; 2. DISTINCT RETURN EXIT
    (set! axioms (cons '(DISTINCT RETURN EXIT) axioms))

    ;; 3. Per-type axioms
    (for-each
     (lambda (ti)
       (set! axioms (append (generate-single-type-axioms ti type-env)
                            axioms)))
     all-types)

    ;; 4. Built-in constants
    (set! axioms (append (generate-builtin-axioms) axioms))

    (reverse axioms)))

;; Generate axioms for a single type
(define (generate-single-type-axioms ti type-env)
  (let ((axioms '())
        (name (type-info-name ti))
        (tc (type-info-typecode ti))
        (is-pred (type-info-is-pred ti))
        (supers (type-info-supers ti)))

    ;; Subtype chain axioms: Is$T => Is$Super for each super
    (for-each
     (lambda (super-name)
       (let ((super-pred (esc-is-pred super-name)))
         (let ((q (esc-gensym "q")))
           (set! axioms
             (cons (sx-forall (list q)
                     (list is-pred q)
                     (sx-implies (sx-is-pred is-pred q)
                                 (sx-is-pred super-pred q)))
                   axioms)))))
     (or supers '()))

    ;; For object/ref types: Is$T <=> SUBTYPE(TYPECODE(q), T.TYPECODE)
    (when (type-info-is-ref ti)
      (let ((q1 (esc-gensym "q"))
            (q2 (esc-gensym "q")))
        ;; Is$T => SUBTYPE(TYPECODE(q), T.TYPECODE)
        (set! axioms
          (cons (sx-forall (list q1)
                  (list is-pred q1)
                  (sx-implies (sx-is-pred is-pred q1)
                              (sx-subtype (sx-typecode q1) tc)))
                axioms))
        ;; SUBTYPE(TYPECODE(q), T.TYPECODE) => Is$T
        (set! axioms
          (cons (sx-forall (list q2)
                  (list (list 'SUBTYPE (sx-typecode q2) tc))
                  (sx-implies (sx-subtype (sx-typecode q2) tc)
                              (sx-is-pred is-pred q2)))
                axioms))
        ;; NULL <: T
        (set! axioms
          (cons (sx-subtype '|NULL.TYPECODE| tc) axioms))))

    ;; For ordinal types: Is$T => Is$ORDINAL
    (when (type-info-is-ordinal ti)
      (let ((q (esc-gensym "q")))
        (set! axioms
          (cons (sx-forall (list q)
                  (list is-pred q)
                  (sx-implies (sx-is-pred is-pred q)
                              (sx-is-pred '|Is$ORDINAL| q)))
                axioms))))

    ;; Direct subtype links for SUBTYPE1
    (for-each
     (lambda (super-name)
       (let ((super-tc (esc-typecode-name super-name)))
         (set! axioms (cons (sx-subtype1 tc super-tc) axioms))))
     (or supers '()))

    axioms))

;; Generate built-in constant axioms
(define (generate-builtin-axioms)
  (list
   ;; Integer bounds
   '(< 1000000 |INTEGER.LAST|)
   '(< |INTEGER.FIRST| -1000000)
   '(< |INTEGER.FIRST| 0)
   '(> |INTEGER.LAST| 0)

   ;; Boolean axioms
   '(EQ |BOOLEAN.FALSE| |@false|)
   '(EQ |BOOLEAN.TRUE| |@true|)
   '(NEQ |@true| |@false|)
   '(DISTINCT |BOOLEAN.TRUE| |BOOLEAN.FALSE|)

   ;; Integer range axiom
   '(FORALL (v) (IMPLIES (EQ (|Is$INTEGER| v) |@true|)
                         (AND (>= v |INTEGER.FIRST|) (<= v |INTEGER.LAST|))))

   ;; NIL axioms
   '(NOT (EQ (MEMBER |$NIL| LL) |@true|))
   '(EQ (|Is$NVIRG| |$NIL| VIRGIN) |@true|)
   '(EQ (|Is$DCL| |$NIL| ALLOCATED) |@true|)))

;; Type environment accessors (stub -- will be replaced by M3 interop)
(define (type-env-all-types env) (or env '()))
(define (type-info-name ti) (if (pair? ti) (car ti) ti))
(define (type-info-typecode ti)
  (esc-typecode-name (type-info-name ti)))
(define (type-info-is-pred ti)
  (esc-is-pred (type-info-name ti)))
(define (type-info-supers ti)
  (if (and (pair? ti) (> (length ti) 1)) (cadr ti) '()))
(define (type-info-is-ref ti)
  (if (and (pair? ti) (> (length ti) 2)) (caddr ti) #f))
(define (type-info-is-ordinal ti)
  (if (and (pair? ti) (> (length ti) 3)) (cadddr ti) #f))
