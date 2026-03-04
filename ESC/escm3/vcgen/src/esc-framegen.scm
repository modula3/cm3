;;; esc-framegen.scm -- Generate framing axioms
;;;
;;; Copyright (c) 2026, Mika Nystrom.  All rights reserved.
;;;
;;; Framing axioms state that if the concrete fields an abstraction
;;; function depends on haven't changed, and the RESIDUE hasn't changed,
;;; then the abstract value is unchanged.
;;;
;;; These are the long MPAT-triggered axioms in addhi.sx (lines 46-49).
;;;
;;; For each abstraction function FUNC.M.V that DEPEND on fields f1..fn:
;;;   (FORALL (f1' f1 f2' f2 ... RESIDUE' RESIDUE ind)
;;;     (PATS (MPAT (select (FUNC.M.V f1 f2 ... RESIDUE) ind)
;;;                 (select (FUNC.M.V f1' f2' ... RESIDUE') ind)))
;;;     (IMPLIES
;;;       (AND (Is$T ind)
;;;            (= (select RESIDUE ind) (select RESIDUE' ind))
;;;            (= (select f1 ind) (select f1' ind))
;;;            ...
;;;            ;; For array fields: also require element-wise equality
;;;            (FORALL (s0) (IMPLIES (range s0) (= (select f1[ind] s0) ...))))
;;;       (= (select (FUNC old) ind) (select (FUNC new) ind))))

;; Generate framing axioms for a single abstraction function
;; func-name: symbol like FUNC.IntSeq.Data
;; depend-fields: list of field symbols this function depends on
;; type-pred: Is$T predicate for the domain type
;; residue-name: RESIDUE.M.V symbol
;; array-fields: subset of depend-fields that are array-valued
(define (generate-frame-axiom func-name depend-fields type-pred
                              residue-name array-fields)
  (let* ((ind (esc-gensym "i"))
         ;; Generate paired pre/post variables for each field
         (field-pairs
          (map (lambda (f)
                 (let ((pre (esc-gensym (symbol->string f)))
                       (post (esc-gensym
                              (string-append (symbol->string f) "'"))))
                   (list f pre post)))
               depend-fields))
         ;; Residue pair
         (res-pre (esc-gensym (symbol->string residue-name)))
         (res-post (esc-gensym
                    (string-append (symbol->string residue-name) "'")))
         ;; Build quantifier variable list
         (all-vars (append
                    (esc-flatmap (lambda (fp) (list (caddr fp) (cadr fp)))
                                field-pairs)
                    (list res-post res-pre ind)))
         ;; Build FUNC applications
         (func-pre (cons func-name
                         (append (map cadr field-pairs) (list res-pre))))
         (func-post (cons func-name
                          (append (map caddr field-pairs) (list res-post))))
         ;; Build equality preconditions
         (eq-conds
          (append
           ;; Residue equality
           (list (sx-eq (sx-select res-pre ind) (sx-select res-post ind)))
           ;; Field equality
           (map (lambda (fp)
                  (sx-eq (sx-select (cadr fp) ind)
                         (sx-select (caddr fp) ind)))
                field-pairs)
           ;; Array element equality for array fields
           (esc-flatmap
            (lambda (fp)
              (let ((f (car fp)))
                (if (memq f array-fields)
                    (let ((s0 (esc-gensym "s0")))
                      (list
                       ;; Array size equality
                       (sx-eq (sx-number (sx-select (cadr fp) ind) 0)
                              (sx-number (sx-select (caddr fp) ind) 0))
                       ;; Element-wise equality
                       (sx-forall (list s0)
                         #f
                         (sx-implies
                          (sx-and (sx-is-pred '|Is$MATHINT| s0)
                                  (sx-and (sx-le 0 s0)
                                          (sx-lt s0 (sx-number
                                                     (sx-select (cadr fp) ind)
                                                     0))))
                          (sx-eq (sx-select (sx-select (cadr fp)
                                                       (sx-select (cadr fp) ind))
                                            s0)
                                 (sx-select (sx-select (caddr fp)
                                                       (sx-select (caddr fp) ind))
                                            s0))))))
                    '())))
            field-pairs)))
         ;; Build the MPAT trigger
         (mpat `(MPAT ,(sx-select func-pre ind)
                      ,(sx-select func-post ind))))

    ;; Build the complete axiom
    (sx-forall all-vars
      (list mpat)
      (sx-implies
       (apply sx-and (cons (sx-is-pred type-pred ind) eq-conds))
       (sx-eq (sx-select func-pre ind)
              (sx-select func-post ind))))))

;; Generate framing axioms for all abstraction functions
(define (generate-all-frame-axioms abstractions)
  (map (lambda (abs)
         (generate-frame-axiom
          (abs-func-name abs)
          (abs-depend-fields abs)
          (abs-type-pred abs)
          (abs-residue-name abs)
          (abs-array-fields abs)))
       abstractions))

;; Abstraction accessors (stub)
(define (abs-func-name a) (list-ref a 0))
(define (abs-depend-fields a) (list-ref a 1))
(define (abs-type-pred a) (list-ref a 2))
(define (abs-residue-name a) (list-ref a 3))
(define (abs-array-fields a) (if (> (length a) 4) (list-ref a 4) '()))
