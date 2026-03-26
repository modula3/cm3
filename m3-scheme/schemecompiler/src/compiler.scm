;;
;; MScheme Level 1 Compiler
;;
;; Compiles top-level (define (name params...) body) forms to Modula-3
;; source code implementing SchemeProcedure.T subclasses.
;;
;; All free variable access goes through SchemeEnvironmentBinding.T cells
;; (Level 1 compilation -- preserves redefinition semantics).
;;
;; Self-tail-calls are optimized to LOOP with parameter reassignment.
;;
;; All generated M3 identifiers use serial numbers to prevent collisions:
;;   Procedures: p0, p1, ...
;;   Parameters: a_0, a_1, ...
;;   Free-var bindings: b_0, b_1, ...
;;   Constants: k_0, k_1, ...
;;   Let-bound vars: v_0, v_1, ...
;;   Temporaries: t_0, t_1, ...
;;
;; Copyright (c) 2026 Mika Nystrom.  All rights reserved.
;;

(require-modules "basic-defs")

;; Newline character for code generation
(define NL (list->string (list #\newline)))

;; Append newline to string (line terminator helper)
(define (L . strs) (string-append (apply string-append strs) NL))

;; Extract the basename from a file path (everything after last /)
(define (path-basename path)
  (let loop ((chars (string->list path)) (acc '()))
    (cond ((null? chars) (list->string acc))
          ((char=? (car chars) #\/)
           (loop (cdr chars) '()))
          (else (loop (cdr chars) (append acc (list (car chars))))))))

;; Strip .scm extension from a filename, or return #f if not present
(define (strip-scm-extension name)
  (let ((chars (string->list name)))
    (let ((len (length chars)))
      (if (and (>= len 4)
               (char=? (list-ref chars (- len 4)) #\.)
               (char=? (list-ref chars (- len 3)) #\s)
               (char=? (list-ref chars (- len 2)) #\c)
               (char=? (list-ref chars (- len 1)) #\m))
          (list->string (let loop ((i 0) (rest chars) (acc '()))
                          (if (= i (- len 4))
                              (reverse acc)
                              (loop (+ i 1) (cdr rest) (cons (car rest) acc)))))
          #f))))

;;;
;;; ==================== Identifier Mapping ====================
;;;

(define (build-indexed-map syms prefix)
  ;; Build an alist mapping symbols to prefix+index strings:
  ;; ((sym0 . "prefix0") (sym1 . "prefix1") ...)
  (let loop ((ss syms) (i 0) (acc '()))
    (if (null? ss) (reverse acc)
        (loop (cdr ss) (+ i 1)
              (cons (cons (car ss)
                          (string-append prefix (number->string i)))
                    acc)))))

;;;
;;; ==================== AST Analysis ====================
;;;

(define (parse-define form)
  ;; Returns (name params body rest-param-or-#f) or #f
  ;; params is always a proper list (rest param, if any, is separate)
  (if (and (pair? form) (eq? (car form) 'define) (pair? (cadr form)))
      (let* ((raw-params (cdadr form))
             (parsed (parse-params raw-params))
             (fixed-params (car parsed))
             (rest-param (cdr parsed)))
        (list (caadr form) fixed-params
              (if (null? (cdddr form))
                  (caddr form)
                  (cons 'begin (cddr form)))
              rest-param))
      #f))

(define (free-variables expr bound)
  ;; Return list of free variable symbols in expr not in bound
  (cond
    ((symbol? expr)
     (if (memq expr bound) '() (list expr)))
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'if)
     (append (free-variables (cadr expr) bound)
             (free-variables (caddr expr) bound)
             (if (pair? (cdddr expr))
                 (free-variables (cadddr expr) bound)
                 '())))
    ((eq? (car expr) 'cond)
     (apply append
            (map (lambda (clause)
                   (if (eq? (car clause) 'else)
                       (free-vars-body (cdr clause) bound)
                       (if (and (pair? (cdr clause))
                                (eq? (cadr clause) '=>))
                           ;; (test => proc)
                           (append (free-variables (car clause) bound)
                                   (free-variables (caddr clause) bound))
                           (append (free-variables (car clause) bound)
                                   (free-vars-body (cdr clause) bound)))))
                 (cdr expr))))
    ((eq? (car expr) 'begin)
     (free-vars-body (cdr expr) bound))
    ((eq? (car expr) 'let)
     (if (symbol? (cadr expr))
         ;; Named let: (let name ((var init) ...) body)
         (let* ((loop-name (cadr expr))
                (bindings (caddr expr))
                (bvars (map car bindings))
                (inits-free (apply append
                                  (map (lambda (b)
                                         (free-variables (cadr b) bound))
                                       bindings)))
                ;; loop name and loop vars are bound in body
                (inner-bound (cons loop-name (append bvars bound))))
           (append inits-free
                   (free-vars-body (cdddr expr) inner-bound)))
         ;; Regular let
         (let* ((bindings (cadr expr))
                (bvars (map car bindings))
                (inits-free (apply append
                                  (map (lambda (b)
                                         (free-variables (cadr b) bound))
                                       bindings))))
           (append inits-free
                   (free-vars-body (cddr expr) (append bvars bound))))))
    ((eq? (car expr) 'let*)
     (let loop ((bindings (cadr expr))
                (bound bound)
                (acc '()))
       (if (null? bindings)
           (append acc (free-vars-body (cddr expr) bound))
           (loop (cdr bindings)
                 (cons (caar bindings) bound)
                 (append acc (free-variables (cadar bindings) bound))))))
    ((eq? (car expr) 'do)
     ;; (do ((var init step) ...) (test exit-expr ...) body ...)
     (let* ((bindings (cadr expr))
            (do-vars (map car bindings))
            (inits-free (apply append
                              (map (lambda (b)
                                     (free-variables (cadr b) bound))
                                   bindings)))
            (inner-bound (append do-vars bound))
            (steps-free (apply append
                              (map (lambda (b)
                                     (if (pair? (cddr b))
                                         (free-variables (caddr b) inner-bound)
                                         '()))
                                   bindings)))
            (test-clause (caddr expr))
            (test-free (free-vars-body test-clause inner-bound))
            (body-free (free-vars-body (cdddr expr) inner-bound)))
       (append inits-free steps-free test-free body-free)))
    ((eq? (car expr) 'case)
     (append (free-variables (cadr expr) bound)  ;; key expression
             (apply append
                    (map (lambda (clause)
                           (if (eq? (car clause) 'else)
                               (free-vars-body (cdr clause) bound)
                               ;; datums are constants, not variable refs
                               (free-vars-body (cdr clause) bound)))
                         (cddr expr)))))
    ((eq? (car expr) 'letrec)
     (let* ((bindings (cadr expr))
            (bvars (map car bindings))
            (inner-bound (append bvars bound)))
       (append (apply append
                      (map (lambda (b) (free-variables (cadr b) inner-bound))
                           bindings))
               (free-vars-body (cddr expr) inner-bound))))
    ((eq? (car expr) 'and)
     (apply append (map (lambda (e) (free-variables e bound)) (cdr expr))))
    ((eq? (car expr) 'or)
     (apply append (map (lambda (e) (free-variables e bound)) (cdr expr))))
    ((eq? (car expr) 'set!)
     (append (if (memq (cadr expr) bound) '() (list (cadr expr)))
             (free-variables (caddr expr) bound)))
    ((eq? (car expr) 'lambda)
     (let ((lam-params (all-params (cadr expr))))
       (free-vars-body (cddr expr)
                       (append lam-params bound))))
    (else
     ;; application: all subexpressions
     (apply append (map (lambda (e) (free-variables e bound)) expr)))))

(define (free-vars-body exprs bound)
  ;; Handle internal defines by desugaring to let+lambda
  (let ((desugared (desugar-internal-defines exprs)))
    (if desugared
        (apply append (map (lambda (e) (free-variables e bound)) desugared))
        (apply append (map (lambda (e) (free-variables e bound)) exprs)))))

(define (unique-symbols lst)
  (let loop ((rest lst) (seen '()) (acc '()))
    (cond ((null? rest) (reverse acc))
          ((memq (car rest) seen) (loop (cdr rest) seen acc))
          (else (loop (cdr rest)
                      (cons (car rest) seen)
                      (cons (car rest) acc))))))

(define (collect-constants expr)
  (cond
    ((number? expr) (list expr))
    ((string? expr) (list expr))
    ((boolean? expr) (list expr))
    ((char? expr) (list expr))
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) (list (cadr expr)))
    ((eq? (car expr) 'case)
     ;; Key expression + datums (implicitly quoted) + clause bodies
     (append (collect-constants (cadr expr))
             (apply append
                    (map (lambda (clause)
                           (if (eq? (car clause) 'else)
                               (apply append (map collect-constants (cdr clause)))
                               (append (map (lambda (d) d) (car clause))  ;; datums as constants
                                       (apply append (map collect-constants (cdr clause))))))
                         (cddr expr)))))
    ((eq? (car expr) 'lambda) '())  ;; lambda manages its own constants
    (else (apply append (map collect-constants expr)))))

(define (unique-constants lst)
  (let loop ((rest lst) (acc '()))
    (cond ((null? rest) (reverse acc))
          ((member? (car rest) acc) (loop (cdr rest) acc))
          (else (loop (cdr rest) (cons (car rest) acc))))))

(define (intersection lst1 lst2)
  ;; Return elements of lst1 that also appear in lst2 (using memq)
  (filter (lambda (x) (memq x lst2)) lst1))

(define (set!-targets expr)
  ;; Return list of symbols that are targets of set! anywhere in expr.
  ;; Does NOT recurse into nested lambda bodies (lambda creates new scope).
  (cond
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'lambda) '())  ;; opaque boundary
    ((eq? (car expr) 'set!)
     (cons (cadr expr) (set!-targets (caddr expr))))
    (else (apply append (map set!-targets expr)))))

(define (expr-has-lambda-capturing? expr vars)
  ;; Does expr contain a lambda that captures any of vars?
  (cond
    ((not (pair? expr)) #f)
    ((eq? (car expr) 'quote) #f)
    ((eq? (car expr) 'lambda)
     (let* ((lam-params (all-params (cadr expr)))
            (lam-body (if (null? (cdddr expr)) (caddr expr)
                          (cons 'begin (cddr expr))))
            (captures (free-variables lam-body lam-params)))
       (not (null? (intersection captures vars)))))
    (else (or (expr-has-lambda-capturing? (car expr) vars)
              (expr-has-lambda-capturing? (cdr expr) vars)))))

(define (desugar-internal-defines exprs)
  ;; Collect ALL internal (define (name params...) body...) forms from
  ;; anywhere in exprs.  Hoists them into a single letrec (or let if
  ;; non-recursive) at the top, with remaining forms as body.
  ;; Returns desugared expr list, or #f if no defines found.
  (let* ((defs (filter (lambda (e)
                         (and (pair? e)
                              (eq? (car e) 'define)
                              (pair? (cdr e))
                              (pair? (cadr e))     ;; function define
                              (pair? (cddr e))))   ;; has body
                       exprs))
         (rest (filter (lambda (e)
                         (not (and (pair? e)
                                   (eq? (car e) 'define)
                                   (pair? (cdr e))
                                   (pair? (cadr e))
                                   (pair? (cddr e)))))
                       exprs)))
    (if (or (null? defs) (null? rest))
        #f
        (let* ((names (map caadr defs))
               (all-free
                (apply append
                       (map (lambda (d)
                              (let ((params (cdadr d))
                                    (body (if (null? (cdddr d))
                                              (caddr d)
                                              (cons 'begin (cddr d)))))
                                (free-variables body (all-params params))))
                            defs)))
               (recursive (not (null? (intersection names all-free))))
               (let-form (if recursive 'letrec 'let))
               (bindings
                (map (lambda (d)
                       (let ((name (caadr d))
                             (params (cdadr d))
                             (body (if (null? (cdddr d))
                                       (caddr d)
                                       (cons 'begin (cddr d)))))
                         (list name
                               (cons 'lambda
                                     (cons params (list body))))))
                     defs)))
          (list (append (list let-form bindings) rest))))))

(define (has-self-tail-call? expr name)
  (cond
    ((not (pair? expr)) #f)
    ((eq? (car expr) 'quote) #f)
    ((eq? (car expr) 'if)
     (or (has-self-tail-call? (caddr expr) name)
         (and (pair? (cdddr expr))
              (has-self-tail-call? (cadddr expr) name))))
    ((eq? (car expr) 'cond)
     (let loop ((clauses (cdr expr)))
       (if (null? clauses) #f
           (or (has-self-tail-call? (last (car clauses)) name)
               (loop (cdr clauses))))))
    ((eq? (car expr) 'begin)
     (if (null? (cdr expr)) #f
         (has-self-tail-call? (last (cdr expr)) name)))
    ((eq? (car expr) 'let)
     (if (symbol? (cadr expr))
         ;; Named let: if loop name shadows function name, don't look inside
         (if (eq? (cadr expr) name) #f
             (has-self-tail-call? (last (cdddr expr)) name))
         (has-self-tail-call? (last (cddr expr)) name)))
    ((eq? (car expr) 'let*)
     (has-self-tail-call? (last (cddr expr)) name))
    ((eq? (car expr) 'letrec)
     (has-self-tail-call? (last (cddr expr)) name))
    ((eq? (car expr) 'do)
     ;; Exit expressions are in tail position
     (let ((test-clause (caddr expr)))
       (if (pair? (cdr test-clause))
           (has-self-tail-call? (last (cdr test-clause)) name)
           #f)))
    ((eq? (car expr) 'case)
     (let loop ((clauses (cddr expr)))
       (if (null? clauses) #f
           (or (has-self-tail-call? (last (car clauses)) name)
               (loop (cdr clauses))))))
    ((eq? (car expr) 'lambda) #f)  ;; opaque boundary
    ((eq? (car expr) 'and)
     (if (null? (cdr expr)) #f
         (has-self-tail-call? (last (cdr expr)) name)))
    ((eq? (car expr) 'or)
     (if (null? (cdr expr)) #f
         (has-self-tail-call? (last (cdr expr)) name)))
    ((eq? (car expr) 'set!) #f)
    ((and (pair? expr) (eq? (car expr) name)) #t)
    (else #f)))

;;;
;;; ==================== Compilation Context ====================
;;;
;;; Context structure (12-element list):
;;;   0: name       - Scheme symbol of the function being compiled
;;;   1: params     - list of parameter symbols (original, for self-tail-call)
;;;   2: free-vars  - list of free variable symbols
;;;   3: constants  - list of constant values
;;;   4: temp-ctr   - mutable cell (list of int) for temporary variables
;;;   5: self-tail? - boolean, has self-tail-call optimization
;;;   6: param-map  - alist mapping symbols to M3 variable names
;;;   7: fv-map     - alist mapping free-var symbols to M3 binding paths
;;;   8: var-ctr    - mutable cell (list of int) for let-bound variables
;;;   9: reassign   - list of M3 var names for tail-call reassignment, or #f
;;;  10: loop-flag  - M3 BOOLEAN var name for non-tail named-let WHILE, or #f
;;;  11: unboxed    - alist mapping param symbols to LONGREAL M3 var names
;;;

(define (make-ctx name params free-vars constants self-tail?)
  (let ((param-map (build-indexed-map params "a_"))
        (fv-map (build-indexed-map free-vars "self.b_")))
    (list name params free-vars constants
          (list 0)      ;; temp counter (mutable cell)
          self-tail?
          param-map fv-map
          (list 0)      ;; var counter (mutable cell)
          #f            ;; reassign-vars (default: use a_N)
          #f            ;; loop-flag (default: none)
          '())));       ;; unboxed-params (default: none)

(define (ctx-name ctx) (car ctx))
(define (ctx-params ctx) (cadr ctx))
(define (ctx-free-vars ctx) (caddr ctx))
(define (ctx-constants ctx) (cadddr ctx))
(define (ctx-temp-counter ctx) (car (cddddr ctx)))
(define (ctx-self-tail? ctx) (cadr (cddddr ctx)))
(define (ctx-param-map ctx) (caddr (cddddr ctx)))
(define (ctx-fv-map ctx) (cadddr (cddddr ctx)))
(define (ctx-var-counter ctx) (car (cddddr (cddddr ctx))))
(define (ctx-reassign-vars ctx) (cadr (cddddr (cddddr ctx))))
(define (ctx-loop-flag ctx)
  (if (pair? (cddr (cddddr (cddddr ctx))))
      (caddr (cddddr (cddddr ctx)))
      #f))

(define (ctx-unboxed ctx)
  (if (and (pair? (cddr (cddddr (cddddr ctx))))
           (pair? (cdddr (cddddr (cddddr ctx)))))
      (cadddr (cddddr (cddddr ctx)))
      '()))

;;;
;;; Procedure-level declaration accumulator.
;;; All temps and vars are collected here during code generation,
;;; then emitted as a single VAR block at procedure top level.
;;; This avoids deeply nested VAR BEGIN...END blocks that overflow
;;; CM3's Scope.NameToPrefix stack.
;;;

(define *proc-decls* '())  ;; list of (name . type) pairs

(define (emit-decl name . opt-type)
  (let ((type (if (null? opt-type) "SchemeObject.T" (car opt-type))))
    (set! *proc-decls* (cons (cons name type) *proc-decls*))))

(define (gen-with-decls thunk)
  ;; Save *proc-decls*, run thunk (which accumulates decls via fresh-temp/
  ;; fresh-var), restore.  Returns (decls-list . body-code-string).
  (let ((saved *proc-decls*))
    (set! *proc-decls* '())
    (let ((body-code (thunk)))
      (let ((decls (reverse *proc-decls*)))
        (set! *proc-decls* saved)
        (cons decls body-code)))))

(define (fresh-temp ctx)
  (let* ((counter-cell (ctx-temp-counter ctx))
         (n (car counter-cell)))
    (set-car! counter-cell (+ n 1))
    (let ((name (string-append "t_" (number->string n))))
      (emit-decl name)
      name)))

(define (fresh-var ctx . opt-type)
  ;; Generate a fresh name for a let/let*-bound variable.
  ;; Optional type argument (default "SchemeObject.T") for BOOLEAN loop flags.
  (let* ((counter-cell (ctx-var-counter ctx))
         (n (car counter-cell)))
    (set-car! counter-cell (+ n 1))
    (let ((name (string-append "v_" (number->string n))))
      (apply emit-decl name opt-type)
      name)))

(define (make-extended-ctx ctx new-param-map)
  ;; Create a new context with an updated param-map (for let/let* bindings).
  ;; Temp and var counters are shared (mutable cells).
  (list (ctx-name ctx)
        (ctx-params ctx)
        (ctx-free-vars ctx)
        (ctx-constants ctx)
        (ctx-temp-counter ctx)
        (ctx-self-tail? ctx)
        new-param-map
        (ctx-fv-map ctx)
        (ctx-var-counter ctx)
        (ctx-reassign-vars ctx)
        (ctx-loop-flag ctx)
        (ctx-unboxed ctx)))

(define (make-extended-ctx-fv ctx new-param-map new-fv-map)
  ;; Create a new context with updated param-map AND fv-map.
  ;; Used by gen-letrec to add binding cells visible to lambda captures.
  (list (ctx-name ctx)
        (ctx-params ctx)
        (ctx-free-vars ctx)
        (ctx-constants ctx)
        (ctx-temp-counter ctx)
        (ctx-self-tail? ctx)
        new-param-map
        new-fv-map
        (ctx-var-counter ctx)
        (ctx-reassign-vars ctx)
        (ctx-loop-flag ctx)
        (ctx-unboxed ctx)))

;;;
;;; ==================== Variable and Constant References ====================
;;;

(define (var-ref sym ctx)
  ;; Look up a variable: check unboxed (LONGREAL) first, then param-map, then fv-map.
  ;; Unboxed params are re-boxed via SchemeLongReal.FromLR() when used generically.
  ;; fv-map values are full binding paths (e.g. "self.b_0" or "lb_0")
  (let ((u (assq sym (ctx-unboxed ctx))))
    (if u
        (string-append "SchemeLongReal.FromLR(" (cdr u) ")")
        (let ((p (assq sym (ctx-param-map ctx))))
          (if p
              (cdr p)
              (let ((f (assq sym (ctx-fv-map ctx))))
                (if f
                    (string-append (cdr f) ".get()")
                    (begin
                      (display "ERROR: var-ref: symbol not found: ")
                      (display sym)
                      (display " in function ")
                      (display (ctx-name ctx))
                      (newline)
                      (string-append "self.UNKNOWN_" (symbol->string sym) ".get()")))))))))

(define (constant-index val ctx)
  (let loop ((cs (ctx-constants ctx)) (i 0))
    (cond ((null? cs) #f)
          ((equal? (car cs) val) i)
          (else (loop (cdr cs) (+ i 1))))))

(define (constant-ref val ctx)
  (let ((idx (constant-index val ctx)))
    (if idx
        (string-append "self.k_" (number->string idx))
        (cond ((and (integer? val) (= val 0)) "SchemeInt.Zero")
              ((and (integer? val) (= val 1)) "SchemeInt.One")
              ((integer? val)
               (string-append "SchemeInt.FromI(" (number->string val) ")"))
              ((number? val)
               (string-append "SchemeLongReal.FromLR(" (number->string val) "d0)"))
              ((symbol? val)
               (string-append "SchemeSymbol.Symbol(\""
                              (symbol->string val) "\")"))
              ((string? val)
               (string-append "SchemeString.FromText(\""
                              (m3-escape-string val) "\")"))
              ((boolean? val)
               (if val "SchemeBoolean.True()" "SchemeBoolean.False()"))
              ((null? val) "NIL")
              (else (string-append "self.k_" (number->string 0)))))))

;;;
;;; ==================== Code Generation Helpers ====================
;;;

(define (indent n)
  (make-string (* n 2) #\space))

(define (emit-assign target expr depth)
  (string-append
   (indent depth)
   (if target
       (L target " := " expr ";")
       (L "RETURN " expr ";"))))

;;;
;;; ==================== Expression Code Generation ====================
;;;

(define (gen-value expr ctx depth)
  (cond
    ((symbol? expr) (var-ref expr ctx))
    ((number? expr) (constant-ref expr ctx))
    ((string? expr) (constant-ref expr ctx))
    ((boolean? expr)
     (if expr "SchemeBoolean.True()" "SchemeBoolean.False()"))
    ((char? expr) (constant-ref expr ctx))
    ((and (pair? expr) (eq? (car expr) 'quote))
     (constant-ref (cadr expr) ctx))
    (else #f)))

(define (gen-expr expr target ctx depth)
  (cond
    ((symbol? expr)
     (emit-assign target (var-ref expr ctx) depth))
    ((number? expr)
     (emit-assign target (constant-ref expr ctx) depth))
    ((string? expr)
     (emit-assign target (constant-ref expr ctx) depth))
    ((boolean? expr)
     (emit-assign target
                  (if expr "SchemeBoolean.True()" "SchemeBoolean.False()")
                  depth))
    ((char? expr)
     (emit-assign target (constant-ref expr ctx) depth))
    ((not (pair? expr))
     (emit-assign target "NIL" depth))
    ((eq? (car expr) 'quote)
     (let ((val (cadr expr)))
       (if (null? val)
           (emit-assign target "NIL" depth)
           (emit-assign target (constant-ref val ctx) depth))))
    ((eq? (car expr) 'if)
     (gen-if (cadr expr) (caddr expr)
             (if (pair? (cdddr expr)) (cadddr expr) '*no-alt*)
             target ctx depth))
    ((eq? (car expr) 'cond)
     (gen-cond (cdr expr) target ctx depth))
    ((eq? (car expr) 'begin)
     (gen-begin (cdr expr) target ctx depth))
    ((eq? (car expr) 'let)
     (if (symbol? (cadr expr))
         ;; Named let: (let name ((var init) ...) body)
         (gen-named-let (cadr expr) (caddr expr) (cdddr expr) target ctx depth)
         (gen-let (cadr expr) (cddr expr) target ctx depth)))
    ((eq? (car expr) 'let*)
     (gen-let* (cadr expr) (cddr expr) target ctx depth))
    ((eq? (car expr) 'letrec)
     (gen-letrec (cadr expr) (cddr expr) target ctx depth))
    ((eq? (car expr) 'case)
     (gen-case (cadr expr) (cddr expr) target ctx depth))
    ((eq? (car expr) 'do)
     (gen-do (cadr expr) (caddr expr) (cdddr expr) target ctx depth))
    ((eq? (car expr) 'and)
     (gen-and (cdr expr) target ctx depth))
    ((eq? (car expr) 'or)
     (gen-or (cdr expr) target ctx depth))
    ((eq? (car expr) 'set!)
     (gen-set! (cadr expr) (caddr expr) target ctx depth))
    ((eq? (car expr) 'not)
     (gen-not (cadr expr) target ctx depth))
    ;; Lambda expression
    ((eq? (car expr) 'lambda)
     (gen-lambda expr target ctx depth))
    ;; Self-tail-call or named-let loop continuation
    ((and (ctx-self-tail? ctx)
          (pair? expr)
          (eq? (car expr) (ctx-name ctx))
          (or (not target) (ctx-loop-flag ctx)))
     (gen-self-tail-call (cdr expr) ctx depth))
    ;; General procedure call
    ((pair? expr)
     (gen-call (car expr) (cdr expr) target ctx depth))
    (else
     (emit-assign target "NIL" depth))))

;;;
;;; ==================== If ====================
;;;

(define (gen-if test con alt target ctx depth)
  (cond
    ;; (not ...) flipping: (if (not test) a b) → (if test b a)
    ((and (pair? test) (eq? (car test) 'not)
          (pair? (cdr test)) (null? (cddr test))
          (not (eq? alt '*no-alt*)))
     (gen-if (cadr test) alt con target ctx depth))

    ;; Inline bool test: predicate in if-test position
    ((try-inline-bool-test test ctx depth)
     => (lambda (result)
          (let ((pre-code (car result))
                (bool-expr (cdr result))
                (no-alt-else (if target
                                 (string-append
                                  (indent depth) (L "ELSE")
                                  (emit-assign target "NIL" (+ depth 1)))
                                 (string-append
                                  (indent depth) (L "ELSE")
                                  (emit-assign #f "NIL" (+ depth 1))))))
            (string-append
             pre-code
             (indent depth) (L "IF " bool-expr " THEN")
             (gen-expr con target ctx (+ depth 1))
             (if (not (eq? alt '*no-alt*))
                 (string-append
                  (indent depth) (L "ELSE")
                  (gen-expr alt target ctx (+ depth 1)))
                 no-alt-else)
             (indent depth) (L "END;")))))

    ;; Compound and/or in if-test position
    ((try-inline-compound-bool-test test ctx depth)
     => (lambda (result)
          (let ((pre-code (car result))
                (bool-expr (cdr result))
                (no-alt-else (if target
                                 (string-append
                                  (indent depth) (L "ELSE")
                                  (emit-assign target "NIL" (+ depth 1)))
                                 (string-append
                                  (indent depth) (L "ELSE")
                                  (emit-assign #f "NIL" (+ depth 1))))))
            (string-append
             pre-code
             (indent depth) (L "IF " bool-expr " THEN")
             (gen-expr con target ctx (+ depth 1))
             (if (not (eq? alt '*no-alt*))
                 (string-append
                  (indent depth) (L "ELSE")
                  (gen-expr alt target ctx (+ depth 1)))
                 no-alt-else)
             (indent depth) (L "END;")))))

    ;; Fallback: existing TruthO-based test
    (else
     (let ((test-val (gen-value test ctx depth))
           (no-alt-else (if target
                            (string-append
                             (indent depth) (L "ELSE")
                             (emit-assign target "NIL" (+ depth 1)))
                            (string-append
                             (indent depth) (L "ELSE")
                             (emit-assign #f "NIL" (+ depth 1))))))
       (if test-val
           (string-append
            (indent depth) (L "IF SchemeBoolean.TruthO(" test-val ") THEN")
            (gen-expr con target ctx (+ depth 1))
            (if (not (eq? alt '*no-alt*))
                (string-append
                 (indent depth) (L "ELSE")
                 (gen-expr alt target ctx (+ depth 1)))
                no-alt-else)
            (indent depth) (L "END;"))
           (let ((tmp (fresh-temp ctx)))
             (string-append
              (gen-expr test tmp ctx depth)
              (indent depth) (L "IF SchemeBoolean.TruthO(" tmp ") THEN")
              (gen-expr con target ctx (+ depth 1))
              (if (not (eq? alt '*no-alt*))
                  (string-append
                   (indent depth) (L "ELSE")
                   (gen-expr alt target ctx (+ depth 1)))
                  no-alt-else)
              (indent depth) (L "END;"))))))))

;;;
;;; ==================== Cond ====================
;;;

(define (gen-cond-arrow-call proc-expr test-tmp target ctx depth)
  ;; Generate code for (cond (test => proc)): call proc with test value
  (let ((proc-val (gen-value proc-expr ctx depth)))
    (if proc-val
        (emit-assign target
                     (string-append "NARROW(" proc-val
                                    ", SchemeProcedure.T).apply1(interp, "
                                    test-tmp ")")
                     depth)
        (let ((proc-tmp (fresh-temp ctx)))
          (string-append
           (gen-expr proc-expr proc-tmp ctx depth)
           (emit-assign target
                        (string-append "NARROW(" proc-tmp
                                       ", SchemeProcedure.T).apply1(interp, "
                                       test-tmp ")")
                        depth))))))

(define (gen-cond-rest rest target ctx depth)
  ;; Generate ELSE + rest clauses or ELSE NIL, shared by normal and arrow clauses
  (if (null? rest)
      (string-append
       (indent depth) (L "ELSE")
       (emit-assign target "NIL" (+ depth 1))
       (indent depth) (L "END;"))
      (string-append
       (indent depth) (L "ELSE")
       (gen-cond rest target ctx (+ depth 1))
       (indent depth) (L "END;"))))

(define (gen-cond clauses target ctx depth)
  (if (null? clauses)
      (emit-assign target "NIL" depth)
      (let ((clause (car clauses))
            (rest (cdr clauses)))
        (cond
         ((eq? (car clause) 'else)
          (gen-begin (cdr clause) target ctx depth))
         ;; Arrow clause: (test => proc)
         ((and (pair? (cdr clause)) (eq? (cadr clause) '=>))
          (let ((test-tmp (fresh-temp ctx)))
            (string-append
             (gen-expr (car clause) test-tmp ctx depth)
             (indent depth) (L "IF SchemeBoolean.TruthO(" test-tmp ") THEN")
             (gen-cond-arrow-call (caddr clause) test-tmp target ctx (+ depth 1))
             (gen-cond-rest rest target ctx depth))))
         (else
          (let ((test-val (gen-value (car clause) ctx depth)))
              (if test-val
                  (string-append
                   (indent depth) (L "IF SchemeBoolean.TruthO(" test-val ") THEN")
                   (gen-begin (cdr clause) target ctx (+ depth 1))
                   (if (null? rest)
                       (string-append
                        (indent depth) (L "ELSE")
                        (emit-assign target "NIL" (+ depth 1))
                        (indent depth) (L "END;"))
                       (string-append
                        (indent depth) (L "ELSE")
                        (gen-cond rest target ctx (+ depth 1))
                        (indent depth) (L "END;"))))
                  (let ((tmp (fresh-temp ctx)))
                    (string-append
                     (gen-expr (car clause) tmp ctx depth)
                     (indent depth) (L "IF SchemeBoolean.TruthO(" tmp ") THEN")
                     (gen-begin (cdr clause) target ctx (+ depth 1))
                     (if (null? rest)
                         (string-append
                          (indent depth) (L "ELSE")
                          (emit-assign target "NIL" (+ depth 1))
                          (indent depth) (L "END;"))
                         (string-append
                          (indent depth) (L "ELSE")
                          (gen-cond rest target ctx (+ depth 1))
                          (indent depth) (L "END;"))))))))))))

;;;
;;; ==================== Begin ====================
;;;

(define (gen-begin exprs target ctx depth)
  (let ((desugared (desugar-internal-defines exprs)))
    (if desugared
        (gen-begin desugared target ctx depth)
        (cond
          ((null? exprs)
           (emit-assign target "NIL" depth))
          ((null? (cdr exprs))
           (gen-expr (car exprs) target ctx depth))
          (else
           (let ((discard (fresh-temp ctx)))
             (string-append
              (gen-expr (car exprs) discard ctx depth)
              (gen-begin (cdr exprs) target ctx depth))))))))

;;;
;;; ==================== Let ====================
;;;

;;; Detect let-bound variables needing binding cells:
;;; A let-bound variable that is both set!-targeted AND captured by a lambda
;;; must use a SchemeEnvironmentBinding.T cell (like letrec) so the lambda
;;; sees mutations.

(define (set!-targets expr)
  ;; Return list of symbols that are set! targets anywhere in expr.
  (cond
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'set!)
     (cons (cadr expr) (set!-targets (caddr expr))))
    (else (append (set!-targets (car expr))
                  (set!-targets (cdr expr))))))

(define (lambda-captured-vars expr vars)
  ;; Return subset of vars that appear free inside any lambda in expr.
  (cond
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'lambda)
     (let ((lam-params (let ((p (cadr expr)))
                         (cond ((symbol? p) (list p))
                               ((pair? p)
                                (let loop ((x p) (acc '()))
                                  (cond ((null? x) acc)
                                        ((symbol? x) (cons x acc))
                                        (else (loop (cdr x) (cons (car x) acc))))))
                               (else '())))))
       ;; vars that appear in the lambda body but are NOT lambda params
       (filter (lambda (v)
                 (and (not (memq v lam-params))
                      (expr-has-symbol? (cddr expr) v)))
               vars)))
    (else (append (lambda-captured-vars (car expr) vars)
                  (lambda-captured-vars (cdr expr) vars)))))

(define (needs-binding-cell? var body)
  ;; True if var is both set!-targeted AND captured by a lambda in body.
  (and (memq var (set!-targets body))
       (memq var (lambda-captured-vars body (list var)))))

(define (gen-let bindings body target ctx depth)
  (let* ((bvars (map car bindings))
         (body-expr (if (null? (cdr body)) (car body) (cons 'begin body)))
         ;; Detect vars needing binding cells (set! + lambda-capture)
         (cell-vars (filter (lambda (v) (needs-binding-cell? v body-expr)) bvars))
         (temps (map (lambda (b) (fresh-temp ctx)) bindings))
         ;; For cell-vars: allocate binding cells; for others: regular vars
         (bvar-m3names (map (lambda (bv)
                              (if (memq bv cell-vars)
                                  (fresh-binding ctx)
                                  (fresh-var ctx)))
                            bvars))
         ;; Cell-vars go into fv-map (captured by reference), others into param-map
         (new-param-entries (filter (lambda (e) (not (memq (car e) cell-vars)))
                                   (map cons bvars bvar-m3names)))
         (new-fv-entries (filter (lambda (e) (memq (car e) cell-vars))
                                (map cons bvars bvar-m3names)))
         (new-ctx (if (null? cell-vars)
                      (make-extended-ctx ctx
                        (append (map cons bvars bvar-m3names) (ctx-param-map ctx)))
                      (make-extended-ctx-fv ctx
                        (append new-param-entries (ctx-param-map ctx))
                        (append new-fv-entries (ctx-fv-map ctx))))))
    (string-append
     ;; Evaluate init expressions into temps (outer scope, no shadowing)
     (apply string-append
            (map (lambda (b tmp)
                   (gen-expr (cadr b) tmp ctx depth))
                 bindings temps))
     ;; Initialize: binding cells get NEW(LetrecCell); regular vars get assignment
     (apply string-append
            (map (lambda (bv m3n tmp)
                   (if (memq bv cell-vars)
                       (string-append
                        (indent depth) (L m3n " := NEW(LetrecCell, val := " tmp ");"))
                       (string-append (indent depth) (L m3n " := " tmp ";"))))
                 bvars bvar-m3names temps))
     (gen-begin body target new-ctx depth))))

;;;
;;; ==================== Named Let ====================
;;;

(define (make-named-let-ctx ctx loop-name loop-params loop-m3names loop-flag
                            . opt-unboxed)
  ;; Create a context for named-let body:
  ;; - ctx-name = loop-name (so gen-expr detects loop tail calls)
  ;; - ctx-self-tail? = #t
  ;; - ctx-params = loop-params (for gen-self-tail-call arg count)
  ;; - ctx-reassign-vars = loop-m3names (reassign targets)
  ;; - ctx-loop-flag = loop-flag (BOOLEAN var name for non-tail WHILE, or #f)
  ;; - param-map extended with loop var -> M3 name mappings
  ;; - opt-unboxed: optional unboxed-params alist
  (let ((new-entries (map cons loop-params loop-m3names))
        (unboxed (if (null? opt-unboxed) '() (car opt-unboxed))))
    (list loop-name
          loop-params
          (ctx-free-vars ctx)
          (ctx-constants ctx)
          (ctx-temp-counter ctx)
          #t  ;; self-tail? = #t for the loop
          (append new-entries (ctx-param-map ctx))
          (ctx-fv-map ctx)
          (ctx-var-counter ctx)
          loop-m3names       ;; reassign-vars
          loop-flag          ;; loop-flag
          unboxed)));        ;; unboxed-params

(define (gen-named-let loop-name bindings body target ctx depth)
  ;; (let loop ((var init) ...) body)
  ;; Compiled as: evaluate inits, declare loop vars, LOOP/WHILE, body.
  ;; Tail calls to loop-name become variable reassignment.
  ;; When target = #f (tail position): uses LOOP (exits via RETURN).
  ;; When target != #f (non-tail): uses WHILE with boolean flag.
  (let* ((bvars (map car bindings))
         (temps (map (lambda (b) (fresh-temp ctx)) bindings))
         (bvar-m3names (map (lambda (bv) (fresh-var ctx)) bvars))
         (loop-flag (if target (fresh-var ctx "BOOLEAN") #f))
         ;; Build a temporary ctx for analyzing the loop body
         (tmp-loop-ctx (make-named-let-ctx ctx loop-name bvars bvar-m3names loop-flag))
         ;; Analyze which named-let vars can be unboxed
         (loop-body (if (null? (cdr body)) (car body) (cons 'begin body)))
         (unboxed (analyze-numeric-params bvars loop-body loop-name tmp-loop-ctx))
         ;; Allocate LONGREAL vars for unboxed params
         (nr-vars (map (lambda (u)
                         (let ((nr-name (fresh-var ctx "LONGREAL")))
                           (cons (car u) nr-name)))
                       unboxed))
         ;; Build the real loop context with unboxed info
         (loop-ctx (make-named-let-ctx ctx loop-name bvars bvar-m3names
                                        loop-flag nr-vars)))
    (string-append
     ;; Evaluate init expressions in outer scope
     (apply string-append
            (map (lambda (b tmp)
                   (gen-expr (cadr b) tmp ctx depth))
                 bindings temps))
     ;; Assign temps to loop variables (boxed)
     (apply string-append
            (map (lambda (m3n tmp)
                   (string-append (indent depth) (L m3n " := " tmp ";")))
                 bvar-m3names temps))
     ;; Initialize unboxed LONGREAL vars by narrowing from boxed temps
     (apply string-append
            (map (lambda (u)
                   (let* ((param-sym (car u))
                          (nr-name (cdr u))
                          (m3n (cdr (assq param-sym
                                         (map cons bvars bvar-m3names)))))
                     (string-append (indent depth)
                                    (L nr-name " := SchemeLongReal.FromO(" m3n ");"))))
                 nr-vars))
     (if loop-flag
         ;; Non-tail: WHILE loop with boolean flag
         (string-append
          (indent depth) (L loop-flag " := TRUE;")
          (indent depth) (L "WHILE " loop-flag " DO")
          (indent (+ depth 1)) (L loop-flag " := FALSE;")
          (gen-begin body target loop-ctx (+ depth 1))
          (indent depth) (L "END;"))
         ;; Tail: infinite LOOP (exits via RETURN)
         (string-append
          (indent depth) (L "LOOP")
          (gen-begin body target loop-ctx (+ depth 1))
          (indent depth) (L "END;"))))))

;;;
;;; ==================== Let* ====================
;;;

(define (gen-let* bindings body target ctx depth)
  (if (null? bindings)
      (gen-begin body target ctx depth)
      (let* ((b (car bindings))
             (bvar (car b))
             (tmp (fresh-temp ctx))
             (bvar-m3name (fresh-var ctx))
             (new-ctx (make-extended-ctx ctx
                        (cons (cons bvar bvar-m3name) (ctx-param-map ctx)))))
        (string-append
         ;; Evaluate init into temp
         (gen-expr (cadr b) tmp ctx depth)
         ;; Assign temp to let*-bound variable
         (indent depth) (L bvar-m3name " := " tmp ";")
         (gen-let* (cdr bindings) body target new-ctx depth)))))

;;;
;;; ==================== Letrec ====================
;;;

(define (fresh-binding ctx)
  ;; Generate a fresh SchemeEnvironmentBinding.T local variable.
  (let* ((counter-cell (ctx-var-counter ctx))
         (n (car counter-cell)))
    (set-car! counter-cell (+ n 1))
    (let ((name (string-append "lb_" (number->string n))))
      (emit-decl name "SchemeEnvironmentBinding.T")
      name)))

(define (gen-letrec bindings body target ctx depth)
  ;; (letrec ((var init) ...) body)
  ;; All vars are in scope during init evaluation.
  ;; Uses SchemeEnvironmentBinding.T cells so lambdas in init expressions
  ;; can capture sibling letrec-bound variables by reference.
  (let* ((bvars (map car bindings))
         (bvar-bindings (map (lambda (bv) (fresh-binding ctx)) bvars))
         ;; fv-map entries: lambdas capture these as binding cells
         (new-fv-entries (map cons bvars bvar-bindings))
         (new-ctx (make-extended-ctx-fv ctx
                    (ctx-param-map ctx)
                    (append new-fv-entries (ctx-fv-map ctx)))))
    (string-append
     ;; Initialize binding cells
     (apply string-append
            (map (lambda (bn)
                   (string-append (indent depth)
                                  (L bn " := NEW(LetrecCell, val := NIL);")))
                 bvar-bindings))
     ;; Assign init values via setB (all vars in scope via new-ctx)
     (apply string-append
            (map (lambda (b bn)
                   (let ((tmp (fresh-temp ctx)))
                     (string-append
                      (gen-expr (cadr b) tmp new-ctx depth)
                      (indent depth) (L bn ".setB(" tmp ");"))))
                 bindings bvar-bindings))
     ;; Evaluate body
     (gen-begin body target new-ctx depth))))

;;;
;;; ==================== Case ====================
;;;

(define (gen-case-clauses clauses key-tmp target ctx depth)
  ;; Generate IF/ELSIF chain for case clauses
  (if (null? clauses)
      (emit-assign target "NIL" depth)
      (let ((clause (car clauses))
            (rest (cdr clauses)))
        (if (eq? (car clause) 'else)
            (gen-begin (cdr clause) target ctx depth)
            ;; datums: ((d1 d2 ...) body ...)
            (let* ((datums (car clause))
                   (datum-tests
                    (apply string-append
                           (let loop ((ds datums) (first #t) (acc '()))
                             (if (null? ds) (reverse acc)
                                 (loop (cdr ds) #f
                                       (cons (string-append
                                              (if first "" " OR ")
                                              "SchemeUtils.Eqv("
                                              key-tmp ", "
                                              (constant-ref (car ds) ctx) ")")
                                             acc)))))))
              (string-append
               (indent depth) (L "IF " datum-tests " THEN")
               (gen-begin (cdr clause) target ctx (+ depth 1))
               (if (null? rest)
                   (string-append
                    (indent depth) (L "ELSE")
                    (emit-assign target "NIL" (+ depth 1))
                    (indent depth) (L "END;"))
                   (string-append
                    (indent depth) (L "ELSE")
                    (gen-case-clauses rest key-tmp target ctx (+ depth 1))
                    (indent depth) (L "END;")))))))))

(define (gen-case key-expr clauses target ctx depth)
  ;; (case key ((d1 d2) body1) ((d3) body2) (else body3))
  (let ((key-val (gen-value key-expr ctx depth)))
    (if key-val
        (let ((key-tmp (fresh-temp ctx)))
          (string-append
           (indent depth) (L key-tmp " := " key-val ";")
           (gen-case-clauses clauses key-tmp target ctx depth)))
        (let ((key-tmp (fresh-temp ctx)))
          (string-append
           (gen-expr key-expr key-tmp ctx depth)
           (gen-case-clauses clauses key-tmp target ctx depth))))))

;;;
;;; ==================== Do ====================
;;;

(define (gen-do var-specs test-clause body target ctx depth)
  ;; (do ((var init step) ...) (test exit-expr ...) body ...)
  (let* ((do-vars (map car var-specs))
         (var-m3names (map (lambda (v) (fresh-var ctx)) do-vars))
         (new-entries (map cons do-vars var-m3names))
         (new-ctx (make-extended-ctx ctx
                    (append new-entries (ctx-param-map ctx))))
         (test-expr (car test-clause))
         (exit-exprs (cdr test-clause))
         ;; Which vars have step expressions?
         (stepped (filter (lambda (spec) (pair? (cddr spec))) var-specs)))
    (string-append
     ;; Evaluate inits in outer scope
     (apply string-append
            (map (lambda (spec m3n)
                   (gen-expr (cadr spec) m3n ctx depth))
                 var-specs var-m3names))
     ;; LOOP
     (indent depth) (L "LOOP")
     ;; Test condition
     (let ((test-val (gen-value test-expr new-ctx (+ depth 1))))
       (if test-val
           (string-append
            (indent (+ depth 1)) (L "IF SchemeBoolean.TruthO(" test-val ") THEN")
            ;; Exit expressions (last one is the result)
            (if (null? exit-exprs)
                (string-append
                 (emit-assign target "NIL" (+ depth 2))
                 (indent (+ depth 2)) (L "EXIT;"))
                (string-append
                 (gen-begin exit-exprs target new-ctx (+ depth 2))
                 (indent (+ depth 2)) (L "EXIT;")))
            (indent (+ depth 1)) (L "END;"))
           (let ((test-tmp (fresh-temp new-ctx)))
             (string-append
              (gen-expr test-expr test-tmp new-ctx (+ depth 1))
              (indent (+ depth 1)) (L "IF SchemeBoolean.TruthO(" test-tmp ") THEN")
              (if (null? exit-exprs)
                  (string-append
                   (emit-assign target "NIL" (+ depth 2))
                   (indent (+ depth 2)) (L "EXIT;"))
                  (string-append
                   (gen-begin exit-exprs target new-ctx (+ depth 2))
                   (indent (+ depth 2)) (L "EXIT;")))
              (indent (+ depth 1)) (L "END;")))))
     ;; Body (side effects, result discarded)
     (if (null? body)
         ""
         (let ((discard (fresh-temp new-ctx)))
           (gen-begin body discard new-ctx (+ depth 1))))
     ;; Step assignments (parallel: evaluate all steps, then assign)
     (if (null? stepped)
         ""
         (let* ((step-temps (map (lambda (s) (fresh-temp new-ctx)) stepped))
                (step-m3names (map (lambda (s)
                                     (cdr (assq (car s) new-entries)))
                                   stepped)))
           (string-append
            (apply string-append
                   (map (lambda (spec tmp)
                          (gen-expr (caddr spec) tmp new-ctx (+ depth 1)))
                        stepped step-temps))
            (apply string-append
                   (map (lambda (m3n tmp)
                          (string-append
                           (indent (+ depth 1)) (L m3n " := " tmp ";")))
                        step-m3names step-temps)))))
     (indent depth) (L "END;"))))

;;;
;;; ==================== And / Or ====================
;;;

(define (gen-and exprs target ctx depth)
  (cond
    ((null? exprs) (emit-assign target "SchemeBoolean.True()" depth))
    ((null? (cdr exprs)) (gen-expr (car exprs) target ctx depth))
    (else
     (let ((tmp (fresh-temp ctx)))
       (string-append
        (gen-expr (car exprs) tmp ctx depth)
        (indent depth) (L "IF NOT SchemeBoolean.TruthO(" tmp ") THEN")
        (emit-assign target tmp (+ depth 1))
        (indent depth) (L "ELSE")
        (gen-and (cdr exprs) target ctx (+ depth 1))
        (indent depth) (L "END;"))))))

(define (gen-or exprs target ctx depth)
  (cond
    ((null? exprs) (emit-assign target "SchemeBoolean.False()" depth))
    ((null? (cdr exprs)) (gen-expr (car exprs) target ctx depth))
    (else
     (let ((tmp (fresh-temp ctx)))
       (string-append
        (gen-expr (car exprs) tmp ctx depth)
        (indent depth) (L "IF SchemeBoolean.TruthO(" tmp ") THEN")
        (emit-assign target tmp (+ depth 1))
        (indent depth) (L "ELSE")
        (gen-or (cdr exprs) target ctx (+ depth 1))
        (indent depth) (L "END;"))))))

;;;
;;; ==================== Not ====================
;;;

(define (gen-not expr target ctx depth)
  (let ((val (gen-value expr ctx depth)))
    (if val
        (emit-assign target
                     (string-append "SchemeBoolean.Truth(NOT SchemeBoolean.TruthO("
                                    val "))")
                     depth)
        (let ((tmp (fresh-temp ctx)))
          (string-append
           (gen-expr expr tmp ctx depth)
           (emit-assign target
                        (string-append "SchemeBoolean.Truth(NOT SchemeBoolean.TruthO("
                                       tmp "))")
                        depth))))))

;;;
;;; ==================== Set! ====================
;;;

(define (gen-set! var val target ctx depth)
  (let* ((val-str (gen-value val ctx depth))
         (param-entry (assq var (ctx-param-map ctx)))
         (fv-entry (if (not param-entry) (assq var (ctx-fv-map ctx)) #f)))
    (if val-str
        (string-append
         (if param-entry
             (string-append (indent depth) (L (cdr param-entry) " := " val-str ";"))
             (string-append (indent depth)
                            (L (cdr fv-entry) ".setB(" val-str ");")))
         (emit-assign target "NIL" depth))
        (let ((tmp (fresh-temp ctx)))
          (string-append
           (gen-expr val tmp ctx depth)
           (if param-entry
               (string-append (indent depth) (L (cdr param-entry) " := " tmp ";"))
               (string-append (indent depth)
                              (L (cdr fv-entry) ".setB(" tmp ");")))
           (emit-assign target "NIL" depth))))))

;;;
;;; ==================== Procedure Call ====================
;;;

(define (gen-call fn args target ctx depth)
  (let ((inline-result (try-inline-primitive fn args target ctx depth)))
    (or inline-result
  (let ((nargs (length args)))
    (let* ((arg-temps (map (lambda (a) (fresh-temp ctx)) args))
           (fn-temp (fresh-temp ctx))
           (fn-val (gen-value fn ctx depth))
           (arg-vals (map (lambda (a) (gen-value a ctx depth)) args)))
      (let ((call-expr
             (cond
               ((= nargs 1)
                (string-append
                 "NARROW(" fn-temp ", SchemeProcedure.T).apply1(interp, "
                 (car arg-temps) ")"))
               ((= nargs 2)
                (string-append
                 "NARROW(" fn-temp ", SchemeProcedure.T).apply2(interp, "
                 (car arg-temps) ", " (cadr arg-temps) ")"))
               (else
                (string-append
                 "NARROW(" fn-temp ", SchemeProcedure.T).apply(interp, "
                 (gen-make-list arg-temps) ")")))))
        (string-append
         ;; Initialize fn and arg temps (values known at this point)
         (if fn-val
             (string-append (indent depth) (L fn-temp " := " fn-val ";"))
             "")
         (apply string-append
                (map (lambda (tmp val)
                       (if val
                           (string-append (indent depth) (L tmp " := " val ";"))
                           ""))
                     arg-temps arg-vals))
         ;; Evaluate non-value fn and args
         (if fn-val "" (gen-expr fn fn-temp ctx depth))
         (apply string-append
                (map (lambda (arg tmp val)
                       (if val "" (gen-expr arg tmp ctx depth)))
                     args arg-temps arg-vals))
         ;; Direct call optimization: if fn is a module-defined proc
         ;; not shadowed by a local, emit pointer-compare guard.
         (let ((direct-info
                (and (symbol? fn)
                     (not (assq fn (ctx-param-map ctx)))
                     (assq fn (ctx-fv-map ctx))
                     (let ((mp (assq fn *module-procs*)))
                       (and mp (cdr mp))))))
           (if direct-info
               (let* ((callee-m3 (car direct-info))
                      (callee-np (cadr direct-info))
                      (callee-rp (caddr direct-info))
                      (direct-var (string-append "direct_" callee-m3))
                      (direct-call-expr
                       (cond
                         ((= nargs 1)
                          (string-append "Apply1_" callee-m3
                            "(" direct-var ", interp, "
                            (car arg-temps) ")"))
                         ((and (= nargs 2)
                               (or (not (= callee-np 1)) callee-rp))
                          (string-append "Apply2_" callee-m3
                            "(" direct-var ", interp, "
                            (car arg-temps) ", " (cadr arg-temps) ")"))
                         ((and (= nargs 0) (= callee-np 0) (not callee-rp))
                          (string-append "Apply0_" callee-m3
                            "(" direct-var ", interp)"))
                         (else
                          (string-append "Apply_" callee-m3
                            "(" direct-var ", interp, "
                            (gen-make-list arg-temps) ")")))))
                 (string-append
                  (indent depth) (L "IF " fn-temp " = " direct-var " THEN")
                  (emit-assign target direct-call-expr (+ depth 1))
                  (indent depth) (L "ELSE")
                  (emit-assign target call-expr (+ depth 1))
                  (indent depth) (L "END;")))
               (emit-assign target call-expr depth))))))))))

(define (gen-make-list temps)
  (if (null? temps)
      "NIL"
      (string-append
       "SchemeUtils.Cons(" (car temps) ", " (gen-make-list (cdr temps))
       ", interp)")))

;;;
;;; ==================== Numeric Unboxing Analysis ====================
;;;
;;; Determines which parameters of a self-tail-call loop can be kept as
;;; unboxed LONGREAL values, eliminating heap allocation in the loop body.
;;;

(define *numeric-ops* '(+ - * quotient remainder))
(define *numeric-cmp-ops* '(= < > <= >=))
(define *all-numeric-ops* '(+ - * quotient remainder = < > <= >=))

(define (expr-numeric-for-param? expr param params ctx)
  ;; Is expr used in a numeric-only way for param?
  ;; Returns #t if every occurrence of param in expr is in a numeric context.
  ;; Bare symbol references to param are allowed because var-ref auto-wraps
  ;; unboxed params with SchemeLongReal.FromLR() (re-boxing at return/escape).
  ;; ctx is used to check whether function names are free vars (inlineable).
  (cond
    ((symbol? expr) #t)  ;; Bare ref OK: var-ref handles re-boxing
    ((number? expr) #t)
    ((string? expr) #t)
    ((boolean? expr) #t)
    ((char? expr) #t)
    ((not (pair? expr)) #t)
    ((eq? (car expr) 'quote) #t)
    ((eq? (car expr) 'if)
     (and (expr-numeric-for-param? (cadr expr) param params ctx)
          (expr-numeric-for-param? (caddr expr) param params ctx)
          (or (not (pair? (cdddr expr)))
              (expr-numeric-for-param? (cadddr expr) param params ctx))))
    ((eq? (car expr) 'cond)
     (let loop ((clauses (cdr expr)))
       (if (null? clauses) #t
           (and (let ((clause (car clauses)))
                  (if (eq? (car clause) 'else)
                      (exprs-numeric-for-param? (cdr clause) param params ctx)
                      (and (expr-numeric-for-param? (car clause) param params ctx)
                           (exprs-numeric-for-param? (cdr clause) param params ctx))))
                (loop (cdr clauses))))))
    ((eq? (car expr) 'begin)
     (exprs-numeric-for-param? (cdr expr) param params ctx))
    ((eq? (car expr) 'let)
     (if (symbol? (cadr expr))
         ;; Named let: conservative — param must not appear
         (not (expr-has-symbol? expr param))
         (and (let loop ((bs (cadr expr)))
                (if (null? bs) #t
                    (and (expr-numeric-for-param? (cadar bs) param params ctx)
                         (loop (cdr bs)))))
              (exprs-numeric-for-param? (cddr expr) param params ctx))))
    ((eq? (car expr) 'let*)
     (and (let loop ((bs (cadr expr)))
            (if (null? bs) #t
                (and (expr-numeric-for-param? (cadar bs) param params ctx)
                     (loop (cdr bs)))))
          (exprs-numeric-for-param? (cddr expr) param params ctx)))
    ((eq? (car expr) 'letrec)
     ;; Conservative: param must not appear in letrec
     (not (expr-has-symbol? expr param)))
    ((eq? (car expr) 'and)
     (exprs-numeric-for-param? (cdr expr) param params ctx))
    ((eq? (car expr) 'or)
     (exprs-numeric-for-param? (cdr expr) param params ctx))
    ((eq? (car expr) 'not)
     (expr-numeric-for-param? (cadr expr) param params ctx))
    ((eq? (car expr) 'set!)
     (expr-numeric-for-param? (caddr expr) param params ctx))
    ((eq? (car expr) 'lambda)
     ;; Lambda captures: param must not appear at all
     (not (expr-has-symbol? expr param)))
    ;; Application: check if this is an inlineable numeric op
    ((and (pair? expr) (symbol? (car expr))
          (memq (car expr) *all-numeric-ops*)
          (not (assq (car expr) (ctx-param-map ctx)))
          (assq (car expr) (ctx-fv-map ctx)))
     ;; Numeric primitive: all args can contain param (numeric context)
     (let loop ((args (cdr expr)))
       (if (null? args) #t
           (and (arg-numeric-for-param? (car args) param params ctx)
                (loop (cdr args))))))
    ;; Self-tail-call: param can appear in the position of a numeric param
    ((and (pair? expr) (symbol? (car expr))
          (eq? (car expr) (ctx-name ctx)))
     ;; Self-tail-call: check that param only appears in its own position
     ;; or in a numeric expression passed to a numeric position.
     ;; For now, conservatively check all args.
     (let loop ((args (cdr expr)))
       (if (null? args) #t
           (and (arg-numeric-for-param? (car args) param params ctx)
                (loop (cdr args))))))
    (else
     ;; General application or unknown form: param must not appear
     (not (expr-has-symbol? expr param)))))

(define (arg-numeric-for-param? expr param params ctx)
  ;; Is expr a numeric-context use of param?
  ;; param can appear directly (as a numeric value) or inside a numeric op.
  (cond
    ((eq? expr param) #t)  ;; direct use as arg to numeric op = OK
    ((symbol? expr) #t)    ;; some other symbol, fine
    ((number? expr) #t)
    ((not (pair? expr))  #t)
    ((eq? (car expr) 'quote) #t)
    ;; Nested numeric op: (+ param 1) etc.
    ((and (symbol? (car expr))
          (memq (car expr) *all-numeric-ops*)
          (not (assq (car expr) (ctx-param-map ctx)))
          (assq (car expr) (ctx-fv-map ctx)))
     (let loop ((args (cdr expr)))
       (if (null? args) #t
           (and (arg-numeric-for-param? (car args) param params ctx)
                (loop (cdr args))))))
    ;; Other expression: param must not appear
    (else (not (expr-has-symbol? expr param)))))

(define (expr-has-symbol? expr sym)
  ;; Does sym appear anywhere in expr?
  (cond
    ((eq? expr sym) #t)
    ((symbol? expr) #f)
    ((not (pair? expr)) #f)
    ((eq? (car expr) 'quote) #f)
    (else (or (expr-has-symbol? (car expr) sym)
              (expr-has-symbol? (cdr expr) sym)))))

(define (exprs-numeric-for-param? exprs param params ctx)
  (cond ((null? exprs) #t)
        ((not (pair? exprs)) #t)
        (else (and (expr-numeric-for-param? (car exprs) param params ctx)
                   (exprs-numeric-for-param? (cdr exprs) param params ctx)))))

(define (tail-call-arg-numeric? expr params ctx)
  ;; Is expr a numeric-valued expression (suitable for assigning to an
  ;; unboxed param in a self-tail-call)?
  ;; Returns #t if expr is: a number literal, a param, or an arithmetic expr.
  (cond
    ((number? expr) #t)
    ((and (symbol? expr) (memq expr params)) #t)
    ((not (pair? expr)) #f)
    ((and (symbol? (car expr))
          (memq (car expr) *numeric-ops*)
          (not (assq (car expr) (ctx-param-map ctx)))
          (assq (car expr) (ctx-fv-map ctx)))
     ;; Arithmetic op: recursively check args
     (let loop ((args (cdr expr)))
       (if (null? args) #t
           (and (tail-call-arg-numeric? (car args) params ctx)
                (loop (cdr args))))))
    (else #f)))

(define (collect-tail-call-args-for-param expr name param-index params)
  ;; Collect all expressions that are passed as the param-index'th argument
  ;; in self-tail-calls to name throughout body expr.
  (cond
    ((not (pair? expr)) '())
    ((eq? (car expr) 'quote) '())
    ((eq? (car expr) 'lambda) '())  ;; opaque boundary
    ((and (eq? (car expr) name)
          (= (length (cdr expr)) (length params)))
     ;; Self-tail-call: extract the arg at param-index
     (let ((arg (list-ref (cdr expr) param-index)))
       (cons arg
             (apply append
                    (map (lambda (a) (collect-tail-call-args-for-param a name param-index params))
                         (cdr expr))))))
    (else
     (apply append
            (map (lambda (sub) (collect-tail-call-args-for-param sub name param-index params))
                 expr)))))

(define (has-forcing-numeric-use? expr param ctx)
  ;; Does param appear in at least one "forcing" numeric context in expr?
  ;; A forcing context is: arg to +,-,*,quotient,remainder,=,<,>,<=,>=
  ;; This ensures the param MUST be numeric (NARROW would fail otherwise),
  ;; justifying the unboxing NARROW at procedure entry.
  (cond
    ((not (pair? expr)) #f)
    ((eq? (car expr) 'quote) #f)
    ((eq? (car expr) 'lambda) #f)  ;; opaque boundary
    ((and (symbol? (car expr))
          (memq (car expr) *all-numeric-ops*)
          (not (assq (car expr) (ctx-param-map ctx)))
          (assq (car expr) (ctx-fv-map ctx)))
     ;; Numeric op: check if param appears as an argument
     (or (let loop ((args (cdr expr)))
           (if (null? args) #f
               (or (eq? (car args) param)
                   (loop (cdr args)))))
         ;; Also check sub-expressions recursively
         (let loop ((subs expr))
           (if (null? subs) #f
               (or (has-forcing-numeric-use? (car subs) param ctx)
                   (has-forcing-numeric-use? (cdr subs) param ctx))))))
    (else
     (or (has-forcing-numeric-use? (car expr) param ctx)
         (has-forcing-numeric-use? (cdr expr) param ctx)))))

(define (analyze-numeric-params params body name ctx)
  ;; Returns an alist mapping numeric-only param symbols to LONGREAL var names.
  ;; A param is numeric-only if:
  ;; 1. Every use in body is in a numeric-compatible context
  ;; 2. At least one use is a "forcing" numeric context (arithmetic/comparison)
  ;; 3. Every value assigned to it in self-tail-calls is a numeric expression
  ;; Condition 2 ensures the param MUST be numeric, so the entry NARROW is safe.
  (if (null? params) '()
      (let loop ((ps params) (i 0) (nr-idx 0) (acc '()))
        (if (null? ps) (reverse acc)
            (let* ((param (car ps))
                   (numeric-use (expr-numeric-for-param? body param params ctx))
                   (forcing (has-forcing-numeric-use? body param ctx))
                   (tail-args (collect-tail-call-args-for-param body name i params))
                   (numeric-assigns
                    (let aloop ((tas tail-args))
                      (if (null? tas) #t
                          (and (tail-call-arg-numeric? (car tas) params ctx)
                               (aloop (cdr tas)))))))
              (if (and numeric-use forcing numeric-assigns)
                  (loop (cdr ps) (+ i 1) (+ nr-idx 1)
                        (cons (cons param
                                    (string-append "nr_" (number->string nr-idx)))
                              acc))
                  (loop (cdr ps) (+ i 1) nr-idx acc)))))))

(define (gen-numeric-value expr ctx)
  ;; Returns a LONGREAL M3 expression string for a simple value, or #f.
  (cond
    ((number? expr)
     (cond ((= expr 0) "0.0d0")
           ((= expr 1) "1.0d0")
           ((integer? expr)
            (string-append (number->string expr) ".0d0"))
           (else (string-append (number->string expr) "d0"))))
    ((symbol? expr)
     (let ((u (assq expr (ctx-unboxed ctx))))
       (if u (cdr u) #f)))
    (else #f)))

;;;
;;; ==================== Inline Built-in Primitives ====================
;;;
;;; Unconditionally inline calls to known built-in primitives when:
;;;   1. The function name is a symbol
;;;   2. Not shadowed by a local (not in param-map)
;;;   3. Is a free variable (in fv-map — i.e. comes from the environment)
;;;   4. Has a known inline expansion at this arity
;;;
;;; No guard for redefinition — user accepts that (set! null? ...) in
;;; compiled modules is unsupported.
;;;

(define (arg-as-longreal scheme-arg m3-expr . opt-ctx)
  ;; If scheme-arg is a compile-time number, emit literal LONGREAL.
  ;; If scheme-arg is an unboxed param (in ctx), use its LONGREAL name directly.
  ;; Otherwise, emit SchemeLongReal.FromO(m3-expr).
  (cond
    ((number? scheme-arg)
     (cond ((= scheme-arg 0) "0.0d0")
           ((= scheme-arg 1) "1.0d0")
           ((integer? scheme-arg)
            (string-append (number->string scheme-arg) ".0d0"))
           (else (string-append (number->string scheme-arg) "d0"))))
    ((and (not (null? opt-ctx))
          (symbol? scheme-arg)
          (assq scheme-arg (ctx-unboxed (car opt-ctx))))
     => (lambda (u) (cdr u)))
    (else
     (string-append "SchemeLongReal.FromO(" m3-expr ")"))))

(define (gen-inline-expr fn-name nargs arg-exprs scheme-args ctx)
  ;; Returns an M3 expression string for inlined primitive, or #f.
  ;; arg-exprs: list of M3 expression strings for evaluated args.
  ;; scheme-args: list of original Scheme arg expressions (for constant detection).
  ;; ctx: compilation context (for unboxed param lookup in arg-as-longreal).
  (cond
    ;; --- Unary predicates ---
    ((and (eq? fn-name 'null?) (= nargs 1))
     (string-append "SchemeBoolean.Truth(" (car arg-exprs) " = NIL)"))
    ((and (eq? fn-name 'pair?) (= nargs 1))
     (string-append "SchemeBoolean.Truth(" (car arg-exprs) " # NIL AND ISTYPE("
                    (car arg-exprs) ", SchemePair.T))"))
    ((and (eq? fn-name 'number?) (= nargs 1))
     (string-append "SchemeBoolean.Truth(SchemeInt.IsNumber(" (car arg-exprs) "))"))
    ((and (eq? fn-name 'boolean?) (= nargs 1))
     (string-append "SchemeBoolean.Truth(" (car arg-exprs)
                    " = SchemeBoolean.True() OR " (car arg-exprs)
                    " = SchemeBoolean.False())"))
    ((and (eq? fn-name 'symbol?) (= nargs 1))
     (string-append "SchemeBoolean.Truth(" (car arg-exprs) " # NIL AND ISTYPE("
                    (car arg-exprs) ", SchemeSymbol.T))"))
    ((and (eq? fn-name 'string?) (= nargs 1))
     (string-append "SchemeBoolean.Truth(" (car arg-exprs) " # NIL AND ISTYPE("
                    (car arg-exprs) ", SchemeString.T))"))
    ((and (eq? fn-name 'char?) (= nargs 1))
     (string-append "SchemeBoolean.Truth(" (car arg-exprs) " # NIL AND ISTYPE("
                    (car arg-exprs) ", SchemeChar.T))"))
    ;; --- Accessors ---
    ((and (eq? fn-name 'car) (= nargs 1))
     (string-append "SchemeUtils.PedanticFirst(" (car arg-exprs) ")"))
    ((and (eq? fn-name 'cdr) (= nargs 1))
     (string-append "SchemeUtils.PedanticRest(" (car arg-exprs) ")"))
    ((and (eq? fn-name 'cadr) (= nargs 1))
     (string-append "SchemeUtils.PedanticFirst(SchemeUtils.PedanticRest("
                    (car arg-exprs) "))"))
    ;; --- Constructor ---
    ((and (eq? fn-name 'cons) (= nargs 2))
     (string-append "SchemeUtils.Cons(" (car arg-exprs) ", "
                    (cadr arg-exprs) ", interp)"))
    ;; --- Binary arithmetic ---
    ((and (eq? fn-name '+) (= nargs 2))
     (string-append "SchemePrimitive.NumericAdd("
                    (car arg-exprs) ", " (cadr arg-exprs) ")"))
    ((and (eq? fn-name '-) (= nargs 2))
     (string-append "SchemePrimitive.NumericSub("
                    (car arg-exprs) ", " (cadr arg-exprs) ")"))
    ((and (eq? fn-name '*) (= nargs 2))
     (string-append "SchemePrimitive.NumericMul("
                    (car arg-exprs) ", " (cadr arg-exprs) ")"))
    ;; --- Binary comparisons ---
    ((and (eq? fn-name '=) (= nargs 2))
     (string-append "SchemeBoolean.Truth(SchemePrimitive.NumericEQ("
                    (car arg-exprs) ", " (cadr arg-exprs) "))"))
    ((and (eq? fn-name '<) (= nargs 2))
     (string-append "SchemeBoolean.Truth(SchemePrimitive.NumericLT("
                    (car arg-exprs) ", " (cadr arg-exprs) "))"))
    ((and (eq? fn-name '>) (= nargs 2))
     (string-append "SchemeBoolean.Truth(SchemePrimitive.NumericGT("
                    (car arg-exprs) ", " (cadr arg-exprs) "))"))
    ((and (eq? fn-name '<=) (= nargs 2))
     (string-append "SchemeBoolean.Truth(SchemePrimitive.NumericLE("
                    (car arg-exprs) ", " (cadr arg-exprs) "))"))
    ((and (eq? fn-name '>=) (= nargs 2))
     (string-append "SchemeBoolean.Truth(SchemePrimitive.NumericGE("
                    (car arg-exprs) ", " (cadr arg-exprs) "))"))
    ;; --- eq? ---
    ((and (eq? fn-name 'eq?) (= nargs 2))
     (string-append "SchemeBoolean.Truth(" (car arg-exprs) " = "
                    (cadr arg-exprs) ")"))
    (else #f)))

(define (gen-inline-bool-expr fn-name nargs arg-exprs scheme-args ctx)
  ;; Returns an M3 BOOLEAN expression string (without Truth wrapping), or #f.
  ;; Used by gen-if for boolean fusion.
  (cond
    ((and (eq? fn-name 'null?) (= nargs 1))
     (string-append (car arg-exprs) " = NIL"))
    ((and (eq? fn-name 'pair?) (= nargs 1))
     (string-append (car arg-exprs) " # NIL AND ISTYPE("
                    (car arg-exprs) ", SchemePair.T)"))
    ((and (eq? fn-name 'number?) (= nargs 1))
     (string-append "SchemeInt.IsNumber(" (car arg-exprs) ")"))
    ((and (eq? fn-name 'boolean?) (= nargs 1))
     (string-append (car arg-exprs) " = SchemeBoolean.True() OR "
                    (car arg-exprs) " = SchemeBoolean.False()"))
    ((and (eq? fn-name 'symbol?) (= nargs 1))
     (string-append (car arg-exprs) " # NIL AND ISTYPE("
                    (car arg-exprs) ", SchemeSymbol.T)"))
    ((and (eq? fn-name 'string?) (= nargs 1))
     (string-append (car arg-exprs) " # NIL AND ISTYPE("
                    (car arg-exprs) ", SchemeString.T)"))
    ((and (eq? fn-name 'char?) (= nargs 1))
     (string-append (car arg-exprs) " # NIL AND ISTYPE("
                    (car arg-exprs) ", SchemeChar.T)"))
    ((and (eq? fn-name '=) (= nargs 2))
     (string-append "SchemePrimitive.NumericEQ("
                    (car arg-exprs) ", " (cadr arg-exprs) ")"))
    ((and (eq? fn-name '<) (= nargs 2))
     (string-append "SchemePrimitive.NumericLT("
                    (car arg-exprs) ", " (cadr arg-exprs) ")"))
    ((and (eq? fn-name '>) (= nargs 2))
     (string-append "SchemePrimitive.NumericGT("
                    (car arg-exprs) ", " (cadr arg-exprs) ")"))
    ((and (eq? fn-name '<=) (= nargs 2))
     (string-append "SchemePrimitive.NumericLE("
                    (car arg-exprs) ", " (cadr arg-exprs) ")"))
    ((and (eq? fn-name '>=) (= nargs 2))
     (string-append "SchemePrimitive.NumericGE("
                    (car arg-exprs) ", " (cadr arg-exprs) ")"))
    ((and (eq? fn-name 'eq?) (= nargs 2))
     (string-append (car arg-exprs) " = " (cadr arg-exprs)))
    (else #f)))

(define (is-inlineable-primitive? fn-name nargs)
  ;; Quick check: is this primitive inlineable at this arity?
  (and (memq fn-name '(null? pair? number? boolean? symbol? string? char?
                        car cdr cadr cons
                        + - *
                        = < > <= >= eq?))
       (or (and (memq fn-name '(null? pair? number? boolean? symbol? string?
                                 char? car cdr cadr))
                (= nargs 1))
           (and (memq fn-name '(cons + - * = < > <= >= eq?))
                (= nargs 2)))))

(define (try-inline-primitive fn args target ctx depth)
  ;; Try to inline a built-in primitive call. Returns M3 code string or #f.
  (and (symbol? fn)
       (not (assq fn (ctx-param-map ctx)))   ;; not shadowed by local
       (assq fn (ctx-fv-map ctx))            ;; is a free variable
       (let ((nargs (length args)))
         (and (is-inlineable-primitive? fn nargs)
              ;; Evaluate each arg: use gen-value for simple, temp for complex
              (let loop ((as args) (m3s '()) (pre '()))
                (if (null? as)
                    (let* ((arg-exprs (reverse m3s))
                           (inline-expr (gen-inline-expr fn nargs arg-exprs args ctx)))
                      (and inline-expr
                           (string-append
                            (apply string-append (reverse pre))
                            (emit-assign target inline-expr depth))))
                    (let ((v (gen-value (car as) ctx depth)))
                      (if v
                          (loop (cdr as) (cons v m3s) pre)
                          (let ((tmp (fresh-temp ctx)))
                            (loop (cdr as)
                                  (cons tmp m3s)
                                  (cons (gen-expr (car as) tmp ctx depth)
                                        pre)))))))))))

(define (try-inline-bool-test test ctx depth)
  ;; Try to inline a predicate call as a raw M3 BOOLEAN expression.
  ;; Returns (pre-code . bool-expr) or #f.
  ;; pre-code is M3 code to evaluate complex args into temps.
  (and (pair? test)
       (symbol? (car test))
       (not (assq (car test) (ctx-param-map ctx)))
       (assq (car test) (ctx-fv-map ctx))
       (let* ((fn-name (car test))
              (args (cdr test))
              (nargs (length args)))
         (and (is-inlineable-primitive? fn-name nargs)
              (let loop ((as args) (m3s '()) (pre '()))
                (if (null? as)
                    (let* ((arg-exprs (reverse m3s))
                           (bool-expr (gen-inline-bool-expr fn-name nargs
                                                            arg-exprs args ctx)))
                      (and bool-expr
                           (cons (apply string-append (reverse pre))
                                 bool-expr)))
                    (let ((v (gen-value (car as) ctx depth)))
                      (if v
                          (loop (cdr as) (cons v m3s) pre)
                          (let ((tmp (fresh-temp ctx)))
                            (loop (cdr as)
                                  (cons tmp m3s)
                                  (cons (gen-expr (car as) tmp ctx depth)
                                        pre)))))))))))

(define (try-inline-compound-bool-test test ctx depth)
  ;; Try to inline (and t1 t2 ...) or (or t1 t2 ...) in if-test position
  ;; as a compound M3 BOOLEAN expression. Returns (pre-code . bool-expr) or #f.
  ;; Only applies when ALL sub-test pre-codes are empty (all args are gen-values).
  (and (pair? test)
       (or (eq? (car test) 'and) (eq? (car test) 'or))
       (pair? (cdr test))          ;; at least one sub-test
       (let* ((combiner (car test))
              (sub-tests (cdr test))
              (m3-op (if (eq? combiner 'and) " AND " " OR ")))
         (let loop ((ts sub-tests) (bools '()))
           (if (null? ts)
               ;; All sub-tests inlined successfully
               (if (null? bools)
                   #f
                   (let combine ((bs (reverse bools)) (acc #f))
                     (if (null? bs)
                         (cons "" acc)  ;; empty pre-code
                         (if (not acc)
                             (combine (cdr bs) (car bs))
                             (combine (cdr bs)
                                      (string-append "(" acc ")" m3-op
                                                     "(" (car bs) ")"))))))
               ;; Try to inline next sub-test
               (let ((inlined (try-inline-bool-test (car ts) ctx depth)))
                 (if (and inlined (string=? (car inlined) ""))
                     ;; No pre-code — safe to combine
                     (loop (cdr ts) (cons (cdr inlined) bools))
                     ;; Sub-test couldn't be inlined or has pre-code;
                     ;; try TruthO fallback for simple expressions
                     (let ((v (gen-value (car ts) ctx depth)))
                       (if v
                           (loop (cdr ts)
                                 (cons (string-append "SchemeBoolean.TruthO(" v ")")
                                       bools))
                           #f)))))))))  ;; give up on compound inlining

;;;
;;; ==================== Self-Tail-Call ====================
;;;

(define (gen-numeric-inline expr ctx)
  ;; Try to evaluate a Scheme arithmetic expression directly as a LONGREAL
  ;; M3 expression string. Returns the expression string or #f.
  ;; Handles: numeric literals, unboxed params, and arithmetic ops on those.
  (cond
    ((number? expr) (gen-numeric-value expr ctx))
    ((symbol? expr)
     (let ((u (assq expr (ctx-unboxed ctx))))
       (if u (cdr u) #f)))
    ((and (pair? expr) (symbol? (car expr))
          (memq (car expr) *numeric-ops*)
          (not (assq (car expr) (ctx-param-map ctx)))
          (assq (car expr) (ctx-fv-map ctx))
          (= (length (cdr expr)) 2))
     (let ((left (gen-numeric-inline (cadr expr) ctx))
           (right (gen-numeric-inline (caddr expr) ctx)))
       (and left right
            (let ((op (cond ((eq? (car expr) '+) " + ")
                            ((eq? (car expr) '-) " - ")
                            ((eq? (car expr) '*) " * ")
                            (else #f))))
              (if op
                  (string-append "(" left op right ")")
                  ;; quotient/remainder need TRUNC/FLOAT
                  (let ((m3-op (if (eq? (car expr) 'quotient) " DIV " " MOD ")))
                    (string-append "FLOAT(TRUNC(" left ")" m3-op "TRUNC(" right "), LONGREAL)")))))))
    (else #f)))

(define (gen-self-tail-call args ctx depth)
  ;; Reassign to the correct M3 variable names and loop.
  ;; Uses ctx-reassign-vars if set (for named let), otherwise a_0, a_1, ...
  ;; For unboxed params: use LONGREAL temps and direct numeric evaluation.
  (let* ((params (ctx-params ctx))
         (unboxed (ctx-unboxed ctx))
         (param-m3names
           (or (ctx-reassign-vars ctx)
               (let loop ((ps params) (i 0) (acc '()))
                 (if (null? ps) (reverse acc)
                     (loop (cdr ps) (+ i 1)
                           (cons (string-append "a_" (number->string i)) acc))))))
         ;; For unboxed params, allocate LONGREAL temps; otherwise SchemeObject.T temps
         (all-temps (map (lambda (p)
                           (let ((u (assq p unboxed)))
                             (if u
                                 (fresh-var ctx "LONGREAL")
                                 (fresh-temp ctx))))
                         params)))
    (string-append
     ;; Evaluate args into temps.  Walk params/args/temps in lockstep;
     ;; stop at the shortest list (args may be shorter than params for
     ;; variadic tail calls detected by has-self-tail-call?).
     (let eval-loop ((ps params) (as args) (ts all-temps) (acc ""))
       (if (or (null? ps) (null? as) (null? ts))
           acc
           (let* ((param (car ps)) (arg (car as)) (tmp (car ts))
                  (u (assq param unboxed))
                  (code
                   (if u
                       ;; Unboxed param: try numeric inline, fallback to gen-expr + NARROW
                       (let ((nr (gen-numeric-inline arg ctx)))
                         (if nr
                             (string-append (indent depth) (L tmp " := " nr ";"))
                             ;; Fallback: evaluate to boxed, then narrow
                             (let ((box-tmp (fresh-temp ctx)))
                               (let ((val (gen-value arg ctx depth)))
                                 (string-append
                                  (if val
                                      (string-append (indent depth) (L box-tmp " := " val ";"))
                                      (gen-expr arg box-tmp ctx depth))
                                  (indent depth) (L tmp " := SchemeLongReal.FromO(" box-tmp ");"))))))
                       ;; Normal boxed param
                       (let ((val (gen-value arg ctx depth)))
                         (if val
                             (string-append (indent depth) (L tmp " := " val ";"))
                             (gen-expr arg tmp ctx depth))))))
             (eval-loop (cdr ps) (cdr as) (cdr ts)
                        (string-append acc code)))))
     ;; Reassign: for unboxed params, assign to nr_N; for boxed, assign to param M3 name
     ;; Only reassign params that had corresponding args above.
     (let reassign-loop ((ps params) (ms param-m3names) (ts all-temps)
                         (as args) (acc ""))
       (if (or (null? ps) (null? ms) (null? ts) (null? as))
           acc
           (let* ((param (car ps)) (m3n (car ms)) (tmp (car ts))
                  (u (assq param unboxed))
                  (code (if u
                            (string-append (indent depth) (L (cdr u) " := " tmp ";"))
                            (string-append (indent depth) (L m3n " := " tmp ";")))))
             (reassign-loop (cdr ps) (cdr ms) (cdr ts) (cdr as)
                            (string-append acc code)))))
     ;; For non-tail named let: set loop flag to continue WHILE
     (let ((flag (ctx-loop-flag ctx)))
       (if flag
           (string-append (indent depth) (L flag " := TRUE;"))
           "")))))

;;;
;;; ==================== Compile a Single Define ====================
;;;

(define (compile-define form serial)
  (let* ((parsed (parse-define form))
         (name (car parsed))
         (params (cadr parsed))
         (body (caddr parsed))
         (rest-param (cadddr parsed))
         ;; All params including rest param for free-variable analysis
         (all-ps (if rest-param (append params (list rest-param)) params))
         (free-vars (unique-symbols (free-variables body all-ps)))
         (constants (unique-constants (collect-constants body)))
         ;; Disable self-tail-call for rest-param functions
         (self-tail (if rest-param #f (has-self-tail-call? body name)))
         (ctx (make-ctx name all-ps free-vars constants self-tail))
         ;; Numeric unboxing analysis: only for self-tail-call functions
         (unboxed (if self-tail
                      (analyze-numeric-params params body name ctx)
                      '()))
         ;; Store unboxed info in the context (field 11)
         (_ (if (not (null? unboxed))
                (set-car! (cdddr (cddddr (cddddr ctx))) unboxed)))
         (m3name (string-append "p" (number->string serial)))
         (nparams (length params)))
    (list
     (cons 'name name)
     (cons 'm3name m3name)
     (cons 'params params)
     (cons 'nparams nparams)
     (cons 'rest-param rest-param)
     (cons 'free-vars free-vars)
     (cons 'constants constants)
     (cons 'self-tail self-tail)
     (cons 'ctx ctx))))

;;;
;;; ==================== Module Generation ====================
;;;

;;;
;;; ==================== Lambda Accumulator ====================
;;;
;;; During code generation, lambda expressions are discovered and
;;; accumulated here. After all top-level procedure bodies are
;;; generated, the accumulated lambda types and procedures are
;;; collected and emitted in the module.
;;;

(define *lambda-types* '())    ;; list of type declaration strings
(define *lambda-procs* '())    ;; list of procedure definition strings
(define *lambda-counter* 0)    ;; serial counter for lam0, lam1, ...
(define *module-procs* '())    ;; alist: name-symbol → (m3name nparams rest-param)

(define (iota n)
  (let loop ((i 0) (acc '()))
    (if (= i n) (reverse acc)
        (loop (+ i 1) (cons i acc)))))

(define (gen-type-decl proc)
  (let* ((m3name (cdr (assq 'm3name proc)))
         (free-vars (cdr (assq 'free-vars proc)))
         (constants (cdr (assq 'constants proc)))
         (nparams (cdr (assq 'nparams proc)))
         (rest-param (cdr (assq 'rest-param proc))))
    (string-append
     (L "  Compiled_" m3name " = SchemeProcedure.T OBJECT")
     (apply string-append
            (map (lambda (fv i)
                   (L "    b_" (number->string i) " : SchemeEnvironmentBinding.T;"))
                 free-vars (iota (length free-vars))))
     (apply string-append
            (map (lambda (c i)
                   (L "    k_" (number->string i) " : SchemeObject.T;"))
                 constants (iota (length constants))))
     (L "  OVERRIDES")
     (L "    apply := Apply_" m3name ";")
     (if rest-param
         ;; Rest-param: always override apply1 and apply2 (delegate to Apply_)
         (string-append
          (L "    apply1 := Apply1_" m3name ";")
          (L "    apply2 := Apply2_" m3name ";"))
         (cond
           ((= nparams 1)
            (L "    apply1 := Apply1_" m3name ";"))
           ((>= nparams 2)
            (string-append
             (L "    apply1 := Apply1_" m3name ";")
             (L "    apply2 := Apply2_" m3name ";")))
           (else "")))
     (L "  END;")
     NL)))

(define (gen-apply-n-proc proc)
  (let* ((m3name (cdr (assq 'm3name proc)))
         (name-pair (assq 'name proc))
         (name (if name-pair (cdr name-pair) #f))
         (params (cdr (assq 'params proc)))
         (nparams (cdr (assq 'nparams proc)))
         (rest-param (cdr (assq 'rest-param proc)))
         (ctx (cdr (assq 'ctx proc)))
         (self-tail (cdr (assq 'self-tail proc)))
         (body (cdr (assq 'body proc)))
         (unboxed (ctx-unboxed ctx))
         (param-m3names (map (lambda (p)
                               (cdr (assq p (ctx-param-map ctx))))
                             params)))
    ;; For rest-param functions, Apply_ has the body; no ApplyN needed
    (if rest-param ""
    (let* ((result (gen-with-decls
                    (lambda ()
                      (gen-expr body #f ctx (if self-tail 3 2)))))
           (decls (car result))
           (body-code (cdr result))
           ;; Merge unboxed LONGREAL declarations into decls
           (nr-decls (map (lambda (u) (cons (cdr u) "LONGREAL")) unboxed))
           (all-decls (append nr-decls decls)))
      (string-append
       (if name
           (L "(* Scheme: (define (" (symbol->string name)
              (apply string-append
                     (map (lambda (p) (string-append " " (symbol->string p)))
                          params))
              ") ...) *)")
           "")
       (L "PROCEDURE Apply" (number->string nparams) "_" m3name
          "(self : Compiled_" m3name ";")
       (L "    interp : Scheme.T")
       (apply string-append
              (map (lambda (m3n)
                     (L "    ; " m3n " : SchemeObject.T"))
                   param-m3names))
       (L "    ) : SchemeObject.T")
       (L "  RAISES { Scheme.E } =")
       (if (null? all-decls)
           ""
           (string-append
            (L "  VAR")
            (apply string-append
                   (map (lambda (d)
                          (L "    " (car d) " : " (cdr d) ";"))
                        all-decls))))
       (if self-tail
           (string-append
            (L "  BEGIN")
            ;; Narrowing assignments for unboxed params before LOOP
            (apply string-append
                   (map (lambda (u)
                          (let* ((param-sym (car u))
                                 (nr-name (cdr u))
                                 (a-name (cdr (assq param-sym (ctx-param-map ctx)))))
                            (L "    " nr-name " := SchemeLongReal.FromO(" a-name ");")))
                        unboxed))
            (L "    LOOP")
            body-code
            (L "    END")
            (L "  END Apply" (number->string nparams) "_" m3name ";")
            NL)
           (string-append
            (L "  BEGIN")
            body-code
            (L "  END Apply" (number->string nparams) "_" m3name ";")
            NL)))))))

(define (gen-apply1-proc proc)
  (let* ((m3name (cdr (assq 'm3name proc)))
         (nparams (cdr (assq 'nparams proc)))
         (rest-param (cdr (assq 'rest-param proc))))
    (if (and (= nparams 1) (not rest-param))
        ""
        (string-append
         (L "PROCEDURE Apply1_" m3name "(self : Compiled_" m3name ";")
         (L "    interp : Scheme.T;")
         (L "    a1 : SchemeObject.T) : SchemeObject.T")
         (L "  RAISES { Scheme.E } =")
         (L "  BEGIN")
         (L "    RETURN Apply_" m3name
            "(self, interp, SchemeUtils.List1(a1, interp))")
         (L "  END Apply1_" m3name ";")
         NL))))

(define (gen-apply2-proc proc)
  (let* ((m3name (cdr (assq 'm3name proc)))
         (nparams (cdr (assq 'nparams proc)))
         (rest-param (cdr (assq 'rest-param proc))))
    (if (and (= nparams 2) (not rest-param))
        ""
        (string-append
         (L "PROCEDURE Apply2_" m3name "(self : Compiled_" m3name ";")
         (L "    interp : Scheme.T;")
         (L "    a1, a2 : SchemeObject.T) : SchemeObject.T")
         (L "  RAISES { Scheme.E } =")
         (L "  BEGIN")
         (L "    RETURN Apply_" m3name
            "(self, interp, SchemeUtils.List2(a1, a2, interp))")
         (L "  END Apply2_" m3name ";")
         NL))))

(define (gen-apply-proc proc)
  (let* ((m3name (cdr (assq 'm3name proc)))
         (name-pair (assq 'name proc))
         (name (if name-pair (cdr name-pair) #f))
         (params (cdr (assq 'params proc)))
         (nparams (cdr (assq 'nparams proc)))
         (rest-param (cdr (assq 'rest-param proc)))
         (ctx (cdr (assq 'ctx proc)))
         (param-m3names (map (lambda (p)
                               (cdr (assq p (ctx-param-map ctx))))
                             params))
         (rest-m3name (if rest-param
                         (cdr (assq rest-param (ctx-param-map ctx)))
                         #f)))
    (string-append
     (if (and rest-param name)
         (L "(* Scheme: (define (" (symbol->string name)
            (apply string-append
                   (map (lambda (p) (string-append " " (symbol->string p)))
                        params))
            " . " (symbol->string rest-param) ") ...) *)")
         "")
     (L "PROCEDURE Apply_" m3name "(self : Compiled_" m3name ";")
     (L "    interp : Scheme.T;")
     (L "    args : SchemeObject.T) : SchemeObject.T")
     (L "  RAISES { Scheme.E } =")
     (if rest-param
         ;; Rest-param function: Apply_ is the primary method with body
         (let* ((body (cdr (assq 'body proc)))
                (result (gen-with-decls
                         (lambda () (gen-expr body #f ctx 2))))
                (decls (car result))
                (body-code (cdr result)))
           (string-append
            (L "  VAR")
            (L "    rest := args;")
            (apply string-append
                   (map (lambda (m3n)
                          (L "    " m3n " : SchemeObject.T;"))
                        param-m3names))
            (L "    " rest-m3name " : SchemeObject.T;")
            ;; Hoisted body declarations
            (apply string-append
                   (map (lambda (d)
                          (L "    " (car d) " : " (cdr d) ";"))
                        decls))
            (L "  BEGIN")
            ;; Extract fixed params
            (apply string-append
                   (map (lambda (m3n)
                          (string-append
                           (L "    " m3n " := SchemeUtils.First(rest);")
                           (L "    rest := SchemeUtils.Rest(rest);")))
                        param-m3names))
            ;; Rest param gets remaining args
            (L "    " rest-m3name " := rest;")
            body-code
            (L "  END Apply_" m3name ";")
            NL))
         ;; Normal function: Apply_ delegates to ApplyN
         (if (= nparams 0)
             (string-append
              (L "  BEGIN")
              (L "    RETURN Apply0_" m3name "(self, interp)")
              (L "  END Apply_" m3name ";")
              NL)
             (string-append
              (L "  VAR")
              (L "    rest := args;")
              (apply string-append
                     (map (lambda (m3n)
                            (L "    " m3n " : SchemeObject.T;"))
                          param-m3names))
              (L "  BEGIN")
              (apply string-append
                     (map (lambda (m3n i)
                            (string-append
                             (L "    " m3n " := SchemeUtils.First(rest);")
                             (if (< i (- nparams 1))
                                 (L "    rest := SchemeUtils.Rest(rest);")
                                 "")))
                          param-m3names (iota nparams)))
              (L "    RETURN Apply" (number->string nparams) "_" m3name "(self, interp"
                 (apply string-append
                        (map (lambda (m3n) (string-append ", " m3n)) param-m3names))
                 ")")
              (L "  END Apply_" m3name ";")
              NL))))))

(define (gen-install-one-proc proc)
  ;; Generate the create/configure/define block for a single compiled proc
  (let* ((m3name (cdr (assq 'm3name proc)))
         (name (cdr (assq 'name proc)))
         (free-vars (cdr (assq 'free-vars proc)))
         (constants (cdr (assq 'constants proc))))
    (string-append
     (L "    (* " (symbol->string name) " *)")
     (L "    obj_" m3name " := NEW(Compiled_" m3name
        ", name := \"" (symbol->string name) " (compiled)\");")
     (apply string-append
            (map (lambda (fv i)
                   (L "    obj_" m3name ".b_" (number->string i)
                      " := env.bind(SchemeSymbol.Symbol(\""
                      (symbol->string fv) "\"));"))
                 free-vars (iota (length free-vars))))
     (apply string-append
            (map (lambda (c i)
                   (L "    obj_" m3name ".k_"
                      (number->string i) " := "
                      (constant-to-m3 c) ";"))
                 constants (iota (length constants))))
     (L "    EVAL env.define(SchemeSymbol.Symbol(\""
        (symbol->string name) "\"), obj_" m3name ");")
     (L "    direct_" m3name " := obj_" m3name ";")
     NL)))

(define (gen-install-proc procs module-name ordered source-basename)
  ;; Collect all external free variables: used by compiled procs but
  ;; not defined by any compiled proc in this module.  These must be
  ;; forward-declared so env.bind() can find them.
  (let* ((defined-names (map (lambda (p) (cdr (assq 'name p))) procs))
         (all-free (apply append (map (lambda (p) (cdr (assq 'free-vars p))) procs)))
         (external-free (filter (lambda (fv) (not (memq fv defined-names)))
                                all-free))
         (unique-external (deduplicate-syms external-free)))
  (string-append
   (L "PROCEDURE Install(interp : Scheme.T) RAISES { Scheme.E } =")
   (L "  VAR")
   (L "    env := NARROW(interp.getGlobalEnvironment(), SchemeEnvironment.T);")
   (apply string-append
          (map (lambda (proc)
                 (let ((m3name (cdr (assq 'm3name proc))))
                   (L "    obj_" m3name " : Compiled_" m3name ";")))
               procs))
   (L "  BEGIN")
   ;; Forward-declare all compiled procedure names so mutually
   ;; recursive bindings can be resolved before any code runs.
   ;; Also forward-declare external free variables so env.bind()
   ;; can find them (they may be defined later by other modules).
   (if (and (null? procs) (null? unique-external))
       ""
       (string-append
        (L "    (* forward declarations *)")
        (apply string-append
               (map (lambda (proc)
                      (let ((name (cdr (assq 'name proc))))
                        (L "    EVAL env.define(SchemeSymbol.Symbol(\""
                           (symbol->string name) "\"), NIL);")))
                    procs))
        (apply string-append
               (map (lambda (fv)
                      (L "    IF NOT env.haveBinding(SchemeSymbol.Symbol(\""
                         (symbol->string fv)
                         "\")) THEN EVAL env.define(SchemeSymbol.Symbol(\""
                         (symbol->string fv) "\"), NIL) END;"))
                    unique-external))
        NL))
   ;; Replay all top-level forms in source order:
   ;; compiled defines are installed natively, everything else
   ;; is evaluated via loadEvalText.
   (apply string-append
          (map (lambda (entry)
                 (if (eq? (car entry) 'compiled)
                     (gen-install-one-proc (cdr entry))
                     (L "    EVAL interp.loadEvalText(\""
                        (m3-escape-string (form->string (cdr entry))) "\");")))
               ordered))
   (L "  END Install;")
   NL)))

(define (deduplicate-syms lst)
  ;; Remove duplicate symbols from a list, preserving order
  (let loop ((rest lst) (seen '()) (acc '()))
    (cond ((null? rest) (reverse acc))
          ((memq (car rest) seen) (loop (cdr rest) seen acc))
          (else (loop (cdr rest) (cons (car rest) seen) (cons (car rest) acc))))))

(define (constant-to-m3 c)
  (cond
    ((and (integer? c) (= c 0)) "SchemeInt.Zero")
    ((and (integer? c) (= c 1)) "SchemeInt.One")
    ((integer? c)
     (string-append "SchemeInt.FromI(" (number->string c) ")"))
    ((number? c)
     (string-append "SchemeLongReal.FromLR(" (number->string c) "d0)"))
    ((string? c)
     (string-append "SchemeString.FromText(\"" (m3-escape-string c) "\")"))
    ((boolean? c)
     (if c "SchemeBoolean.True()" "SchemeBoolean.False()"))
    ((char? c)
     (string-append "SchemeChar.Character(VAL("
                    (number->string (char->integer c)) ", CHAR))"))
    ((symbol? c)
     (string-append "SchemeSymbol.Symbol(\"" (symbol->string c) "\")"))
    ((null? c) "NIL")
    ((pair? c)
     (string-append "SchemeUtils.Cons("
                    (constant-to-m3 (car c)) ", "
                    (constant-to-m3 (cdr c)) ")"))
    (else "NIL")))

(define (m3-escape-string s)
  ;; Escape a string for M3 TEXT literal
  ;; M3 uses backslash escapes similar to C
  (apply string-append
         (map (lambda (c)
                (cond ((char=? c #\") (list->string (list #\\ #\")))
                      ((char=? c #\\) (list->string (list #\\ #\\)))
                      ((char=? c #\newline) (list->string (list #\\ #\n)))
                      (else (list->string (list c)))))
              (string->list s))))

(define (scheme-escape-string s)
  ;; Escape a string for Scheme string literal (quotes and backslashes)
  (apply string-append
         (map (lambda (c)
                (cond ((char=? c #\") (list->string (list #\\ #\")))
                      ((char=? c #\\) (list->string (list #\\ #\\)))
                      (else (list->string (list c)))))
              (string->list s))))

(define (form->string form)
  ;; Serialize a Scheme form to readable text (for embedding in M3 source)
  (cond
    ((string? form) (string-append "\"" (scheme-escape-string form) "\""))
    ((symbol? form) (symbol->string form))
    ((number? form) (number->string form))
    ((null? form) "()")
    ((boolean? form) (if form "#t" "#f"))
    ((char? form)
     (cond ((char=? form #\newline) "#\\newline")
           ((char=? form #\space) "#\\space")
           ((char=? form (integer->char 9)) "#\\tab")
           (else (string-append "#\\" (list->string (list form))))))
    ((vector? form)
     (string-append "#("
       (let loop ((i 0) (acc ""))
         (if (= i (vector-length form))
             (string-append acc ")")
             (loop (+ i 1)
                   (string-append acc (if (= i 0) "" " ")
                                 (form->string (vector-ref form i))))))))
    ((pair? form)
     (string-append "(" (form->string (car form))
                    (let loop ((rest (cdr form)))
                      (cond ((null? rest) ")")
                            ((pair? rest) (string-append " " (form->string (car rest))
                                                         (loop (cdr rest))))
                            (else (string-append " . " (form->string rest) ")"))))))
    (else "")))

(define (build-ordered forms supported compiled-procs)
  ;; Build an ordered list of tagged entries from all top-level forms.
  ;; Each entry is ('compiled . proc-alist) or ('passthrough . form).
  ;; supported and compiled-procs are in source order (subsets of forms).
  (let loop ((fs forms) (ss supported) (cs compiled-procs) (acc '()))
    (if (null? fs)
        (reverse acc)
        (if (and (not (null? ss)) (eq? (car fs) (car ss)))
            ;; This form is a compiled define
            (loop (cdr fs) (cdr ss) (cdr cs)
                  (cons (cons 'compiled (car cs)) acc))
            ;; Everything else is passthrough
            (loop (cdr fs) ss cs
                  (cons (cons 'passthrough (car fs)) acc))))))

;;;
;;; ==================== Lambda Code Generation ====================
;;;

(define (gen-lambda-type-decl m3name binding-caps value-caps constants nparams rest-param)
  ;; Generate type declaration for a lambda closure type.
  (string-append
   (L "  Compiled_" m3name " = SchemeProcedure.T OBJECT")
   (apply string-append
          (map (lambda (bc i)
                 (L "    b_" (number->string i) " : SchemeEnvironmentBinding.T;"))
               binding-caps (iota (length binding-caps))))
   (apply string-append
          (map (lambda (vc i)
                 (L "    cap_" (number->string i) " : SchemeObject.T;"))
               value-caps (iota (length value-caps))))
   (apply string-append
          (map (lambda (c i)
                 (L "    k_" (number->string i) " : SchemeObject.T;"))
               constants (iota (length constants))))
   (L "  OVERRIDES")
   (L "    apply := Apply_" m3name ";")
   (if rest-param
       (string-append
        (L "    apply1 := Apply1_" m3name ";")
        (L "    apply2 := Apply2_" m3name ";"))
       (cond
         ((= nparams 1)
          (L "    apply1 := Apply1_" m3name ";"))
         ((>= nparams 2)
          (string-append
           (L "    apply1 := Apply1_" m3name ";")
           (L "    apply2 := Apply2_" m3name ";")))
         (else "")))
   (L "  END;")
   NL))

(define (gen-lambda-new m3name binding-caps value-caps constants ctx)
  ;; Generate a NEW expression to instantiate a lambda closure.
  (string-append
   "NEW(Compiled_" m3name
   ", name := \"lambda (compiled)\""
   ;; Binding captures: copy binding cell from enclosing scope
   (apply string-append
          (map (lambda (sym i)
                 (let ((fv-entry (assq sym (ctx-fv-map ctx))))
                   (string-append ", b_" (number->string i)
                                  " := " (cdr fv-entry))))
               binding-caps (iota (length binding-caps))))
   ;; Value captures: snapshot current value from enclosing scope
   (apply string-append
          (map (lambda (sym i)
                 (string-append ", cap_" (number->string i)
                                " := " (var-ref sym ctx)))
               value-caps (iota (length value-caps))))
   ;; Constants
   (apply string-append
          (map (lambda (c i)
                 (string-append ", k_" (number->string i)
                                " := " (constant-to-m3 c)))
               constants (iota (length constants))))
   ")"))

(define (gen-lambda expr target ctx depth)
  ;; Compile a lambda expression to a closure instantiation.
  ;; Accumulates the closure type and apply procedures into *lambda-types*
  ;; and *lambda-procs* for later inclusion in the module.
  (let* ((raw-params (cadr expr))
         (parsed (parse-params raw-params))
         (lam-params (car parsed))
         (lam-rest-param (cdr parsed))
         (all-lam-params (if lam-rest-param
                             (append lam-params (list lam-rest-param))
                             lam-params))
         (lam-body (if (null? (cdddr expr)) (caddr expr)
                       (cons 'begin (cddr expr))))
         ;; Compute captures
         (captures (unique-symbols (free-variables lam-body all-lam-params)))
         (binding-caps (filter (lambda (s) (assq s (ctx-fv-map ctx))) captures))
         (value-caps (filter (lambda (s) (assq s (ctx-param-map ctx))) captures))
         ;; Lambda's own constants
         (lam-constants (unique-constants (collect-constants lam-body)))
         ;; Serial number
         (serial *lambda-counter*)
         (_ (set! *lambda-counter* (+ serial 1)))
         (lam-m3name (string-append "lam" (number->string serial)))
         (nparams (length lam-params))
         ;; Build lambda context (include rest param in param-map)
         (lam-param-map (append
                         (build-indexed-map all-lam-params "a_")
                         (build-indexed-map value-caps "self.cap_")))
         (lam-fv-map (build-indexed-map binding-caps "self.b_"))
         (lam-ctx (list (string->symbol lam-m3name)
                        all-lam-params '() lam-constants
                        (list 0) #f  ;; no self-tail-call for anonymous lambda
                        lam-param-map lam-fv-map
                        (list 0) #f #f '()))
         ;; Build proc alist for gen-apply-* functions
         (proc (list (cons 'body lam-body)
                     (cons 'm3name lam-m3name)
                     (cons 'params lam-params)
                     (cons 'nparams nparams)
                     (cons 'rest-param lam-rest-param)
                     (cons 'free-vars '())
                     (cons 'constants lam-constants)
                     (cons 'self-tail #f)
                     (cons 'ctx lam-ctx))))
    ;; Accumulate type declaration
    (set! *lambda-types*
      (cons (gen-lambda-type-decl lam-m3name binding-caps value-caps
                                  lam-constants nparams lam-rest-param)
            *lambda-types*))
    ;; Accumulate apply procedures
    (set! *lambda-procs*
      (cons (let ((np nparams)
                  (rp lam-rest-param))
              (string-append
               (gen-apply-n-proc proc)
               (gen-apply-proc proc)
               (if rp
                   (string-append
                    (gen-apply1-proc proc)
                    (gen-apply2-proc proc))
                   (cond ((= np 1) "")
                         ((= np 2) (gen-apply1-proc proc))
                         (else (string-append
                                (gen-apply1-proc proc)
                                (gen-apply2-proc proc)))))))
            *lambda-procs*))
    ;; Generate instantiation at this site
    (emit-assign target
      (gen-lambda-new lam-m3name binding-caps value-caps lam-constants ctx)
      depth)))

(define (gen-module module-name procs ordered source-basename)
  (let ((i3 (gen-i3 module-name))
        (m3 (gen-m3 module-name procs ordered source-basename)))
    (list (cons 'i3 i3) (cons 'm3 m3))))

(define (gen-i3 module-name)
  (string-append
   (L "(* Generated by MScheme Level 1 Compiler *)")
   (L "INTERFACE " module-name ";")
   (L "IMPORT Scheme;")
   NL
   (L "PROCEDURE Install(interp : Scheme.T) RAISES { Scheme.E };")
   NL
   (L "END " module-name ".")))

(define (gen-m3 module-name procs ordered source-basename)
  ;; Two-phase generation: generate procedure bodies first (which discovers
  ;; and accumulates lambda closures), then assemble the complete module.
  (set! *lambda-types* '())
  (set! *lambda-procs* '())
  (set! *lambda-counter* 0)
  (set! *module-procs*
    (map (lambda (proc)
           (list (cdr (assq 'name proc))
                 (cdr (assq 'm3name proc))
                 (cdr (assq 'nparams proc))
                 (cdr (assq 'rest-param proc))))
         procs))
  (let* ((type-decls (apply string-append (map gen-type-decl procs)))
         ;; Generate all procedure bodies -- this triggers lambda discovery
         (proc-bodies
          (apply string-append
                 (map (lambda (proc)
                        (let ((np (cdr (assq 'nparams proc)))
                              (rp (cdr (assq 'rest-param proc))))
                          (string-append
                           (gen-apply-n-proc proc)
                           (gen-apply-proc proc)
                           (if rp
                               ;; Rest-param: always emit apply1 and apply2 stubs
                               (string-append
                                (gen-apply1-proc proc)
                                (gen-apply2-proc proc))
                               (cond ((= np 1) "")
                                     ((= np 2) (gen-apply1-proc proc))
                                     (else (string-append
                                            (gen-apply1-proc proc)
                                            (gen-apply2-proc proc))))))))
                      procs)))
         ;; Collect accumulated lambda declarations
         (lam-type-decls (apply string-append (reverse *lambda-types*)))
         (lam-proc-bodies (apply string-append (reverse *lambda-procs*))))
    (string-append
     (L "(* Generated by MScheme Level 1 Compiler *)")
     (L "MODULE " module-name ";")
     NL
     (L "IMPORT Scheme, SchemeObject, SchemeProcedure, SchemeProcedureClass;")
     (L "IMPORT SchemeEnvironment, SchemeEnvironmentBinding;")
     (L "IMPORT SchemeSymbol, SchemeBoolean, SchemeLongReal, SchemePair, SchemeInt, SchemePrimitive;")
     (L "IMPORT SchemeUtils, SchemeString, SchemeChar;")
     (L "IMPORT SchemeCompiledRegistry;")
     (L "FROM Scheme IMPORT Object;")
     NL
     (L "TYPE")
     type-decls
     lam-type-decls        ;; lambda types follow regular types
     ;; LetrecCell: lightweight binding cell for letrec-bound variables
     (L "  LetrecCell = SchemeEnvironmentBinding.T OBJECT")
     (L "    val : SchemeObject.T;")
     (L "  OVERRIDES")
     (L "    name := LCName; env := LCEnv; get := LCGet; setB := LCSet;")
     (L "  END;")
     NL
     ;; Module-level VARs for direct call optimization
     (if (null? procs) ""
         (string-append
          (L "VAR")
          (apply string-append
                 (map (lambda (proc)
                        (let ((m3name (cdr (assq 'm3name proc))))
                          (L "  direct_" m3name " : Compiled_" m3name ";")))
                      procs))
          NL))
     ;; LetrecCell method implementations
     (L "PROCEDURE LCName(<*UNUSED*> c : LetrecCell) : SchemeSymbol.T =")
     (L "  BEGIN RETURN NIL END LCName;")
     (L "PROCEDURE LCEnv(<*UNUSED*> c : LetrecCell) : SchemeObject.T =")
     (L "  BEGIN RETURN NIL END LCEnv;")
     (L "PROCEDURE LCGet(c : LetrecCell) : SchemeObject.T =")
     (L "  BEGIN RETURN c.val END LCGet;")
     (L "PROCEDURE LCSet(c : LetrecCell; v : SchemeObject.T) =")
     (L "  BEGIN c.val := v END LCSet;")
     NL
     proc-bodies           ;; regular procs (contain lambda NEW expressions)
     lam-proc-bodies       ;; lambda apply procedures
     (gen-install-proc procs module-name ordered source-basename)
     (L "BEGIN")
     (L "  SchemeCompiledRegistry.Register(\"" module-name "\", Install);")
     (L "  SchemeCompiledRegistry.Register(\"" module-name ".scm\", Install);")
     (if (and source-basename
              (not (string=? source-basename (string-append module-name ".scm"))))
         (string-append
          (L "  SchemeCompiledRegistry.Register(\"" source-basename "\", Install);")
          (let ((bare (strip-scm-extension source-basename)))
            (if bare
                (L "  SchemeCompiledRegistry.Register(\"" bare "\", Install);")
                "")))
         "")
     (L "END " module-name "."))))

;;;
;;; ==================== Compilability Check ====================
;;;
;;; Before compiling a define form, verify that its parameter list and
;;; body use only constructs the compiler supports.  Unsupported forms
;;; are silently skipped -- the interpreter handles them as usual.
;;;

(define (proper-list? x)
  (cond ((null? x) #t)
        ((pair? x) (proper-list? (cdr x)))
        (else #f)))

(define (cond-clauses-compilable? clauses mutated)
  ;; Check that all cond clauses are compilable, including => syntax
  (cond ((null? clauses) #t)
        ((eq? (car (car clauses)) 'else)
         (exprs-compilable? (cdr (car clauses)) mutated))
        ((and (pair? (cdr (car clauses)))
              (eq? (cadr (car clauses)) '=>))
         ;; (test => proc) -- accept if test and proc compilable
         (and (expr-compilable? (caar clauses) mutated)
              (expr-compilable? (caddr (car clauses)) mutated)
              (cond-clauses-compilable? (cdr clauses) mutated)))
        (else (and (exprs-compilable? (car clauses) mutated)
                   (cond-clauses-compilable? (cdr clauses) mutated)))))

;; Helper: does sym not appear anywhere in expr? (for named-let checking)
(define (nlc-not-in? sym expr)
  (cond
    ((symbol? expr) (not (eq? expr sym)))
    ((not (pair? expr)) #t)
    ((eq? (car expr) 'quote) #t)
    (else (and (nlc-not-in? sym (car expr)) (nlc-not-in? sym (cdr expr))))))

(define (nlc-all-not-in? sym lst)
  (cond ((null? lst) #t)
        ((not (pair? lst)) (nlc-not-in? sym lst))
        (else (and (nlc-not-in? sym (car lst)) (nlc-all-not-in? sym (cdr lst))))))

(define (nlc-not-in-bindings? sym bindings)
  (cond ((null? bindings) #t)
        (else (and (nlc-not-in? sym (cadr (car bindings)))
                   (nlc-not-in-bindings? sym (cdr bindings))))))

(define (nlc-in-tail-body? sym exprs)
  (cond ((null? exprs) #t)
        ((null? (cdr exprs)) (nlc-in-tail? sym (car exprs)))
        (else (and (nlc-not-in? sym (car exprs))
                   (nlc-in-tail-body? sym (cdr exprs))))))

(define (nlc-in-tail? sym expr)
  ;; Does expr use sym only in tail-call position?
  (cond
    ((symbol? expr) (not (eq? expr sym)))  ;; name used as value = bad
    ((not (pair? expr)) #t)
    ((eq? (car expr) 'quote) #t)
    ((eq? (car expr) 'if)
     (and (nlc-not-in? sym (cadr expr))
          (nlc-in-tail? sym (caddr expr))
          (or (not (pair? (cdddr expr)))
              (nlc-in-tail? sym (cadddr expr)))))
    ((eq? (car expr) 'cond)
     (let loop ((clauses (cdr expr)))
       (if (null? clauses) #t
           (and (if (eq? (car (car clauses)) 'else)
                    (nlc-in-tail-body? sym (cdr (car clauses)))
                    (and (nlc-not-in? sym (car (car clauses)))
                         (nlc-in-tail-body? sym (cdr (car clauses)))))
                (loop (cdr clauses))))))
    ((eq? (car expr) 'begin)
     (nlc-in-tail-body? sym (cdr expr)))
    ((eq? (car expr) 'let)
     (if (symbol? (cadr expr))
         ;; nested named let — sym is shadowed if same name;
         ;; if different name, reject: cross-loop calls can't be compiled
         (if (eq? (cadr expr) sym) #t
             (and (nlc-not-in-bindings? sym (caddr expr))
                  (nlc-all-not-in? sym (cdddr expr))))
         (and (nlc-not-in-bindings? sym (cadr expr))
              (nlc-in-tail-body? sym (cddr expr)))))
    ((eq? (car expr) 'let*)
     (and (nlc-not-in-bindings? sym (cadr expr))
          (nlc-in-tail-body? sym (cddr expr))))
    ((eq? (car expr) 'letrec)
     (and (nlc-not-in-bindings? sym (cadr expr))
          (nlc-in-tail-body? sym (cddr expr))))
    ((eq? (car expr) 'and) (nlc-in-tail-body? sym (cdr expr)))
    ((eq? (car expr) 'or) (nlc-in-tail-body? sym (cdr expr)))
    ((eq? (car expr) 'not) (nlc-not-in? sym (cadr expr)))
    ((eq? (car expr) 'set!) (nlc-not-in? sym (caddr expr)))
    ((eq? (car expr) 'case)
     (and (nlc-not-in? sym (cadr expr))
          (let loop ((clauses (cddr expr)))
            (if (null? clauses) #t
                (and (if (eq? (car (car clauses)) 'else)
                         (nlc-in-tail-body? sym (cdr (car clauses)))
                         (nlc-in-tail-body? sym (cdr (car clauses))))
                     (loop (cdr clauses)))))))
    ((eq? (car expr) 'do)
     (and (nlc-all-not-in? sym (map cadr (cadr expr)))
          (nlc-all-not-in? sym (filter pair? (map cddr (cadr expr))))
          (nlc-in-tail-body? sym (cdr (caddr expr)))
          (nlc-all-not-in? sym (cdddr expr))))
    ;; Application
    ((eq? (car expr) sym)
     ;; Tail call to sym — check args don't use name
     (nlc-all-not-in? sym (cdr expr)))
    (else
     ;; Other call: name must not appear anywhere
     (nlc-all-not-in? sym expr))))

(define (named-let-calls-ok? loop-name body)
  ;; Verify that loop-name only appears in tail-call position in body.
  (nlc-in-tail? loop-name body))

(define (do-bindings-compilable? bindings mutated)
  ;; (var init step) or (var init)
  (cond ((null? bindings) #t)
        (else (and (expr-compilable? (cadr (car bindings)) mutated)  ;; init
                   (or (null? (cddr (car bindings)))          ;; no step
                       (expr-compilable? (caddr (car bindings)) mutated))  ;; step
                   (do-bindings-compilable? (cdr bindings) mutated)))))

(define (case-clauses-compilable? clauses mutated)
  (cond ((null? clauses) #t)
        ((eq? (car (car clauses)) 'else)
         (exprs-compilable? (cdr (car clauses)) mutated))
        ((not (pair? (car (car clauses)))) #f)  ;; datums must be a list
        (else (and (exprs-compilable? (cdr (car clauses)) mutated)
                   (case-clauses-compilable? (cdr clauses) mutated)))))

(define (expr-compilable? expr mutated)
  (cond
    ((symbol? expr) #t)
    ((number? expr) #t)
    ((string? expr) #t)
    ((boolean? expr) #t)
    ((char? expr) #t)
    ((not (pair? expr)) #t)
    ((eq? (car expr) 'quote) #t)
    ((eq? (car expr) 'lambda)
     (let* ((lam-params (all-params (cadr expr)))
            (lam-body (if (null? (cdddr expr)) (caddr expr)
                          (cons 'begin (cddr expr))))
            (body-mutated (set!-targets lam-body)))
       ;; Level 1: all captures are by reference (env.bind()),
       ;; so mutation of captured variables is safe.
       (expr-compilable? lam-body body-mutated)))
    ((eq? (car expr) 'define) #f)     ;; internal define handled by begin desugaring
    ((eq? (car expr) 'do)
     (and (do-bindings-compilable? (cadr expr) mutated)    ;; var specs
          (exprs-compilable? (caddr expr) mutated)         ;; (test exit-expr ...)
          (exprs-compilable? (cdddr expr) mutated)))
    ((eq? (car expr) 'case)
     (and (expr-compilable? (cadr expr) mutated)  ;; key expression
          (case-clauses-compilable? (cddr expr) mutated)))
    ((eq? (car expr) 'letrec)
     (and (let-bindings-compilable? (cadr expr) mutated)
          (exprs-compilable? (cddr expr) mutated)))
    ((eq? (car expr) 'if)
     (and (pair? (cddr expr))             ;; must have consequent
          (expr-compilable? (cadr expr) mutated)
          (expr-compilable? (caddr expr) mutated)
          (or (not (pair? (cdddr expr)))
              (expr-compilable? (cadddr expr) mutated))))
    ((eq? (car expr) 'cond)
     (cond-clauses-compilable? (cdr expr) mutated))
    ((eq? (car expr) 'begin)
     (let ((desugared (desugar-internal-defines (cdr expr))))
       (if desugared
           (exprs-compilable? desugared mutated)
           (exprs-compilable? (cdr expr) mutated))))
    ((eq? (car expr) 'let)
     (if (symbol? (cadr expr))
         ;; Named let: (let name ((var init) ...) body)
         (and (let-bindings-compilable? (caddr expr) mutated)
              (exprs-compilable? (cdddr expr) mutated)
              ;; All calls to loop name must be in tail position
              (named-let-calls-ok? (cadr expr)
                                   (if (null? (cddddr expr))
                                       (cadddr expr)
                                       (cons 'begin (cdddr expr)))))
         (and (let-bindings-compilable? (cadr expr) mutated)
              (exprs-compilable? (cddr expr) mutated))))
    ((eq? (car expr) 'let*)
     (and (let-bindings-compilable? (cadr expr) mutated)
          (exprs-compilable? (cddr expr) mutated)))
    ((eq? (car expr) 'and)
     (exprs-compilable? (cdr expr) mutated))
    ((eq? (car expr) 'or)
     (exprs-compilable? (cdr expr) mutated))
    ((eq? (car expr) 'not)
     (expr-compilable? (cadr expr) mutated))
    ((eq? (car expr) 'set!)
     (expr-compilable? (caddr expr) mutated))
    ;; Quasiquote is a macro not handled by the compiler
    ((eq? (car expr) 'quasiquote) #f)
    ;; Application: all subexpressions must be compilable
    (else (exprs-compilable? expr mutated))))

(define (exprs-compilable? exprs mutated)
  (cond ((null? exprs) #t)
        ((not (pair? exprs)) #t)
        (else (and (expr-compilable? (car exprs) mutated)
                   (exprs-compilable? (cdr exprs) mutated)))))

(define (let-bindings-compilable? bindings mutated)
  (cond ((null? bindings) #t)
        (else (and (expr-compilable? (cadr (car bindings)) mutated)
                   (let-bindings-compilable? (cdr bindings) mutated)))))

(define (parse-params param-list)
  ;; Split a possibly-dotted parameter list into (fixed-params . rest-param-or-#f).
  ;; (a b c) => ((a b c) . #f)
  ;; (a b . rest) => ((a b) . rest)
  (let loop ((pl param-list) (acc '()))
    (cond ((null? pl) (cons (reverse acc) #f))
          ((pair? pl) (loop (cdr pl) (cons (car pl) acc)))
          ((symbol? pl) (cons (reverse acc) pl))
          (else (cons (reverse acc) #f)))))

(define (all-params param-list)
  ;; Convert a possibly-dotted parameter list to a proper list of all param symbols.
  (let ((parsed (parse-params param-list)))
    (if (cdr parsed)
        (append (car parsed) (list (cdr parsed)))
        (car parsed))))

(define (define-compilable? form)
  ;; Check that a (define (name params...) body) form can be compiled.
  ;; Accepts both proper and dotted parameter lists.
  (if (null? (cddr form))
      #f  ;; no body: (define (name params))
      (let* ((sig (cadr form))
             (body (if (null? (cdddr form))
                       (caddr form)
                       (cons 'begin (cddr form))))
             (mutated (set!-targets body)))
        (and (or (proper-list? (cdr sig))
                 ;; dotted param list: must end in a symbol
                 (symbol? (let tail ((x (cdr sig)))
                            (if (pair? x) (tail (cdr x)) x))))
             (expr-compilable? body mutated)))))

;;;
;;; ==================== Deduplication ====================
;;;

(define (deduplicate-defines defs)
  ;; Keep only the last definition of each name (Scheme semantics:
  ;; later define shadows earlier).  This handles files that are loaded
  ;; multiple times in a concatenated input.
  (let loop ((rest (reverse defs)) (seen '()) (acc '()))
    (if (null? rest)
        acc
        (let ((name (caadr (car rest))))
          (if (memq name seen)
              (loop (cdr rest) seen acc)
              (loop (cdr rest) (cons name seen) (cons (car rest) acc)))))))

;;;
;;; ==================== Top-Level Entry Points ====================
;;;

(define (compile-file input-file module-name)
  (let* ((forms (read-file input-file))
         (all-defines (filter (lambda (f)
                                (and (pair? f) (eq? (car f) 'define)
                                     (pair? (cadr f))
                                     (pair? (cddr f))))  ;; has body
                              forms))
         (deduped (deduplicate-defines all-defines))
         (supported (filter define-compilable? deduped))
         (skipped (filter (lambda (f) (not (define-compilable? f)))
                          deduped))
         (compiled-procs
          (let loop ((defs supported) (serial 0) (acc '()))
            (if (null? defs)
                (reverse acc)
                (let* ((form (car defs))
                       (parsed (parse-define form))
                       (body (caddr parsed))
                       (compiled (compile-define form serial)))
                  (loop (cdr defs) (+ serial 1)
                        (cons (cons (cons 'body body) compiled)
                              acc))))))
         (source-basename (path-basename input-file))
         (ordered (build-ordered forms supported compiled-procs))
         (procs (map cdr (filter (lambda (e) (eq? (car e) 'compiled))
                                 ordered)))
         (module (gen-module module-name procs ordered source-basename))
         (i3-text (cdr (assq 'i3 module)))
         (m3-text (cdr (assq 'm3 module)))
         (i3-file (string-append module-name ".i3"))
         (m3-file (string-append module-name ".m3")))
    (let ((p (open-output-file i3-file)))
      (display i3-text p)
      (close-output-port p))
    (let ((p (open-output-file m3-file)))
      (display m3-text p)
      (close-output-port p))
    (display "Compiled ")
    (display (length supported))
    (display " of ")
    (display (length deduped))
    (display " unique defines to ")
    (display i3-file)
    (display " and ")
    (display m3-file)
    (newline)
    (if (not (= (length all-defines) (length deduped)))
        (begin
          (display "  (Deduplicated: ")
          (display (length all-defines))
          (display " total defines, ")
          (display (- (length all-defines) (length deduped)))
          (display " duplicates removed)")
          (newline)))
    (if (not (null? skipped))
        (begin
          (display "  Skipped (unsupported constructs): ")
          (display (map (lambda (f) (caadr f)) skipped))
          (newline)))
    (list i3-file m3-file)))

(define (read-file filename)
  (let ((p (open-input-file filename)))
    (let loop ((acc '()))
      (let ((form (read p)))
        (if (eof-object? form)
            (begin (close-input-port p) (reverse acc))
            (loop (cons form acc)))))))

(define (compile-and-show input-file module-name)
  (let* ((forms (read-file input-file))
         (all-defines (filter (lambda (f)
                                (and (pair? f) (eq? (car f) 'define)
                                     (pair? (cadr f))
                                     (pair? (cddr f))))  ;; has body
                              forms))
         (deduped (deduplicate-defines all-defines))
         (supported (filter define-compilable? deduped))
         (compiled-procs
          (let loop ((defs supported) (serial 0) (acc '()))
            (if (null? defs)
                (reverse acc)
                (let* ((form (car defs))
                       (parsed (parse-define form))
                       (body (caddr parsed))
                       (compiled (compile-define form serial)))
                  (loop (cdr defs) (+ serial 1)
                        (cons (cons (cons 'body body) compiled)
                              acc))))))
         (source-basename (path-basename input-file))
         (ordered (build-ordered forms supported compiled-procs))
         (procs (map cdr (filter (lambda (e) (eq? (car e) 'compiled))
                                 ordered)))
         (module (gen-module module-name procs ordered source-basename)))
    (display "=== ")
    (display module-name)
    (display ".i3 ===")
    (newline)
    (display (cdr (assq 'i3 module)))
    (newline)
    (display "=== ")
    (display module-name)
    (display ".m3 ===")
    (newline)
    (display (cdr (assq 'm3 module)))
    (newline)))
