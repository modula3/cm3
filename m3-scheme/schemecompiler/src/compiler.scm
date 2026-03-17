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
;; Copyright (c) 2026 Mika Nystrom.  All rights reserved.
;;

(require-modules "basic-defs")

;; Newline character for code generation
(define NL (list->string (list #\newline)))

;; Append newline to string (line terminator helper)
(define (L . strs) (string-append (apply string-append strs) NL))

;;;
;;; ==================== Identifier Mapping ====================
;;;

(define *m3-ident-table*
  '((#\+ . "plus") (#\- . "minus") (#\* . "star") (#\/ . "slash")
    (#\= . "eq") (#\< . "lt") (#\> . "gt") (#\! . "B") (#\? . "Q")
    (#\. . "_dot_") (#\@ . "_at_") (#\~ . "_tilde_") (#\^ . "_caret_")
    (#\% . "_pct_") (#\& . "_amp_")))

(define (m3-char c)
  (cond ((assq c *m3-ident-table*) => cdr)
        ((or (and (char>=? c #\a) (char<=? c #\z))
             (and (char>=? c #\A) (char<=? c #\Z))
             (and (char>=? c #\0) (char<=? c #\9))
             (char=? c #\_))
         (list->string (list c)))
        (else (string-append "_" (number->string (char->integer c))))))

(define (m3-ident sym)
  ;; Convert a Scheme symbol to a legal Modula-3 identifier
  (let ((s (if (symbol? sym) (symbol->string sym) sym)))
    (apply string-append (map m3-char (string->list s)))))

;;;
;;; ==================== AST Analysis ====================
;;;

(define (parse-define form)
  ;; Returns (name params body) or #f
  (if (and (pair? form) (eq? (car form) 'define) (pair? (cadr form)))
      (list (caadr form) (cdadr form) (if (null? (cdddr form))
                                          (caddr form)
                                          (cons 'begin (cddr form))))
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
                       (append (free-variables (car clause) bound)
                               (free-vars-body (cdr clause) bound))))
                 (cdr expr))))
    ((eq? (car expr) 'begin)
     (free-vars-body (cdr expr) bound))
    ((eq? (car expr) 'let)
     (let* ((bindings (cadr expr))
            (bvars (map car bindings))
            (inits-free (apply append
                              (map (lambda (b)
                                     (free-variables (cadr b) bound))
                                   bindings))))
       (append inits-free
               (free-vars-body (cddr expr) (append bvars bound)))))
    ((eq? (car expr) 'let*)
     (let loop ((bindings (cadr expr))
                (bound bound)
                (acc '()))
       (if (null? bindings)
           (append acc (free-vars-body (cddr expr) bound))
           (loop (cdr bindings)
                 (cons (caar bindings) bound)
                 (append acc (free-variables (cadar bindings) bound))))))
    ((eq? (car expr) 'and)
     (apply append (map (lambda (e) (free-variables e bound)) (cdr expr))))
    ((eq? (car expr) 'or)
     (apply append (map (lambda (e) (free-variables e bound)) (cdr expr))))
    ((eq? (car expr) 'set!)
     (append (if (memq (cadr expr) bound) '() (list (cadr expr)))
             (free-variables (caddr expr) bound)))
    (else
     ;; application: all subexpressions
     (apply append (map (lambda (e) (free-variables e bound)) expr)))))

(define (free-vars-body exprs bound)
  (apply append (map (lambda (e) (free-variables e bound)) exprs)))

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
    (else (apply append (map collect-constants (cdr expr))))))

(define (unique-constants lst)
  (let loop ((rest lst) (acc '()))
    (cond ((null? rest) (reverse acc))
          ((member? (car rest) acc) (loop (cdr rest) acc))
          (else (loop (cdr rest) (cons (car rest) acc))))))

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
     (has-self-tail-call? (last (cddr expr)) name))
    ((eq? (car expr) 'let*)
     (has-self-tail-call? (last (cddr expr)) name))
    ((eq? (car expr) 'and) #f)
    ((eq? (car expr) 'or) #f)
    ((eq? (car expr) 'set!) #f)
    ((and (pair? expr) (eq? (car expr) name)) #t)
    (else #f)))

;;;
;;; ==================== Compilation Context ====================
;;;

(define (make-ctx name params free-vars constants self-tail?)
  (list name params free-vars constants (list 0) self-tail?))

(define (ctx-name ctx) (car ctx))
(define (ctx-params ctx) (cadr ctx))
(define (ctx-free-vars ctx) (caddr ctx))
(define (ctx-constants ctx) (cadddr ctx))
(define (ctx-temp-counter ctx) (car (cddddr ctx)))
(define (ctx-self-tail? ctx) (cadr (cddddr ctx)))

(define (fresh-temp ctx)
  (let* ((counter-cell (car (cddddr ctx)))
         (n (car counter-cell)))
    (set-car! counter-cell (+ n 1))
    (string-append "t_" (number->string n))))

;;;
;;; ==================== Variable and Constant References ====================
;;;

(define (var-ref sym ctx)
  (cond
    ((memq sym (ctx-params ctx))
     (string-append "a_" (m3-ident sym)))
    ((memq sym (ctx-free-vars ctx))
     (string-append "self.b_" (m3-ident sym) ".get()"))
    (else
     (string-append "self.b_" (m3-ident sym) ".get()"))))

(define (constant-index val ctx)
  (let loop ((cs (ctx-constants ctx)) (i 0))
    (cond ((null? cs) #f)
          ((equal? (car cs) val) i)
          (else (loop (cdr cs) (+ i 1))))))

(define (constant-ref val ctx)
  (let ((idx (constant-index val ctx)))
    (if idx
        (string-append "self.k_" (number->string idx))
        (cond ((and (integer? val) (= val 0)) "SchemeLongReal.Zero")
              ((and (integer? val) (= val 1)) "SchemeLongReal.One")
              ((integer? val)
               (string-append "SchemeLongReal.FromI(" (number->string val) ")"))
              ((number? val)
               (string-append "SchemeLongReal.FromLR(" (number->string val) "d0)"))
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
     (gen-let (cadr expr) (cddr expr) target ctx depth))
    ((eq? (car expr) 'let*)
     (gen-let* (cadr expr) (cddr expr) target ctx depth))
    ((eq? (car expr) 'and)
     (gen-and (cdr expr) target ctx depth))
    ((eq? (car expr) 'or)
     (gen-or (cdr expr) target ctx depth))
    ((eq? (car expr) 'set!)
     (gen-set! (cadr expr) (caddr expr) target ctx depth))
    ((eq? (car expr) 'not)
     (gen-not (cadr expr) target ctx depth))
    ;; Self-tail-call in tail position
    ((and (not target)
          (ctx-self-tail? ctx)
          (pair? expr)
          (eq? (car expr) (ctx-name ctx)))
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
  (let ((test-val (gen-value test ctx depth)))
    (if test-val
        (string-append
         (indent depth) (L "IF SchemeBoolean.TruthO(" test-val ") THEN")
         (gen-expr con target ctx (+ depth 1))
         (if (not (eq? alt '*no-alt*))
             (string-append
              (indent depth) (L "ELSE")
              (gen-expr alt target ctx (+ depth 1)))
             (if target
                 (string-append
                  (indent depth) (L "ELSE")
                  (emit-assign target "NIL" (+ depth 1)))
                 ""))
         (indent depth) (L "END;"))
        (let ((tmp (fresh-temp ctx)))
          (string-append
           (indent depth) (L "VAR " tmp " : SchemeObject.T; BEGIN")
           (gen-expr test tmp ctx (+ depth 1))
           (indent (+ depth 1)) (L "IF SchemeBoolean.TruthO(" tmp ") THEN")
           (gen-expr con target ctx (+ depth 2))
           (if (not (eq? alt '*no-alt*))
               (string-append
                (indent (+ depth 1)) (L "ELSE")
                (gen-expr alt target ctx (+ depth 2)))
               (if target
                   (string-append
                    (indent (+ depth 1)) (L "ELSE")
                    (emit-assign target "NIL" (+ depth 2)))
                   ""))
           (indent (+ depth 1)) (L "END")
           (indent depth) (L "END;"))))))

;;;
;;; ==================== Cond ====================
;;;

(define (gen-cond clauses target ctx depth)
  (if (null? clauses)
      (emit-assign target "NIL" depth)
      (let ((clause (car clauses))
            (rest (cdr clauses)))
        (if (eq? (car clause) 'else)
            (gen-begin (cdr clause) target ctx depth)
            (let ((test-val (gen-value (car clause) ctx depth)))
              (if test-val
                  (string-append
                   (indent depth) (L "IF SchemeBoolean.TruthO(" test-val ") THEN")
                   (gen-begin (cdr clause) target ctx (+ depth 1))
                   (if (null? rest)
                       (if target
                           (string-append
                            (indent depth) (L "ELSE")
                            (emit-assign target "NIL" (+ depth 1))
                            (indent depth) (L "END;"))
                           (string-append (indent depth) (L "END;")))
                       (string-append
                        (indent depth) (L "ELSE")
                        (gen-cond rest target ctx (+ depth 1))
                        (indent depth) (L "END;"))))
                  (let ((tmp (fresh-temp ctx)))
                    (string-append
                     (indent depth) (L "VAR " tmp " : SchemeObject.T; BEGIN")
                     (gen-expr (car clause) tmp ctx (+ depth 1))
                     (indent (+ depth 1)) (L "IF SchemeBoolean.TruthO(" tmp ") THEN")
                     (gen-begin (cdr clause) target ctx (+ depth 2))
                     (if (null? rest)
                         (if target
                             (string-append
                              (indent (+ depth 1)) (L "ELSE")
                              (emit-assign target "NIL" (+ depth 2))
                              (indent (+ depth 1)) (L "END"))
                             (string-append (indent (+ depth 1)) (L "END")))
                         (string-append
                          (indent (+ depth 1)) (L "ELSE")
                          (gen-cond rest target ctx (+ depth 2))
                          (indent (+ depth 1)) (L "END")))
                     (indent depth) (L "END;")))))))))

;;;
;;; ==================== Begin ====================
;;;

(define (gen-begin exprs target ctx depth)
  (cond
    ((null? exprs)
     (emit-assign target "NIL" depth))
    ((null? (cdr exprs))
     (gen-expr (car exprs) target ctx depth))
    (else
     (let ((discard (fresh-temp ctx)))
       (string-append
        (indent depth) (L "VAR " discard " : SchemeObject.T; BEGIN")
        (gen-expr (car exprs) discard ctx (+ depth 1))
        (gen-begin (cdr exprs) target ctx (+ depth 1))
        (indent depth) (L "END;"))))))

;;;
;;; ==================== Let ====================
;;;

(define (gen-let bindings body target ctx depth)
  (let* ((bvars (map car bindings))
         (temps (map (lambda (b) (fresh-temp ctx)) bindings))
         (new-ctx (list (ctx-name ctx)
                        (append bvars (ctx-params ctx))
                        (ctx-free-vars ctx)
                        (ctx-constants ctx)
                        (car (cddddr ctx))
                        (ctx-self-tail? ctx))))
    (string-append
     ;; Evaluate init expressions into temps (outer scope, no shadowing)
     (indent depth) (L "VAR")
     (apply string-append
            (map (lambda (tmp)
                   (string-append (indent (+ depth 1))
                                  (L tmp " : SchemeObject.T;")))
                 temps))
     (indent depth) (L "BEGIN")
     (apply string-append
            (map (lambda (b tmp)
                   (gen-expr (cadr b) tmp ctx (+ depth 1)))
                 bindings temps))
     ;; Declare let-bound variables in nested scope, init from temps
     (indent (+ depth 1)) (L "VAR")
     (apply string-append
            (map (lambda (b tmp)
                   (string-append (indent (+ depth 2))
                                  (L "a_" (m3-ident (car b))
                                     " : SchemeObject.T := " tmp ";")))
                 bindings temps))
     (indent (+ depth 1)) (L "BEGIN")
     (gen-begin body target new-ctx (+ depth 2))
     (indent (+ depth 1)) (L "END;")
     (indent depth) (L "END;"))))

;;;
;;; ==================== Let* ====================
;;;

(define (gen-let* bindings body target ctx depth)
  (if (null? bindings)
      (gen-begin body target ctx depth)
      (let* ((b (car bindings))
             (bvar (car b))
             (tmp (fresh-temp ctx))
             (new-ctx (list (ctx-name ctx)
                            (cons bvar (ctx-params ctx))
                            (ctx-free-vars ctx)
                            (ctx-constants ctx)
                            (car (cddddr ctx))
                            (ctx-self-tail? ctx))))
        (string-append
         ;; Evaluate init in outer scope (into temp)
         (indent depth) (L "VAR " tmp " : SchemeObject.T; BEGIN")
         (gen-expr (cadr b) tmp ctx (+ depth 1))
         ;; Declare let*-bound variable in nested scope, init from temp
         (indent (+ depth 1)) (L "VAR a_" (m3-ident bvar)
                                  " : SchemeObject.T := " tmp "; BEGIN")
         (gen-let* (cdr bindings) body target new-ctx (+ depth 2))
         (indent (+ depth 1)) (L "END;")
         (indent depth) (L "END;")))))

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
        (indent depth) (L "VAR " tmp " : SchemeObject.T; BEGIN")
        (gen-expr (car exprs) tmp ctx (+ depth 1))
        (indent (+ depth 1)) (L "IF NOT SchemeBoolean.TruthO(" tmp ") THEN")
        (emit-assign target tmp (+ depth 2))
        (indent (+ depth 1)) (L "ELSE")
        (gen-and (cdr exprs) target ctx (+ depth 2))
        (indent (+ depth 1)) (L "END")
        (indent depth) (L "END;"))))))

(define (gen-or exprs target ctx depth)
  (cond
    ((null? exprs) (emit-assign target "SchemeBoolean.False()" depth))
    ((null? (cdr exprs)) (gen-expr (car exprs) target ctx depth))
    (else
     (let ((tmp (fresh-temp ctx)))
       (string-append
        (indent depth) (L "VAR " tmp " : SchemeObject.T; BEGIN")
        (gen-expr (car exprs) tmp ctx (+ depth 1))
        (indent (+ depth 1)) (L "IF SchemeBoolean.TruthO(" tmp ") THEN")
        (emit-assign target tmp (+ depth 2))
        (indent (+ depth 1)) (L "ELSE")
        (gen-or (cdr exprs) target ctx (+ depth 2))
        (indent (+ depth 1)) (L "END")
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
           (indent depth) (L "VAR " tmp " : SchemeObject.T; BEGIN")
           (gen-expr expr tmp ctx (+ depth 1))
           (emit-assign target
                        (string-append "SchemeBoolean.Truth(NOT SchemeBoolean.TruthO("
                                       tmp "))")
                        (+ depth 1))
           (indent depth) (L "END;"))))))

;;;
;;; ==================== Set! ====================
;;;

(define (gen-set! var val target ctx depth)
  (let ((val-str (gen-value val ctx depth)))
    (if val-str
        (string-append
         (if (memq var (ctx-params ctx))
             (string-append (indent depth) (L "a_" (m3-ident var) " := " val-str ";"))
             (string-append (indent depth)
                            (L "self.b_" (m3-ident var) ".setB(" val-str ");")))
         (if target (emit-assign target "NIL" depth) ""))
        (let ((tmp (fresh-temp ctx)))
          (string-append
           (indent depth) (L "VAR " tmp " : SchemeObject.T; BEGIN")
           (gen-expr val tmp ctx (+ depth 1))
           (if (memq var (ctx-params ctx))
               (string-append (indent (+ depth 1)) (L "a_" (m3-ident var) " := " tmp ";"))
               (string-append (indent (+ depth 1))
                              (L "self.b_" (m3-ident var) ".setB(" tmp ");")))
           (if target (emit-assign target "NIL" (+ depth 1)) "")
           (indent depth) (L "END;"))))))

;;;
;;; ==================== Procedure Call ====================
;;;

(define (gen-call fn args target ctx depth)
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
         (indent depth) (L "VAR")
         (indent (+ depth 1)) (L fn-temp " : SchemeObject.T"
                                 (if fn-val (string-append " := " fn-val) "")
                                 ";")
         (apply string-append
                (map (lambda (tmp val)
                       (string-append
                        (indent (+ depth 1)) (L tmp " : SchemeObject.T"
                                                (if val (string-append " := " val) "")
                                                ";")))
                     arg-temps arg-vals))
         (indent depth) (L "BEGIN")
         (if fn-val "" (gen-expr fn fn-temp ctx (+ depth 1)))
         (apply string-append
                (map (lambda (arg tmp val)
                       (if val "" (gen-expr arg tmp ctx (+ depth 1))))
                     args arg-temps arg-vals))
         (emit-assign target call-expr (+ depth 1))
         (indent depth) (L "END;"))))))

(define (gen-make-list temps)
  (if (null? temps)
      "NIL"
      (string-append
       "SchemeUtils.Cons(" (car temps) ", " (gen-make-list (cdr temps))
       ", interp)")))

;;;
;;; ==================== Self-Tail-Call ====================
;;;

(define (gen-self-tail-call args ctx depth)
  (let* ((params (ctx-params ctx))
         (all-temps (map (lambda (p) (fresh-temp ctx)) params)))
    (string-append
     (indent depth) (L "VAR")
     (apply string-append
            (map (lambda (tmp)
                   (string-append (indent (+ depth 1))
                                  (L tmp " : SchemeObject.T;")))
                 all-temps))
     (indent depth) (L "BEGIN")
     (apply string-append
            (map (lambda (arg tmp)
                   (let ((val (gen-value arg ctx depth)))
                     (if val
                         (string-append
                          (indent (+ depth 1)) (L tmp " := " val ";"))
                         (gen-expr arg tmp ctx (+ depth 1)))))
                 args all-temps))
     (apply string-append
            (map (lambda (param tmp)
                   (string-append
                    (indent (+ depth 1))
                    (L "a_" (m3-ident param) " := " tmp ";")))
                 params all-temps))
     (indent depth) (L "END;"))))

;;;
;;; ==================== Compile a Single Define ====================
;;;

(define (compile-define form)
  (let* ((parsed (parse-define form))
         (name (car parsed))
         (params (cadr parsed))
         (body (caddr parsed))
         (free-vars (unique-symbols (free-variables body params)))
         (constants (unique-constants (collect-constants body)))
         (self-tail (has-self-tail-call? body name))
         (ctx (make-ctx name params free-vars constants self-tail))
         (m3name (m3-ident name))
         (nparams (length params)))
    (list
     (cons 'name name)
     (cons 'm3name m3name)
     (cons 'params params)
     (cons 'nparams nparams)
     (cons 'free-vars free-vars)
     (cons 'constants constants)
     (cons 'self-tail self-tail)
     (cons 'ctx ctx))))

;;;
;;; ==================== Module Generation ====================
;;;

(define (iota n)
  (let loop ((i 0) (acc '()))
    (if (= i n) (reverse acc)
        (loop (+ i 1) (cons i acc)))))

(define (gen-type-decl proc)
  (let* ((m3name (cdr (assq 'm3name proc)))
         (free-vars (cdr (assq 'free-vars proc)))
         (constants (cdr (assq 'constants proc)))
         (nparams (cdr (assq 'nparams proc))))
    (string-append
     (L "  Compiled_" m3name " = SchemeProcedure.T OBJECT")
     (apply string-append
            (map (lambda (fv)
                   (L "    b_" (m3-ident fv) " : SchemeEnvironmentBinding.T;"))
                 free-vars))
     (apply string-append
            (map (lambda (c i)
                   (L "    k_" (number->string i) " : SchemeObject.T;"))
                 constants (iota (length constants))))
     (L "  OVERRIDES")
     (L "    apply := Apply_" m3name ";")
     (cond
       ((= nparams 1)
        (L "    apply1 := Apply1_" m3name ";"))
       ((>= nparams 2)
        (string-append
         (L "    apply1 := Apply1_" m3name ";")
         (L "    apply2 := Apply2_" m3name ";")))
       (else ""))
     (L "  END;")
     NL)))

(define (gen-apply-n-proc proc)
  (let* ((m3name (cdr (assq 'm3name proc)))
         (params (cdr (assq 'params proc)))
         (nparams (cdr (assq 'nparams proc)))
         (ctx (cdr (assq 'ctx proc)))
         (self-tail (cdr (assq 'self-tail proc)))
         (body (cdr (assq 'body proc))))
    (string-append
     (L "PROCEDURE Apply" (number->string nparams) "_" m3name
        "(self : Compiled_" m3name ";")
     (L "    interp : Scheme.T")
     (apply string-append
            (map (lambda (p)
                   (L "    ; a_" (m3-ident p) " : SchemeObject.T"))
                 params))
     (L "    ) : SchemeObject.T")
     (L "  RAISES { Scheme.E } =")
     (if self-tail
         (string-append
          (L "  BEGIN")
          (L "    LOOP")
          (gen-expr body #f ctx 3)
          (L "    END")
          (L "  END Apply" (number->string nparams) "_" m3name ";")
          NL)
         (string-append
          (L "  BEGIN")
          (gen-expr body #f ctx 2)
          (L "  END Apply" (number->string nparams) "_" m3name ";")
          NL)))))

(define (gen-apply1-proc proc)
  (let* ((m3name (cdr (assq 'm3name proc)))
         (nparams (cdr (assq 'nparams proc))))
    (if (= nparams 1)
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
         (nparams (cdr (assq 'nparams proc))))
    (if (= nparams 2)
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
         (params (cdr (assq 'params proc)))
         (nparams (cdr (assq 'nparams proc))))
    (string-append
     (L "PROCEDURE Apply_" m3name "(self : Compiled_" m3name ";")
     (L "    interp : Scheme.T;")
     (L "    args : SchemeObject.T) : SchemeObject.T")
     (L "  RAISES { Scheme.E } =")
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
                 (map (lambda (p)
                        (L "    a_" (m3-ident p) " : SchemeObject.T;"))
                      params))
          (L "  BEGIN")
          (apply string-append
                 (map (lambda (p i)
                        (string-append
                         (L "    a_" (m3-ident p) " := SchemeUtils.First(rest);")
                         (if (< i (- nparams 1))
                             (L "    rest := SchemeUtils.Rest(rest);")
                             "")))
                      params (iota nparams)))
          (L "    RETURN Apply" (number->string nparams) "_" m3name "(self, interp"
             (apply string-append
                    (map (lambda (p) (string-append ", a_" (m3-ident p))) params))
             ")")
          (L "  END Apply_" m3name ";")
          NL)))))

(define (gen-install-proc procs module-name)
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
   ;; Forward-declare all procedure names so self-recursive and
   ;; mutually recursive bindings can be resolved
   (L "    (* forward declarations *)")
   (apply string-append
          (map (lambda (proc)
                 (let ((name (cdr (assq 'name proc))))
                   (L "    EVAL env.define(SchemeSymbol.Symbol(\""
                      (symbol->string name) "\"), NIL);")))
               procs))
   NL
   (apply string-append
          (map (lambda (proc)
                 (let* ((m3name (cdr (assq 'm3name proc)))
                        (name (cdr (assq 'name proc)))
                        (free-vars (cdr (assq 'free-vars proc)))
                        (constants (cdr (assq 'constants proc))))
                   (string-append
                    (L "    obj_" m3name " := NEW(Compiled_" m3name
                       ", name := \"" (symbol->string name) " (compiled)\");")
                    (apply string-append
                           (map (lambda (fv)
                                  (L "    obj_" m3name ".b_" (m3-ident fv)
                                     " := env.bind(SchemeSymbol.Symbol(\""
                                     (symbol->string fv) "\"));"))
                                free-vars))
                    (apply string-append
                           (map (lambda (c i)
                                  (L "    obj_" m3name ".k_"
                                     (number->string i) " := "
                                     (constant-to-m3 c) ";"))
                                constants (iota (length constants))))
                    (L "    EVAL env.define(SchemeSymbol.Symbol(\""
                       (symbol->string name) "\"), obj_" m3name ");")
                    NL)))
               procs))
   (L "  END Install;")
   NL))

(define (constant-to-m3 c)
  (cond
    ((and (integer? c) (= c 0)) "SchemeLongReal.Zero")
    ((and (integer? c) (= c 1)) "SchemeLongReal.One")
    ((integer? c)
     (string-append "SchemeLongReal.FromI(" (number->string c) ")"))
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

(define (gen-module module-name procs)
  (let ((i3 (gen-i3 module-name))
        (m3 (gen-m3 module-name procs)))
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

(define (gen-m3 module-name procs)
  (string-append
   (L "(* Generated by MScheme Level 1 Compiler *)")
   (L "MODULE " module-name ";")
   NL
   (L "IMPORT Scheme, SchemeObject, SchemeProcedure, SchemeProcedureClass;")
   (L "IMPORT SchemeEnvironment, SchemeEnvironmentBinding;")
   (L "IMPORT SchemeSymbol, SchemeBoolean, SchemeLongReal;")
   (L "IMPORT SchemeUtils, SchemeString, SchemeChar;")
   (L "FROM Scheme IMPORT Object;")
   NL
   (L "TYPE")
   (apply string-append (map gen-type-decl procs))
   (apply string-append
          (map (lambda (proc)
                 (string-append
                  (gen-apply-n-proc proc)
                  (gen-apply-proc proc)
                  (let ((np (cdr (assq 'nparams proc))))
                    (cond ((= np 1) "")
                          ((= np 2) (gen-apply1-proc proc))
                          (else (string-append
                                 (gen-apply1-proc proc)
                                 (gen-apply2-proc proc)))))))
               procs))
   (gen-install-proc procs module-name)
   (L "BEGIN")
   (L "END " module-name ".")))

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

(define (cond-clauses-compilable? clauses)
  ;; Reject cond clauses that use => syntax
  (cond ((null? clauses) #t)
        ((eq? (car (car clauses)) 'else)
         (exprs-compilable? (cdr (car clauses))))
        ((and (pair? (cdr (car clauses)))
              (eq? (cadr (car clauses)) '=>))
         #f)
        (else (and (exprs-compilable? (car clauses))
                   (cond-clauses-compilable? (cdr clauses))))))

(define (expr-compilable? expr)
  ;; Return #t if expr uses only constructs the compiler handles.
  (cond
    ((symbol? expr) #t)
    ((number? expr) #t)
    ((string? expr) #t)
    ((boolean? expr) #t)
    ((char? expr) #t)
    ((not (pair? expr)) #t)
    ((eq? (car expr) 'quote) #t)
    ((eq? (car expr) 'lambda) #f)     ;; closures not supported
    ((eq? (car expr) 'define) #f)     ;; internal define not supported
    ((eq? (car expr) 'do) #f)         ;; do loop not supported
    ((eq? (car expr) 'case) #f)       ;; case not supported
    ((eq? (car expr) 'letrec) #f)     ;; letrec not supported
    ((eq? (car expr) 'if)
     (and (expr-compilable? (cadr expr))
          (expr-compilable? (caddr expr))
          (or (not (pair? (cdddr expr)))
              (expr-compilable? (cadddr expr)))))
    ((eq? (car expr) 'cond)
     (cond-clauses-compilable? (cdr expr)))
    ((eq? (car expr) 'begin)
     (exprs-compilable? (cdr expr)))
    ((eq? (car expr) 'let)
     (if (symbol? (cadr expr))
         #f  ;; named let not supported
         (and (let-bindings-compilable? (cadr expr))
              (exprs-compilable? (cddr expr)))))
    ((eq? (car expr) 'let*)
     (and (let-bindings-compilable? (cadr expr))
          (exprs-compilable? (cddr expr))))
    ((eq? (car expr) 'and)
     (exprs-compilable? (cdr expr)))
    ((eq? (car expr) 'or)
     (exprs-compilable? (cdr expr)))
    ((eq? (car expr) 'not)
     (expr-compilable? (cadr expr)))
    ((eq? (car expr) 'set!)
     (expr-compilable? (caddr expr)))
    ;; Application: all subexpressions must be compilable
    (else (exprs-compilable? expr))))

(define (exprs-compilable? exprs)
  (cond ((null? exprs) #t)
        ((not (pair? exprs)) #t)
        (else (and (expr-compilable? (car exprs))
                   (exprs-compilable? (cdr exprs))))))

(define (let-bindings-compilable? bindings)
  (cond ((null? bindings) #t)
        (else (and (expr-compilable? (cadr (car bindings)))
                   (let-bindings-compilable? (cdr bindings))))))

(define (define-compilable? form)
  ;; Check that a (define (name params...) body) form can be compiled.
  (let ((sig (cadr form))
        (body (if (null? (cdddr form))
                  (caddr form)
                  (cons 'begin (cddr form)))))
    (and (proper-list? (cdr sig))       ;; no rest parameters
         (expr-compilable? body))))

;;;
;;; ==================== Top-Level Entry Points ====================
;;;

(define (compile-file input-file module-name)
  (let* ((forms (read-file input-file))
         (all-defines (filter (lambda (f)
                                (and (pair? f) (eq? (car f) 'define)
                                     (pair? (cadr f))))
                              forms))
         (supported (filter define-compilable? all-defines))
         (skipped (filter (lambda (f) (not (define-compilable? f)))
                          all-defines))
         (procs (map (lambda (form)
                       (let* ((parsed (parse-define form))
                              (body (caddr parsed))
                              (compiled (compile-define form)))
                         (cons (cons 'body body) compiled)))
                     supported))
         (module (gen-module module-name procs))
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
    (display (length all-defines))
    (display " defines to ")
    (display i3-file)
    (display " and ")
    (display m3-file)
    (newline)
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
                                     (pair? (cadr f))))
                              forms))
         (supported (filter define-compilable? all-defines))
         (procs (map (lambda (form)
                       (let* ((parsed (parse-define form))
                              (body (caddr parsed))
                              (compiled (compile-define form)))
                         (cons (cons 'body body) compiled)))
                     supported))
         (module (gen-module module-name procs)))
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
