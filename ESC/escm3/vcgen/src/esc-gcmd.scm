;;; esc-gcmd.scm -- Translate M3 AST + specs into guarded commands
;;;
;;; Copyright (c) 2026, Mika Nystrom.  All rights reserved.
;;;
;;; Guarded commands are S-expression lists:
;;;   (assert e)        -- assert condition e
;;;   (assume e)        -- assume condition e
;;;   (seq s1 s2 ...)   -- sequential composition
;;;   (choice s1 s2)    -- nondeterministic choice
;;;   (var x s)         -- introduce local variable x
;;;   (assign x e)      -- assignment (pre-passification)
;;;   (havoc vars)      -- havoc a list of variables
;;;
;;; Translation rules from Modula-3:
;;;
;;; Assignment: x := e
;;;   -> (assign x e)
;;;
;;; IF c THEN s1 ELSE s2 END
;;;   -> (choice (seq (assume c) <T(s1)>) (seq (assume (not c)) <T(s2)>))
;;;
;;; WHILE c DO s END  with LOOPINV I, MODIFIES mods
;;;   -> (seq (assert I) (havoc mods) (assume I)
;;;        (choice (seq (assume c) <T(s)> (assert I) (assume #f))
;;;                (assume (not c))))
;;;
;;; Procedure call f(args) with REQUIRES pre, ENSURES post, MODIFIES mods
;;;   -> (seq (assert pre[args]) (havoc mods) (assume post[args]))
;;;
;;; LOCK mu DO s END
;;;   -> (seq (assert (< (sup LL) mu))
;;;        (assign LL (INSERT mu LL))
;;;        <T(s)>
;;;        (assign LL (DELETE mu LL)))

;; Generate guarded command for a procedure body
;;   proc-spec: parsed ProcSpec (from M3 side)
;;   body-stmts: list of statement AST nodes (from M3 side)
;;   specs-env: alist of (name . ProcSpec) for called procedures
(define (generate-gcmd proc-spec body-stmts specs-env type-env)
  (let ((requires (proc-spec-requires proc-spec))
        (ensures (proc-spec-ensures proc-spec))
        (modifies (proc-spec-modifies proc-spec)))
    `(seq
      ;; Assume precondition
      ,@(map (lambda (r) `(assume ,r)) requires)
      ;; Translate body
      ,@(map (lambda (s) (translate-stmt s specs-env type-env)) body-stmts)
      ;; Assert postcondition
      ,@(map (lambda (e) `(assert ,e)) ensures))))

;; Translate a single statement to a guarded command
(define (translate-stmt stmt specs-env type-env)
  (let ((kind (stmt-kind stmt)))
    (cond
     ((eq? kind 'assign)
      (translate-assign stmt type-env))
     ((eq? kind 'if)
      (translate-if stmt specs-env type-env))
     ((eq? kind 'while)
      (translate-while stmt specs-env type-env))
     ((eq? kind 'call)
      (translate-call stmt specs-env type-env))
     ((eq? kind 'lock)
      (translate-lock stmt specs-env type-env))
     ((eq? kind 'block)
      (translate-block stmt specs-env type-env))
     ((eq? kind 'return)
      (translate-return stmt type-env))
     ((eq? kind 'raise)
      '(assume |@false|))  ;; exceptional exit -- assume false for normal path
     (else
      `(skip)))))

;; Assignment: x := e  ->  (assign x <E(e)>)
(define (translate-assign stmt type-env)
  (let ((lhs (assign-lhs stmt))
        (rhs (assign-rhs stmt)))
    `(assign ,(translate-lhs lhs type-env) ,(translate-expr rhs type-env))))

;; IF c THEN s1 ELSE s2 END
(define (translate-if stmt specs-env type-env)
  (let ((cond-expr (translate-expr (if-cond stmt) type-env))
        (then-stmts (if-then stmt))
        (else-stmts (if-else stmt)))
    `(choice
      (seq (assume ,cond-expr)
           ,@(map (lambda (s) (translate-stmt s specs-env type-env))
                  then-stmts))
      (seq (assume ,(sx-not cond-expr))
           ,@(map (lambda (s) (translate-stmt s specs-env type-env))
                  (or else-stmts '()))))))

;; WHILE c INV I DO s END
(define (translate-while stmt specs-env type-env)
  (let ((cond-expr (translate-expr (while-cond stmt) type-env))
        (loop-inv (while-invariant stmt))
        (body-stmts (while-body stmt))
        (modifies (while-modifies stmt)))
    `(seq
      ,@(if loop-inv `((assert ,loop-inv)) '())
      (havoc ,modifies)
      ,@(if loop-inv `((assume ,loop-inv)) '())
      (choice
       (seq (assume ,cond-expr)
            ,@(map (lambda (s) (translate-stmt s specs-env type-env))
                   body-stmts)
            ,@(if loop-inv `((assert ,loop-inv)) '())
            (assume |@false|))
       (assume ,(sx-not cond-expr))))))

;; Procedure call: f(args)
(define (translate-call stmt specs-env type-env)
  (let* ((proc-name (call-name stmt))
         (args (call-args stmt))
         (spec (assoc proc-name specs-env)))
    (if spec
        (let ((pre (proc-spec-requires (cdr spec)))
              (post (proc-spec-ensures (cdr spec)))
              (mods (proc-spec-modifies (cdr spec))))
          `(seq
            ,@(map (lambda (r) `(assert ,r)) pre)
            (havoc ,mods)
            ,@(map (lambda (e) `(assume ,e)) post)))
        ;; No spec available -- havoc nothing, assume nothing
        `(skip))))

;; LOCK mu DO s END
(define (translate-lock stmt specs-env type-env)
  (let ((mu (translate-expr (lock-mu stmt) type-env))
        (body (lock-body stmt)))
    `(seq
      (assert ,(sx-lt (list 'sup 'LL) mu))
      (assign LL ,(list 'INSERT mu 'LL))
      ,@(map (lambda (s) (translate-stmt s specs-env type-env)) body)
      (assign LL ,(list 'DELETE mu 'LL)))))

;; Block: BEGIN decls; stmts END
(define (translate-block stmt specs-env type-env)
  (let ((stmts (block-stmts stmt)))
    `(seq ,@(map (lambda (s) (translate-stmt s specs-env type-env)) stmts))))

;; RETURN e
(define (translate-return stmt type-env)
  (let ((expr (return-expr stmt)))
    (if expr
        `(seq (assign RES ,(translate-expr expr type-env))
              (assume |@false|))  ;; exit normal flow
        `(assume |@false|))))

;; Translate an expression to a Simplify term
(define (translate-expr expr type-env)
  ;; For now, expressions come through as pre-built S-expressions
  ;; from the M3 side (ESCSpecParse produces Expr objects which
  ;; ESCDriver converts to S-expressions before calling Scheme)
  expr)

;; Translate an lvalue
(define (translate-lhs lhs type-env)
  ;; For now, same as translate-expr
  lhs)

;; Stub accessors -- these will be replaced with real M3 interop
(define (stmt-kind s) (if (pair? s) (car s) 'skip))
(define (assign-lhs s) (cadr s))
(define (assign-rhs s) (caddr s))
(define (if-cond s) (cadr s))
(define (if-then s) (caddr s))
(define (if-else s) (if (> (length s) 3) (cadddr s) '()))
(define (while-cond s) (cadr s))
(define (while-invariant s) (if (> (length s) 4) (list-ref s 4) #f))
(define (while-body s) (caddr s))
(define (while-modifies s) (if (> (length s) 3) (cadddr s) '()))
(define (call-name s) (cadr s))
(define (call-args s) (cddr s))
(define (lock-mu s) (cadr s))
(define (lock-body s) (cddr s))
(define (block-stmts s) (cdr s))
(define (return-expr s) (if (> (length s) 1) (cadr s) #f))
(define (proc-spec-requires s) (or (assoc-val 'requires s) '()))
(define (proc-spec-ensures s) (or (assoc-val 'ensures s) '()))
(define (proc-spec-modifies s) (or (assoc-val 'modifies s) '()))
(define (assoc-val key alist)
  (let ((pair (assoc key alist)))
    (if pair (cdr pair) #f)))
