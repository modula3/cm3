;;
;; $Id: compiler.scm,v 1.1 2009/06/28 11:38:26 mika Exp $
;;

(require-modules "hashtable" "struct")

(load "~/t/mscheme/sstubgen/program/src/sstubgen.scm")

(define (install-procedures-without-exception-handlers)
  (map (lambda(pn) 
         (define-global-symbol (string->symbol
                                (string-append (car pn) "." (cdr pn)))
           (lambda x
             
             (scheme-procedure-stubs-call pn x '()))))
       (scheme-procedure-stubs-list)))

(install-procedures-without-exception-handlers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-int-hash-table size)
  (define (int-hash i) i)
  (make-hash-table size int-hash))

(define typecode-table (make-string-hash-table 100))

(define (make-typecode-table max-tc)    

  (unwind-protect
   (begin(typecode-table 'update-entry! (rtbrand-getname max-tc) max-tc) #t)
   ()
   ())

  (if (>= max-tc 0)
      (make-typecode-table (- max-tc 1))))

(define (lookup-typecode typename) (typecode-table 'retrieve typename))

(make-typecode-table (RTType.MaxTypecode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define primitive-tc    (lookup-typecode "SchemePrimitive.T"))

(define closure-tc      (lookup-typecode "SchemeClosure.T"))
(define closureclass-tc (lookup-typecode "SchemeClosureClass.Private"))

(define environment-tc  (lookup-typecode "SchemeEnvironment.T"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (closure? x)
  (RTType.IsSubtype (rttype-typecode x) closure-tc))

(define (primitive-procedure? x)
  (RTType.IsSubtype (rttype-typecode x) primitive-tc))

(define (get-proc-body proc)
  (modula-type-op closureclass-tc 'get-field proc 'body))

(define (get-proc-params proc)
  (modula-type-op closureclass-tc 'get-field proc 'params))

(define (get-proc-env proc)
  (modula-type-op closureclass-tc 'get-field proc 'env))

(define (base-env-lookup env name)
  (modula-type-op environment-tc 'call-method env 'lookup (list name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extend-env-with-params env arg-list marker)
  (if (null? arg-list) 
      env
      (env-define (extend-env-with-params env (cdr arg-list) marker) 
                  (car arg-list) marker)))

(define (compile-procedure proc)
  (if (not (closure? proc))
      (error "cant compile " proc))

  (let* ((arg (cons 'arg ())) ;; unique id --> '(arg)
         (env 
          (extend-env-with-params
           (make-env (get-proc-env proc))
           (get-proc-params proc)
           arg))
         )
    env
    

    )
  )

(define (analyze-procedure proc)
  (if (not (closure? proc))
      (error "cant analyze " proc))

  (list (get-proc-params proc)
        (get-proc-body proc)
        (get-proc-env proc)))

(define (macro? op)
  (RTType.IsSubtype (rttype-typecode op) (lookup-typecode "SchemeMacro.T")))

;; this isnt quite right is it?  what if a macro is redefined as a 
;; procedure?

(define (macro-procedure macro)
  (eval `(lambda ,(get-proc-params macro) ,(get-proc-body macro))))

(define (macro-expand macro args)
  (apply (macro-procedure macro) args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; two reps for environments, a modula-3 rep and a scheme rep (assoc list)

(define (make-env base) base)

(define (env-define env var val) (cons (cons var val) env))

(define (env-lookup env var)
  (cond ((not (pair? env)) (base-env-lookup env var))
        ((eq? (caar env) var) (cdar env))
        (else (env-lookup (cdr env) var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char-convert c)
  (if (or (and (char>=? c #\0) (char<=? c #\9))
          (and (char>=? c #\a) (char<=? c #\z))                                 
          (and (char>=? c #\A) (char<=? c #\Z)))

      ;; pass thru
      c
      ;; else
      (string-append "_" (number->string (char->integer c)))))



(define (map-scheme-ident ident)
  ;; make legal m3 identifier
  
  (string-flatten "Scm_"
                  (map char-convert (string->list ident))
                  "(*"ident"*)"))


(define t 0.5)

(define (nested x)
  (define (f y)
    (lambda (z)(+ x y z t)))
  (f 1)
  (f 3)
  )

(define (dotted . x)
  (length x))

(define (lambdafy x)
  ;; remove syntactic sugar for defines
  (if (not (eq? (car x) 'define)) (error "lambdafy can't handle " x))
  
  (if (pair? (cadr x))
      `(define ,(caadr x) (lambda ,(cdadr x) ,@(cddr x)))
      x))

(define (condify expr)
  ;; turn ifs into conds
  `(cond (,(cadr expr) ,(caddr expr))
         (else ,(cadddr expr))))


(define stuff-type
  (make-struct-type
   'stuff-type
   `((global-env () )
     (def-frames (()) )
     (constants ())
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-var-to-current-frame frames sym)
  (cons
   (cons sym (car frames))
   (cdr frames)))

(define (push-new-frame frames)
  (cons '() frames))

(define (search-var-in-frame frame var)
  (let loop ((this-frame (reverse frame))
             (i 0))
    (cond ((null? this-frame) #f)
          ((eq? (car this-frame) var) i)
          (else (loop (cdr this-frame) (+ i 1))))))

(define (search-var frames var)
  (let loop ((p frames)
             (i 0))
    (cond ((null? p) #f)
          ((search-var-in-frame (car p) var) => (lambda(x)(cons i x)))
          (else (loop (cdr p) (+ i 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define global-stuff (stuff-type 'new))

(define (find-member mem lst)
  (cond ((null? lst) #f)
        ((equal? mem (car lst)) 0)
        ((find-member mem (cdr lst)) => (lambda(pos)(+ 1 pos)))
        (else #f)))

(define (make-constant constant stuff)
  (if (not (member? constant (stuff 'get 'constants)))
      (stuff 'set! 'constants (cons constant (stuff 'get 'constants))))
  (string-append "RETURN Constant" 
                 (- (length (stuff 'get 'constants))
                    (find-member constant (stuff 'get 'constants))
                    1)))

(define (extend-frame sym stuff) 
  (let ((oldframes (stuff 'get 'def-frames)))
    (cond ((and (not (null? oldframes))
                (search-var-in-frame (car oldframes) sym)) => 
                (lambda(x) (cons 0 x)))
          (else 
           (stuff 'set! 'def-frames (add-var-to-current-frame 
                                     (stuff 'get 'def-frames) sym))
           (search-var (stuff 'get 'def-frames) sym)))))

(define (analyze-expr expr stuff tail)

  (define (recurse x tail) (analyze-expr x stuff tail))

  (define (add-to-frame sym) (extend-frame sym stuff))

  (dis "analyze-expr " expr dnl)

  (if (pair? expr)
      (case (car expr)
        ((quote)
         (if tail (make-constant (cadr expr) stuff) "")
         )

        ((unwind-protect) 
         ;; hmm, tricky tricky tricky
         )

        ((begin) 
         (let loop ((x (cdr expr)))
           (if (null? x) ""
               (string-append 
                (recurse (car x) (null? (cdr x)))
                (loop (cdr x)))))

         )
        
        ((define)
         (dis (cons (length (stuff 'get 'def-frames))
                    (length (car (stuff 'get 'def-frames))))
              dnl
              )
         (let ((defn (lambdafy expr)))
           (add-to-frame (cadr defn))
           (recurse (caddr defn) #t))
         )

        ((set!) 
         (recurse (cdr expr) #t)
         )
        
        ;; hmm doesn't really need to be here?
        ;; well, need to construct a "proper" SchemeEnvironment.T
        ;;((eval) 
        ;; )

        ((if)
         (recurse (condify expr) #t)
         )

        ((cond)
         )
        
        ((lambda)
         ;; compile this procedure
         )

        (else ;; macro or procedure call
         (if (macro? (car expr))
             (recurse (macro-expand (car expr) (cdr expr) tail))
             (let ((args
                    (map (lambda(a)(recurse a #t) (cdr expr))
                         ))
                   
                   (recurse (car expr) #t))))))

      (if tail (make-constant (cadr expr) stuff) "")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add1 x)(+ x 1))


(define env (get-proc-env add1))

(define (get-tail pr)
  (cond ((null? pr) '())
        ((pair? pr) (get-tail (cdr pr)))
        (else pr)))

(define (mapi op lst)
  (define (loop lst i)
    (cond ((null? lst) '())
          ((not (pair? lst)) '())
          (else (cons (op (car lst) i) (loop (cdr lst) (+ i 1))))))
  (loop lst 0))

(define (make-arg-decl arg i)
  (string-flatten
   "      arg_" arg " := args[" (+ i 1) "];" dnl
   ))


(define (make-local-decl arg i)
  (string-flatten
   "      local_" arg " := args[L - M + " i "];" dnl
   ))

(define (make-var-arg-decl nam)
  (string-flatten
   "      arg_" nam " := MakeList(SUBARRAY(args,N+1,L-M-N));" dnl
   ))

(define (make-lambda-wrapper name args locals)

  (let ((is-varargs (not (list? args))))
    (string-flatten
     "PROCEDURE "name"(VAR args : ARRAY OF REFANY;" dnl
     "                 g : Getter; s : Setter; c : Copier; a : Adapter) : LocalProcedureResult RAISES { E } =" dnl
     ;; 0 is dummy for up-pointer in Frame
     ;; 1..N is args
     ;; N+1..N+M is locals
     dnl
     "  PROCEDURE Get(up, pos : CARDINAL) : REFANY =" dnl
     "    BEGIN RETURN GenericGet(args, frame, g, up, pos) END Get;" dnl
     dnl
     "  PROCEDURE Set(up, pos : CARDINAL; to : REFANY) =" dnl
     "    BEGIN RETURN GenericSet(args, frame, s, up, pos, to) END Set;" dnl
     dnl
     "  PROCEDURE Copy(up : CARDINAL) : REF Frame =" dnl
     "    BEGIN RETURN GenericCopy(args, frame, c, up) END Copy;" dnl
     dnl
     "  PROCEDURE Adapt() : SchemeEnvironment.T =" dnl
     "    BEGIN RETURN GenericAdapt(args, frame, a) END Adapt;" dnl
     dnl
     "  VAR" dnl
     "    frame : REF Frame := NIL;" dnl
     "  BEGIN" dnl
     "    CheckLengthAtLeast(args, " (+ 1 (length args) (length locals)) ");"dnl
     "    args[0] := NIL; (* no up pointer yet *)" dnl
     (if (not is-varargs)
         (string-append 
          "    CheckLengthAtMost(args, " (+ 1 (length args) (length locals)) ");"dnl)                    
         ""
         )
     "    CONST" dnl
     "      N = " (length args) "; (* required args *)" dnl
     "      V = " (if is-varargs "TRUE" "FALSE") "; (* has varargs? *)" dnl
     "      M = " (length locals) "; (* space for locals *)" dnl
     "    VAR" dnl
     "      L := NUMBER(args);" dnl
     (mapi make-arg-decl args)
     (if is-varargs
         (make-var-arg-decl (get-tail args))
         ""
         )
     (mapi make-local-decl locals)
     
     "    BEGIN" dnl
     "    END" dnl
     "  END "name";" dnl
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-to-frame sym env)
  (env 'set! 'def-frames (add-var-to-current-frame
                          (env 'get 'def-frames) sym)))

(define (push-lookup-symbol s e tail)
  (cond ((search-var (e 'get 'def-frames) s) => 
         (lambda(x) `((push-value ,x))))
        ((primitive-procedure? (eval s)) `((push-primitive ,s)))
        (else `((push-global ,s)))))


(define (push-x x e tail)
  (if (pair? x)
      (push-pair-x x e tail)

      (if (symbol? x) 
          (push-lookup-symbol x e tail)
          (list
           `(push-constant ,x)
           ))))

(define (push-pair-x x e tail)
  (case (car x)
    ((quote)
     (list `(push-constant ,(cadr x)))
     )

    ((define)
     (let ((defn (lambdafy expr)))
       (extend-frame (cadr defn) e)))

    ((begin)
     (let loop ((x (cdr x)))
       (cond ((null? x) '())
             ((null? (cdr x)) (append  (push-x (car x) e tail)))
             (else (append 
                    (push-x (car x) e #f)
                    '((pop))
                    (loop (cdr x))))))
     )
    )
  )
