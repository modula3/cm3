;;; Section 4.1.1

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((quoted? exp) (text-of-quotation exp))
        ((variable? exp) (lookup-variable-value exp env))
        ((definition? exp) (eval-definition exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((lambda? exp) (make-procedure exp env))
        ((conditional? exp) (eval-cond (clauses exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment
                         (parameters procedure)
                         arguments
                         (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
        (else (cons (eval (first-operand exps) env)
                    (list-of-values (rest-operands exps)
                                    env)))))

(define (eval-cond clist env)
  (cond ((no-clauses? clist) nil)
        ((else-clause? (first-clause clist))
         (eval-sequence (actions (first-clause clist)) env))
        ((true? (eval (predicate (first-clause clist)) env))
         (eval-sequence (actions (first-clause clist)) env))
        (else (eval-cond (rest-clauses clist) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (let ((new-value (eval (assignment-value exp) env)))
    (set-variable-value! (assignment-variable exp)
                         new-value
                         env)
    new-value))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  (definition-variable exp))

;;; Section 4.1.2 -- Representing expressions

;;; numbers

(define (self-evaluating? exp) (number? exp))

;;; quote

(define (quoted? exp)
  (if (atom? exp)
      nil
      (eq? (car exp) 'quote)))

(define (text-of-quotation exp) (cadr exp))

;;; variables

(define (variable? exp) (symbol? exp))

;;; assignment

(define (assignment? exp)
  (if (atom? exp)
      nil
      (eq? (car exp) 'set!)))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;;; definitions

(define (definition? exp)
  (if (atom? exp)
      nil
      (eq? (car exp) 'define)))

(define (definition-variable exp)
  (if (variable? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp) 
  (if (variable? (cadr exp))
      (caddr exp)
      (cons 'lambda
            (cons (cdadr exp)     ;formal parameters
                  (cddr exp)))))  ;body

;;; lambda expressions

(define (lambda? exp)
  (if (atom? exp)
      nil
      (eq? (car exp) 'lambda)))

;;; conditionals
(define (conditional? exp)
  (if (atom? exp)
      nil
      (eq? (car exp) 'cond)))

(define (clauses exp) (cdr exp))

(define (no-clauses? clauses) (null? clauses))

(define (first-clause clauses) (car clauses))

(define (rest-clauses clauses) (cdr clauses))

(define (predicate clause) (car clause))

(define (actions clause) (cdr clause))

(define (true? x) (not (null? x)))

(define (else-clause? clause)
  (eq? (predicate clause) 'else))

;;; sequences

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

;;; procedure applications

(define (application? exp) (not (atom? exp)))

(define (operator app) (car app))

(define (operands app) (cdr app))

(define (no-operands? args) (null? args))

(define (first-operand args) (car args))

(define (rest-operands args) (cdr args))

;;; Representation of procedure objects
;;; (This is actually not part of the represenation of expressions)

(define (make-procedure lambda-exp env)
  (list 'procedure lambda-exp env))

(define (compound-procedure? proc)
  (if (atom? proc)
      nil
      (eq? (car proc) 'procedure)))

(define (parameters proc) (cadr (cadr proc)))

(define (procedure-body proc) (cddr (cadr proc)))

(define (procedure-environment proc) (caddr proc))

;;; Section 4.1.3

;;; Operations on environments

(define (lookup-variable-value var env)
  (let ((b (binding-in-env var env)))
    (if (found-binding? b)
        (binding-value b)
        (error "Unbound variable" var))))

(define (binding-in-env var env)
  (if (no-more-frames? env)
      no-binding
      (let ((b (binding-in-frame var (first-frame env))))
        (if (found-binding? b)
            b
            (binding-in-env var (rest-frames env))))))

(define (extend-environment variables values base-env)
  (adjoin-frame (make-frame variables values) base-env))

(define (set-variable-value! var val env)
  (let ((b (binding-in-env var env)))
    (if (found-binding? b)
        (set-binding-value! b val)
        (error "Unbound variable" var))))

(define (define-variable! var val env)
  (let ((b (binding-in-frame var (first-frame env))))
    (if (found-binding? b)
        (set-binding-value! b val)
        (set-first-frame!
          env
          (adjoin-binding (make-binding var val)
                          (first-frame env))))))

;;; Representing environments

(define (first-frame env) (car env))

(define (rest-frames env) (cdr env))

(define (no-more-frames? env) (null? env))

(define (adjoin-frame frame env) (cons frame env))

(define (set-first-frame! env new-frame)
  (set-car! env new-frame))

;;; Representing frames

(define (make-frame variables values)
  (cond ((and (null? variables) (null? values)) '())
        ((null? variables)
         (error "Too many values supplied" values))
        ((null? values)
         (error "Too few values supplied" variables))
        (else
         (cons (make-binding (car variables) (car values))
               (make-frame (cdr variables) (cdr values))))))

(define (adjoin-binding binding frame)
  (cons binding frame))

(define (assq key bindings)
  (cond ((null? bindings) no-binding)
        ((eq? key (binding-variable (car bindings))) 
         (car bindings))
        (else (assq key (cdr bindings)))))

(define (binding-in-frame var frame)
  (assq var frame))

(define (found-binding? b)
  (not (eq? b no-binding)))

(define no-binding nil)

;;; Representing bindings

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (set-binding-value! binding value)
  (set-cdr! binding value))

;;; Section 4.1.4 Running the evaluator

(define primitive-procedure-names
  '(car cdr cons
        ;;** add names of more primitives
        ))

(define primitive-procedure-objects
  '((primitive car)
    (primitive cdr)
    (primitive cons)
    ;;** add more primitives
    ))

(define (setup-environment)
  (let ((initial-env
         (extend-environment primitive-procedure-names
                             primitive-procedure-objects
                             '())))
    (define-variable! 'nil nil initial-env)
    (define-variable! 't (not nil) initial-env)
    initial-env))

(define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (if (atom? proc)
      nil
      (eq? (car proc) 'primitive)))

(define (primitive-id proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (let ((p (primitive-id proc)))
    (cond ((eq? p 'car) (car (car args)))
          ((eq? p 'cdr) (cdr (car args)))
          ((eq? p 'cons) (cons (car args) (cadr args)))
          ;;** add more primitives
          (else (error "Unknown primitive procedure" proc)))))

;;; Driver loop

(define (driver-loop)
  (newline)
  (princ "MC-EVAL==> ")
  (user-print (eval (read) the-global-environment))
  (driver-loop))

(define (user-print object)
  (cond ((compound-procedure? object)
         (print (list 'compound-procedure
                      (parameters object)
                      (procedure-body object)
                      '[procedure-env])))
        (else (print object))))

;;; Section 4.2.1 -- Normal-order evaluation

(define (try a b)
  (cond ((= a 0) 1)
        (else b)))

;;; IF as a procedure with delayed arguments

(define (if pred (delayed consequent) (delayed alternative))
  (cond (pred consequent)
        (else alternative)))

;;; Exercise 4.9

(define (unless pred (delayed default-action) (delayed exception))
  (if (not pred)
      default-action
      exception))

(define (factorial n)
  (unless (= n 1)
          (* (factorial (- n 1)) n)
          1))

;;; Exercise 4.10

(define (foo (delayed x))
  (cond (x 0)
        (else 1)))

;;; Section 4.2.2  -- Binding disciplines

;;; SUM from section 1.3.1

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (expt x 3))

(define (sum-cubes a b)
  (sum cube a 1+ b))

(define (sum-powers a b n)
  (define (nth-power x)
    (expt x n))
  (sum nth-power a 1+ b))

;;; Dynamic binding

(define (sum-powers a b n)
  (sum nth-power a 1+ b))

(define (product-powers a b n)
  (product nth-power a 1+ b))

(define (nth-power x)
    (expt x n))

;;; Versions of EVAL and APPLY that implement dynamic binding

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((quoted? exp) (text-of-quotation exp))
        ((variable? exp) (lookup-variable-value exp env))
        ((definition? exp) (eval-definition exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((lambda? exp) (make-procedure exp env))
        ((conditional? exp) (eval-cond (clauses exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)
                env))                    ;***
        (else (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments env)  ;***
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment
                         (parameters procedure)
                         arguments
                         env)))          ;***
        (else
         (error "Unknown procedure type -- APPLY" 
                procedure))))

;;; Exercise 4.17

(define (make-adder increment)
  (lambda (x) (+ x increment)))

;;; Exercise 4.19

(define (with-new-radix new-radix proc)
  (let ((old-radix radix))
    (set! radix new-radix)
    (let ((value (proc)))
      (set! radix old-radix)
      value)))

(define (with-new-radix new-radix proc)
  (fluid-let ((radix new-radix))
    (proc)))

;;; Section 4.3.2 -- Packages

;;; Manifest-type support, from section 2.3

(define (attach-type type contents)
  (cons type contents))

(define (type datum) (car datum))

(define (contents datum) (cdr datum))

;;; Generic operators

(define (make-generic-operator-1 operator)
  (lambda (arg)
    ((eval operator (type arg)) (contents arg))))

(define real-part (make-generic-operator-1 'real-part))
(define imag-part (make-generic-operator-1 'imag-part))
(define magnitude (make-generic-operator-1 'magnitude))
(define angle (make-generic-operator-1 'angle))

(define (make-generic-operator-2 operator)
  (lambda (arg1 arg2)
    (let ((t1 (type arg1)))
      (if (eq? t1 (type arg2))
          ((eval operator t1) (contents arg1) 
                              (contents arg2))
          (error "Operands not of same type"
                 (list operator arg1 arg2))))))

(define add (make-generic-operator-2 'add))
(define sub (make-generic-operator-2 'sub))
(define mul (make-generic-operator-2 'mul))
(define div (make-generic-operator-2 'div))

;;; Two versions of generic constructor

(define (make type object-parts)
  (attach-type type (apply (eval 'maker type) object-parts)))

(define (make type . object-parts)
  (attach-type type (apply (eval 'maker type) object-parts)))


(define (restrict-1 operator type-pack)
  (let ((proc (eval operator type-pack)))
    (lambda (arg)
      (if (eq? type-pack (type arg))
          (proc (contents arg))
          (error "Type mismatch -- restricted operator" 
                 (list operator type-pack arg))))))

;;; generic SQUARE operator

(define (square x) (mul x x))      

;;; definition of REAL goes here

(define complex
  ;; First we declare the imported procedures.
  (let ((+ (restrict-2 'add real))
        (- (restrict-2 'sub real))
        (* (restrict-2 'mul real))
        (/ (restrict-2 'div real)))

    ;; Next, we define the subpackages.
    (define rectangular          
      (let ((sqrt (restrict-1 'sqrt real))
            (atan (restrict-2 'atan real)))
        (make-environment
         (define (real-part z) (car z))
         (define (imag-part z) (cdr z))
         (define (magnitude z)
           (sqrt (+ (square (car z)) (square (cdr z)))))
         (define (angle z)
           (atan (cdr z) (car z)))
         (define (maker x y) (cons x y))
         )))

    ;; definition of POLAR goes here

    ;; Next we define the body of the COMPLEX manipulations.
    (define (add z1 z2)                      
      (make rectangular
            (+ (real-part z1) (real-part z2))
            (+ (imag-part z1) (imag-part z2))))
    (define (sub z1 z2)
      (make rectangular
            (- (real-part z1) (real-part z2))
            (- (imag-part z1) (imag-part z2))))
    (define (mul z1 z2)
      (make polar
            (* (magnitude z1) (magnitude z2))
            (+ (angle z1) (angle z2))))
    (define (div z1 z2)
      (make polar
            (/ (magnitude z1) (magnitude z2))
            (- (angle z1) (angle z2))))

    ;; Finally, we define the exports from the COMPLEX package.
    (let ((+ add) (- sub) (* mul) (/ div))
      (make-environment
       (define (add z1 z2)
         (attach-type complex (+ z1 z2)))
       (define (sub z1 z2)
         (attach-type complex (- z1 z2)))
       (define (mul z1 z2)
         (attach-type complex (* z1 z2)))
       (define (div z1 z2)
         (attach-type complex (/ z1 z2)))

       ;; We choose (somewhat arbitrarily) to make complex numbers
       ;; initially in rectangular form.
       (define (maker real imag)
         (make rectangular real imag))
       ;; End of COMPLEX package
       ))))

;;; Section 4.4.1

;;; The Itsey Bitsey Machine Corporation's personnel data base

(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 40000)

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 35000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 32000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 15000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 20000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 100000)

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 69000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 12000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Forrest Rosemary) (Slumerville (Onion Square) 5))
(job (Forrest Rosemary) (administration secretary))
(salary (Forrest Rosemary) 15000)
(supervisor (Forrest Rosemary) (Warbucks Oliver))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))

(can-do-job (computer programmer)
            (computer programmer trainee))

(can-do-job (administration secretary)
            (administration big wheel))

;;; end of data-base assertions

;;; Some rules

;;; This version of lives-near is from the text
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (lisp-value equal? ?person-1 ?person-2))))

;;; This improved version of lives-near is not in the text
;;; (see instructor's manual)
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

;;; This is not in the text (see instructor's manual)
(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

;;; Logic as programs

(rule (append-to-form () ?y ?y))

(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

;;; Exercise 4.33

(son Adam Cain)
(son Cain Enoch)
(son Enoch Irad)
(son Irad Mehujael)
(son Mehujael Methushael)
(son Methushael Lamech)
(wife Lamech Ada)
(son Ada Jabal)
(son Ada Jubal)

;;; Section 4.5 -- Query system implementation

;;; Section 4.5.1 -- Driver loop

(define (query-driver-loop)
  (newline)
  (princ "query==> ")
  (let ((q (query-syntax-process (read))))
    (if (assertion-to-be-added? q)
        (sequence (add-rule-or-assertion!
                   (add-assertion-body q))
                  (print "assertion added to data base")
                  (query-driver-loop))
        (sequence
         (print-stream-elements-on-separate-lines
          (map (lambda (frame)
                 (instantiate q
                              frame
                              (lambda (v f) 
                                (contract-question-mark v))))
               (qeval q (singleton '()))))
         (query-driver-loop)))))

;;; Section 4.5.1 -- Instantiation

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((constant? exp) exp)
          ((var? exp)
           (let ((vcell (binding-in-frame exp frame)))
             (if (null? vcell)             
                 (unbound-var-handler exp frame)
                 (copy (binding-value vcell)))))
          (else (cons (copy (car exp))
                      (copy (cdr exp))))))
  (copy exp))

;;; Section 4.5.2 -- query evaluator

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if (not (null? qproc))
        (qproc (contents query) frame-stream)
        (asserted? query frame-stream))))

;;; Section 4.5.2 -- simple queries

;;; The first version is from early printings of the text
(define (asserted? query-pattern frame-stream)
  (append-streams 
   (flatmap (lambda (frame)
              (find-assertions query-pattern frame))
            frame-stream)
   (flatmap (lambda (frame)
              (apply-rules query-pattern frame))
            frame-stream)))

;;; The following improved version is from later printings of the text
;;; (see also the instructor's manual)
(define (asserted? query-pattern frame-stream)
  (flatmap
   (lambda (frame)
     (append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;;; For use by the new version of Asserted?
(define (append-delayed s1 delayed-s2)
  (if (empty-stream? s1)
      (force delayed-s2)
      (cons-stream (head s1)
                   (append-delayed (tail s1) delayed-s2))))

;;; Section 4.5.2 -- compound queries

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(put 'and 'qeval conjoin)

;;; The first version is from early printings of the text
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave (qeval (first-disjunct disjuncts)
                         frame-stream)
                  (disjoin (rest-disjuncts disjuncts)
                           frame-stream))))

;;; The following improved version is from later printings of the text
;;; (see also the instructor's manual)
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(put 'or 'qeval disjoin)

;;; Section 4.5.2 -- Filters

(define (negate a frame-stream)
  (flatmap
   (lambda (frame)
     (if (empty-stream? (qeval (negated-query a)
                               (singleton frame)))
         (singleton frame)
         the-empty-stream))
   frame-stream))

(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (flatmap
   (lambda (frame)
     (if (execute
          (instantiate call
                       frame
                       (lambda (v f)
                         (error "Unknown pat var--LISP-VALUE"
                                v))))
         (singleton frame)
         the-empty-stream))
   frame-stream))

(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream)
  frame-stream)                                        

(put 'always-true 'qeval always-true)

;;;Section 4.5.3 -- Assertions and pattern matching

(define (find-assertions pattern frame)
  (flatmap (lambda (datum)
             (pattern-match pattern datum frame))
           (fetch-assertions pattern frame)))

(define (pattern-match pat dat frame)
  (let ((result (internal-match pat dat frame)))
    (if (eq? result 'failed)
        the-empty-stream
        (singleton result))))

(define (internal-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((constant? pat)
         (if (constant? dat)
             (if (same-constant? pat dat) frame 'failed)
             'failed))
        ((constant? dat) 'failed)
        (else (internal-match (cdr pat)
                              (cdr dat)
                              (internal-match (car pat)
                                              (car dat)
                                              frame)))))

(define (extend-if-consistent var dat frame)
  (let ((value (binding-in-frame var frame)))
    (if (null? value)
        (extend var dat frame)
        (internal-match (binding-value value) dat frame))))

;;; Section 4.5.4 -- Rules and unification

(define (apply-rules pattern frame)
  (flatmap (lambda (rule)
             (apply-a-rule rule pattern frame))
           (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (empty-stream? unify-result)
          the-empty-stream
          (qeval (rule-body clean-rule) unify-result)))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((constant? exp) exp)
            ((var? exp)
             (make-new-variable exp rule-application-id))
            (else (cons (tree-walk (car exp))
                        (tree-walk (cdr exp))))))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (let ((result (internal-unify p1 p2 frame)))
    (if (eq? result 'failed)
        the-empty-stream
        (singleton result))))

(define (internal-unify p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))   ;***
        ((constant? p1)
         (if (constant? p2)
             (if (same-constant? p1 p2) frame 'failed)
             'failed))
        ((constant? p2) 'failed)
        (else (internal-unify (cdr p1)
                              (cdr p2)
                              (internal-unify (car p1)
                                              (car p2)
                                              frame)))))

(define (extend-if-possible var val frame)
  (if (equal? var val)                         ;***
      frame
      (let ((value-cell (binding-in-frame var frame)))
        (if (null? value-cell)
            (if (freefor? var val frame)       ;***
                (extend var val frame)
                'failed)
            (internal-unify (binding-value value-cell)
                            val
                            frame)))))

(define (freefor? var exp frame)
  (define (freewalk e)
    (cond ((constant? e) t)
          ((var? e)
           (if (equal? var e)
               nil
               (freewalk (lookup-in-frame e frame))))
          ((freewalk (car e)) (freewalk (cdr e)))
          (else nil)))
  (freewalk exp))

;;; Section 4.5.5 -- The query data base

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if (null? s) the-empty-stream s)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (append-streams
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

;;; Adding rules and assertions to the data base

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

;;; The data-base index

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

;;; Exercise 4.40 -- a bad version of add-assertion!

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (set! THE-ASSERTIONS
        (cons-stream assertion THE-ASSERTIONS))
  'ok)

;;; Section 4.5.6

;;; The syntax of queries

(define (type exp)
  (if (atom? exp) 
      (error "Unknown expression TYPE" exp)
      (if (symbol? (car exp)) (car exp) nil)))

(define (contents exp)
  (if (atom? exp) 
      (error "Unknown expression CONTENTS" exp)
      (cdr exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp) 
  (car (contents exp)))

(define empty-conjunction? null?)
(define first-conjunct car)
(define rest-conjuncts cdr)

(define empty-disjunction? null?)
(define first-disjunct car)
(define rest-disjuncts cdr)

(define negated-query car)

(define predicate car)
(define args cdr)

(define (rule? statement)
  (if (atom? statement)
      nil
      (eq? (car statement) 'rule)))

(define conclusion cadr)

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

;;; Internal representation of variables

(define (query-syntax-process exp)
  (map-over-atoms expand-question-mark exp))

(define (map-over-atoms proc exp)
  (if (atom? exp)
      (proc exp)
      (cons (map-over-atoms proc (car exp))
            (map-over-atoms proc (cdr exp)))))

(define (expand-question-mark atom)
  (if (symbol? atom)
      (let ((characters (explode atom)))
        (if (eq? (car characters) '?)
            (list '? (implode (cdr characters)))
            atom))
      atom))

(define (var? exp)
  (if (atom? exp)
      nil
      (eq? (car exp) '?)))

(define constant? atom?)
(define constant-symbol? symbol?)
(define same-constant? equal?)


(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (1+ rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

(define (contract-question-mark variable)
  (implode (cons '? (explode (if (number? (cadr variable))
                                 (caddr variable)
                                 (cadr variable))))))

;;; The following (not in the text) retains the version number of the variable.
;;; It assumes that Explode can work on integers.
(define (contract-question-mark variable)
  (if (number? (cadr variable))  ;rule application id
      (implode (append '(?)
                       (explode (caddr variable))
                       '(-)
                       (explode (cadr variable))))
      (implode (append '(?) (explode (cadr variable))))))

;;; Frames

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

(define (lookup-in-frame variable frame)
  (binding-value (binding-in-frame variable frame)))

;;; Printer

(define (print-stream-elements-on-separate-lines s)
  (if (empty-stream? s)
      (print "done")
      (sequence (print (head s))
                (print-stream-elements-on-separate-lines
                 (tail s)))))

