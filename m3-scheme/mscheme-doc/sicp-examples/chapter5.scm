;;; Section 5.1 -- Programs we implement as register machines

;;; GCD from section 1.2.5

(define (gcd a b)
  (if (= b 0) 
      a
      (gcd b (remainder a b))))

;;; Exercise 5.1 -- FACTORIAL from section 1.2.1 (block-structured)

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;;; Section 5.1.1

;;; GCD machine definition

(define-machine gcd
  (registers a b t)
  (controller
   test-b
     (branch (zero? (fetch b)) gcd-done)
     (assign t (remainder (fetch a) (fetch b)))
     (assign a (fetch b))
     (assign b (fetch t))
     (goto test-b)
   gcd-done))

;;; GCD machine with I/O (Figure 5.4)

(define-machine gcd
  (registers a b t)
  (controller
   gcd-loop
     (assign a (read))
     (assign b (read))
   test-b
     (branch (zero? (fetch b)) gcd-done)
     (assign t (remainder (fetch a) (fetch b)))
     (assign a (fetch b))
     (assign b (fetch t))
     (goto test-b)
   gcd-done
     (perform (print (fetch a)))
     (goto gcd-loop)))

;;; Section 5.1.2

(define (remainder n d)
  (if (< n d)
      n
      (remainder (- n d) d)))

;;; GCD machine with nonprimitive REMAINDER computation

(define-machine gcd
  (registers a b t)
  (controller
   test-b
     (branch (zero? (fetch b)) gcd-done)
     (assign t (fetch a))
   rem-loop
     (branch (< (fetch t) (fetch b)) rem-done)
     (assign t (- (fetch t) (fetch b)))
     (goto rem-loop)
   rem-done
     (assign a (fetch b))
     (assign b (fetch t))
     (goto test-b)
   gcd-done))

;;; Section 5.1.4 -- Recursion

;;; FACTORIAL same as in section 1.2.1 but with args reversed

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;;; FACTORIAL machine (Figure 5.10)

(define-machine factorial
  (registers n val continue)
  (controller
     (assign continue fact-done)     ;set up final return address
   fact-loop
     (branch (=1? (fetch n)) base-case)
     (save continue)
     (save n)
     (assign n (-1+ (fetch n)))
     (assign continue after-fact)
     (goto fact-loop)
   after-fact
     (restore n)
     (restore continue)
     (assign val
             (* (fetch n) (fetch val)))
     (goto (fetch continue))
   base-case
     (assign val (fetch n))
     (goto (fetch continue))
   fact-done))

;;; FIB as in 1.2.2 but with base case written differently

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;;; FIB machine (Figure 5.11)

(define-machine fib
  (registers n val continue)
  (controller
     (assign continue fib-done)
   fib-loop
     (branch (< (fetch n) 2) immediate-answer)
     (save continue)
     (assign continue afterfib-n-1)
     (save n)
     (assign n (- (fetch n) 1))
     (goto fib-loop)
   afterfib-n-1
     (restore n)
     (restore continue)
     (assign n (- (fetch n) 2))
     (save continue)
     (assign continue afterfib-n-2)
     (save val)
     (goto fib-loop)
   afterfib-n-2
     (assign n (fetch val))
     (restore val)
     (restore continue)
     (assign val
             (+ (fetch val)(fetch n)))
     (goto (fetch continue))
   immediate-answer
     (assign val (fetch n))
     (goto (fetch continue))
   fib-done))

;;; Exercise 5.4

;;; Recursive EXPT from section 1.2.4

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;;; Iterative EXPT from section 1.2.4 but block-structured

(define (expt b n)
  (define (exp-iter counter product)
    (if (= counter 0)
        product
        (exp-iter (- counter 1) (* b product))))
  (exp-iter n 1))

;;; COUNTATOMS from section 2.2.2

(define (countatoms tree)
  (cond ((null? tree) 0)
        ((atom? tree) 1)
        (else (+ (countatoms (car tree))
                 (countatoms (cdr tree))))))

;;; A version of COUNTATOMS not shown earlier

(define (countatoms tree)
  (define (count-iter tree n)
    (cond ((null? tree) n)
          ((atom? tree) (1+ n))
          (else (count-iter (cdr tree)
                            (count-iter (car tree) n)))))
  (count-iter tree 0))

;;; Section 5.1.5 -- Register-machine simulator

;;; Expansion of first GCD machine definition above

(define gcd
  (build-model '(a b t)
               '(test-b
                 (branch (zero? (fetch b)) gcd-done)
                 (assign t (remainder (fetch a) (fetch b)))
                 (assign a (fetch b))
                 (assign b (fetch t))
                 (goto test-b)
                 gcd-done)))

;;; Start of Register-machine simulator

;;; Constructing the machine model

(define (build-model registers controller)
  (let ((machine (make-new-machine)))
    (set-up-registers machine registers)
    (set-up-controller machine controller)
    machine))

(define (set-up-registers machine registers)
  (mapc (lambda (register-name)
          (make-machine-register machine register-name))
        registers))

(define (mapc proc l)
  (if (null? l)
      'done
      (sequence (proc (car l))
                (mapc proc (cdr l)))))

(define (set-up-controller machine controller)
  (build-instruction-list machine
                          (cons '*start* controller)))

(define (build-instruction-list machine op-list)
  (if (null? op-list)
      '()
      (let ((rest-of-instructions
             (build-instruction-list machine (cdr op-list))))
        (if (label? (car op-list))
            (sequence
             (declare-label machine
                            (car op-list)
                            rest-of-instructions)
             rest-of-instructions)
            (cons (make-machine-instruction machine
                                            (car op-list))
                  rest-of-instructions)))))

(define (label? expression)
  (symbol? expression))

;;; Registers

(define (make-machine-register machine name)
  (remote-define machine name (make-register name)))

(define (make-register name)
  (define contents nil)
  (define (get) contents)
  (define (set value)
    (set! contents value))
  (define (dispatch message)
    (cond ((eq? message 'get) (get))
          ((eq? message 'set) set)
          (else (error "Unknown request -- REGISTER"
                       name
                       message))))
  dispatch)

(define (get-contents register)
  (register 'get))

(define (set-contents register value)
  ((register 'set) value))

;;; Labels

(define (declare-label machine label labeled-entry)
  (let ((defined-labels (remote-get machine '*labels*)))
    (if (memq label defined-labels)
        (error "Multiply-defined label" label)
        (sequence
         (remote-define machine label labeled-entry)
         (remote-set machine
                     '*labels*
                     (cons label defined-labels))))))

;;; The Stack

(define (make-stack)
  (define s '())
  (define (push x)
    (set! s (cons x s)))
  (define (pop)
    (if (null? s)
        (error "Empty stack -- POP")
        (let ((top (car s)))
          (set! s (cdr s))
          top)))
  (define (initialize)
    (set! s '()))
  (define (dispatch message)
    (cond ((eq? message 'push) push)
          ((eq? message 'pop) (pop))
          ((eq? message 'initialize) (initialize))
          (else (error "Unknown request -- STACK" message))))
  dispatch)

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;;; Representation of machines as environments

(define (remote-get machine variable)
  (eval variable machine))

(define (remote-set machine variable value)
  (eval (list 'set! variable (list 'quote value))
        machine))

(define (remote-define machine variable value)
  (eval (list 'define variable (list 'quote value))
        machine))

;;; Instructions as procedures

(define (make-machine-instruction machine exp)
  (eval (list 'lambda '() exp) machine))

;;; Creating an initial machine (environment)

(define (make-new-machine)
  (make-environment

(define *labels* '())

(define *the-stack* (make-stack))

(define (initialize-stack)
  (*the-stack* 'initialize))

(define fetch get-contents)

(define *program-counter* '())

(define (execute sequence)
  (set! *program-counter* sequence)
  (if (null? *program-counter*)
      'done
      ((car *program-counter*))))

(define (normal-next-instruction)
  (execute (cdr *program-counter*)))

(define (assign register value)
  (set-contents register value)
  (normal-next-instruction))

(define (save reg)
  (push *the-stack* (get-contents reg))
  (normal-next-instruction))

(define (restore reg)
  (set-contents reg (pop *the-stack*))
  (normal-next-instruction))

(define (goto new-sequence)
  (execute new-sequence))

(define (branch predicate alternate-next)
  (if predicate
      (goto alternate-next)
      (normal-next-instruction)))

(define (perform operation)
  (normal-next-instruction))

)) ;;end of MAKE-NEW-MACHINE

;;; External interface to a simulated machine

(define (remote-fetch machine register-name)
  (get-contents (remote-get machine register-name)))

(define (remote-assign machine register-name value)
  (set-contents (remote-get machine register-name) value)
  'done)

(define (start machine)
  (eval '(goto *start*) machine))

;;; The monitored stack
;;; (MAKE-STACK and INITIALIZE-STACK can be substituted for the versions above)

(define (make-stack)
  (define s '())
  (define number-pushes 0)
  (define max-depth 0)
  (define (push x)
    (set! s (cons x s))
    (set! number-pushes (1+ number-pushes))
    (set! max-depth (max (length s) max-depth)))
  (define (pop)
    (if (null? s)
        (error "Empty stack -- POP")
        (let ((top (car s)))
          (set! s (cdr s))
          top)))
  (define (initialize)
    (set! s '())
    (set! number-pushes 0)
    (set! max-depth 0))
  (define (print-statistics)
    (print (list 'total-pushes: number-pushes
                 'maximum-depth: max-depth)))
  (define (dispatch message)
    (cond ((eq? message 'push) push)
          ((eq? message 'pop) (pop))
          ((eq? message 'initialize) (initialize))
          ((eq? message 'print-statistics) 
           (print-statistics))
          (else (error "Unknown request -- STACK" message))))
  dispatch)

(define (initialize-stack)
  (*the-stack* 'print-statistics)
  (*the-stack* 'initialize))

;;; Section 5.2

eval-dispatch
  (branch (self-evaluating? (fetch exp)) ev-self-eval)
  (branch (quoted? (fetch exp)) ev-quote)
  (branch (variable? (fetch exp)) ev-variable)
  (branch (definition? (fetch exp)) ev-definition)
  (branch (assignment? (fetch exp)) ev-assignment)
  (branch (lambda? (fetch exp)) ev-lambda)
  (branch (conditional? (fetch exp)) ev-cond)
  (branch (no-args? (fetch exp)) ev-no-args)
  (branch (application? (fetch exp)) ev-application)
  (goto unknown-expression-type-error)

(define (no-args? exp)
  (if (atom? exp)
      nil
      (null? (cdr exp))))

(define (application? exp)
  (if (atom? exp)
      nil
      (not (null? (cdr exp)))))


ev-self-eval
  (assign val (fetch exp))
  (goto (fetch continue))
ev-quote
  (assign val (text-of-quotation (fetch exp)))
  (goto (fetch continue))
ev-variable
  (assign val
          (lookup-variable-value (fetch exp) (fetch env)))
  (goto (fetch continue))
ev-lambda
  (assign val (make-procedure (fetch exp) (fetch env)))
  (goto (fetch continue))

ev-no-args
  (assign exp (operator (fetch exp)))
  (save continue)
  (assign continue setup-no-arg-apply)
  (goto eval-dispatch)

setup-no-arg-apply
  (assign fun (fetch val))
  (assign argl '())
  (goto apply-dispatch)

ev-application
  (assign unev (operands (fetch exp)))
  (assign exp (operator (fetch exp)))
  (save continue)
  (save env)
  (save unev)
  (assign continue eval-args)
  (goto eval-dispatch)

eval-args
  (restore unev)
  (restore env)
  (assign fun (fetch val))
  (save fun)
  (assign argl '())
  (goto eval-arg-loop)

(define (last-operand? args)
  (null? (cdr args)))

eval-arg-loop
  (save argl)
  (assign exp (first-operand (fetch unev)))
  (branch (last-operand? (fetch unev)) eval-last-arg)
  (save env)
  (save unev)
  (assign continue accumulate-arg)
  (goto eval-dispatch)

accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (cons (fetch val) (fetch argl)))
  (assign unev (rest-operands (fetch unev)))
  (goto eval-arg-loop)

eval-last-arg
  (assign continue accumulate-last-arg)
  (goto eval-dispatch)
accumulate-last-arg
  (restore argl)
  (assign argl (cons (fetch val) (fetch argl)))
  (restore fun)
  (goto apply-dispatch)

apply-dispatch
  (branch (primitive-procedure? (fetch fun)) primitive-apply)
  (branch (compound-procedure? (fetch fun)) compound-apply)
  (goto unknown-procedure-type-error)

(define (apply-primitive-procedure p args)
  (apply (eval (primitive-id p) user-initial-environment)
         (reverse args)))

primitive-apply
  (assign val
          (apply-primitive-procedure (fetch fun)
                                     (fetch argl)))
  (restore continue)
  (goto (fetch continue))

compound-apply
  (assign env (make-bindings (fetch fun) (fetch argl)))
  (assign unev (procedure-body (fetch fun)))
  (goto eval-sequence)

(define (make-bindings proc args)
  (extend-binding-environment (parameters proc)
                              args
                              (procedure-environment proc)))

(define (extend-binding-environment vars args env)
  (extend-environment vars (reverse args) env))

(define no-more-exps? null?)

eval-sequence
  (assign exp (first-exp (fetch unev)))
  (branch (last-exp? (fetch unev)) last-exp)
  (save unev)
  (save env)
  (assign continue eval-sequence-continue)
  (goto eval-dispatch)
eval-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (rest-exps (fetch unev)))
  (goto eval-sequence)
last-exp
  (restore continue)
  (goto eval-dispatch)

;;; Non-tail-recursive version of EVAL-SEQUENCE
eval-sequence
  (branch (no-more-exps? (fetch unev)) end-sequence)  ;***
  (assign exp (first-exp (fetch unev)))               ;***
  (save unev)
  (save env)
  (assign continue eval-sequence-continue)
  (goto eval-dispatch)
eval-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (rest-exps (fetch unev)))
  (goto eval-sequence)
end-sequence
  (restore continue)
  (goto (fetch continue))                             ;***

(define (count n)
  (print n)
  (count (1+ n)))

ev-cond
  (save continue)
  (assign continue evcond-decide)
  (assign unev (clauses (fetch exp)))
evcond-pred
  (branch (no-clauses? (fetch unev)) evcond-return-nil)
  (assign exp (first-clause (fetch unev)))
  (branch (else-clause? (fetch exp)) evcond-else-clause)
  (save env)
  (save unev)
  (assign exp (predicate (fetch exp)))
  (goto eval-dispatch)
evcond-return-nil
  (restore continue)
  (assign val nil)
  (goto (fetch continue))

evcond-decide
  (restore unev)
  (restore env)
  (branch (true? (fetch val)) evcond-true-predicate)
  (assign unev (rest-clauses (fetch unev)))
  (goto evcond-pred)

evcond-true-predicate
  (assign exp (first-clause (fetch unev)))
evcond-else-clause
  (assign unev (actions (fetch exp)))
  (goto eval-sequence)

ev-assignment
  (assign unev (assignment-variable (fetch exp)))
  (save unev)
  (assign exp (assignment-value (fetch exp)))
  (save env)
  (save continue)
  (assign continue ev-assignment-1)
  (goto eval-dispatch)
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (set-variable-value! (fetch unev) 
                                (fetch val) 
                                (fetch env)))
  (goto (fetch continue))

ev-definition
  (assign unev (definition-variable (fetch exp)))
  (save unev)
  (assign exp (definition-value (fetch exp)))
  (save env)
  (save continue)
  (assign continue ev-definition-1)
  (goto eval-dispatch)
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (define-variable! (fetch unev) (fetch val) (fetch env)))
  (assign val (fetch unev))     ;return as value
                                ;the symbol being defined
  (goto (fetch continue))

;;; Controller starts here
read-eval-print-loop
  (perform (initialize-stack))
  (perform (newline))
  (perform (princ "EC-EVAL==> "))
  (assign exp (read))
  (assign env the-global-environment)
  (assign continue print-result)
  (goto eval-dispatch)
print-result
  (perform (user-print (fetch val)))
  (goto read-eval-print-loop)

unknown-procedure-type-error
  (assign val 'unknown-procedure-type-error)
  (goto signal-error)

unknown-expression-type-error
  (assign val 'unknown-expression-type-error)
  (goto signal-error)
signal-error
  (perform (user-print (fetch val)))
  (goto read-eval-print-loop)

(define the-global-environment (setup-environment))

(define-machine explicit-control-evaluator
  (registers exp env val continue fun argl unev)
  (controller
    ;;body of the controller as given in this section
   ))

(start explicit-control-evaluator)

;;; Exercise 5.20

(define (factorial n)
  (define (iter product counter)
    (cond ((> counter n) product)
          (else (iter (* counter product)
                      (+ counter 1)))))
  (iter 1 1))

;;; Exercise 5.21
(define (factorial n)
  (cond ((= n 1) 1)
        (else (* (factorial (- n 1)) n))))

;;; Exercise 5.22
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

;;; Section 5.2.5 -- Lexical addressing

;;; from exercise 5.27
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))

;;; Section 5.3 -- Compilation

;;; Section 5.3.1

(define (compile-expression exp c-t-env target cont)
  (cond ((self-evaluating? exp)
         (compile-constant exp c-t-env target cont))
        ((quoted? exp)
         (compile-constant (text-of-quotation exp)
                           c-t-env target cont))
        ((variable? exp)
         (compile-variable-access exp c-t-env target cont))
        ((assignment? exp)
         (compile-assignment exp c-t-env target cont))
        ((definition? exp)
         (compile-definition exp c-t-env target cont))
        ((lambda? exp)
         (compile-lambda exp c-t-env target cont))
        ((conditional? exp)
         (compile-cond (clauses exp) c-t-env target cont))
        ((no-args? exp)
         (compile-no-args exp c-t-env target cont))
        ((application? exp)
         (compile-application exp c-t-env target cont))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (preserving reg seq1 seq2)
  (if (and (needs-register seq2 reg)
           (modifies-register seq1 reg))
      (append-instruction-sequences
       (wrap-save-restore seq1 reg)
       seq2)
      (append-instruction-sequences seq1 seq2)))

;;; Section 5.3.2

(define (compile-continuation continuation)
  (cond ((eq? continuation 'return) (compile-return))
        ((eq? continuation 'next)
         (empty-instruction-sequence))
        (else (make-jump continuation))))

;;; Simple expressions

(define (compile-constant constant c-t-env target cont)
  (append-instruction-sequences
   (make-register-assignment target (make-constant constant))
   (compile-continuation cont)))

(define (compile-variable-access var c-t-env target cont)
  (append-instruction-sequences
   (make-register-assignment target
                             (make-variable-access var
                                                   c-t-env))
   (compile-continuation cont)))

;;; Procedure applications

(define (compile-application app c-t-env target cont)
  (preserving
   'env
   (compile-expression (operator app) c-t-env 'fun 'next)
   (preserving 'fun
               (compile-operands (operands app) c-t-env)
               (compile-call target cont))))

(define (compile-operands rands c-t-env)
  (let ((first-operand-code
         (compile-first-operand rands c-t-env)))
    (if (last-operand? rands)
        first-operand-code
        (preserving
         'env
         first-operand-code
         (compile-rest-operands (rest-operands rands)
                                c-t-env)))))

(define (compile-first-operand rands c-t-env)
  (append-instruction-sequences
   (compile-expression (first-operand rands)
                       c-t-env 'val 'next)
   (make-register-assignment
    'argl
    (make-singleton-arglist (make-fetch 'val)))))

(define (compile-rest-operands rands c-t-env)
  (let ((next-operand-code
         (compile-next-operand rands c-t-env)))
    (if (last-operand? rands)
        next-operand-code
        (preserving
         'env
         next-operand-code
         (compile-rest-operands (rest-operands rands)
                                c-t-env)))))

(define (compile-next-operand rands c-t-env)
  (preserving 
   'argl
   (compile-expression (first-operand rands)
                       c-t-env 'val 'next)
   (make-register-assignment
    'argl
    (make-add-to-arglist (make-fetch 'val)
                         (make-fetch 'argl)))))

(define (compile-no-args app c-t-env target cont)
  (append-instruction-sequences
   (compile-expression (operator app) c-t-env 'fun 'next)
   (make-register-assignment 'argl (make-empty-arglist))
   (compile-call target cont)))

(define (compile-call target cont)
  (if (eq? target 'val)
      (compile-call-result-in-val cont)
      (append-instruction-sequences
       (compile-call-result-in-val 'next)
       (make-register-assignment target (make-fetch 'val))
       (compile-continuation cont))))

(define (compile-call-result-in-val cont)
  (cond ((eq? cont 'return)
         (compile-call-return-to nil))
        ((eq? cont 'next)
         (let ((after-call (make-new-label 'after-call)))
           (append-instruction-sequences
            (compile-call-return-to after-call)
            (make-entry-point-designator after-call))))
        (else
         (compile-call-return-to cont))))

(define (compile-return)
  (append-instruction-sequences
   (make-restore 'continue)
   (make-return-from-procedure)))

(define (compile-call-return-to return-entry)
  (if (null? return-entry)
      (make-transfer-to-procedure)
      (append-instruction-sequences
       (make-register-assignment 'continue return-entry)
       (make-save 'continue)
       (make-transfer-to-procedure))))

;;; Conditionals

(define (compile-cond clauses c-t-env target cont)
  (if (eq? cont 'next)
      (let ((end-of-cond (make-new-label 'cond-end)))
        (append-instruction-sequences
         (compile-clauses clauses c-t-env target end-of-cond)
         (make-entry-point-designator end-of-cond)))
      (compile-clauses clauses c-t-env target cont)))

(define (compile-clauses clauses c-t-env target cont)
  (if (no-clauses? clauses)
      (compile-constant nil c-t-env target cont)
      (compile-a-clause (first-clause clauses)
                        (rest-clauses clauses)
                        c-t-env target cont)))

(define (compile-a-clause clause rest c-t-env target cont)
  (let ((consequent (compile-sequence (actions clause)
                                      c-t-env target cont)))
    (if (else-clause? clause)
        consequent
        (let
         ((alternative (compile-clauses rest
                                        c-t-env target cont))
          (pred (compile-expression (predicate clause)
                                    c-t-env 'val 'next))
          (true-branch (make-new-label 'true-branch)))
         (let ((alternative-and-consequent
                (parallel-instruction-sequences
                 alternative
                 (append-instruction-sequences
                  (make-entry-point-designator true-branch)
                  consequent))))
           (preserving
            'env
            pred
            (append-instruction-sequences
             (make-branch (make-test 'val) true-branch)
             alternative-and-consequent)))))))

(define (compile-sequence seq c-t-env target cont)
  (if (last-exp? seq)
      (compile-expression (first-exp seq) 
                          c-t-env target cont)
      (preserving
       'env
       (compile-expression (first-exp seq) c-t-env nil 'next)
       (compile-sequence (rest-exps seq) c-t-env target cont)
       )))

;;; Assignments

(define (compile-assignment exp c-t-env target cont)
  (let ((hold-value (if (null? target) 'val target)))
    (preserving
     'env
     (compile-expression (assignment-value exp)
                         c-t-env hold-value 'next)
     (append-instruction-sequences
      (make-variable-assignment (assignment-variable exp)
                                c-t-env
                                (make-fetch hold-value))
      (compile-continuation cont)))))

;;; Definitions

(define (compile-definition exp c-t-env target cont)
  (let ((hold-value (if (null? target) 'val target))
        (var (definition-variable exp)))
    (preserving
     'env
     (compile-expression (definition-value exp)
                         c-t-env hold-value 'next)
     (append-instruction-sequences
      (make-variable-definition var
                                c-t-env
                                (make-fetch hold-value))
      (make-register-assignment target (make-constant var))
      (compile-continuation cont)))))

;;; Lambda expressions

(define (compile-lambda exp c-t-env target cont)
  (if (eq? cont 'next)
      (let ((after-lambda (make-new-label 'after-lambda)))
        (append-instruction-sequences
         (compile-lambda-2 exp c-t-env target after-lambda)
         (make-entry-point-designator after-lambda)))
      (compile-lambda-2 exp c-t-env target cont)))

(define (compile-lambda-2 exp c-t-env target cont)
  (let ((proc-entry (make-new-label 'entry)))
    (tack-on-instruction-sequence
     (append-instruction-sequences
      (make-register-assignment
       target
       (make-procedure-constructor proc-entry))
      (compile-continuation cont))
     (compile-lambda-body exp c-t-env proc-entry))))

(define (compile-lambda-body exp c-t-env proc-entry)
  (append-instruction-sequences
   (make-entry-point-designator proc-entry)
   (make-environment-switch (lambda-parameters exp))
   (compile-sequence
    (lambda-body exp)
    (extend-compile-time-env (lambda-parameters exp) c-t-env)
    'val
    'return)))

;;; New syntax procedures
(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))


;;; Section 5.3.3 -- compiler data structures

;;; Instruction sequences

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (registers-needed s) (car s))

(define (registers-modified s) (cadr s))

(define (statements s) (caddr s))

(define (needs-register seq reg)
  (element-of-set? reg (registers-needed seq)))

(define (modifies-register seq reg)
  (element-of-set? reg (registers-modified seq)))

(define (make-instruction needed modified statement)
  (make-instruction-sequence needed 
                             modified 
                             (list statement)))

(define (empty-instruction-sequence)
  (make-instruction-sequence empty-set empty-set '()))

;;; Combining instruction sequences

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (union-set (registers-needed seq1)
                (difference-set (registers-needed seq2)
                                (registers-modified seq1)))
     (union-set (registers-modified seq1)
                (registers-modified seq2))
     (append (statements seq1) (statements seq2))))

  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

;;; Combiner used by Compile-lambda

(define (tack-on-instruction-sequence seq body-seq)
  (append-instruction-sequences
   seq
   (make-instruction-sequence empty-set
                              empty-set
                              (statements body-seq))))

;;; Combiner used by Compile-cond

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (union-set (registers-needed seq1) 
              (registers-needed seq2))
   (union-set (registers-modified seq1) 
              (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

;;; Sets of registers

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (union-set (cdr s1) s2))
        (else (cons (car s1) (union-set (cdr s1) s2)))))

(define (difference-set s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (difference-set (cdr s1) s2))
        (else (cons (car s1) (difference-set (cdr s1) s2)))))

(define (element-of-set? x s) (memq x s))

(define (singleton x) (list x))

(define (make-set list-of-elements) list-of-elements)

(define empty-set '())

;;; Value specifiers

(define (make-val-spec registers-needed expression)
  (list registers-needed expression))

(define (val-spec-registers-needed value)
  (car value))

(define (val-spec-expression value)
  (cadr value))

;;; Section 5.3.4 -- Primitive code generators

;;; Generators for any register machine

(define (make-constant c)
  (make-val-spec empty-set (list 'quote c)))

(define (make-label symbol)
  (make-val-spec empty-set symbol))

(define (make-new-label name)
  (make-label (make-new-symbol name)))

(define (make-fetch reg)
  (make-val-spec (singleton reg) (list 'fetch reg)))

(define (make-operation operation . inputs)
  (make-val-spec
   (union-all-sets (mapcar val-spec-registers-needed inputs))
   (cons operation (mapcar val-spec-expression inputs))))

(define (union-all-sets sets)
  (if (null? sets)
      empty-set
      (union-set (car sets) (union-all-sets (cdr sets)))))

(define (make-register-assignment reg val-spec)
  (if (null? reg)
      (empty-instruction-sequence)
      (make-instruction
       (val-spec-registers-needed val-spec)
       (singleton reg)
       (list 'assign reg (val-spec-expression val-spec)))))

(define (make-nonlocal-goto continuation cont-needs)
  (make-goto continuation (make-set cont-needs) all))

(define all (make-set '(fun env val argl continue)))

(define (make-jump continuation)
  (make-goto continuation empty-set empty-set))

(define (make-goto cont cont-needs cont-modifies)
  (make-instruction
   (union-set (val-spec-registers-needed cont) cont-needs)
   cont-modifies
   (list 'goto (val-spec-expression cont))))

(define (make-branch predicate true-branch)
  (make-instruction
   (union-set (val-spec-registers-needed predicate)
              (val-spec-registers-needed true-branch))
   empty-set
   (list 'branch
         (val-spec-expression predicate)
         (val-spec-expression true-branch))))

(define (make-save reg)
  (make-instruction (singleton reg)
                    empty-set
                    (list 'save reg)))

(define (make-restore reg)
  (make-instruction empty-set
                    (singleton reg)
                    (list 'restore reg)))

(define (make-perform action)
  (make-instruction
   (val-spec-registers-needed action)
   empty-set
   (list 'perform (val-spec-expression action))))

(define (make-entry-point-designator label-val-spec)
  (make-instruction empty-set
                    empty-set
                    (val-spec-expression label-val-spec)))

;;; The following is used by Preserving

(define (wrap-save-restore seq reg)
  (make-instruction-sequence
   (registers-needed seq)
   (difference-set (registers-modified seq) (singleton reg))
   (append (statements (make-save reg))
           (statements seq)
           (statements (make-restore reg)))))

;;; Generators for the evaluator machine

(define (make-variable-access var c-t-env)
  (make-operation 'lookup-variable-value
                  (make-constant var)
                  (make-fetch 'env)))

(define (make-test reg)
  (make-operation 'true? (make-fetch reg)))

(define (make-variable-assignment var c-t-env value)
  (make-perform
   (make-operation 'set-variable-value!
                   (make-constant var)
                   value
                   (make-fetch 'env))))

(define (make-variable-definition var c-t-env value)
  (make-perform
   (make-operation 'define-variable!
                   (make-constant var)
                   value
                   (make-fetch 'env))))

(define (make-procedure-constructor entry)
  (make-operation 'make-compiled-procedure
                  entry
                  (make-fetch 'env)))

(define (make-environment-switch formals)
  (append-instruction-sequences
   (make-register-assignment
    'env
    (make-operation 'compiled-procedure-env
                    (make-fetch 'fun)))
   (make-register-assignment
    'env
    (make-operation 'extend-binding-environment
                    (make-constant formals)
                    (make-fetch 'argl)
                    (make-fetch 'env)))))

(define (make-singleton-arglist first-arg-spec)
  (make-operation 'cons first-arg-spec (make-constant '())))

(define (make-add-to-arglist next-arg-spec rest-args-spec)
  (make-operation 'cons next-arg-spec rest-args-spec))

(define (make-empty-arglist)
  (make-constant '()))

(define (make-transfer-to-procedure)
  (make-nonlocal-goto (make-label 'apply-dispatch)
                      '(fun argl)))

(define (make-return-from-procedure)
  (make-nonlocal-goto (make-fetch 'continue)
                      '(val)))

;;; Section 5.3.5 -- sample compilation

(compile-expression
 '(define (factorial n)
    (cond ((= n 1) 1)
          (else (* (factorial (- n 1)) n))))
 initial-c-t-env
 'val
 'next)

;;; Exercise 5.30
(define (factorial-alt n)
  (cond ((= n 1) 1)
        (else (* n (factorial-alt (- n 1))))))

;;; Exercise 5.31
(define (factorial-iter n)
  (define (iter product counter)
    (cond ((> counter n) product)
          (else (iter (* counter product) (+ counter 1)))))
  (iter 1 1))

;;; Section 5.3.6 -- Compiler/evaluator interface

apply-dispatch
  (branch (primitive-procedure? (fetch fun)) primitive-apply)
  (branch (compound-procedure? (fetch fun)) compound-apply)
  (branch (compiled-procedure? (fetch fun)) compiled-apply)
  (goto unknown-procedure-type-error)

compiled-apply
   (assign val (compiled-procedure-entry (fetch fun)))
   (goto (fetch val))

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc)
  (if (atom? proc)
      nil
      (eq? (car proc) 'compiled-procedure)))

(define (compiled-procedure-entry proc)
  (cadr proc))

(define (compiled-procedure-env proc)
  (caddr proc))


(define (compile-and-go expression)
  (remote-assign
   explicit-control-evaluator
   'val
   (build-instruction-list explicit-control-evaluator
                           (compile expression)))
  (eval '(goto external-entry)
        explicit-control-evaluator))


external-entry
   (perform (initialize-stack))
   (assign env the-global-environment)
   (assign continue print-result)
   (save continue)
   (goto (fetch val))

 (define (user-print object)}}}
   (cond ((compound-procedure? object)
          (print (list 'compound-procedure
                       (parameters object)
                       (procedure-body object)
                       '[procedure-env])))
         ((compiled-procedure? object)                  ;new clause
          (print '[compiled-procedure]))
         (else (print object))))

(define (compile expression)
  (statements (compile-expression expression
                                  initial-c-t-env
                                  'val
                                  'return)))

;;; Section 5.3.7 -- Lexical addressing

(let ((x 3) (y 4))
  (lambda (a b c d e)
    (let ((y (* a b x))
          (z (+ c d x)))
      (* x y z))))

((lambda (x y)
   (lambda (a b c d e)
     ((lambda (y z) (* x y z))
      (* a b x)
      (+ c d x))))
 3
 4)


(define (extend-compile-time-env params c-t-env)
  (cons params c-t-env))   

;;; Exercise 5.39

((lambda (n)
   ((lambda (fact-iter)
      (fact-iter fact-iter 1 1))
    (lambda (f-i product counter)
      (cond ((> counter n) product)
            (else (f-i f-i
                       (* counter product)
                       (+ counter 1)))))))
 4)

;;; Section 5.4.2 -- Stop-and-copy garbage collector

begin-garbage-collection
  (assign free 0)
  (assign scan 0)
  (assign old (fetch root))
  (assign relocate-continue reassign-root)
  (goto relocate-old-result-in-new)
reassign-root
  (assign root (fetch new))
  (goto gc-loop)

gc-loop
  (branch (= (fetch scan) (fetch free)) gc-flip)
  (assign old (vector-ref (fetch new-cars) (fetch scan)))
  (assign relocate-continue update-car)
  (goto relocate-old-result-in-new)

update-car
  (perform
   (vector-set! (fetch new-cars) (fetch scan) (fetch new)))
  (assign old (vector-ref (fetch new-cdrs) (fetch scan)))
  (assign relocate-continue update-cdr)
  (goto relocate-old-result-in-new)

update-cdr
  (perform
   (vector-set! (fetch new-cdrs) (fetch scan) (fetch new)))
  (assign scan (1+ (fetch scan)))
  (goto gc-loop)

relocate-old-result-in-new
  (branch (pointer-to-pair? (fetch old)) pair)
  (assign new (fetch old))
  (goto (fetch relocate-continue))

pair
  (assign oldcr (vector-ref (fetch the-cars) (fetch old)))
  (branch (broken-heart? (fetch oldcr)) already-moved)
  (assign new (fetch free))         ;new location for pair
  (assign free (1+ (fetch free)))   ;update free pointer

  ;;Copy the car and cdr to new memory.
  (perform
   (vector-set! (fetch new-cars) (fetch new) (fetch oldcr)))
  (assign oldcr (vector-ref (fetch the-cdrs) (fetch old)))
  (perform
   (vector-set! (fetch new-cdrs) (fetch new) (fetch oldcr)))

  ;;Construct the broken heart.
  (perform
   (vector-set! (fetch the-cars) (fetch old) broken-heart))
  (perform
   (vector-set! (fetch the-cdrs) (fetch old) (fetch new)))
  (goto (fetch relocate-continue))

already-moved
  (assign new (vector-ref (fetch the-cdrs) (fetch old)))
  (goto (fetch relocate-continue))

gc-flip
  (assign temp (fetch the-cdrs))
  (assign the-cdrs (fetch new-cdrs))
  (assign new-cdrs (fetch temp))
  (assign temp (fetch the-cars))
  (assign the-cars (fetch new-cars))
  (assign new-cars (fetch temp))
