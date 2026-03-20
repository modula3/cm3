;;; run-tests.scm --- DV test suite for SICP 1st edition examples under MScheme
;;;
;;; Tests each chapter's key programs section by section.
;;; Skips: redefinitions of +/*, infinite loops, undefined helpers,
;;;        collect macro, define-machine, put/get before ch3 tables.
;;;
;;; Usage: mscheme run-tests.scm

(load "sicp-compat.scm")

(define *tests-run* 0)
(define *tests-passed* 0)
(define *tests-failed* 0)
(define *test-failures* '())

(define (check label expected actual)
  (set! *tests-run* (+ *tests-run* 1))
  (if (equal? expected actual)
      (begin (set! *tests-passed* (+ *tests-passed* 1))
             (display "  PASS  ") (display label) (newline))
      (begin (set! *tests-failed* (+ *tests-failed* 1))
             (set! *test-failures*
                   (cons (list label expected actual) *test-failures*))
             (display "  FAIL  ") (display label)
             (display "  expected=") (display expected)
             (display "  actual=") (display actual) (newline))))

(define (section title)
  (newline)
  (display "--- ") (display title) (display " ---") (newline))

;;; ====================================================================
;;; CHAPTER 1: Building Abstractions with Procedures
;;; ====================================================================

(display "======================================") (newline)
(display "CHAPTER 1") (newline)
(display "======================================") (newline)

;;; Section 1.1.4
(section "1.1.4 Compound Procedures")
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (f a) (sum-of-squares (+ a 1) (* a 2)))
(check "f(5)" 136 (f 5))
(check "square(21)" 441 (square 21))

;;; Section 1.1.6
(section "1.1.6 Conditional Expressions")
(define (abs-val x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))
(check "abs(-5)" 5 (abs-val -5))
(check "abs(0)" 0 (abs-val 0))
(check "abs(3)" 3 (abs-val 3))

;;; Section 1.1.7
(section "1.1.7 Square Roots")
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (improve guess x) (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))
(define (good-enough? guess x) (< (abs (- (square guess) x)) .001))
(define (my-sqrt x) (sqrt-iter 1 x))
(check "sqrt(9)~3" #t (< (abs (- (my-sqrt 9) 3)) 0.001))
(check "sqrt(2)~1.414" #t (< (abs (- (my-sqrt 2) 1.41421)) 0.001))

;;; Section 1.1.8 -- block-structured sqrt with lexical scoping
(section "1.1.8 Block Structure")
(define (sqrt-block x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) .001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess) guess (sqrt-iter (improve guess))))
  (sqrt-iter 1))
(check "sqrt-block(25)~5" #t (< (abs (- (sqrt-block 25) 5)) 0.001))

;;; Section 1.2.1
(section "1.2.1 Factorial")
(define (factorial-rec n)
  (if (= n 1) 1 (* n (factorial-rec (- n 1)))))
(check "5! recursive" 120 (factorial-rec 5))
(check "10!" 3628800 (factorial-rec 10))

(define (factorial-iter n)
  (define (iter product counter)
    (if (> counter n) product
        (iter (* counter product) (+ counter 1))))
  (iter 1 1))
(check "5! iterative" 120 (factorial-iter 5))
(check "10! iterative" 3628800 (factorial-iter 10))

;;; Section 1.2.2
(section "1.2.2 Fibonacci & Counting Change")
(define (fib-rec n)
  (cond ((= n 0) 0) ((= n 1) 1)
        (else (+ (fib-rec (- n 1)) (fib-rec (- n 2))))))
(check "fib(0)" 0 (fib-rec 0))
(check "fib(10)" 55 (fib-rec 10))

(define (fib-iter n)
  (define (iter a b count)
    (if (= count 0) b (iter (+ a b) a (- count 1))))
  (iter 1 0 n))
(check "fib-iter(10)" 55 (fib-iter 10))
(check "fib-iter(20)" 6765 (fib-iter 20))

(define (count-change amount)
  (define (cc amount kinds)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds 0)) 0)
          (else (+ (cc (- amount (first-denom kinds)) kinds)
                   (cc amount (- kinds 1))))))
  (define (first-denom k)
    (cond ((= k 1) 1) ((= k 2) 5) ((= k 3) 10)
          ((= k 4) 25) ((= k 5) 50)))
  (cc amount 5))
(check "count-change(100)" 292 (count-change 100))

;;; Section 1.2.4
(section "1.2.4 Exponentiation")
(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))
(check "fast-exp(2,10)" 1024 (fast-exp 2 10))
(check "fast-exp(3,5)" 243 (fast-exp 3 5))

;;; Section 1.2.5
(section "1.2.5 GCD")
(define (my-gcd a b) (if (= b 0) a (my-gcd b (remainder a b))))
(check "gcd(28,16)" 4 (my-gcd 28 16))
(check "gcd(206,40)" 2 (my-gcd 206 40))

;;; Section 1.2.6
(section "1.2.6 Primality")
(define (smallest-divisor n)
  (define (find-divisor n test)
    (cond ((> (square test) n) n)
          ((= (remainder n test) 0) test)
          (else (find-divisor n (+ test 1)))))
  (find-divisor n 2))
(define (prime? n) (= n (smallest-divisor n)))
(check "prime?(7)" #t (prime? 7))
(check "prime?(12)" #f (prime? 12))
(check "prime?(199)" #t (prime? 199))
(check "smallest-divisor(199)" 199 (smallest-divisor 199))
(check "smallest-divisor(1999)" 1999 (smallest-divisor 1999))

;;; Section 1.3
(section "1.3 Higher-Order Procedures")
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b) 0
      (+ (term a) (sum term (next a) next b))))

(define (sum-cubes a b) (sum cube a 1+ b))
(check "sum-cubes(1,10)" 3025 (sum-cubes 1 10))

(define (sum-integers a b)
  (sum (lambda (x) x) a 1+ b))
(check "sum-integers(1,100)" 5050 (sum-integers 1 100))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1 (* x (+ x 2))))
       a (lambda (x) (+ x 4)) b))
(check "8*pi-sum~pi" #t (< (abs (- (* 8 (pi-sum 1 1000)) 3.14159)) 0.01))

;;; Section 1.3.2
(section "1.3.2 Lambda and Let")
(define (f-let x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a)) (* y b) (* a b))))
(check "f-let(3,4)" 456 (f-let 3 4))

;;; Section 1.3.3
(section "1.3.3 Half-Interval Method")
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (< (abs (- neg-point pos-point)) 0.001)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((> test-value 0) (search f neg-point midpoint))
                ((< test-value 0) (search f midpoint pos-point))
                (else midpoint))))))
(define (half-interval-method f a b)
  (let ((a-value (f a)) (b-value (f b)))
    (cond ((and (< a-value 0) (> b-value 0)) (search f a b))
          ((and (< b-value 0) (> a-value 0)) (search f b a))
          (else (error "Not opposite sign" a b)))))
(check "sin root ~pi" #t
       (< (abs (- (half-interval-method sin 2.0 4.0) 3.14159)) 0.01))

;;; Section 1.3.4
(section "1.3.4 Newton's Method")
(define (deriv-fn f dx)
  (lambda (x) (/ (- (f (+ x dx)) (f x)) dx)))
(define (newton f guess)
  (if (< (abs (f guess)) 0.001)
      guess
      (newton f (- guess (/ (f guess) ((deriv-fn f 0.001) guess))))))
(check "newton sqrt(2)" #t
       (< (abs (- (newton (lambda (x) (- (square x) 2)) 1.0) 1.41421)) 0.001))

;;; ====================================================================
;;; CHAPTER 2: Building Abstractions with Data
;;; ====================================================================

(display "======================================") (newline)
(display "CHAPTER 2") (newline)
(display "======================================") (newline)

;;; Section 2.1.1
(section "2.1.1 Rational Numbers")
(define (make-rat n d)
  (let ((g (gcd n d))) (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (+rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (denom x) (numer y)))
            (* (denom x) (denom y))))
(define (*rat x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
(let ((r (+rat (make-rat 1 3) (make-rat 1 4))))
  (check "1/3+1/4 numer" 7 (numer r))
  (check "1/3+1/4 denom" 12 (denom r)))
(let ((r (*rat (make-rat 2 3) (make-rat 3 5))))
  (check "2/3*3/5 numer" 2 (numer r))
  (check "2/3*3/5 denom" 5 (denom r)))

;;; Section 2.1.3
(section "2.1.3 Pairs")
(check "cons/car/cdr" '(1 2 3) (cons 1 (cons 2 (cons 3 '()))))
(check "list" '(1 2 3) (list 1 2 3))

;;; Section 2.2.1
(section "2.2.1 List Operations")
(define (my-length x)
  (if (null? x) 0 (+ 1 (my-length (cdr x)))))
(check "length" 4 (my-length (list 1 2 3 4)))
(define (my-append x y)
  (if (null? x) y (cons (car x) (my-append (cdr x) y))))
(check "append" '(1 2 3 4 5 6) (my-append '(1 2 3) '(4 5 6)))

;;; Section 2.2.2
(section "2.2.2 Trees")
(define (countatoms x)
  (cond ((null? x) 0)
        ((atom? x) 1)
        (else (+ (countatoms (car x)) (countatoms (cdr x))))))
(check "countatoms" 4 (countatoms '(1 (2 (3 4)))))

;;; Section 2.2.3
(section "2.2.3 Symbols")
(define (my-memq item x)
  (cond ((null? x) '())
        ((eq? item (car x)) x)
        (else (my-memq item (cdr x)))))
(check "memq found" '(b c d) (my-memq 'b '(a b c d)))
(check "memq not found" '() (my-memq 'z '(a b c d)))

;;; Section 2.2.4
(section "2.2.4 Symbolic Differentiation")
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (number? a1) (= a1 0)) a2)
        ((and (number? a2) (= a2 0)) a1)
        (else (list '+ a1 a2))))
(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (number? m1) (= m1 0)) 0)
        ((and (number? m2) (= m2 0)) 0)
        ((and (number? m1) (= m1 1)) m2)
        ((and (number? m2) (= m2 1)) m1)
        (else (list '* m1 m2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))
(define (deriv-sym exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-sym (addend exp) var)
                   (deriv-sym (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp) (deriv-sym (multiplicand exp) var))
          (make-product (deriv-sym (multiplier exp) var)
                        (multiplicand exp))))
        (else (error "unknown type -- DERIV" exp))))
(check "d/dx(x+3)" 1 (deriv-sym '(+ x 3) 'x))
(check "d/dx(x*y)" 'y (deriv-sym '(* x y) 'x))
(check "d/dx(x*x)" '(+ x x) (deriv-sym '(* x x) 'x))
(check "d/dx(*(+ x 1)(+ x -1))"
       '(+ (+ x 1) (+ x -1))
       (deriv-sym '(* (+ x 1) (+ x -1)) 'x))

;;; Section 2.2.5
(section "2.2.5 Sets")
;; Sets as unordered lists
(define (element-of-set-unord? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set-unord? x (cdr set)))))
(define (adjoin-set-unord x set)
  (if (element-of-set-unord? x set) set (cons x set)))
(define (intersection-set-unord set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set-unord? (car set1) set2)
         (cons (car set1) (intersection-set-unord (cdr set1) set2)))
        (else (intersection-set-unord (cdr set1) set2))))
(check "element-of-set?" #t (element-of-set-unord? 3 '(1 2 3 4)))
(check "element-of-set? not" #f (element-of-set-unord? 5 '(1 2 3 4)))
(check "adjoin-set" '(5 1 2 3) (adjoin-set-unord 5 '(1 2 3)))
(check "adjoin-set dup" '(1 2 3) (adjoin-set-unord 2 '(1 2 3)))
(check "intersection" '(2 3) (intersection-set-unord '(1 2 3) '(2 3 4)))

;; Sets as binary trees
(define (make-tree entry left right) (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (element-of-set-tree? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set-tree? x (left-branch set)))
        (else (element-of-set-tree? x (right-branch set)))))
(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-tree x (left-branch set))
                    (right-branch set)))
        (else
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-tree x (right-branch set))))))
(define test-tree (adjoin-set-tree 5 (adjoin-set-tree 3
                   (adjoin-set-tree 7 (adjoin-set-tree 1 '())))))
(check "tree contains 3" #t (element-of-set-tree? 3 test-tree))
(check "tree not contains 4" #f (element-of-set-tree? 4 test-tree))

;;; Section 2.2.6
(section "2.2.6 Huffman Encoding")
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (make-code-tree left right)
  (list left right
        (append (huff-symbols left) (huff-symbols right))
        (+ (huff-weight left) (huff-weight right))))
(define (huff-left-branch tree) (car tree))
(define (huff-right-branch tree) (cadr tree))
(define (huff-symbols tree)
  (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))
(define (huff-weight tree)
  (if (leaf? tree) (weight-leaf tree) (cadddr tree)))
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits) '()
        (let ((next-branch
               (if (= (car bits) 0)
                   (huff-left-branch current-branch)
                   (huff-right-branch current-branch))))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
(check "huffman decode" '(A D A B B C A)
       (decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) sample-tree))

;;; Section 2.3.1
(section "2.3.1 Complex Numbers (rectangular)")
(define (make-rectangular-c x y) (cons x y))
(define (real-part-c z) (car z))
(define (imag-part-c z) (cdr z))
(define (magnitude-c z) (sqrt (+ (square (car z)) (square (cdr z)))))
(define (+c z1 z2)
  (make-rectangular-c (+ (real-part-c z1) (real-part-c z2))
                      (+ (imag-part-c z1) (imag-part-c z2))))
(let ((z (+c (make-rectangular-c 3 4) (make-rectangular-c 1 2))))
  (check "complex +" '(4 . 6) z)
  (check "complex magnitude" #t (< (abs (- (magnitude-c z) 7.211)) 0.01)))

;;; ====================================================================
;;; CHAPTER 3: Modularity, Objects, and State
;;; ====================================================================

(display "======================================") (newline)
(display "CHAPTER 3") (newline)
(display "======================================") (newline)

;;; Section 3.1.1
(section "3.1.1 Local State")
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount)) balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount)) balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request" m))))
  dispatch)
(define acc (make-account 100))
(check "withdraw 50" 50 ((acc 'withdraw) 50))
(check "withdraw 60" "Insufficient funds" ((acc 'withdraw) 60))
(check "deposit 40" 90 ((acc 'deposit) 40))
(check "withdraw 20" 70 ((acc 'withdraw) 20))

;;; Section 3.1.2
(section "3.1.2 Mutation as Assignment")
(define (make-simplified-withdraw balance)
  (lambda (amount) (set! balance (- balance amount)) balance))
(define w (make-simplified-withdraw 25))
(check "simp-withdraw 10" 15 (w 10))
(check "simp-withdraw 10 again" 5 (w 10))

;;; Section 3.3.1
(section "3.3.1 Mutable Lists")
(define x (list 'a 'b))
(define z1 (cons x x))
(set-car! (cdr x) 'z)
(check "shared structure" 'z (cadr (car z1)))

;;; Section 3.3.2
(section "3.3.2 Queues")
(define (make-queue) (cons '() '()))
(define (front-ptr q) (car q))
(define (rear-ptr q) (cdr q))
(define (set-front-ptr! q item) (set-car! q item))
(define (set-rear-ptr! q item) (set-cdr! q item))
(define (empty-queue? q) (null? (front-ptr q)))
(define (front-queue q)
  (if (empty-queue? q) (error "FRONT empty queue" q)
      (car (front-ptr q))))
(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr! q new-pair) q)
          (else (set-cdr! (rear-ptr q) new-pair)
                (set-rear-ptr! q new-pair) q))))
(define (delete-queue! q)
  (cond ((empty-queue? q) (error "DELETE empty queue" q))
        (else (set-front-ptr! q (cdr (front-ptr q))) q)))
(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(insert-queue! q 'c)
(check "queue front" 'a (front-queue q))
(delete-queue! q)
(check "queue after delete" 'b (front-queue q))

;;; Section 3.3.3
(section "3.3.3 Tables (put/get)")
;; sicp-compat.scm overrides assq to return '() on miss (MIT Scheme behavior).
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assq key-1 (cdr local-table))))
        (if (null? subtable) #f
            (let ((record (assq key-2 (cdr subtable))))
              (if (null? record) #f (cdr record))))))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assq key-1 (cdr local-table))))
        (if (null? subtable)
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table)))
            (let ((record (assq key-2 (cdr subtable))))
              (if (null? record)
                  (set-cdr! subtable
                            (cons (cons key-2 value) (cdr subtable)))
                  (set-cdr! record value))))) 'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown op" m))))
    dispatch))
(define op-table (make-table))
(define get (op-table 'lookup-proc))
(define put (op-table 'insert-proc!))
(put 'math 'add +)
(put 'math 'mul *)
(check "table get" #t (number? ((get 'math 'add) 3 4)))
(check "table get value" 7 ((get 'math 'add) 3 4))
(check "table get miss" #f (get 'math 'sub))

;;; Section 3.3.4 -- just verify wire/circuit definitions parse
(section "3.3.4 Circuit Simulator (definitions)")
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (call-each procs)
      (if (null? procs) 'done
          (begin ((car procs)) (call-each (cdr procs)))))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation" m))))
    dispatch))
(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define w1 (make-wire))
(set-signal! w1 1)
(check "wire signal" 1 (get-signal w1))

;;; Section 3.4.1
(section "3.4.1 Streams (basic)")
;; streams already defined in sicp-compat
(define (enumerate-interval low high)
  (if (> low high) the-empty-stream
      (cons-stream low (enumerate-interval (1+ low) high))))
(define (stream-filter pred stream)
  (cond ((empty-stream? stream) the-empty-stream)
        ((pred (head stream))
         (cons-stream (head stream)
                      (stream-filter pred (tail stream))))
        (else (stream-filter pred (tail stream)))))
(define (stream-ref s n)
  (if (= n 0) (head s) (stream-ref (tail s) (- n 1))))
(define (stream->list s n)
  (if (= n 0) '() (cons (head s) (stream->list (tail s) (- n 1)))))
(check "enumerate 1-5" '(1 2 3 4 5)
       (stream->list (enumerate-interval 1 5) 5))
(check "filter even 1-10" '(2 4 6 8 10)
       (stream->list (stream-filter even? (enumerate-interval 1 10)) 5))

;;; Section 3.4.2
(section "3.4.2 Stream Operations")
(define (stream-map proc stream)
  (if (empty-stream? stream) the-empty-stream
      (cons-stream (proc (head stream))
                   (stream-map proc (tail stream)))))
(define (stream-accumulate combiner init stream)
  (if (empty-stream? stream) init
      (combiner (head stream)
                (stream-accumulate combiner init (tail stream)))))
(check "stream-map square" '(1 4 9 16 25)
       (stream->list (stream-map square (enumerate-interval 1 5)) 5))
(check "stream-accumulate +" 15
       (stream-accumulate + 0 (enumerate-interval 1 5)))

;;; Section 3.4.4
(section "3.4.4 Infinite Streams")
(define (integers-from n)
  (cons-stream n (integers-from (1+ n))))
(define integers (integers-from 1))
(check "integers 1-5" '(1 2 3 4 5) (stream->list integers 5))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7))) integers))
(check "no-sevens[0..4]" '(1 2 3 4 5) (stream->list no-sevens 5))
(check "no-sevens 100th" 116 (stream-ref no-sevens 99))

(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))
(check "fibs 0-7" '(0 1 1 2 3 5 8 13) (stream->list fibs 8))

(define (sieve stream)
  (cons-stream (head stream)
               (sieve (stream-filter
                       (lambda (x) (not (divisible? x (head stream))))
                       (tail stream)))))
(define primes (sieve (integers-from 2)))
(check "primes 0-9" '(2 3 5 7 11 13 17 19 23 29)
       (stream->list primes 10))

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else (cons-stream (+ (head s1) (head s2))
                           (add-streams (tail s1) (tail s2))))))
(define integers2 (cons-stream 1 (add-streams ones integers2)))
(check "integers2 1-5" '(1 2 3 4 5) (stream->list integers2 5))

(define fibs2
  (cons-stream 0 (cons-stream 1 (add-streams (tail fibs2) fibs2))))
(check "fibs2 0-7" '(0 1 1 2 3 5 8 13) (stream->list fibs2 8))

(define (scale-stream c stream)
  (stream-map (lambda (x) (* x c)) stream))
(define double (cons-stream 1 (scale-stream 2 double)))
(check "powers of 2" '(1 2 4 8 16) (stream->list double 5))

;;; Section 3.4.5
(section "3.4.5 Streams as Signals")
(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream dt integrand) int)))
  int)
;; integrate constant 1 from 0 => should give 0, dt, 2dt, 3dt ...
(define unit-integral (integral ones 0 0.1))
(check "integral step 0" 0 (stream-ref unit-integral 0))
(check "integral step 1" #t (< (abs (- (stream-ref unit-integral 1) 0.1)) 0.001))
(check "integral step 10" #t (< (abs (- (stream-ref unit-integral 10) 1.0)) 0.001))

;;; ====================================================================
;;; CHAPTER 4: Metalinguistic Abstraction (selected pieces)
;;; ====================================================================

(display "======================================") (newline)
(display "CHAPTER 4") (newline)
(display "======================================") (newline)

;;; We can't load the metacircular evaluator wholesale (it redefines
;;; eval/apply) but we can test the data-structure helpers.

(section "4.1.2 Representation of Expressions")
(define (self-evaluating? exp)
  (or (number? exp) (string? exp)))
(define (quoted? exp) (and (pair? exp) (eq? (car exp) 'quote)))
(define (text-of-quotation exp) (cadr exp))
(define (variable? exp) (symbol? exp))
(define (assignment? exp) (and (pair? exp) (eq? (car exp) 'set!)))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (definition? exp) (and (pair? exp) (eq? (car exp) 'define)))
(define (lambda? exp) (and (pair? exp) (eq? (car exp) 'lambda)))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(check "self-eval? 42" #t (self-evaluating? 42))
(check "self-eval? sym" #f (self-evaluating? 'x))
(check "quoted?" #t (quoted? '(quote foo)))
(check "text-of-quotation" 'foo (text-of-quotation '(quote foo)))
(check "variable?" #t (variable? 'x))
(check "assignment?" #t (assignment? '(set! x 5)))
(check "lambda?" #t (lambda? '(lambda (x) x)))
(check "lambda-params" '(x y) (lambda-parameters '(lambda (x y) (+ x y))))

;;; Section 4.1.3 -- environment model
(section "4.1.3 Environment Model")
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())
(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "Too many/few arguments")))
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (null? env)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame) (frame-values frame)))))
  (env-loop env))
(define test-env
  (extend-environment '(x y z) '(1 2 3)
    (extend-environment '(a b) '(10 20) the-empty-environment)))
(check "env lookup x" 1 (lookup-variable-value 'x test-env))
(check "env lookup a" 10 (lookup-variable-value 'a test-env))
(check "env lookup z" 3 (lookup-variable-value 'z test-env))

;;; ====================================================================
;;; CHAPTER 5: Register Machines (selected pieces)
;;; ====================================================================

(display "======================================") (newline)
(display "CHAPTER 5") (newline)
(display "======================================") (newline)

;;; We can't run define-machine, but we can test the stack and
;;; supporting data structures.

(section "5.2.1 Stack")
(define (make-stack)
  (let ((s '()))
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s) (error "Empty stack")
          (let ((top (car s))) (set! s (cdr s)) top)))
    (define (init) (set! s '()) 'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'init) (init))
            (else (error "Unknown request" message))))
    dispatch))
(define stk (make-stack))
((stk 'push) 1)
((stk 'push) 2)
((stk 'push) 3)
(check "stack pop 3" 3 (stk 'pop))
(check "stack pop 2" 2 (stk 'pop))
(check "stack pop 1" 1 (stk 'pop))

;;; Section 5.2.2 -- Registers
(section "5.2.2 Registers")
(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set) (lambda (value) (set! contents value)))
            (else (error "Unknown request" message))))
    dispatch))
(define r (make-register 'test))
((r 'set) 42)
(check "register get" 42 (r 'get))
((r 'set) 'hello)
(check "register get 2" 'hello (r 'get))

;;; ====================================================================
;;; COMPATIBILITY LAYER TESTS
;;; ====================================================================

(display "======================================") (newline)
(display "SICP-COMPAT") (newline)
(display "======================================") (newline)

(section "Compatibility Prelude")
(check "nil" '() nil)
(check "t" #t t)
(check "atom? symbol" #t (atom? 'x))
(check "atom? pair" #f (atom? '(1 2)))
(check "atom? number" #t (atom? 42))
(check "1+" 6 (1+ 5))
(check "-1+" 4 (-1+ 5))
(check "sequence" 7 (sequence (+ 1 2) (+ 3 4)))
(check "cons-stream/head" 1 (head (cons-stream 1 (cons-stream 2 the-empty-stream))))
(check "cons-stream/tail/head" 2 (head (tail (cons-stream 1 (cons-stream 2 the-empty-stream)))))
(check "empty-stream?" #t (empty-stream? the-empty-stream))
(check "empty-stream? non" #f (empty-stream? (cons-stream 1 the-empty-stream)))

;;; ====================================================================
;;; SUMMARY
;;; ====================================================================

(newline)
(display "======================================") (newline)
(display "RESULTS") (newline)
(display "======================================") (newline)
(display "Total:  ") (display *tests-run*) (newline)
(display "Passed: ") (display *tests-passed*) (newline)
(display "Failed: ") (display *tests-failed*) (newline)
(if (> *tests-failed* 0)
    (begin
      (newline)
      (display "FAILURES:") (newline)
      (for-each (lambda (f)
                  (display "  ") (display (car f))
                  (display "  expected=") (display (cadr f))
                  (display "  actual=") (display (caddr f))
                  (newline))
                (reverse *test-failures*))))
(newline)
(if (= *tests-failed* 0)
    (display "ALL TESTS PASSED")
    (display "SOME TESTS FAILED"))
(newline)
(exit (if (= *tests-failed* 0) 0 1))
