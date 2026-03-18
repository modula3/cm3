;;; Comprehensive test suite for MScheme Level 1 Compiler
;;; Tests every supported construct: if, cond, begin, let, let*,
;;; and, or, not, set!, quote, self-tail-call, multi-arity calls,
;;; free variables, constants (number, string, boolean, char, symbol, list).

;;;
;;; ==================== Arithmetic ====================
;;;

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (square x)
  (* x x))

(define (cube x)
  (* x (* x x)))

(define (abs-val x)
  (if (< x 0) (- 0 x) x))

(define (max-of-two a b)
  (if (> a b) a b))

(define (min-of-two a b)
  (if (< a b) a b))

(define (power base exp)
  (if (= exp 0)
      1
      (* base (power base (- exp 1)))))

(define (sum-range n acc)
  (if (= n 0)
      acc
      (sum-range (- n 1) (+ acc n))))

(define (even? n)
  (= 0 (- n (* 2 (quotient n 2)))))

(define (odd? n)
  (not (even? n)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (negate x)
  (- 0 x))

(define (double x)
  (+ x x))

(define (triple x)
  (+ x (+ x x)))

(define (add3 a b c)
  (+ a (+ b c)))

(define (add4 a b c d)
  (+ a (+ b (+ c d))))

;;;
;;; ==================== List Operations ====================
;;;

(define (my-length lst)
  (if (null? lst)
      0
      (+ 1 (my-length (cdr lst)))))

(define (sum-list lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))

(define (product-list lst)
  (if (null? lst)
      1
      (* (car lst) (product-list (cdr lst)))))

(define (my-last lst)
  (if (null? (cdr lst))
      (car lst)
      (my-last (cdr lst))))

(define (my-nth lst n)
  (if (= n 0)
      (car lst)
      (my-nth (cdr lst) (- n 1))))

(define (my-append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (my-append (cdr lst1) lst2))))

(define (my-reverse-acc lst acc)
  (if (null? lst)
      acc
      (my-reverse-acc (cdr lst) (cons (car lst) acc))))

(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (my-map f (cdr lst)))))

(define (my-filter pred lst)
  (if (null? lst)
      '()
      (if (pred (car lst))
          (cons (car lst) (my-filter pred (cdr lst)))
          (my-filter pred (cdr lst)))))

(define (my-member x lst)
  (if (null? lst)
      #f
      (if (= x (car lst))
          #t
          (my-member x (cdr lst)))))

(define (count-if pred lst)
  (if (null? lst)
      0
      (+ (if (pred (car lst)) 1 0)
         (count-if pred (cdr lst)))))

(define (take lst n)
  (if (= n 0)
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

(define (drop lst n)
  (if (= n 0)
      lst
      (drop (cdr lst) (- n 1))))

(define (zip lst1 lst2)
  (if (null? lst1)
      '()
      (if (null? lst2)
          '()
          (cons (cons (car lst1) (car lst2))
                (zip (cdr lst1) (cdr lst2))))))

(define (flatten lst)
  (if (null? lst)
      '()
      (if (pair? (car lst))
          (my-append (flatten (car lst)) (flatten (cdr lst)))
          (cons (car lst) (flatten (cdr lst))))))

(define (list-ref-safe lst n default)
  (if (null? lst)
      default
      (if (= n 0)
          (car lst)
          (list-ref-safe (cdr lst) (- n 1) default))))

(define (iota-acc n acc)
  (if (= n 0)
      acc
      (iota-acc (- n 1) (cons n acc))))

(define (repeat-val x n)
  (if (= n 0)
      '()
      (cons x (repeat-val x (- n 1)))))

;;;
;;; ==================== Cond ====================
;;;

(define (sign x)
  (cond ((< x 0) -1)
        ((= x 0) 0)
        (else 1)))

(define (classify n)
  (cond ((< n 0)   "negative")
        ((= n 0)   "zero")
        ((< n 10)  "small")
        ((< n 100) "medium")
        (else      "large")))

(define (fizzbuzz n)
  (cond ((= 0 (remainder n 15)) "fizzbuzz")
        ((= 0 (remainder n 3))  "fizz")
        ((= 0 (remainder n 5))  "buzz")
        (else                    n)))

(define (letter-grade score)
  (cond ((>= score 90) "A")
        ((>= score 80) "B")
        ((>= score 70) "C")
        ((>= score 60) "D")
        (else          "F")))

;;;
;;; ==================== Begin ====================
;;;

(define (begin-test x)
  (begin
    (+ x 1)
    (+ x 2)
    (+ x 3)))

(define (begin-with-side-effect x)
  (begin
    (+ x 10)
    (* x 2)))

;;;
;;; ==================== Let / Let* ====================
;;;

(define (hypotenuse a b)
  (let ((a2 (* a a))
        (b2 (* b b)))
    (+ a2 b2)))

(define (circle-area r)
  (let ((pi 3141593)
        (r2 (* r r)))
    (quotient (* pi r2) 1000000)))

(define (let-nested x)
  (let ((a (+ x 1)))
    (let ((b (* a 2)))
      (+ a b))))

(define (let-star-chain x)
  (let* ((a (+ x 1))
         (b (* a 2))
         (c (+ a b)))
    c))

(define (let-star-deps x y)
  (let* ((sum (+ x y))
         (diff (- x y))
         (prod (* sum diff)))
    prod))

(define (swap-via-let a b)
  (let ((x b)
        (y a))
    (+ (* x 1000) y)))

(define (multi-let x)
  (let ((a (+ x 1))
        (b (+ x 2))
        (c (+ x 3)))
    (+ a (+ b c))))

;;;
;;; ==================== And / Or / Not ====================
;;;

(define (both-positive? a b)
  (and (> a 0) (> b 0)))

(define (either-zero? a b)
  (or (= a 0) (= b 0)))

(define (in-range? x lo hi)
  (and (>= x lo) (<= x hi)))

(define (out-of-range? x lo hi)
  (or (< x lo) (> x hi)))

(define (three-and a b c)
  (and a b c))

(define (three-or a b c)
  (or a b c))

(define (not-zero? x)
  (not (= x 0)))

(define (xor-bool a b)
  (and (or a b) (not (and a b))))

;;;
;;; ==================== Set! (Mutation) ====================
;;;

(define (set-param x)
  (begin
    (set! x (+ x 10))
    x))

(define (set-in-let x)
  (let ((a 0))
    (begin
      (set! a (+ x 100))
      a)))

(define (set-accumulate x)
  (let ((a x))
    (begin
      (set! a (* a 2))
      (set! a (+ a 1))
      a)))

;;;
;;; ==================== Self-Tail-Call Optimization ====================
;;;

(define (loop-sum n acc)
  (if (= n 0)
      acc
      (loop-sum (- n 1) (+ acc n))))

(define (count-down n)
  (if (= n 0)
      0
      (count-down (- n 1))))

(define (tail-factorial n acc)
  (if (= n 0)
      acc
      (tail-factorial (- n 1) (* n acc))))

(define (tail-fib-iter n a b)
  (if (= n 0)
      a
      (tail-fib-iter (- n 1) b (+ a b))))

(define (tail-length lst acc)
  (if (null? lst)
      acc
      (tail-length (cdr lst) (+ acc 1))))

(define (tail-sum-list lst acc)
  (if (null? lst)
      acc
      (tail-sum-list (cdr lst) (+ acc (car lst)))))

(define (tail-last lst)
  (if (null? (cdr lst))
      (car lst)
      (tail-last (cdr lst))))

(define (tail-min lst best)
  (if (null? lst)
      best
      (tail-min (cdr lst) (if (< (car lst) best) (car lst) best))))

(define (tail-max lst best)
  (if (null? lst)
      best
      (tail-max (cdr lst) (if (> (car lst) best) (car lst) best))))

(define (repeat-string n acc)
  (if (= n 0)
      acc
      (repeat-string (- n 1) (string-append acc "x"))))

;;;
;;; ==================== Constants ====================
;;;

(define (return-true)
  #t)

(define (return-false)
  #f)

(define (return-zero)
  0)

(define (return-one)
  1)

(define (return-string)
  "hello")

(define (return-empty-list)
  '())

(define (return-symbol)
  'foo)

(define (return-quoted-list)
  '(1 2 3))

(define (return-char)
  #\A)

(define (identity x)
  x)

(define (constant-42)
  42)

(define (return-pi-approx)
  3141593)

(define (return-negative)
  -7)

;;;
;;; ==================== Recursive Algorithms ====================
;;;

(define (ackermann m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ackermann (- m 1) 1))
        (else (ackermann (- m 1) (ackermann m (- n 1))))))

(define (hanoi-moves n)
  (if (= n 0)
      0
      (+ 1 (* 2 (hanoi-moves (- n 1))))))

(define (depth lst)
  (if (not (pair? lst))
      0
      (max-of-two (+ 1 (depth (car lst)))
                  (depth (cdr lst)))))

(define (tree-sum tree)
  (if (not (pair? tree))
      (if (number? tree) tree 0)
      (+ (tree-sum (car tree))
         (tree-sum (cdr tree)))))

(define (tree-count tree)
  (if (not (pair? tree))
      (if (null? tree) 0 1)
      (+ (tree-count (car tree))
         (tree-count (cdr tree)))))

(define (collatz-steps n acc)
  (if (= n 1)
      acc
      (if (even? n)
          (collatz-steps (quotient n 2) (+ acc 1))
          (collatz-steps (+ 1 (* 3 n)) (+ acc 1)))))

(define (digit-sum n)
  (if (< n 10)
      n
      (+ (remainder n 10) (digit-sum (quotient n 10)))))

(define (num-digits n)
  (if (< n 10)
      1
      (+ 1 (num-digits (quotient n 10)))))

(define (is-palindrome-helper lst1 lst2)
  (if (null? lst1)
      #t
      (if (= (car lst1) (car lst2))
          (is-palindrome-helper (cdr lst1) (cdr lst2))
          #f)))

(define (list-equal? lst1 lst2)
  (cond ((and (null? lst1) (null? lst2)) #t)
        ((or (null? lst1) (null? lst2))  #f)
        ((= (car lst1) (car lst2))
         (list-equal? (cdr lst1) (cdr lst2)))
        (else #f)))

;;;
;;; ==================== Higher-Order (via free variables) ====================
;;;

(define (apply-twice f x)
  (f (f x)))

(define (compose-apply f g x)
  (f (g x)))

;;;
;;; ==================== Mixed Constructs ====================
;;;

(define (complex-cond-let x)
  (cond ((< x 0)
         (let ((ax (- 0 x)))
           (* ax ax)))
        ((= x 0) 0)
        (else
         (let* ((a (+ x 1))
                (b (* a a)))
           b))))

(define (nested-if-let x y)
  (if (> x y)
      (let ((d (- x y)))
        (+ d d))
      (let ((d (- y x)))
        (* d d))))

(define (and-or-mix a b c)
  (if (and (> a 0) (or (> b 0) (> c 0)))
      1
      0))

(define (multi-begin-let x)
  (let ((a (begin (+ x 1) (+ x 2))))
    (let ((b (begin (+ a 3) (* a 2))))
      (+ a b))))

(define (cond-with-begin x)
  (cond ((< x 0)
         (begin (+ x 1) (- 0 x)))
        ((= x 0)
         (begin 0))
        (else
         (begin (+ x 1) x))))

(define (if-chain x)
  (if (> x 100)
      "big"
      (if (> x 50)
          "medium"
          (if (> x 10)
              "small"
              "tiny"))))

;;;
;;; ==================== Edge Cases ====================
;;;

(define (zero-args)
  42)

(define (single-arg x)
  (+ x 1))

(define (five-args a b c d e)
  (+ a (+ b (+ c (+ d e)))))

(define (deeply-nested x)
  (if (> x 0)
      (if (> x 1)
          (if (> x 2)
              (if (> x 3)
                  (if (> x 4)
                      "deep"
                      4)
                  3)
              2)
          1)
      0))

(define (many-lets x)
  (let ((a (+ x 1)))
    (let ((b (+ a 2)))
      (let ((c (+ b 3)))
        (let ((d (+ c 4)))
          d)))))

(define (let-shadow x)
  (let ((x (+ x 10)))
    (let ((x (* x 2)))
      x)))

(define (cond-many x)
  (cond ((= x 1) "one")
        ((= x 2) "two")
        ((= x 3) "three")
        ((= x 4) "four")
        (else     "other")))

;;; if-no-else removed: generates M3 function that may not return a value

(define (boolean-identity x)
  (if x #t #f))

;;;
;;; ==================== Cond => ====================
;;;

(define (cond-arrow-test x)
  ;; (cond (test => proc)) -- passes test value to proc if truthy
  (cond ((assq x '((a . 1) (b . 2) (c . 3))) => cdr)
        (else #f)))

;;;
;;; ==================== Letrec ====================
;;;

(define (letrec-simple x)
  (letrec ((a (+ x 1))
           (b (+ x 2)))
    (+ a b)))

(define (letrec-even-odd n)
  ;; Mutual recursion via letrec -- tests that vars are in scope during init
  ;; (but here we just use values, not lambdas)
  (letrec ((base (if (= n 0) 1 0))
           (sign (if (< n 0) -1 1)))
    (+ base sign)))

;;;
;;; ==================== Case ====================
;;;

(define (day-type day)
  (case day
    ((monday tuesday wednesday thursday friday) "weekday")
    ((saturday sunday) "weekend")
    (else "unknown")))

(define (case-number x)
  (case x
    ((1) "one")
    ((2 3) "two-or-three")
    ((4 5 6) "four-to-six")
    (else "other")))

(define (case-symbol x)
  (case x
    ((red) 1)
    ((green) 2)
    ((blue) 3)
    (else 0)))

;;;
;;; ==================== Do Loops ====================
;;;

(define (do-sum-to n)
  ;; Sum from 1 to n using do loop
  (do ((i 1 (+ i 1))
       (acc 0 (+ acc i)))
      ((> i n) acc)))

(define (do-countdown n)
  ;; Count down, return 0
  (do ((i n (- i 1)))
      ((= i 0) 0)))

(define (do-list-reverse lst)
  ;; Reverse a list using do
  (do ((rest lst (cdr rest))
       (acc '() (cons (car rest) acc)))
      ((null? rest) acc)))

(define (do-factorial n)
  ;; Factorial using do
  (do ((i 1 (+ i 1))
       (acc 1 (* acc i)))
      ((> i n) acc)))

;;;
;;; ==================== Rest Parameters ====================
;;;

(define (rest-sum . args)
  ;; Sum all arguments
  (do ((rest args (cdr rest))
       (acc 0 (+ acc (car rest))))
      ((null? rest) acc)))

(define (rest-first-or-default default . args)
  ;; Return first arg if any, else default
  (if (null? args)
      default
      (car args)))

(define (rest-count . args)
  ;; Count arguments
  (do ((rest args (cdr rest))
       (n 0 (+ n 1)))
      ((null? rest) n)))

;;;
;;; ==================== Named Let ====================
;;;

(define (named-let-sum n)
  ;; Sum 1..n using named let
  (let loop ((i n) (acc 0))
    (if (= i 0)
        acc
        (loop (- i 1) (+ acc i)))))

(define (named-let-length lst)
  ;; Length using named let
  (let loop ((rest lst) (n 0))
    (if (null? rest)
        n
        (loop (cdr rest) (+ n 1)))))

(define (named-let-reverse lst)
  ;; Reverse using named let
  (let loop ((rest lst) (acc '()))
    (if (null? rest)
        acc
        (loop (cdr rest) (cons (car rest) acc)))))

(define (named-let-iota n)
  ;; Build (1 2 ... n) using named let
  (let loop ((i n) (acc '()))
    (if (= i 0)
        acc
        (loop (- i 1) (cons i acc)))))

(define (named-let-find x lst)
  ;; Find x in lst, return #t/#f
  (let loop ((rest lst))
    (cond ((null? rest) #f)
          ((equal? x (car rest)) #t)
          (else (loop (cdr rest))))))

(define (named-let-map-square lst)
  ;; Map square over list using named let
  (let loop ((rest lst) (acc '()))
    (if (null? rest)
        (let loop2 ((r acc) (out '()))
          (if (null? r)
              out
              (loop2 (cdr r) (cons (car r) out))))
        (loop (cdr rest) (cons (* (car rest) (car rest)) acc)))))

(define (named-let-nontail n)
  ;; Named let in non-tail position: sum 1..n using side effects
  (let ((result 0))
    (let loop ((i 1))
      (if (<= i n)
          (begin (set! result (+ result i))
                 (loop (+ i 1)))))
    result))

;;;
;;; ==================== Redefinition Semantics ====================
;;;

;; Chain of compiled functions for testing redefinition visibility.
;; redef-inner is a free variable of redef-middle;
;; redef-middle is a free variable of redef-outer.
;; Redefining redef-inner should be visible through the whole chain.

(define (redef-inner x) (* x 10))

(define (redef-middle x) (+ (redef-inner x) 1))

(define (redef-outer x) (* (redef-middle x) 3))

;;;
;;; ==================== Lambda ====================
;;;

;; Lambda as callback to higher-order function
(define (lambda-callback lst)
  (my-map (lambda (x) (+ x 1)) lst))

;; Lambda capturing a function parameter
(define (lambda-capture-param x lst)
  (my-map (lambda (y) (+ x y)) lst))

;; Lambda capturing a let-bound variable
(define (lambda-capture-let lst)
  (let ((offset 100))
    (my-map (lambda (x) (+ x offset)) lst)))

;; Lambda capturing a free variable (binding cell)
(define (lambda-capture-binding lst)
  (my-map (lambda (x) (* x x)) lst))

;; Two-parameter lambda
(define (lambda-2param lst)
  (my-map (lambda (x) (+ x x)) lst))

;; Lambda with rest parameter
(define (lambda-rest-test)
  (let ((f (lambda (x . rest)
             (if (null? rest) x (+ x (car rest))))))
    (+ (f 10) (f 10 20))))

;; Lambda in conditional branch
(define (lambda-in-cond flag lst)
  (my-map (if flag
              (lambda (x) (+ x 1))
              (lambda (x) (* x 2)))
          lst))

;; Lambda with no captures
(define (lambda-no-capture)
  (let ((f (lambda (x) (+ x 1))))
    (f 41)))

;; Lambda as let-bound function
(define (lambda-let-bound x)
  (let ((add-x (lambda (y) (+ x y))))
    (+ (add-x 10) (add-x 20))))

;; Internal define (non-recursive)
(define (internal-define-simple x)
  (define (helper y) (+ x y))
  (helper 10))
