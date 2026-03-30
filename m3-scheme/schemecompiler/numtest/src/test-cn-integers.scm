;;; test-cn-integers.scm --- Compiler numerics: integer + bignum tests
;;; Uses only exact integers and floats — no rationals/complex.


(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (if (if (and (number? expected) (number? actual))
          (= expected actual)
          (equal? expected actual))
      (begin (set! pass-count (+ pass-count 1))
             (display "PASS: ") (display name) (newline))
      (begin (set! fail-count (+ fail-count 1))
             (display "FAIL: ") (display name)
             (display "  expected=") (display expected)
             (display "  actual=") (display actual)
             (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (section title)
  (newline)
  (display "--- ") (display title) (display " ---") (newline))

(define (test-no-error name thunk)
  (set! test-count (+ test-count 1))
  (unwind-protect
    (begin (thunk)
           (set! pass-count (+ pass-count 1))
           (display "PASS: ") (display name) (newline))
    #f
    (begin (set! fail-count (+ fail-count 1))
           (display "FAIL: ") (display name)
           (display " -- unexpected exception") (newline))))

(define (test-error name thunk)
  (set! test-count (+ test-count 1))
  (unwind-protect
    (begin (thunk)
           (set! fail-count (+ fail-count 1))
           (display "FAIL: ") (display name)
           (display " -- no exception raised") (newline))
    #f
    (begin (set! pass-count (+ pass-count 1))
           (display "PASS: ") (display name) (newline))))

(define (approx= a b tolerance)
  (< (abs (- a b)) tolerance))

(define (test-approx name expected actual tolerance)
  (set! test-count (+ test-count 1))
  (if (approx= expected actual tolerance)
      (begin (set! pass-count (+ pass-count 1))
             (display "PASS: ") (display name) (newline))
      (begin (set! fail-count (+ fail-count 1))
             (display "FAIL: ") (display name)
             (display "  expected~=") (display expected)
             (display "  actual=") (display actual)
             (newline))))

(define (test-summary label)
  (newline)
  (display "=== ") (display label) (display ": ")
  (display pass-count) (display "/") (display test-count) (display " passed")
  (if (> fail-count 0)
      (begin (display ", ") (display fail-count) (display " FAILED")))
  (display " ===") (newline))


(section "CN-INTEGERS: predicates")

(test-true  "number? 42"          (number? 42))
(test-true  "integer? 42"         (integer? 42))
(test-true  "exact? 42"           (exact? 42))
(test-false "inexact? 42"         (inexact? 42))
(test-true  "real? 42"            (real? 42))
(test-true  "number? 2^100"       (number? (expt 2 100)))
(test-true  "integer? 2^100"      (integer? (expt 2 100)))
(test-true  "exact? 2^100"        (exact? (expt 2 100)))

(section "CN-INTEGERS: constants")

(test "const 0"      0      0)
(test "const 1"      1      1)
(test "const -1"     -1     -1)
(test "const 42"     42     42)
(test "fold 2+3"     5      (+ 2 3))
(test "fold 2*3"     6      (* 2 3))

(section "CN-INTEGERS: loops")

;; Sum 1..100 = 5050
(test "sum 1..100"
  5050
  (let loop ((i 1) (acc 0))
    (if (> i 100) acc
        (loop (+ i 1) (+ acc i)))))

;; Factorial 20 (bignum result)
(test "factorial 20"
  2432902008176640000
  (let loop ((i 1) (acc 1))
    (if (> i 20) acc
        (loop (+ i 1) (* acc i)))))

;; Factorial 50 (definitely bignum, self-consistent check)
(test "factorial 50"
  (let loop ((i 1) (acc 1))
    (if (> i 50) acc
        (loop (+ i 1) (* acc i))))
  (let loop ((i 1) (acc 1))
    (if (> i 50) acc
        (loop (+ i 1) (* acc i)))))

;; Fibonacci
(test "fib 30"
  832040
  (let loop ((i 0) (a 0) (b 1))
    (if (= i 30) a
        (loop (+ i 1) b (+ a b)))))

;; 2^64 (bignum on 64-bit)
(test "2^64 via loop"
  (expt 2 64)
  (let loop ((i 0) (acc 1))
    (if (= i 64) acc
        (loop (+ i 1) (* acc 2)))))

(section "CN-INTEGERS: bignum precision boundary")

(test-true "big loop result is exact"
  (exact?
    (let loop ((i 0) (acc 1))
      (if (= i 60) acc
          (loop (+ i 1) (* acc 2))))))

(test "2^60 via loop"
  (expt 2 60)
  (let loop ((i 0) (acc 1))
    (if (= i 60) acc
        (loop (+ i 1) (* acc 2)))))

(test "bignum sum"
  (+ (expt 2 100) (expt 2 100))
  (* 2 (expt 2 100)))

(define big-exact (expt 2 100))
(define big-inexact (exact->inexact big-exact))
(test-true  "big->inexact is inexact" (inexact? big-inexact))
(test-true  "big->inexact is number"  (number? big-inexact))
(test-no-error "big inexact->exact roundtrip"
  (lambda () (inexact->exact big-inexact)))

(section "CN-INTEGERS: unboxing patterns")

(test "unbox: sum 1..10"
  55
  (let loop ((i 1) (sum 0))
    (if (> i 10) sum
        (loop (+ i 1) (+ sum i)))))

(test "unbox: product 1..15"
  1307674368000
  (let loop ((i 1) (prod 1))
    (if (> i 15) prod
        (loop (+ i 1) (* prod i)))))

(define big-sum
  (let loop ((i 0) (acc 0))
    (if (= i 100) acc
        (loop (+ i 1) (+ acc (expt 2 53))))))
(test-true "big sum is number" (number? big-sum))
(test "big sum value" (* 100 (expt 2 53)) big-sum)

(test "unbox: exact result from small loop"
  100
  (let loop ((i 0) (acc 0))
    (if (= i 100) acc
        (loop (+ i 1) (+ acc 1)))))

(test-true "unbox: exact result stays exact"
  (exact?
    (let loop ((i 0) (acc 0))
      (if (= i 100) acc
          (loop (+ i 1) (+ acc 1))))))

(section "CN-INTEGERS: tail-recursive algorithms")

(define (my-gcd a b)
  (if (= b 0) a
      (my-gcd b (remainder a b))))

(test "gcd 12 8"    4  (my-gcd 12 8))
(test "gcd 100 75"  25 (my-gcd 100 75))
(test "gcd 7 13"    1  (my-gcd 7 13))

(define (collatz-length n)
  (let loop ((x n) (count 0))
    (if (= x 1) count
        (if (= (remainder x 2) 0)
            (loop (/ x 2) (+ count 1))
            (loop (+ (* 3 x) 1) (+ count 1))))))

(test "collatz 1"     0   (collatz-length 1))
(test "collatz 2"     1   (collatz-length 2))
(test "collatz 27"    111 (collatz-length 27))

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(test "ack 0 0" 1   (ack 0 0))
(test "ack 1 1" 3   (ack 1 1))
(test "ack 2 2" 7   (ack 2 2))
(test "ack 3 3" 61  (ack 3 3))

(section "CN-INTEGERS: closures")

(define (make-adder n) (lambda (x) (+ n x)))
(test "closure over fixnum"  52 ((make-adder 10) 42))
(define add-big (make-adder (expt 2 100)))
(test "closure over bignum"  (+ (expt 2 100) 1) (add-big 1))

(section "CN-INTEGERS: higher-order")

(test "map number? ints"
  '(#t #t #t)
  (map number? (list 0 42 (expt 2 100))))

(test "map exact? ints"
  '(#t #t #t)
  (map exact? (list 0 42 (expt 2 100))))

(define (fold-left f init lst)
  (if (null? lst) init
      (fold-left f (f init (car lst)) (cdr lst))))

(test "fold + over integers"
  15
  (fold-left + 0 '(1 2 3 4 5)))

(test-summary "CN-INTEGERS")
