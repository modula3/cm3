;;; test-cn-rationals.scm --- Compiler numerics: exact rational tests
;;; Exercises rational literals, arithmetic, loops, and exactness.


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


(section "CN-RATIONALS: predicates")

(test-true  "number? 1/3"         (number? 1/3))
(test-true  "rational? 1/3"       (rational? 1/3))
(test-true  "exact? 1/3"          (exact? 1/3))
(test-false "integer? 1/3"        (integer? 1/3))
(test-false "inexact? 1/3"        (inexact? 1/3))
(test-true  "rational? 42"        (rational? 42))
(test-true  "integer? 4/2"        (integer? 4/2))

(section "CN-RATIONALS: constants and demotion")

(test "const 1/3"    1/3    1/3)
(test "const -7/4"   -7/4   -7/4)
(test "const 1/2"    1/2    1/2)
(test "4/2 demotes"  2      4/2)
(test "3/1 demotes"  3      3/1)
(test "6/2 demotes"  3      6/2)
(test "-8/4 demotes" -2     -8/4)

;; GCD reduction
(test "2/4 = 1/2"    1/2    2/4)
(test "6/9 = 2/3"    2/3    6/9)
(test "-4/6 = -2/3"  -2/3   -4/6)

(section "CN-RATIONALS: arithmetic")

(test "1/3 + 1/3 = 2/3"      2/3     (+ 1/3 1/3))
(test "1/2 + 1/3 = 5/6"      5/6     (+ 1/2 1/3))
(test "1/2 + 1/2 = 1"        1       (+ 1/2 1/2))
(test "1/3 + 2 = 7/3"        7/3     (+ 1/3 2))
(test "1/2 - 1/3 = 1/6"      1/6     (- 1/2 1/3))
(test "1/3 - 1/3 = 0"        0       (- 1/3 1/3))
(test "1/2 * 1/3 = 1/6"      1/6     (* 1/2 1/3))
(test "2/3 * 3/2 = 1"        1       (* 2/3 3/2))
(test "3 * 1/3 = 1"          1       (* 3 1/3))
(test "(/ 1/2 1/3) = 3/2"    3/2     (/ 1/2 1/3))
(test "(- 1/3) = -1/3"       -1/3    (- 1/3))
(test "fold 1/2+1/3"         5/6     (+ 1/2 1/3))
(test "fold 1/3*3"            1       (* 1/3 3))

(section "CN-RATIONALS: comparisons")

(test-true  "(< 1/3 1/2)"        (< 1/3 1/2))
(test-false "(< 1/2 1/3)"        (< 1/2 1/3))
(test-true  "(= 2/4 1/2)"        (= 2/4 1/2))
(test-true  "(> 3/4 2/3)"        (> 3/4 2/3))
(test-true  "(<= 1/3 1/3)"       (<= 1/3 1/3))
(test-true  "(>= 1/2 1/3)"       (>= 1/2 1/3))
(test-true  "(= 1/2 0.5)"        (= 1/2 0.5))
(test-true  "(< 1/3 0.5)"        (< 1/3 0.5))
(test-true  "positive? 1/3"      (positive? 1/3))
(test-true  "negative? -1/3"     (negative? -1/3))
(test-false "zero? 1/3"          (zero? 1/3))

(section "CN-RATIONALS: numerator/denominator")

(test "numerator 1/3"    1  (numerator 1/3))
(test "denominator 1/3"  3  (denominator 1/3))
(test "numerator -3/4"   -3 (numerator -3/4))
(test "denominator -3/4" 4  (denominator -3/4))
(test "numerator 5"      5  (numerator 5))
(test "denominator 5"    1  (denominator 5))

(section "CN-RATIONALS: conversions")

(test "exact->inexact 1/2" 0.5  (exact->inexact 1/2))
(test-true "inexact? (exact->inexact 1/3)" (inexact? (exact->inexact 1/3)))

;; Mixed rational / inexact -> inexact contagion
(test-true "inexact? (+ 1/3 0.5)" (inexact? (+ 1/3 0.5)))
(test-true "inexact? (* 1/3 2.0)" (inexact? (* 1/3 2.0)))

(section "CN-RATIONALS: loops")

;; Harmonic number H(10) = 7381/2520
(test "harmonic H(10)"
  7381/2520
  (let loop ((i 1) (acc 0))
    (if (> i 10) acc
        (loop (+ i 1) (+ acc (/ 1 i))))))

;; Sum of 1/k^2 rational
(test-no-error "sum 1/k^2 rational"
  (lambda ()
    (let loop ((k 1) (acc 0))
      (if (> k 20) acc
          (loop (+ k 1) (+ acc (/ 1 (* k k))))))))

;; Golden ratio via continued fraction
(test-approx "golden ratio via rationals"
  1.6180339887498949
  (exact->inexact
    (let loop ((i 0) (acc 1))
      (if (= i 30) acc
          (loop (+ i 1) (+ 1 (/ 1 acc))))))
  1e-10)

;; Egyptian fraction expansion
(define (exact-ceiling-div d n)
  (let ((q (quotient d n))
        (r (remainder d n)))
    (if (= r 0) q (+ q 1))))

(define (egyptian-expand num den steps)
  (let loop ((n num) (d den) (i 0) (result '()))
    (if (or (= n 0) (= i steps))
        (reverse result)
        (let ((k (exact-ceiling-div d n)))
          (loop (- (* n k) d)
                (* d k)
                (+ i 1)
                (cons (/ 1 k) result))))))

(test "egyptian 5/7"
  '(1/2 1/5 1/70)
  (egyptian-expand 5 7 10))

;; Stern-Brocot mediants
(define (mediant a/b c/d)
  (/ (+ (numerator a/b) (numerator c/d))
     (+ (denominator a/b) (denominator c/d))))

(test "mediant 0/1 1/1" 1/2 (mediant 0 1))
(test "mediant 0/1 1/2" 1/3 (mediant 0 1/2))
(test "mediant 1/2 1/1" 2/3 (mediant 1/2 1))

(section "CN-RATIONALS: exactness preservation")

(test-true "1/3 stays exact through +"
  (exact? (+ 1/3 1/3)))

(test-true "rational stays exact through loop"
  (exact?
    (let loop ((i 0) (acc 0))
      (if (= i 10) acc
          (loop (+ i 1) (+ acc 1/7))))))

(test-true "exact + inexact = inexact in loop"
  (inexact?
    (let loop ((i 0) (acc 0))
      (if (= i 10)
          (+ acc 0.5)
          (loop (+ i 1) (+ acc 1))))))

(section "CN-RATIONALS: closures")

(define (make-adder n) (lambda (x) (+ n x)))
(define add-third (make-adder 1/3))
(test "closure over 1/3"    4/3  (add-third 1))
(test "closure over 1/3 v2" 2/3  (add-third 1/3))

(section "CN-RATIONALS: higher-order")

(test "fold + over rationals"
  1
  (let ((fold-left (lambda (f init lst)
                     (if (null? lst) init
                         (let loop ((rest (cdr lst)) (acc (f init (car lst))))
                           (if (null? rest) acc
                               (loop (cdr rest) (f acc (car rest)))))))))
    (fold-left + 0 '(1/4 1/4 1/4 1/4))))

(section "CN-RATIONALS: rationalize")

;; Basic: simplest rational within tolerance
(test "rationalize 3/10 1/10"    1/3   (rationalize 3/10 1/10))
(test "rationalize 1/3 0"        1/3   (rationalize 1/3 0))
(test "rationalize 1/2 0"        1/2   (rationalize 1/2 0))
(test "rationalize 3 0"          3     (rationalize 3 0))

;; Simplest in range: 0 is in [lo, hi]
(test "rationalize 1/10 1/2"     0     (rationalize 1/10 1/2))
(test "rationalize 0 1"          0     (rationalize 0 1))

;; Integer result when integer is in range
(test "rationalize 7/3 1/2"      2     (rationalize 7/3 1/2))
(test "rationalize 11/4 1/2"     3     (rationalize 11/4 1/2))

;; Negative inputs
(test "rationalize -3/10 1/10"   -1/3  (rationalize -3/10 1/10))
(test "rationalize -7/3 1/2"     -2    (rationalize -7/3 1/2))

;; Exact inputs produce exact results
(test-true "rationalize exact? 1"
  (exact? (rationalize 3/10 1/10)))
(test-true "rationalize exact? 2"
  (exact? (rationalize 1/3 0)))

;; Inexact inputs produce inexact results
(test-false "rationalize inexact input"
  (exact? (rationalize 0.3 0.1)))

;; Well-known approximation: pi ~ 355/113
(test "rationalize 355/113 1/100" 22/7  (rationalize 355/113 1/100))

;; Large tolerance: anything in [0, 2] -> simplest is 0
(test "rationalize 1 1"          0     (rationalize 1 1))

(test-summary "CN-RATIONALS")
