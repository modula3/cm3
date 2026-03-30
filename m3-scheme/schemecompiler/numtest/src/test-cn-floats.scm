;;; test-cn-floats.scm --- Compiler numerics: float/LONGREAL tests
;;; Uses only integers and floats — no rationals/complex.


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


(section "CN-FLOATS: predicates")

(test-true  "number? 3.14"        (number? 3.14))
(test-true  "inexact? 3.14"       (inexact? 3.14))
(test-false "exact? 3.14"         (exact? 3.14))
(test-true  "real? 3.14"          (real? 3.14))

;; Special floats
(test-true  "number? +inf.0"      (number? +inf.0))
(test-true  "number? -inf.0"      (number? -inf.0))
(test-true  "number? +nan.0"      (number? +nan.0))
(test-true  "inexact? +inf.0"     (inexact? +inf.0))

(section "CN-FLOATS: constants")

(test "const 3.14"   3.14   3.14)
(test "const -0.0"   -0.0   -0.0)
(test "const 0.0"    0.0    0.0)

(section "CN-FLOATS: loops")

;; Sum 1.0..100.0
(test-approx "sum 1.0..100.0"
  5050.0
  (let loop ((i 1.0) (acc 0.0))
    (if (> i 100.0) acc
        (loop (+ i 1.0) (+ acc i))))
  0.001)

;; Newton's method for sqrt(2)
(test-approx "newton sqrt(2)"
  1.4142135623730951
  (let loop ((x 1.0) (n 0))
    (if (= n 20) x
        (loop (/ (+ x (/ 2.0 x)) 2.0) (+ n 1))))
  1e-14)

;; Geometric series: sum of 1/2^k for k=0..50
(test-approx "geometric series"
  2.0
  (let loop ((k 0) (term 1.0) (acc 0.0))
    (if (> k 50) acc
        (loop (+ k 1) (/ term 2.0) (+ acc term))))
  1e-10)

(section "CN-FLOATS: unboxing patterns")

(test-approx "unbox: float accumulator"
  100.0
  (let loop ((i 0) (acc 0.0))
    (if (= i 100) acc
        (loop (+ i 1) (+ acc 1.0))))
  0.001)

(test-false "float stays inexact through loop"
  (exact?
    (let loop ((i 0) (acc 0.0))
      (if (= i 10) acc
          (loop (+ i 1) (+ acc 1.0))))))

(section "CN-FLOATS: special float arithmetic")

(test-true  "(> +inf.0 1e308)"      (> +inf.0 1e308))
(test-true  "(< -inf.0 -1e308)"     (< -inf.0 -1e308))
(test-no-error "+inf.0 + 1" (lambda () (+ +inf.0 1)))
(test-no-error "-inf.0 * -1" (lambda () (* -inf.0 -1)))

(section "CN-FLOATS: cross-type int/float")

(test-true  "(= 1 1.0)"          (= 1 1.0))
(test-true  "(= 42 42.0)"        (= 42 42.0))
(test "exact->inexact 42"        42.0  (exact->inexact 42))
(test "inexact->exact 42.0"      42    (inexact->exact 42.0))
(test-true "exact? (inexact->exact 42.0)" (exact? (inexact->exact 42.0)))

(section "CN-FLOATS: closures")

(define (make-adder n) (lambda (x) (+ n x)))
(define add-pi (make-adder 3.14))
(test-approx "closure over float" 4.14 (add-pi 1.0) 1e-10)

(section "CN-FLOATS: comparisons")

(test-true  "zero? 0.0"          (zero? 0.0))
(test-false "zero? 3.14"         (zero? 3.14))
(test-true  "positive? 3.14"     (positive? 3.14))
(test-true  "negative? -1.0"     (negative? -1.0))

(test-summary "CN-FLOATS")
