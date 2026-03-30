;;; test-compiler-numerics.scm --- Compiler acceptance tests for numeric tower
;;;
;;; Every test here must produce identical results whether run interpreted
;;; or compiled.  The test exercises:
;;;
;;;  1. All Phase 1+2 numeric operations through the compiler
;;;  2. Loops with numeric accumulation (named let, self-tail-call)
;;;  3. Constant folding of numeric literals
;;;  4. Mixed-type arithmetic in loops (int/bignum/rational/float)
;;;  5. Bignum -> LONGREAL unboxing with precision loss (must work)
;;;  6. Closures capturing numeric values of each type
;;;  7. Higher-order functions over numeric types
;;;
;;; Usage: run via numtest binary, or: mscheme test-compiler-numerics.scm

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

;;; ============================================================
;;; 1. Basic type predicates (must work through compiler)
;;; ============================================================

(section "type predicates through compiler")

;; Fixnum
(test-true  "number? 42"          (number? 42))
(test-true  "integer? 42"         (integer? 42))
(test-true  "exact? 42"           (exact? 42))
(test-false "inexact? 42"         (inexact? 42))
(test-true  "real? 42"            (real? 42))
(test-true  "rational? 42"        (rational? 42))

;; Bignum
(test-true  "number? 2^100"       (number? (expt 2 100)))
(test-true  "integer? 2^100"      (integer? (expt 2 100)))
(test-true  "exact? 2^100"        (exact? (expt 2 100)))

;; Rational
(test-true  "number? 1/3"         (number? 1/3))
(test-true  "rational? 1/3"       (rational? 1/3))
(test-true  "exact? 1/3"          (exact? 1/3))
(test-false "integer? 1/3"        (integer? 1/3))
(test-false "inexact? 1/3"        (inexact? 1/3))

;; Inexact
(test-true  "number? 3.14"        (number? 3.14))
(test-true  "inexact? 3.14"       (inexact? 3.14))
(test-false "exact? 3.14"         (exact? 3.14))

;; Complex
(test-true  "number? 3+4i"        (number? (make-rectangular 3 4)))
(test-true  "complex? 3+4i"       (complex? (make-rectangular 3 4)))
(test-false "real? 3+4i"          (real? (make-rectangular 3 4)))

;; Special floats
(test-true  "number? +inf.0"      (number? +inf.0))
(test-true  "number? -inf.0"      (number? -inf.0))
(test-true  "number? +nan.0"      (number? +nan.0))

;;; ============================================================
;;; 2. Constant folding
;;; ============================================================

(section "constant folding")

;; The compiler should handle these literals correctly,
;; even if it folds or embeds them as M3 constants.

;; Integer constants
(test "const 0"      0      0)
(test "const 1"      1      1)
(test "const -1"     -1     -1)
(test "const 42"     42     42)

;; Rational constants
(test "const 1/3"    1/3    1/3)
(test "const -7/4"   -7/4   -7/4)
(test "const 1/2"    1/2    1/2)
(test "const 4/2"    2      4/2)  ;; demotes

;; Float constants
(test "const 3.14"   3.14   3.14)
(test "const -0.0"   -0.0   -0.0)

;; Complex constants
(test "const 3+4i"   (make-rectangular 3 4)   (make-rectangular 3 4))

;; Constant arithmetic (compiler may fold these)
(test "fold 2+3"     5      (+ 2 3))
(test "fold 1/2+1/3" 5/6    (+ 1/2 1/3))
(test "fold 2*3"     6      (* 2 3))
(test "fold 1/3*3"   1      (* 1/3 3))

;;; ============================================================
;;; 3. Integer loops (named let with self-tail-call)
;;; ============================================================

(section "integer loops — named let")

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

;; Factorial 50 (definitely bignum)
(test "factorial 50"
  (let loop ((i 1) (acc 1))
    (if (> i 50) acc
        (loop (+ i 1) (* acc i))))
  (let loop ((i 1) (acc 1))
    (if (> i 50) acc
        (loop (+ i 1) (* acc i)))))

;; Fibonacci (two accumulators)
(test "fib 30"
  832040
  (let loop ((i 0) (a 0) (b 1))
    (if (= i 30) a
        (loop (+ i 1) b (+ a b)))))

;; Power of 2: 2^64 (bignum on 64-bit)
(test "2^64 via loop"
  (expt 2 64)
  (let loop ((i 0) (acc 1))
    (if (= i 64) acc
        (loop (+ i 1) (* acc 2)))))

;;; ============================================================
;;; 4. Float loops
;;; ============================================================

(section "float loops")

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

;;; ============================================================
;;; 5. Rational loops
;;; ============================================================

(section "rational loops")

;; Harmonic number H(10) = 1 + 1/2 + 1/3 + ... + 1/10 = 7381/2520
(test "harmonic H(10)"
  7381/2520
  (let loop ((i 1) (acc 0))
    (if (> i 10) acc
        (loop (+ i 1) (+ acc (/ 1 i))))))

;; Sum of 1/k^2 for k=1..20 (rational)
(test-no-error "sum 1/k^2 rational"
  (lambda ()
    (let loop ((k 1) (acc 0))
      (if (> k 20) acc
          (loop (+ k 1) (+ acc (/ 1 (* k k))))))))

;; Continued fraction for golden ratio: 1+1/(1+1/(1+1/...))
;; After enough iterations, should be close to (1+sqrt(5))/2
(test-approx "golden ratio via rationals"
  1.6180339887498949
  (exact->inexact
    (let loop ((i 0) (acc 1))
      (if (= i 30) acc
          (loop (+ i 1) (+ 1 (/ 1 acc))))))
  1e-10)

;;; ============================================================
;;; 6. Mixed-type loops
;;; ============================================================

(section "mixed-type loops")

;; Start with integer, accumulate rationals
(test "int + rational loop"
  (+ 10 7381/2520)  ;; 10 + H(10)
  (let loop ((i 1) (acc 10))
    (if (> i 10) acc
        (loop (+ i 1) (+ acc (/ 1 i))))))

;; Integer loop with float comparison
(test "int loop, float exit"
  55
  (let loop ((i 1) (acc 0))
    (if (> (exact->inexact acc) 50.0) acc
        (loop (+ i 1) (+ acc i)))))

;; Rational accumulator meeting inexact
(test-true "rational + float -> inexact"
  (inexact?
    (let loop ((i 0) (acc 1/3))
      (if (= i 5) (+ acc 0.5)  ;; inject inexact at the end
          (loop (+ i 1) (+ acc 1/7))))))

;;; ============================================================
;;; 7. Bignum in loops — precision boundary
;;; ============================================================

(section "bignum loops — precision boundary")

;; Build a number bigger than 2^53 (LONGREAL can't represent exactly)
;; then check it's still exact
(test-true "big loop result is exact"
  (exact?
    (let loop ((i 0) (acc 1))
      (if (= i 60) acc
          (loop (+ i 1) (* acc 2))))))

;; 2^60 is exact as an integer
(test "2^60 via loop"
  (expt 2 60)
  (let loop ((i 0) (acc 1))
    (if (= i 60) acc
        (loop (+ i 1) (* acc 2)))))

;; Bignum arithmetic preserves exactness
(test "bignum sum"
  (+ (expt 2 100) (expt 2 100))
  (* 2 (expt 2 100)))

;; Converting huge exact int to inexact and back should lose precision
;; but the conversion itself must not crash
(define big-exact (expt 2 100))
(define big-inexact (exact->inexact big-exact))
(test-true  "big->inexact is inexact" (inexact? big-inexact))
(test-true  "big->inexact is number"  (number? big-inexact))

;; The round-trip is lossy: (inexact->exact (exact->inexact 2^100))
;; may not equal 2^100, but must not crash
(test-no-error "big inexact->exact roundtrip"
  (lambda () (inexact->exact big-inexact)))

;;; ============================================================
;;; 8. Unboxing-relevant patterns
;;; ============================================================

(section "unboxing-relevant patterns")

;; When numeric unboxing is enabled, loop variables get unboxed to
;; LONGREAL. These tests verify that the results are correct regardless
;; of whether unboxing is active.

;; Simple numeric loop that the compiler might unbox
(test "unbox: sum 1..10"
  55
  (let loop ((i 1) (sum 0))
    (if (> i 10) sum
        (loop (+ i 1) (+ sum i)))))

;; Loop with multiplication (might overflow LONGREAL if unboxed wrongly)
(test "unbox: product 1..15"
  1307674368000
  (let loop ((i 1) (prod 1))
    (if (> i 15) prod
        (loop (+ i 1) (* prod i)))))

;; Loop where accumulator grows past 2^53 — if unboxed to LONGREAL,
;; precision loss occurs but result must still be a valid number
(define big-sum
  (let loop ((i 0) (acc 0))
    (if (= i 100)
        acc
        (loop (+ i 1) (+ acc (expt 2 53))))))

(test-true "big sum is number" (number? big-sum))
(test "big sum value"
  (* 100 (expt 2 53))
  big-sum)

;; Float loop — should stay as LONGREAL throughout
(test-approx "unbox: float accumulator"
  100.0
  (let loop ((i 0) (acc 0.0))
    (if (= i 100) acc
        (loop (+ i 1) (+ acc 1.0))))
  0.001)

;; Loop that mixes exact integers and produces a result that fits
;; in a fixnum — the compiler must not lose this to float
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

;;; ============================================================
;;; 9. Closures capturing numeric values
;;; ============================================================

(section "closures capturing numeric values")

;; Closure over fixnum
(define (make-adder n)
  (lambda (x) (+ n x)))

(test "closure over fixnum"  52 ((make-adder 10) 42))

;; Closure over bignum
(define add-big (make-adder (expt 2 100)))
(test "closure over bignum"
  (+ (expt 2 100) 1)
  (add-big 1))

;; Closure over rational
(define add-third (make-adder 1/3))
(test "closure over 1/3"    4/3  (add-third 1))
(test "closure over 1/3 v2" 2/3  (add-third 1/3))

;; Closure over float
(define add-pi (make-adder 3.14))
(test-approx "closure over float" 4.14 (add-pi 1.0) 1e-10)

;; Closure over complex
(define add-i (make-adder (make-rectangular 0 1)))
(test "closure over +i"
  (make-rectangular 3 1)
  (add-i 3))

;;; ============================================================
;;; 10. Higher-order numeric operations
;;; ============================================================

(section "higher-order numeric operations")

;; map over list of mixed types
(define mixed-nums (list 1 1/3 3.14 (expt 2 100)))

(test "map number? mixed"
  '(#t #t #t #t)
  (map number? mixed-nums))

(test "map exact? mixed"
  '(#t #t #f #t)
  (map exact? mixed-nums))

;; fold (reduce) with +
(define (fold-left f init lst)
  (if (null? lst) init
      (fold-left f (f init (car lst)) (cdr lst))))

(test "fold + over integers"
  15
  (fold-left + 0 '(1 2 3 4 5)))

(test "fold + over rationals"
  1
  (fold-left + 0 '(1/4 1/4 1/4 1/4)))

;; apply with mixed args
(test "apply + mixed"
  (+ 1 1/3 3.14)
  (apply + '(1 1/3 3.14)))

;;; ============================================================
;;; 11. Rational arithmetic in loops
;;; ============================================================

(section "rational arithmetic in loops")

;; Egyptian fraction expansion of 5/7
;; Greedy algorithm: subtract largest 1/n <= remainder
;; Uses exact integer ceiling: ceil(d/n) for exact rationals
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

;;; ============================================================
;;; 12. Complex arithmetic in loops
;;; ============================================================

(section "complex arithmetic in loops")

;; Repeated squaring of (1+i): (1+i)^2 = 2i, (2i)^2 = -4, etc.
(test "(1+i)^2 = 2i"
  (make-rectangular 0 2)
  (* (make-rectangular 1 1) (make-rectangular 1 1)))

(test "(1+i)^4 = -4"
  -4
  (let loop ((i 0) (z (make-rectangular 1 1)))
    (if (= i 2) z
        (loop (+ i 1) (* z z)))))

;; Sum of complex series: sum (1/k + i/k) for k=1..5
(test "complex series sum"
  (make-rectangular (+ 1 1/2 1/3 1/4 1/5) (+ 1 1/2 1/3 1/4 1/5))
  (let loop ((k 1) (acc 0))
    (if (> k 5) acc
        (loop (+ k 1) (+ acc (make-rectangular (/ 1 k) (/ 1 k)))))))

;;; ============================================================
;;; 13. Exactness preservation in compiled code
;;; ============================================================

(section "exactness preservation")

;; These are critical: the compiler must NOT convert exact values
;; to inexact through unboxing or constant folding.

(test-true  "1/3 stays exact through +"
  (exact? (+ 1/3 1/3)))

(test-true  "bignum stays exact through *"
  (exact? (* (expt 2 60) 2)))

(test-true  "integer stays exact through loop"
  (exact?
    (let loop ((i 0) (acc 0))
      (if (= i 10) acc
          (loop (+ i 1) (+ acc 1))))))

(test-true  "rational stays exact through loop"
  (exact?
    (let loop ((i 0) (acc 0))
      (if (= i 10) acc
          (loop (+ i 1) (+ acc 1/7))))))

(test-false "float stays inexact through loop"
  (exact?
    (let loop ((i 0) (acc 0.0))
      (if (= i 10) acc
          (loop (+ i 1) (+ acc 1.0))))))

;; Mixed: exact + inexact = inexact
(test-true "exact + inexact = inexact in loop"
  (inexact?
    (let loop ((i 0) (acc 0))
      (if (= i 10)
          (+ acc 0.5)   ;; inject inexact at the end
          (loop (+ i 1) (+ acc 1))))))

;;; ============================================================
;;; 14. Numeric comparison edge cases
;;; ============================================================

(section "numeric comparison edge cases")

;; Cross-type comparisons
(test-true  "(= 1 1.0)"          (= 1 1.0))
(test-true  "(= 1/2 0.5)"        (= 1/2 0.5))
(test-true  "(< 1/3 0.5)"        (< 1/3 0.5))
(test-true  "(< 1/3 1/2)"        (< 1/3 1/2))
(test-true  "(> (expt 2 100) (expt 2 99))" (> (expt 2 100) (expt 2 99)))

;; zero? on different types
(test-true  "zero? 0"            (zero? 0))
(test-true  "zero? 0.0"          (zero? 0.0))
(test-false "zero? 1/3"          (zero? 1/3))
(test-false "zero? 1"            (zero? 1))

;; negative? positive?
(test-true  "positive? 1/3"      (positive? 1/3))
(test-true  "negative? -1/3"     (negative? -1/3))
(test-false "negative? 0"        (negative? 0))

;;; ============================================================
;;; 15. Conversion functions
;;; ============================================================

(section "conversion functions")

(test       "exact->inexact 1/3"       (exact->inexact 1/3)  (exact->inexact 1/3))
(test-true  "inexact? (exact->inexact 1/3)" (inexact? (exact->inexact 1/3)))
(test       "exact->inexact 42"        42.0  (exact->inexact 42))
(test       "inexact->exact 42.0"      42    (inexact->exact 42.0))
(test-true  "exact? (inexact->exact 42.0)" (exact? (inexact->exact 42.0)))

;; numerator, denominator
(test "numerator 1/3"    1  (numerator 1/3))
(test "denominator 1/3"  3  (denominator 1/3))
(test "numerator 5"      5  (numerator 5))
(test "denominator 5"    1  (denominator 5))

;; real-part, imag-part
(test "real-part 3+4i"   3  (real-part (make-rectangular 3 4)))
(test "imag-part 3+4i"   4  (imag-part (make-rectangular 3 4)))
(test "real-part 5"      5  (real-part 5))
(test "imag-part 5"      0  (imag-part 5))

;; magnitude
(test "magnitude 3+4i"   5.0 (magnitude (make-rectangular 3 4)))
(test "magnitude -5"     5   (magnitude -5))

;;; ============================================================
;;; 16. Tail-recursive functions with mixed numerics
;;; ============================================================

(section "tail-recursive with mixed numerics")

;; GCD via Euclidean algorithm (works on exact integers and rationals)
(define (my-gcd a b)
  (if (= b 0) a
      (my-gcd b (remainder a b))))

(test "gcd 12 8"    4  (my-gcd 12 8))
(test "gcd 100 75"  25 (my-gcd 100 75))
(test "gcd 7 13"    1  (my-gcd 7 13))

;; Collatz sequence length
(define (collatz-length n)
  (let loop ((x n) (count 0))
    (if (= x 1) count
        (if (= (remainder x 2) 0)
            (loop (/ x 2) (+ count 1))
            (loop (+ (* 3 x) 1) (+ count 1))))))

(test "collatz 1"     0   (collatz-length 1))
(test "collatz 2"     1   (collatz-length 2))
(test "collatz 27"    111 (collatz-length 27))

;; Ackermann (small values, tests deep recursion + arithmetic)
(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(test "ack 0 0" 1   (ack 0 0))
(test "ack 1 1" 3   (ack 1 1))
(test "ack 2 2" 7   (ack 2 2))
(test "ack 3 3" 61  (ack 3 3))

;;; ============================================================
;;; 17. Numeric dispatch stress
;;; ============================================================

(section "numeric dispatch stress")

;; A function that exercises multiple numeric types in one call
(define (numeric-gauntlet x)
  (list (number? x)
        (exact? x)
        (inexact? x)
        (integer? x)
        (rational? x)
        (real? x)
        (complex? x)
        (zero? x)
        (positive? x)))

(test "gauntlet fixnum"
  '(#t #t #f #t #t #t #t #f #t)
  (numeric-gauntlet 42))

(test "gauntlet rational"
  '(#t #t #f #f #t #t #t #f #t)
  (numeric-gauntlet 1/3))

(test "gauntlet float"
  '(#t #f #t #f #f #t #t #f #t)
  (numeric-gauntlet 3.14))

(test "gauntlet zero"
  '(#t #t #f #t #t #t #t #t #f)
  (numeric-gauntlet 0))

;;; ============================================================
;;; 18. Interop roundtrip through compiler
;;; ============================================================

(section "interop roundtrip through compiler")

(test "rt INTEGER 42"     42     (interop-roundtrip 'INTEGER 42))
(test "rt CARDINAL 42"    42     (interop-roundtrip 'CARDINAL 42))
(test "rt LONGREAL 3.14"  3.14   (interop-roundtrip 'LONGREAL 3.14))
(test "rt TEXT hello"      "hello" (interop-roundtrip 'TEXT "hello"))
(test-true "rt BOOLEAN 42" (interop-roundtrip 'BOOLEAN 42))
(test-false "rt BOOLEAN #f" (interop-roundtrip 'BOOLEAN #f))
(test "rt REFANY 1/3"     1/3    (interop-roundtrip 'REFANY 1/3))
(test "rt Mpz_T big"
  (expt 2 100)
  (interop-roundtrip 'Mpz_T (expt 2 100)))
(test-no-error "rt Mpfr_T 3.14"
  (lambda () (interop-roundtrip 'Mpfr_T 3.14)))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "============================================================") (newline)
(display "Compiler numeric tests: ")
(display pass-count) (display "/") (display test-count) (display " passed")
(if (> fail-count 0)
    (begin (display ", ") (display fail-count) (display " FAILED"))
    (display ", all passed"))
(newline)
(display "============================================================") (newline)

(exit (if (= fail-count 0) 0 1))
