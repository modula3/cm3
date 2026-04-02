;;; test-cn-coverage.scm --- Fill coverage gaps across the numeric tower
;;; Tests string->number, number->string, predicates, comparisons,
;;; rounding, min/max, abs, etc. for types not covered elsewhere.

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "string->number: integers")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "s->n integer"         42      (string->number "42"))
(test "s->n negative"        -7      (string->number "-7"))
(test "s->n zero"            0       (string->number "0"))
(test-true "s->n int exact"  (exact? (string->number "42")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "string->number: bignums")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "s->n bignum"
  (expt 2 100)
  (string->number "1267650600228229401496703205376"))
(test-true "s->n bignum exact"
  (exact? (string->number "1267650600228229401496703205376")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "string->number: floats")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "s->n float"           3.14    (string->number "3.14"))
(test "s->n neg float"       -2.5    (string->number "-2.5"))
(test "s->n sci"             1000.0  (string->number "1e3"))
(test-false "s->n float exact" (exact? (string->number "3.14")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "string->number: rationals")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "s->n 1/3"             1/3     (string->number "1/3"))
(test "s->n -7/4"            -7/4    (string->number "-7/4"))
(test "s->n 22/7"            22/7    (string->number "22/7"))
(test "s->n 4/2 demotes"     2       (string->number "4/2"))
(test-true "s->n rational exact" (exact? (string->number "1/3")))
(test-true "s->n rational?" (rational? (string->number "1/3")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "string->number: complex")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "s->n 3+4i"            3+4i    (string->number "3+4i"))
(test "s->n 3-4i"            3-4i    (string->number "3-4i"))
(test "s->n +i"              +i      (string->number "+i"))
(test "s->n -i"              -i      (string->number "-i"))
(test "s->n 0+1i"            +i      (string->number "0+1i"))
(test-true "s->n complex?"   (complex? (string->number "3+4i")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "string->number: dual")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "s->n 3+4eps"          3+4eps  (string->number "3+4eps"))
(test "s->n 3-4eps"          3-4eps  (string->number "3-4eps"))
(test "s->n 5eps"            5eps    (string->number "5eps"))
(test "s->n -1eps"           -1eps   (string->number "-1eps"))
(test-true "s->n dual?"      (dual? (string->number "3+4eps")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "string->number: special floats")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-true "s->n +inf.0"    (= +inf.0 (string->number "+inf.0")))
(test-true "s->n -inf.0"    (= -inf.0 (string->number "-inf.0")))
(test-true "s->n +nan.0 is number" (number? (string->number "+nan.0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "string->number: bad input")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "s->n bad"             #f      (string->number "hello"))
(test "s->n empty"           #f      (string->number ""))
(test "s->n just sign"       #f      (string->number "+"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "string->number: radix")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "s->n hex"             255     (string->number "FF" 16))
(test "s->n octal"           63      (string->number "77" 8))
(test "s->n binary"          10      (string->number "1010" 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "number->string: all types")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "n->s integer"         "42"    (number->string 42))
(test "n->s negative"        "-7"    (number->string -7))
(test "n->s rational"        "1/3"   (number->string 1/3))
(test "n->s neg rational"    "-7/4"  (number->string -7/4))
(test "n->s complex"         "3+4i"  (number->string 3+4i))
(test "n->s neg imag"        "3-4i"  (number->string 3-4i))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "predicates on duals")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-true  "number? dual"   (number? 3+4eps))
(test-true  "dual? dual"     (dual? 3+4eps))
(test-false "real? dual"     (real? 3+4eps))
(test-false "rational? dual" (rational? 3+4eps))
(test-false "integer? dual"  (integer? 3+4eps))
(test-false "complex? dual"  (complex? 3+4eps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "predicates on special floats")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-true  "number? +inf.0"   (number? +inf.0))
(test-false "exact? +inf.0"    (exact? +inf.0))
(test-true  "inexact? +inf.0"  (inexact? +inf.0))
(test-true  "real? +inf.0"     (real? +inf.0))
(test-true  "number? +nan.0"   (number? +nan.0))
(test-false "integer? +inf.0"  (integer? +inf.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "predicates on bignums")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-true  "number? bignum"   (number? (expt 2 100)))
(test-true  "integer? bignum"  (integer? (expt 2 100)))
(test-true  "exact? bignum"    (exact? (expt 2 100)))
(test-true  "rational? bignum" (rational? (expt 2 100)))
(test-true  "real? bignum"     (real? (expt 2 100)))
(test-false "inexact? bignum"  (inexact? (expt 2 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "positive?/negative?/zero? coverage")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bignums
(test-true  "positive? bignum"  (positive? (expt 2 100)))
(test-false "negative? bignum"  (negative? (expt 2 100)))
(test-true  "negative? -bignum" (negative? (- (expt 2 100))))
(test-false "zero? bignum"      (zero? (expt 2 100)))

;; special floats
(test-true  "positive? +inf.0"  (positive? +inf.0))
(test-true  "negative? -inf.0"  (negative? -inf.0))
(test-false "zero? +inf.0"      (zero? +inf.0))
(test-false "positive? -inf.0"  (positive? -inf.0))

;; rationals
(test-true  "positive? 1/3"     (positive? 1/3))
(test-true  "negative? -1/3"    (negative? -1/3))
(test-false "zero? 1/3"         (zero? 1/3))

;; dual errors
(test-error "positive? dual" (lambda () (positive? 3+4eps)))
(test-error "negative? dual" (lambda () (negative? 3+4eps)))

;; complex errors
(test-error "positive? complex" (lambda () (positive? 3+4i)))
(test-error "negative? complex" (lambda () (negative? 3+4i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "abs coverage")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "abs bignum"           (expt 2 100)  (abs (expt 2 100)))
(test "abs -bignum"          (expt 2 100)  (abs (- (expt 2 100))))
(test "abs rational"         1/3           (abs 1/3))
(test "abs -rational"        1/3           (abs -1/3))
(test-true "abs exact preserves" (exact? (abs -1/3)))
(test-true "abs +inf.0"     (= +inf.0 (abs -inf.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "min/max coverage")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "min rationals"        1/4           (min 1/3 1/2 1/4))
(test "max rationals"        1/2           (max 1/3 1/2 1/4))
(test "min mixed rat/int"    1/3           (min 1 1/3 2))
(test "max mixed rat/float"  3.14          (max 1/3 3.14 2))
(test-true "min exact preserves" (exact? (min 1/3 1/2)))
(test-false "min mixed inexact"  (exact? (min 1/3 0.5)))
(test "min single"           42            (min 42))
(test "max single"           42            (max 42))
(test-true "min +inf.0"     (= 1 (min 1 +inf.0)))
(test-true "max +inf.0"     (= +inf.0 (max 1 +inf.0)))
(test-true "min -inf.0"     (= -inf.0 (min 1 -inf.0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "floor/ceiling/round/truncate on rationals")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "floor 7/3"            2       (floor 7/3))
(test "floor -7/3"           -3      (floor -7/3))
(test "ceiling 7/3"          3       (ceiling 7/3))
(test "ceiling -7/3"         -2      (ceiling -7/3))
(test "round 5/2"            2       (round 5/2))
(test "round 7/2"            4       (round 7/2))
(test "truncate 7/3"         2       (truncate 7/3))
(test "truncate -7/3"        -2      (truncate -7/3))
(test-true "floor rational exact"    (exact? (floor 7/3)))
(test-true "ceiling rational exact"  (exact? (ceiling 7/3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "floor/ceiling/round/truncate on integers (no-op)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "floor 5"              5       (floor 5))
(test "ceiling 5"            5       (ceiling 5))
(test "round 5"              5       (round 5))
(test "truncate 5"           5       (truncate 5))
(test "floor bignum"         (expt 2 100) (floor (expt 2 100)))
(test-true "floor int exact" (exact? (floor 5)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "comparisons: mixed types")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-true  "(< 1/3 0.5)"      (< 1/3 0.5))
(test-true  "(> 0.5 1/3)"      (> 0.5 1/3))
(test-true  "(= 1/2 0.5)"      (= 1/2 0.5))
(test-true  "(< 1 3/2)"        (< 1 3/2))
(test-true  "(> 3/2 1)"        (> 3/2 1))
(test-true  "(< bignum bignum+1)"
  (< (expt 2 100) (+ (expt 2 100) 1)))
(test-true  "(< 1 +inf.0)"     (< 1 +inf.0))
(test-true  "(> +inf.0 1e308)"  (> +inf.0 1e308))
(test-true  "(< -inf.0 -1e308)" (< -inf.0 -1e308))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "gcd/lcm on bignums")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "gcd bignums"
  (expt 2 50)
  (gcd (expt 2 100) (expt 2 50)))
(test "gcd negative"         6 (gcd -12 18))
(test "gcd zero"             5 (gcd 0 5))
(test "lcm basic"            12 (lcm 4 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "quotient/remainder/modulo on bignums")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "quotient big"
  (expt 2 50)
  (quotient (expt 2 100) (expt 2 50)))
(test "remainder big"
  0
  (remainder (expt 2 100) (expt 2 50)))
(test "modulo negative"      2 (modulo -3 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "numerator/denominator edge cases")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test "numerator bignum"     (expt 2 100) (numerator (expt 2 100)))
(test "denominator bignum"   1            (denominator (expt 2 100)))
(test "numerator 0"          0            (numerator 0))
(test "denominator 0"        1            (denominator 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(section "exact->inexact / inexact->exact coverage")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; rational round-trip
(test-approx "exact->inexact 1/3"
  0.3333333333333333 (exact->inexact 1/3) 1e-15)
(test-true "exact->inexact rational is inexact"
  (inexact? (exact->inexact 1/3)))

;; bignum
(test-true "exact->inexact bignum is inexact"
  (inexact? (exact->inexact (expt 2 100))))

;; inexact->exact on float
(test "inexact->exact 0.5"  1/2  (inexact->exact 0.5))
(test-true "inexact->exact is exact"
  (exact? (inexact->exact 0.5)))
(test "inexact->exact 0.25" 1/4  (inexact->exact 0.25))


(test-summary "CN-COVERAGE")
