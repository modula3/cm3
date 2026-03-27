;;; test-numeric-tower.scm --- Phase 1 numeric tower tests
;;;
;;; Tests for exact integer support (fixnum + bignum).
;;; These tests define the target behavior for Phase 1 of the
;;; numeric tower implementation.  Many will fail against the
;;; current (LONGREAL-only) MScheme; they should be fixed
;;; incrementally as the implementation progresses.
;;;
;;; Usage: mscheme test-numeric-tower.scm

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
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

;;; ============================================================
;;; 1. Exact/inexact distinction — predicates
;;; ============================================================

(section "exact/inexact predicates")

;; exact? on integer literals
(test-true  "exact? 0"           (exact? 0))
(test-true  "exact? 1"           (exact? 1))
(test-true  "exact? -1"          (exact? -1))
(test-true  "exact? 42"          (exact? 42))
(test-true  "exact? 1000000"     (exact? 1000000))

;; inexact? on float literals
(test-true  "inexact? 0.0"       (inexact? 0.0))
(test-true  "inexact? 1.0"       (inexact? 1.0))
(test-true  "inexact? 3.14"      (inexact? 3.14))
(test-true  "inexact? -2.5"      (inexact? -2.5))
(test-true  "inexact? 1e10"      (inexact? 1e10))

;; cross-checks
(test-false "inexact? 42"        (inexact? 42))
(test-false "exact? 3.14"        (exact? 3.14))

;;; ============================================================
;;; 2. Number type predicates
;;; ============================================================

(section "number type predicates")

;; number? covers both exact and inexact
(test-true  "number? 42"         (number? 42))
(test-true  "number? 3.14"       (number? 3.14))
(test-false "number? \"hello\""  (number? "hello"))
(test-false "number? #t"         (number? #t))

;; integer? on exact integers
(test-true  "integer? 0"         (integer? 0))
(test-true  "integer? 42"        (integer? 42))
(test-true  "integer? -7"        (integer? -7))

;; integer? on integer-valued floats
(test-true  "integer? 1.0"       (integer? 1.0))
(test-true  "integer? -0.0"      (integer? -0.0))

;; integer? on non-integer floats
(test-false "integer? 3.14"      (integer? 3.14))
(test-false "integer? 0.5"       (integer? 0.5))

;; real?, rational?, complex? all equal number? in Phase 1
(test-true  "real? 42"           (real? 42))
(test-true  "real? 3.14"         (real? 3.14))
(test-true  "rational? 42"       (rational? 42))
(test-true  "complex? 42"        (complex? 42))
(test-true  "complex? 3.14"      (complex? 3.14))

;;; ============================================================
;;; 3. Printing — exact integers print without decimal point
;;; ============================================================

(section "printing")

;; R5RS: (number->string 42) => "42", not "42.0"
(test "display 0"         "0"      (number->string 0))
(test "display 42"        "42"     (number->string 42))
(test "display -7"        "-7"     (number->string -7))
(test "display 1000000"   "1000000" (number->string 1000000))

;; Inexact numbers keep decimal representation
(test "display 0.0"       "0.0"    (number->string 0.0))

;;; ============================================================
;;; 4. eqv? exactness semantics
;;; ============================================================

(section "eqv? exactness")

;; Same type, same value
(test-true  "eqv? 1 1"           (eqv? 1 1))
(test-true  "eqv? 0 0"           (eqv? 0 0))
(test-true  "eqv? -1 -1"         (eqv? -1 -1))
(test-true  "eqv? 1.0 1.0"       (eqv? 1.0 1.0))

;; Different exactness => #f (R5RS 6.1)
(test-false "eqv? 1 1.0"         (eqv? 1 1.0))
(test-false "eqv? 0 0.0"         (eqv? 0 0.0))

;;; ============================================================
;;; 5. Arithmetic preserves exactness
;;; ============================================================

(section "arithmetic exactness preservation")

;; Exact + exact => exact
(test-true  "exact? (+ 1 2)"         (exact? (+ 1 2)))
(test-true  "exact? (- 10 3)"        (exact? (- 10 3)))
(test-true  "exact? (* 6 7)"         (exact? (* 6 7)))
(test       "(+ 1 2) = 3"        3   (+ 1 2))
(test       "(- 10 3) = 7"       7   (- 10 3))
(test       "(* 6 7) = 42"       42  (* 6 7))

;; Inexact + inexact => inexact
(test-true  "inexact? (+ 1.0 2.0)"   (inexact? (+ 1.0 2.0)))

;; Mixed: exact + inexact => inexact (contagion)
(test-true  "inexact? (+ 1 2.0)"     (inexact? (+ 1 2.0)))
(test-true  "inexact? (+ 1.0 2)"     (inexact? (+ 1.0 2)))
(test-true  "inexact? (* 2 3.0)"     (inexact? (* 2 3.0)))

;;; ============================================================
;;; 6. Exact integer arithmetic — basic operations
;;; ============================================================

(section "exact integer arithmetic")

(test "(+ 0 0)"           0       (+ 0 0))
(test "(+ 1 0)"           1       (+ 1 0))
(test "(+ -1 1)"          0       (+ -1 1))
(test "(* 0 100)"         0       (* 0 100))
(test "(* 1 42)"          42      (* 1 42))
(test "(* -1 42)"         -42     (* -1 42))
(test "(- 0)"             0       (- 0))
(test "(- 42)"            -42     (- 42))
(test "(- -42)"           42      (- -42))

;; Multi-arg
(test "(+ 1 2 3 4 5)"     15      (+ 1 2 3 4 5))
(test "(* 1 2 3 4 5)"     120     (* 1 2 3 4 5))
(test "(- 10 1 2 3)"      4       (- 10 1 2 3))

;; Identity elements
(test "(+)"               0       (+))
(test "(*)"               1       (*))

;;; ============================================================
;;; 7. Division semantics
;;; ============================================================

(section "division")

;; Exact division with zero remainder => exact
(test       "(/ 6 3) = 2"        2   (/ 6 3))
(test-true  "exact? (/ 6 3)"         (exact? (/ 6 3)))
(test       "(/ 42 7) = 6"       6   (/ 42 7))
(test       "(/ -12 4) = -3"     -3  (/ -12 4))
(test       "(/ 0 5) = 0"        0   (/ 0 5))

;; Non-exact division => inexact (Phase 1, no rationals)
(test-true  "inexact? (/ 1 3)"       (inexact? (/ 1 3)))
(test-true  "inexact? (/ 2 3)"       (inexact? (/ 2 3)))

;; quotient, remainder, modulo — exact integer only
(test "quotient 7 3"      2       (quotient 7 3))
(test "quotient -7 3"     -2      (quotient -7 3))
(test "quotient 7 -3"     -2      (quotient 7 -3))
(test "quotient -7 -3"    2       (quotient -7 -3))

(test "remainder 7 3"     1       (remainder 7 3))
(test "remainder -7 3"    -1      (remainder -7 3))
(test "remainder 7 -3"    1       (remainder 7 -3))
(test "remainder -7 -3"   -1      (remainder -7 -3))

(test "modulo 7 3"        1       (modulo 7 3))
(test "modulo -7 3"       2       (modulo -7 3))
(test "modulo 7 -3"       -2      (modulo 7 -3))
(test "modulo -7 -3"      -1      (modulo -7 -3))

;;; ============================================================
;;; 8. gcd and lcm
;;; ============================================================

(section "gcd / lcm")

(test "gcd 12 8"          4       (gcd 12 8))
(test "gcd 0 5"           5       (gcd 0 5))
(test "gcd 5 0"           5       (gcd 5 0))
(test "gcd 0 0"           0       (gcd 0 0))
(test "gcd -12 8"         4       (gcd -12 8))
(test "gcd 12 -8"         4       (gcd 12 -8))
(test-true "exact? gcd"           (exact? (gcd 12 8)))

(test "lcm 4 6"           12      (lcm 4 6))
(test "lcm 0 5"           0       (lcm 0 5))
(test "lcm 5 0"           0       (lcm 5 0))
(test-true "exact? lcm"           (exact? (lcm 4 6)))

;;; ============================================================
;;; 9. Comparisons across exactness
;;; ============================================================

(section "comparison across exactness")

;; Numeric equality (=) ignores exactness
(test-true  "(= 1 1)"            (= 1 1))
(test-true  "(= 1 1.0)"          (= 1 1.0))
(test-true  "(= 1.0 1)"          (= 1.0 1))
(test-false "(= 1 2)"            (= 1 2))

;; Ordering
(test-true  "(< 1 2)"            (< 1 2))
(test-true  "(< 1 2.0)"          (< 1 2.0))
(test-true  "(< 1.0 2)"          (< 1.0 2))
(test-false "(< 2 1)"            (< 2 1))
(test-true  "(<= 1 1)"           (<= 1 1))
(test-true  "(<= 1 1.0)"         (<= 1 1.0))
(test-true  "(> 2 1)"            (> 2 1))
(test-true  "(>= 1 1)"           (>= 1 1))

;;; ============================================================
;;; 10. exact->inexact / inexact->exact
;;; ============================================================

(section "exact->inexact / inexact->exact")

(test       "exact->inexact 42"       42.0  (exact->inexact 42))
(test-true  "inexact? exact->inexact"       (inexact? (exact->inexact 42)))
(test       "inexact->exact 42.0"     42    (inexact->exact 42.0))
(test-true  "exact? inexact->exact"         (exact? (inexact->exact 42.0)))

;; Round-trip
(test-true  "exact->inexact->exact int"
            (exact? (inexact->exact (exact->inexact 7))))
(test       "exact->inexact->exact value"
            7 (inexact->exact (exact->inexact 7)))

;;; ============================================================
;;; 11. Bit-boundary rollover tests
;;; ============================================================

(section "bit-boundary rollover")

;; ---- 31-bit boundary (2^30 = 1073741824) ----
;; This is the fixnum limit on 32-bit Schemes.
;; On MScheme (64-bit INTEGER), these should stay fixnum.

(define 2^30 (expt 2 30))
(define 2^31 (expt 2 31))

(test       "2^30"                1073741824          2^30)
(test-true  "exact? 2^30"        (exact? 2^30))
(test       "2^30 - 1"           1073741823          (- 2^30 1))
(test       "2^30 + 2^30"        2147483648          (+ 2^30 2^30))
(test-true  "exact? 2^30+2^30"   (exact? (+ 2^30 2^30)))

;; ---- 32-bit boundary (2^31 = 2147483648) ----
;; Unsigned 32-bit max is 2^32-1 = 4294967295.

(test       "2^31"                2147483648          2^31)
(test-true  "exact? 2^31"        (exact? 2^31))
(test       "2^31 - 1"           2147483647          (- 2^31 1))
(test       "2^32 - 1"           4294967295          (- (expt 2 32) 1))
(test       "2^31 * 2"           4294967296          (* 2^31 2))
(test-true  "exact? 2^31*2"      (exact? (* 2^31 2)))

;; Negative side of 32-bit
(test       "-2^31"               -2147483648         (- 2^31))
(test       "-2^31 - 1"          -2147483649         (- (- 2^31) 1))
(test-true  "exact? -2^31-1"     (exact? (- (- 2^31) 1)))

;; ---- 63-bit boundary (2^62, near fixnum limit on 64-bit) ----
;; On 64-bit MScheme, INTEGER range is [-2^63, 2^63-1].
;; 2^62 is the largest power of 2 where 2*x still fits in INTEGER.

(define 2^62 (expt 2 62))
(define 2^63 (expt 2 63))

(test       "2^62"                4611686018427387904  2^62)
(test-true  "exact? 2^62"        (exact? 2^62))
(test       "2^62 + 2^62 = 2^63" 2^63                 (+ 2^62 2^62))
(test-true  "exact? 2^62+2^62"   (exact? (+ 2^62 2^62)))

;; 2^63 - 1 = LAST(INTEGER) on 64-bit = 9223372036854775807
(test       "2^63 - 1"           9223372036854775807  (- 2^63 1))
(test-true  "exact? 2^63-1"      (exact? (- 2^63 1)))

;; ---- 64-bit boundary: overflow to bignum ----
;; 2^63 itself exceeds LAST(INTEGER), must be a bignum.

(test       "2^63"                9223372036854775808  2^63)
(test-true  "exact? 2^63"        (exact? 2^63))

;; Addition that crosses the 64-bit boundary
(test       "2^63-1 + 1 = 2^63"  2^63                 (+ (- 2^63 1) 1))
(test-true  "exact? 2^63-1+1"    (exact? (+ (- 2^63 1) 1)))

;; 2^63 + 2^63 = 2^64
(test       "2^63 + 2^63 = 2^64" (expt 2 64)          (+ 2^63 2^63))
(test-true  "exact? 2^64"        (exact? (expt 2 64)))
(test       "2^64"                18446744073709551616  (expt 2 64))

;; Multiplication that overflows 64-bit
(test       "2^62 * 4 = 2^64"    (expt 2 64)          (* 2^62 4))
(test-true  "exact? 2^62*4"      (exact? (* 2^62 4)))

;; ---- Negative 64-bit boundary ----
;; FIRST(INTEGER) on 64-bit = -2^63 = -9223372036854775808

(test       "-2^63"               -9223372036854775808 (- 2^63))
(test-true  "exact? -2^63"       (exact? (- 2^63)))
(test       "-2^63 - 1"          -9223372036854775809 (- (- 2^63) 1))
(test-true  "exact? -2^63-1"     (exact? (- (- 2^63) 1)))

;; ---- Very large exact integers ----

(test-true  "exact? 2^100"       (exact? (expt 2 100)))
(test       "2^100"
            1267650600228229401496703205376
            (expt 2 100))

(test-true  "exact? 2^100 + 1"   (exact? (+ (expt 2 100) 1)))
(test       "2^100 + 1"
            1267650600228229401496703205377
            (+ (expt 2 100) 1))

;; 2^100 - 2^100 should demote back to fixnum (zero)
(test       "2^100 - 2^100 = 0"  0   (- (expt 2 100) (expt 2 100)))
(test-true  "exact? 2^100-2^100" (exact? (- (expt 2 100) (expt 2 100))))

;; ---- Multiplication stress: known products ----
(test       "2^32 * 2^32 = 2^64"
            (expt 2 64)
            (* (expt 2 32) (expt 2 32)))
(test       "2^50 * 2^50 = 2^100"
            (expt 2 100)
            (* (expt 2 50) (expt 2 50)))

;; ---- Subtraction across zero with bignums ----
(test       "-(2^100) + 2^100 = 0"
            0
            (+ (- (expt 2 100)) (expt 2 100)))

;;; ============================================================
;;; 11b. LONGREAL precision limits
;;; ============================================================

(section "LONGREAL precision limits")

;; IEEE 754 double has 53-bit mantissa.
;; Exact integer range: [-2^53, 2^53].
;; Beyond 2^53, consecutive integers are not representable.

;; 2^53 = 9007199254740992
(define 2^53 (expt 2 53))

;; exact->inexact should preserve value up to 2^53
(test       "exact->inexact 2^53"
            9007199254740992.0
            (exact->inexact 2^53))

;; But 2^53 + 1 loses precision when converted to LONGREAL:
;; FLOAT(2^53 + 1, LONGREAL) = 2^53 (rounds down)
;; This is inherent in IEEE 754, not a bug.
(test       "inexact 2^53+1 = inexact 2^53"
            (exact->inexact 2^53)
            (exact->inexact (+ 2^53 1)))

;; Exact integers preserve the distinction that LONGREAL cannot
(test-false "2^53 /= 2^53+1 (exact)"
            (= 2^53 (+ 2^53 1)))
(test-true  "exact? 2^53+1"
            (exact? (+ 2^53 1)))

;; 2^53 + 2 IS representable as a double (it's even)
(test       "exact->inexact 2^53+2"
            9007199254740994.0
            (exact->inexact (+ 2^53 2)))

;; Larger values: 2^60 is exact as a double (power of 2)
(test       "exact->inexact 2^60"
            1152921504606846976.0
            (exact->inexact (expt 2 60)))

;; But 2^60 + 1 is NOT representable — rounds to 2^60
(test       "inexact 2^60+1 = inexact 2^60"
            (exact->inexact (expt 2 60))
            (exact->inexact (+ (expt 2 60) 1)))

;; Again, exact integers distinguish these
(test-false "2^60 /= 2^60+1 (exact)"
            (= (expt 2 60) (+ (expt 2 60) 1)))

;; Round-trip: inexact->exact(exact->inexact(2^53+1)) loses the +1
(test       "round-trip loses 2^53+1"
            2^53
            (inexact->exact (exact->inexact (+ 2^53 1))))

;; But round-trip for small integers is lossless
(test       "round-trip 12345"
            12345
            (inexact->exact (exact->inexact 12345)))

;; Factorial of 30 is exact, and much larger than 2^53
;; 30! = 265252859812191058636308480000000
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(test-true  "exact? 30!"         (exact? (factorial 30)))
(test       "30!"
            265252859812191058636308480000000
            (factorial 30))

;; 30! converted to inexact loses precision
;; but the exact value is preserved in the exact domain
(test-false "30! /= inexact->exact(exact->inexact 30!)"
            (= (factorial 30)
               (inexact->exact (exact->inexact (factorial 30)))))

;;; ============================================================
;;; 11c. Infinity and NaN
;;; ============================================================

(section "infinity and NaN")

;; IEEE 754 special values should be inexact numbers
(define +inf (/ 1.0 0.0))
(define -inf (/ -1.0 0.0))
(define nan  (/ 0.0 0.0))

(test-true  "number? +inf"       (number? +inf))
(test-true  "number? -inf"       (number? -inf))
(test-true  "number? nan"        (number? nan))

(test-true  "inexact? +inf"      (inexact? +inf))
(test-true  "inexact? -inf"      (inexact? -inf))
(test-true  "inexact? nan"       (inexact? nan))

(test-false "exact? +inf"        (exact? +inf))
(test-false "exact? -inf"        (exact? -inf))
(test-false "exact? nan"         (exact? nan))

;; integer? should be false for infinities and NaN
(test-false "integer? +inf"      (integer? +inf))
(test-false "integer? -inf"      (integer? -inf))
(test-false "integer? nan"       (integer? nan))

;; Arithmetic with infinities
(test-true  "+inf + 1 = +inf"    (= (+ +inf 1) +inf))
(test-true  "+inf + 1.0 = +inf"  (= (+ +inf 1.0) +inf))
(test-true  "-inf - 1 = -inf"    (= (- -inf 1) -inf))
(test-true  "+inf * 2 = +inf"    (= (* +inf 2) +inf))
(test-true  "+inf + -inf = nan"  (not (= (+ +inf -inf) (+ +inf -inf))))

;; NaN is not equal to itself
(test-false "nan = nan"          (= nan nan))
(test-false "eqv? nan nan"       (eqv? nan nan))

;; Comparisons with infinities
(test-true  "1 < +inf"           (< 1 +inf))
(test-true  "-inf < 1"           (< -inf 1))
(test-true  "-inf < +inf"        (< -inf +inf))
(test-false "nan < 1"            (< nan 1))
(test-false "1 < nan"            (< 1 nan))

;; Division producing infinities
(test-true  "1/0.0 = +inf"       (= (/ 1.0 0.0) +inf))
(test-true  "-1/0.0 = -inf"      (= (/ -1.0 0.0) -inf))

;; R7RS read syntax for specials
;; NOTE: these will fail until the reader is updated
;; (test-true  "read +inf.0"  (= +inf.0 (/ 1.0 0.0)))
;; (test-true  "read -inf.0"  (= -inf.0 (/ -1.0 0.0)))
;; (test-true  "read +nan.0"  (not (= +nan.0 +nan.0)))

;; Printer round-trip: write then read should recover specials
;; (Requires R7RS output: +inf.0, -inf.0, +nan.0)
;; These are commented out until both reader and printer are updated.
;; (test-true  "round-trip +inf"
;;             (= (string->number (number->string +inf)) +inf))
;; (test-true  "round-trip -inf"
;;             (= (string->number (number->string -inf)) -inf))

;;; ============================================================
;;; 11d. Rounding procedures
;;; ============================================================

(section "rounding procedures")

;; floor — toward negative infinity
(test       "floor 2.7"      2.0    (floor 2.7))
(test       "floor -2.7"     -3.0   (floor -2.7))
(test       "floor 2.0"      2.0    (floor 2.0))
(test       "floor -2.0"     -2.0   (floor -2.0))

;; ceiling — toward positive infinity
(test       "ceiling 2.7"    3.0    (ceiling 2.7))
(test       "ceiling -2.7"   -2.0   (ceiling -2.7))
(test       "ceiling 2.0"    2.0    (ceiling 2.0))

;; truncate — toward zero
(test       "truncate 2.7"   2.0    (truncate 2.7))
(test       "truncate -2.7"  -2.0   (truncate -2.7))
(test       "truncate 2.0"   2.0    (truncate 2.0))

;; round — to nearest even (banker's rounding, R5RS 6.2.5)
;; NOTE: M3's ROUND uses half-away-from-zero, so these
;; four half-integer tests fail pre-tower.  Pre-existing bug.
(test       "round 2.5"      2.0    (round 2.5))
(test       "round 3.5"      4.0    (round 3.5))
(test       "round 2.7"      3.0    (round 2.7))
(test       "round -2.5"     -2.0   (round -2.5))
(test       "round -3.5"     -4.0   (round -3.5))
(test       "round 0.5"      0.0    (round 0.5))
(test       "round 1.5"      2.0    (round 1.5))
(test       "round -0.5"     0.0    (round -0.5))

;; Rounding on exact integers should be identity
;; (these are exact->exact, no change)
(test       "floor 2"        2      (floor 2))
(test       "ceiling 2"      2      (ceiling 2))
(test       "truncate 2"     2      (truncate 2))
(test       "round 2"        2      (round 2))
(test-true  "exact? floor exact"    (exact? (floor 2)))
(test-true  "exact? round exact"    (exact? (round 2)))

;; Rounding on inexact returns inexact
(test-true  "inexact? floor inexact"   (inexact? (floor 2.7)))
(test-true  "inexact? round inexact"   (inexact? (round 2.7)))

;;; ============================================================
;;; 12. Fixnum cache identity (optional, implementation detail)
;;; ============================================================

(section "small integer identity (eq?)")

;; Small integers in the cached range should be eq?
;; This is an implementation detail, not R5RS-required,
;; but verifies the cache works.
(test-true  "eq? 0 0"            (eq? 0 0))
(test-true  "eq? 1 (+ 0 1)"     (eq? 1 (+ 0 1)))
(test-true  "eq? -1 (- 0 1)"    (eq? -1 (- 0 1)))
(test-true  "eq? 100 (* 10 10)"  (eq? 100 (* 10 10)))

;;; ============================================================
;;; 13. Reader: exactness prefixes
;;; ============================================================

(section "reader exactness prefixes")

;; #e forces exact
(test-true  "exact? #e1"         (exact? #e1))
(test-true  "exact? #e1.0"       (exact? #e1.0))
(test       "#e1.0 = 1"      1   #e1.0)

;; #i forces inexact
(test-true  "inexact? #i1"       (inexact? #i1))
(test       "#i1 = 1.0"      1.0 #i1)

;;; ============================================================
;;; 14. Reader: non-decimal bases produce exact integers
;;; ============================================================

(section "non-decimal base literals")

(test-true  "exact? #b1010"      (exact? #b1010))
(test       "#b1010 = 10"    10  #b1010)
(test-true  "exact? #o17"        (exact? #o17))
(test       "#o17 = 15"      15  #o17)
(test-true  "exact? #xFF"        (exact? #xFF))
(test       "#xFF = 255"     255 #xFF)
(test-true  "exact? #d42"        (exact? #d42))
(test       "#d42 = 42"      42  #d42)

;;; ============================================================
;;; 15. number->string / string->number with exact integers
;;; ============================================================

(section "number->string / string->number")

(test "number->string 0"     "0"       (number->string 0))
(test "number->string 42"    "42"      (number->string 42))
(test "number->string -7"    "-7"      (number->string -7))

;; string->number should return exact for integer strings
(test       "string->number \"42\""   42    (string->number "42"))
(test-true  "exact? string->number"         (exact? (string->number "42")))

;; string->number with radix
(test "string->number hex"    255     (string->number "FF" 16))
(test "string->number oct"    8       (string->number "10" 8))
(test "string->number bin"    5       (string->number "101" 2))

;; string->number for floats stays inexact
(test-true  "inexact? string->number float"
            (inexact? (string->number "3.14")))

;;; ============================================================
;;; 16. Edge cases
;;; ============================================================

(section "edge cases")

;; Zero is exact
(test-true  "exact? 0"           (exact? 0))
(test-true  "integer? 0"         (integer? 0))

;; Negative zero — 0 and -0 are the same exact integer
(test-true  "eqv? 0 -0"          (eqv? 0 -0))

;; But 0.0 and -0.0 are both inexact
(test-true  "inexact? 0.0"       (inexact? 0.0))
(test-true  "inexact? -0.0"      (inexact? -0.0))

;; max/min preserve exactness
(test-true  "exact? max exact"    (exact? (max 1 2 3)))
(test-true  "inexact? max mixed"  (inexact? (max 1 2.0 3)))
(test       "max 1 2 3"      3   (max 1 2 3))
(test       "min 1 2 3"      1   (min 1 2 3))

;; abs preserves exactness
(test-true  "exact? abs exact"    (exact? (abs -42)))
(test       "abs -42"         42  (abs -42))
(test-true  "inexact? abs inex"   (inexact? (abs -3.14)))

;;; ============================================================
;;; 17. Error cases
;;; ============================================================

(section "error cases")

;; Type errors in arithmetic
(test-error "+(1,\"a\")"          (lambda () (+ 1 "hello")))
(test-error "*(1,#t)"            (lambda () (* 1 #t)))
(test-error "-(\"a\")"           (lambda () (- "hello")))

;; Division by exact zero should error
(test-error "(/ 1 0)"            (lambda () (/ 1 0)))
(test-error "(quotient 7 0)"     (lambda () (quotient 7 0)))
(test-error "(remainder 7 0)"    (lambda () (remainder 7 0)))
(test-error "(modulo 7 0)"       (lambda () (modulo 7 0)))

;; string->number on non-numeric strings returns #f, not error
(test       "string->number bad"  #f  (string->number "abc"))
(test       "string->number \"\""  #f  (string->number ""))

;; Comparison type errors
(test-error "(< 1 \"a\")"        (lambda () (< 1 "hello")))
(test-error "(= \"a\" 1)"        (lambda () (= "hello" 1)))

;;; ============================================================
;;; 18. Negative bignum arithmetic
;;; ============================================================

(section "negative bignum arithmetic")

(define big  (expt 2 100))
(define -big (- big))

(test       "-2^100"
            -1267650600228229401496703205376
            -big)
(test-true  "exact? -2^100"       (exact? -big))

;; Addition
(test       "-2^100 + 1"
            -1267650600228229401496703205375
            (+ -big 1))
(test       "-2^100 + 2^100 = 0"  0  (+ -big big))

;; Subtraction
(test       "-2^100 - 1"
            -1267650600228229401496703205377
            (- -big 1))

;; Multiplication
(test-true  "exact? -2^100 * -2^100"  (exact? (* -big -big)))
(test       "-2^100 * -1 = 2^100"     big  (* -big -1))
(test       "-2^100 * 2 = -2^101"
            (- (expt 2 101))
            (* -big 2))

;; Sign of bignum results
(test-true  "-big * -big > 0"     (> (* -big -big) 0))
(test-true  "-big * big < 0"      (< (* -big big) 0))

;; Division with negative bignums
(test       "-2^100 / 2 = -2^99"
            (- (expt 2 99))
            (/ -big 2))
(test       "2^100 / -2 = -2^99"
            (- (expt 2 99))
            (/ big -2))

;;; ============================================================
;;; 19. Mixed fixnum/bignum operations
;;; ============================================================

(section "mixed fixnum/bignum")

;; Fixnum + bignum
(test       "1 + 2^100"
            (+ (expt 2 100) 1)
            (+ 1 big))
(test       "2^100 + 1"
            (+ (expt 2 100) 1)
            (+ big 1))
(test-true  "exact? 1 + 2^100"   (exact? (+ 1 big)))

;; Fixnum * bignum
(test       "0 * 2^100 = 0"      0  (* 0 big))
(test       "1 * 2^100 = 2^100"  big  (* 1 big))
(test       "-1 * 2^100 = -2^100" -big  (* -1 big))
(test       "2 * 2^100 = 2^101"  (expt 2 101)  (* 2 big))

;; Fixnum - bignum
(test       "0 - 2^100 = -2^100" -big  (- 0 big))
(test       "1 - 2^100"
            (- 1 (expt 2 100))
            (- 1 big))

;; gcd with bignums
(test       "gcd(2^100, 6) = 2"  2  (gcd big 6))
(test       "gcd(2^100, 2^100) = 2^100"  big  (gcd big big))
(test       "gcd(0, 2^100) = 2^100"  big  (gcd 0 big))

;; lcm with bignums
(test       "lcm(2^100, 3)"
            (* big 3)
            (lcm big 3))

;;; ============================================================
;;; 20. Bignum comparisons
;;; ============================================================

(section "bignum comparisons")

(test-true  "2^100 = 2^100"      (= big big))
(test-false "2^100 = 2^100+1"    (= big (+ big 1)))
(test-true  "2^100 < 2^101"      (< big (expt 2 101)))
(test-true  "2^101 > 2^100"      (> (expt 2 101) big))
(test-true  "-2^100 < 2^100"     (< -big big))
(test-true  "-2^100 < 0"         (< -big 0))
(test-true  "-2^100 < -1"        (< -big -1))
(test-true  "2^100 > 0"          (> big 0))
(test-true  "2^100 >= 2^100"     (>= big big))
(test-true  "2^100 <= 2^100"     (<= big big))

;; Mixed bignum/fixnum comparisons
(test-true  "2^100 > 42"         (> big 42))
(test-true  "42 < 2^100"         (< 42 big))
(test-false "2^100 = 42"         (= big 42))

;; Mixed bignum/inexact comparisons
(test-true  "2^100 > 1.0"        (> big 1.0))
(test-true  "1.0 < 2^100"        (< 1.0 big))

;;; ============================================================
;;; 21. Bignum printing and reading round-trip
;;; ============================================================

(section "bignum print/read round-trip")

(test       "number->string 2^100"
            "1267650600228229401496703205376"
            (number->string big))
(test       "number->string -2^100"
            "-1267650600228229401496703205376"
            (number->string -big))

;; Round-trip: print then parse
(test       "read back 2^100"
            big
            (string->number (number->string big)))
(test       "read back -2^100"
            -big
            (string->number (number->string -big)))
(test-true  "exact? read-back big"
            (exact? (string->number (number->string big))))

;;; ============================================================
;;; 22. expt edge cases
;;; ============================================================

(section "expt edge cases")

(test       "(expt 0 0) = 1"     1   (expt 0 0))
(test       "(expt 2 0) = 1"     1   (expt 2 0))
(test       "(expt 0 1) = 0"     0   (expt 0 1))
(test       "(expt 1 1000) = 1"  1   (expt 1 1000))
(test       "(expt -1 0) = 1"    1   (expt -1 0))
(test       "(expt -1 1) = -1"   -1  (expt -1 1))
(test       "(expt -1 2) = 1"    1   (expt -1 2))

;; Large odd/even exponents of -1
(test       "(expt -1 101) = -1" -1  (expt -1 101))
(test       "(expt -1 100) = 1"  1   (expt -1 100))
(test-true  "exact? (expt -1 101)"  (exact? (expt -1 101)))

;; Exactness of expt
(test-true  "exact? (expt 2 10)"    (exact? (expt 2 10)))
(test       "(expt 2 10) = 1024"    1024  (expt 2 10))

;; Inexact base => inexact result
(test-true  "inexact? (expt 2.0 10)"  (inexact? (expt 2.0 10)))

;; Negative exponent => inexact in Phase 1 (no rationals)
(test-true  "inexact? (expt 2 -1)"    (inexact? (expt 2 -1)))
(test       "(expt 2 -1) = 0.5"       0.5  (expt 2 -1))

;;; ============================================================
;;; 23. min/max with bignums
;;; ============================================================

(section "min/max with bignums")

(test       "max(1, 2^100)"       big  (max 1 big))
(test       "min(1, 2^100)"       1    (min 1 big))
(test       "max(-2^100, 0)"      0    (max -big 0))
(test       "min(-2^100, 0)"      -big (min -big 0))
(test       "max(2^100, 2^101)"   (expt 2 101)  (max big (expt 2 101)))
(test-true  "exact? max bignums"  (exact? (max big (expt 2 101))))

;;; ============================================================
;;; 24. Zero/one-arg arithmetic edge cases
;;; ============================================================

(section "zero/one-arg arithmetic")

;; Already tested: (+) = 0, (*) = 1
;; One-arg minus is negation
(test       "(- 0) = 0"          0    (- 0))
(test       "(- 42) = -42"       -42  (- 42))
(test       "(- -42) = 42"       42   (- -42))
(test       "(- 2^100) = -2^100" -big (- big))
(test       "(- -2^100) = 2^100" big  (- -big))
(test-true  "exact? (- 2^100)"   (exact? (- big)))

;; One-arg / is reciprocal
(test       "(/ 1) = 1"          1    (/ 1))
(test       "(/ 2) = 0.5"        0.5  (/ 2))
(test       "(/ 0.5) = 2.0"      2.0  (/ 0.5))
(test-true  "inexact? (/ 2)"     (inexact? (/ 2)))
(test-error "(/ 0)"              (lambda () (/ 0)))

;; Multi-arg subtraction and division
(test       "(- 10 1 2 3) = 4"   4    (- 10 1 2 3))
(test       "(/ 120 2 3 4) = 5"  5    (/ 120 2 3 4))
(test-true  "exact? (/ 120 2 3 4)"  (exact? (/ 120 2 3 4)))

;;; ============================================================
;;; 25. Non-commutative operator argument ordering
;;; ============================================================

(section "non-commutative argument ordering")

;; ---- Subtraction: (- a b) = a - b ----

;; Unary (negation)
(test       "(- 1) = -1"           -1      (- 1))
(test       "(- -1) = 1"          1       (- -1))
(test       "(- 7) = -7"          -7      (- 7))
(test       "(- 1.0) = -1.0"     -1.0    (- 1.0))
(test       "(- 3.14) = -3.14"   -3.14   (- 3.14))

;; Binary: argument order matters
(test       "(- 10 3) = 7"        7       (- 10 3))
(test       "(- 3 10) = -7"       -7      (- 3 10))
(test       "(- 1 0) = 1"         1       (- 1 0))
(test       "(- 0 1) = -1"        -1      (- 0 1))
(test       "(- 100 1) = 99"      99      (- 100 1))
(test       "(- 1 100) = -99"     -99     (- 1 100))

;; Binary inexact
(test       "(- 10.0 3.0) = 7.0"  7.0     (- 10.0 3.0))
(test       "(- 3.0 10.0) = -7.0" -7.0    (- 3.0 10.0))

;; Binary mixed exact/inexact
(test       "(- 10 3.0) = 7.0"    7.0     (- 10 3.0))
(test       "(- 3.0 10) = -7.0"   -7.0    (- 3.0 10))

;; Multi-arg: (- a b c) = a - b - c
(test       "(- 20 3 7) = 10"     10      (- 20 3 7))
(test       "(- 1 2 3) = -4"      -4      (- 1 2 3))
(test       "(- 100 10 20 30) = 40" 40     (- 100 10 20 30))

;; Bignum subtraction ordering
(test       "(- big 1) > 0"       #t      (> (- big 1) 0))
(test       "(- 1 big) < 0"       #t      (< (- 1 big) 0))

;; ---- Division: (/ a b) = a / b ----

;; Unary (reciprocal)
(test       "(/ 1) = 1"           1       (/ 1))
(test       "(/ -1) = -1"         -1      (/ -1))
(test       "(/ 2) = 0.5"         0.5     (/ 2))
(test       "(/ 4) = 0.25"        0.25    (/ 4))
(test       "(/ 10) = 0.1"        0.1     (/ 10))
(test       "(/ 0.5) = 2.0"       2.0     (/ 0.5))
(test       "(/ 0.25) = 4.0"      4.0     (/ 0.25))
(test       "(/ 2.0) = 0.5"       0.5     (/ 2.0))
(test       "(/ -2) = -0.5"       -0.5    (/ -2))
(test       "(/ -0.5) = -2.0"     -2.0    (/ -0.5))
(test-error "(/ 0)"               (lambda () (/ 0)))
(test-error "(/ 0.0)"             (lambda () (/ 0.0)))

;; Binary: argument order matters — THIS IS THE BUG
;; (/ a b) must return a/b, not b/a
(test       "(/ 1 3) ~ 0.333"     #t      (< (abs (- (/ 1 3) 0.333333333333333)) 1e-10))
(test       "(/ 3 1) = 3"         3       (/ 3 1))
(test       "(/ 7 2) = 3.5"       3.5     (/ 7 2))
(test       "(/ 2 7) ~ 0.286"     #t      (< (abs (- (/ 2 7) 0.285714285714286)) 1e-10))
(test       "(/ 1 10) = 0.1"      0.1     (/ 1 10))
(test       "(/ 10 1) = 10"       10      (/ 10 1))
(test       "(/ 2 8) = 0.25"      0.25    (/ 2 8))
(test       "(/ 8 2) = 4"         4       (/ 8 2))
(test       "(/ 100 3) ~ 33.3"    #t      (< (abs (- (/ 100 3) 33.3333333333333)) 1e-10))
(test       "(/ 3 100) = 0.03"    0.03    (/ 3 100))

;; Binary exact division (zero remainder)
(test       "(/ 6 2) = 3"         3       (/ 6 2))
(test       "(/ 2 6) ~ 0.333"     #t      (< (abs (- (/ 2 6) 0.333333333333333)) 1e-10))
(test       "(/ 12 3) = 4"        4       (/ 12 3))
(test       "(/ 3 12) = 0.25"     0.25    (/ 3 12))
(test-true  "exact? (/ 6 2)"     (exact? (/ 6 2)))
(test-true  "exact? (/ 12 3)"    (exact? (/ 12 3)))

;; Binary inexact
(test       "(/ 1.0 3.0) ~ 0.333" #t     (< (abs (- (/ 1.0 3.0) 0.333333333333333)) 1e-10))
(test       "(/ 3.0 1.0) = 3.0"   3.0    (/ 3.0 1.0))
(test       "(/ 7.0 2.0) = 3.5"   3.5    (/ 7.0 2.0))
(test       "(/ 2.0 7.0) ~ 0.286" #t     (< (abs (- (/ 2.0 7.0) 0.285714285714286)) 1e-10))

;; Binary mixed exact/inexact
(test       "(/ 1 3.0) ~ 0.333"   #t     (< (abs (- (/ 1 3.0) 0.333333333333333)) 1e-10))
(test       "(/ 1.0 3) ~ 0.333"   #t     (< (abs (- (/ 1.0 3) 0.333333333333333)) 1e-10))
(test       "(/ 7.0 2) = 3.5"     3.5    (/ 7.0 2))
(test       "(/ 7 2.0) = 3.5"     3.5    (/ 7 2.0))

;; Multi-arg: (/ a b c) = (a / b) / c = a / (b * c)
(test       "(/ 120 2 3) = 20"    20      (/ 120 2 3))
(test       "(/ 120 3 2) = 20"    20      (/ 120 3 2))
(test       "(/ 100 5 4) = 5"     5       (/ 100 5 4))
(test       "(/ 1 2 5) = 0.1"     0.1     (/ 1 2 5))
(test       "(/ 1 3 7) ~ 0.048"   #t      (< (abs (- (/ 1 3 7) (/ 1.0 21.0))) 1e-10))

;; Division by zero
(test-error "(/ 1 0)"             (lambda () (/ 1 0)))
(test-error "(/ 7 0)"             (lambda () (/ 7 0)))

;; Negative division ordering
(test       "(/ -6 2) = -3"       -3      (/ -6 2))
(test       "(/ 6 -2) = -3"       -3      (/ 6 -2))
(test       "(/ -6 -2) = 3"       3       (/ -6 -2))
(test       "(/ -1 3) ~ -0.333"   #t      (< (abs (- (/ -1 3) -0.333333333333333)) 1e-10))
(test       "(/ 1 -3) ~ -0.333"   #t      (< (abs (- (/ 1 -3) -0.333333333333333)) 1e-10))

;; Bignum division ordering
(test       "(/ big 2) = 2^99"    (expt 2 99) (/ big 2))
(test       "(/ 2 big) ~ 0"       #t      (< (/ 2 big) 1e-20))

;; ---- quotient: asymmetric ----

(test       "(quotient 10 3) = 3"  3      (quotient 10 3))
(test       "(quotient 3 10) = 0"  0      (quotient 3 10))
(test       "(quotient 17 5) = 3"  3      (quotient 17 5))
(test       "(quotient 5 17) = 0"  0      (quotient 5 17))
(test       "(quotient -17 5) = -3" -3    (quotient -17 5))
(test       "(quotient 17 -5) = -3" -3    (quotient 17 -5))
(test       "(quotient -17 -5) = 3" 3     (quotient -17 -5))

;; ---- remainder: asymmetric ----

(test       "(remainder 10 3) = 1"  1     (remainder 10 3))
(test       "(remainder 3 10) = 3"  3     (remainder 3 10))
(test       "(remainder 17 5) = 2"  2     (remainder 17 5))
(test       "(remainder 5 17) = 5"  5     (remainder 5 17))
(test       "(remainder -17 5) = -2" -2   (remainder -17 5))
(test       "(remainder 17 -5) = 2"  2    (remainder 17 -5))

;; ---- modulo: asymmetric ----

(test       "(modulo 10 3) = 1"     1     (modulo 10 3))
(test       "(modulo 3 10) = 3"     3     (modulo 3 10))
(test       "(modulo 17 5) = 2"     2     (modulo 17 5))
(test       "(modulo 5 17) = 5"     5     (modulo 5 17))
(test       "(modulo -17 5) = 3"    3     (modulo -17 5))
(test       "(modulo 17 -5) = -3"   -3    (modulo 17 -5))

;; ---- expt: asymmetric ----

(test       "(expt 2 3) = 8"        8     (expt 2 3))
(test       "(expt 3 2) = 9"        9     (expt 3 2))
(test       "(expt 2 10) = 1024"    1024  (expt 2 10))
(test       "(expt 10 2) = 100"     100   (expt 10 2))
(test       "(expt 2 0) = 1"        1     (expt 2 0))
(test       "(expt 0 2) = 0"        0     (expt 0 2))
(test       "(expt 5 3) = 125"      125   (expt 5 3))
(test       "(expt 3 5) = 243"      243   (expt 3 5))

;; expt with inexact
(test       "(expt 2.0 3) = 8.0"    8.0   (expt 2.0 3))
(test       "(expt 2 3.0) = 8.0"    8.0   (expt 2 3.0))

;; expt with negative exponent
(test       "(expt 2 -1) = 0.5"     0.5   (expt 2 -1))
(test       "(expt 2 -3) = 0.125"   0.125 (expt 2 -3))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "======================================") (newline)
(display "Numeric tower Phase 1 tests: ")
(display pass-count) (display "/") (display test-count) (display " passed")
(if (> fail-count 0)
    (begin (display ", ") (display fail-count) (display " FAILED"))
    (display ", all passed"))
(newline)
(display "======================================") (newline)

(exit (if (= fail-count 0) 0 1))
