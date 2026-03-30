;;; test-cn-mixed.scm --- Compiler numerics: mixed-type dispatch + interop
;;; Tests that exercise cross-type operations, numeric dispatch,


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

;;; and the interop roundtrip primitive.

(section "CN-MIXED: mixed-type loops")

;; Start with integer, accumulate rationals
(test "int + rational loop"
  (+ 10 7381/2520)
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
      (if (= i 5) (+ acc 0.5)
          (loop (+ i 1) (+ acc 1/7))))))

(section "CN-MIXED: numeric dispatch gauntlet")

(define (numeric-gauntlet x)
  (list (number? x)
        (exact? x)
        (inexact? x)
        (integer? x)
        (real? x)
        (complex? x)
        (zero? x)
        (positive? x)))

(test "gauntlet fixnum"
  '(#t #t #f #t #t #t #f #t)
  (numeric-gauntlet 42))

(test "gauntlet rational"
  '(#t #t #f #f #t #t #f #t)
  (numeric-gauntlet 1/3))

(test "gauntlet float"
  '(#t #f #t #f #t #t #f #t)
  (numeric-gauntlet 3.14))

(test "gauntlet zero"
  '(#t #t #f #t #t #t #t #f)
  (numeric-gauntlet 0))

(section "CN-MIXED: higher-order over mixed types")

(define mixed-nums (list 1 1/3 3.14 (expt 2 100)))

(test "map number? mixed"
  '(#t #t #t #t)
  (map number? mixed-nums))

(test "map exact? mixed"
  '(#t #t #f #t)
  (map exact? mixed-nums))

(test "apply + mixed"
  (+ 1 1/3 3.14)
  (apply + '(1 1/3 3.14)))

(section "CN-MIXED: exactness preservation across types")

(test-true "integer stays exact through loop"
  (exact?
    (let loop ((i 0) (acc 0))
      (if (= i 10) acc
          (loop (+ i 1) (+ acc 1))))))

(test-true "rational stays exact through loop"
  (exact?
    (let loop ((i 0) (acc 0))
      (if (= i 10) acc
          (loop (+ i 1) (+ acc 1/7))))))

(test-true "bignum stays exact through *"
  (exact? (* (expt 2 60) 2)))

(section "CN-MIXED: interop roundtrip")

(test "rt INTEGER 42"     42      (interop-roundtrip 'INTEGER 42))
(test "rt CARDINAL 42"    42      (interop-roundtrip 'CARDINAL 42))
(test "rt LONGREAL 3.14"  3.14    (interop-roundtrip 'LONGREAL 3.14))
(test "rt TEXT hello"      "hello" (interop-roundtrip 'TEXT "hello"))
(test-true  "rt BOOLEAN 42"       (interop-roundtrip 'BOOLEAN 42))
(test-false "rt BOOLEAN #f"       (interop-roundtrip 'BOOLEAN #f))
(test "rt REFANY 1/3"     1/3     (interop-roundtrip 'REFANY 1/3))
(test "rt Mpz_T big"
  (expt 2 100)
  (interop-roundtrip 'Mpz_T (expt 2 100)))
(test-no-error "rt Mpfr_T 3.14"
  (lambda () (interop-roundtrip 'Mpfr_T 3.14)))
(test "rt CHAR #\\A"       #\A     (interop-roundtrip 'CHAR #\A))
(test "rt REFANY list"    '(1 2)  (interop-roundtrip 'REFANY '(1 2)))

(test-summary "CN-MIXED")
