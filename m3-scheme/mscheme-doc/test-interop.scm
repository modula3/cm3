;;; test-interop.scm --- Exhaustive M3<->Scheme interop test suite
;;;
;;; Tests every cell in the (Scheme-type x M3-type) conversion matrix
;;; exercised by the interop-roundtrip primitive, which calls
;;; ToModula_<TYPE>(x) then ToScheme_<TYPE>(result).
;;;
;;; Usage: mscheme test-interop.scm

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

;;; approx= : for precision-lossy roundtrips (REAL, rationals->LONGREAL)
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

;;; Shorthand
(define (rt type val) (interop-roundtrip type val))

;;; ============================================================
;;; 1. Fixnum -> each M3 type
;;; ============================================================

(section "fixnum -> M3 types")

;; INTEGER
(test "fixnum 42 -> INTEGER"        42  (rt 'INTEGER 42))
(test "fixnum 0 -> INTEGER"          0  (rt 'INTEGER 0))
(test "fixnum -1 -> INTEGER"        -1  (rt 'INTEGER -1))
(test "fixnum 1000000 -> INTEGER"   1000000 (rt 'INTEGER 1000000))

;; CARDINAL
(test "fixnum 42 -> CARDINAL"       42  (rt 'CARDINAL 42))
(test "fixnum 0 -> CARDINAL"         0  (rt 'CARDINAL 0))
(test-error "fixnum -1 -> CARDINAL" (lambda () (rt 'CARDINAL -1)))

;; LONGREAL
(test "fixnum 42 -> LONGREAL"       42.0 (rt 'LONGREAL 42))
(test "fixnum 0 -> LONGREAL"         0.0 (rt 'LONGREAL 0))
(test "fixnum -1 -> LONGREAL"       -1.0 (rt 'LONGREAL -1))

;; REAL (precision loss possible)
(test "fixnum 42 -> REAL"           42.0 (rt 'REAL 42))
(test "fixnum 0 -> REAL"             0.0 (rt 'REAL 0))

;; EXTENDED
(test "fixnum 42 -> EXTENDED"       42.0 (rt 'EXTENDED 42))

;; Mpz_T
(test "fixnum 42 -> Mpz_T"         42  (rt 'Mpz_T 42))
(test "fixnum 0 -> Mpz_T"           0  (rt 'Mpz_T 0))
(test "fixnum -1 -> Mpz_T"         -1  (rt 'Mpz_T -1))

;; Mpfr_T (returns SchemeMpfr, compare via LONGREAL roundtrip)
(test-no-error "fixnum 42 -> Mpfr_T"
  (lambda () (rt 'Mpfr_T 42)))

;; TEXT
(test-error "fixnum 42 -> TEXT" (lambda () (rt 'TEXT 42)))

;; BOOLEAN (TruthO: everything is #t except #f)
(test-true "fixnum 42 -> BOOLEAN"   (rt 'BOOLEAN 42))
(test-true "fixnum 0 -> BOOLEAN"    (rt 'BOOLEAN 0))
(test-true "fixnum -1 -> BOOLEAN"   (rt 'BOOLEAN -1))

;; CHAR
(test-error "fixnum 42 -> CHAR" (lambda () (rt 'CHAR 42)))

;; REFANY
(test "fixnum 42 -> REFANY"        42  (rt 'REFANY 42))

;;; ============================================================
;;; 2. Bignum -> each M3 type
;;; ============================================================

(section "bignum -> M3 types")

(define big (expt 2 100))
(define neg-big (- big))

;; INTEGER -- out of range
(test-error "bignum 2^100 -> INTEGER" (lambda () (rt 'INTEGER big)))

;; CARDINAL -- out of range
(test-error "bignum 2^100 -> CARDINAL" (lambda () (rt 'CARDINAL big)))

;; LONGREAL -- precision loss but succeeds
(test-no-error "bignum 2^100 -> LONGREAL"
  (lambda () (rt 'LONGREAL big)))

;; REAL
(test-no-error "bignum 2^100 -> REAL"
  (lambda () (rt 'REAL big)))

;; EXTENDED
(test-no-error "bignum 2^100 -> EXTENDED"
  (lambda () (rt 'EXTENDED big)))

;; Mpz_T -- preserves exactly
(test "bignum 2^100 -> Mpz_T"      big (rt 'Mpz_T big))
(test "bignum -2^100 -> Mpz_T"     neg-big (rt 'Mpz_T neg-big))

;; Mpfr_T
(test-no-error "bignum 2^100 -> Mpfr_T"
  (lambda () (rt 'Mpfr_T big)))

;; TEXT
(test-error "bignum -> TEXT" (lambda () (rt 'TEXT big)))

;; BOOLEAN
(test-true "bignum -> BOOLEAN" (rt 'BOOLEAN big))

;; CHAR
(test-error "bignum -> CHAR" (lambda () (rt 'CHAR big)))

;; REFANY
(test "bignum -> REFANY" big (rt 'REFANY big))

;; Bignum that fits in INTEGER
(define big-fits (+ 1 (expt 2 40)))  ; fits in 64-bit INTEGER
(test "fits-bignum -> INTEGER" big-fits (rt 'INTEGER big-fits))

;;; ============================================================
;;; 3. Exact rational -> each M3 type
;;; ============================================================

(section "exact rational -> M3 types")

;; INTEGER -- error (not an integer)
(test-error "1/3 -> INTEGER" (lambda () (rt 'INTEGER 1/3)))

;; But integer-valued rationals work
(test "4/2 -> INTEGER" 2 (rt 'INTEGER 4/2))

;; CARDINAL
(test-error "1/3 -> CARDINAL" (lambda () (rt 'CARDINAL 1/3)))
(test "4/2 -> CARDINAL" 2 (rt 'CARDINAL 4/2))

;; LONGREAL
(test-no-error "1/3 -> LONGREAL"
  (lambda () (rt 'LONGREAL 1/3)))
(test-no-error "1/2 -> LONGREAL"
  (lambda () (rt 'LONGREAL 1/2)))

;; Check that 1/2 roundtrips exactly
(test "1/2 -> LONGREAL exact" 0.5 (rt 'LONGREAL 1/2))

;; REAL
(test-no-error "1/3 -> REAL" (lambda () (rt 'REAL 1/3)))

;; EXTENDED
(test-no-error "1/3 -> EXTENDED" (lambda () (rt 'EXTENDED 1/3)))

;; Mpz_T -- error (not an integer)
(test-error "1/3 -> Mpz_T" (lambda () (rt 'Mpz_T 1/3)))

;; Mpfr_T
(test-no-error "1/3 -> Mpfr_T" (lambda () (rt 'Mpfr_T 1/3)))

;; TEXT
(test-error "1/3 -> TEXT" (lambda () (rt 'TEXT 1/3)))

;; BOOLEAN
(test-true "1/3 -> BOOLEAN" (rt 'BOOLEAN 1/3))

;; CHAR
(test-error "1/3 -> CHAR" (lambda () (rt 'CHAR 1/3)))

;; REFANY
(test-no-error "1/3 -> REFANY" (lambda () (rt 'REFANY 1/3)))

;;; ============================================================
;;; 4. Inexact LONGREAL -> each M3 type
;;; ============================================================

(section "inexact LONGREAL -> M3 types")

;; INTEGER -- non-integer float errors
(test-error "3.14 -> INTEGER" (lambda () (rt 'INTEGER 3.14)))
;; integer-valued float works
(test "42.0 -> INTEGER" 42 (rt 'INTEGER 42.0))

;; CARDINAL
(test-error "3.14 -> CARDINAL" (lambda () (rt 'CARDINAL 3.14)))
(test "42.0 -> CARDINAL" 42 (rt 'CARDINAL 42.0))
(test-error "-1.0 -> CARDINAL" (lambda () (rt 'CARDINAL -1.0)))

;; LONGREAL
(test "3.14 -> LONGREAL" 3.14 (rt 'LONGREAL 3.14))
(test "0.0 -> LONGREAL"  0.0  (rt 'LONGREAL 0.0))
(test "-0.0 -> LONGREAL" -0.0 (rt 'LONGREAL -0.0))

;; Special values
(test-no-error "+inf.0 -> LONGREAL" (lambda () (rt 'LONGREAL +inf.0)))
(test-no-error "-inf.0 -> LONGREAL" (lambda () (rt 'LONGREAL -inf.0)))
(test-no-error "+nan.0 -> LONGREAL" (lambda () (rt 'LONGREAL +nan.0)))

;; REAL
(test-approx "3.14 -> REAL" 3.14 (rt 'REAL 3.14) 1e-6)

;; EXTENDED
(test-approx "3.14 -> EXTENDED" 3.14 (rt 'EXTENDED 3.14) 1e-6)

;; Mpz_T -- non-integer float errors
(test-error "3.14 -> Mpz_T" (lambda () (rt 'Mpz_T 3.14)))
;; integer-valued float works
(test "42.0 -> Mpz_T" 42 (rt 'Mpz_T 42.0))

;; Mpfr_T
(test-no-error "3.14 -> Mpfr_T" (lambda () (rt 'Mpfr_T 3.14)))

;; INTEGER boundary: +inf.0
(test-error "+inf.0 -> INTEGER" (lambda () (rt 'INTEGER +inf.0)))

;; TEXT
(test-error "3.14 -> TEXT" (lambda () (rt 'TEXT 3.14)))

;; BOOLEAN
(test-true "3.14 -> BOOLEAN" (rt 'BOOLEAN 3.14))
(test-true "0.0 -> BOOLEAN"  (rt 'BOOLEAN 0.0))

;; CHAR
(test-error "3.14 -> CHAR" (lambda () (rt 'CHAR 3.14)))

;; REFANY
(test "3.14 -> REFANY" 3.14 (rt 'REFANY 3.14))

;;; ============================================================
;;; 5. SchemeMpfr -> each M3 type
;;; ============================================================

(section "SchemeMpfr -> M3 types")

(define mpfr-pi (make-mpfr 3.14 128))
(define mpfr-42 (make-mpfr 42.0 64))

;; INTEGER -- GAP: SchemeLongReal.FromO doesn't handle SchemeMpfr.T
(test-error "mpfr-pi -> INTEGER" (lambda () (rt 'INTEGER mpfr-pi)))

;; CARDINAL
(test-error "mpfr-pi -> CARDINAL" (lambda () (rt 'CARDINAL mpfr-pi)))

;; LONGREAL -- GAP
(test-error "mpfr-pi -> LONGREAL" (lambda () (rt 'LONGREAL mpfr-pi)))

;; REAL -- GAP
(test-error "mpfr-pi -> REAL" (lambda () (rt 'REAL mpfr-pi)))

;; EXTENDED -- GAP
(test-error "mpfr-pi -> EXTENDED" (lambda () (rt 'EXTENDED mpfr-pi)))

;; Mpz_T -- GAP
(test-error "mpfr-42 -> Mpz_T" (lambda () (rt 'Mpz_T mpfr-42)))

;; Mpfr_T -- direct, works
(test-no-error "mpfr-pi -> Mpfr_T" (lambda () (rt 'Mpfr_T mpfr-pi)))
(test-no-error "mpfr-42 -> Mpfr_T" (lambda () (rt 'Mpfr_T mpfr-42)))

;; TEXT
(test-error "mpfr -> TEXT" (lambda () (rt 'TEXT mpfr-pi)))

;; BOOLEAN
(test-true "mpfr -> BOOLEAN" (rt 'BOOLEAN mpfr-pi))

;; CHAR
(test-error "mpfr -> CHAR" (lambda () (rt 'CHAR mpfr-pi)))

;; REFANY
(test-no-error "mpfr -> REFANY" (lambda () (rt 'REFANY mpfr-pi)))

;;; ============================================================
;;; 6. Complex -> each M3 type
;;; ============================================================

(section "complex -> M3 types")

(define c1 (make-rectangular 3 4))   ; 3+4i (exact parts)
(define c2 (make-rectangular 3.0 4.0))  ; inexact complex

;; Note: (make-rectangular 5 0) demotes to 5 (exact zero imaginary)
;; So we only test actual complex values here

;; INTEGER
(test-error "3+4i -> INTEGER" (lambda () (rt 'INTEGER c1)))

;; CARDINAL
(test-error "3+4i -> CARDINAL" (lambda () (rt 'CARDINAL c1)))

;; LONGREAL
(test-error "3+4i -> LONGREAL" (lambda () (rt 'LONGREAL c1)))

;; REAL
(test-error "3+4i -> REAL" (lambda () (rt 'REAL c1)))

;; EXTENDED
(test-error "3+4i -> EXTENDED" (lambda () (rt 'EXTENDED c1)))

;; Mpz_T
(test-error "3+4i -> Mpz_T" (lambda () (rt 'Mpz_T c1)))

;; Mpfr_T
(test-error "3+4i -> Mpfr_T" (lambda () (rt 'Mpfr_T c1)))

;; TEXT
(test-error "3+4i -> TEXT" (lambda () (rt 'TEXT c1)))

;; BOOLEAN -- TruthO: complex is #t
(test-true "3+4i -> BOOLEAN" (rt 'BOOLEAN c1))

;; CHAR
(test-error "3+4i -> CHAR" (lambda () (rt 'CHAR c1)))

;; REFANY
(test-no-error "3+4i -> REFANY" (lambda () (rt 'REFANY c1)))

;; Complex with zero imaginary (exact) demotes to real
(test "5+0i -> INTEGER" 5 (rt 'INTEGER (make-rectangular 5 0)))

;;; ============================================================
;;; 7. Non-numeric -> numeric types
;;; ============================================================

(section "non-numeric -> numeric types")

;; Strings
(test-error "string -> INTEGER"  (lambda () (rt 'INTEGER "hello")))
(test-error "string -> CARDINAL" (lambda () (rt 'CARDINAL "hello")))
(test-error "string -> LONGREAL" (lambda () (rt 'LONGREAL "hello")))
(test-error "string -> REAL"     (lambda () (rt 'REAL "hello")))
(test-error "string -> EXTENDED" (lambda () (rt 'EXTENDED "hello")))
(test-error "string -> Mpz_T"   (lambda () (rt 'Mpz_T "hello")))
(test-error "string -> Mpfr_T"  (lambda () (rt 'Mpfr_T "hello")))

;; Symbols
(test-error "symbol -> INTEGER"  (lambda () (rt 'INTEGER 'foo)))
(test-error "symbol -> LONGREAL" (lambda () (rt 'LONGREAL 'foo)))

;; Lists
(test-error "list -> INTEGER"    (lambda () (rt 'INTEGER '(1 2 3))))

;;; ============================================================
;;; 8. Edge cases
;;; ============================================================

(section "edge cases")

;; Zero
(test "0 -> INTEGER"   0 (rt 'INTEGER 0))
(test "0 -> CARDINAL"  0 (rt 'CARDINAL 0))
(test "0 -> LONGREAL"  0.0 (rt 'LONGREAL 0))
(test "0 -> Mpz_T"     0 (rt 'Mpz_T 0))

;; Integer-valued rationals (denominator 1 already demoted)
;; 4/2 = 2 (exact integer after demotion)
(test "4/2 -> INTEGER"  2 (rt 'INTEGER 4/2))
(test "4/2 -> Mpz_T"    2 (rt 'Mpz_T 4/2))

;; Integer-valued floats
(test "42.0 -> INTEGER"  42 (rt 'INTEGER 42.0))
(test "42.0 -> CARDINAL" 42 (rt 'CARDINAL 42.0))
(test "42.0 -> Mpz_T"   42 (rt 'Mpz_T 42.0))

;;; ============================================================
;;; 9. BOOLEAN semantics
;;; ============================================================

(section "BOOLEAN semantics (TruthO)")

(test-true  "0 -> BOOLEAN"         (rt 'BOOLEAN 0))
(test-false "#f -> BOOLEAN"        (rt 'BOOLEAN #f))
(test-true  "#t -> BOOLEAN"        (rt 'BOOLEAN #t))
(test-true  "'() -> BOOLEAN"       (rt 'BOOLEAN '()))
(test-true  "\"\" -> BOOLEAN"      (rt 'BOOLEAN ""))
(test-true  "42 -> BOOLEAN"        (rt 'BOOLEAN 42))
(test-true  "'foo -> BOOLEAN"      (rt 'BOOLEAN 'foo))
(test-true  "\"hello\" -> BOOLEAN" (rt 'BOOLEAN "hello"))
(test-true  "3.14 -> BOOLEAN"      (rt 'BOOLEAN 3.14))

;;; ============================================================
;;; 10. TEXT round-trip
;;; ============================================================

(section "TEXT round-trip")

(test "\"hello\" -> TEXT"       "hello"  (rt 'TEXT "hello"))
(test "\"\" -> TEXT"            ""       (rt 'TEXT ""))
(test "\"with spaces\" -> TEXT" "with spaces" (rt 'TEXT "with spaces"))
(test "\"newline\\n\" -> TEXT"  "line1\nline2" (rt 'TEXT "line1\nline2"))

;;; ============================================================
;;; 11. CHAR round-trip
;;; ============================================================

(section "CHAR round-trip")

(test "char A -> CHAR"       #\A      (rt 'CHAR #\A))
(test "char space -> CHAR"   #\space  (rt 'CHAR #\space))
(test "char newline -> CHAR" #\newline (rt 'CHAR #\newline))

;; Non-char types -> CHAR should error
(test-error "42 -> CHAR"      (lambda () (rt 'CHAR 42)))
(test-error "\"A\" -> CHAR"   (lambda () (rt 'CHAR "A")))

;;; ============================================================
;;; 12. REFANY passthrough
;;; ============================================================

(section "REFANY passthrough")

(test "fixnum -> REFANY"     42     (rt 'REFANY 42))
(test "string -> REFANY"     "hi"   (rt 'REFANY "hi"))
(test "symbol -> REFANY"     'foo   (rt 'REFANY 'foo))
(test "boolean -> REFANY"    #t     (rt 'REFANY #t))
(test "boolean #f -> REFANY" #f     (rt 'REFANY #f))
(test "list -> REFANY"       '(1 2) (rt 'REFANY '(1 2)))
(test "char -> REFANY"       #\A    (rt 'REFANY #\A))
(test "float -> REFANY"      3.14   (rt 'REFANY 3.14))
(test-no-error "mpfr -> REFANY"  (lambda () (rt 'REFANY (make-mpfr 1.0 53))))
(test-no-error "rational -> REFANY" (lambda () (rt 'REFANY 1/3)))
(test-no-error "bignum -> REFANY" (lambda () (rt 'REFANY (expt 2 100))))

;;; ============================================================
;;; 13. Precision tests
;;; ============================================================

(section "precision tests")

;; REAL has less precision than LONGREAL
(let ((lr-result (rt 'LONGREAL 1/3))
      (r-result  (rt 'REAL 1/3)))
  ;; Both approximate 1/3 but REAL is less precise
  (test-approx "1/3 -> LONGREAL" 0.3333333333333333 lr-result 1e-15)
  (test-approx "1/3 -> REAL"     0.3333333333333333 r-result  1e-6))

;; Mpfr_T roundtrip preserves value
(let ((m (make-mpfr 3.14159265358979323846 128)))
  (test-no-error "mpfr high-prec roundtrip"
    (lambda ()
      (let ((result (rt 'Mpfr_T m)))
        ;; Result should be a valid mpfr
        (rt 'Mpfr_T result)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "============================================================") (newline)
(display "TOTAL: ") (display test-count) (newline)
(display "PASS:  ") (display pass-count) (newline)
(display "FAIL:  ") (display fail-count) (newline)
(display "============================================================") (newline)

(if (> fail-count 0)
    (begin (display "*** SOME TESTS FAILED ***") (newline))
    (begin (display "All tests passed.") (newline)))
