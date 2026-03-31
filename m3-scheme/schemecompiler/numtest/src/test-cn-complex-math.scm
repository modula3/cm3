;;; test-complex-math.scm --- Complex number math builtin acceptance tests
;;;
;;; Documents which operations work with complex numbers and which don't.
;;; Tests marked "GAP" are operations that should work per R4RS/R5RS
;;; but currently error. As gaps are fixed, change test-error to the
;;; appropriate passing test.
;;;
;;; Usage: mscheme test-complex-math.scm

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

(define z1 (make-rectangular 3 4))      ; 3+4i
(define z2 (make-rectangular 1 2))      ; 1+2i
(define zi (make-rectangular 0 1))      ; +i
(define z0 (make-rectangular 0 0))      ; 0+0i -> demotes to 0
(define zr (make-rectangular 5 0))      ; 5+0i -> demotes to 5

;;; ============================================================
;;; 1. Basic arithmetic (WORKING)
;;; ============================================================

(section "complex arithmetic")

(test "(+ 3+4i 1+2i)"  (make-rectangular 4 6)   (+ z1 z2))
(test "(- 3+4i 1+2i)"  (make-rectangular 2 2)   (- z1 z2))
(test "(* 3+4i 1+2i)"  (make-rectangular -5 10) (* z1 z2))
(test "(/ 3+4i 1+2i)"  (make-rectangular 11/5 -2/5) (/ z1 z2))
(test "(- 3+4i)"        (make-rectangular -3 -4) (- z1))

;; Complex + real
(test "(+ 3+4i 5)"     (make-rectangular 8 4)   (+ z1 5))
(test "(+ 5 3+4i)"     (make-rectangular 8 4)   (+ 5 z1))
(test "(* 3+4i 2)"     (make-rectangular 6 8)   (* z1 2))
(test "(/ 3+4i 2)"     (make-rectangular 3/2 2) (/ z1 2))

;; Complex + rational
(test "(+ 3+4i 1/2)"   (make-rectangular 7/2 4) (+ z1 1/2))

;; i * i = -1
(test "i*i = -1"       -1  (* zi zi))

;; Demotion: zero imaginary -> real
(test "5+0i = 5"       5   zr)
(test-true "integer? 5+0i" (integer? zr))

;;; ============================================================
;;; 2. Predicates (WORKING)
;;; ============================================================

(section "complex predicates")

(test-true  "number? 3+4i"   (number? z1))
(test-true  "complex? 3+4i"  (complex? z1))
(test-false "real? 3+4i"     (real? z1))
(test-false "integer? 3+4i"  (integer? z1))
(test-true  "exact? 3+4i"    (exact? z1))
(test-false "inexact? 3+4i"  (inexact? z1))

;; Inexact complex
(define z-inex (make-rectangular 3.0 4.0))
(test-true  "inexact? 3.0+4.0i" (inexact? z-inex))
(test-false "exact? 3.0+4.0i"   (exact? z-inex))

;;; ============================================================
;;; 3. Accessors (WORKING)
;;; ============================================================

(section "complex accessors")

(test "real-part 3+4i"   3   (real-part z1))
(test "imag-part 3+4i"   4   (imag-part z1))
(test "real-part +i"     0   (real-part zi))
(test "imag-part +i"     1   (imag-part zi))
(test "magnitude 3+4i"   5.0 (magnitude z1))
(test-no-error "angle 3+4i" (lambda () (angle z1)))

;; real-part/imag-part on reals
(test "real-part 5"      5   (real-part 5))
(test "imag-part 5"      0   (imag-part 5))

;;; ============================================================
;;; 4. Equality and ordering (PARTIALLY WORKING)
;;; ============================================================

(section "complex equality and ordering")

(test-true  "(= 3+4i 3+4i)"  (= z1 z1))
(test-false "(= 3+4i 1+2i)"  (= z1 z2))

;; Ordering should error on complex
(test-error "(< 3+4i 1+2i)"  (lambda () (< z1 z2)))
(test-error "(> 3+4i 1+2i)"  (lambda () (> z1 z2)))
(test-error "(<= 3+4i 1+2i)" (lambda () (<= z1 z2)))
(test-error "(>= 3+4i 1+2i)" (lambda () (>= z1 z2)))

;;; ============================================================
;;; 5. zero? (GAP — should work)
;;; ============================================================

(section "complex zero?")

;; zero? should return #f for nonzero complex
;; Currently errors because zero? doesn't handle complex
(test-false "zero? 3+4i"   (zero? z1))
(test-true  "zero? 0+0i"  (zero? (make-rectangular 0 0)))
(test-false "zero? 0+1i"  (zero? zi))

;;; ============================================================
;;; 6. abs (GAP — should return magnitude)
;;; ============================================================

(section "complex abs")

(test "abs 3+4i = 5.0" 5.0 (abs z1))

;;; ============================================================
;;; 7. sqrt (GAP — should work)
;;; ============================================================

(section "complex sqrt")

;; sqrt(3+4i) = 2+i
(test "sqrt 3+4i" (make-rectangular 2 1) (sqrt z1))

;; sqrt of negative real should produce complex: sqrt(-1) = +i
(test "sqrt -1 = +i" (make-rectangular 0 1) (sqrt -1))
(test "sqrt -4 = 2i" (make-rectangular 0 2) (sqrt -4))

;;; ============================================================
;;; 8. expt (GAP — should work)
;;; ============================================================

(section "complex expt")

;; expt via log/exp loses precision for integer exponents;
;; exact result would be -7+24i
(let ((r (expt z1 2)))
  (test-approx "expt 3+4i 2 real" -7.0 (real-part r) 1e-10)
  (test-approx "expt 3+4i 2 imag" 24.0 (imag-part r) 1e-10))

;; Exact integer exponent via multiplication: (3+4i)*(3+4i) = -7+24i
(test "(3+4i)^2 via *" (make-rectangular -7 24) (* z1 z1))

;;; ============================================================
;;; 9. Transcendental functions (GAP — should work)
;;; ============================================================

(section "complex transcendentals")

;; exp(i*pi) = -1 (Euler's formula)
(test-approx "e^(i*pi) real" -1.0
  (real-part (exp (make-rectangular 0 3.141592653589793))) 1e-10)
(test-approx "e^(i*pi) imag" 0.0
  (imag-part (exp (make-rectangular 0 3.141592653589793))) 1e-10)

;; log(e^z) = z (roundtrip)
(test-approx "log(exp(1+2i)) real" 1.0
  (real-part (log (exp (make-rectangular 1 2)))) 1e-10)
(test-approx "log(exp(1+2i)) imag" 2.0
  (imag-part (log (exp (make-rectangular 1 2)))) 1e-10)

;; sin^2 + cos^2 = 1 for complex
(let ((s (sin z1)) (c (cos z1)))
  (test-approx "sin^2+cos^2=1 real" 1.0
    (real-part (+ (* s s) (* c c))) 1e-10)
  (test-approx "sin^2+cos^2=1 imag" 0.0
    (imag-part (+ (* s s) (* c c))) 1e-10))

;; tan = sin/cos
(test-approx "tan=sin/cos real"
  (real-part (/ (sin z1) (cos z1)))
  (real-part (tan z1)) 1e-10)
(test-approx "tan=sin/cos imag"
  (imag-part (/ (sin z1) (cos z1)))
  (imag-part (tan z1)) 1e-10)

;; asin, acos, atan return complex
(test-true "asin complex?" (not (real? (asin z1))))
(test-true "acos complex?" (not (real? (acos z1))))
(test-true "atan complex?" (not (real? (atan z1))))

;;; ============================================================
;;; 10. exact->inexact / inexact->exact (PARTIAL)
;;; ============================================================

(section "complex exact/inexact conversion")

;; exact->inexact on exact complex should make parts inexact
;; exact->inexact should make parts inexact
(test-true "exact->inexact complex is inexact"
  (inexact? (exact->inexact z1)))

;; inexact->exact should make parts exact
(test-true "inexact->exact complex is exact"
  (exact? (inexact->exact (make-rectangular 3.0 4.0))))

;;; ============================================================
;;; 11. number->string (GAP — should work)
;;; ============================================================

(section "complex number->string")

(test "number->string 3+4i" "3+4i" (number->string z1))
(test "number->string +i"  "0+1i" (number->string zi))

;;; ============================================================
;;; 12. Operations that should correctly error on complex
;;; ============================================================

(section "correctly errors on complex")

(test-error "min complex"       (lambda () (min z1 z2)))
(test-error "max complex"       (lambda () (max z1 z2)))
(test-error "quotient complex"  (lambda () (quotient z1 1)))
(test-error "remainder complex" (lambda () (remainder z1 1)))
(test-error "modulo complex"    (lambda () (modulo z1 1)))
(test-error "gcd complex"       (lambda () (gcd z1 1)))
(test-error "floor complex"     (lambda () (floor z1)))
(test-error "ceiling complex"   (lambda () (ceiling z1)))
(test-error "truncate complex"  (lambda () (truncate z1)))
(test-error "round complex"     (lambda () (round z1)))

;;; ============================================================
;;; 13. Mixed complex/real arithmetic in loops
;;; ============================================================

(section "complex arithmetic in loops")

;; Sum (k + ki) for k=1..10
(test "complex sum loop"
  (make-rectangular 55 55)
  (let loop ((k 1) (acc 0))
    (if (> k 10) acc
        (loop (+ k 1) (+ acc (make-rectangular k k))))))

;; Repeated multiplication: (1+i)^10
(test-no-error "(1+i)^10 loop"
  (lambda ()
    (let loop ((k 0) (acc 1))
      (if (= k 10) acc
          (loop (+ k 1) (* acc (make-rectangular 1 1)))))))

;; Magnitude in loop
(test-no-error "magnitudes in loop"
  (lambda ()
    (let loop ((k 1) (sum 0.0))
      (if (> k 5) sum
          (loop (+ k 1)
                (+ sum (magnitude (make-rectangular k k))))))))

(test-summary "COMPLEX-MATH")
