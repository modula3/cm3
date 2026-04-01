;;; test-dual.scm --- Comprehensive dual number acceptance tests
;;;
;;; Tests for SchemeDual.T — dual numbers (a + bε, ε²=0) for
;;; automatic differentiation.  Written against the spec in
;;; dual-numbers-proposal.tex.  All tests should pass when the
;;; implementation is complete.
;;;
;;; Usage: mscheme test-dual.scm

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

;; Dual-aware approx: compare both parts
(define (test-dual-approx name expected-re expected-eps actual tolerance)
  (set! test-count (+ test-count 2))
  (let ((are (dual-real actual))
        (aeps (dual-epsilon actual)))
    (if (approx= expected-re are tolerance)
        (begin (set! pass-count (+ pass-count 1))
               (display "PASS: ") (display name) (display " [re]") (newline))
        (begin (set! fail-count (+ fail-count 1))
               (display "FAIL: ") (display name) (display " [re]")
               (display "  expected~=") (display expected-re)
               (display "  actual=") (display are) (newline)))
    (if (approx= expected-eps aeps tolerance)
        (begin (set! pass-count (+ pass-count 1))
               (display "PASS: ") (display name) (display " [eps]") (newline))
        (begin (set! fail-count (+ fail-count 1))
               (display "FAIL: ") (display name) (display " [eps]")
               (display "  expected~=") (display expected-eps)
               (display "  actual=") (display aeps) (newline)))))

;; Helper: compute derivative of f at x
(define (derivative f x)
  (dual-epsilon (f (make-dual x 1))))

(define (test-summary label)
  (newline)
  (display "============================================================") (newline)
  (display label) (display ": ")
  (display pass-count) (display "/") (display test-count) (display " passed")
  (if (> fail-count 0)
      (begin (display ", ") (display fail-count) (display " FAILED")))
  (newline)
  (display "============================================================") (newline))

;;; ============================================================
;;; 1. Construction and demotion
;;; ============================================================

(section "construction and demotion")

;; Basic construction
(test-true "dual? (make-dual 3 4)" (dual? (make-dual 3 4)))
(test "dual-real (make-dual 3 4)" 3 (dual-real (make-dual 3 4)))
(test "dual-epsilon (make-dual 3 4)" 4 (dual-epsilon (make-dual 3 4)))

;; Demotion: zero epsilon -> not dual
(test "make-dual 5 0 = 5" 5 (make-dual 5 0))
(test-false "dual? (make-dual 5 0)" (dual? (make-dual 5 0)))
(test-true "integer? (make-dual 5 0)" (integer? (make-dual 5 0)))

;; Demotion with exact zero
(test "make-dual 3.14 0 = 3.14" 3.14 (make-dual 3.14 0))
(test-false "dual? (make-dual 3.14 0)" (dual? (make-dual 3.14 0)))

;; No demotion with inexact zero
;; (make-dual 3 0.0) — should this demote? Spec says "exactly zero"
(test-true "dual? (make-dual 3 0.0)" (dual? (make-dual 3 0.0)))

;; Various part types
(test-true "dual? fixnum/fixnum" (dual? (make-dual 3 4)))
(test-true "dual? rational/rational" (dual? (make-dual 1/3 2/5)))
(test-true "dual? float/float" (dual? (make-dual 3.14 2.71)))
(test-true "dual? bignum/fixnum" (dual? (make-dual (expt 2 100) 1)))
(test-true "dual? complex/fixnum" (dual? (make-dual (make-rectangular 1 2) 1)))
(test-true "dual? fixnum/complex" (dual? (make-dual 1 (make-rectangular 1 2))))
(test-true "dual? complex/complex" (dual? (make-dual (make-rectangular 1 2) (make-rectangular 3 4))))
(test-true "dual? rational/float" (dual? (make-dual 1/3 3.14)))

;;; ============================================================
;;; 2. Accessors
;;; ============================================================

(section "accessors")

;; dual-real on duals
(test "dual-real fixnum" 3 (dual-real (make-dual 3 4)))
(test "dual-real rational" 1/3 (dual-real (make-dual 1/3 1)))
(test "dual-real float" 3.14 (dual-real (make-dual 3.14 1)))
(test "dual-real bignum" (expt 2 100) (dual-real (make-dual (expt 2 100) 1)))
(test "dual-real complex" (make-rectangular 1 2) (dual-real (make-dual (make-rectangular 1 2) 1)))

;; dual-epsilon on duals
(test "dual-epsilon fixnum" 4 (dual-epsilon (make-dual 3 4)))
(test "dual-epsilon rational" 2/5 (dual-epsilon (make-dual 1 2/5)))
(test "dual-epsilon float" 2.71 (dual-epsilon (make-dual 1 2.71)))
(test "dual-epsilon complex" (make-rectangular 3 4) (dual-epsilon (make-dual 1 (make-rectangular 3 4))))

;; dual-real on non-duals (identity)
(test "dual-real 42" 42 (dual-real 42))
(test "dual-real 1/3" 1/3 (dual-real 1/3))
(test "dual-real 3.14" 3.14 (dual-real 3.14))
(test "dual-real 3+4i" (make-rectangular 3 4) (dual-real (make-rectangular 3 4)))

;; dual-epsilon on non-duals (zero)
(test "dual-epsilon 42" 0 (dual-epsilon 42))
(test "dual-epsilon 3.14" 0 (dual-epsilon 3.14))
(test "dual-epsilon 1/3" 0 (dual-epsilon 1/3))
(test "dual-epsilon 3+4i" 0 (dual-epsilon (make-rectangular 3 4)))

;;; ============================================================
;;; 3. Predicates
;;; ============================================================

(section "predicates")

(define d1 (make-dual 3 4))

(test-true  "number? dual" (number? d1))
(test-true  "dual? dual" (dual? d1))
(test-false "complex? dual" (complex? d1))
(test-false "real? dual" (real? d1))
(test-false "rational? dual" (rational? d1))
(test-false "integer? dual" (integer? d1))

;; exact?/inexact? depends on parts
(test-true  "exact? (make-dual 3 4)" (exact? (make-dual 3 4)))
(test-true  "exact? (make-dual 1/3 1/2)" (exact? (make-dual 1/3 1/2)))
(test-false "exact? (make-dual 3.0 4)" (exact? (make-dual 3.0 4)))
(test-false "exact? (make-dual 3 4.0)" (exact? (make-dual 3 4.0)))
(test-true  "inexact? (make-dual 3.0 4.0)" (inexact? (make-dual 3.0 4.0)))
(test-false "inexact? (make-dual 3 4)" (inexact? (make-dual 3 4)))

;; dual? on non-duals
(test-false "dual? 42" (dual? 42))
(test-false "dual? 3.14" (dual? 3.14))
(test-false "dual? 1/3" (dual? 1/3))
(test-false "dual? 3+4i" (dual? (make-rectangular 3 4)))
(test-false "dual? #t" (dual? #t))
(test-false "dual? \"hello\"" (dual? "hello"))

;;; ============================================================
;;; 4. Dual + dual arithmetic — fixnum parts
;;; ============================================================

(section "dual+dual arithmetic: fixnum parts")

(test "(make-dual 3 4) + (make-dual 1 2)"
  (make-dual 4 6) (+ (make-dual 3 4) (make-dual 1 2)))
(test "(make-dual 3 4) - (make-dual 1 2)"
  (make-dual 2 2) (- (make-dual 3 4) (make-dual 1 2)))
(test "(make-dual 3 4) * (make-dual 1 2)"
  (make-dual 3 10) (* (make-dual 3 4) (make-dual 1 2)))
;; (3+4e)/(1+2e) = 3/1 + (4*1-3*2)/1^2 e = 3 + (-2)e
(test "(make-dual 3 4) / (make-dual 1 2)"
  (make-dual 3 -2) (/ (make-dual 3 4) (make-dual 1 2)))
(test "- (make-dual 3 4)"
  (make-dual -3 -4) (- (make-dual 3 4)))

;;; ============================================================
;;; 5. Dual + dual arithmetic — rational parts
;;; ============================================================

(section "dual+dual arithmetic: rational parts")

(test "(make-dual 1/3 1/2) + (make-dual 1/6 1/4)"
  (make-dual 1/2 3/4) (+ (make-dual 1/3 1/2) (make-dual 1/6 1/4)))
(test "(make-dual 1/2 1/3) * (make-dual 1/4 1/5)"
  (make-dual 1/8 (+ (* 1/2 1/5) (* 1/3 1/4)))
  (* (make-dual 1/2 1/3) (make-dual 1/4 1/5)))
(test "rational dual stays exact"
  #t (exact? (+ (make-dual 1/3 1/2) (make-dual 1/6 1/4))))

;;; ============================================================
;;; 6. Dual + dual arithmetic — float parts
;;; ============================================================

(section "dual+dual arithmetic: float parts")

(test-dual-approx "(make-dual 3.0 4.0) + (make-dual 1.0 2.0)"
  4.0 6.0 (+ (make-dual 3.0 4.0) (make-dual 1.0 2.0)) 1e-10)
(test-dual-approx "(make-dual 3.0 4.0) * (make-dual 2.0 1.0)"
  6.0 11.0 (* (make-dual 3.0 4.0) (make-dual 2.0 1.0)) 1e-10)

;;; ============================================================
;;; 7. Dual + dual arithmetic — complex parts
;;; ============================================================

(section "dual+dual arithmetic: complex parts")

(let ((d1 (make-dual (make-rectangular 1 2) (make-rectangular 3 4)))
      (d2 (make-dual (make-rectangular 5 6) (make-rectangular 7 8))))
  (test "complex dual add re"
    (make-rectangular 6 8) (dual-real (+ d1 d2)))
  (test "complex dual add eps"
    (make-rectangular 10 12) (dual-epsilon (+ d1 d2)))
  ;; mul: (1+2i)(5+6i) = -7+16i ; eps = (1+2i)(7+8i) + (3+4i)(5+6i)
  ;; = (-9+22i) + (-9+38i) = (-18+60i)... let me just test no-error
  (test-no-error "complex dual mul" (lambda () (* d1 d2))))

;;; ============================================================
;;; 8. Dual + non-dual arithmetic (all non-dual types)
;;; ============================================================

(section "dual + non-dual arithmetic")

(define d (make-dual 3 4))

;; dual + fixnum
(test "dual + fixnum" (make-dual 8 4) (+ d 5))
(test "fixnum + dual" (make-dual 8 4) (+ 5 d))
(test "dual * fixnum" (make-dual 6 8) (* d 2))
(test "fixnum * dual" (make-dual 6 8) (* 2 d))
(test "dual / fixnum" (make-dual 3/2 2) (/ d 2))
(test "dual - fixnum" (make-dual -2 4) (- d 5))

;; dual + rational
(test "dual + 1/3" (make-dual 10/3 4) (+ d 1/3))
(test "1/3 + dual" (make-dual 10/3 4) (+ 1/3 d))
(test "dual * 1/3" (make-dual 1 4/3) (* d 1/3))

;; dual + float
(test-dual-approx "dual + 3.14" 6.14 4.0 (+ d 3.14) 1e-10)
(test-dual-approx "3.14 + dual" 6.14 4.0 (+ 3.14 d) 1e-10)

;; dual + bignum
(test "dual + 2^100"
  (+ 3 (expt 2 100)) (dual-real (+ d (expt 2 100))))
(test "dual + 2^100 eps"
  4 (dual-epsilon (+ d (expt 2 100))))

;; dual + complex
(let ((r (+ d (make-rectangular 1 2))))
  (test "dual + complex re" (make-rectangular 4 2) (dual-real r))
  (test "dual + complex eps" 4 (dual-epsilon r)))

;;; ============================================================
;;; 9. Equality
;;; ============================================================

(section "equality")

(test-true  "(= d d)" (= (make-dual 3 4) (make-dual 3 4)))
(test-false "(= d d')" (= (make-dual 3 4) (make-dual 3 5)))
(test-false "(= d d'')" (= (make-dual 3 4) (make-dual 4 4)))
(test-true  "(= rational duals)" (= (make-dual 1/3 1/2) (make-dual 1/3 1/2)))

;;; ============================================================
;;; 10. Ordering errors
;;; ============================================================

(section "ordering errors")

(test-error "< dual" (lambda () (< (make-dual 1 2) (make-dual 3 4))))
(test-error "> dual" (lambda () (> (make-dual 1 2) (make-dual 3 4))))
(test-error "<= dual" (lambda () (<= (make-dual 1 2) (make-dual 3 4))))
(test-error ">= dual" (lambda () (>= (make-dual 1 2) (make-dual 3 4))))
(test-error "min dual" (lambda () (min (make-dual 1 2) (make-dual 3 4))))
(test-error "max dual" (lambda () (max (make-dual 1 2) (make-dual 3 4))))

;;; ============================================================
;;; 11. zero?
;;; ============================================================

(section "zero?")

(test-false "zero? (make-dual 3 4)" (zero? (make-dual 3 4)))
(test-false "zero? (make-dual 0 1)" (zero? (make-dual 0 1)))
(test-true  "zero? (make-dual 0 0)" (zero? (make-dual 0 0)))
;; But make-dual 0 0 demotes to 0, so this is really just zero? on 0
;; (make-dual 0 0.0) doesn't demote (inexact zero) but is mathematically zero
(test-true  "zero? (make-dual 0 0.0)" (zero? (make-dual 0 0.0)))
(test-false "zero? (make-dual 1 0.0)" (zero? (make-dual 1 0.0)))

;;; ============================================================
;;; 12. positive?, negative? (error on dual)
;;; ============================================================

(section "positive?/negative? errors")

(test-error "positive? dual" (lambda () (positive? (make-dual 3 4))))
(test-error "negative? dual" (lambda () (negative? (make-dual -3 4))))

;;; ============================================================
;;; 13. Real-only operations (error on dual)
;;; ============================================================

(section "real-only operations error on dual")

(test-error "floor dual" (lambda () (floor (make-dual 3.5 1))))
(test-error "ceiling dual" (lambda () (ceiling (make-dual 3.5 1))))
(test-error "truncate dual" (lambda () (truncate (make-dual 3.5 1))))
(test-error "round dual" (lambda () (round (make-dual 3.5 1))))
(test-error "quotient dual" (lambda () (quotient (make-dual 7 1) 2)))
(test-error "remainder dual" (lambda () (remainder (make-dual 7 1) 2)))
(test-error "modulo dual" (lambda () (modulo (make-dual 7 1) 2)))
(test-error "gcd dual" (lambda () (gcd (make-dual 6 1) 4)))
(test-error "even? dual" (lambda () (even? (make-dual 4 1))))
(test-error "odd? dual" (lambda () (odd? (make-dual 3 1))))

;;; ============================================================
;;; 14. abs on dual
;;; ============================================================

(section "abs on dual")

;; abs(a + be) = |a| + b*sign(a)*e
(test "abs (make-dual 3 4)" (make-dual 3 4) (abs (make-dual 3 4)))
(test "abs (make-dual -3 4)" (make-dual 3 -4) (abs (make-dual -3 4)))
(test "abs (make-dual -5 -2)" (make-dual 5 2) (abs (make-dual -5 -2)))
;; abs on dual with complex parts: error (like complex)
(test-error "abs complex dual"
  (lambda () (abs (make-dual (make-rectangular 1 2) 1))))

;;; ============================================================
;;; 15. Transcendentals — derivative verification
;;;
;;; For f(a + be) = f(a) + b*f'(a)*e, we test with b=1 so
;;; dual-epsilon gives f'(a) directly.
;;; ============================================================

(section "transcendentals: exp")

;; exp(a + e) = exp(a) + exp(a)*e
(test-dual-approx "exp(1+e)" (exp 1) (exp 1)
  (exp (make-dual 1 1)) 1e-10)
(test-dual-approx "exp(0+e)" 1.0 1.0
  (exp (make-dual 0 1)) 1e-10)
(test-dual-approx "exp(2+3e)" (exp 2) (* 3 (exp 2))
  (exp (make-dual 2 3)) 1e-10)

(section "transcendentals: log")

;; log(a + be) = log(a) + b/a * e
(test-dual-approx "log(1+e)" 0.0 1.0
  (log (make-dual 1 1)) 1e-10)
(test-dual-approx "log(2+e)" (log 2) 0.5
  (log (make-dual 2 1)) 1e-10)
(test-dual-approx "log(3+2e)" (log 3) 2/3
  (log (make-dual 3 2)) 1e-10)

(section "transcendentals: sin")

;; sin(a + be) = sin(a) + b*cos(a)*e
(test-dual-approx "sin(0+e)" 0.0 1.0
  (sin (make-dual 0 1)) 1e-10)
(test-dual-approx "sin(pi/2+e)" 1.0 0.0
  (sin (make-dual 1.5707963267948966 1)) 1e-6)
(test-dual-approx "sin(1+e)" (sin 1) (cos 1)
  (sin (make-dual 1 1)) 1e-10)

(section "transcendentals: cos")

;; cos(a + be) = cos(a) - b*sin(a)*e
(test-dual-approx "cos(0+e)" 1.0 0.0
  (cos (make-dual 0 1)) 1e-10)
(test-dual-approx "cos(1+e)" (cos 1) (- (sin 1))
  (cos (make-dual 1 1)) 1e-10)

(section "transcendentals: tan")

;; tan(a + be) = tan(a) + b/cos^2(a) * e
(test-dual-approx "tan(0+e)" 0.0 1.0
  (tan (make-dual 0 1)) 1e-10)
(test-dual-approx "tan(1+e)" (tan 1) (/ 1.0 (* (cos 1) (cos 1)))
  (tan (make-dual 1 1)) 1e-10)

(section "transcendentals: sqrt")

;; sqrt(a + be) = sqrt(a) + b/(2*sqrt(a)) * e
(test-dual-approx "sqrt(4+e)" 2.0 0.25
  (sqrt (make-dual 4 1)) 1e-10)
(test-dual-approx "sqrt(9+e)" 3.0 (/ 1.0 6.0)
  (sqrt (make-dual 9 1)) 1e-10)
(test-dual-approx "sqrt(2+3e)" (sqrt 2) (/ 3.0 (* 2 (sqrt 2)))
  (sqrt (make-dual 2 3)) 1e-10)

(section "transcendentals: expt")

;; (a+be)^n = a^n + n*b*a^(n-1) * e  (integer n)
(test "expt dual 2" (make-dual 9 24) (expt (make-dual 3 4) 2))
;; 3^2=9, 2*4*3=24
(test "expt dual 3" (make-dual 27 108) (expt (make-dual 3 4) 3))
;; 3^3=27, 3*4*3^2=108
(test-dual-approx "expt dual 0.5 (=sqrt)"
  (sqrt 4.0) (/ 1.0 (* 2 (sqrt 4.0)))
  (expt (make-dual 4 1) 0.5) 1e-10)

(section "transcendentals: asin")

;; asin'(a) = 1/sqrt(1-a^2)
(test-dual-approx "asin(0+e)" 0.0 1.0
  (asin (make-dual 0 1)) 1e-10)
(test-dual-approx "asin(0.5+e)" (asin 0.5) (/ 1.0 (sqrt (- 1 (* 0.5 0.5))))
  (asin (make-dual 0.5 1)) 1e-10)

(section "transcendentals: acos")

;; acos'(a) = -1/sqrt(1-a^2)
(test-dual-approx "acos(0+e)" (acos 0) -1.0
  (acos (make-dual 0 1)) 1e-10)
(test-dual-approx "acos(0.5+e)" (acos 0.5) (/ -1.0 (sqrt (- 1 (* 0.5 0.5))))
  (acos (make-dual 0.5 1)) 1e-10)

(section "transcendentals: atan")

;; atan'(a) = 1/(1+a^2)
(test-dual-approx "atan(0+e)" 0.0 1.0
  (atan (make-dual 0 1)) 1e-10)
(test-dual-approx "atan(1+e)" (atan 1) 0.5
  (atan (make-dual 1 1)) 1e-10)

;;; ============================================================
;;; 16. Transcendentals with exact parts
;;; ============================================================

(section "transcendentals: exact dual parts")

;; exp with rational part
(test-no-error "exp (make-dual 1/2 1)"
  (lambda () (exp (make-dual 1/2 1))))
(test-approx "exp(1/2) re" (exp 0.5)
  (dual-real (exp (make-dual 1/2 1))) 1e-10)

;; sin with rational part
(test-no-error "sin (make-dual 1/3 1)"
  (lambda () (sin (make-dual 1/3 1))))

;; sqrt with rational
(test-no-error "sqrt (make-dual 2 1/3)"
  (lambda () (sqrt (make-dual 2 1/3))))

;;; ============================================================
;;; 17. Transcendentals with complex parts
;;; ============================================================

(section "transcendentals: complex dual parts")

;; exp of dual with complex real part
(test-no-error "exp (make-dual 1+2i 1)"
  (lambda () (exp (make-dual (make-rectangular 1 2) 1))))

;; sin of dual with complex real part
(test-no-error "sin (make-dual 1+2i 1)"
  (lambda () (sin (make-dual (make-rectangular 1 2) 1))))

;; sqrt of dual with complex real part
(test-no-error "sqrt (make-dual 1+2i 1)"
  (lambda () (sqrt (make-dual (make-rectangular 1 2) 1))))

;; Complex epsilon part
(test-no-error "exp (make-dual 1 1+2i)"
  (lambda () (exp (make-dual 1 (make-rectangular 1 2)))))

;;; ============================================================
;;; 18. Automatic differentiation — known derivatives
;;; ============================================================

(section "AD: polynomial derivatives")

;; d/dx (x^2) = 2x
(test "d/dx x^2 at 3" 6 (derivative (lambda (x) (* x x)) 3))
(test "d/dx x^2 at 0" 0 (derivative (lambda (x) (* x x)) 0))
(test "d/dx x^2 at -2" -4 (derivative (lambda (x) (* x x)) -2))

;; d/dx (x^3) = 3x^2
(test "d/dx x^3 at 2" 12 (derivative (lambda (x) (* x x x)) 2))

;; d/dx (3x^2 + 2x + 1) = 6x + 2
(test "d/dx 3x^2+2x+1 at 1" 8
  (derivative (lambda (x) (+ (* 3 x x) (* 2 x) 1)) 1))
(test "d/dx 3x^2+2x+1 at 0" 2
  (derivative (lambda (x) (+ (* 3 x x) (* 2 x) 1)) 0))

;; With rational input
(test "d/dx x^2 at 1/3" 2/3
  (derivative (lambda (x) (* x x)) 1/3))

(section "AD: transcendental derivatives")

;; d/dx exp(x) = exp(x)
(test-approx "d/dx exp at 0" 1.0
  (derivative exp 0) 1e-10)
(test-approx "d/dx exp at 1" (exp 1)
  (derivative exp 1) 1e-10)

;; d/dx log(x) = 1/x
(test-approx "d/dx log at 1" 1.0
  (derivative log 1) 1e-10)
(test-approx "d/dx log at 2" 0.5
  (derivative log 2) 1e-10)

;; d/dx sin(x) = cos(x)
(test-approx "d/dx sin at 0" 1.0
  (derivative sin 0) 1e-10)
(test-approx "d/dx sin at pi/2" 0.0
  (derivative sin 1.5707963267948966) 1e-6)

;; d/dx cos(x) = -sin(x)
(test-approx "d/dx cos at 0" 0.0
  (derivative cos 0) 1e-10)
(test-approx "d/dx cos at pi/2" -1.0
  (derivative cos 1.5707963267948966) 1e-6)

;; d/dx sqrt(x) = 1/(2*sqrt(x))
(test-approx "d/dx sqrt at 4" 0.25
  (derivative sqrt 4) 1e-10)
(test-approx "d/dx sqrt at 1" 0.5
  (derivative sqrt 1) 1e-10)

;; d/dx (1/x) = -1/x^2
(test "d/dx 1/x at 2" -1/4
  (derivative (lambda (x) (/ 1 x)) 2))
(test "d/dx 1/x at 1/3" -9
  (derivative (lambda (x) (/ 1 x)) 1/3))

(section "AD: chain rule")

;; d/dx sin(x^2) = 2x*cos(x^2)
(test-approx "d/dx sin(x^2) at 1"
  (* 2 (cos 1))
  (derivative (lambda (x) (sin (* x x))) 1) 1e-10)

;; d/dx exp(sin(x)) = cos(x)*exp(sin(x))
(test-approx "d/dx exp(sin(x)) at 0"
  (* (cos 0) (exp (sin 0)))
  (derivative (lambda (x) (exp (sin x))) 0) 1e-10)

;; d/dx log(x^2 + 1) = 2x/(x^2+1)
(test-approx "d/dx log(x^2+1) at 1"
  1.0  ;; 2*1/(1+1) = 1
  (derivative (lambda (x) (log (+ (* x x) 1))) 1) 1e-10)

(section "AD: complex function derivatives")

;; d/dz z^2 = 2z (complex derivative)
(test "d/dz z^2 at 1+2i"
  (make-rectangular 2 4)
  (derivative (lambda (z) (* z z)) (make-rectangular 1 2)))

;; d/dz z^3 = 3z^2
(let ((z (make-rectangular 1 1)))
  (test "d/dz z^3 at 1+i"
    (* 3 (* z z))
    (derivative (lambda (z) (* z z z)) z)))

;;; ============================================================
;;; 19. exact->inexact, inexact->exact
;;; ============================================================

(section "exact/inexact conversion")

;; exact->inexact on exact dual
(let ((d (exact->inexact (make-dual 3 4))))
  (test-true "exact->inexact dual is dual" (dual? d))
  (test-true "exact->inexact dual is inexact" (inexact? d))
  (test-approx "exact->inexact dual re" 3.0 (dual-real d) 1e-10)
  (test-approx "exact->inexact dual eps" 4.0 (dual-epsilon d) 1e-10))

;; exact->inexact on rational dual
(let ((d (exact->inexact (make-dual 1/3 1/2))))
  (test-true "exact->inexact rational dual is inexact" (inexact? d)))

;; inexact->exact on inexact dual
(let ((d (inexact->exact (make-dual 3.0 4.0))))
  (test-true "inexact->exact dual is exact" (exact? d))
  (test "inexact->exact dual re" 3 (dual-real d))
  (test "inexact->exact dual eps" 4 (dual-epsilon d)))

;; Already exact: identity
(test "inexact->exact on exact dual"
  (make-dual 3 4) (inexact->exact (make-dual 3 4)))

;;; ============================================================
;;; 20. number->string
;;; ============================================================

(section "number->string")

(test-no-error "number->string dual"
  (lambda () (number->string (make-dual 3 4))))
(test-no-error "number->string rational dual"
  (lambda () (number->string (make-dual 1/3 1/2))))
(test-no-error "number->string float dual"
  (lambda () (number->string (make-dual 3.14 2.71))))
(test-no-error "number->string complex dual"
  (lambda () (number->string (make-dual (make-rectangular 1 2) 3))))

;;; ============================================================
;;; 21. Interop roundtrip
;;; ============================================================

(section "interop roundtrip")

;; REFANY passthrough
(test "REFANY roundtrip"
  (make-dual 3 4)
  (interop-roundtrip 'REFANY (make-dual 3 4)))

;; SchemeDual_T roundtrip
(let ((d (make-dual 3 4)))
  (test-no-error "SchemeDual_T roundtrip"
    (lambda () (interop-roundtrip 'SchemeDual_T d)))
  (test "SchemeDual_T re"
    3 (dual-real (interop-roundtrip 'SchemeDual_T d)))
  (test "SchemeDual_T eps"
    4 (dual-epsilon (interop-roundtrip 'SchemeDual_T d))))

;; Rational parts roundtrip
(test-no-error "SchemeDual_T rational"
  (lambda () (interop-roundtrip 'SchemeDual_T (make-dual 1/3 2/5))))

;; Complex parts roundtrip
(test-no-error "SchemeDual_T complex"
  (lambda () (interop-roundtrip 'SchemeDual_T
    (make-dual (make-rectangular 1 2) (make-rectangular 3 4)))))

;; Error on real M3 types
(test-error "dual -> INTEGER" (lambda () (interop-roundtrip 'INTEGER (make-dual 3 4))))
(test-error "dual -> LONGREAL" (lambda () (interop-roundtrip 'LONGREAL (make-dual 3 4))))
(test-error "dual -> CARDINAL" (lambda () (interop-roundtrip 'CARDINAL (make-dual 3 4))))
(test-error "dual -> TEXT" (lambda () (interop-roundtrip 'TEXT (make-dual 3 4))))
(test-error "dual -> CHAR" (lambda () (interop-roundtrip 'CHAR (make-dual 3 4))))

;; BOOLEAN: TruthO — duals are truthy
(test-true "dual -> BOOLEAN" (interop-roundtrip 'BOOLEAN (make-dual 3 4)))

;;; ============================================================
;;; 22. Mixed dual/non-dual type matrix
;;; ============================================================

(section "type matrix: dual + every numeric type")

;; Ensure + works for dual with every numeric type (both orderings)
(define dtst (make-dual 1 1))

(test-true "dual + fixnum is dual" (dual? (+ dtst 5)))
(test-true "fixnum + dual is dual" (dual? (+ 5 dtst)))
(test-true "dual + bignum is dual" (dual? (+ dtst (expt 2 100))))
(test-true "bignum + dual is dual" (dual? (+ (expt 2 100) dtst)))
(test-true "dual + rational is dual" (dual? (+ dtst 1/3)))
(test-true "rational + dual is dual" (dual? (+ 1/3 dtst)))
(test-true "dual + float is dual" (dual? (+ dtst 3.14)))
(test-true "float + dual is dual" (dual? (+ 3.14 dtst)))
(test-true "dual + complex is dual" (dual? (+ dtst (make-rectangular 1 2))))
(test-true "complex + dual is dual" (dual? (+ (make-rectangular 1 2) dtst)))

;; Same for *
(test-true "dual * fixnum is dual" (dual? (* dtst 5)))
(test-true "fixnum * dual is dual" (dual? (* 5 dtst)))
(test-true "dual * rational is dual" (dual? (* dtst 1/3)))
(test-true "dual * complex is dual" (dual? (* dtst (make-rectangular 1 2))))

;; Same for /
(test-true "dual / fixnum is dual" (dual? (/ dtst 5)))
(test-true "dual / rational is dual" (dual? (/ dtst 1/3)))
(test-true "fixnum / dual is dual" (dual? (/ 5 dtst)))

;;; ============================================================
;;; 23. Loops with dual accumulators
;;; ============================================================

(section "loops with dual accumulators")

;; Sum of duals in a loop
(test "dual sum loop"
  (make-dual 55 55)
  (let loop ((k 1) (acc (make-dual 0 0.0)))  ;; 0.0 to avoid demotion
    (if (> k 10) acc
        (loop (+ k 1) (+ acc (make-dual k k))))))

;; Product of duals: (1+e)^10
;; (1+e)^n = 1 + n*e (since e^2=0)
(let ((result (let loop ((k 0) (acc (make-dual 1 0.0)))
               (if (= k 10) acc
                   (loop (+ k 1) (* acc (make-dual 1 1)))))))
  (test "dual-real (1+e)^10" 1 (dual-real result))
  (test "dual-epsilon (1+e)^10" 10 (dual-epsilon result)))

;; Derivative via loop: d/dx (sum_{k=0}^{5} x^k) = sum_{k=1}^{5} k*x^{k-1}
;; At x=2: 1 + 2*2 + 3*4 + 4*8 + 5*16 = 1+4+12+32+80 = 129
(test "AD: d/dx polynomial sum at 2" 129
  (derivative
    (lambda (x)
      (let loop ((k 0) (xk 1) (sum 0))
        (if (> k 5) sum
            (loop (+ k 1) (* xk x) (+ sum xk)))))
    2))

;;; ============================================================
;;; 24. Exactness preservation in dual arithmetic
;;; ============================================================

(section "exactness preservation")

(test-true "exact dual + exact dual is exact"
  (exact? (+ (make-dual 1 2) (make-dual 3 4))))
(test-true "exact dual * exact dual is exact"
  (exact? (* (make-dual 1/3 1/2) (make-dual 1/4 1/5))))
(test-false "exact dual + inexact is inexact"
  (exact? (+ (make-dual 1 2) 3.14)))
(test-true "rational dual stays exact"
  (exact? (* (make-dual 1/3 1/2) (make-dual 2/3 3/4))))

;;; ============================================================
;;; 25. Edge cases
;;; ============================================================

(section "edge cases")

;; Dual with zero real part
(test "make-dual 0 1" (make-dual 0 1) (make-dual 0 1))
(test-true "dual? (make-dual 0 1)" (dual? (make-dual 0 1)))

;; Dual times zero
(test "dual * 0" 0 (* (make-dual 3 4) 0))

;; Dual divided by itself = 1 + 0e (should demote to 1)
(let ((d (make-dual 3 4)))
  (test "d/d = 1" 1 (/ d d)))

;; Chain of operations preserving dual
(let ((d (make-dual 2 1)))
  ;; (2+e)^2 - 4*(2+e) + 4 = (4+4e) - (8+4e) + 4 = 0
  ;; Should be (make-dual 0 0) which demotes to 0
  (test "(x-2)^2 at x=2" 0
    (+ (- (* d d) (* 4 d)) 4)))

;;; ============================================================
;;; Summary
;;; ============================================================

(test-summary "DUAL NUMBERS")

(exit (if (= fail-count 0) 0 1))
