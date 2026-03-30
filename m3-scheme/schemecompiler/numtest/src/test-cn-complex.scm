;;; test-cn-complex.scm --- Compiler numerics: complex number tests



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

(section "CN-COMPLEX: predicates")

(test-true  "complex? (make-rectangular 1 2)"  (complex? (make-rectangular 1 2)))
(test-true  "number? (make-rectangular 1 2)"   (number? (make-rectangular 1 2)))
(test-false "real? (make-rectangular 1 2)"      (real? (make-rectangular 1 2)))
(test-false "integer? (make-rectangular 1 2)"   (integer? (make-rectangular 1 2)))

;; Demotion: im=0 -> real
(test       "(make-rectangular 5 0) = 5"  5  (make-rectangular 5 0))
(test-true  "integer? (make-rectangular 5 0)" (integer? (make-rectangular 5 0)))

;; Reader syntax
(test-true  "complex? 3+4i"         (complex? 3+4i))
(test-true  "complex? -1+2i"        (complex? -1+2i))
(test-true  "complex? +i"           (complex? +i))
(test-true  "complex? -i"           (complex? -i))

(section "CN-COMPLEX: real-part, imag-part")

(test "real-part 3+4i"   3  (real-part (make-rectangular 3 4)))
(test "imag-part 3+4i"   4  (imag-part (make-rectangular 3 4)))
(test "real-part +i"     0  (real-part +i))
(test "imag-part +i"     1  (imag-part +i))
(test "imag-part -i"     -1 (imag-part -i))
(test "real-part 5"      5  (real-part 5))
(test "imag-part 5"      0  (imag-part 5))

(section "CN-COMPLEX: arithmetic")

(test "(+ 1+2i 3+4i)"
  (make-rectangular 4 6)
  (+ (make-rectangular 1 2) (make-rectangular 3 4)))

(test "(+ 1+2i 5)"
  (make-rectangular 6 2)
  (+ (make-rectangular 1 2) 5))

(test "(- 3+4i 1+2i)"
  (make-rectangular 2 2)
  (- (make-rectangular 3 4) (make-rectangular 1 2)))

;; (1+2i)(3+4i) = -5+10i
(test "(* 1+2i 3+4i)"
  (make-rectangular -5 10)
  (* (make-rectangular 1 2) (make-rectangular 3 4)))

;; (1+2i)/(3+4i) = 11/25 + 2/25 i
(test "(/ 1+2i 3+4i)"
  (make-rectangular 11/25 2/25)
  (/ (make-rectangular 1 2) (make-rectangular 3 4)))

;; Magnitude and angle
(test "magnitude 3+4i"   5.0  (magnitude (make-rectangular 3 4)))
(test "magnitude -5"     5    (magnitude -5))
(test-true "number? (angle 1+1i)" (number? (angle (make-rectangular 1 1))))

(section "CN-COMPLEX: comparisons")

(test-error "(< 1+2i 3+4i)" (lambda () (< (make-rectangular 1 2) (make-rectangular 3 4))))
(test-true  "(= 1+2i 1+2i)"  (= (make-rectangular 1 2) (make-rectangular 1 2)))
(test-false "(= 1+2i 1+3i)"  (= (make-rectangular 1 2) (make-rectangular 1 3)))

(section "CN-COMPLEX: loops")

;; (1+i)^2 = 2i
(test "(1+i)^2 = 2i"
  (make-rectangular 0 2)
  (* (make-rectangular 1 1) (make-rectangular 1 1)))

;; (1+i)^4 = -4 via loop
(test "(1+i)^4 = -4"
  -4
  (let loop ((i 0) (z (make-rectangular 1 1)))
    (if (= i 2) z
        (loop (+ i 1) (* z z)))))

;; Complex series sum
(test "complex series sum"
  (make-rectangular (+ 1 1/2 1/3 1/4 1/5) (+ 1 1/2 1/3 1/4 1/5))
  (let loop ((k 1) (acc 0))
    (if (> k 5) acc
        (loop (+ k 1) (+ acc (make-rectangular (/ 1 k) (/ 1 k)))))))

(section "CN-COMPLEX: with rationals")

(test-true "exact? (make-rectangular 1/3 2/5)"
  (exact? (make-rectangular 1/3 2/5)))

(section "CN-COMPLEX: closures")

(define (make-adder n) (lambda (x) (+ n x)))
(define add-i (make-adder (make-rectangular 0 1)))
(test "closure over +i"
  (make-rectangular 3 1)
  (add-i 3))

(test-summary "CN-COMPLEX")
