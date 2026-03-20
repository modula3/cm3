;;; Section 1.1.4

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))


;;; Section 1.1.6 -- several versions of ABS

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

;;; some arithmetic predicates

(define (>= x y)
  (or (> x y) (= x y)))

(define (>= x y)
  (not (< x y)))

;;; Exercise 1.3

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p))

;;; Section 1.1.7 -- square roots

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) .001))

(define (sqrt x)
  (sqrt-iter 1 x))

;;; Exercise 1.4

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

;;; Section 1.1.8

;;; another verson of SQUARE

(define (square x) 
  (exp (double (log x))))

(define (double x) (+ x x))

;;; Reinstate the simple version

(define (square x) (* x x))

;;; Block-structured SQRT

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) .001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1 x))

;;; Block-structured SQRT using lexical scoping

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) .001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1))

;;; Section 1.2.1

;;; Recursive FACTORIAL

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

;;; Iterative FACTORIAL

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

;;; Iterative FACTORIAL -- block-structured version

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;;; Exercise 1.7
(define (+ a b)
  (if (= a 0)
      b
      (1+ (+ (-1+ a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (-1+ a) (1+ b))))

;;; Exercise 1.8

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))


;;; Section 1.2.2

;;; Recursive FIB

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;;; Iterative FIB
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

;;; Counting change
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)
                 (cc amount
                     (- kinds-of-coins 1))))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

;;; Section 1.2.4 -- exponentiation

;;; Linear recursive version

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;;; Linear iterative version

(define (expt b n)
  (exp-iter b n 1))

(define (exp-iter b counter product)
  (if (= counter 0)
      product
      (exp-iter b
                (- counter 1)
                (* b product)))) 

;;; Logarithmic recursive version
(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

;;; Exercise 1.12

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

;;; Section 1.2.5 -- Greatest common divisor

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; Section 1.2.6 -- Primality

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e)
         (remainder (square (expmod b (/ e 2) m))
                    m))
        (else
         (remainder (* b (expmod b (- e 1) m))
                    m))))        

(define (fermat-test n)
  (define a (+ 2 (random (- n 2))))
  (= (expmod a n n) a))

(define (fast-prime? n times)
  (cond ((= times 0) t)
        ((fermat-test n)
         (fast-prime? n (- times 1)))
        (else nil)))

;;; Exercise 1.17

(define (timed-prime-test n)
  (define start-time (runtime))
  (define found-prime? (prime? n))
  (define elapsed-time (- (runtime) start-time))
  (print n)
  (cond (found-prime?
         (print " *** ")
         (print elapsed-time))))

;;; Exercise 1.20

(define (expmod base exp m)
  (remainder (fast-exp base exp) m))

;;; Exercise 1.21

(define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e)
         (remainder (* (expmod b (/ e 2) m)
                       (expmod b (/ e 2) m))
                    m))
        (else
         (remainder (* b (expmod b (- e 1) m))
                    m))))

;;; Section 1.3

(define (cube x) (* x x x))

;;; Section 1.3.1

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a 1+ b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b)
     dx))

;;; Section 1.3.2

(define (pi-sum a b)
  (sum (lambda (x) (/ 1 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2))
          (lambda (x) (+ x dx))
          b)
     dx))

;;; Four equivalent procedure definitions

(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))

(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y)) 
            (- 1 y)))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;;; Exercise 1.28

(define (f g)
  (g 2))

;;; Section 1.3.3

;;; Half-interval method

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? x y)
  (< (abs (- x y)) .001))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

;;; Golden section method

(define (reduce f a x y b fx fy)
  (cond ((close-enough? a b) x)
        ((> fx fy)
         (let ((new (x-point a y)))
           (reduce f a new x y (f new) fx)))
        (else
         (let ((new (y-point x b)))
           (reduce f x y new b fy (f new))))))

(define (x-point a b)
  (+ a (* golden-ratio-squared (- b a))))

(define (y-point a b)
  (+ a (* golden-ratio (- b a))))

(define golden-ratio
  (/ (- (sqrt 5) 1) 2))

(define golden-ratio-squared (square golden-ratio))

(define (golden f a b)
  (let ((x (x-point a b))
        (y (y-point a b)))
    (reduce f a x y b (f x) (f y))))

;;; Section 1.3.4

;;; Derivative of a function

(define (deriv f dx)
  (lambda (x)
    (/ (- (f (+ x dx)) (f x))
       dx)))

;;; Newton's method

(define (newton f guess)
  (if (good-enough? guess f)
      guess
      (newton f (improve guess f))))

(define (improve guess f)
  (- guess (/ (f guess)
              ((deriv f .001) guess))))

(define (good-enough? guess f)
  (< (abs (f guess)) .001))
