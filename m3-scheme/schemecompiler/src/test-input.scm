;;; Test input for MScheme Level 1 Compiler

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (loop-sum n acc)
  (if (= n 0)
      acc
      (loop-sum (- n 1) (+ acc n))))

(define (my-length lst)
  (if (null? lst)
      0
      (+ 1 (my-length (cdr lst)))))
