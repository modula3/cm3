;;
;; $Id$
;;

(load "/home/mika/t/mscheme/scheme-lib/mergesort.scm")

(define (repeat cnt what)
  (if (= cnt 0) #t
      (begin (what) (repeat (- cnt 1) what))))

(define (job)
  (mergesort '(1 2 98 23 45 -100 3 3 3 19) <))

(define (timeit cnt what)
  (let ((start-time (timenow)))
    (repeat cnt what)
    (- (timenow) start-time)))

(define (x)
  (let ((s (timenow)))
    (- (timenow) s)))

(display (timeit 10000 job) '())