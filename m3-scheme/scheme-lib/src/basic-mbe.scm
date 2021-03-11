;; $Id$
;;
;; from mbe.scm from norvig.com

(define reverse!
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
          (let ((d (cdr s)))
            (set-cdr! s r)
            (loop d s))))))

(define append!
  (lambda (l1 l2)
    (cond ((null? l1) l2)
          ((null? l2) l1)
          (else (let loop ((l1 l1))
                  (if (null? (cdr l1))
                      (set-cdr! l1 l2)
                      (loop (cdr l1))))
                l1))))

(define list-tail
  (lambda (s i)
    (let loop ((s s) (i i))
      (if (= i 0) s
          (loop (cdr s) (+ i 1))))))

(define ormap
  (lambda (f l)
    (let loop ((l l))
      (if (null? l) #f
          (or (f (car l)) (loop (cdr l)))))))

(define andmap
  (lambda (f l)
    (let loop ((l l))
      (if (null? l) #t
          (and (f (car l) (loop (cdr l))))))))

