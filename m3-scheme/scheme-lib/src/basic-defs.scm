;;
;; $Id$
;;

(define (nth lst n)
  (if (= n 0) (car lst) (nth (cdr lst) (- n 1))))

(define (mem? elem-eq? x list)
  (cond ((null? list) #f)
        ((elem-eq? x (car list)) #t)
        (else (mem? elem-eq? x (cdr list)))))

(define (member? x list) (mem? equal? x list))

(define (map2 op-not-last op-last lst)
  ;;
  ;; Like map (mapcar) but with a different operation for the last
  ;; element.  Especially useful for infix code generation!
  ;;
  (cond ((null? lst) '())
				((null? (cdr lst)) (cons (op-last (car lst))
																 (map2 op-not-last op-last (cdr lst))))
				(else (cons (op-not-last (car lst))
										(map2 op-not-last op-last (cdr lst))))))

(define (uniq elem-eq? lst)
  ;; slow... you can do better with mergesort and a unix-style uniq.
  (define (iter L M)
    (cond ((null? L) M)
	  ((mem? elem-eq? (car L) M) (iter (cdr L) M))
	  (else (iter (cdr L) (cons (car L) M)))))

  (iter lst '()))


(define (filter x? lst)
  (cond ((null? lst) '())
	((not (x? (car lst))) (filter x? (cdr lst)))
	(else (cons (car lst) (filter x? (cdr lst))))))

(define (filter x? lst)
  (let loop ((rest lst)
             (res '()))
     (cond ((null? rest) (reverse res))
           ((not (x? (car rest))) (loop (cdr rest) res))
           (else (loop (cdr rest) (cons (car rest) res))))))

(define (accumulate op initial sequence)
  ;; SICP2 p. 116
  (if (null? sequence) initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; quick hacks, not very efficient
(define (all-except-last  lst)
  (if (null? (cdr lst)) '()
      (cons (car lst) (all-except-last (cdr lst)))))

(define (last lst)
  (if (null? (cdr lst)) (car lst) (last (cdr lst))))

(define (identity x) x)

(define (head n lst)
	(if (= n 0) 
			'()
			(cons (car lst)
						(head (- n 1) (cdr lst)))))

(define (tail n lst)
	(reverse (head n (reverse lst))))

(require-modules "basic-mbe")
