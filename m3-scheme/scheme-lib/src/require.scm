;;
;; $Id$
;;
;; require.scm -- simple module system
;;

(define require-modules '())

(let ((a-noticeable-symbol 0)) )

(let ((loaded-modules '()))

	(define (mem? elem-eq? x list)
		(cond ((null? list) #f)
					((elem-eq? x (car list)) #t)
					(else (mem? elem-eq? x (cdr list)))))
	
	(define (rq . args)
		(define (iter lst)
			(cond ((null? lst) #t)
						((mem? equal? (car lst) loaded-modules) (iter (cdr lst)))
						(else 
						 ;; note we have to update loaded-modules first,
						 ;; or else a module that requires itself would go into
						 ;; an infinite loop
						 (set! loaded-modules (cons (car lst) loaded-modules))
						 (load (car lst))
						 (iter (cdr lst)))))
		(iter args))

	(set! require-modules rq))

				
