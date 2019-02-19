;;
;; $Id$
;;

;; time functions

(require-modules "basic-defs")

;; convert seconds to (weeks days hours minutes seconds):
;; (convert 234525 '(7 24 60 60))

(define (convert secs lst)
	
	(let loop ((v secs)
						 (p (reverse lst))
						 (res '()))

		(if (null? p) 
				(cons v res)
				(let ((m (modulo v (car p))))
					(loop (/ (- v m) (car p))
								(cdr p)
								(cons m res))))))
	

(define (seconds->human-time secs)
	(if (= secs 0) 
			"0s"
			(infixize
			 (filter (lambda(x) (not (null? x)))
							 (map (lambda (count suffix)
											(if (> count 0)
													(string-append (number->string count) suffix)
													'()))
										(convert secs '(7 24 60 60))
										'("w" "d" "h" "m" "s")))
			 ":")))

(define (infixize lst sop)
	(let loop ((first #t)
						 (res "")
						 (p lst))
		(if (null? p) 
				res
				(loop #f (string-append res (if first "" sop) (car p)) (cdr p)))))