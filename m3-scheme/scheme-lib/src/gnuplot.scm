;; 
;; $Id$
;;
;; basic Scheme definitions for GNUPLOT
;;

(require-modules "basic-defs" "display")

(define (make-plot lst nam)
	(define (iter lst port)
		(if (null? lst) #t
				(begin (dis (car lst) dnl port)
							 (iter (cdr lst) port))))

	
	(let ((p (open-output-file nam)))
		(iter lst p)
		(close-output-port p)))
