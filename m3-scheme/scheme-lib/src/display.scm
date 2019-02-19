;;
;; $Id$
;;

(require-modules "basic-defs")

(define dnl #\newline)

(define (dis . x)
	(let ((maybe-port (last x))
				(everything-else (accumulate string-append "" (all-except-last x))))
		(if (or (null? maybe-port) 
						(output-port? maybe-port))
				(display everything-else maybe-port)
				(display (string-append everything-else maybe-port)))))

