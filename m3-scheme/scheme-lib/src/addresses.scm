;; basic bignum code

(require-modules "hashtable")

(define (add-based-digits base dd0 dd1)

  (define (rest lst) (if (null? lst) '() (cdr lst)))

  (let loop ((result '())
	     (carry 0)
	     (d0 (reverse dd0))
	     (d1 (reverse dd1)))
    
    (define (loopit next)
      (loop (cons (modulo next base) result)
	    (div next base)
	    (rest d0)
	    (rest d1)))

    (cond ((and (null? d0) (null? d1)) 
	   (if (= 0 carry) result (cons carry result)))
	  ((null? d0) (loopit (+ carry (car d1))))
	  ((null? d1) (loopit (+ carry (car d0))))
	  (else       (loopit (+ carry (car d0) (car d1)))))))


(define (multiply-single-based-digits base a b)
  (list (div (* a b) base) (modulo (* a b) base)))

(define (multiply-by-based-digit base num digit)
  (let loop ((result '())
	     (r       (reverse num))
	     (carry   0))
    (if (null? r) 
	(if (= 0 carry) result (cons carry result))
	(let ((partial (+ carry (* (car r) digit))))
	  (loop (cons (modulo partial base) result)
		(cdr r)
		(div partial base))))))
	   

(define (multiply-based-digits base d0 d1)
  (let loop ((result '())
	     (r1 (reverse d1))
	     (trail '()))
    (if (null? r1) 
	result
	(loop (add-based-digits base 
				result 
				(append 
				 (multiply-by-based-digit base d0 (car r1))
				 trail))
	      (cdr r1)
	      (cons 0 trail))
	)
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (literal-to-based-digits literal base)
  ;; "small" base conversion
  (define (recurse literal)
    (if (= 0 literal) 
	'()
	(cons (modulo literal base)
	      (recurse (div literal base)))))
  (reverse (recurse literal)))

(define (make-number-list-hash-table size) 
  (make-hash-table size (lambda(n) (apply + n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (power-of-literal-in-base-impl literal n base)
  ;; small power of a small number in a given base
  (cond ((= 0 n) '(1))
	((= 1 n) (literal-to-based-digits literal base))
	(else (multiply-based-digits 
	       base
	       (power-of-literal-in-base literal (div n 2) base)
	       (power-of-literal-in-base literal (- n (div n 2)) base)))))

;; memoization of above

(define power-of-literal-in-base #f)

(let ((ans (make-number-list-hash-table 100)))
  (set! power-of-literal-in-base 
	(lambda(literal n base)
	  (let* ((lst (list literal n base))
		 (old (ans 'retrieve lst)))
	    (if (eq? old '*hash-table-search-failed*)
		(let ((new (power-of-literal-in-base-impl literal n base)))
		  (ans 'update-entry! lst new) new)
		old)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-based-bases number from-base to-base)
  (define (null->zero x) (if (null? x) '(0) x))
  (let loop ((r      (reverse number))
	     (result '())
	     (n      0))
    (if (null? r) 
	result
	(let ((current-digit-value 
	       (power-of-literal-in-base from-base n to-base))
	      (current-digit 
	       (null->zero
		(power-of-literal-in-base (car r)   1 to-base))))
	  (loop (cdr r)
		(add-based-digits 
		 to-base
		 result
		 (multiply-based-digits to-base 
					current-digit
					current-digit-value))
		(+ n 1))))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-based-number base number) 

  (define (checker d) 
    (cond 
     ((< d 0) (error (string-append "negative digit " (number->string d))))
     ((>= d base) (error (string-append "digit too large " (number->string d))))))

  (map checker number)
  (cons base (list number)))

(define (based-number-number bn) (cadr bn))

(define (based-number-base bn) (car bn))

(define (coerce-base base a) 
  (let ((b (based-number-base a))
	(n (based-number-number a)))
  (if (= base b) n (convert-based-bases n b base))))

(define (multiply-addresses base a1 a2)

  (define (convert a) (coerce-base base a))
  (multiply-based-digits base (convert a1) (convert a2)))

(define (add-addresses base a1 a2)
  (define (convert a) (coerce-base base a))
  (add-based-digits base (convert a1) (convert a2)))

(define *s #f)
(define *l #f)

(define (mapdigits l)
  (map (lambda(d)
	 (if (and (char>=? d #\0)
		  (char<=? d #\9))
	     (- (char->integer d) (char->integer #\0))
	     (+ 10 (- (char->integer (char-upcase d)) 
		      (char->integer #\A)))))
       l))

(define (parse-address s)
  (define (stringify s)
    (set! *s s)
    (cond ((number? s) (number->string s))
	  ((symbol? s) (symbol->string s))
	  (else s)))

  (let ((lst (string->list (stringify s))))
    (set! *l lst)
    (cond ((not (eq? (car lst) #\0)) 
	   (make-based-number 10 (mapdigits lst)))
	  ((member (cadr lst) '(#\b #\B))
	   (make-based-number 2 (mapdigits (cddr lst))))
	  ((member (cadr lst) '(#\x #\X))
	   (make-based-number 16 (mapdigits (cddr lst))))
	  (else (make-based-number 8 (mapdigits (cdr lst)))))))
		
    
	     

