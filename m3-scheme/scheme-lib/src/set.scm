;;
;; $Id$
;;

(require-modules "hashtable")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-set make-hash-table)
  (let ((hashtable (make-hash-table))
        (res '()))
    (set! res 
          (lambda (message . args)
            (case message
              ((insert!)  (let ((result (res 'member? (car args))))
														(if (not result)
                                (hashtable 'add-entry! (car args)))
                            result))

              ((member?) (not (eq? (hashtable 'retrieve (car args))
                                   '*hash-table-search-failed*)))
              ((delete!)  
               (let ((result (res 'member? (car args))))
                 (hashtable 'delete-entry! (car args))
                 result))

              ((size)     (length (hashtable 'keys)))

              ((rehash! keys clear! display)     (hashtable message))

							;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
							
							((intersection)
							 (let ((new (make-set make-hash-table)))
								 (map (lambda(k)(new 'insert! k))
											(filter (lambda(x)(res 'member? x)) 
															((car args) 'keys)))
								 new))

							((copy)
							 (let ((new (make-set make-hash-table)))
								 (map (lambda(k)(new 'insert! k)) 
											(res 'keys))
								 new))

							((union)
							 (let ((new (make-set make-hash-table)))
								 (map (lambda(k)(new 'insert! k)) 
											(append (res 'keys) ((car args) 'keys)))
								 new))

							((diff)
							 (let ((new (make-set make-hash-table)))
								 (map (lambda(k)(new 'insert! k)) 
											(res 'keys))
								 (map (lambda(k)(new 'delete! k))
											((car args) 'keys))
								 new))

              (else (error "Unknown message " message))
              )))
    res))

(define (make-string-set size)
	(make-set (lambda() (make-string-hash-table size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-symbol-hash-table size)
  (define (symbol-hash s) 
    (accumulate + 
												0 
												(map char->integer 
														 (string->list (symbol->string s)))) )
						

  (make-hash-table size symbol-hash))


(define (make-symbol-set size)
	(make-set (lambda() (make-symbol-hash-table size))))

