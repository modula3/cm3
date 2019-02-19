;; $Id$ 
;; 
;; Scheme hash tables
;;
;; from John David Stone <stone@math.grin.edu>
;;
;; http://www.cs.grinnell.edu/~stone/events/scheme-workshop/hash-tables.html
;;
;; Changes from Stone's version:
;; * require-modules (used for default string hash function)
;; * recoded for R4RS (no => in cond)
;; * added 'keys message
;; * made a simple default for strings
;;

(require-modules "basic-defs")

(define (make-hash-table size hash-function)

  (define (copy-elements! from to)
    (map (lambda (a) (to 'add-entry! (car a) (cdr a)))
         (map (lambda (k) (cons k (from 'retrieve k))) (from 'keys)) ))

  (define (hash-index-function x) ;; the actual function to use for indexing
    (modulo (hash-function x) size))

  (let ((me '()))
    (let ((table (make-vector size '())))
      (set! 
       me
       (lambda (message . args)
         (case message
           ((size) (length (me 'keys)))

           ((add-entry!)
            (let* ((key (car args))
                   (index (hash-index-function key)))
              (vector-set! table
                           index
                           (cons (cons key (cadr args))
                                 (vector-ref table index)))))

           ((update-entry!)
            (let* ((key (car args)))
              (me 'delete-entry! key)
              (me 'add-entry! key (cadr args))))

           ((rehash!)
            (let ((new-size (car args))
                  (temp (make-hash-table size hash-function)))
              (copy-elements! me temp)
              (set! size new-size)
              (set! table (make-vector size '()))
              (copy-elements! temp me)
              ))

           ((retrieve)
            (let* ((key (car args))
                   (index (hash-index-function key)))
              (cond ( (assoc key (vector-ref table index))  (cdr (assoc key (vector-ref table index))))
                    (else '*hash-table-search-failed*))))

           ((delete-entry!)
            (let* ((key (car args))
                   (index (hash-index-function key)))
              (let loop ((bucket (vector-ref table index))
                         (so-far '()))
                (cond ((null? bucket)
                       (vector-set! table index (reverse so-far)))
                      ((equal? key (caar bucket))
                       (loop (cdr bucket) so-far))
                      (else
                       (loop (cdr bucket)
                             (cons (car bucket) so-far)))))))

           ((keys)
            (let jloop ((index 0)
                        (so-far '()))

              (if (= index size) 
                  so-far
                  (let iloop ((bucket (vector-ref table index))
                              (s2 so-far))
                    (if (null? bucket) 
                        (jloop (+ index 1) s2)
                        (iloop (cdr bucket) (cons (caar bucket) s2)))))))

           ((values)
            (map (lambda(k)(me 'retrieve k)) (me 'keys)))
           
           ((display)
            (do ((index 0 (+ index 1)))
                ((= index size))
              (let ((bucket (vector-ref table index)))
                (if (not (null? bucket))
                    (begin
                      (display "Bucket #")
                      (display index)
                      (display ": ")
                      (display bucket)
                      (newline))))))

           ((clear!)
            (do ((index 0 (+ index 1)))
                ((= index size))
              (vector-set! table index '())))

           (else (error "hashtable : unknown message : " message))
           )
         )
       )
      )
    me))

(define (make-string-hash-table size)
  (define (string-hash s) 
    (accumulate + 0 (map char->integer (string->list s))))

  (make-hash-table size string-hash))

(define (failed-search? x) (= x '*hash-table-search-failed*))
