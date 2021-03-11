;;
;; $Id$
;;

;;
;; history objects
;;
;; Copyright (c) 2009, Generation Capital Ltd.  All rights reserved.
;;
;; Author: Mika Nystrom <mika@alum.mit.edu>
;;
;; remember a fixed number of objects from the past
;;

(define (make-history n)
  (let ((value (list 
                'history          ;; tag
                0                 ;; pointer to next elem
                0                 ;; total # of elems inserted
                (make-vector n)   ;; data
                )))
    (lambda args
      (case (car args)
        ((put!) 
         ;; store value
         (vector-set! (cadddr value) (cadr value) (cadr args))
         (set-car! (cdr value) (modulo (+ 1 (cadr value)) n))
         (set-car! (cddr value) (+ 1 (caddr value)))
         value)
        
        ((display) value)

        ((size) (caddr value))

        ((get)
         (vector-ref (cadddr value)
                     (modulo (- (cadr value) 
                                1 
                                (if (null? (cdr args))
                                    0
                                    (cadr args))) 
                             n)))
        (else (error "Unknown history cmd " (car args)))

        ))))         


