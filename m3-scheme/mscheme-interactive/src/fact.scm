;; $Id$
;;
;; Copyright (c) 2008, Generation Capital Ltd.  All rights reserved.
;;
;; Author: Mika Nystrom <mika@alum.mit.edu>
;;

(define (fact x)
  (if (= x 0) 1
      (* x (fact (- x 1)))))

(define (loop n) (if (> n 0) (loop (- n 1))))


