;;; Section 3.1.1

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (sequence (set! balance (- balance amount))
                balance)
      "Insufficient funds"))

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (sequence (set! balance (- balance amount))
                    balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (sequence (set! balance (- balance amount))
                  balance)
        "Insufficient funds")))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (sequence (set! balance (- balance amount))
                  balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

;;; Section 3.1.2

(define (make-simplified-withdraw balance)
    (lambda (amount)
      (set! balance (- balance amount))
      balance))

(define (make-decrementer balance)
  (lambda (amount)
    (- balance amount)))

;;; Section 3.1.3

;;; NB. We do not provide a definition of RAND-UPDATE

(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
   (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (-1+ trials-remaining) (1+ trials-passed)))
          (else
           (iter (-1+ trials-remaining) trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)   
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (-1+ trials-remaining)
                     (1+ trials-passed)
                     x2))
              (else
               (iter (-1+ trials-remaining)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))

;;; Exercise 3.7

(define (real-random low high)
  (let ((range (- high low)))
    (+ low
       (/ (random (round (* 10000 range)))
          10000))))


;;; Exercise 3.10 -- a variant of the earlier make-withdraw

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (sequence (set! balance (- balance amount))
                    balance)
          "Insufficient funds"))))

;;; Section 3.3.1

(define (cons x y)
  (let ((new (get-new-pair)))
    (set-car! new x)
    (set-cdr! new y)
    new))

;;; Exercise 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (append! x y)
  (set-cdr! (last x) y)
  x)

(define (last x)
  (if (null? (cdr x))
      x
      (last (cdr x))))

;;; Exercise 3.13

(define (make-cycle x)
  (set-cdr! (last x) x)
  x)

;;; Exercise 3.14

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;;; Exercise 3.16

(define (count-pairs x)
  (if (atom? x)
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;;; Implementation of pairs as procedures

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))

(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)

;;; Section 3.3.2 -- Queues

;;; Representaton of queues

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

;;; Operations on queues

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item nil)))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue)))) 

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "Delete called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue))) 

;;; Section 3.3.3 -- Tables

;;; One-dimensional tables

(define (lookup key table)
  (let ((record (assq key (cdr table))))
    (if (null? record)
        nil
        (cdr record))))

(define (assq key records)
  (cond ((null? records) nil)
        ((eq? key (caar records)) (car records))
        (else (assq key (cdr records)))))

(define (insert! key value table)
  (let ((record (assq key (cdr table))))
    (if (null? record)
        (set-cdr! table
                  (cons (cons key value) (cdr table)))
        (set-cdr! record value)))
  'ok)

(define (make-table)
  (list '*table*))

;;; Two-dimensional tables

(define (lookup key-1 key-2 table)
  (let ((subtable (assq key-1 (cdr table))))
    (if (null? subtable)
        nil
        (let ((record (assq key-2 (cdr subtable))))
          (if (null? record)
              nil
              (cdr record))))))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assq key-1 (cdr table))))
    (if (null? subtable)
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))
        (let ((record (assq key-2 (cdr subtable))))
          (if (null? record)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))
              (set-cdr! record value)))))
  'ok)

;;; Local tables

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assq key-1 (cdr local-table))))
        (if (null? subtable)
            nil
            (let ((record (assq key-2 (cdr subtable))))
              (if (null? record)
                  nil
                  (cdr record))))))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assq key-1 (cdr local-table))))
        (if (null? subtable)
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))
            (let ((record (assq key-2 (cdr subtable))))
              (if (null? record)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))
                  (set-cdr! record value)))))
       `ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))

;;; The PUT and GET operations used in chapter 2

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;; Exercise 3.27

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (if (not (null? previously-computed-result))
            previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

;;; Section 3.3.4 -- Digial-circuit simulator

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)))

;;; Primitive function boxes

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output
                                  new-value)))))
  (add-action! input invert-input))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure))

;;; Wires

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (sequence (set! signal-value new-value)
                    (call-each action-procedures))
          'done))

    (define (accept-action-procedure proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure)
            (else (error "Unknown operation -- WIRE" m))))

    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (sequence
       ((car procedures))
       (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;;; Agenda use

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

;;; Top level of simulation

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;;; Probing a wire

(define (probe name wire)
  (add-action! wire
              (lambda ()        
                (print name)
                (princ (current-time the-agenda))
                (princ "  New-value = ")
                (princ (get-signal wire)))))

;;; Implementation of the agenda

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda)
  (list '*agenda*
        (make-time-segment 0 (make-queue))))

(define (segments agenda) (cdr agenda))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (current-time agenda)
  (segment-time (first-segment agenda)))

(define (empty-agenda? agenda)
  (and (empty-queue? (segment-queue (first-segment agenda)))
       (null? (rest-segments agenda))))

(define (add-to-agenda! time action agenda)
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (cond ((null? rest)
                 (insert-new-time! time action segments))
                ((> (segment-time (car rest)) time)
                 (insert-new-time! time action segments))
                (else (add-to-segments! rest))))))
  (add-to-segments! (segments agenda)))

(define (insert-new-time! time action segments)
  (let ((q (make-queue)))
    (insert-queue! q action)
    (set-cdr! segments
              (cons (make-time-segment time q)
                    (cdr segments)))))

(define (remove-first-agenda-item! agenda)
  (delete-queue! (segment-queue (first-segment agenda))))

(define (first-agenda-item agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (if (empty-queue? q)
        (sequence (set-segments! agenda
                                 (rest-segments agenda))
                  (first-agenda-item agenda))
        (front q))))

;;; Section 3.3.5 -- Propagation of constraints

(define (centigrade-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)))

;;; Primitive constraints

(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))

  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)  
           process-new-value)
          ((eq? request 'I-lost-my-value) 
           process-forget-value)
          (else 
           (error "Unknown request -- ADDER" request))))

  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  ((constraint 'I-have-a-value)))

(define (inform-about-no-value constraint)
  ((constraint 'I-lost-my-value)))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (if (has-value? m1) (= (get-value m1) 0) nil)
               (if (has-value? m2) (= (get-value m2) 0) nil))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))

  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           process-new-value)
          ((eq? request 'I-lost-my-value)
           process-forget-value)
          (else
           (error "Unknown request -- MULTIPLIER" request))))

  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (process-new-value)
    (newline)
    (princ "Probe: ")
    (princ name)
    (princ " = ")
    (princ (get-value connector)))

  (define (process-forget-value)
    (newline)
    (princ "Probe: ")
    (princ name)
    (princ " = ")
    (princ "?"))

  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           process-new-value)
          ((eq? request 'I-lost-my-value)
           process-forget-value)
          (else
           (error "Unknown request -- PROBE" request))))

  (connect connector me)
  me)

;;; Connectors

(define (make-connector)
  (let ((value nil) (informant nil) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))))

    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (sequence (set! informant nil)
                    (for-each-except retractor
                                     inform-about-no-value
                                     constraints))))

    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints 
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint)))

    (define (me request)
      (cond ((eq? request 'has-value?)
             (not (null? informant)))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR"
                         request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

;;; Interface to connectors

(define (has-value? connector)
  (connector 'has-value?))

(define (get-value connector)
  (connector 'value))

(define (forget-value! connector retractor)
  ((connector 'forget) retractor))

(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))

(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))


;;; Exercise 3.37

(define (centigrade-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

;;; Section 3.4.1

(define (sum-odd-squares tree)
  (if (leaf-node? tree)
      (if (odd? tree)
          (square tree)
          0)
      (+ (sum-odd-squares (left-branch tree))
         (sum-odd-squares (right-branch tree)))))

(define (odd-fibs n)
  (define (next k)
    (if (> k n)
        '()
        (let ((f (fib k)))
          (if (odd? f)
              (cons f (next (1+ k)))
              (next (1+ k))))))
  (next 1))

(define (enumerate-tree tree)
  (if (leaf-node? tree)
      (cons-stream tree the-empty-stream)
      (append-streams (enumerate-tree (left-branch tree))
                      (enumerate-tree (right-branch tree)))))

(define (append-streams s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (append-streams (tail s1) s2))))

(define (filter-odd s)
  (cond ((empty-stream? s) the-empty-stream)
        ((odd? (head s))
         (cons-stream (head s) (filter-odd (tail s))))
        (else (filter-odd (tail s)))))

(define (map-square s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (square (head s))
                   (map-square (tail s)))))

(define (accumulate-+ s)
  (if (empty-stream? s)
      0
      (+ (head s) (accumulate-+ (tail s)))))

(define (sum-odd-squares tree)
  (accumulate-+
    (map-square
      (filter-odd
        (enumerate-tree tree)))))

(define (enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (enumerate-interval (1+ low) high))))

(define (map-fib s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (fib (head s))
                   (map-fib (tail s)))))

(define (accumulate-cons s)
  (if (empty-stream? s)
      '()
      (cons (head s) (accumulate-cons (tail s)))))

(define (odd-fibs n)
  (accumulate-cons
    (filter-odd
      (map-fib
        (enumerate-interval 1 n)))))

(define (list-square-fibs n)
  (accumulate-cons
    (map-square
      (map-fib
        (enumerate-interval 1 n)))))

;;; Section 3.4.2

(define (accumulate combiner initial-value stream)
  (if (empty-stream? stream)
      initial-value
      (combiner (head stream)
                (accumulate combiner
                            initial-value
                            (tail stream)))))

(define (sum-stream stream)
  (accumulate + 0 stream))

(define (product-stream stream)
  (accumulate * 1 stream))

(define (accumulate-cons stream)
  (accumulate cons '() stream))

(define (flatten stream)
  (accumulate append-streams the-empty-stream stream))

(define (horner-eval x coefficient-stream)
  (define (add-term coeff higher-terms)
    (+ coeff (* x higher-terms)))
  (accumulate add-term
              0
              coefficient-stream))


(define (map proc stream)
  (if (empty-stream? stream)
      the-empty-stream
      (cons-stream (proc (head stream))
                   (map proc (tail stream)))))

(define (filter pred stream)
  (cond ((empty-stream? stream) the-empty-stream)
        ((pred (head stream))
         (cons-stream (head stream)
                      (filter pred (tail stream))))
        (else (filter pred (tail stream)))))

;;; Examples

(define (product-of-squares-of-odd-elements stream)
  (accumulate *
              1
              (map square
                   (filter odd? stream))))

(define (salary-of-highest-paid-programmer record-stream)
  (accumulate max
              0
              (map salary
                   (filter programmer?
                           record-stream))))

;;; Stream printer

(define (for-each proc stream)
  (if (empty-stream? stream)
      'done
      (sequence (proc (head stream))
                (for-each proc (tail stream)))))

(define (print-stream s)
  (for-each print s))

;;; Exercise 3.38

(define (left-accumulate combiner initial-value stream)
  (if (empty-stream? stream)
      initial-value
      (left-accumulate combiner
                       (combiner initial-value (head stream))
                       (tail stream))))


;;; Nested mappings

(define (flatmap f s) (flatten (map f s)))

(define (prime-sum-pairs n)
  (map (lambda (pair) (list (car pair)
                            (cadr pair)
                            (+ (car pair) (cadr pair))))
       (filter (lambda (pair) (prime? (+ (car pair)
                                         (cadr pair))))
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (-1+ i))))
                (enumerate-interval 1 n)))))

(define (triples n s)
  (filter (lambda (triple) 
            (= (+ (car triple) (cadr triple) (caddr triple))
               s))
          (flatmap 
           (lambda (i)
             (flatmap
              (lambda (j)
                (map (lambda (k) (list i j k))
                     (enumerate-interval 1 (-1+ j))))
              (enumerate-interval 1 (-1+ i))))
           (enumerate-interval 1 n))))

;;; Versions of the above with COLLECT

(define (prime-sum-pairs n)
  (collect (list i j (+ i j))
           ((i (enumerate-interval 1 n))
            (j (enumerate-interval 1 (-1+ i))))
           (prime? (+ i j))))

(define (triples n s)
  (collect (list i j k)
           ((i (enumerate-interval 1 n))
            (j (enumerate-interval 1 (-1+ i)))
            (k (enumerate-interval 1 (-1+ j))))
           (= (+ i j k) s)))

(define (permutations S)
  (if (empty-stream? S)
      (singleton the-empty-stream)
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons-stream x p))
                      (permutations (remove x S))))
               S)))

(define (singleton s)
  (cons-stream s the-empty-stream)) 

;;; Version with COLLECT

(define (permutations S)
  (if (empty-stream? S)
      (singleton the-empty-stream)
      (collect (cons-stream x p)
               ((x S)
                (p (permutations (remove x S)))))))

(define (remove item stream)
  (filter (lambda (x) (not (equal? x item)))
          stream))

;;; Exercise 3.41

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (singleton empty-board)
        (collect (adjoin-position new-row k rest-of-queens)
                 ((rest-of-queens (queen-cols (-1+ k)))
                  (new-row (enumerate-interval 1 board-size)))
                 (safe? new-row k rest-of-queens))))
  (queen-cols board-size))

;;; Section 3.4.3 -- Implementing streams

(define (sum-primes a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (1+ count) (+ count accum)))
          (else (iter (1+ count) accum))))
  (iter a 0))

(define (sum-primes a b)
  (accumulate +
              0
              (filter prime?
                      (enumerate-interval a b))))

;;; Implementation of HEAD and TAIL

(define (head stream) (car stream))

(define (tail stream) (force (cdr stream)))

(define (force delayed-object)
  (delayed-object))

;;; Memoization of streams

(define (memo-proc proc)
  (let ((already-run? nil) (result nil))
    (lambda ()
      (if (not already-run?)
          (sequence (set! result (proc))
                    (set! already-run? (not nil))
                    result)
          result))))

;;; Exercise 3.43

(define (show x)
  (print x)
  x)

(define (nth-stream n s)
  (if (= n 0)
      (head s)
      (nth-stream (-1+ n) (tail s))))

;;; Exercise 3.45

(define (copy-stream s)
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (head s) (copy-stream (tail s)))))

(define (*copy-stream s)
  (accumulate cons the-empty-stream s))

;;; Section 3.4.4 -- Infinite streams

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (1+ n))))

(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))

(define no-sevens
  (filter (lambda (x) (not (divisible? x 7)))
          integers))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

(define fibs (fibgen 0 1))

(define (sieve stream)
  (cons-stream
   (head stream)
   (sieve (filter
           (lambda (x) (not (divisible? x (head stream))))
           (tail stream)))))

(define primes (sieve (integers-starting-from 2)))

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (cons-stream (+ (head s1) (head s2))
                      (add-streams (tail s1) (tail s2))))))

(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (tail fibs) fibs))))

(define (scale-stream c stream)
  (map (lambda (x) (* x c)) stream))

(define double (cons-stream 1 (scale-stream 2 double)))

(define primes
  (cons-stream 2 (filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (head ps)) n) t)
          ((divisible? n (head ps)) nil)
          (else (iter (tail ps)))))
  (iter primes))

;;; Exercise 3.46

(define (merge s1 s2)
  (cond ((empty-stream? s1) s2)
        ((empty-stream? s2) s1)
        (else
         (let ((h1 (head s1))
               (h2 (head s2)))
           (cond ((< h1 h2) (cons-stream h1 (merge (tail s1) s2)))
                 ((> h1 h2) (cons-stream h2 (merge s1 (tail s2))))
                 (else
                  (cons-stream h1
                               (merge (tail s1) (tail s2)))))))))

;;; Exercise 3.48

(define (expand num den radix)
  (cons-stream (quotient (* num radix) den)
               (expand (remainder (* num radix) den) den radix)))

;;; Exercise 3.49

(define (integrate-term t)
  (let ((new-order (1+ (order t))))
    (make-term new-order
               (rat/int (coeff t) new-order))))

(define (integrate-series series)
  (map integrate-term series))

(define (rat/int r i) (/rat r (make-rat i 1)))

(define unit-term (make-term 0 (make-rat 1 1)))

(define exp-series
  (cons-stream unit-term (integrate-series exp-series)))

;;; Streams as signals

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream dt integrand)
                              int)))
  int)

;;; Exrecise 3.51

(define (make-zero-crossings input-stream last-value)
  (cons-stream (sign-change-detector (head input-stream) last-value)
               (make-zero-crossings (tail input-stream)
                                    (head input-stream))))

(define zero-crossings (make-zero-crossings sense-data 0))

(define (map-2 proc s1 s2)
  (cons-stream (proc (head s1) (head s2))
               (map-2 proc (tail s1) (tail s2))))

(define (make-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (head input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-zero-crossings (tail input-stream) avpt))))

;;; Section 3.4.5

(define (solve f y-init dt)
  (define y (integral dy y-init dt))
  (define dy (map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream dt integrand)
                                int))))
  int)

(define (solve f y-init dt)
  (define y (integral (delay dy) y-init dt))
  (define dy (map f y))
  y)

(define (integral integrand initial-value dt)
  (cons-stream initial-value
               (if (empty-stream? integrand)
                   the-empty-stream
                   (integral (tail integrand)
                             (+ (* dt (head integrand))
                                initial-value)
                             dt))))

;;; Nested mappings

(define (pairs S1 S2)
  (collect (list i j)
           ((i S1)
            (j S2))))

(define (pairs S1 S2)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  S2))
           S1))

(define (flatmap f s) (flatten (map f s)))

(define (flatten stream)
  (accumulate append-streams the-empty-stream stream))

(define (interleave s1 s2)
  (if (empty-stream? s1)
      s2
      (cons-stream (head s1)
                   (interleave s2
                               (tail s1)))))

(define (flatten stream)
  (accumulate interleave the-empty-stream stream))

(define (accumulate-delayed combiner initial-value stream)
  (if (empty-stream? stream)
      initial-value
      (combiner (head stream)
                (delay
                 (accumulate-delayed combiner
                                     initial-value
                                     (tail stream))))))

(define (interleave-delayed s1 delayed-s2)
  (if (empty-stream? s1)
      (force delayed-s2)
      (cons-stream (head s1)
                   (interleave-delayed (force delayed-s2)
                                       (delay (tail s1))))))

(define (flatten stream)
  (accumulate-delayed interleave-delayed
                      the-empty-stream
                      stream))

;;; Section 3.4.6

(define (stream-withdraw balance amount-stream)
    (cons-stream balance
                 (stream-withdraw (- balance
                                     (head amount-stream))
                                  (tail amount-stream))))

(define random-numbers
  (cons-stream random-init
               (map rand-update random-numbers)))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (map-successive-pairs f s)
  (cons-stream (f (head s) (head (tail s)))
               (map-successive-pairs f (tail (tail s)))))

(define (monte-carlo experiment-stream nt nf)
  (define (next nt nf)
    (cons-stream (/ nt (+ nt nf))
                 (monte-carlo (tail experiment-stream)
                              nt
                              nf)))
  (if (head experiment-stream)
      (next (+ nt 1) nf)
      (next nt (+ nf 1))))

(define pi
  (map (lambda (p) (sqrt (/ 6 p)))
       (monte-carlo cesaro-stream 0 0)))

