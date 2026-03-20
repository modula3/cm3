;;; Section 2.1.1

;;; Rational-number operations

(define (+rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (denom x) (numer y)))
            (* (denom x) (denom y))))

(define (*rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (/rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (=rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;; Rational-number representation -- unreduced

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))


;;; Printing rational numbers

(define (print-rat x)
  (newline)
  (princ (numer x))
  (princ "/")
  (princ (denom x)))

;;; Rational-number representation -- reduced at construction time

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))


;;; Section 2.1.2

;;; Rational-number representation -- reduced at selection time

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))

;;; Section 2.1.3

;;; Procedural representation of pairs

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))


;;; Exercise 2.3 -- part of a procedural representation of pairs

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))


;;; Exercise 2.5 -- Church numerals

(define zero (lambda (f) (lambda (x) x)))

(define (1+ n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;;; Section 2.1.4 -- Interval Arithmetic

(define (intadd x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (intmul x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (intdiv x y)
  (intmul x 
          (make-interval (/ 1 (upper-bound y))
                         (/ 1 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (par1 r1 r2)
  (intdiv (intmul r1 r2)
          (intadd r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
       (intdiv one
               (intadd (intdiv one r1)
                       (intdiv one r2)))))

;;; Section 2.2.1 -- List operations

(define (nth n x)
  (if (= n 0)
      (car x)
      (nth (- n 1) (cdr x))))

(define (length x)
  (if (null? x)
      0
      (+ 1 (length (cdr x)))))

(define (length x)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter x 0))

(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

;;; Exercise 2.19

(define (square-list x)
  (define (iter list answer)
    (if (null? list)
        answer
        (iter (cdr list) 
              (cons (square (car list))
                    answer))))
  (iter x nil))

(define (square-list x)
  (define (iter list answer)
    (if (null? list)
        answer
        (iter (cdr list)
              (cons answer
                    (square (car list))))))
  (iter x nil))


;;; Exercise 2.21 -- counting change

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 .5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? kinds-of-coins)) 0)
        (else (+ (cc (- amount (first-denomination kinds-of-coins))
                     kinds-of-coins)
                 (cc amount
                     (except-first-denomination kinds-of-coins))))))

;;; Section 2.2.2 -- Trees

(define (countatoms x)
  (cond ((null? x) 0)  
        ((atom? x) 1)  
        (else (+ (countatoms (car x))
                 (countatoms (cdr x))))))

;;; Exercise 2.27 -- Mobiles

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;;; Section 2.2.3 -- Symbols

(define (memq item x)
  (cond ((null? x) '())
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;; Section 2.2.4 -- Symbolic differentiation

(define (deriv exp var)
  (cond ((constant? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))))

(define (constant? x) (number? x))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (if (not (atom? x)) (eq? (car x) '+) nil))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (if (not (atom? x)) (eq? (car x) '*) nil))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

;;; Better versions of make-sum and make-product

(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((number? a1) (if (= a1 0) a2 (list '+ a1 a2)))
        ((number? a2) (if (= a2 0) a1 (list '+ a1 a2)))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((and (number? m1) (number? m2)) (* m1 m2))
        ((number? m1)
         (cond ((= m1 0) 0)
               ((= m1 1) m2)
               (else (list '* m1 m2))))
        ((number? m2)
         (cond ((= m2 0) 0)
               ((= m2 1) m1)
               (else (list '* m1 m2))))
        (else (list '* m1 m2))))

;;; Section 2.2.5 -- Sets

;;; Sets as unordered lists

(define (element-of-set? x set)
  (cond ((null? set) nil)
        ((equal? x (car set)) t)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))


;;; Sets as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) nil)
        ((= x (car set)) t)
        ((< x (car set)) nil)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1
                                 (cdr set2)))))))


;;; Sets as binary trees

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) nil)
        ((= x (entry set)) t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set x
                                (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x
                                (right-branch set))))))


;;; Sets and information retrieval

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) nil)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))

;;; Section 2.2.6 -- Huffman encoding

;;; representation for tree leaves

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

;;; representation for trees

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;;; decoding messages

(define (decode bits tree)
  (decode-1 bits tree tree))

(define (decode-1 bits tree current-branch)
  (if (null? bits)
      '()
      (let ((next-branch
             (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
            (cons (symbol-leaf next-branch)
                  (decode-1 (cdr bits) tree tree))
            (decode-1 (cdr bits) tree next-branch)))))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;;; procedure for handling ordered sets

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;;;transform a list of symbol-frequency pairs into an ordered set of
;;;leaves 

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ;symbol
                               (cadr pair))  ;frequency
                    (make-leaf-set (cdr pairs))))))

;;; Exercise 2.39

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree
                                   (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;;; Exercise 2.40
;;; top level of encoding procedure

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;;; Exercise 2.41
;;; top level of tree-generating procedure

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;;; Section 2.3.1

;;; Complex-number arithmetic

(define (+c z1 z2)
  (make-rectangular (+ (real-part z1) (real-part z2))
                    (+ (imag-part z1) (imag-part z2))))

(define (-c z1 z2)
  (make-rectangular (- (real-part z1) (real-part z2))
                    (- (imag-part z1) (imag-part z2))))

(define (*c z1 z2)
  (make-polar (* (magnitude z1) (magnitude z2))
              (+ (angle z1) (angle z2))))

(define (/c z1 z2)
  (make-polar (/ (magnitude z1) (magnitude z2))
              (- (angle z1) (angle z2))))

;;; Rectangular representation of complex numbers

(define (make-rectangular x y) (cons x y))

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (make-polar r a) 
  (cons (* r (cos a)) (* r (sin a))))

(define (magnitude z)
  (sqrt (+ (square (car z)) (square (cdr z)))))

(define (angle z)
  (atan (cdr z) (car z)))

;;; Polar representation of complex numbers

(define (make-rectangular x y) 
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (real-part z)
  (* (car z) (cos (cdr z))))

(define (imag-part z)
  (* (car z) (sin (cdr z))))

(define (make-polar r a) (cons r a))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

;;; Section 2.3.2

;;; Manifest types

(define (attach-type type contents)
  (cons type contents))

(define (type datum)
  (if (not (atom? datum))
      (car datum)
      (error "Bad typed datum -- TYPE" datum)))

(define (contents datum)
  (if (not (atom? datum)) 
      (cdr datum)
      (error "Bad typed datum -- CONTENTS" datum)))

;;; Complex numbers represented with manifest type

(define (rectangular? z)
  (eq? (type z) 'rectangular))

(define (polar? z)
  (eq? (type z) 'polar))

(define (make-rectangular x y)
  (attach-type 'rectangular (cons x y)))

(define (make-polar r a)
  (attach-type 'polar (cons r a)))

(define (real-part z)
  (cond ((rectangular? z) 
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))))

(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (car z))
           (square (cdr z)))))

(define (angle-rectangular z)
  (atan (cdr z) (car z)))

(define (real-part-polar z)
  (* (car z) (cos (cdr z))))

(define (imag-part-polar z)
  (* (car z) (sin (cdr z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))


;;; Section 2.3.3

;;; Note that in order to run this you need definitions of put and get
;;; (see section 3.3.3)

(put 'rectangular 'real-part real-part-rectangular)
(put 'rectangular 'imag-part imag-part-rectangular)
(put 'rectangular 'magnitude magnitude-rectangular)
(put 'rectangular 'angle angle-rectangular)

(put 'polar 'real-part real-part-polar)
(put 'polar 'imag-part imag-part-polar)
(put 'polar 'magnitude magnitude-polar)
(put 'polar 'angle angle-polar)

(define (operate op obj)
  (let ((proc (get (type obj) op)))
    (if (not (null? proc))
        (proc (contents obj))
        (error "Operator undefined for this type -- OPERATE"
               (list op obj)))))

(define (real-part obj) (operate 'real-part obj))
(define (imag-part obj) (operate 'imag-part obj))
(define (magnitude obj) (operate 'magnitude obj))
(define (angle obj) (operate 'angle obj))

;;; Exercise 2.45 -- data-directed symbolic differentiation

(define (deriv exp var)
   (cond ((constant? exp) 0)
         ((variable? exp)
          (if (same-variable? exp var) 1 0))
         (else ((get (operator exp) 'deriv) (operands exp) var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

((get 'deriv (operator exp)) (operands exp) var)

;;; Message-passing

(define (make-rectangular x y)
  (define (dispatch m)
    (cond ((eq? m 'real-part) x)
          ((eq? m 'imag-part) y)
          ((eq? m 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? m 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-RECTANGULAR" m))))
  dispatch)

(define (operate op obj) (obj op))


;;; Section 2.4.1

(define (+number x y)
   (make-number (+ x y)))

(define (-number x y)
   (make-number (- x y)))

(define (*number x y)
   (make-number (* x y)))

(define (/number x y)
   (make-number (/ x y)))

(define (make-number n)
   (attach-type 'number n))

(put 'number 'add +number)
(put 'number 'sub -number)
(put 'number 'mul *number)
(put 'number 'div /number)

(define (add x y) (operate-2 'add x y))
(define (sub x y) (operate-2 'sub x y))
(define (mul x y) (operate-2 'mul x y))
(define (div x y) (operate-2 'div x y))

(define (operate-2 op arg1 arg2)
  (let ((t1 (type arg1)))
    (if (eq? t1 (type arg2))
        (let ((proc (get t1 op)))
          (if (not (null? proc)) 
              (proc (contents arg1) (contents arg2))
              (error
               "Operator undefined on this type -- OPERATE-2"
               (list op arg1 arg2))))
        (error "Operands not of same type -- OPERATE-2"
               (list op arg1 arg2)))))

(define (make-complex z)
  (attach-type 'complex z))

(define (+complex z1 z2) (make-complex (+c z1 z2)))
(define (-complex z1 z2) (make-complex (-c z1 z2)))
(define (*complex z1 z2) (make-complex (*c z1 z2)))
(define (/complex z1 z2) (make-complex (/c z1 z2)))

(put 'complex 'add +complex)
(put 'complex 'sub -complex)
(put 'complex 'mul *complex)
(put 'complex 'div /complex)

(put 'complex 'real-part real-part)
(put 'complex 'imag-part imag-part)
(put 'complex 'magnitude magnitude)
(put 'complex 'angle angle)

;;; Section 2.4.2 -- coercion

;;;We don't use the following coercion procedures anywhere.
;;;Note that put-coercion and get-coercion, used here, are not defined.

(define (number->complex n)
  (make-complex (make-rectangular (contents n) 0)))

(put-coercion 'number 'complex number->complex)

(define (operate-2 op obj1 obj2)
  (let ((t1 (type obj1)) (t2 (type obj2)))
    (if (eq? t1 t2)
        (let ((proc (get t1 op)))
          (if (not (null? proc))
              (proc (contents obj1) (contents obj2))
              (error
               "Operator undefined on this type -- OPERATE-2"
               (list op obj1 obj2))))
        (let ((t1->t2 (get-coercion t1 t2))
              (t2->t1 (get-coercion t2 t1)))
          (cond ((not (null? t1->t2))
                 (operate-2 op (t1->t2 obj1) obj2))
                ((not (null? t2->t1))
                 (operate-2 op obj1 (t2->t1 obj2)))
                (else
                 (error
                  "Operands not of same type -- OPERATE-2"
                  (list op obj1 obj2))))))))

;;; Section 2.4.3

;;; Polynomial arithmetic

(define (+poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-polynomial (variable p1)
                       (+terms (term-list p1)
                               (term-list p2)))
      (error "Polys not in same var -- +POLY" (list p1 p2))))

(define (*poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-polynomial (variable p1)
                       (*terms (term-list p1)
                               (term-list p2)))
      (error "Polys not in same var -- *POLY" (list p1 p2))))

;;; Install polynomial arithmetic in the generic-arithmetic system

(put 'polynomial 'add +poly)
(put 'polynomial 'mul *poly)

;;; operations on term lists, for polynomial arithmetic

(define (+terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term t1
                               (+terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term t2
                               (+terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term (make-term (order t1)
                                          (add (coeff t1)
                                               (coeff t2)))
                               (+terms (rest-terms L1)
                                       (rest-terms L2)))))))))

(define (*terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (+terms (*-term-by-all-terms (first-term L1) L2)
              (*terms (rest-terms L1) L2))))

(define (*-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term (make-term (+ (order t1) (order t2))
                                (mul (coeff t1) (coeff t2)))
                     (*-term-by-all-terms t1 
                                          (rest-terms L))))))

;;; Representation of term lists

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

;;; Representation of terms

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

;;; Representation of polynomials

(define (make-polynomial variable term-list)
  (attach-type 'polynomial (cons variable term-list)))

(define (variable p) (car p))

(define (term-list p) (cdr p))

;;; (note that same-variable? is from section 2.2.4)

;;; Exercise 2.63
;;; Note that the procedure /terms is incomplete

(define (/terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     ;;compute rest of result recursively
                     ))
                ;;form complete result
                ))))))

;;; Exercise 2.65

(define (make-rational n d)
  (attach-type 'rational (make-rat n d)))

(define p1 (make-polynomial 'x '((2 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 1))))
(define rf (make-rational p2 p1))

;;; gcd repeated from chapter 1

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;;; gcd for term lists

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))

;;; Exercise 2.66

(define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p2 (make-polynomial 'x '((3 1) (1 -1))))
(greatest-common-divisor p1 p2)

;;; Exercise 2.69

(define p1 (make-polynomial 'x '((1 1)(0 1))))
(define p2 (make-polynomial 'x '((3 1)(0 -1))))
(define p3 (make-polynomial 'x '((1 1))))
(define p4 (make-polynomial 'x '((2 1)(0 -1))))

(define rf1 (make-rational p1 p2))
(define rf2 (make-rational p3 p4))

