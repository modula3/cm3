;; $Id$
;;
;; downloaded from norvig.com 2008/11/18
;;
;; moved reverse! and append! to basic-defs.scm
;; removed => (not in early JScheme)

;Hygienic R5RS macro-by-example for SILK ;-*-scheme-*-
;Dorai Sitaram ds26@gte.com http://www.cs.rice.edu/~dorai/
;April 17, 1998

;some common Scheme utilities

(require-modules "basic-mbe")

(define *gentemp-counter* -1)

(define gentemp
  (lambda ()
    (set! *gentemp-counter* (+ *gentemp-counter* 1))
    (string->symbol (string-append "GenTemp%" 
				   (number->string *gentemp-counter*)))))

;Hygiene

(define hyg:rassq
  (lambda (k al)
    (let loop ((al al))
      (if (null? al) #f
        (let ((c (car al)))
          (if (eq? (cdr c) k) c
            (loop (cdr al))))))))

(define hyg:tag
  (lambda (e kk al)
    (cond ((pair? e)
            (let* ((a-te-al (hyg:tag (car e) kk al))
                    (d-te-al (hyg:tag (cdr e) kk (cdr a-te-al))))
              (cons (cons (car a-te-al) (car d-te-al))
                (cdr d-te-al))))
      ((vector? e)
        (list->vector
          (hyg:tag (vector->list e) kk al)))
      ((symbol? e)
        (cond ((eq? e '...) (cons '... al))
          ((memq e kk) (cons e al))
          ((hyg:rassq e al) 
            ((lambda (c)
              (cons (car c) al)) (hyg:rassq e al))    )
          (else
            (let ((te (gentemp)))
              (cons te (cons (cons te e) al))))))
      (else (cons e al)))))

;untagging

(define hyg:untag
  (lambda (e al tmps)
    (if (pair? e)
      (let ((a (hyg:untag (car e) al tmps)))
        (if (list? e)
          (case a
            ((quote) (hyg:untag-no-tags e al))
            ((if begin)
              `(,a ,@(map (lambda (e1)
                            (hyg:untag e1 al tmps)) (cdr e))))
            ((set! define)
              `(,a ,(hyg:untag-vanilla (cadr e) al tmps)
                 ,@(map (lambda (e1)
                          (hyg:untag e1 al tmps)) (cddr e))))
            ((lambda) (hyg:untag-lambda (cadr e) (cddr e) al tmps))
            ((letrec) (hyg:untag-letrec (cadr e) (cddr e) al tmps))
            ((let)
              (let ((e2 (cadr e)))
                (if (symbol? e2)
                  (hyg:untag-named-let e2 (caddr e) (cdddr e) al tmps)
                  (hyg:untag-let e2 (cddr e) al tmps))))
            ((let*) (hyg:untag-let* (cadr e) (cddr e) al tmps))
            ((do) (hyg:untag-do (cadr e) (caddr e) (cdddr e) al tmps))
            ((case)
              `(case ,(hyg:untag-vanilla (cadr e) al tmps)
                 ,@(map
                     (lambda (c)
                       `(,(hyg:untag-vanilla (car c) al tmps)
                          ,@(hyg:untag-list (cdr c) al tmps)))
                     (cddr e))))
            ((cond)
              `(cond ,@(map
                         (lambda (c)
                           (hyg:untag-list c al tmps))
                         (cdr e))))
            (else (cons a (hyg:untag-list (cdr e) al tmps))))
          (cons a (hyg:untag-list* (cdr e) al tmps))))
      (hyg:untag-vanilla e al tmps))))

(define hyg:untag-list
  (lambda (ee al tmps)
    (map (lambda (e)
           (hyg:untag e al tmps)) ee)))

(define hyg:untag-list*
  (lambda (ee al tmps)
    (let loop ((ee ee))
      (if (pair? ee)
        (cons (hyg:untag (car ee) al tmps)
          (loop (cdr ee)))
        (hyg:untag ee al tmps)))))

(define hyg:untag-no-tags
  (lambda (e al)
    (cond ((pair? e)
            (cons (hyg:untag-no-tags (car e) al)
              (hyg:untag-no-tags (cdr e) al)))
      ((vector? e)
        (list->vector
          (hyg:untag-no-tags (vector->list e) al)))
      ((not (symbol? e)) e)
      ((assq e al)  (cdr (assq e al)) )
      (else e))))

(define hyg:untag-lambda
  (lambda (bvv body al tmps)
    (let ((tmps2 (append! (hyg:flatten bvv) tmps)))
      `(lambda ,bvv
         ,@(hyg:untag-list body al tmps2)))))

(define hyg:untag-letrec
  (lambda (varvals body al tmps)
    (let ((tmps (append! (map car varvals) tmps)))
      `(letrec
         ,(map
            (lambda (varval)
              `(,(car varval)
                 ,(hyg:untag (cadr varval) al tmps)))
            varvals)
         ,@(hyg:untag-list body al tmps)))))

(define hyg:untag-let
  (lambda (varvals body al tmps)
    (let ((tmps2 (append! (map car varvals) tmps)))
      `(let
         ,(map
             (lambda (varval)
               `(,(car varval)
                  ,(hyg:untag (cadr varval) al tmps)))
             varvals)
         ,@(hyg:untag-list body al tmps2)))))

(define hyg:untag-named-let
  (lambda (lname varvals body al tmps)
    (let ((tmps2 (cons lname (append! (map car varvals) tmps))))
      `(let ,lname
         ,(map
             (lambda (varval)
               `(,(car varval)
                  ,(hyg:untag (cadr varval) al tmps)))
             varvals)
         ,@(hyg:untag-list body al tmps2)))))

(define hyg:untag-let*
  (lambda (varvals body al tmps)
    (let ((tmps2 (append! (reverse! (map car varvals)) tmps)))
      `(let*
         ,(let loop ((varvals varvals)
                      (i (length varvals)))
            (if (null? varvals) '()
              (let ((varval (car varvals)))
                (cons `(,(car varval)
                         ,(hyg:untag (cadr varval)
                            al (list-tail tmps2 i)))
                  (loop (cdr varvals) (- i 1))))))
         ,@(hyg:untag-list body al tmps2)))))

(define hyg:untag-do
  (lambda (varinistps exit-test body al tmps)
    (let ((tmps2 (append! (map car varinistps) tmps)))
      `(do
         ,(map
            (lambda (varinistp)
              (let ((var (car varinistp)))
                `(,var ,@(hyg:untag-list (cdr varinistp) al
                           (cons var tmps)))))
            varinistps)
         ,(hyg:untag-list exit-test al tmps2)
         ,@(hyg:untag-list body al tmps2)))))

(define hyg:untag-vanilla
  (lambda (e al tmps)
    (cond ((pair? e)
            (cons (hyg:untag-vanilla (car e) al tmps)
              (hyg:untag-vanilla (cdr e) al tmps)))
      ((vector? e)
        (list->vector
          (hyg:untag-vanilla (vector->list e) al tmps)))
      ((not (symbol? e)) e)
      ((memq e tmps) e)
      ((assq e al)  (cdr (assq e al)))
      (else e))))

(define hyg:flatten
  (lambda (e)
    (let loop ((e e) (r '()))
      (cond ((pair? e) (loop (car e)
			     (loop (cdr e) r)))
	    ((null? e) r)
	    (else (cons e r))))))

;End of hygiene filter.

;finds the leftmost index of list l where something equal to x
;occurs

(define mbe:position
  (lambda (x l)
    (let loop ((l l) (i 0))
      (cond ((not (pair? l)) #f)
	    ((equal? (car l) x) i)
	    (else (loop (cdr l) (+ i 1)))))))

;tests if expression e matches pattern p where k is the list of
;keywords

(define mbe:matches-pattern?
  (lambda (p e k)
    (cond ((mbe:ellipsis? p)
	   (and (or (null? e) (pair? e))
		(let* ((p-head (car p))
		       (p-tail (cddr p))
		       (e-head=e-tail (mbe:split-at-ellipsis e p-tail)))
		  (and e-head=e-tail
		       (let ((e-head (car e-head=e-tail))
			     (e-tail (cdr e-head=e-tail)))
			 (and (andmap
			       (lambda (x) (mbe:matches-pattern? p-head x k))
			       e-head)
			      (mbe:matches-pattern? p-tail e-tail k)))))))
	  ((pair? p)
	   (and (pair? e)
		(mbe:matches-pattern? (car p) (car e) k)
		(mbe:matches-pattern? (cdr p) (cdr e) k)))
	  ((symbol? p) (if (memq p k) (eq? p e) #t))
	  (else (equal? p e)))))

;gets the bindings of pattern variables of pattern p for
;expression e;
;k is the list of keywords

(define mbe:get-bindings
  (lambda (p e k)
    (cond ((mbe:ellipsis? p)
	   (let* ((p-head (car p))
		  (p-tail (cddr p))
		  (e-head=e-tail (mbe:split-at-ellipsis e p-tail))
		  (e-head (car e-head=e-tail))
		  (e-tail (cdr e-head=e-tail)))
	     (cons (cons (mbe:get-ellipsis-nestings p-head k)
		     (map (lambda (x) (mbe:get-bindings p-head x k))
			  e-head))
	       (mbe:get-bindings p-tail e-tail k))))
	  ((pair? p)
	   (append (mbe:get-bindings (car p) (car e) k)
	     (mbe:get-bindings (cdr p) (cdr e) k)))
	  ((symbol? p)
	   (if (memq p k) '() (list (cons p e))))
	  (else '()))))

;expands pattern p using environment r;
;k is the list of keywords

(define mbe:expand-pattern
  (lambda (p r k)
    (cond ((mbe:ellipsis? p)
	   (append (let* ((p-head (car p))
			  (nestings (mbe:get-ellipsis-nestings p-head k))
			  (rr (mbe:ellipsis-sub-envs nestings r)))
		     (map (lambda (r1)
			    (mbe:expand-pattern p-head (append r1 r) k))
			  rr))
	     (mbe:expand-pattern (cddr p) r k)))
	  ((pair? p)
	   (cons (mbe:expand-pattern (car p) r k)
	     (mbe:expand-pattern (cdr p) r k)))
	  ((symbol? p)
	   (if (memq p k) p
	     (let ((x (assq p r)))
	       (if x (cdr x) p))))
	  (else p))))

;returns a list that nests a pattern variable as deeply as it
;is ellipsed

(define mbe:get-ellipsis-nestings
  (lambda (p k)
    (let sub ((p p))
      (cond ((mbe:ellipsis? p) (cons (sub (car p)) (sub (cddr p))))
	    ((pair? p) (append (sub (car p)) (sub (cdr p))))
	    ((symbol? p) (if (memq p k) '() (list p)))
	    (else '())))))

;finds the subenvironments in r corresponding to the ellipsed
;variables in nestings

(define mbe:ellipsis-sub-envs
  (lambda (nestings r)
    (ormap (lambda (c)
		    (if (mbe:contained-in? nestings (car c)) (cdr c) #f))
		  r)))

;checks if nestings v and y have an intersection

(define mbe:contained-in?
  (lambda (v y)
    (if (or (symbol? v) (symbol? y)) (eq? v y)
	(ormap (lambda (v_i)
			(ormap (lambda (y_j)
					(mbe:contained-in? v_i y_j))
				      y))
		      v))))

;split expression e so that its second half matches with
;pattern p-tail

(define mbe:split-at-ellipsis
  (lambda (e p-tail)
    (if (null? p-tail) (cons e '())
      (let ((i (mbe:position (car p-tail) e)))
	(if i (cons (butlast e (- (length e) i))
		    (list-tail e i))
	    (error 'mbe:split-at-ellipsis 'bad-arg))))))

;tests if x is an ellipsing pattern, i.e., of the form
;(blah ... . blah2)

(define mbe:ellipsis?
  (lambda (x)
    (and (pair? x) (pair? (cdr x)) (eq? (cadr x) '...))))

;syntax-rules

(define syntax-rules
  (macro (keywords . clauses)
    (let ((macro-name (caar clauses)))
      `(macro __syntax-rules-arg__
         ,(mbe:syntax-rules-proc macro-name keywords clauses
                                 '__syntax-rules-arg__
                                 '__syntax-rules-keywords__)))))

(define mbe:syntax-rules-proc
  (lambda (macro-name keywords clauses arg-sym keywords-sym)
    (let ((keywords (cons macro-name keywords)))
      `(let ((,arg-sym (cons ',macro-name ,arg-sym))
             (,keywords-sym ',keywords-sym))
         (cond ,@(map
                  (lambda (clause)
                    (let ((in-pattern (car clause))
                          (out-pattern (cadr clause)))
                      `((mbe:matches-pattern? ',in-pattern ,arg-sym
                                              ,keywords-sym)
                        (let ((tagged-out-pattern+alist
                               (hyg:tag
                                ',out-pattern
                                (append! (hyg:flatten ',in-pattern)
                                         ,keywords-sym) '())))
                          (hyg:untag
                           (mbe:expand-pattern
                            (car tagged-out-pattern+alist)
                            (mbe:get-bindings ',in-pattern ,arg-sym
                                              ,keywords-sym)
                            ,keywords-sym)
                           (cdr tagged-out-pattern+alist)
                           '())))))
                  clauses)
               (else (error ',macro-name 'no-matching-clause
                            ',clauses)))))))

(define define-syntax 
  (macro arg
    (cons 'define arg)))

(define let-syntax 
  (macro arg
    (cons 'let arg)))

(define letrec-syntax
  (macro arg
    (cons 'letrec arg)))


;end of file
