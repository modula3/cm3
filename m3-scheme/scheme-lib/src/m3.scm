;;
;; $Id$
;;

;; set up Scheme - Modula-3 bindings

(define (install-procedures-without-exception-handlers)
  (map (lambda(pn) 
         (define-global-symbol (string->symbol
                                (string-append (car pn) "." (cdr pn)))
           (lambda x
             
             (scheme-procedure-stubs-call pn x '()))))
       (scheme-procedure-stubs-list)))

(install-procedures-without-exception-handlers)

;; basic support for wrapping objects, only to do methods
(define (obj-method-wrap obj type)
   (lambda args
      (if (eq? (car args) '*get-base*) 
          obj
          (modula-type-op type 'call-method obj (car args) (cdr args)))))

;; does this type have any type ops registered?
(define (have-type-ops? tc) (not (null?  (list-modula-type-ops tc))))

(define (closest-opped-supertype tc)
   (cond ((have-type-ops? tc) tc)
         ((> tc (rttype-maxtypecode)) #f)
         (else (closest-opped-supertype (rttype-supertype
 tc)))))

(define (closest-type-op a . op)
   (let ((ctc (closest-opped-supertype (rttype-typecode a))))
        (apply modula-type-op (append (list ctc (car op) a) (cdr op)))))

;; generic Modula-3 to Scheme object conversion
(define (schemify obj)

  ;; the following helpers are grabbed from sstubgen/program/src/sstubgen.src
  ;; should probably be shared in some nice, generic place

  (define (visible-what what)
    (require-modules "set")
    (let ((res '()))
      (set! res
            (lambda(obj-type)
              ;; a type can only have methods if its opaque or object
              (if (null? obj-type) 
                  (make-symbol-hash-table 100)

                  (case (car obj-type)
                    ((Ref)
                     (let ((target (extract-field 'target obj-type))
                           (res (make-symbol-hash-table 100)))
                       (if (not (eq? (car target) 'Record))
                           (error "Cant get " what " from " obj-type))
                       (map (lambda(m)
                              (res 'update-entry!
                                   (extract-field 'name m)
                                   m))
                            (extract-field what target))
                       res))

                    ((Object)
                     (let ((super-visible 
                            (res (extract-field 'super obj-type))))
                       (map 
                        (lambda(m)
                          (super-visible 'update-entry!
                                         (extract-field 'name m)
                                         m))
                        
                        (extract-field what obj-type))
                       super-visible))

                    ((Opaque)
                     (res (extract-field 'revealedSuperType obj-type)))

                    (else (error "Cant get " what " from " obj-type))))))
      res))

  (define visible-methods (visible-what 'methods))

  (define visible-fields (visible-what 'fields))

  (define (extract-field  field-name type)
    (let ((ass (assoc field-name type)))
      (if (not (pair? ass))
          (error "No assoc for " field-name " in " type)
          (cdr ass))))

  (define (have-field? field-name type)
    (assoc field-name type))

  (define (get-name type) 
    (if (not (have-field? 'name type)) '()
        (let ((qid (extract-field 'name type)))
          (if (null? qid) 
              '()
              (cleanup-qid qid)))))

  (define (cleanup-qid qid)
    (if (= 0 (string-length (symbol->string (extract-field 'intf qid))))
        (cons '() (extract-field 'item qid))
        (cons (extract-field 'intf qid)
              (extract-field 'item qid))))

  (let* ((tc    (closest-opped-supertype (rttype-typecode obj)))
         (class (modula-type-class tc))
         (name (get-name class))
         (fields (map cdadr ((visible-fields class) 'values))))
    (cons name
          (map cons
               fields
               (map (lambda(f)(modula-type-op tc 'get-field obj f))
                    fields)))))



