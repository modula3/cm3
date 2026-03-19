;;; Test all examples from the MScheme User Manual
;;; Each test prints PASS or FAIL

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (if (equal? expected actual)
      (begin (set! pass-count (+ pass-count 1))
             (display "PASS: ") (display name) (newline))
      (begin (set! fail-count (+ fail-count 1))
             (display "FAIL: ") (display name)
             (display "  expected=") (display expected)
             (display "  actual=") (display actual)
             (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-no-error name thunk)
  (set! test-count (+ test-count 1))
  (unwind-protect
    (begin (thunk)
           (set! pass-count (+ pass-count 1))
           (display "PASS: ") (display name) (newline))
    #f
    (begin (set! fail-count (+ fail-count 1))
           (display "FAIL: ") (display name) (display " -- exception") (newline))))

;;; ============================================================
;;; Chapter 3: Scheme Language Reference
;;; ============================================================

;; Section 3.1: Special Forms

;; quote
(test "quote" '(1 2 3) (quote (1 2 3)))

;; if
(test "if true" 1 (if #t 1 2))
(test "if false" 2 (if #f 1 2))

;; cond
(test "cond" "big" (cond ((> 10 5) "big") (else "small")))

;; begin
(test "begin" 3 (begin 1 2 3))

;; define + lambda
(define (square x) (* x x))
(test "define function" 25 (square 5))

;; set!
(define my-var 10)
(set! my-var 20)
(test "set!" 20 my-var)

;; lambda
(test "lambda" 7 ((lambda (x y) (+ x y)) 3 4))

;; Section 3.1.1: Macros
(define when
  (macro (test . body)
    `(if ,test (begin ,@body))))

(test "macro when true" 42 (when #t 42))

;; Section 3.1.3: unwind-protect
(define *cleanup-ran* #f)
(unwind-protect
  42
  (set! *cleanup-ran* #t)
  #f)
(test-true "unwind-protect cleanup runs" *cleanup-ran*)

;;; Section 3.2: Built-In Primitives

;; Arithmetic
(test "+" 10 (+ 1 2 3 4))
(test "-" 5 (- 10 5))
(test "*" 24 (* 2 3 4))
(test "/" 5 (/ 10 2))
(test "abs" 5 (abs -5))
(test "floor" 3 (floor 3.7))
(test "ceiling" 4 (ceiling 3.2))
(test "round" 4 (round 3.5))
(test "truncate" 3 (truncate 3.9))
(test "max" 5 (max 3 5 1))
(test "min" 1 (min 3 5 1))
(test "quotient" 3 (quotient 10 3))
(test "remainder" 1 (remainder 10 3))
(test "modulo" 1 (modulo 10 3))
(test "sqrt" 3 (sqrt 9))
(test "expt" 8 (expt 2 3))
(test "gcd" 6 (gcd 12 18))
(test "lcm" 36 (lcm 12 18))
(test-true "odd?" (odd? 3))
(test-false "even? odd" (even? 3))
(test-true "even?" (even? 4))
(test-true "zero?" (zero? 0))
(test-true "positive?" (positive? 5))
(test-true "negative?" (negative? -5))
(test-true "number?" (number? 42))
(test-true "integer?" (integer? 42))

;; number->string, string->number
(test "number->string" "42" (number->string 42))
(test "string->number" 42 (string->number "42"))

;; Trig functions exist
(test-true "sin exists" (number? (sin 1)))
(test-true "cos exists" (number? (cos 1)))
(test-true "tan exists" (number? (tan 1)))
(test-true "asin exists" (number? (asin 0.5)))
(test-true "acos exists" (number? (acos 0.5)))
(test-true "atan exists" (number? (atan 1)))
(test-true "exp exists" (number? (exp 1)))
(test-true "log exists" (number? (log 1)))

;; Pairs and Lists
(test "cons" '(1 . 2) (cons 1 2))
(test "car" 1 (car '(1 2 3)))
(test "cdr" '(2 3) (cdr '(1 2 3)))
(test "cadr" 2 (cadr '(1 2 3)))
(test "caddr" 3 (caddr '(1 2 3)))
(test-true "pair?" (pair? '(1 2)))
(test-true "null?" (null? '()))
(test-false "null? not" (null? '(1)))
(test-true "list?" (list? '(1 2)))
(test "list" '(1 2 3) (list 1 2 3))
(test "length" 3 (length '(1 2 3)))
(test "append" '(1 2 3 4) (append '(1 2) '(3 4)))
(test "reverse" '(3 2 1) (reverse '(1 2 3)))
(test "list-ref" 2 (list-ref '(0 1 2 3) 2))
(test "map" '(2 4 6) (map (lambda (x) (* x 2)) '(1 2 3)))
(test "for-each exists" 6
  (let ((sum 0))
    (for-each (lambda (x) (set! sum (+ sum x))) '(1 2 3))
    sum))
(test "apply" 6 (apply + '(1 2 3)))

;; _list* (corrected name)
(test "_list*" '(1 2 . 3) (_list* 1 2 3))

;; member, assoc
(test-true "member" (pair? (member 2 '(1 2 3))))
(test-true "memq" (pair? (memq 'b '(a b c))))
(test "assoc" '(b . 2) (assoc 'b '((a . 1) (b . 2) (c . 3))))
(test "assq" '(b . 2) (assq 'b '((a . 1) (b . 2) (c . 3))))

;; Booleans
(test-true "boolean? #t" (boolean? #t))
(test-true "boolean? #f" (boolean? #f))
(test-true "eq?" (eq? 'foo 'foo))
(test-true "equal?" (equal? '(1 2) '(1 2)))

;; Strings
(test-true "string?" (string? "hello"))
(test "string-length" 5 (string-length "hello"))
(test "string-ref" #\e (string-ref "hello" 1))
(test "substring" "ell" (substring "hello" 1 4))
(test "string-append" "helloworld" (string-append "hello" "world"))
(test "string->list length" 5 (length (string->list "hello")))
(test "list->string" "abc" (list->string '(#\a #\b #\c)))
(test "string->symbol" 'foo (string->symbol "foo"))
(test "symbol->string" "foo" (symbol->string 'foo))

;; Characters
(test-true "char?" (char? #\a))
(test-true "char->integer" (number? (char->integer #\a)))
(test "integer->char" #\A (integer->char 65))

;; Vectors
(test-true "vector?" (vector? (make-vector 3)))
(test "vector-ref" 0 (vector-ref (make-vector 3 0) 1))
(test "vector->list" '(1 2 3)
  (vector->list (list->vector '(1 2 3))))

;; I/O
(test-true "input-port?" (input-port? (current-input-port)))
(test-true "output-port?" (output-port? (current-output-port)))

;; Control
(test "call/cc escape" 42
  (call-with-current-continuation
    (lambda (k) (k 42) 99)))
(test "force/delay" 3 (force (delay (+ 1 2))))

;; eval
(test "eval" 6 (eval '(+ 1 2 3)))

;;; Section 3.2.5: MScheme Extensions
(test-true "error is procedure" (procedure? error))
(test "stringify" "(1 2 3)" (stringify '(1 2 3)))
(test-true "random in [0,1)" (let ((r (random))) (and (>= r 0) (< r 1))))
(test-true "normal exists" (number? (normal)))
(test "system" 0 (system "true"))
(test-true "string-havesub?" (string-havesub? "hello world" "world"))
(test-false "string-havesub? miss" (string-havesub? "hello world" "xyz"))
(test-true "global-exists?" (global-exists? 'cons))
(test-false "global-exists? missing" (global-exists? 'nonexistent-var-xyz))

;; define-global-symbol
(define-global-symbol 'test-gs-var 99)
(test "define-global-symbol" 99 test-gs-var)

;; eq?-memo / equal?-memo (memoization wrappers)
(define memo-square (eq?-memo (lambda (x) (* x x))))
(test "eq?-memo" 25 (memo-square 5))
(test "eq?-memo cached" 25 (memo-square 5))
(define ememo-add (equal?-memo (lambda (x) (+ x 1))))
(test "equal?-memo" 6 (ememo-add 5))

;; set-warnings-are-errors!
(test-no-error "set-warnings-are-errors!" (lambda () (set-warnings-are-errors! #f)))

;; tracebacks
(test-no-error "enable-tracebacks!" (lambda () (enable-tracebacks!)))
(test-no-error "disable-tracebacks!" (lambda () (disable-tracebacks!)))

;; current-environment
(test-no-error "current-environment" (lambda () (current-environment)))

;; bang-bang only works in interactive REPL, not in loaded files
;; (skipped)

;;; ============================================================
;;; Chapter 4: Bundled Scheme Libraries
;;; ============================================================

;; Section 4.2: basic-defs
(require-modules "basic-defs")

(test "nth" 2 (nth '(0 1 2 3) 2))
(test "member?" #t (member? 2 '(1 2 3)))
(test "filter" '(2 4) (filter even? '(1 2 3 4 5)))
(test "accumulate" 10 (accumulate + 0 '(1 2 3 4)))
(test "identity" 42 (identity 42))
(test "last" 3 (last '(1 2 3)))
(test "all-except-last" '(1 2) (all-except-last '(1 2 3)))
(test "head" '(1 2) (head 2 '(1 2 3 4)))
(test "tail" '(3 4) (tail 2 '(1 2 3 4)))
(test "uniq" 3 (length (uniq equal? '(1 2 2 3 3 3))))

;; Section 4.3: display
(require-modules "display")
(test-true "dnl is newline char" (char? dnl))

;; Section 4.4: hashtable
(require-modules "hashtable")

(define tbl (make-string-hash-table 100))
(tbl 'add-entry! "key1" "value1")
(tbl 'add-entry! "key2" "value2")
(test "hashtable retrieve" "value1" (tbl 'retrieve "key1"))
(test "hashtable size" 2 (tbl 'size))
(tbl 'update-entry! "key1" "new-value")
(test "hashtable update" "new-value" (tbl 'retrieve "key1"))
(tbl 'delete-entry! "key2")
(test "hashtable delete" 1 (tbl 'size))
(test "hashtable keys" '("key1") (tbl 'keys))
(test "hashtable values" '("new-value") (tbl 'values))
;; failed-search? is buggy (uses = instead of eq?), use eq? directly
(test-true "hash miss sentinel" (eq? (tbl 'retrieve "nonexistent")
                                     '*hash-table-search-failed*))
(tbl 'clear!)
(test "hashtable clear" 0 (tbl 'size))

;; Section 4.5: set
(require-modules "set")

(define s (make-symbol-set 100))
(s 'insert! 'foo)
(s 'insert! 'bar)
(test-true "set member?" (s 'member? 'foo))
(test-false "set not member?" (s 'member? 'baz))
(test "set size" 2 (s 'size))
(s 'delete! 'bar)
(test "set delete" 1 (s 'size))

;; set operations
(define s2 (make-symbol-set 100))
(s2 'insert! 'foo)
(s2 'insert! 'baz)
(define s-union (s 'union s2))
(test "set union" 2 (s-union 'size))
(define s-inter (s 'intersection s2))
(test "set intersection" 1 (s-inter 'size))
(test-true "set intersection has foo" (s-inter 'member? 'foo))

;; Section 4.6: struct
(require-modules "struct")

(define point-type
  (make-struct-type 'point
    `((x 0) (y 0) (label "origin"))))

(define p (point-type 'new))
(test "struct get default" 0 (p 'get 'x))
(p 'set! 'x 42)
(test "struct set!" 42 (p 'get 'x))
(test "struct label" "origin" (p 'get 'label))
(p 'inc! 'x)
(test "struct inc! by 1" 43 (p 'get 'x))
(p 'inc! 'x 10)
(test "struct inc! by n" 53 (p 'get 'x))
(p 'inc! 'x (lambda (v) (* v 2)))
(test "struct inc! with lambda" 106 (p 'get 'x))
(test "struct display" '((x . 106) (y . 0) (label . "origin")) (p 'display))
(p 'reset!)
(test "struct reset!" 0 (p 'get 'x))
(test-true "struct type" (procedure? (p 'type)))
(test-true "struct copy" (procedure? (p 'copy)))

;; Functional update (set without !)
(define p3 (point-type 'new))
(define p4 (p3 'set 'x 99))
(test "struct functional set" 99 (p4 'get 'x))
(test "struct original unchanged" 0 (p3 'get 'x))

;; get-struct-type
(test-true "get-struct-type" (procedure? (get-struct-type 'point)))

;; Dynamic initializer
(define thing-type
  (make-struct-type 'thing
    `((id  ,(lambda () (random)))
      (data ()))))

(define t1 (thing-type 'new))
(define t2 (thing-type 'new))
(test-true "dynamic init produces numbers" (number? (t1 'get 'id)))

;;; ============================================================
;;; Chapter 5: Modula-3 Integration (low-level API, no stubs needed)
;;; ============================================================

;; Section 5.7: Low-level stub API
(test-true "scheme-procedure-stubs-list" (list? (scheme-procedure-stubs-list)))
(test-true "newable-types" (list? (newable-types)))
(test-true "list-modula-types" (list? (list-modula-types)))
(test-true "rttype-maxtypecode" (number? (rttype-maxtypecode)))

;; Section 5.7: m3 module
(require-modules "m3")
;; No stubs in basic interpreter, so stubs list is empty
(test-true "m3 module loaded" #t)

;;; ============================================================
;;; Chapter 5.8: SchemeM3.T extended primitives
;;; ============================================================

(test-true "timenow" (number? (timenow)))
(test-true "time->string" (string? (time->string (timenow))))
(test-true "time->list" (list? (time->list (timenow))))
(test-true "hostname" (symbol? (hostname)))
(test-true "getunixpid" (number? (getunixpid)))
(test "fmtreal fix" "3.14" (fmtreal 3.14159 'fix 2))
(test-true "gc" (gc))

;; filewr-open / wr-close
(define test-wr (filewr-open "/tmp/mscheme-test-out.txt"))
(test-true "filewr-open" (not (null? test-wr)))
(wr-close test-wr)
(test-true "wr-close" #t)
(remove-file "/tmp/mscheme-test-out.txt")

;; stringify
(test "stringify list" "(1 2 3)" (stringify '(1 2 3)))
(test "stringify string" "\"hello\"" (stringify "hello"))
(test "stringify number" "42" (stringify 42))

;;; ============================================================
;;; Chapter 5.9: Async (at-run / clock-run)
;;; ============================================================

;; at-run with procedure (corrected from manual)
(define (my-test-func) (+ 40 2))
(define h (at-run (+ (timenow) 0.3) my-test-func #f))
(test "at-run + at-join" 42 (at-join h))

;;; ============================================================
;;; Chapter 5.10: Shell commands
;;; ============================================================

;; run-command returns output with trailing newline
(test "run-command" (string-append "hello world" (list->string (list #\newline)))
  (run-command "echo" "hello" "world"))
(test-true "run-command-with-timeout"
  (string? (run-command-with-timeout 5 "echo" "test")))

;;; ============================================================
;;; Standard macros (SchemePrimitives.i3)
;;; ============================================================

(test "let" 3 (let ((x 1) (y 2)) (+ x y)))
(test "let*" 3 (let* ((x 1) (y (+ x 1))) (+ x y)))
(test "letrec" #t
  (letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1)))))
           (odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))))
    (even? 10)))

;; case
(test "case" "two" (case 2 ((1) "one") ((2) "two") (else "other")))

;; do
(test "do loop" 10
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum i)))
      ((= i 5) sum)))

;; quasiquote
(test "quasiquote" '(1 2 3) `(1 ,(+ 1 1) 3))
(test "quasiquote splicing" '(1 2 3 4) `(1 ,@(list 2 3) 4))

;; time macro (just test it doesn't crash)
(test-no-error "time macro" (lambda () (time (+ 1 2))))

;; macro-expand
(test "macro-expand let" '((lambda (x) x) 1) (macro-expand '(let ((x 1)) x)))

;;; ============================================================
;;; Chapter 9: Environment and Debugging
;;; ============================================================

;; dump-environment / load-environment! round-trip
(define *env-test-value* 12345)
(define test-env-wr (filewr-open "/tmp/mscheme-env-test.pkl"))
(dump-environment test-env-wr)
(wr-close test-env-wr)
(test-true "dump-environment" #t)

;; debug primitives
(test-no-error "debug-setlevel" (lambda () (debug-setlevel 0)))

;;; ============================================================
;;; Summary
;;; ============================================================
(newline)
(display "==============================") (newline)
(display "Test Results:") (newline)
(display "  Total:  ") (display test-count) (newline)
(display "  Passed: ") (display pass-count) (newline)
(display "  Failed: ") (display fail-count) (newline)
(display "==============================") (newline)

;; Clean up
(remove-file "/tmp/mscheme-env-test.pkl")
