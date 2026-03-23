;;; test-r4rs-gaps.scm --- Tests for R4RS conformance gap fixes
;;;
;;; Tests each fix made to close MScheme's R4RS gaps.
;;; Usage: mscheme test-r4rs-gaps.scm

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

(define (section title)
  (newline)
  (display "--- ") (display title) (display " ---") (newline))

;; MScheme's equal? does identity comparison on vectors,
;; so we need a structural comparison helper.
(define (vector-equal? a b)
  (and (vector? a)
       (vector? b)
       (= (vector-length a) (vector-length b))
       (let loop ((i 0))
         (or (= i (vector-length a))
             (and (equal? (vector-ref a i) (vector-ref b i))
                  (loop (+ i 1)))))))

;;; ============================================================
;;; exact->inexact / inexact->exact
;;; ============================================================

(section "exact->inexact / inexact->exact")

(test "exact->inexact integer" 42 (exact->inexact 42))
(test "exact->inexact float" 3.14 (exact->inexact 3.14))
(test "exact->inexact zero" 0 (exact->inexact 0))
(test "exact->inexact negative" -7 (exact->inexact -7))
(test "inexact->exact integer" 42 (inexact->exact 42))
(test "inexact->exact float" 3.14 (inexact->exact 3.14))
(test "inexact->exact zero" 0 (inexact->exact 0))
(test "inexact->exact negative" -7 (inexact->exact -7))

;;; ============================================================
;;; 2-arg atan (atan2)
;;; ============================================================

(section "2-arg atan (atan2)")

;; 1-arg atan still works
(test "atan 0" 0 (atan 0))
(test "atan 1 ~ pi/4" #t (< (abs (- (atan 1) 0.7853981633974483)) 1e-10))

;; 2-arg atan
(test "atan 1 1 ~ pi/4" #t (< (abs (- (atan 1 1) 0.7853981633974483)) 1e-10))
(test "atan 0 1 = 0" 0.0 (atan 0 1))
(test "atan 1 0 ~ pi/2" #t (< (abs (- (atan 1 0) 1.5707963267948966)) 1e-10))
(test "atan 0 -1 ~ pi" #t (< (abs (- (atan 0 -1) 3.141592653589793)) 1e-10))
(test "atan -1 0 ~ -pi/2" #t (< (abs (- (atan -1 0) -1.5707963267948966)) 1e-10))
(test "atan 1 -1 ~ 3pi/4" #t (< (abs (- (atan 1 -1) 2.356194490192345)) 1e-10))
(test "atan -1 -1 ~ -3pi/4" #t (< (abs (- (atan -1 -1) -2.356194490192345)) 1e-10))

;;; ============================================================
;;; case macro (memv fix)
;;; ============================================================

(section "case macro (memv)")

(test "case basic" "one" (case 1 ((1) "one") ((2) "two") (else "other")))
(test "case second" "two" (case 2 ((1) "one") ((2) "two") (else "other")))
(test "case else" "other" (case 3 ((1) "one") ((2) "two") (else "other")))
(test "case multi-datum" "small"
      (case 2 ((1 2 3) "small") ((4 5 6) "big") (else "other")))
(test "case symbol" "b"
      (case 'b ((a) "a") ((b) "b") (else "other")))

;; The memv fix: case should use eqv?, not equal?
;; With eqv?, numeric comparison works correctly
(test "case number" "found" (case 42 ((42) "found") (else "missing")))
(test "case char" "found" (case #\a ((#\a) "found") (else "missing")))

;;; ============================================================
;;; string-copy
;;; ============================================================

(section "string-copy")

(test "string-copy basic" "hello" (string-copy "hello"))
(test "string-copy empty" "" (string-copy ""))
(test "string-copy independence"
      #f
      (let ((s "hello"))
        (let ((t (string-copy s)))
          (string-set! t 0 #\H)
          (equal? s t))))
(test "string-copy original unchanged"
      "hello"
      (let ((s "hello"))
        (let ((t (string-copy s)))
          (string-set! t 0 #\H)
          s)))
(test "string-copy modified copy"
      "Hello"
      (let ((s "hello"))
        (let ((t (string-copy s)))
          (string-set! t 0 #\H)
          t)))

;;; ============================================================
;;; string-fill!
;;; ============================================================

(section "string-fill!")

(test "string-fill! basic"
      "xxxxx"
      (let ((s (string-copy "hello")))
        (string-fill! s #\x)
        s))
(test "string-fill! single char"
      "Z"
      (let ((s (string-copy "a")))
        (string-fill! s #\Z)
        s))
(test "string-fill! returns string"
      "aaa"
      (string-fill! (make-string 3 #\space) #\a))

;;; ============================================================
;;; vector-fill!
;;; ============================================================

(section "vector-fill!")

(test-true "vector-fill! basic"
      (let ((v (make-vector 5 1)))
        (vector-fill! v 0)
        (vector-equal? v (vector 0 0 0 0 0))))
(test-true "vector-fill! with symbol"
      (let ((v (make-vector 3)))
        (vector-fill! v 'x)
        (vector-equal? v (vector 'x 'x 'x))))
(test-true "vector-fill! returns vector"
      (vector-equal? (vector-fill! (make-vector 2) 7)
                     (vector 7 7)))

;;; ============================================================
;;; with-input-from-file / with-output-to-file
;;; ============================================================

(section "with-input-from-file / with-output-to-file")

;; Write a test file
(with-output-to-file "/tmp/mscheme-r4rs-test.txt"
  (lambda ()
    (display "hello world")
    (newline)
    (display 42)
    (newline)))

;; Read it back
(test "with-input-from-file read"
      "hello world"
      (with-input-from-file "/tmp/mscheme-r4rs-test.txt"
        (lambda ()
          (read-char) ;; h
          (read-char) ;; e
          (read-char) ;; l
          (read-char) ;; l
          (read-char) ;; o
          (read-char) ;; space
          (read-char) ;; w
          (read-char) ;; o
          (read-char) ;; r
          (read-char) ;; l
          (read-char) ;; d
          "hello world")))

;; Read with read procedure
(test "with-input-from-file read symbol"
      'hello
      (with-input-from-file "/tmp/mscheme-r4rs-test.txt"
        (lambda () (read))))

;; Current port is restored after with-input-from-file
(test "with-input-from-file restores port"
      #t
      (let ((before (current-input-port)))
        (with-input-from-file "/tmp/mscheme-r4rs-test.txt"
          (lambda () (read)))
        (eq? before (current-input-port))))

;; Current port is restored after with-output-to-file
(test "with-output-to-file restores port"
      #t
      (let ((before (current-output-port)))
        (with-output-to-file "/tmp/mscheme-r4rs-test2.txt"
          (lambda () (display "test")))
        (eq? before (current-output-port))))

;; Verify written content via call-with-input-file
(test "with-output-to-file content"
      "test"
      (call-with-input-file "/tmp/mscheme-r4rs-test2.txt"
        (lambda (p)
          (list->string (list (read-char p) (read-char p)
                              (read-char p) (read-char p))))))

;; Return value
(test "with-input-from-file return value"
      'hello
      (with-input-from-file "/tmp/mscheme-r4rs-test.txt"
        (lambda () (read))))

(test "with-output-to-file return value"
      42
      (with-output-to-file "/tmp/mscheme-r4rs-test3.txt"
        (lambda () (display "ignored") 42)))

;;; ============================================================
;;; char-ready?
;;; ============================================================

(section "char-ready?")

;; char-ready? on a file port should be true
(test-true "char-ready? on file port"
  (with-input-from-file "/tmp/mscheme-r4rs-test.txt"
    (lambda () (char-ready?))))

;; char-ready? with explicit port argument
(test-true "char-ready? with port arg"
  (call-with-input-file "/tmp/mscheme-r4rs-test.txt"
    (lambda (p) (char-ready? p))))

;; char-ready? on default port (stdin, piped)
(test-true "char-ready? on stdin (piped)" (char-ready?))

;;; ============================================================
;;; #b binary literal prefix
;;; ============================================================

(section "#b binary literals")

(test "#b0" 0 #b0)
(test "#b1" 1 #b1)
(test "#b10" 2 #b10)
(test "#b1010" 10 #b1010)
(test "#b11111111" 255 #b11111111)
(test "#b-101" -5 #b-101)
(test "#b arithmetic" 15 (+ #b1010 #b101))

;;; ============================================================
;;; #o octal literal prefix
;;; ============================================================

(section "#o octal literals")

(test "#o0" 0 #o0)
(test "#o7" 7 #o7)
(test "#o10" 8 #o10)
(test "#o77" 63 #o77)
(test "#o377" 255 #o377)
(test "#o-17" -15 #o-17)
(test "#o arithmetic" 73 (+ #o100 #o11))

;;; ============================================================
;;; #x hexadecimal literal prefix
;;; ============================================================

(section "#x hexadecimal literals")

(test "#x0" 0 #x0)
(test "#x9" 9 #x9)
(test "#xA" 10 #xA)
(test "#xa" 10 #xa)
(test "#xFF" 255 #xFF)
(test "#xff" 255 #xff)
(test "#x1F" 31 #x1F)
(test "#x-1F" -31 #x-1F)
(test "#x100" 256 #x100)
(test "#x arithmetic" 256 (+ #x80 #x80))

;;; ============================================================
;;; #d decimal literal prefix (explicit)
;;; ============================================================

(section "#d decimal literals")

(test "#d0" 0 #d0)
(test "#d42" 42 #d42)
(test "#d-7" -7 #d-7)
(test "#d3.14" 3.14 #d3.14)

;;; ============================================================
;;; #e and #i exactness prefixes
;;; ============================================================

(section "#e and #i exactness prefixes")

(test "#e42" 42 #e42)
(test "#e-7" -7 #e-7)
(test "#i42" 42 #i42)
(test "#i3.14" 3.14 #i3.14)
(test "#i-7" -7 #i-7)

;;; ============================================================
;;; Combined prefixes (R4RS allows any order)
;;; ============================================================

(section "combined prefixes")

(test "#e#x1F" 31 #e#x1F)
(test "#x#e1F" 31 #x#e1F)
(test "#i#xFF" 255 #i#xFF)
(test "#x#iFF" 255 #x#iFF)
(test "#e#b1010" 10 #e#b1010)
(test "#b#e1010" 10 #b#e1010)
(test "#i#o77" 63 #i#o77)
(test "#o#i77" 63 #o#i77)
(test "#e#d42" 42 #e#d42)
(test "#d#e42" 42 #d#e42)

;;; ============================================================
;;; Cross-base consistency
;;; ============================================================

(section "cross-base consistency")

(test-true "255 = #xFF" (= 255 #xFF))
(test-true "255 = #o377" (= 255 #o377))
(test-true "255 = #b11111111" (= 255 #b11111111))
(test-true "#xFF = #o377" (= #xFF #o377))
(test-true "#xFF = #b11111111" (= #xFF #b11111111))
(test-true "#o377 = #b11111111" (= #o377 #b11111111))
(test-true "10 = #xA = #o12 = #b1010" (= 10 #xA #o12 #b1010))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "======================================") (newline)
(display "R4RS gap tests: ")
(display pass-count) (display "/") (display test-count) (display " passed")
(if (> fail-count 0)
    (begin (display ", ") (display fail-count) (display " FAILED"))
    (display ", all passed"))
(newline)
(display "======================================") (newline)

(exit (if (= fail-count 0) 0 1))
