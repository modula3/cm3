;;; test-cn-exceptions.scm --- Compiler acceptance: unwind-protect behavior
;;;
;;; The compiler may punt unwind-protect to the interpreter, but the
;;; behavior MUST be identical to interpreted code. These tests verify
;;; that exceptions are caught, side effects happen correctly, and
;;; execution continues normally after exception handling.
;;;
;;; unwind-protect semantics:
;;;   (unwind-protect body no-exception-value exception-handler)
;;;   - body is evaluated for side effects
;;;   - if no exception: return no-exception-value
;;;   - if exception: evaluate exception-handler, return its value

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (test name expected actual)
  (set! test-count (+ test-count 1))
  (if (if (and (number? expected) (number? actual))
          (= expected actual)
          (equal? expected actual))
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

(define (test-summary label)
  (newline)
  (display "=== ") (display label) (display ": ")
  (display pass-count) (display "/") (display test-count) (display " passed")
  (if (> fail-count 0)
      (begin (display ", ") (display fail-count) (display " FAILED")))
  (display " ===") (newline))

;;; ============================================================
;;; 1. Basic unwind-protect: no exception
;;; ============================================================

(section "CN-EXCEPTIONS: unwind-protect no exception")

;; Normal path: body succeeds, returns no-exception-value
(test "uwp normal return"
  "no-err"
  (unwind-protect 42 "no-err" "should not reach"))

;; Body side effects execute
(define uwp-side-effect 0)
(unwind-protect
  (set! uwp-side-effect 1)
  "ok"
  "error")
(test "uwp body side effect" 1 uwp-side-effect)

;;; ============================================================
;;; 2. Basic unwind-protect: exception caught
;;; ============================================================

(section "CN-EXCEPTIONS: unwind-protect catches exception")

;; Exception path: error is caught, handler value returned
(test "uwp catches error"
  "caught"
  (unwind-protect
    (error "boom")
    "no-err"
    "caught"))

;; Exception from division by zero
(test "uwp catches div/0"
  "caught"
  (unwind-protect
    (/ 1 0)
    "no-err"
    "caught"))

;; Exception from type error
(test "uwp catches type error"
  "caught"
  (unwind-protect
    (+ 1 "not a number")
    "no-err"
    "caught"))

;;; ============================================================
;;; 3. Execution continues after unwind-protect
;;; ============================================================

(section "CN-EXCEPTIONS: execution continues after exception")

;; This is the critical test: after catching an exception,
;; subsequent code must execute normally.

(define post-uwp-val
  (begin
    (unwind-protect (error "boom") #f "caught")
    (+ 1 2)))

(test "code after uwp exception" 3 post-uwp-val)

;; Multiple sequential unwind-protects
(define seq-result
  (list (unwind-protect (error "a") "no" "a")
        (unwind-protect (error "b") "no" "b")
        (unwind-protect (+ 1 2) "ok" "err")))
(test "sequential uwp" '("a" "b" "ok") seq-result)

;;; ============================================================
;;; 4. Side effects in exception handler
;;; ============================================================

(section "CN-EXCEPTIONS: side effects in handler")

(define handler-ran #f)
(unwind-protect
  (error "boom")
  #f
  (set! handler-ran #t))
(test-true "handler side effect" handler-ran)

;; Counter incremented in handler
(define error-count 0)
(define (count-error thunk)
  (unwind-protect
    (thunk)
    "ok"
    (begin (set! error-count (+ error-count 1)) "error")))

(count-error (lambda () (error "one")))
(count-error (lambda () (error "two")))
(count-error (lambda () "no error"))
(test "error counter" 2 error-count)

;;; ============================================================
;;; 5. unwind-protect in loops
;;; ============================================================

(section "CN-EXCEPTIONS: unwind-protect in loops")

;; Count errors in a loop
(define loop-errors 0)
(define loop-successes 0)
(let loop ((i 0))
  (if (= i 10) 'done
      (begin
        (unwind-protect
          (if (= (remainder i 3) 0)
              (error "divisible by 3")
              (set! loop-successes (+ loop-successes 1)))
          #f
          (set! loop-errors (+ loop-errors 1)))
        (loop (+ i 1)))))

;; i=0,3,6,9 are divisible by 3 -> 4 errors, 6 successes
(test "loop errors" 4 loop-errors)
(test "loop successes" 6 loop-successes)

;;; ============================================================
;;; 6. Nested unwind-protect
;;; ============================================================

(section "CN-EXCEPTIONS: nested unwind-protect")

;; Inner handler re-raises, outer catches
(test "nested uwp outer catches"
  "outer"
  (unwind-protect
    (unwind-protect
      (error "deep")
      "no"
      (error "rethrow"))
    "no"
    "outer"))

;; Inner handler catches, outer sees no exception
(test "nested uwp inner catches"
  "no-outer"
  (unwind-protect
    (unwind-protect
      (error "deep")
      "no-inner"
      "inner-caught")
    "no-outer"
    "should not reach"))

;;; ============================================================
;;; 7. unwind-protect with closures
;;; ============================================================

(section "CN-EXCEPTIONS: closures + exception handling")

;; Closure that catches errors and returns a default
(define (make-safe-caller default)
  (lambda (thunk)
    (unwind-protect
      (thunk)
      #f       ;; body ran OK, but we want the thunk's side effects
      default)))

;; For safe-caller, we need to capture the body result differently.
;; unwind-protect returns no-exception-value on success, not the body result.
;; So test the error path and that execution continues.
(define safe (make-safe-caller -1))
(test "safe caller error"  -1 (safe (lambda () (error "boom"))))

;; Verify continued execution after safe call
(define safe-after (+ 1 (safe (lambda () (error "x")))))
(test "safe caller arithmetic after" 0 safe-after)

;; Closure state survives exception handling
(define call-count 0)
(define (counting-safe thunk)
  (set! call-count (+ call-count 1))
  (unwind-protect (thunk) "ok" "error"))

(counting-safe (lambda () 1))
(counting-safe (lambda () (error "x")))
(counting-safe (lambda () 2))
(test "closure state after exceptions" 3 call-count)

;;; ============================================================
;;; 8. Arithmetic after exception handling
;;; ============================================================

(section "CN-EXCEPTIONS: arithmetic after exceptions")

;; These verify that the numeric runtime is not corrupted
;; after exception handling.

(unwind-protect (error "boom") #f "caught")
(test "add after exception" 5 (+ 2 3))
(test "mul after exception" 6 (* 2 3))
(test "sub after exception" -1 (- 2 3))

;; Bignum arithmetic after exception
(unwind-protect (error "boom") #f "caught")
(test "bignum after exception"
  (expt 2 100)
  (let loop ((i 0) (acc 1))
    (if (= i 100) acc
        (loop (+ i 1) (* acc 2)))))

;; Loop after exception
(unwind-protect (/ 1 0) #f "caught")
(test "loop after div/0"
  55
  (let loop ((i 1) (sum 0))
    (if (> i 10) sum
        (loop (+ i 1) (+ sum i)))))

;;; ============================================================
;;; 9. test-error / test-no-error pattern
;;; ============================================================

(section "CN-EXCEPTIONS: test-error/test-no-error pattern")

;; This is the pattern used in all our test files.
;; It must work correctly through the compiler.

(define (test-no-error name thunk)
  (set! test-count (+ test-count 1))
  (unwind-protect
    (begin (thunk)
           (set! pass-count (+ pass-count 1))
           (display "PASS: ") (display name) (newline))
    #f
    (begin (set! fail-count (+ fail-count 1))
           (display "FAIL: ") (display name)
           (display " -- unexpected exception") (newline))))

(define (test-error name thunk)
  (set! test-count (+ test-count 1))
  (unwind-protect
    (begin (thunk)
           (set! fail-count (+ fail-count 1))
           (display "FAIL: ") (display name)
           (display " -- no exception raised") (newline))
    #f
    (begin (set! pass-count (+ pass-count 1))
           (display "PASS: ") (display name) (newline))))

;; test-no-error: body succeeds
(test-no-error "tne succeeds" (lambda () (+ 1 2)))

;; test-no-error: body fails (should report FAIL)
(test-no-error "tne fails [EXPECTED FAIL]" (lambda () (error "boom")))

;; test-error: body raises (should report PASS)
(test-error "te catches error" (lambda () (error "boom")))

;; test-error: body succeeds (should report FAIL)
(test-error "te no error [EXPECTED FAIL]" (lambda () (+ 1 2)))

;; Arithmetic still works after all that
(test "arithmetic after test-error" 42 (* 6 7))

;; A loop still works after all that
(test "loop after test-error"
  10
  (let loop ((i 0) (sum 0))
    (if (= i 4) sum
        (loop (+ i 1) (+ sum (+ i 1))))))

(test-summary "CN-EXCEPTIONS")
