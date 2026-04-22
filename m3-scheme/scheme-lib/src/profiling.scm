;;
;; profiling.scm -- call graph report for the sampling profiler
;;
;; Usage:
;;   (require-modules "profiling")
;;   (sampling-profiler-call-graph-report)
;;   (sampling-profiler-call-graph-report 10)  ; top 10 only
;;

(require-modules "display" "mergesort")

(define (sampling-profiler-call-graph-report . args)
  (let* ((graph (sampling-profiler-call-graph))
         (flat  (sampling-profiler-results))
         (total (sampling-profiler-total))
         (limit (if (null? args) (length graph) (car args)))
         (count 0))

    (define (pad-left s width)
      (let loop ((s (if (string? s) s (number->string s))))
        (if (>= (string-length s) width) s
            (loop (string-append " " s)))))

    (define (pad-right s width)
      (let loop ((s (if (string? s) s (number->string s))))
        (if (>= (string-length s) width) s
            (loop (string-append s " ")))))

    (define (pct n)
      (if (= total 0) "  0.0"
          (let* ((p (* 100.0 (/ n total)))
                 (whole (inexact->exact (truncate p)))
                 (frac  (inexact->exact (truncate (* 10 (- p whole))))))
            (string-append (pad-left whole 3) "." (number->string frac)))))

    (define (flat-count name)
      (let ((entry (assoc name flat)))
        (if entry (cdr entry) 0)))

    (define (sort-by-count alist)
      (mergesort alist (lambda (a b) (> (cdr a) (cdr b)))))

    (define (show-entry entry)
      (let* ((callee  (car entry))
             (callers (sort-by-count (cdr entry)))
             (self    (flat-count callee)))
        (dis (pad-left self 9) (pct self) "%   " callee dnl)
        (for-each
         (lambda (arc)
           (dis "             " (pad-left (cdr arc) 7) "  <- " (car arc) dnl))
         callers)
        (newline)))

    (if (= total 0)
        (dis "No samples collected." dnl)
        (begin
          (dis "  samples   %     procedure" dnl)
          (dis "  -------  ----   ---------" dnl)
          (for-each
           (lambda (entry)
             (if (< count limit)
                 (begin
                   (show-entry entry)
                   (set! count (+ count 1)))))
           graph)
          (dis "  -------" dnl)
          (dis (pad-left total 9) " total samples" dnl)
          'ok))))
