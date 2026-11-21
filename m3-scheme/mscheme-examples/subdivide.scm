;;
;; $Id: subdivide.scm,v 1.2 2010/04/06 03:10:35 mika Exp $
;;

;; example Scheme code to recursively subdivide rectangle in Euclidean plane
;;
;; Run "mscheme subdivide.scm"
;; then run, e.g., 
;; (subdivide (pt 0 0) (pt 1 1) 4)

(require-modules "display")

;; make a point
(define (pt x y) (list x y))

;; accessors for x and y of point
(define (x pt) (car pt))
(define (y pt) (cadr pt))

(define (subdivide ll ur n)
  (let ((lx (x ll))                    ;; 4 coordinates of input rectangle
        (ly (y ll))
        (ux (x ur))
        (uy (y ur))
        (mx (/ (+ (x ll) (x ur)) 2))   ;; mid x
        (my (/ (+ (y ll) (y ur)) 2))   ;; mid y
        )
    (if (= n 0)
        ;; IF clause just prints the current rectangle
        (begin 
          (dis "(" (x ll) " " (y ll) "),(" (x ur) " " (y ur) ")" dnl) 
         #t)

        ;; else clause calls routine recursively on 4 subrects
        (begin
          (subdivide        ll  (pt mx my) (- n 1)) ;; LL subrect
          (subdivide (pt mx ly) (pt ux my) (- n 1)) ;; LR subrect
          (subdivide (pt lx my) (pt mx uy) (- n 1)) ;; UL subrect
          (subdivide (pt mx my)        ur  (- n 1)) ;; UR subrect
        )
    )
)) 
          