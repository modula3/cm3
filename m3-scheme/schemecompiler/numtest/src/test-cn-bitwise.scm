;;; test-cn-bitwise.scm --- SRFI-151 bitwise operations

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

(define (test-true name actual) (test name #t actual))
(define (test-false name actual) (test name #f actual))

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


(section "basic bitwise (pre-existing)")

(test "and 12 10"       8    (bitwise-and 12 10))
(test "ior 12 10"       14   (bitwise-ior 12 10))
(test "xor 12 10"       6    (bitwise-xor 12 10))
(test "not 0"           -1   (bitwise-not 0))
(test "not -1"          0    (bitwise-not -1))
(test "not 255"         -256 (bitwise-not 255))
(test "and identity"    -1   (bitwise-and))
(test "ior identity"    0    (bitwise-ior))
(test "xor identity"    0    (bitwise-xor))
(test "and variadic"    8    (bitwise-and 15 12 10))
(test "ior variadic"    15   (bitwise-ior 1 2 4 8))
(test "shift left"      40   (arithmetic-shift 5 3))
(test "shift right"     5    (arithmetic-shift 10 -1))
(test "shift right neg" -3   (arithmetic-shift -5 -1))
(test "integer-length 0" 0   (integer-length 0))
(test "integer-length 1" 1   (integer-length 1))
(test "integer-length 255" 8 (integer-length 255))
(test "integer-length -1" 0  (integer-length -1))
(test "integer-length -128" 7 (integer-length -128))


(section "composite bitwise ops")

(test "nand 12 10"      -9   (bitwise-nand 12 10))
(test "nor 12 10"       -15  (bitwise-nor 12 10))
(test "andc1 12 10"     2    (bitwise-andc1 12 10))
(test "andc2 12 10"     4    (bitwise-andc2 12 10))
(test "orc1 12 10"      -5   (bitwise-orc1 12 10))
(test "orc2 12 10"      -3   (bitwise-orc2 12 10))
(test "eqv 12 10"       -7   (bitwise-eqv 12 10))
(test "eqv identity"    -1   (bitwise-eqv))
(test "eqv single"      (bitwise-not 42)  (bitwise-eqv 42))

;; verify identities
(test "nand = not(and)"  (bitwise-not (bitwise-and 12 10))  (bitwise-nand 12 10))
(test "nor = not(ior)"   (bitwise-not (bitwise-ior 12 10))  (bitwise-nor 12 10))
(test "andc1 = and(not,y)" (bitwise-and (bitwise-not 12) 10) (bitwise-andc1 12 10))
(test "andc2 = and(x,not)" (bitwise-and 12 (bitwise-not 10)) (bitwise-andc2 12 10))
(test "orc1 = or(not,y)"   (bitwise-ior (bitwise-not 12) 10) (bitwise-orc1 12 10))
(test "orc2 = or(x,not)"   (bitwise-ior 12 (bitwise-not 10)) (bitwise-orc2 12 10))
(test "eqv = not(xor)"  (bitwise-not (bitwise-xor 12 10))  (bitwise-eqv 12 10))


(section "single-bit operations")

(test-false "bit-set? 0 10"  (bit-set? 0 10))   ; 1010, bit0=0
(test-true  "bit-set? 1 10"  (bit-set? 1 10))   ; 1010, bit1=1
(test-false "bit-set? 2 10"  (bit-set? 2 10))   ; 1010, bit2=0
(test-true  "bit-set? 3 10"  (bit-set? 3 10))   ; 1010, bit3=1
(test-false "bit-set? 4 10"  (bit-set? 4 10))   ; 1010, bit4=0
(test-true  "bit-set? 0 -1"  (bit-set? 0 -1))   ; all 1s
(test-true  "bit-set? 100 -1" (bit-set? 100 -1))

(test "copy-bit set"    11   (copy-bit 0 10 #t))  ; 1010 -> 1011
(test "copy-bit clear"  8    (copy-bit 1 10 #f))  ; 1010 -> 1000
(test "copy-bit noop"   10   (copy-bit 1 10 #t))  ; already set
(test "copy-bit noop2"  10   (copy-bit 0 10 #f))  ; already clear
(test "copy-bit 0->1"   1    (copy-bit 0 0 #t))


(section "counting")

(test "bit-count 0"     0    (bit-count 0))
(test "bit-count 1"     1    (bit-count 1))
(test "bit-count 10"    2    (bit-count 10))     ; 1010
(test "bit-count 255"   8    (bit-count 255))
(test "bit-count -1"    0    (bit-count -1))     ; ~(-1)=0, popcount=0
(test "bit-count -2"    1    (bit-count -2))     ; ~(-2)=1, popcount=1
(test "bit-count bignum" 100 (bit-count (- (expt 2 100) 1)))  ; 100 ones

(test "first-set-bit 1"   0   (first-set-bit 1))
(test "first-set-bit 2"   1   (first-set-bit 2))
(test "first-set-bit 4"   2   (first-set-bit 4))
(test "first-set-bit 12"  2   (first-set-bit 12))  ; 1100
(test "first-set-bit 40"  3   (first-set-bit 40))  ; 101000


(section "bit-field")

(test "bit-field 255 0 4"   15   (bit-field 255 0 4))
(test "bit-field 255 4 8"   15   (bit-field 255 4 8))
(test "bit-field 255 0 8"   255  (bit-field 255 0 8))
(test "bit-field 0 0 4"     0    (bit-field 0 0 4))
(test "bit-field 170 0 4"   10   (bit-field 170 0 4))   ; 10101010 -> 1010
(test "bit-field 170 4 8"   10   (bit-field 170 4 8))   ; 10101010 -> 1010
(test "bit-field width 0"   0    (bit-field 255 4 4))   ; empty field


(section "bit-field-set")

(test "bfs 255 10 4 8"    175  (bit-field-set 255 10 4 8))   ; replace bits[4..8) with 1010
(test "bfs 0 15 0 4"      15   (bit-field-set 0 15 0 4))
(test "bfs 255 0 0 8"     0    (bit-field-set 255 0 0 8))    ; clear all 8 bits
(test "bfs no change"     255  (bit-field-set 255 15 0 4))   ; already 1111


(section "bit-field-rotate")

(test "bfr 6 1 1 4"       12   (bit-field-rotate 6 1 1 4))   ; 0110 -> 1100
(test "bfr 6 -1 1 4"      10   (bit-field-rotate 6 -1 1 4))  ; 0110 -> 1010 (right)
(test "bfr no rotate"     6    (bit-field-rotate 6 0 1 4))   ; no change
(test "bfr full rotate"   6    (bit-field-rotate 6 3 1 4))   ; rotate by width


(section "bit-field-reverse")

(test "bfrev 6 1 4"       12   (bit-field-reverse 6 1 4))    ; bits[1..4)=011 rev=110
(test "bfrev 0 0 4"       0    (bit-field-reverse 0 0 4))
(test "bfrev 15 0 4"      15   (bit-field-reverse 15 0 4))   ; 1111 reversed = 1111
(test "bfrev 9 0 4"       9    (bit-field-reverse 9 0 4))    ; 1001 reversed = 1001
(test "bfrev 5 0 4"       10   (bit-field-reverse 5 0 4))    ; 0101 rev = 1010


(section "bignum bitwise")

(test "and bignums"
  0
  (bitwise-and (expt 2 100) (- (expt 2 100) 1)))

(test "ior bignums"
  (- (expt 2 101) 1)
  (bitwise-ior (expt 2 100) (- (expt 2 100) 1)))

(test "shift bignum"
  (expt 2 200)
  (arithmetic-shift (expt 2 100) 100))

(test "bit-set? bignum"
  #t
  (bit-set? 100 (expt 2 100)))

(test "bit-count bignum"
  1
  (bit-count (expt 2 100)))

(test-true "exact? bitwise result"
  (exact? (bitwise-and 12 10)))


(test-summary "CN-BITWISE")
