(*
m3-sys/m3cc/AMD64_DARWIN-SOLgnu/gcc/m3cgc1 -munaligned-doubles -mcpu=v9 \
        -quiet -fno-reorder-blocks -funwind-tables -fPIC -m32 -O3 \
        -Wuninitialized -gstabs+ Poly.mc -o Poly.ms
m3core/src/fingerprint/Poly.m3: In function 'Poly__F2':
m3core/src/fingerprint/Poly.m3:14:0: internal compiler error:
  in referenced_var_lookup, at tree-dfa.c:519
*)

MODULE Main;

PROCEDURE F1 (x: INTEGER): INTEGER =
  BEGIN
    IF x = 0 THEN RETURN 0; END;
    RETURN 1;
  END F1;

<*NOWARN*>PROCEDURE F2 () =
  BEGIN
    EVAL F1(1);
  END F2;

BEGIN
END Main.
