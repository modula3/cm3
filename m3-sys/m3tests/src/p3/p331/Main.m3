(* Signed packed array elements read through a run-time subscript: the C
   backend asserted in extract_n, which did not sign-extend. *)

MODULE Main;
IMPORT IO, Fmt;
TYPE A = ARRAY [0..7] OF BITS 2 FOR [-2..1];
     B = ARRAY [0..5] OF BITS 5 FOR [-16..15];
     C = ARRAY [0..3] OF BITS 3 FOR [0..7];
VAR a := A{-2, -1, 0, 1, -2, -1, 0, 1};  b := B{-16, 15, -1, 0, 7, -9};  c := C{0, 7, 5, 2};
BEGIN
  IO.Put("a"); FOR i := 0 TO 7 DO IO.Put(" " & Fmt.Int(a[i])) END; IO.Put("\n");
  IO.Put("b"); FOR i := 0 TO 5 DO IO.Put(" " & Fmt.Int(b[i])) END; IO.Put("\n");
  IO.Put("c"); FOR i := 0 TO 3 DO IO.Put(" " & Fmt.Int(c[i])) END; IO.Put("\n");
END Main.
