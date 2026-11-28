MODULE Main;

(* ROUND breaks ties according to FloatMode.RoundDefault, NearestElseEven:
   halves round to the even neighbour.  The C backend used to take them
   away from zero. *)

IMPORT IO, Fmt;

VAR
  r := ARRAY [0..13] OF REAL {
         0.5, -0.5, 1.5, -1.5, 2.5, -2.5, 3.5, -3.5,
         0.49, 0.51, -0.51, 2.4999, 2.5001, 1.0E9 };
  l := ARRAY [0..9] OF LONGREAL {
         0.5D0, 1.5D0, 2.5D0, -2.5D0, 4503599627370495.5D0,
         -4503599627370496.5D0, 9007199254740993.0D0, 0.49999999999999994D0,
         -7.0D0, 1.0D18 };
  e := ARRAY [0..3] OF EXTENDED { 0.5X0, 2.5X0, -2.5X0, 3.5X0 };

BEGIN
  FOR i := FIRST (r) TO LAST (r) DO IO.Put (Fmt.Int (ROUND (r[i])) & " ") END;
  IO.Put ("\n");
  (* LONGINT: these exceed a 32-bit INTEGER, where ROUND (l[i]) overflows *)
  FOR i := FIRST (l) TO LAST (l) DO
    IO.Put (Fmt.LongInt (ROUND (l[i], LONGINT)) & " ")
  END;
  IO.Put ("\n");
  FOR i := FIRST (e) TO LAST (e) DO IO.Put (Fmt.Int (ROUND (e[i])) & " ") END;
  IO.Put ("\n");
  IO.Put (Fmt.LongInt (ROUND (2.5D0, LONGINT)) & " "
          & Fmt.LongInt (ROUND (-3.5D0, LONGINT)) & "\n");
END Main.
