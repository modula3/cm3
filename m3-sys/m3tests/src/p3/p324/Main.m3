MODULE Main;

(* ROUND of constants, folded by the compiler, breaks ties according to
   FloatMode.RoundDefault, NearestElseEven, as p306 checks at run time.
   The fold used the ROUND of the compiler that built the compiler, so a
   compiler built before the C back end was fixed (#1281) folded 2.5 to 3. *)

IMPORT IO, Fmt;

CONST
  R = ARRAY [0..13] OF INTEGER {
        ROUND (0.5), ROUND (-0.5), ROUND (1.5), ROUND (-1.5),
        ROUND (2.5), ROUND (-2.5), ROUND (3.5), ROUND (-3.5),
        ROUND (0.49), ROUND (0.51), ROUND (-0.51), ROUND (2.4999),
        ROUND (2.5001), ROUND (1.0E9) };
  (* LONGINT: these exceed a 32-bit INTEGER *)
  L = ARRAY [0..10] OF LONGINT {
        ROUND (0.5D0, LONGINT), ROUND (1.5D0, LONGINT),
        ROUND (2.5D0, LONGINT), ROUND (-2.5D0, LONGINT),
        ROUND (4503599627370495.5D0, LONGINT),
        ROUND (-4503599627370496.5D0, LONGINT),
        ROUND (9007199254740993.0D0, LONGINT),
        ROUND (0.49999999999999994D0, LONGINT),
        ROUND (-0.49999999999999994D0, LONGINT), ROUND (-7.0D0, LONGINT),
        ROUND (1.0D18, LONGINT) };
  E = ARRAY [0..3] OF INTEGER {
        ROUND (0.5X0), ROUND (2.5X0), ROUND (-2.5X0), ROUND (3.5X0) };
  LI = ROUND (2.5D0, LONGINT);
  LJ = ROUND (-3.5D0, LONGINT);

BEGIN
  FOR i := FIRST (R) TO LAST (R) DO IO.Put (Fmt.Int (R[i]) & " ") END;
  IO.Put ("\n");
  FOR i := FIRST (L) TO LAST (L) DO IO.Put (Fmt.LongInt (L[i]) & " ") END;
  IO.Put ("\n");
  FOR i := FIRST (E) TO LAST (E) DO IO.Put (Fmt.Int (E[i]) & " ") END;
  IO.Put ("\n");
  IO.Put (Fmt.LongInt (LI) & " " & Fmt.LongInt (LJ) & "\n");
END Main.
