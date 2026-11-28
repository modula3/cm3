MODULE Main;

(* FLOOR, CEILING, ROUND and TRUNC to LONGINT, and FLOAT of a LONGINT, of
   values beyond 32 bits.  The I386 integrated back end converted them
   with the 32-bit FISTP and FILD whatever the type, so FLOOR (x, LONGINT)
   gave an INTEGER (and M3CG_Check failed); the values here are not
   constants, so nothing is folded. *)

IMPORT IO, Fmt;

VAR
  x := ARRAY [0..5] OF LONGREAL {
         4294967296.75D0, -4294967296.75D0, 9.0D15 + 0.5D0,
         -1.0D18, 2147483648.5D0, 1.0D0 };
  n := ARRAY [0..4] OF LONGINT {
         4294967296L, -4294967296L, 9007199254740993L,
         16_7FFFFFFFFFFFFFFFL, -16_7FFFFFFFFFFFFFFFL - 1L };

BEGIN
  FOR i := FIRST (x) TO LAST (x) DO
    IO.Put (Fmt.LongInt (FLOOR (x[i], LONGINT)) & " "
            & Fmt.LongInt (CEILING (x[i], LONGINT)) & " "
            & Fmt.LongInt (ROUND (x[i], LONGINT)) & " "
            & Fmt.LongInt (TRUNC (x[i], LONGINT)) & "\n");
  END;
  FOR i := FIRST (n) TO LAST (n) DO
    IO.Put (Fmt.LongReal (FLOAT (n[i], LONGREAL)) & "\n");
  END;
END Main.
