UNSAFE MODULE Main;

(* LOOPHOLE between REAL and a 32-bit packed subrange of INTEGER.  On a
   64-bit target the subrange's stack type is Int64, and the C back end
   converted the value instead of reinterpreting its bits (libm3's
   PickleStubs.SwapReal, via Swap.Int32), so LOOPHOLE (1.5, Int32) was 1. *)

IMPORT IO, Fmt, Swap;

TYPE
  Int32 = Swap.Int32;                        (* BITS 32 FOR [-2^31 .. 2^31-1] *)
  Word32 = BITS 32 FOR [0 .. 16_FFFFFFFF];

PROCEDURE SwapReal (x: REAL): REAL =
  BEGIN
    RETURN LOOPHOLE (Swap.Swap4 (LOOPHOLE (x, Int32)), REAL);
  END SwapReal;

PROCEDURE Hex (i: INTEGER): TEXT =
  BEGIN
    RETURN Fmt.Int (i, 16);
  END Hex;

VAR
  r  := 1.5;
  n  := -1.5;
  i  : Int32 := 16_40490FDB;                 (* pi *)
  w  : Word32 := 16_C0000000;                (* -2.0 *)

BEGIN
  IO.Put ("REAL -> Int32  " & Hex (LOOPHOLE (r, Int32)) & " "
          & Hex (LOOPHOLE (n, Int32)) & "\n");
  IO.Put ("REAL -> Word32 " & Hex (LOOPHOLE (r, Word32)) & " "
          & Hex (LOOPHOLE (n, Word32)) & "\n");
  IO.Put ("Int32 -> REAL  " & Fmt.Real (LOOPHOLE (i, REAL), prec := 7) & "\n");
  IO.Put ("Word32 -> REAL " & Fmt.Real (LOOPHOLE (w, REAL)) & "\n");
  IO.Put ("SwapReal       " & Hex (LOOPHOLE (SwapReal (r), Int32)) & " "
          & Fmt.Real (SwapReal (SwapReal (r))) & " "
          & Fmt.Real (SwapReal (SwapReal (n))) & "\n");
END Main.
