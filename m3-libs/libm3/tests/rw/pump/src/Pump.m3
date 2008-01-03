(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Tue Nov  1 09:03:19 PST 1994 by kalsow  *)

UNSAFE MODULE Pump EXPORTS Main;

IMPORT Wr, Params, Lex, Stdio, Time, Fmt, TextF, Convert;

VAR buffer: ARRAY [0..1024*1024 - 1] OF CHAR;
   used: INTEGER;

BEGIN
  WITH repeat = Convert.ToInt(Params.Get(1)^, used),
       size   = Convert.ToInt(Params.Get(2)^, used),
       t0     = Time.Now()                           DO
    FOR i := 1 TO repeat DO
      Wr.PutString(Stdio.stdout, SUBARRAY(buffer, 0, size));
    END;
    WITH t1 = Time.Now() - t0 DO
      Wr.PutText(
        Stdio.stderr, "Time: " & Fmt.LongReal(t1) & " Chars: "
                        & Fmt.Int(repeat * size) & " Time/char: "
                        & Fmt.LongReal(t1 / FLOAT(repeat * size, LONGREAL),
                                       6, Fmt.Style.Sci) & "\n");
    END;
  END;
END Pump.


