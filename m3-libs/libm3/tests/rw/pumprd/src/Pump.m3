(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Tue Nov  1 09:05:25 PST 1994 by kalsow  *)

UNSAFE MODULE Pump EXPORTS Main;

IMPORT Wr, Rd, Params, Lex, Stdio, Time, Fmt, TextF, Convert, UnsafeRd, Process;

VAR buffer: ARRAY [0..1024*1024 - 1] OF CHAR;
   used: INTEGER; chars: CARDINAL;

BEGIN
  WITH repeat = Convert.ToInt(Params.Get(1)^, used),
       size   = Convert.ToInt(Params.Get(2)^, used),
       t0     = Time.Now()                           DO
    FOR i := 1 TO repeat DO
      IF size # Rd.GetSub(Stdio.stdin, SUBARRAY(buffer, 0, size)) THEN
        Wr.PutText(Stdio.stderr, "Ran out of input\n");
        Process.Exit(1);
      END;
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
