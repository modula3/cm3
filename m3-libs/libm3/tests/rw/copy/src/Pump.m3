(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Tue Nov  1 09:03:50 PST 1994 by kalsow  *)

UNSAFE MODULE Pump EXPORTS Main;

IMPORT Wr, Rd, RdCopy, Params, Lex, Stdio, Time, Fmt, TextF, Convert, UnsafeRd;

VAR buffer: ARRAY [0..1024*1024 - 1] OF CHAR;
   used: INTEGER; moved, read: INTEGER := 0;

BEGIN
  WITH size   = Convert.ToInt(Params.Get(1)^, used),
       fast = Convert.ToInt(Params.Get(2)^, used),
       t0     = Time.Now()                           DO
    IF fast = 1 THEN
      moved := RdCopy.ToWriter(Stdio.stdin, Stdio.stdout);
    ELSE
      REPEAT
        read := Rd.GetSub(Stdio.stdin, SUBARRAY(buffer, 0, size));
        INC(moved, read);
        Wr.PutString(Stdio.stdout, SUBARRAY(buffer, 0, read));
      UNTIL read < size;
    END;
    WITH t1 = Time.Now() - t0 DO
      Wr.PutText(
        Stdio.stderr, "Time: " & Fmt.LongReal(t1) & " Chars: "
                        & Fmt.Int(moved) & " Time/char: "
                        & Fmt.LongReal(t1 / FLOAT(moved, LONGREAL),
                                       6, Fmt.Style.Sci) & "\n");
    END;
  END;
END Pump.
