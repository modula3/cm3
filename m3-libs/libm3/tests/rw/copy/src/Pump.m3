(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Tue Nov  1 09:03:50 PST 1994 by kalsow  *)

UNSAFE MODULE Pump EXPORTS Main;

IMPORT Wr, Rd, RdCopy, Params, Stdio, Time, Text, Fmt, Convert, Thread,
       TextClass, IO; <* NOWARN *>

<* FATAL Thread.Alerted, Rd.Failure, Wr.Failure *>

VAR 
  buffer: ARRAY [0..1024*1024 - 1] OF CHAR;
  moved, read: INTEGER := 0;

PROCEDURE ToInt( s: TEXT ): INTEGER =
  VAR 
    used: INTEGER;
    len := Text.Length( s );
    buf := NEW( REF ARRAY OF CHAR, len );
  BEGIN
    TextClass.GetChars( s, buf^, 0 );
    RETURN Convert.ToInt( buf^, used );
  END ToInt;

BEGIN
  WITH size = ToInt(Params.Get(1)),
       fast = ToInt(Params.Get(2)),
       t0   = Time.Now()          DO
    (*
    IO.Put( "size = " & Fmt.Int( size ) & "\n");
    IO.Put( "fast = " & Fmt.Int( fast ) & "\n");
    *)
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
        Stdio.stdout, "Time: " & Fmt.LongReal(t1) & " Chars: "
                        & Fmt.Int(moved) & " Time/char: "
                        & Fmt.LongReal(t1 / FLOAT(moved, LONGREAL),
                                       Fmt.Style.Sci, 6) & "\n");
    END;
  END;
END Pump.
