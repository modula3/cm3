(* Copyright (C) 1994, Digital Equipment Corporation        *)
(* All rights reserved.                                     *)
(* See the file COPYRIGHT for a full description.           *)
(*                                                          *)
(* Last modified on Tue Nov  1 09:02:24 PST 1994 by kalsow  *)

MODULE AutoTest EXPORTS Main;

IMPORT Wr, AutoFlushWr, Rd, Params, TextRd, Lex, Stdio, Time, Fmt, Random, Thread;

VAR buffer: ARRAY [0..1024*1024 - 1] OF CHAR;

VAR total := 0;
BEGIN
  WITH repeat = Lex.Int(NEW(TextRd.T).init(Params.Get(1))),
       size   = Lex.Int(NEW(TextRd.T).init(Params.Get(2))),
       pause  = Lex.LongReal(NEW(TextRd.T).init(Params.Get(3))),
       flush  = Lex.LongReal(NEW(TextRd.T).init(Params.Get(4))),
       wr     = NEW(AutoFlushWr.T).init(Stdio.stdout, flush),
       r      = NEW(Random.Default).init()                       DO
    FOR i := 1 TO repeat DO
      Thread.Pause(r.longreal() * pause);
      WITH bufSize = r.integer(1, size),
           read    = Rd.GetSub(Stdio.stdin, SUBARRAY(buffer, 0, bufSize)) DO
        Wr.PutString(wr, SUBARRAY(buffer, 0, read));
        INC(total, read);
        IF read < bufSize THEN EXIT; END;
      END;
    END;
  END;
  Wr.PutText(Stdio.stderr, "Bytes written: " & Fmt.Int(total) & "\n");
END AutoTest.
