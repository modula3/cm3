(* Copyright 1996, Critical Mass, Inc.   All rights reserved. *)

MODULE LineWr;

IMPORT WrClass, Text, Thread, Wr;

REVEAL
  T = Public BRANDED OBJECT
        user_proc : CallBack   := NIL;
        user_ref  : REFANY     := NIL;
        overflow  : TEXT       := NIL;
        line_len  : INTEGER    := 0;
        line      : ARRAY [0..255] OF CHAR;
      OVERRIDES
        seek   := Seek;
        flush  := Flush;
        close  := Close;
        length := Length;
        init   := Init;
      END;

PROCEDURE Init (wr: T;  proc: CallBack;  ref: REFANY): T =
  BEGIN
    WrClass.Lock(wr);
    TRY
      (* LineWr fields *)
      wr.user_proc := proc;
      wr.user_ref  := ref;
      wr.overflow  := NIL;
      wr.line_len  := 0;

      (* generic Wr fields *)
      IF (wr.buff = NIL) THEN  wr.buff := NEW (REF ARRAY OF CHAR, 1); END;
      wr.st        := 0;
      wr.cur       := 0;
      wr.lo        := 0;
      wr.hi        := 1;
      wr.closed    := FALSE;
      wr.seekable  := FALSE;
      wr.buffered  := FALSE;
    FINALLY
      WrClass.Unlock(wr);
    END;
    RETURN wr;
  END Init;

PROCEDURE New (proc: CallBack;  ref: REFANY): T =
  BEGIN
    RETURN NEW (T).init (proc, ref);
  END New;

PROCEDURE Clear (wr: T) RAISES {} =
  BEGIN
    IF wr.line_len > 0 THEN
      TRY
        FlushLine (wr);
      EXCEPT Wr.Failure, Thread.Alerted =>
        (* ignore *)
      END;
    END;
  END Clear;

PROCEDURE Length (wr: T): LONGINT RAISES {} =
  BEGIN
    RETURN wr.cur;
  END Length;

PROCEDURE Seek(wr: T; n: LONGINT)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    <*ASSERT wr.cur = wr.hi *>
    AddChar (wr, wr.buff[0]);
    wr.cur := n;
    wr.lo  := n;
    wr.hi  := n + 1L;
  END Seek;

PROCEDURE Flush(wr: T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF wr.cur > wr.lo THEN
      AddChar (wr, wr.buff[0]);
      INC (wr.lo);
      INC (wr.hi);
    END;
  END Flush;

PROCEDURE Close (wr: T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF wr.line_len > 0 THEN  FlushLine (wr);  END;
    wr.buff      := NIL;
    wr.user_proc := NIL;
    wr.user_ref  := NIL;
    wr.line_len  := 0;
    wr.overflow  := NIL;
  END Close;

PROCEDURE AddChar (wr: T;  c: CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (c = '\r') THEN
      (* ignore *)
    ELSIF (c = '\n') THEN
      FlushLine (wr);
    ELSE (* regular character, add it to the current line *)
      IF (wr.line_len > LAST (wr.line)) THEN
        IF (wr.overflow = NIL) THEN wr.overflow := ""; END;
        wr.overflow := wr.overflow & Text.FromChars (wr.line);
        wr.line_len := 0;
      END;
      wr.line [wr.line_len] := c;
      INC (wr.line_len);
    END;
  END AddChar;

PROCEDURE FlushLine (wr: T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR txt := Text.FromChars (SUBARRAY (wr.line, 0, wr.line_len));
  BEGIN
    IF wr.overflow # NIL THEN txt := wr.overflow & txt; END;
    wr.user_proc (wr.user_ref, txt);
    wr.line_len := 0;
    wr.overflow := NIL;
  END FlushLine;

BEGIN
END LineWr.
