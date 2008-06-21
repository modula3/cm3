(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Dec  8 09:51:33 PST 1994 by kalsow     *)

MODULE Wx;

IMPORT Text, Text8, Convert, Wr, Thread, TCP;

CONST
  ChunkSize = 2 * 1024 - 3 * BYTESIZE (INTEGER);
  (* leave some slop for the 'next' pointer & the allocator overhead *)

TYPE
  Chunk = REF RECORD
            next : Chunk := NIL;
            buf  : ARRAY [0..ChunkSize-1] OF CHAR;
          END;

REVEAL
  T = T_ BRANDED "Wx.T" OBJECT
        nFull   : INTEGER := 0;
        next    : INTEGER := 0;
        head    : Chunk   := NIL;
        tail    : Chunk   := NIL;
        drain   : TCP.T   := NIL;
      OVERRIDES
        init    := Init;
        flush   := Flush;
        put     := PutText;
        putChar := PutChar;
        putInt  := PutInt;
        putStr  := PutStr;
        putSub  := PutSub;
        toText  := ToText;
      END;

PROCEDURE Init (t: T;  drain: TCP.T): T =
  BEGIN
    t.nFull   := 0;
    t.next    := 0;
    t.drain   := drain;
    IF (t.head = NIL) THEN
      t.head := NEW (Chunk);
      t.tail := t.head;
    END;
    RETURN t;
  END Init;

PROCEDURE Flush (t: T)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR c := t.head;
  BEGIN
    IF (t.drain # NIL) THEN
      TRY
        FOR i := 1 TO t.nFull DO  t.drain.put (c.buf); c := c.next;  END;
        IF (t.next # 0) THEN  t.drain.put (SUBARRAY (c.buf, 0, t.next)); END;
      FINALLY
        Reset (t);
      END;
    END;
  END Flush;

PROCEDURE PutText (t: T;  a, b, c, d: TEXT := NIL)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (a # NIL) THEN DoPutSub (t, a, 0, Text.Length (a)); END;
    IF (b # NIL) THEN DoPutSub (t, b, 0, Text.Length (b)); END;
    IF (c # NIL) THEN DoPutSub (t, c, 0, Text.Length (c)); END;
    IF (d # NIL) THEN DoPutSub (t, d, 0, Text.Length (d)); END;
  END PutText;

PROCEDURE PutSub (t: T;  txt: TEXT;  start, len: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (txt # NIL) THEN
      start := MAX (0, start);
      len   := MIN (len, Text.Length (txt) - start);
      IF (len > 0) THEN DoPutSub (t, txt, start, len); END;
    END;
  END PutSub;

PROCEDURE DoPutSub (t: T;  a: TEXT;  start, len: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR buf: ARRAY [0..255] OF CHAR;
  BEGIN
    WHILE (len > 0) DO
      Text.SetChars (buf, a, start);
      WITH n = MIN (NUMBER (buf), len) DO
        PutStr (t, SUBARRAY (buf, 0, n));
        INC (start, n);
        DEC (len, n);
      END;
    END;
  END DoPutSub;

PROCEDURE PutChar (t: T;  ch: CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (t.next >= ChunkSize) THEN Expand (t) END;
    t.tail.buf[t.next] := ch;
    INC (t.next);
  END PutChar;

PROCEDURE PutStr (t: T;  READONLY x: ARRAY OF CHAR)
  RAISES {Wr.Failure, Thread.Alerted} =
  VAR
    next := 0;
    len  := NUMBER (x);
    n : INTEGER;
  BEGIN
    IF (len < ChunkSize - t.next) THEN
      SUBARRAY (t.tail.buf, t.next, len) := x;
      INC (t.next, len);
    ELSE
      WHILE (len > 0) DO
        n := MIN (len, ChunkSize - t.next);
        SUBARRAY (t.tail.buf, t.next, n) := SUBARRAY (x, next, n);
        DEC (len, n);
        INC (next, n);
        INC (t.next, n);
        IF (len > 0) THEN Expand (t) END;
      END;
    END;
  END PutStr;

CONST digits = ARRAY [0..9] OF CHAR {'0','1','2','3','4','5','6','7','8','9'};

CONST digits_100A = ARRAY [0..99] OF CHAR {
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
  };

CONST digits_100B = ARRAY [0..99] OF CHAR {
  '0', '0', '0', '0', '0', '0', '0', '0', '0', '0',
  '1', '1', '1', '1', '1', '1', '1', '1', '1', '1',
  '2', '2', '2', '2', '2', '2', '2', '2', '2', '2',
  '3', '3', '3', '3', '3', '3', '3', '3', '3', '3',
  '4', '4', '4', '4', '4', '4', '4', '4', '4', '4',
  '5', '5', '5', '5', '5', '5', '5', '5', '5', '5',
  '6', '6', '6', '6', '6', '6', '6', '6', '6', '6',
  '7', '7', '7', '7', '7', '7', '7', '7', '7', '7',
  '8', '8', '8', '8', '8', '8', '8', '8', '8', '8',
  '9', '9', '9', '9', '9', '9', '9', '9', '9', '9'
  };

PROCEDURE PutInt  (t: T;  i: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    IF (i < 0) THEN
      PutBigInt (t, i);
    ELSIF (i < 10) THEN
      PutChar (t, digits[i]);
    ELSIF (i < 100) THEN
      PutChar (t, digits_100B[i]);
      PutChar (t, digits_100A[i]);
    ELSIF (i < 1000) THEN
      PutChar (t, digits[i DIV 100]);   i := i MOD 100;
      PutChar (t, digits_100B[i]);
      PutChar (t, digits_100A[i]);
    ELSE
      PutBigInt (t, i);
    END;
  END PutInt;

PROCEDURE PutBigInt  (t: T;  i: INTEGER)
  RAISES {Wr.Failure, Thread.Alerted} =
  <*FATAL Convert.Failed*>
  VAR
    buf : ARRAY [0..BITSIZE(INTEGER) + 3] OF CHAR;
    len := Convert.FromInt (buf, i, 10);
  BEGIN
    PutStr (t, SUBARRAY (buf, 0, len));  
  END PutBigInt;

PROCEDURE ToText (t: T): TEXT =
  VAR txt: TEXT;
  BEGIN
    IF (t.nFull = 0)
      THEN txt := Text.FromChars (SUBARRAY (t.head.buf, 0, t.next));
      ELSE txt := MessyToText (t);
    END;
    Reset (t);
    RETURN txt;
  END ToText;

PROCEDURE MessyToText (t: T): TEXT =
  VAR
    len := t.nFull * ChunkSize + t.next;
    txt := Text8.Create (len);
    c   := t.head;
    n   := 0;
  BEGIN
    FOR i := 1 TO t.nFull DO
      SUBARRAY (txt.contents^, n, ChunkSize) := c.buf;
      c := c.next;
      INC (n, ChunkSize);
    END;
    IF (t.next # 0) THEN
      SUBARRAY (txt.contents^, n, t.next) := SUBARRAY (c.buf, 0, t.next);
    END;
    RETURN txt;
  END MessyToText;

(*------------------------------------ internal ----------------------------*)

PROCEDURE Expand (t: T)
  RAISES {Wr.Failure, Thread.Alerted} =
  BEGIN
    <* ASSERT t.next = ChunkSize *>
    IF (t.drain # NIL) THEN
      t.next := 0;
      INC (t.nFull); (* for the flush... *)
      Flush (t);
    ELSE
      IF (t.tail.next = NIL) THEN t.tail.next := NEW (Chunk); END;
      t.tail := t.tail.next;
      t.next := 0;
      INC (t.nFull);
    END;
  END Expand;

PROCEDURE Reset (t: T) =
  BEGIN
    (* NOTE: we're not freeing the allocated chunks... *)
    t.tail  := t.head;
    t.nFull := 0;
    t.next  := 0;
  END Reset;

BEGIN
END Wx.
