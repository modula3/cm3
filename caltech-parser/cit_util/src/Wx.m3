(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Dec  8 09:51:33 PST 1994 by kalsow     *)

MODULE Wx;

IMPORT TextF, Convert;

CONST
  ChunkSize = 2 * 1024 - 3 * BYTESIZE (INTEGER);
  (* leave some slop for the 'next' pointer & the allocator overhead *)

TYPE
  Chunk = REF RECORD
            next : Chunk := NIL;
            buf  : ARRAY [0..ChunkSize-1] OF CHAR;
          END;

REVEAL
  T = BRANDED "Wx.T" REF RECORD
        nFull : INTEGER;
        next  : INTEGER;
        head  : Chunk;
        tail  : Chunk;
      END;

PROCEDURE New (): T =
  VAR t := NEW (T);
  BEGIN
    t.nFull := 0;
    t.next  := 0;
    t.head  := NEW (Chunk);
    t.tail  := t.head;
    RETURN t;
  END New;

PROCEDURE PutChar (t: T;  ch: CHAR) =
  BEGIN
    IF (t.next >= ChunkSize) THEN Expand (t) END;
    t.tail.buf[t.next] := ch;
    INC (t.next);
  END PutChar;

PROCEDURE PutStr (t: T;  READONLY x: ARRAY OF CHAR) =
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

PROCEDURE PutText (t: T;  txt: TEXT) =
  BEGIN
    PutStr (t, SUBARRAY (txt^, 0, LAST (txt^)));
  END PutText;

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

PROCEDURE PutInt  (t: T;  i: INTEGER) =
  <*FATAL Convert.Failed*>
  VAR len: INTEGER;  buf: ARRAY [0..BITSIZE(INTEGER) + 3] OF CHAR;
  BEGIN
    IF (0 <= i) THEN
      IF (i < 10) THEN
        PutChar (t, digits[i]);
        RETURN;
      ELSIF (i < 100) THEN
        PutChar (t, digits_100B[i]);
        PutChar (t, digits_100A[i]);
        RETURN;
      ELSIF (i < 1000) THEN
        PutChar (t, digits[i DIV 100]);
        PutChar (t, digits[(i DIV 10) MOD 10]);
        PutChar (t, digits[i MOD 10]);
        RETURN;
      END;
    END;
    len := Convert.FromInt (buf, i, 10);
    PutStr (t, SUBARRAY (buf, 0, len));  
  END PutInt;

PROCEDURE ToText (t: T): TEXT =
  VAR txt := NEW (TEXT, t.nFull * ChunkSize + t.next + 1);
  VAR c := t.head;   n := 0;
  BEGIN
    FOR i := 1 TO t.nFull DO
      SUBARRAY (txt^, n, ChunkSize) := c.buf;
      c := c.next;
      INC (n, ChunkSize);
    END;
    IF (t.next # 0) THEN
      SUBARRAY (txt^, n, t.next) := SUBARRAY (c.buf, 0, t.next);
    END;
    txt [LAST (txt^)] := '\000';
    Reset (t);
    RETURN txt;
  END ToText;

(*------------------------------------ internal ----------------------------*)

PROCEDURE Expand (t: T) =
  BEGIN
    <* ASSERT t.next = ChunkSize *>
    IF (t.tail.next = NIL) THEN t.tail.next := NEW (Chunk); END;
    t.tail := t.tail.next;
    t.next := 0;
    INC (t.nFull);
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
