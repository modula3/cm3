(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Thu Apr  7 16:17:52 PDT 1994 by kalsow                   *)

UNSAFE MODULE DBWr;

IMPORT FS, File, OSError, TextF;

CONST BIG = 16_1000000; (* 2^24 => 16M *)
TYPE BigPtr = UNTRACED REF ARRAY [0..BIG-1] OF File.Byte;

CONST BufSize = 4096;
TYPE BufPtr = UNTRACED REF ARRAY [0..BufSize-1] OF File.Byte;

REVEAL
  T = T_ BRANDED OBJECT
    file: File.T;
    buf : ARRAY [0..BufSize-1] OF CHAR;
    ptr : BufPtr;
    len : INTEGER;
  OVERRIDES
    init     := Init;
    put_int  := PutInt;
    put_line := PutLine;
    close    := Close;
  END;

PROCEDURE Init (t: T;  path: TEXT): T =
  <*FATAL OSError.E*>
  BEGIN
    t.file := FS.OpenFile (path);
    t.ptr  := ADR (t.buf[0]);
    t.len  := 0;
    RETURN t;
  END Init;

PROCEDURE PutInt (t: T;  i: INTEGER) =
  VAR digits: ARRAY [0..BITSIZE(INTEGER)] OF CHAR;  next := LAST (digits);
  BEGIN
    digits[next] := '\n';  DEC (next);
    REPEAT
      digits [next] := VAL (i MOD 10 + ORD ('0'), CHAR);  DEC (next);
      i := i DIV 10;
    UNTIL (i = 0);
    PutBuf (t, SUBARRAY (digits, next+1, LAST (digits) - next));
  END PutInt;

VAR newline := ARRAY [0..0] OF CHAR { '\n' };

PROCEDURE PutLine (t: T;  txt: TEXT) =
  BEGIN
    PutBuf (t, SUBARRAY (txt^, 0, LAST (txt^)));
    PutBuf (t, newline);
  END PutLine;

PROCEDURE PutBuf (t: T;  READONLY buf: ARRAY OF CHAR) =
  <*FATAL OSError.E*>
  VAR
    len   := NUMBER (buf);
    empty := NUMBER (t.buf) - t.len;
    ptr: BigPtr;
  BEGIN
    IF (len > empty) THEN
      IF (t.len > 0) THEN t.file.write (SUBARRAY (t.ptr^, 0, t.len)); END;
      t.len := 0;
      empty := NUMBER (t.buf);
    END;

    IF (len > empty) THEN
      ptr := ADR (buf[0]);
      t.file.write (SUBARRAY (ptr^, 0, len));
    ELSE
      SUBARRAY (t.buf, t.len, len) := buf;
      INC (t.len, len);
    END;
  END PutBuf;

PROCEDURE Close (t: T) =
  <*FATAL OSError.E *>
  BEGIN
    IF (t.len > 0) THEN t.file.write (SUBARRAY (t.ptr^, 0, t.len)); END;
    t.file.close ();
  END Close;


BEGIN
END DBWr.
