(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Jun 20 15:34:10 PDT 1994 by kalsow     *)

UNSAFE MODULE RTPacking;

IMPORT FloatMode, Word;

CONST
  Bits = ARRAY [0..3] OF INTEGER { 8, 16, 32, 64 };

VAR
  init_done := FALSE;
  local     : T;

PROCEDURE Local (): T =
  VAR
    a: RECORD ch: CHAR;  x: EXTENDED; END;
    b: RECORD ch: CHAR;  x: RECORD ch: CHAR; END; END;
    i: INTEGER := 1;
    x: ADDRESS := ADR (i);
    p: UNTRACED REF CHAR := x;
  BEGIN
    IF NOT init_done THEN
      local.word_size     := SizeOf (ADRSIZE (INTEGER));
      local.max_align     := SizeOf (ADR (a.x) - ADR (a.ch));
      local.struct_align  := SizeOf (ADR (b.x) - ADR (b.ch));
      local.little_endian := (p^ = VAL (1, CHAR));
      local.float         := FloatKind.IEEE;
      IF NOT FloatMode.IEEE THEN local.float := FloatKind.VAX; END;
      init_done := TRUE;
    END;
    RETURN local;
  END Local;

PROCEDURE SizeOf (n: INTEGER): CARDINAL =
  BEGIN
    (* convert address units to bits *)
    n := n DIV ADRSIZE(CHAR) * BITSIZE(CHAR);

    (* look for a known size *)
    FOR i := FIRST (Bits) TO LAST (Bits) DO
      IF (Bits[i] = n) THEN RETURN n; END;
    END;
    <*ASSERT FALSE*>
  END SizeOf;

PROCEDURE Encode (READONLY t: T): INTEGER =
  VAR n := 0;
  BEGIN
    n := Word.Or (Word.Shift (n, 2), BitSize (t.word_size));
    n := Word.Or (Word.Shift (n, 2), BitSize (t.max_align));
    n := Word.Or (Word.Shift (n, 2), BitSize (t.struct_align));
    n := Word.Or (Word.Shift (n, 1), ORD (t.little_endian));
    n := Word.Or (Word.Shift (n, 2), ORD (t.float));
    RETURN n;
  END Encode;

PROCEDURE Decode (i: INTEGER): T =
  VAR t: T;
  BEGIN
    t.float := VAL (Word.And (i, 3), FloatKind); i := Word.Shift (i, -2);
    t.little_endian := VAL (Word.And (i, 1), BOOLEAN); i := Word.Shift (i, -1);
    t.struct_align  := Bits[Word.And (i, 3)];    i := Word.Shift (i, -2);
    t.max_align     := Bits[Word.And (i, 3)];    i := Word.Shift (i, -2);
    t.word_size     := Bits[Word.And (i, 3)];    i := Word.Shift (i, -2);
    <*ASSERT i = 0*>
    RETURN t;
  END Decode;

PROCEDURE BitSize (n: CARDINAL): CARDINAL =
  BEGIN
    FOR i := FIRST (Bits) TO LAST (Bits) DO
      IF (Bits[i] = n) THEN RETURN i; END;
    END;
    <*ASSERT FALSE*>
  END BitSize;

BEGIN
END RTPacking.
