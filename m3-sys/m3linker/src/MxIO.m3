(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Mx.m3                                                 *)
(* Last Modified On Mon Sep 19 14:22:36 PDT 1994 By kalsow     *)

MODULE MxIO;

IMPORT Word, M3FP, M3Buf, Mx;

CONST
  HexDigit = ARRAY [0..15] OF CHAR {
                 '0','1','2','3','4','5','6','7',
                 '8','9','a','b','c','d','e','f' };

PROCEDURE PutTxt (wr: M3Buf.T;  a, b, c, d, e: TEXT := NIL) =
  BEGIN
    IF (a # NIL) THEN M3Buf.PutText (wr, a); END; 
    IF (b # NIL) THEN M3Buf.PutText (wr, b); END; 
    IF (c # NIL) THEN M3Buf.PutText (wr, c); END; 
    IF (d # NIL) THEN M3Buf.PutText (wr, d); END; 
    IF (e # NIL) THEN M3Buf.PutText (wr, e); END; 
  END PutTxt;

PROCEDURE PutCh  (wr: M3Buf.T;  ch: CHAR) =
  BEGIN
    M3Buf.PutChar (wr, ch);
  END PutCh;

PROCEDURE PutInt (wr: M3Buf.T;  i: INTEGER;  ch: TEXT) =
  BEGIN
    M3Buf.PutInt (wr, i);
    M3Buf.PutText (wr, ch);
  END PutInt;

PROCEDURE PutFP (wr: M3Buf.T;  READONLY x: M3FP.T;  ch: TEXT) =
  VAR n, j: INTEGER;  buf: ARRAY [0 .. 2 * NUMBER (x.byte) - 1] OF CHAR;
  BEGIN
    j := 0;
    FOR i := FIRST (x.byte) TO LAST (x.byte) DO
      n := x.byte[i];
      buf[j] := HexDigit [Word.RightShift (n, 4)];  INC (j);
      buf[j] := HexDigit [Word.And (n, 16_f)];      INC (j);
    END;
    M3Buf.PutSub (wr, buf);
    M3Buf.PutText (wr, ch);
  END PutFP;

PROCEDURE PutHex (wr: M3Buf.T;  xx: Mx.Int32;  ch: TEXT) =
  VAR buf: ARRAY [0..7] OF CHAR;  x: INTEGER := xx;
  BEGIN
    FOR j := 7 TO 0 BY -1 DO
      buf[j] := HexDigit [Word.Mod (x, 16)];
      x := Word.Divide (x, 16);
    END;
    M3Buf.PutSub (wr, buf);
    M3Buf.PutText (wr, ch);
  END PutHex;

BEGIN
END MxIO.
