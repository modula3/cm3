(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: M3FP.m3                                               *)
(* Last modified on Fri Jul 15 11:56:32 PDT 1994 by kalsow     *)
(*      modified on Fri May 28 15:51:55 PDT 1993 by muller     *)

UNSAFE MODULE M3FP;

IMPORT Fingerprint, Word;

TYPE 
  NChars = ARRAY [0..BYTESIZE (Int)-1] OF CHAR;
  MChars = ARRAY [0..BYTESIZE (T)-1] OF CHAR;

PROCEDURE FromText (t: TEXT): T =
  BEGIN
    RETURN Fingerprint.FromText (t);
  END FromText;

PROCEDURE FromChars (READONLY buf: ARRAY OF CHAR; READONLY t: T): T =
  BEGIN
    RETURN Fingerprint.FromChars (buf, t);
  END FromChars;

PROCEDURE Combine (a, b: T): T =
  VAR tmp: [0..255];
  BEGIN
    tmp   := a.byte [0];
    a.byte [0] := b.byte [3];
    b.byte [3] := b.byte [2];
    b.byte [2] := a.byte [7];
    a.byte [7] := a.byte [4];
    a.byte [4] := b.byte [6];
    b.byte [6] := b.byte [4];
    b.byte [4] := a.byte [1];
    a.byte [1] := a.byte [5];
    a.byte [5] := b.byte [0];
    b.byte [0] := b.byte [7];
    b.byte [7] := a.byte [3];
    a.byte [3] := a.byte [6];
    a.byte [6] := tmp;

    RETURN Fingerprint.FromChars (LOOPHOLE (b, MChars), a);
  END Combine;

PROCEDURE ExtendByInt (READONLY a: T;  i: Int): T =
  VAR buf: NChars;
  BEGIN
    FOR x := FIRST (buf) TO LAST (buf) DO
      buf [x] := VAL (Word.And (i, 16_ff), CHAR);
      i := Word.Shift (i, -8);
    END;
    RETURN Fingerprint.FromChars (buf, a);
  END ExtendByInt;

PROCEDURE ToInt (READONLY t: T): INTEGER =
  CONST
    Sign = 16_80000000;
    SignExtend = Word.LeftShift (Word.Not (0), 31);
  VAR a, b, c: INTEGER;
  BEGIN
    a := Word.Or (Word.Or (                t.byte[0],
                           Word.LeftShift (t.byte[1], 8)),
                  Word.Or (Word.LeftShift (t.byte[2], 16),
                           Word.LeftShift (t.byte[3], 24)));
    b := Word.Or (Word.Or (                t.byte[4],
                           Word.LeftShift (t.byte[5], 8)),
                  Word.Or (Word.LeftShift (t.byte[6], 16),
                           Word.LeftShift (t.byte[7], 24)));
    c := Word.Xor (a, b);
    IF Word.And (c, Sign) # 0 THEN c := Word.Or (SignExtend, c); END;
    RETURN c;
  END ToInt;

PROCEDURE FromInt (a: INTEGER;  VAR t: T) =
  BEGIN
    t.byte [0] := 0;
    t.byte [1] := 0;
    t.byte [2] := 0;
    t.byte [3] := 0;
    t.byte [4] := Word.Extract (a, 0, 8);
    t.byte [5] := Word.Extract (a, 8, 8);
    t.byte [6] := Word.Extract (a, 16, 8);
    t.byte [7] := Word.Extract (a, 24, 8);
  END FromInt;

PROCEDURE ToChars (READONLY t: T;  VAR buf: CharBuf) =
  CONST Map = ARRAY [0..7] OF [0..7] { 3, 2, 1, 0, 7, 6, 5, 4 };
  CONST Digit = ARRAY [0..15] OF CHAR { '0','1','2','3','4','5','6','7',
                                        '8','9','a','b','c','d','e','f' };
  VAR j := 0;  k: INTEGER;
  BEGIN
    FOR i := FIRST (t.byte) TO LAST (t.byte) DO
      k := t.byte [Map [i]];
      buf[j] := Digit [Word.Divide (k, 16)];  INC (j);
      buf[j] := Digit [Word.Mod (k, 16)];  INC (j);
    END;
  END ToChars;

BEGIN
  OfEmpty := Fingerprint.FromText ("");
END M3FP.
