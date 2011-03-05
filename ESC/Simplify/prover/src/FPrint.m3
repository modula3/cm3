(* Copyright (C) 2002 Hewlett-Packard Company *)
(* Copyright (C) 2000, 2002 Compaq Computer Corporation *)
(* Copyright 1996 by Digital  *)
(* modified on Tue Mar 2 22:25:09 PST 1993 by gnelson *)

UNSAFE MODULE FPrint; 

IMPORT Fingerprint, Fmt;

PROCEDURE OfChars(
    init: T; 
    READONLY t: ARRAY OF CHAR): T =
  BEGIN
    RETURN Fingerprint.FromChars(t, init)
  END OfChars;

PROCEDURE FromText(t: TEXT): T =
  BEGIN
    RETURN Fingerprint.FromText(t)
  END FromText;

PROCEDURE FromInt(i: INTEGER): T =
  BEGIN
    RETURN Fingerprint.FromChars( 
      LOOPHOLE(i, ARRAY [0..BYTESIZE(INTEGER)-1] OF CHAR), Zero)
  END FromInt;

PROCEDURE FromLongReal(r: LONGREAL): T =
  BEGIN
    RETURN Fingerprint.FromChars( 
      LOOPHOLE(r, ARRAY [0..BYTESIZE(LONGREAL)-1] OF CHAR), Zero)
  END FromLongReal;

PROCEDURE AddInt(fp: T; i: INTEGER): T =
  BEGIN 
    RETURN Fingerprint.FromChars(
      LOOPHOLE(i, ARRAY [0..BYTESIZE(INTEGER)-1] OF CHAR), fp)
  END AddInt;

PROCEDURE Combine(f, g: T): T = 
  BEGIN RETURN Fingerprint.Combine(f,g) 
  END Combine;

PROCEDURE Equal(fp1, fp2: T): BOOLEAN =
  BEGIN RETURN fp1 = fp2 END Equal;

PROCEDURE Hash(fp: T): INTEGER =
  BEGIN RETURN Fingerprint.Hash(fp) END Hash;

PROCEDURE Compare(f,g: T): [-1..1] =
  VAR i := 0; BEGIN
    WHILE i # 8 AND f.byte[i] = g.byte[i] DO INC(i) END;
    IF i = 8 THEN
      RETURN 0
    ELSIF f.byte[i] < g.byte[i] THEN
      RETURN -1
    ELSE
      RETURN 1
    END
  END Compare;

PROCEDURE ToText(fp: T): TEXT =
  VAR res := ""; BEGIN
    FOR i := 7 TO 0 BY -1 DO
      res := res & Fmt.Int(fp.byte[i], base := 16)
    END;
    RETURN res
  END ToText;

BEGIN
  OfEmpty := Fingerprint.OfEmpty
END FPrint.
