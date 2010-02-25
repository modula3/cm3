(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: M3BackInt.m3                                          *)
(* Last Modified On Tue Jul 12 08:31:56 PDT 1994 By kalsow     *)
(*      Modified On Thu May 20 08:46:32 PDT 1993 By muller     *)

MODULE M3BackInt; (* also known as TInt *)

IMPORT  Text, Fmt, TInt;

PROCEDURE SignedTruncate(VAR a: Int; n: CARDINAL): BOOLEAN =
  BEGIN
    TInt.Chop(a, n);
    RETURN TRUE;
  END SignedTruncate;

PROCEDURE FromInt (x: INTEGER;  n: CARDINAL;  VAR r: Int): BOOLEAN =
  BEGIN
    RETURN TInt.FromInt(x, r) AND SignedTruncate(r, n);
  END FromInt;

PROCEDURE ToInt (READONLY r: Int;  VAR x: INTEGER): BOOLEAN =
  VAR r4 := r;
  BEGIN
    TInt.Chop(r4, 4);
    RETURN TInt.ToInt(r4, x);
  END ToInt;

PROCEDURE FromTargetInt (READONLY i: Int; byteSize: CARDINAL): Int =
  VAR j: Int;
  BEGIN
    j := i;
    TInt.Chop(j, byteSize);
    RETURN j;
  END FromTargetInt;

PROCEDURE ToDiagnosticText(a: Int): TEXT =
  VAR t := "";
  BEGIN
    FOR i := FIRST(a) TO LAST(a) DO
      t := t & Fmt.Unsigned(a[i]);
      IF i # LAST(a) THEN
        t := t & ",";
      END;
    END;
    RETURN t;
  END ToDiagnosticText;

BEGIN
END M3BackInt.
