(* Copyright (C) 1994, Digital Equipment Corporation. *)
(* All rights reserved.                               *)
(* See the file COPYRIGHT for a full description.     *)
 
MODULE Main;
IMPORT Wr, Stdio, Fmt, RTTypeFP, Fingerprint, Test, Text;

PROCEDURE Put (a, b, c: TEXT := NIL) =
  <*FATAL ANY*>
  BEGIN
    IF (a # NIL) THEN Wr.PutText (Stdio.stderr, a); END;
    IF (b # NIL) THEN Wr.PutText (Stdio.stderr, b); END;
    IF (c # NIL) THEN Wr.PutText (Stdio.stderr, c); END;
    Wr.PutChar (Stdio.stderr, '\n');
  END Put;

PROCEDURE FmtFP (READONLY fp: Fingerprint.T): TEXT =
  CONST Digits = ARRAY [0..15] OF CHAR { '0','1','2','3','4','5','6','7',
                                         '8','9','a','b','c','d','e','f'};
  VAR buf: ARRAY [0..31] OF CHAR;  len := 0;  x: [0..255];
  BEGIN
    FOR i := FIRST (fp.byte) TO LAST (fp.byte) DO
      x := fp.byte[i];
      buf [len] := Digits [x DIV 16];  INC (len);
      buf [len] := Digits [x MOD 16];  INC (len);
    END;
    RETURN Text.FromChars (SUBARRAY (buf, 0, len));
  END FmtFP;

VAR
  tc0, tc1: INTEGER;
  fp: Fingerprint.T;
BEGIN
  tc0 := TYPECODE (NULL);
  Put ("TYPECODE (NULL) = ", Fmt.Int (tc0));

  fp := RTTypeFP.ToFingerprint (tc0);
  Put ("FP (NULL)       = ", FmtFP (fp));

  tc1 := RTTypeFP.FromFingerprint (fp);
  Put ("TC (FP)         = ", Fmt.Int (tc1));

  Test.checkI (tc0, tc1);
  Test.done ();
END Main.
