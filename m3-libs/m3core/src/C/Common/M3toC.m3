(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 21 11:56:30 PDT 1994 by kalsow     *)
(*      modified on Fri May  7 21:05:40 PDT 1993 by mjordan    *)
(*      modified on Wed Mar 13 01:14:25 1991 by muller         *)
(*      modified on Tue Apr 24 16:40:16 1990 by jerome         *)

UNSAFE MODULE M3toC;

IMPORT Ctypes, Cstdlib, Cstring, TextF, RT0;

VAR
  zeroValue := 0;
  zeroPtr   := LOOPHOLE (ADR (zeroValue), Ctypes.char_star);

TYPE
  M3Text = UNTRACED REF RT0.TextHeader;

  M3TextWithHeader = UNTRACED REF RECORD
    header : RT0.RefHeader;
    body   : RT0.TextHeader;
  END;

PROCEDURE TtoS (t: TEXT): Ctypes.char_star =
  BEGIN
    IF (t = NIL) OR (NUMBER (t^) <= 1)
      THEN RETURN zeroPtr;
      ELSE RETURN LOOPHOLE (LOOPHOLE (t, M3Text).chars, Ctypes.char_star);
    END;
  END TtoS;

PROCEDURE CopyTtoS (t: TEXT): Ctypes.char_star =
  VAR len: INTEGER;
  BEGIN
    IF (t = NIL) THEN RETURN zeroPtr; END;
    len := NUMBER (t^);
    IF (len <= 1) THEN RETURN zeroPtr; END;
    RETURN Cstring.memcpy (Cstdlib.malloc (len), ADR (t[0]), len);
  END CopyTtoS;

PROCEDURE FreeCopiedS (s: Ctypes.char_star) =
  BEGIN
    IF (s # zeroPtr) THEN Cstdlib.free (s); END;
  END FreeCopiedS;

PROCEDURE StoT (s: Ctypes.char_star): TEXT =
  VAR t := NEW (M3TextWithHeader);
  BEGIN
    t.header.typecode := RT0.TextTypecode;
    t.body.chars      := LOOPHOLE (s, ADDRESS);
    t.body.length     := 1 + Cstring.strlen (s);
    RETURN LOOPHOLE (ADR (t.body), TEXT);
  END StoT;

PROCEDURE CopyStoT (s: Ctypes.char_star): TEXT =
  VAR len := Cstring.strlen (s) + 1;  t := NEW (TEXT, len);
  BEGIN
    EVAL Cstring.memcpy (ADR (t[0]), s, len);
    RETURN t;
  END CopyStoT;


BEGIN
END M3toC.
