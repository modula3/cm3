(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Tue Jun 21 11:56:30 PDT 1994 by kalsow     *)
(*      modified on Fri May  7 21:05:40 PDT 1993 by mjordan    *)
(*      modified on Wed Mar 13 01:14:25 1991 by muller         *)
(*      modified on Tue Apr 24 16:40:16 1990 by jerome         *)

UNSAFE MODULE M3toC;

IMPORT Ctypes, Cstdlib, Cstring;
IMPORT Text, TextClass, Text8, Text8CString;
IMPORT Scheduler;

VAR
  zeroValue := 0;
  zeroPtr   := LOOPHOLE (ADR (zeroValue), Ctypes.char_star);

TYPE
  CharPtr  = UNTRACED REF CHAR;
  ArrayPtr = UNTRACED REF ARRAY OF CHAR;
  OpenArray = RECORD
    start  : ADDRESS;
    length : INTEGER;
  END;

PROCEDURE CopyTtoS (t: TEXT): Ctypes.char_star =
  VAR info: TextClass.Info;  arr: OpenArray;
  BEGIN
    IF (t = NIL) THEN RETURN zeroPtr; END;
    t.get_info (info);
    Scheduler.DisableSwitching ();
    arr.start  := Cstdlib.malloc (info.length + 1);
    Scheduler.EnableSwitching ();
    arr.length := info.length;
    Text.SetChars (LOOPHOLE (ADR (arr), ArrayPtr)^, t, 0);
    LOOPHOLE (arr.start + info.length, CharPtr)^ := '\000';
    RETURN arr.start;
  END CopyTtoS;

PROCEDURE FreeCopiedS (s: Ctypes.const_char_star) =
  BEGIN
    IF (s # NIL) AND (s # zeroPtr) THEN
      Cstdlib.free (s);
    END;
  END FreeCopiedS;

PROCEDURE SharedTtoS (t: TEXT): Ctypes.const_char_star =
  VAR info: TextClass.Info;
  BEGIN
    IF (t = NIL) THEN RETURN zeroPtr; END;
    t.get_info (info);
    IF info.start # NIL AND NOT info.wide THEN
      (* make sure the thing is null terminated! *)
      IF LOOPHOLE (info.start + info.length, Ctypes.char_star)^ = 0 THEN
        RETURN info.start;
      END;
    END;
    RETURN CopyTtoS (t);
  END SharedTtoS;

PROCEDURE FreeSharedS (t: TEXT;  s: Ctypes.const_char_star) =
  VAR info: TextClass.Info;
  BEGIN
    IF (s # NIL) AND (s # zeroPtr) THEN
      t.get_info (info);
      IF (info.start # s) THEN
        Cstdlib.free (s);
      END;
    END;
  END FreeSharedS;

PROCEDURE FlatTtoS (t: TEXT): Ctypes.const_char_star =
  VAR info: TextClass.Info;
  BEGIN
    IF (t = NIL) THEN RETURN zeroPtr; END;
    t.get_info (info);
    IF info.start # NIL AND NOT info.wide THEN
      (* make sure the thing is null terminated! *)
      IF LOOPHOLE (info.start + info.length, Ctypes.char_star)^ = 0 THEN
        RETURN info.start;
      END;
    END;
    (* force a runtime fault *)
    VAR i: CARDINAL; BEGIN i := -1; <*NOWARN*> END;
    RETURN NIL;
  END FlatTtoS;

PROCEDURE StoT (s: Ctypes.const_char_star): TEXT =
  BEGIN
    RETURN Text8CString.New (s);
  END StoT;

PROCEDURE CopyStoT (s: Ctypes.const_char_star): TEXT =
  VAR len := Cstring.strlen (s);  t := Text8.Create (len);
  BEGIN
    EVAL Cstring.memcpy (ADR (t.contents[0]), s, len);
    RETURN t;
  END CopyStoT;


BEGIN
END M3toC.
