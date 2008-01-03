(* Copyright (C) 1994, Digital Equipment Corporation.         *)
(* All rights reserved.                                       *)
(* See the file COPYRIGHT for a full description.             *)
(*                                                            *)
(* Last modified on Fri Nov  4 10:33:37 PST 1994 by kalsow    *)

MODULE SilError;

IMPORT AtomList, Atom, Process;

PROCEDURE Put (info: AtomList.T;  a, b, c: TEXT := NIL) =
  VAR msg := "";  d := OSErr (info);
  BEGIN
    IF (a # NIL) THEN msg := msg & a END;
    IF (b # NIL) THEN msg := msg & b END;
    IF (c # NIL) THEN msg := msg & c END;
    IF (d # NIL) THEN msg := msg & d END;
    Process.Crash (msg);
  END Put;

PROCEDURE OSErr (args: AtomList.T): TEXT =
  VAR msg : TEXT := NIL;
  BEGIN
    WHILE (args # NIL) DO
      IF (msg = NIL) THEN  msg := ": ";  ELSE  msg := msg & "  ***  ";  END;
      msg  := msg & Atom.ToText (args.head);
      args := args.tail;
    END;
    RETURN msg;
  END OSErr;

BEGIN
END SilError.


