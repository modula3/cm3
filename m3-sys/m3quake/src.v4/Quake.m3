(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Feb 22 13:08:06 PST 1995 by kalsow     *)

MODULE Quake;

IMPORT Thread, QIdent, QMachine, QValue, QVal, QCompiler;

PROCEDURE NewMachine (map: IDMap): Machine =
  BEGIN
    RETURN NEW (QMachine.T).init (map);
  END NewMachine;

PROCEDURE Run (m: Machine;  source_file: TEXT)
  RAISES {Error, Thread.Alerted} =
  BEGIN
    m.evaluate (QCompiler.CompileFile (source_file, m.map));
  END Run;

PROCEDURE Define (m: Machine;  symbol, value: TEXT) RAISES {Error} =
  VAR v: QValue.T;
  BEGIN
    v.kind := QValue.Kind.String;
    v.int  := m.map.txt2id (value);
    v.ref  := NIL;
    m.put (m.map.txt2id (symbol), v);
  END Define;

PROCEDURE LookUp (m: Machine;  symbol: TEXT): TEXT RAISES {Error} =
  VAR b := m.lookup (m.map.txt2id (symbol));
  BEGIN
    IF (b = NIL)
      THEN RETURN NIL;
      ELSE RETURN QVal.ToText (m, b.value);
    END;
  END LookUp;

PROCEDURE Done (m: Machine) RAISES {Error} =
  BEGIN
    m.cleanup ();
  END Done;

BEGIN
END Quake.
