(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Feb 20 11:59:05 PST 1995 by kalsow     *)
(*      modified on Fri Apr  1 13:49:15 PST 1994 by harrison   *)

MODULE QVal;

IMPORT Fmt, Convert, Text;
IMPORT M3Buf, QIdent, QValue, QVTbl, QVSeq, QMachine, QCode;
FROM Quake IMPORT Error, Machine, ID;

TYPE
  QK = QValue.Kind;

PROCEDURE ToTag (m: Machine;  READONLY t: T): TEXT
  RAISES {Error} =
  VAR txt: TEXT;
  BEGIN
    CASE t.kind OF
    | QK.Var      => txt := "<variable " & m.map.id2txt (t.int) & ">";
    | QK.Integer  => txt := Fmt.Int (t.int);
    | QK.String   => txt := m.map.id2txt (t.int);
    | QK.Table    => txt := TableText (m, t.ref, TRUE);
    | QK.Array    => txt := ArrayText (m, t.ref, TRUE);
    | QK.Proc     => txt := ProcText (m, t.ref);
    END; (*CASE*)
    RETURN txt;
  END ToTag;

PROCEDURE ToBool (m: Machine;  READONLY t: T): BOOLEAN
  RAISES {Error} =
  BEGIN
    IF t.kind # QK.String THEN
      m.error ("attempting to use non-string value as a boolean: " & ToTag (m, t))
    END;
    RETURN (t.int # m.map.boolean [FALSE]);
  END ToBool;

PROCEDURE ToInt (m: Machine;  READONLY t: T): INTEGER
  RAISES {Error} =
  VAR
    txt: TEXT;
    used, val, len: INTEGER;
    buf: ARRAY [0..BITSIZE(INTEGER)-1] OF CHAR;
  BEGIN
    IF t.kind = QK.Integer THEN
      RETURN t.int;
    END;
    IF t.kind # QK.String THEN
      m.error ("cannot convert value to an integer: " & ToTag (m, t))
    END;
    txt := m.map.id2txt (t.int);
    len := Text.Length (txt);
    Text.SetChars (buf, txt);
    val := Convert.ToInt (SUBARRAY (buf, 0, len), used);
    IF (used # len) THEN
      m.error ("cannot convert value to an integer: " & ToTag (m, t))
    END;
    RETURN val;
  END ToInt;

PROCEDURE ToText (m: Machine;  READONLY t: T): TEXT
  RAISES {Error} =
  BEGIN
    CASE t.kind OF
    | QK.Integer => RETURN Fmt.Int (t.int);
    | QK.String  => RETURN m.map.id2txt (t.int);
    | QK.Array   => RETURN ArrayText (m, t.ref, FALSE);
    | QK.Table   => RETURN TableText (m, t.ref, FALSE);
    ELSE m.error ("cannot convert value to string: " & ToTag (m, t));  RETURN NIL;
    END;
  END ToText;

PROCEDURE ToID (m: Machine;  READONLY t: T): ID
  RAISES {Error} =
  BEGIN
    IF (t.kind = QK.String)
      THEN RETURN t.int;
      ELSE RETURN m.map.txt2id (ToText (m, t));
    END;
  END ToID;

PROCEDURE ToTable (m: Machine;  READONLY t: T): QVTbl.T
  RAISES {Error} =
  BEGIN
    IF t.kind # QK.Table THEN
      m.error ("cannot convert value to table: " & ToTag (m, t));
    END;
    RETURN t.ref;
  END ToTable;

PROCEDURE ToArray (m: Machine;  READONLY t: T): QVSeq.T
  RAISES {Error} =
  BEGIN
    IF t.kind # QK.Array THEN
      m.error ("cannot convert value to array: " & ToTag (m, t));
    END;
    RETURN t.ref;
  END ToArray;

PROCEDURE ToProc (m: Machine;  READONLY t: T): QValue.Proc
  RAISES {Error} =
  BEGIN
    IF t.kind # QK.Proc THEN
      m.error ("attempting to call a non-procedure value: " & ToTag (m, t));
    END;
    RETURN t.ref;
  END ToProc;

PROCEDURE ToBuf (m: Machine;  READONLY t: T;  buf: M3Buf.T)
  RAISES {Error} =
  BEGIN
    FillBuf (m, t, buf, FALSE);
  END ToBuf;

(*-------------------------------------------------------------- internal ---*)

PROCEDURE FillBuf (m: Machine;  READONLY t: T;  buf: M3Buf.T;  tag_only: BOOLEAN)
  RAISES {Error} =
  BEGIN
    CASE t.kind OF
    | QK.Integer => M3Buf.PutInt  (buf, t.int);
    | QK.String  => M3Buf.PutText (buf, m.map.id2txt (t.int));
    | QK.Array   => ArrayToBuf (m, t.ref, buf, tag_only);
    | QK.Table   => TableToBuf (m, t.ref, buf, tag_only);
    ELSE m.error ("cannot convert value to string: " & ToTag (m, t));
    END;
  END FillBuf;

PROCEDURE TableText (m: Machine;  tbl: QVTbl.T;  tag_only: BOOLEAN): TEXT
  RAISES {Error} =
  VAR buf := M3Buf.New ();
  BEGIN
    TableToBuf (m, tbl, buf, tag_only);
    RETURN M3Buf.ToText (buf);
  END TableText;

PROCEDURE TableToBuf (m: Machine;  tbl: QVTbl.T;  buf: M3Buf.T;  tag_only: BOOLEAN)
  RAISES {Error} =
  VAR
    iter  := tbl.iterate();
    key   : INTEGER;
    val   : T;
    first := TRUE;
  BEGIN
    IF tag_only THEN M3Buf.PutText (buf, "{ "); END;
    WHILE iter.next(key, val) DO
      IF tag_only THEN
        IF NOT first THEN M3Buf.PutText (buf, ", ..."); EXIT; END;
        M3Buf.PutText (buf, m.map.id2txt (key));
        M3Buf.PutText (buf, ": ");
      END;
      IF NOT first THEN M3Buf.PutChar (buf, ' '); END;
      FillBuf (m, val, buf, tag_only);
      first := FALSE;
    END;
    IF tag_only THEN M3Buf.PutText (buf, " }"); END;
  END TableToBuf;

PROCEDURE ArrayText (m: Machine;  arr: QVSeq.T;  tag_only: BOOLEAN): TEXT
  RAISES {Error} =
  VAR buf := M3Buf.New ();
  BEGIN
    ArrayToBuf (m, arr, buf, tag_only);
    RETURN M3Buf.ToText (buf);
  END ArrayText;

PROCEDURE ArrayToBuf (m: Machine;  arr: QVSeq.T;  buf: M3Buf.T;  tag_only: BOOLEAN)
  RAISES {Error} =
  BEGIN
    IF tag_only THEN M3Buf.PutText (buf, "[ "); END;
    FOR i := 0 TO arr.size() - 1 DO
      IF tag_only AND (i > 0) THEN M3Buf.PutText (buf, ", ..."); EXIT; END;
      IF i > 0 THEN M3Buf.PutChar (buf, ' '); END;
      FillBuf (m, arr.get(i), buf, tag_only);
    END;
    IF tag_only THEN M3Buf.PutText (buf, " ]"); END;
  END ArrayToBuf;

PROCEDURE ProcText (m: Machine;  proc: QValue.Proc): TEXT =
  BEGIN
    IF proc.info.builtin THEN
      RETURN "<builtin procedure "
               & m.map.id2txt (proc.info.name)
               & ">";
    ELSE
      RETURN "<procedure "
               & m.map.id2txt (proc.info.name)
               & " from "
               & m.map.id2txt (proc.info.code.source_file)
               & ">";
    END;
  END ProcText;

BEGIN
END QVal.
