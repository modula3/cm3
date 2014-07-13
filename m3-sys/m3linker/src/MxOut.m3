(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxOut.m3                                              *)
(* Last Modified On Tue Sep 27 09:35:31 PDT 1994 By kalsow     *)
(*      Modified On Wed May 26 15:59:13 PDT 1993 By muller     *)


MODULE MxOut;

IMPORT Wr, IntIntTbl;
IMPORT M3Buf, M3ID;
IMPORT Mx, MxVS, MxIO;

TYPE
  State = RECORD
    wr        : Wr.T;
    buf       : M3Buf.T     := NIL;
    nameMap   : IntIntTbl.T := NIL;
    next_name : INTEGER     := 0;
    vsMap     : IntIntTbl.T := NIL;
    next_vs   : INTEGER     := 0;
  END;

PROCEDURE WriteUnits (units: Mx.UnitList;  output: Wr.T) =
  VAR s: State;
  BEGIN
    IF (units = NIL) THEN RETURN END;
    IF (output = NIL) THEN RETURN END;

    s.wr        := output;
    s.buf       := M3Buf.New ();
    s.nameMap   := NEW (IntIntTbl.Default).init (409);
    s.next_name := 0;
    s.vsMap     := NEW (IntIntTbl.Default).init (709);
    s.next_vs   := 0;

    M3Buf.AttachDrain (s.buf, s.wr);

    IF Mx.UnicodeWideChar 
    THEN MxIO.PutTxt (s.buf,  Mx.LinkerMagicWCUni, Wr.EOL);
    ELSE MxIO.PutTxt (s.buf,  Mx.LinkerMagicWC16, Wr.EOL);
    END; 
    WHILE (units # NIL) DO
      WriteUnit (s, units.unit);
      units := units.next;
    END;

    M3Buf.Flush (s.buf, s.wr);

    (* give the collector a chance. *)
    s.wr      := NIL;
    s.buf     := NIL;
    s.nameMap := NIL;
    s.vsMap   := NIL;
  END WriteUnits;

PROCEDURE WriteUnit (VAR s: State;  u: Mx.Unit) =
  CONST Tag = ARRAY BOOLEAN OF TEXT {"M", "I"};
  VAR nm := WriteName (s, u.name);
  BEGIN
    MxIO.PutTxt (s.buf, Wr.EOL, Tag[u.interface]);
    MxIO.PutInt (s.buf, nm, " ");
    MxIO.PutInt (s.buf, u.exported_units.cnt, " ");
    MxIO.PutInt (s.buf, u.imported_units.cnt, " ");
    MxIO.PutInt (s.buf, u.imported_generics.cnt, " ");
    MxIO.PutInt (s.buf, u.used_interfaces.cnt, " ");
    MxIO.PutInt (s.buf, u.used_modules.cnt, " ");
    MxIO.PutInt (s.buf, u.import_def_syms.cnt, " ");
    MxIO.PutInt (s.buf, u.import_use_syms.cnt, " ");
    MxIO.PutInt (s.buf, u.export_def_syms.cnt, " ");
    MxIO.PutInt (s.buf, u.export_use_syms.cnt, " ");
    MxIO.PutInt (s.buf, u.imported_types.cnt, " ");
    MxIO.PutInt (s.buf, u.exported_types.cnt, " ");
    MxIO.PutInt (s.buf, u.wishes.cnt, Wr.EOL);

    WriteNameInfo    (s, u, u.exported_units,    "A");
    WriteNameInfo    (s, u, u.imported_units,    "B");
    WriteNameInfo    (s, u, u.used_interfaces,   "C");
    WriteNameInfo    (s, u, u.used_modules,      "D");
    WriteNameInfo    (s, u, u.imported_generics, "g");
    WriteVSInfo      (s, u, u.import_def_syms,   "J");
    WriteVSInfo      (s, u, u.import_use_syms,   "i");
    WriteVSInfo      (s, u, u.export_def_syms,   "E");
    WriteVSInfo      (s, u, u.export_use_syms,   "e");
    WriteTypeInfo    (s, u, u.imported_types,    "t");
    WriteTypeInfo    (s, u, u.exported_types,    "T");
    WriteTypeInfo    (s, u, u.wishes,            "w");
    WriteOpaques     (s, u.opaques);
    WriteObjects     (s, u.imported_objects, FALSE);
    WriteObjects     (s, u.exported_objects, TRUE);
    WriteRevelations (s, u.revelations);
  END WriteUnit;

PROCEDURE WriteNameInfo (VAR s: State;  u: Mx.Unit;
                         READONLY x: Mx.InfoList;  tag: TEXT) =
  VAR nm: INTEGER;
  BEGIN
    FOR i := x.start TO x.start + x.cnt - 1 DO
      nm := WriteName (s, u.info[i]);
      MxIO.PutTxt (s.buf,  tag);
      MxIO.PutInt (s.buf, nm, Wr.EOL);
    END;
  END WriteNameInfo;

PROCEDURE WriteVSInfo (VAR s: State;  u: Mx.Unit;
                         READONLY x: Mx.InfoList;  tag: TEXT) =
  VAR vs: INTEGER;
  BEGIN
    FOR i := x.start TO x.start + x.cnt - 1 DO
      vs := WriteVS (s, u.info[i]);
      MxIO.PutTxt (s.buf,  tag);
      MxIO.PutInt (s.buf, vs, Wr.EOL);
    END;
  END WriteVSInfo;

PROCEDURE WriteTypeInfo (VAR s: State;  u: Mx.Unit;
                         READONLY x: Mx.InfoList;  tag: TEXT) =
  BEGIN
    FOR i := x.start TO x.start + x.cnt - 1 DO
      MxIO.PutTxt (s.buf,  tag);
      MxIO.PutHex (s.buf,  u.info[i], Wr.EOL);
    END;
  END WriteTypeInfo;

PROCEDURE WriteOpaques (VAR s: State;  o: Mx.OpaqueType) =
  VAR name: INTEGER; 
  BEGIN
    WHILE (o # NIL) DO
      IF Mx.UnicodeWideChar AND o.TypeName # M3ID.NoID THEN 
         (* ^Don't write "q" line unless writing >= v4.3 of mx file. *) 
        name := WriteName(s, o.TypeName); 
        MxIO.PutTxt (s.buf,  "q");
        MxIO.PutHex (s.buf,  o.type, " ");
        MxIO.PutHex (s.buf,  o.super_type, " ");
        MxIO.PutInt (s.buf, name, Wr.EOL);
      ELSE
        MxIO.PutTxt (s.buf,  "Q");
        MxIO.PutHex (s.buf,  o.type, " ");
        MxIO.PutHex (s.buf,  o.super_type, Wr.EOL);
      END; 
      o := o.next;
    END;
  END WriteOpaques;

PROCEDURE WriteObjects (VAR s: State;  obj: Mx.ObjectType;  export: BOOLEAN) =
  VAR nm: INTEGER;
  BEGIN
    WHILE (obj # NIL) DO
      IF (export) THEN
        MxIO.PutTxt (s.buf,  "O");
      ELSIF (obj.from_module) THEN
        nm := WriteName (s, obj.source);
        MxIO.PutTxt (s.buf,  "p");
        MxIO.PutInt (s.buf, nm, " ");
      ELSE (* import from interface *)
        nm := WriteName (s, obj.source);
        MxIO.PutTxt (s.buf,  "o");
        MxIO.PutInt (s.buf, nm, " ");
      END;
      MxIO.PutHex (s.buf, obj.type, " ");
      MxIO.PutHex (s.buf, obj.super_type, " ");
      MxIO.PutInt (s.buf, obj.data_size, " ");
      MxIO.PutInt (s.buf, obj.data_align, " ");
      MxIO.PutInt (s.buf, obj.method_size, Wr.EOL);
      obj := obj.next;
    END;
  END WriteObjects;

PROCEDURE WriteRevelations (VAR s: State;  r: Mx.Revelation) =
  CONST import_tag = ARRAY BOOLEAN OF TEXT { "r", "x" };
  CONST export_tag = ARRAY BOOLEAN OF TEXT { "R", "X" };
  VAR tag: TEXT;  nm: INTEGER;
  BEGIN
    WHILE (r # NIL) DO
      IF (r.export)
        THEN tag := export_tag [r.partial];
        ELSE tag := import_tag [r.partial];
      END;
      nm := WriteName (s, r.source);
      MxIO.PutTxt (s.buf,  tag);
      MxIO.PutInt (s.buf, nm, " ");
      MxIO.PutHex (s.buf, r.lhs, " ");
      MxIO.PutHex (s.buf, r.rhs, Wr.EOL);
      r := r.next;
    END;
  END WriteRevelations;

PROCEDURE WriteVS (VAR s: State;  vs: MxVS.T): INTEGER =
  VAR id, src, sym: INTEGER;  info: MxVS.Info;
  BEGIN
    IF NOT s.vsMap.get (vs, id) THEN
      MxVS.Get (vs, info);
      src := WriteName (s, info.source);
      sym := WriteName (s, info.symbol);
      id := s.next_vs;  INC(s.next_vs);
      EVAL s.vsMap.put (vs, id);
      MxIO.PutTxt (s.buf,  "V");
      MxIO.PutInt (s.buf, id, " ");
      MxIO.PutInt (s.buf, src, " ");
      MxIO.PutInt (s.buf, sym, " ");
      MxIO.PutFP  (s.buf, info.stamp, Wr.EOL);
    END;
    RETURN id;
  END WriteVS;

PROCEDURE WriteName (VAR s: State;  nm: Mx.Name): INTEGER =
  VAR id: INTEGER;
  BEGIN
    IF NOT s.nameMap.get (nm, id) THEN
      id := s.next_name;  INC (s.next_name);
      EVAL s.nameMap.put (nm, id);
      MxIO.PutTxt (s.buf, "N");
      MxIO.PutInt (s.buf, id, " ");
      M3ID.Put    (s.buf, nm);
      MxIO.PutTxt (s.buf, Wr.EOL);
    END;
    RETURN id;
  END WriteName;

BEGIN
END MxOut.
