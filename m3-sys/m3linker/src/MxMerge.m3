(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxMerge.m3                                            *)
(* Last Modified On Thu Jan 26 14:37:15 PST 1995 By kalsow     *)

MODULE MxMerge;

IMPORT Text, Wr, Fmt;
IMPORT M3ID, M3FP, M3Buf;
IMPORT Mx, MxRep, MxMap, MxVS, MxSet, MxVSSet, MxIO;

CONST
  Margin = 72;

TYPE
  State = RECORD
    unit    : Mx.Unit;
    base    : Mx.LinkSet;
    errs    : Wr.T;
    err_buf : M3Buf.T;
    failed  : BOOLEAN;
  END;

(*------------------------------------------------------------------------*)


PROCEDURE MergeUnit (unit   : Mx.Unit;
                     base   : Mx.LinkSet;
                     errors : Wr.T): Mx.UnitList =
  VAR s: State;  v: Mx.Unit;  map: MxMap.T;  cl: MxSet.T;
  BEGIN
    IF (unit = NIL) THEN RETURN NIL END;
    <*ASSERT base # NIL*>

    s.unit   := unit;
    s.base   := base;
    s.errs   := errors;
    s.failed := FALSE;

    (* check for an existing unit *)
    IF (unit.interface)
      THEN map := s.base.interfaces;
      ELSE map := s.base.modules;
    END;
    v := MxMap.Get (map, unit.name);
    IF (v # NIL) THEN
      RETURN Flush (s, DuplicateUnit (s, unit, v));
    END;

    (* check the existing symbols with the new ones *)
    CheckUnit (s, unit);
    IF (s.failed) THEN
      RETURN Flush (s, NEW (Mx.UnitList, next := NIL, unit := unit));
    END;

    (* add the new unit *)
    MxMap.Insert (map, unit.name, unit);

    (* and merge its symbols *)
    AddStamps (s, unit.export_def_syms, TRUE, TRUE);
    AddStamps (s, unit.export_use_syms, TRUE, FALSE);
    AddStamps (s, unit.import_def_syms, FALSE, TRUE);
    AddStamps (s, unit.import_use_syms, FALSE, FALSE);
    AddObjects (s, unit.imported_objects);
    AddRevelations (s, unit.revelations);
    AddExportedTypes (s, unit.exported_types);

    (* delete any existing virtual unit *)
    MxMap.Delete (s.base.virtuals, unit.name);

    (* and recheck any clients of the old virtual unit *)
    cl := MxMap.Get (s.base.clients, unit.name);
    IF (cl # NIL) THEN
      MxMap.Delete (s.base.clients, unit.name);
      RETURN Flush (s, FilterClients (s, MxSet.ToList (cl)));
    END;

    RETURN Flush (s, NIL);
  END MergeUnit;

PROCEDURE Flush (VAR s: State;  ux: Mx.UnitList): Mx.UnitList =
  BEGIN
    IF (s.errs # NIL) AND (s.err_buf # NIL) THEN
      M3Buf.Flush (s.err_buf, s.errs);
    END;
    RETURN ux;
  END Flush;

(*------------------------------------------------------------------------*)

PROCEDURE CheckUnit (VAR s: State;  u: Mx.Unit) =
  BEGIN
    CheckStamps (s, u, u.export_def_syms, TRUE, TRUE);
    IF (s.failed) AND (s.errs = NIL) THEN RETURN END;
    CheckStamps (s, u, u.export_use_syms, TRUE, FALSE);
    IF (s.failed) AND (s.errs = NIL) THEN RETURN END;
    CheckStamps (s, u, u.import_def_syms, FALSE, TRUE);
    IF (s.failed) AND (s.errs = NIL) THEN RETURN END;
    CheckStamps (s, u, u.import_use_syms, FALSE, FALSE);
    IF (s.failed) AND (s.errs = NIL) THEN RETURN END;

    CheckImportedTypes (s, u.imported_types);
    IF (s.failed) AND (s.errs = NIL) THEN RETURN END;
    CheckObjects (s, u.exported_objects);
    IF (s.failed) AND (s.errs = NIL) THEN RETURN END;
    CheckObjects (s, u.imported_objects);
    IF (s.failed) AND (s.errs = NIL) THEN RETURN END;
    CheckRevelations (s, u.revelations);
  END CheckUnit;
 
(*------------------------------------------------------------------------*)

PROCEDURE DuplicateUnit (VAR s: State;  u, v: Mx.Unit): Mx.UnitList =
  VAR errs := NEW (Mx.UnitList, next := NIL, unit := u);
  BEGIN
    s.failed := TRUE;
    IF (s.errs = NIL) THEN RETURN errs; END;
    Out (s, "duplicate ", MxRep.UnitName (u), ":", Wr.EOL);
    Out (s, "  in ", u.file.name, Wr.EOL);
    Out (s, " and ", v.file.name, Wr.EOL);
    RETURN errs;
  END DuplicateUnit;

(*------------------------------------------------------------------------*)

PROCEDURE CheckStamps (VAR s: State;  u: Mx.Unit;
                       READONLY z: Mx.InfoList;
                       export, implement: BOOLEAN)=
  VAR uu: Mx.Unit;  vs, x: MxVS.T;
  BEGIN
    FOR i := z.start TO z.start + z.cnt - 1 DO
      vs := u.info[i];
      MxRep.GetStamp (s.base, s.base.vs_exports, vs, uu, x);
      IF (uu = NIL) AND (x = MxVS.NoVS) THEN
        (* ok, we'll add a virtual unit later *)
      ELSIF (x = MxVS.NoVS) THEN
        (* we don't know about this stamp yet *)
        IF (NOT uu.virtual) THEN
          (* it's too late to add another stamp *)
          IF MissingStamp (s, vs) THEN EXIT; END;
        END;
      ELSIF (x # vs) THEN
        IF BadStamps (s, uu, x, vs) THEN EXIT END;
      ELSIF (export) THEN
        (* this is the primary declaration *)
        IF NOT uu.virtual THEN
          IF DuplicateStamp (s, uu, x) THEN EXIT END;
        END;
      END;

      IF (implement) THEN
        (* check for a duplicate implementation *)
        MxRep.GetStamp (s.base, s.base.vs_impls, vs, uu, x);
        IF (x # MxVS.NoVS) THEN
          IF DuplicateStampImpl (s, uu, x) THEN EXIT END;
        END;
      END;
    END;
  END CheckStamps;

PROCEDURE ReCheckStamps (VAR s: State;  client: Mx.Unit;
                         READONLY z: Mx.InfoList)=
  VAR u: Mx.Unit;  vs, x: MxVS.T;  key := s.unit.name;  info: MxVS.Info;
  BEGIN
    FOR i := z.start TO z.start + z.cnt - 1 DO
      vs := client.info[i];
      MxVS.Get (vs, info);
      IF (info.source = key) THEN
        MxRep.GetStamp (s.base, s.base.vs_exports, vs, u, x);
        IF (u = NIL) OR (x = MxVS.NoVS) THEN
          (* we don't have a primary definition for this symbol *)
          IF (s.unit.interface) THEN
            (* it's too late to add a virtual interface... *)
            IF MissingStamp (s, vs) THEN EXIT; END;
          ELSE
            (* re-install the virtual unit & stamp *)
            AddVirtualExport (s, vs, client);
          END;
        END;
      END;
    END;
  END ReCheckStamps;

(*------------------------------------------------------------------------*)

PROCEDURE AddStamps (VAR s: State;  READONLY z: Mx.InfoList;
                     export, implement: BOOLEAN) =
  VAR x, vs: MxVS.T;
  BEGIN
    FOR i := z.start TO z.start + z.cnt - 1 DO
      vs := s.unit.info [i];
      IF (export) THEN
        (* this is the primary definition *)
        MxVSSet.Insert (s.base.vs_exports, vs);
      ELSE
        x := MxVSSet.Get (s.base.vs_exports, vs);
        IF (x = MxVS.NoVS) THEN
          (* this is the first reference to the stamp *)
          AddVirtualExport (s, vs, s.unit);
        END;
      END;
      IF (implement) THEN
        (* this is the primary implmentation *)
        MxVSSet.Insert (s.base.vs_impls, vs);
      END;
    END;
  END AddStamps;

PROCEDURE AddVirtualExport (VAR s: State;  vs: MxVS.T;  client: Mx.Unit) =
  VAR u: Mx.Unit;  info: MxVS.Info;
  BEGIN
    MxVS.Get (vs, info);
    u := MxRep.GetVirtualUnit (s.base, info.source, client);
    (* MxRep.AddVirtualInfo (u, u.export_use_syms, vs); *)
    MxVSSet.Insert (s.base.vs_exports, vs);
  END AddVirtualExport;

(*------------------------------------------------------------------------*)

PROCEDURE MissingStamp (VAR s: State;  vs: MxVS.T): BOOLEAN =
  VAR info: MxVS.Info;
  BEGIN
    s.failed := TRUE;
    IF (s.errs = NIL) THEN RETURN TRUE; END;
    MxVS.Get (vs, info);
    Out (s, "missing export: ", M3ID.ToText (info.source));
    Out (s, ".", M3ID.ToText (info.symbol), " imported by ");
    Out (s, MxRep.UnitName (s.unit), Wr.EOL);
    RETURN FALSE;
  END MissingStamp;

(*------------------------------------------------------------------------*)

PROCEDURE DuplicateStamp (VAR s: State;  u: Mx.Unit;  vs: MxVS.T): BOOLEAN =
  VAR info: MxVS.Info;
  BEGIN
    s.failed := TRUE;
    IF (s.errs = NIL) THEN RETURN TRUE; END;
    MxVS.Get (vs, info);
    Out (s, "multiple definitions: ", M3ID.ToText (info.source));
    Out (s, ".", M3ID.ToText (info.symbol), " implemented by:", Wr.EOL);
    Out (s, "   ", MxRep.UnitName (u), Wr.EOL);
    Out (s, "   ", MxRep.UnitName (s.unit), Wr.EOL);
    RETURN FALSE;
  END DuplicateStamp;

(*------------------------------------------------------------------------*)

PROCEDURE DuplicateStampImpl (VAR s: State;  u: Mx.Unit;  vs: MxVS.T): BOOLEAN =
  VAR info: MxVS.Info;
  BEGIN
    s.failed := TRUE;
    IF (s.errs = NIL) THEN RETURN TRUE; END;
    MxVS.Get (vs, info);
    Out (s, "multiple implementations: ", M3ID.ToText (info.source));
    Out (s, ".", M3ID.ToText (info.symbol), " implemented by:", Wr.EOL);
    Out (s, "   ", MxRep.UnitName (u), Wr.EOL);
    Out (s, "   ", MxRep.UnitName (s.unit), Wr.EOL);
    RETURN FALSE;
  END DuplicateStampImpl;

(*------------------------------------------------------------------------*)

PROCEDURE BadStamps (VAR s: State;
                         u: Mx.Unit;
                         vs1, vs2: MxVS.T): BOOLEAN =
  VAR width := 20;  ui: Mx.UnitList;  cl: MxSet.T;  info: MxVS.Info;

  PROCEDURE PrintUnit (x: Mx.Unit) =
    VAR name := MxRep.UnitName (x);  len := Text.Length (name);
    BEGIN
      IF (width + len > Margin) THEN Out (s, Wr.EOL, "     ");  width := 5 END;
      Out (s, name, "  "); INC (width, len + 2);
    END PrintUnit;

  PROCEDURE CheckUnitStamps (x: Mx.Unit;  READONLY z: Mx.InfoList;
                             vs: MxVS.T): BOOLEAN =
    BEGIN
      FOR i := z.start TO z.start + z.cnt - 1 DO
        IF (x.info[i] = vs) THEN
          PrintUnit (x);
          RETURN TRUE;
        END;
      END;
      RETURN FALSE;
    END CheckUnitStamps;

  BEGIN
    s.failed := TRUE;
    IF (s.errs = NIL) THEN RETURN TRUE; END;

    MxVS.Get (vs2, info);
    Out  (s, "version stamp mismatch: ", M3ID.ToText (info.source));
    Out  (s, ".", M3ID.ToText (info.symbol), Wr.EOL);
    OutX (s, info.stamp);
    MxVS.Get (vs1, info);
    Out  (s, " => ", MxRep.UnitName (s.unit), Wr.EOL);
    OutX (s, info.stamp);
    Out  (s, " => ");

    IF (NOT u.virtual) THEN PrintUnit (u) END;

    cl := MxMap.Get (s.base.clients, info.source);
    ui := MxSet.ToList (cl);
    WHILE (ui # NIL) DO
      u := ui.unit;
      IF   CheckUnitStamps (u, u.export_def_syms, vs2)
        OR CheckUnitStamps (u, u.export_use_syms, vs2)
        OR CheckUnitStamps (u, u.import_def_syms, vs2)
        OR CheckUnitStamps (u, u.import_use_syms, vs2) THEN
      END;
      ui := ui.next;
    END;
    IF (width > 0) THEN Out (s, Wr.EOL) END;

    RETURN FALSE;
  END BadStamps;

(*------------------------------------------------------------------------*)

PROCEDURE CheckRevelations (VAR s: State;  r: Mx.Revelation) =
  VAR u: Mx.Unit;  x: Mx.Revelation;
  BEGIN
    WHILE (r # NIL) DO
      IF (NOT r.export) THEN
        MxRep.GetExportedRevelation (s.base, r, u, x);
        IF (u = NIL) THEN
          (* Ok, we'll add a virtual unit later *)
        ELSIF (x = NIL) THEN
          (* we don't know about this revelation yet *)
          IF (NOT u.virtual) THEN
            (* it's too late to add another revelation *)
            IF MissingRevelation (s, r) THEN EXIT; END;
          END;
        END;
      END;
      r := r.next;
    END;
  END CheckRevelations;

PROCEDURE ReCheckRevelations (VAR s: State;  r: Mx.Revelation;
                              client: Mx.Unit) =
  VAR u: Mx.Unit;  x: Mx.Revelation;  key := s.unit.name;
  BEGIN
    WHILE (r # NIL) DO
      IF (r.source = key) AND (NOT r.export) THEN
        MxRep.GetExportedRevelation (s.base, r, u, x);
        IF (x = NIL) OR (NOT x.export) THEN
          (* we don't have a definitive revelation *)
          IF (s.unit.interface) THEN
            (* it's too late to add a virtual interface... *)
            IF MissingRevelation (s, r) THEN EXIT; END;
          ELSE
            (* make sure somebody "exports" this revelation *)
            u := MxRep.GetVirtualUnit (s.base, r.source, client);
            x := NEW (Mx.Revelation);  x^ := r^;
            x.next := u.revelations;  u.revelations := x;
            x.export := TRUE;
          END;
        END;
      END;
      r := r.next;
    END;
  END ReCheckRevelations;

PROCEDURE MissingRevelation (VAR s: State;  r: Mx.Revelation): BOOLEAN =
  CONST op = ARRAY BOOLEAN OF TEXT { "= ", "<: " };
  BEGIN
    s.failed := TRUE;
    IF (s.errs = NIL) THEN RETURN TRUE; END;
    Out  (s, MxRep.UnitName (s.unit), ": missing imported revelation: ");
    OutT (s, r.lhs);
    Out  (s, op[r.partial]);
    OutT (s, r.rhs);
    Out  (s, "from ", M3ID.ToText (r.source), ".i3", Wr.EOL);
    RETURN FALSE;
  END MissingRevelation;

(*------------------------------------------------------------------------*)

PROCEDURE AddRevelations (VAR s: State;  r: Mx.Revelation) =
  VAR u: Mx.Unit;  x: Mx.Revelation;
  BEGIN
    WHILE (r # NIL) DO
      IF (NOT r.export) THEN
        MxRep.GetExportedRevelation (s.base, r, u, x);
        IF (x = NIL) THEN
          (* make sure somebody "exports" this revelation *)
          u := MxRep.GetVirtualUnit (s.base, r.source, s.unit);
          x := NEW (Mx.Revelation);  x^ := r^;
          x.next := u.revelations;  u.revelations := x;
          x.export := TRUE;
        END;
      END;
      r := r.next;
    END;
  END AddRevelations;

(*---------------------------------------------------------------------------*)

PROCEDURE CheckImportedTypes (VAR s: State;  READONLY z: Mx.InfoList) =
  VAR t: INTEGER;
  BEGIN
    FOR i := z.start TO z.start + z.cnt - 1 DO
      t := s.unit.info [i];
      IF MxMap.Get (s.base.exported_types, t) = NIL THEN
        s.failed := TRUE;
        IF (s.errs = NIL) THEN RETURN; END;
        Out  (s, MxRep.UnitName (s.unit), ": missing imported type: ");
        OutT (s, t);
        Out  (s, Wr.EOL);
      END;
    END;
  END CheckImportedTypes;

PROCEDURE AddExportedTypes (VAR s: State;  READONLY z: Mx.InfoList) =
  VAR t: INTEGER;
  BEGIN
    FOR i := z.start TO z.start + z.cnt - 1 DO
      t := s.unit.info [i];
      MxMap.Insert (s.base.exported_types, t, s.unit);
    END;
  END AddExportedTypes;

(*------------------------------------------------------------------------*)

PROCEDURE CheckObjects (VAR s: State;  o: Mx.ObjectType) =
  VAR u: Mx.Unit;  x: Mx.ObjectType;
  BEGIN
    WHILE (o # NIL) DO
      IF (NOT o.export) THEN
        MxRep.GetExportedObject (s.base, o, u, x);
        IF (u = NIL) THEN
          (* ok, we'll create a virtual unit later *)
        ELSIF (x = NIL) THEN
          (* we don't know about this object type yet *)
          IF (NOT u.virtual) THEN
            (* it's too late to add this object type *)
            IF MissingObject (s, o) THEN EXIT; END;
          END;
        ELSIF (x.super_type # o.super_type)
           OR (x.data_size # o.data_size)
           OR (x.data_align # o.data_align)
           OR (x.method_size # o.method_size)
           OR (x.from_module # o.from_module) THEN
          IF BadObject (s, u, x, o) THEN EXIT; END;
        END;
      END;
      o := o.next;
    END;
  END CheckObjects;

PROCEDURE ReCheckObjects (VAR s: State;  o: Mx.ObjectType;  client: Mx.Unit) =
  VAR u: Mx.Unit;  x: Mx.ObjectType;  key := s.unit.name;
  BEGIN
    WHILE (o # NIL) DO
      IF (o.source = key) AND (NOT o.export) THEN
        MxRep.GetExportedObject (s.base, o, u, x);
        IF (x = NIL) OR (NOT x.export) THEN
          (* we don't know about this object type yet *)
          IF (u # NIL) AND (NOT u.virtual)
            AND (u.interface # o.from_module) THEN
            (* it's too late to add it *)
            IF MissingObject (s, o) THEN EXIT; END;
          ELSE
            (* make sure somebody "exports" this object type *)
            u := MxRep.GetVirtualUnit (s.base, o.source, client);
            x := NEW (Mx.ObjectType);  x^ := o^;
            x.next := u.exported_objects;  u.exported_objects := x;
            x.export := TRUE;
          END;
        END;
      END;
      o := o.next;
    END;
  END ReCheckObjects;

(*------------------------------------------------------------------------*)

PROCEDURE MissingObject (VAR s: State;  o: Mx.ObjectType): BOOLEAN =
  CONST Tag = ARRAY BOOLEAN OF TEXT { ".i3", ".m3" };
  BEGIN
    s.failed := TRUE;
    IF (s.errs = NIL) THEN RETURN TRUE; END;

    Out  (s,  MxRep.UnitName (s.unit), ": ");
    Out  (s, "missing opaque object info for type ");
    OutT (s, o.type);
    Out  (s, "from ", M3ID.ToText (o.source), Tag[o.from_module]);
    Out  (s, Wr.EOL);

    RETURN FALSE;
  END MissingObject;

(*------------------------------------------------------------------------*)

PROCEDURE BadObject (VAR s: State;  u: Mx.Unit;  o1, o2: Mx.ObjectType): BOOLEAN =
  VAR width := 4;  ui: Mx.UnitList;  cl: MxSet.T;

  PROCEDURE PrintUnit (x: Mx.Unit) =
    VAR name := MxRep.UnitName (x);  len := Text.Length (name);
    BEGIN
      IF (width + len > Margin) THEN Out (s, Wr.EOL, "     ");  width := 5 END;
      Out (s, name, "  "); INC (width, len + 2);
    END PrintUnit;

  PROCEDURE CheckUnitObjs (x: Mx.Unit;  o1, o2: Mx.ObjectType): BOOLEAN =
    BEGIN
      WHILE (o1 # NIL) DO
        IF (o1.source = o2.source)
          AND (o1.type = o2.type)
          AND (o1.from_module = o2.from_module) THEN
          PrintUnit (x);
          RETURN TRUE;
        END;
        o1 := o1.next;
      END;
      RETURN FALSE;
    END CheckUnitObjs;

  BEGIN
    s.failed := TRUE;
    IF (s.errs = NIL) THEN RETURN TRUE; END;

    Out  (s, "inconsistent opaque object info for type ");
    OutT (s, o1.type);
    Out  (s, Wr.EOL);
    DumpObj (s, o2);
    Out  (s, " => ", MxRep.UnitName (s.unit), Wr.EOL);
    DumpObj (s, o1);
    Out  (s, " => ");

    IF (NOT u.virtual) THEN PrintUnit (u) END;

    cl := MxMap.Get (s.base.clients, o1.source);
    ui := MxSet.ToList (cl);
    WHILE (ui # NIL) DO
      u := ui.unit;
      IF NOT CheckUnitObjs (u, u.exported_objects, o2) THEN
        EVAL CheckUnitObjs (u, u.imported_objects, o2);
      END;
      ui := ui.next;
    END;

    RETURN FALSE;
  END BadObject;

PROCEDURE DumpObj (VAR s: State;  o: Mx.ObjectType) =
  BEGIN
    Out  (s, "super type: ");
    OutT (s, o.super_type);
    Out  (s, " data: (");
    Out  (s, "size: ", Fmt.Int (o.data_size));
    Out  (s, ", align: ", Fmt.Int (o.data_align));
    Out  (s, ")  method: (");
    Out  (s, "size: ", Fmt.Int (o.method_size));
    Out  (s, ")", Wr.EOL);
  END DumpObj;

(*------------------------------------------------------------------------*)

PROCEDURE AddObjects (VAR s: State;  o: Mx.ObjectType) =
  VAR u: Mx.Unit;  x: Mx.ObjectType;
  BEGIN
    WHILE (o # NIL) DO
      IF (NOT o.export) THEN
        MxRep.GetExportedObject (s.base, o, u, x);
        IF (x = NIL) THEN
          (* make sure somebody "exports" this object type *)
          u := MxRep.GetVirtualUnit (s.base, o.source, s.unit);
          x := NEW (Mx.ObjectType);  x^ := o^;
          x.next := u.exported_objects;  u.exported_objects := x;
          x.export := TRUE;
        END;
      END;
      o := o.next;
    END;
  END AddObjects;

(*------------------------------------------------------------------------*)

PROCEDURE FilterClients (VAR s: State;  ui: Mx.UnitList): Mx.UnitList =
  VAR
    bad_guys: MxSet.T := NIL;
  BEGIN
    WHILE (ui # NIL) DO
      IF ReCheckUnit (s, ui.unit) THEN
        IF (bad_guys = NIL) THEN bad_guys := MxSet.New ();  END;
        MxSet.Insert (bad_guys, ui.unit);
        ui.unit.virtual := TRUE;
      END;
      ui := ui.next;
    END;
    RETURN MxSet.ToList (bad_guys);
  END FilterClients;

PROCEDURE ReCheckUnit (VAR s: State;  u: Mx.Unit): BOOLEAN =
  VAR old_failed := s.failed;  bad_unit := FALSE;
  BEGIN
    s.failed := FALSE;

    (* check the old importers against the new exporter *)
    IF (NOT s.failed) THEN ReCheckStamps (s, u, u.import_def_syms); END;
    IF (NOT s.failed) THEN ReCheckStamps (s, u, u.import_use_syms); END;
    IF (NOT s.failed) THEN ReCheckRevelations (s, u.revelations, u);  END;
    IF (NOT s.failed) THEN ReCheckObjects (s, u.imported_objects, u); END;
    bad_unit := s.failed;

    s.failed := old_failed;
    RETURN bad_unit;
  END ReCheckUnit;

(*------------------------------------------------------------------------*)

PROCEDURE OutT (VAR s: State;  x: INTEGER) =
  BEGIN
    IF (s.errs = NIL) THEN RETURN END;
    IF (s.err_buf = NIL) THEN s.err_buf := M3Buf.New (); END;
    MxIO.PutTxt (s.err_buf, "_t");
    MxIO.PutHex (s.err_buf, x, " ");
  END OutT;

PROCEDURE OutX (VAR s: State;  READONLY x: M3FP.T) =
  BEGIN
    IF (s.errs = NIL) THEN RETURN END;
    IF (s.err_buf = NIL) THEN s.err_buf := M3Buf.New (); END;
    MxIO.PutTxt (s.err_buf, "  <");
    MxIO.PutFP  (s.err_buf, x, ">");
  END OutX;

PROCEDURE Out (VAR s: State;  a, b, c, d: TEXT := NIL) =
  BEGIN
    IF (s.errs = NIL) THEN RETURN END;
    IF (s.err_buf = NIL) THEN s.err_buf := M3Buf.New (); END;
    IF (a # NIL) THEN MxIO.PutTxt (s.err_buf, a); END;
    IF (b # NIL) THEN MxIO.PutTxt (s.err_buf, b); END;
    IF (c # NIL) THEN MxIO.PutTxt (s.err_buf, c); END;
    IF (d # NIL) THEN MxIO.PutTxt (s.err_buf, d); END;
  END Out;

BEGIN
END MxMerge.
