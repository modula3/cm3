(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxCheck.m3                                            *)
(* Last Modified On Mon Sep 19 14:29:36 PDT 1994 By kalsow     *)
(*      Modified On Wed May 26 15:47:11 PDT 1993 By muller     *)

MODULE MxCheck;

IMPORT Text, Fmt, Wr, Thread, Word;
IMPORT Mx, MxRep, MxMap, M3ID, MxSet, MxVS, MxVSSet;
<*FATAL Wr.Failure, Thread.Alerted*>

CONST
  Margin = 78;

TYPE
  State = RECORD
    base       : Mx.LinkSet;
    errors     : Wr.T;
    failed     : BOOLEAN     := FALSE;
    opaques    : MxMap.T     := NIL; (* type name -> OpaqueInfo *)
    all_opaques: OpaqueInfo  := NIL;
    import_err : ImportError := NIL;
    err_width  : INTEGER     := 0;
    bad_vs     : MxVS.Info;
  END;

TYPE
  UnitProc = PROCEDURE (VAR s: State;  u: Mx.Unit);

TYPE
  ImportError = REF RECORD
    import_name : Mx.Name;
    importer    : Mx.Unit;
    next        : ImportError;
  END;

TYPE
  OpaqueInfo = REF RECORD
    next   : OpaqueInfo     := NIL;
    type   : Mx.OpaqueType  := NIL;
    t_unit : Mx.Unit        := NIL;
    reveal : Mx.Revelation  := NIL;
    r_unit : Mx.Unit        := NIL;
  END;

(*------------------------------------------------------------------------*)

PROCEDURE IsProgram (base: Mx.LinkSet;  errors : Wr.T): BOOLEAN =
  VAR s: State;
  BEGIN
    InitState (s, base, errors);
    CheckUnits (s);    IF (s.failed) THEN RETURN FALSE END;
    CheckMain (s);     IF (s.failed) THEN RETURN FALSE END;
    CheckStamps (s);   IF (s.failed) THEN RETURN FALSE END;
    CheckOpaques (s);  IF (s.failed) THEN RETURN FALSE END;
    RETURN TRUE;
  END IsProgram;

PROCEDURE IsLibrary (base: Mx.LinkSet;  errors : Wr.T): BOOLEAN =
  VAR s: State;
  BEGIN
    InitState (s, base, errors);
    CheckUnits (s);    IF (s.failed) THEN RETURN FALSE END;
    RETURN TRUE;
  END IsLibrary;

PROCEDURE InitState (VAR s: State;  base: Mx.LinkSet;  errors: Wr.T) =
  BEGIN
    s.base       := base;
    s.errors     := errors;
    s.failed     := FALSE;
  END InitState;

(*------------------------------------------------------------------------*)

PROCEDURE CheckUnits (VAR s: State) =
  BEGIN
    s.import_err := NIL;

    (* make sure that there are no virtual units remaining *)
    ForEachUnit (s, CheckVirtualUnit);

    (* check to make sure that all imports and exports are satisfied *)
    ForEachUnit (s, CheckUnitImports);

    IF (s.import_err # NIL) THEN DumpImportErrors (s, s.import_err) END;
  END CheckUnits;

PROCEDURE CheckVirtualUnit (VAR s: State;  u: Mx.Unit) =
  BEGIN
    IF (u.virtual) THEN
      s.import_err := NEW (ImportError, next := s.import_err, importer := u,
                           import_name := LAST(Mx.Name));
    END;
  END CheckVirtualUnit;

PROCEDURE CheckUnitImports (VAR s: State;  u: Mx.Unit) =
  BEGIN
    CheckUnitList (s, u, u.imported_units);
    CheckUnitList (s, u, u.exported_units);
    CheckUnitList (s, u, u.used_interfaces);
  END CheckUnitImports;

PROCEDURE CheckUnitList (VAR s: State;  u: Mx.Unit;  READONLY n: Mx.InfoList) =
  VAR nm: INTEGER;
  BEGIN
    FOR i := n.start TO n.start + n.cnt - 1 DO
      nm := u.info [i];
      IF MxMap.Get (s.base.interfaces, nm) = NIL THEN
        s.import_err := NEW (ImportError, next := s.import_err, importer := u,
                             import_name := nm);
      END;
    END;
  END CheckUnitList;

PROCEDURE DumpImportErrors (VAR s: State;  err: ImportError) =
  VAR new, match, tmp: ImportError;
  BEGIN
    WHILE (err # NIL) DO
      new := NIL;
      match := err;
      err := err.next;
      match.next := NIL;
      WHILE (err # NIL) DO
        tmp := err.next;
        IF (err.import_name = match.import_name)
          THEN err.next := match;  match := err;
          ELSE err.next := new;    new := err;
        END;
        err := tmp;
      END;
      IF (match.import_name = LAST (Mx.Name))
        THEN DumpMissingUnit (s, match);
        ELSE DumpImportErrorList (s, match);
      END;
      err := new;
    END;
  END DumpImportErrors;

PROCEDURE DumpImportErrorList (VAR s: State;  err: ImportError) =
  VAR name := M3ID.ToText (err.import_name);
  BEGIN
    Err (s, "missing compiled interface \"", name, ".io\" imported by:  ");
    WHILE (err # NIL) DO
      Err (s, MxRep.UnitName (err.importer), "  ");
      err := err.next;
    END;
    ErrNL (s);
  END DumpImportErrorList;

PROCEDURE DumpMissingUnit (VAR s: State;  err: ImportError) =
  CONST RSym = ARRAY BOOLEAN OF TEXT { " = ", " <: " };
  VAR u_name: TEXT;  x: ImportError;
      r: Mx.Revelation;  o: Mx.ObjectType;  u: Mx.Unit;
  BEGIN
    x := err;
    WHILE (x # NIL) DO
      u := x.importer;
      u_name := MxRep.UnitName (u);
      IF (u.export_def_syms.cnt + u.export_use_syms.cnt > 0) THEN
        Err (s, u_name, ": missing exported symbols:  ");
        DumpVSList (s, u, u.export_def_syms);
        DumpVSList (s, u, u.export_use_syms);
        ErrNL (s);
      END;
      r := u.revelations;
      IF (r # NIL) THEN
        Err (s, u_name, ": missing revelations:  ");
        WHILE (r # NIL) DO
          Err (s, TName (s, r.lhs), RSym[r.partial], TName (s, r.rhs), "   ");
          r := r.next;
        END;
        ErrNL (s);
      END;
      o := u.exported_objects;
      IF (o # NIL) THEN
        Err (s, u_name, ": missing object types:  ");
        WHILE (o # NIL) DO
          Err (s, TName (s, o.type), "  ");
          o := o.next;
        END;
        ErrNL (s);
      END;
      DumpClients (s, u);
      x := x.next;
    END;
  END DumpMissingUnit;

PROCEDURE DumpVSList (VAR s: State;  u: Mx.Unit;  READONLY z: Mx.InfoList) =
  VAR info: MxVS.Info;
  BEGIN
    FOR i := z.start TO z.start + z.cnt - 1 DO
       MxVS.Get (u.info[i], info);
       Err (s, M3ID.ToText (info.source), ".",
               M3ID.ToText (info.symbol),"  ");
    END;
  END DumpVSList;

PROCEDURE DumpClients (VAR s: State;  u: Mx.Unit) =
  VAR cl: MxSet.T;  ux: Mx.UnitList;
  BEGIN
    cl := MxMap.Get (s.base.clients, u.name);
    ux := MxSet.ToList (cl);
    IF (ux = NIL) THEN RETURN; END;
    Err (s, "imported by:  ");
    WHILE (ux # NIL) DO
      IF (ux.unit # u) THEN
        Err (s, MxRep.UnitName (ux.unit), "  ");
      END;
      ux := ux.next;
    END;
    ErrNL (s);
  END DumpClients;

(*------------------------------------------------------------------------*)

PROCEDURE CheckMain (VAR s: State) =
  (* check to make sure that "Main" is exported *)
  VAR main := M3ID.Add ("Main");
  VAR unit := MxMap.Get (s.base.interfaces, main);
  BEGIN
    IF (unit = NIL) THEN
      Err (s, "missing \"Main\" module", Wr.EOL);
      ErrNL (s);
    END;
  END CheckMain;

(*------------------------------------------------------------------------*)

PROCEDURE CheckStamps (VAR s: State) =
  VAR c: MxVSSet.Contents;  vs: MxVS.T;
  BEGIN
    (* make sure that every defined stamp is implemented *)
    c := MxVSSet.GetData (s.base.vs_exports);
    FOR i := 0 TO LAST (c^) DO
      vs := c[i];
      IF (vs # MxVS.NoVS) THEN
        IF MxVSSet.Get (s.base.vs_impls, vs) = MxVS.NoVS THEN
          DumpStamp (s, vs, ": is exported, but not implemented:  ");
        END;
      END;
    END;
 
    (* make sure that every implemented stamp is defined *)
    c := MxVSSet.GetData (s.base.vs_impls);
    FOR i := 0 TO LAST (c^) DO
      vs := c[i];
      IF (vs # MxVS.NoVS) THEN
        IF MxVSSet.Get (s.base.vs_exports, vs) = MxVS.NoVS THEN
          DumpStamp (s, vs, ": is implemented, but not exported:  ");
        END;
      END;
    END;
  END CheckStamps;

PROCEDURE DumpStamp (VAR s: State;  vs: MxVS.T;  msg: TEXT) =
  VAR info: MxVS.Info;
  BEGIN
    MxVS.Get (vs, info);
    Err (s, M3ID.ToText (info.source), ".");
    Err (s, M3ID.ToText (info.symbol), msg);
    MxVS.Get (vs, s.bad_vs);
    ForEachUnit (s, DumpBadVS);
    ErrNL (s);
  END DumpStamp;

PROCEDURE DumpBadVS (VAR s: State;  u: Mx.Unit) =
  BEGIN
    IF   DumpBadVStamps (s, u, u.export_def_syms, s.bad_vs)
      OR DumpBadVStamps (s, u, u.export_use_syms, s.bad_vs)
      OR DumpBadVStamps (s, u, u.import_def_syms, s.bad_vs)
      OR DumpBadVStamps (s, u, u.import_use_syms, s.bad_vs) THEN
    END;
  END DumpBadVS;

PROCEDURE DumpBadVStamps (VAR s: State;  u: Mx.Unit;  READONLY z: Mx.InfoList;
                          READONLY bad: MxVS.Info): BOOLEAN =
  VAR info: MxVS.Info;  vs: MxVS.T;
  BEGIN
    FOR i := z.start TO z.start + z.cnt - 1 DO
      vs := u.info [i];
      MxVS.Get (vs, info);
      IF (info.source = bad.source) AND (info.symbol = bad.symbol) THEN
        Err (s, MxRep.UnitName (u), "  ");
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END DumpBadVStamps;
(*------------------------------------------------------------------------*)

PROCEDURE CheckOpaques (VAR s: State) =
  VAR o: OpaqueInfo;
  BEGIN
    s.opaques := MxMap.New (503);
    ForEachUnit (s, NoteOpaques);
    ForEachUnit (s, IdentifyOpaques);
    o := s.all_opaques;
    WHILE (o # NIL) DO
      IF (o.reveal = NIL) THEN
        Err (s, "opaque type never revealed: ", TName (s, o.type.type));
        ErrNL (s);
        Err (s, "  defined in ", MxRep.UnitName (o.t_unit));
        ErrNL (s);
      END;
      o := o.next;
    END;
  END CheckOpaques;

PROCEDURE NoteOpaques (VAR s: State;  u: Mx.Unit) =
  VAR o: Mx.OpaqueType;  z: OpaqueInfo;
  BEGIN
    o := u.opaques;
    WHILE (o # NIL) DO
      z := MxMap.Get (s.opaques, o.type);
      IF (z # NIL) THEN
        Err (s, "opaque type defined twice: ", TName (s, z.type.type));
        ErrNL (s);
        Err (s, "  defined in  ", MxRep.UnitName (z.t_unit));  ErrNL (s);
        Err (s, "  and also    ", MxRep.UnitName (u));         ErrNL (s);
      ELSE
        z := NEW (OpaqueInfo, type := o, t_unit := u, next:= s.all_opaques);
        s.all_opaques := z;
        MxMap.Insert (s.opaques, o.type, z);
      END;
      o := o.next;
    END;
  END NoteOpaques;

PROCEDURE IdentifyOpaques (VAR s: State;  u: Mx.Unit) =
  VAR z: OpaqueInfo;  r := u.revelations;
  BEGIN
    WHILE (r # NIL) DO
      IF (r.partial) OR (NOT r.export) THEN
        (* ignore for now *)
      ELSE
        z := MxMap.Get (s.opaques, r.lhs);
        IF (z # NIL) THEN
          IF (z.reveal # NIL) THEN
            Err (s, "multiple revelations for opaque type:  ",
                     TName(s, z.type.type));                       ErrNL (s);
            Err (s, "  defined in  ", MxRep.UnitName (z.t_unit));  ErrNL (s);
            Err (s, "  revealed in ", MxRep.UnitName (z.r_unit));  ErrNL (s);
            Err (s, "  and also in ", MxRep.UnitName (u));         ErrNL (s);
          ELSE
            z.reveal := r;
            z.r_unit := u;
          END;
        ELSE
          Err (s, "revelation without matching opaque type declaration:  ",
                   TName (s, r.lhs));  ErrNL (s);
          Err (s, "  revealed in ", MxRep.UnitName (u));  ErrNL (s);
        END;
      END;
      r := r.next;
    END;
  END IdentifyOpaques;

(*------------------------------------------------------------------------*)

PROCEDURE ForEachUnit (VAR s: State;  p: UnitProc) =
  VAR x: MxMap.Contents;  u: Mx.Unit;
  BEGIN
    x := MxMap.GetData (s.base.interfaces);
    FOR i := 0 TO LAST (x^) DO
      u := x[i].value;
      IF (u # NIL) THEN p (s, u) END;
    END;
    x := MxMap.GetData (s.base.modules);
    FOR i := 0 TO LAST (x^) DO
      u := x[i].value;
      IF (u # NIL) THEN p (s, u) END;
    END;
    x := MxMap.GetData (s.base.virtuals);
    FOR i := 0 TO LAST (x^) DO
      u := x[i].value;
      IF (u # NIL) THEN p (s, u) END;
    END;
  END ForEachUnit;

PROCEDURE TName (<*UNUSED*> VAR s: State;  t: Mx.TypeName): TEXT =
  BEGIN
    RETURN "_t" & Fmt.Unsigned (Word.And (t, 16_ffffffff), 16);
  END TName;

PROCEDURE Err (VAR s: State;  a, b, c, d: TEXT := NIL) =
  VAR len: INTEGER;
  BEGIN
    s.failed := TRUE;
    IF (s.errors = NIL) THEN RETURN END;
    
    len := 0;
    IF (a # NIL) THEN INC (len, Text.Length (a)); END;
    IF (b # NIL) THEN INC (len, Text.Length (b)); END;
    IF (c # NIL) THEN INC (len, Text.Length (c)); END;
    IF (d # NIL) THEN INC (len, Text.Length (d)); END;

    IF (s.err_width + len > Margin) THEN
      Wr.PutText (s.errors, Wr.EOL);
      Wr.PutText (s.errors, "   ");
      s.err_width := 3;
    END;

    IF (a # NIL) THEN Wr.PutText (s.errors, a); END;
    IF (b # NIL) THEN Wr.PutText (s.errors, b); END;
    IF (c # NIL) THEN Wr.PutText (s.errors, c); END;
    IF (d # NIL) THEN Wr.PutText (s.errors, d); END;
    INC (s.err_width, len);
  END Err;

PROCEDURE ErrNL (VAR s: State) =
  BEGIN
    IF (s.errors = NIL) THEN RETURN END;
    IF (s.err_width > 0) THEN
      Wr.PutText (s.errors, Wr.EOL);
      s.err_width := 0;
    END;
  END ErrNL;

BEGIN
END MxCheck.
