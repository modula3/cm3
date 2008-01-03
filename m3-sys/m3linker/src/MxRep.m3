(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Mx.m3                                                 *)
(* Last Modified On Tue Aug  2 07:36:17 PDT 1994 By kalsow     *)

MODULE MxRep;

IMPORT M3ID, Mx, MxMap, MxSet, MxVS, MxVSSet;

PROCEDURE UnitName (u: Mx.Unit): TEXT =
  CONST Tag = ARRAY BOOLEAN OF TEXT { ".m3", ".i3" };
  BEGIN
    IF (u = NIL) THEN RETURN "<NIL>" END;
    RETURN M3ID.ToText (u.name) & Tag [u.interface];
  END UnitName;

VAR import_file: Mx.File := NIL;

PROCEDURE GetVirtualUnit (x: Mx.LinkSet;  nm: Mx.Name;
                          client: Mx.Unit): Mx.Unit =
  VAR
    u: Mx.Unit := MxMap.Get (x.virtuals, nm);
    c: MxSet.T := MxMap.Get (x.clients, nm);
  BEGIN
    IF (import_file = NIL) THEN
      import_file := NEW (Mx.File, name := "*virtual*", imported := FALSE);
    END;
    IF (u = NIL) THEN
      u := NEW (Mx.Unit, interface := TRUE, name := nm,
                file := import_file, virtual := TRUE);
      MxMap.Insert (x.virtuals, nm, u);
      u.info := NEW (Mx.InfoVec, 20);
    END;
    IF (c = NIL) THEN
      c := MxSet.New ();
      MxMap.Insert (x.clients, nm, c);
    END;
    MxSet.Insert (c, client);
    RETURN u;
  END GetVirtualUnit;

(**
PROCEDURE AddVirtualInfo (u: Mx.Unit;  VAR z: Mx.InfoList;  i: INTEGER) =
  VAR n := NUMBER (u.info^);  new: Mx.InfoVec;
  BEGIN
    <*ASSERT z.start = 0*>
    IF (z.cnt >= n) THEN
      new := NEW (Mx.InfoVec, n + n);
      SUBARRAY (new^, 0, n) := u.info^;
      u.info := new;
    END;
    u.info [z.cnt] := i;
    INC (z.cnt);
  END AddVirtualInfo;
***)

PROCEDURE GetStamp  (x       : Mx.LinkSet;
                     set     : MxVSSet.T;
                     vs      : MxVS.T;
          VAR(*OUT*) unit    : Mx.Unit;
          VAR(*OUT*) stamp   : MxVS.T) =
  VAR info: MxVS.Info;
  BEGIN
    stamp := MxVSSet.Get (set, vs);
    IF (stamp # MxVS.NoVS) THEN
      MxVS.Get (vs, info);
      unit  := MxMap.Get (x.interfaces, info.source);
      IF (unit = NIL) THEN unit := MxMap.Get (x.virtuals, info.source); END;
    END;
  END GetStamp;

PROCEDURE GetExportedObject (x       : Mx.LinkSet;
                             o       : Mx.ObjectType;
                  VAR(*OUT*) unit    : Mx.Unit;
                  VAR(*OUT*) object  : Mx.ObjectType) =
  VAR u: Mx.Unit;  oo: Mx.ObjectType;
  BEGIN
    unit := NIL;
    object := NIL;

    IF (o.from_module)
      THEN u := MxMap.Get (x.modules, o.source);
      ELSE u := MxMap.Get (x.interfaces, o.source);
    END;
    IF (u = NIL) THEN u := MxMap.Get (x.virtuals, o.source); END;
    IF (u = NIL) THEN RETURN; END;
    unit := u;

    oo := u.exported_objects;
    WHILE (oo # NIL) DO
      IF (oo.type = o.type) AND (oo.from_module = o.from_module) THEN
        object := oo;
        RETURN;
      END;
      oo := oo.next;
    END;
  END GetExportedObject;

PROCEDURE GetExportedRevelation (x       : Mx.LinkSet;
                                 r       : Mx.Revelation;
                      VAR(*OUT*) unit    : Mx.Unit;
                      VAR(*OUT*) rev     : Mx.Revelation) =
  VAR rx: Mx.Revelation;  u: Mx.Unit;
  BEGIN
    unit := NIL;
    rev := NIL;

    u := MxMap.Get (x.interfaces, r.source);
    IF (u = NIL) THEN u := MxMap.Get (x.virtuals, r.source); END;
    IF (u = NIL) THEN RETURN; END;
    unit := u;

    rx := u.revelations;
    WHILE (rx # NIL) DO
      IF (rx.export)
        AND (rx.lhs = r.lhs)
        AND (rx.rhs = r.rhs)
        AND (rx.partial = r.partial) THEN
        rev := rx;
        RETURN;
      END;
      rx := rx.next;
    END;
  END GetExportedRevelation;

BEGIN
END MxRep.
