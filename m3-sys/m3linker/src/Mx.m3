(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: Mx.m3                                                 *)
(* Last Modified On Tue Aug  2 07:32:49 PDT 1994 By kalsow     *)

MODULE Mx;

IMPORT MxRep, MxMap, MxVSSet;

PROCEDURE NewSet (): LinkSet =
  VAR s := NEW (LinkSet);
  BEGIN
    s.interfaces     := MxMap.New (1009);
    s.modules        := MxMap.New (1009);
    s.virtuals       := MxMap.New (39);
    s.clients        := MxMap.New (39);
    s.vs_exports     := MxVSSet.New (7001);
    s.vs_impls       := MxVSSet.New (7001);
    s.exported_types := MxMap.New (7001);
    RETURN s;
  END NewSet;

PROCEDURE Contents (base: LinkSet): UnitList =
  VAR units: UnitList := NIL;  x: MxMap.Contents;    u: Unit;
  BEGIN
    IF (base # NIL) THEN
      x := MxMap.GetData (base.interfaces);
      FOR i := 0 TO LAST (x^) DO
        u := x[i].value;
        IF (u # NIL) THEN
          units := NEW (UnitList, next := units, unit := u);
        END;
      END;
      x := MxMap.GetData (base.modules);
      FOR i := 0 TO LAST (x^) DO
        u := x[i].value;
        IF (u # NIL) THEN
          units := NEW (UnitList, next := units, unit := u);
        END;
      END;
    END;
    RETURN units;
  END Contents;

BEGIN
END Mx.
