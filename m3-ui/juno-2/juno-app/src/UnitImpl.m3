(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Tue Jun 20 21:08:02 PDT 1995 by heydon                   *)

MODULE UnitImpl;

IMPORT View, Drawing, ExternalProc;
FROM ExternalProc IMPORT Closure, Bind;
IMPORT JunoScope;
IMPORT JunoArgs, RTVal;
IMPORT Filter;
IMPORT Atom;

VAR (*CONST*)
  UnitSym := Atom.FromText("Unit");
  PixelSym  := Atom.FromText("Pixel");

PROCEDURE New(rt: View.Root): JunoScope.Mod =
  VAR
    scp := JunoScope.New(NIL, size := 6);
    res := NEW(JunoScope.Mod, public_scp := scp, scp := scp);
  BEGIN
    ExternalProc.SetupBind(UnitSym, scp, rt);
    Bind(PixelSym, NEW(Closure, invoke := PixelProc), in := 0, out := 2);
    RETURN res
  END New;

PROCEDURE PixelProc(cl: Closure): BOOLEAN =
  VAR ch: Drawing.ChildPublic := Filter.Child(cl.rt.currView); BEGIN
    JunoArgs.WriteValue(2, RTVal.FromReal(1.0 / ch.xform.xScale));
    JunoArgs.WriteValue(1, RTVal.FromReal(1.0 / ch.xform.yScale));
    RETURN TRUE
  END PixelProc;

BEGIN
END UnitImpl.
