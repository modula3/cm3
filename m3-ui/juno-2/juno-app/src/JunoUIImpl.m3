(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Sun Apr 14 14:47:23 PDT 1996 by heydon                   *)
(*      modified on Sun Jun 11 17:28:23 PDT 1995 by gnelson                  *)

MODULE JunoUIImpl;

IMPORT View, Drawing, Drag, JunoPt, ExternalProc;
FROM ExternalProc IMPORT Closure, Bind;
IMPORT JunoScope;
IMPORT JunoArgs, RTVal;
IMPORT Filter;
IMPORT Atom;

VAR (*CONST*)
  DragNameSym := Atom.FromText("DragName");
  DragLocSym := Atom.FromText("DragLoc");

PROCEDURE New(rt: View.Root): JunoScope.Mod =
  VAR
    scp := JunoScope.New(NIL, size := 2);
    res := NEW(JunoScope.Mod, public_scp := scp, scp := scp);
  BEGIN
    ExternalProc.SetupBind(ModSym, scp, rt);
    Bind(DragNameSym, NEW(Closure, invoke := DragNameProc),
      in := 0, out := 1);
    Bind(DragLocSym,  NEW(Closure, invoke := DragLocProc),
      in := 0, out := 1);
    Bind(DragSym,     NEW(Closure, invoke := Drag.CallbackProc),
      in := 1, out := 1);
    RETURN res
  END New;

PROCEDURE DragNameProc(cl: Closure): BOOLEAN =
  VAR drag: Drag.T := cl.rt.drawing; val: RTVal.T; BEGIN
    IF drag.dragging
      THEN val := RTVal.FromText(drag.dragName)
      ELSE val := RTVal.nil
    END;
    JunoArgs.WriteValue(1, val);
    RETURN TRUE
  END DragNameProc;

PROCEDURE DragLocProc(cl: Closure): BOOLEAN =
  VAR drag: Drag.T := cl.rt.drawing; val: RTVal.T; BEGIN
    IF drag.dragging THEN 
      VAR 
        child: Drawing.Child := Filter.Child(cl.rt.drawing);
        xypt := JunoPt.FromHV(drag.draggee, child.xform); 
      BEGIN
        val := RTVal.FromPair(RTVal.FromReal(xypt.x), RTVal.FromReal(xypt.y))
      END
    ELSE
      val := RTVal.nil
    END;
    JunoArgs.WriteValue(1, val);
    RETURN TRUE
  END DragLocProc;

BEGIN
  (* initialize globals *)
  ModSym  := Atom.FromText("JunoUI");
  DragSym := Atom.FromText("_DRAG");
END JunoUIImpl.
