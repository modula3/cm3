(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Fri Jan  6 00:41:27 PST 1995 by najork     *)
(*      modified on Thu Sep 24 13:17:01 PDT 1992 by mhb        *)

MODULE ViewMFn;

IMPORT ChipsVBT, FormsVBT, Fmt, VBT, View, ViewFn, ZeusPanel;

<* FATAL FormsVBT.Error, FormsVBT.Unimplemented *>

TYPE
  T = ViewFn.T BRANDED OBJECT
        frame: FormsVBT.T;
        cells: INTEGER;
        ct   : INTEGER      := 0;
      OVERRIDES
        startrun := Startrun;
      END;

PROCEDURE Startrun (view: T) =
  VAR chips := NEW(ChipsVBT.T).init();
  BEGIN
    INC(view.ct);
    LOCK VBT.mu DO
      FormsVBT.PutGeneric(
        view.frame, "v" & Fmt.Int(view.ct), chips);
    END;
    ViewFn.SetChipsVBT(view, chips);
    ViewFn.T.startrun(view);
    view.ct := view.ct MOD view.cells;
  END Startrun;

PROCEDURE New (): View.T =
  VAR
    fv : FormsVBT.T := ZeusPanel.NewForm("zviewframe.fv");
    res: T          := NEW(T).init(fv);
  BEGIN
    res.cells := FormsVBT.GetInteger(fv, "cells");
    res.frame := fv;
    RETURN res
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Panel History", "ZPaper");
END ViewMFn.
