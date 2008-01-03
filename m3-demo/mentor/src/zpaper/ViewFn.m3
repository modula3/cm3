(* Copyright 1992 Digital Equipment Corporation.           *)
(* Distributed only by permission.                         *)
(* Last modified on Mon Sep 14 22:38:39 PDT 1992 by mhb        *)

MODULE ViewFn;

IMPORT ChipsVBT, ZPaperViewClass, View, ZeusPanel;

REVEAL
  T = ZPaperViewClass.T BRANDED OBJECT
        chips: ChipsVBT.T;
      OVERRIDES
        oeInit   := Init;
        oeSet    := Set;
      END;

PROCEDURE SetChipsVBT (view: T; chips: ChipsVBT.T) =
  BEGIN
    view.chips := chips
  END SetChipsVBT;

PROCEDURE Init (view: T; N, k: INTEGER) =
  BEGIN
    ChipsVBT.Reset(view.chips, N, N, k);
  END Init;

PROCEDURE Set (view: T; x, y, val: INTEGER) =
  BEGIN
    ChipsVBT.Set(view.chips, x, y, 1 + val)
  END Set;

PROCEDURE New (): View.T =
  VAR c := NEW(ChipsVBT.T).init();
  BEGIN
    RETURN NEW(T, chips := c).init(c)
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Panel", "ZPaper");
END ViewFn.
















