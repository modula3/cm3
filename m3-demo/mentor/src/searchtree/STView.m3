(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Jan  6 15:55:38 PST 1993 by steveg                   *)
(*      modified on Thu Sep 17 14:12:55 PDT 1992 by heydon                   *)
(*      modified on Fri Sep  4 13:47:12 PDT 1992 by johnh                    *)

MODULE STView;

IMPORT View, MGPublic, GenericTree, VBT, Axis, Font;

REVEAL
  T = TPublic BRANDED OBJECT
    OVERRIDES
      shape := Shape
    END;

CONST
  InitWidth = 400;
  InitHeight = 300;

PROCEDURE New(view: T; v: GenericTree.V): View.T =
  BEGIN
    v.border := ARRAY Axis.T OF REAL{BorderWidth, BorderHeight};
    view.v := v.init();
(*
    RETURN view.init(NEW(Scale.T).init(view.v));
*)
    RETURN view.init(view.v);
  END New;

PROCEDURE StartRun(view: T) =
  VAR v := view.v; BEGIN
    v.setRoot(NIL);
    MGPublic.ResetLookups(v);
    VBT.NewShape(v);
    VBT.Mark(v);
    View.T.startrun(view);
  END StartRun;

PROCEDURE Shape(
  <* UNUSED *> v: VBT.T;
  axis: Axis.T;
  <* UNUSED *> n: CARDINAL): VBT.SizeRange =
  BEGIN
    CASE axis OF
      Axis.T.Hor =>
        RETURN VBT.SizeRange{VBT.DefaultShape.lo,
          InitWidth, VBT.DefaultShape.hi}
    | Axis.T.Ver =>
        RETURN VBT.SizeRange{VBT.DefaultShape.lo,
          InitHeight, VBT.DefaultShape.hi}
    END
  END Shape;

BEGIN
  nodeColor := MGPublic.ColorFromText("VerySlightlyDarkCyan");
  currentColor := MGPublic.ColorFromText("LightYellow");
  compareColor := MGPublic.ColorFromText("PeachPuff");    (* was "Peach" *)
  font := Font.FromName(ARRAY OF TEXT{
    "-*-helvetica-medium-r-*-*-*-100-*-*-*-*-iso8859-1"})
END STView.
