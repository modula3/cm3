(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Thu Jan  5 23:44:12 PST 1995 by najork *)
(*      modified on Wed Jan  6 16:19:18 PST 1993 by steveg *)
(*      modified on Thu Sep 24 11:01:56 PDT 1992 by mhb    *)
(*      modified on Thu Jul 23 22:37:36 PDT 1992 by johnh*)

MODULE SticksView;

IMPORT ColorName, DotsView, PaintOp, RectsVBT, Sort, View,
       ZeusPanel;

REVEAL
  T = DotsView.T BRANDED OBJECT
      OVERRIDES
        setPosition := SticksSetPosition;
      END;

PROCEDURE SticksSetPosition (<* UNUSED *> view : T;
                                          rects: RectsVBT.T;
                                          i    : CARDINAL;
                                          val  : Sort.Key    ) =
  BEGIN
    RectsVBT.Position(rects, i, FLOAT(i) - 0.5, 0.5,
                      FLOAT(i) + 0.5, FLOAT(val) + 0.5)
  END SticksSetPosition;

VAR
  color: PaintOp.T;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(color)
  END New;

<* FATAL ColorName.NotFound *>
BEGIN
 WITH rgb = ColorName.ToRGB("Blue") DO
    color := PaintOp.FromRGB(rgb.r, rgb.g, rgb.b)
  END;
  ZeusPanel.RegisterView (New, "Sticks", "Sort");
END SticksView.
