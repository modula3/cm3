(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Wed Jan  6 16:20:54 PST 1993 by steveg   *)
(*      modified on Thu Sep 24 11:02:06 PDT 1992 by mhb      *)
(*      modified on Mon Jul 27 23:05:50 1992 by najork*)
(*      modified on Thu Jul 23 22:37:49 PDT 1992 by johnh*)


MODULE DotsView;

IMPORT ColorName, PaintOp, RectsVBT, Sort,
       SortViewClass, View, ZeusPanel;

REVEAL
  T = Public BRANDED OBJECT
        N    : CARDINAL     := 0;
        a    : Sort.Keys;
        rects: RectsVBT.T;
        op   : PaintOp.T;
      OVERRIDES
        init        := ViewInit;
        startrun    := Startrun;
        oeInit      := Init;
        oeSetVal    := SetVal;
        oeSwapElts  := SwapElts;
        setPosition := DotsSetPosition;
      END;

PROCEDURE Startrun (view: T) =
  BEGIN
    SortViewClass.T.startrun(view);
    RectsVBT.SetN(view.rects, 0, TRUE);
  END Startrun;

PROCEDURE Init (view: T; N: CARDINAL; <* UNUSED *> passes: CARDINAL) =
  BEGIN
    view.N := N;
    FOR i := 1 TO N DO view.a[i] := 0 END;
    RectsVBT.SetWC(
      view.rects, 0.0, 0.0, FLOAT(N + 1), FLOAT(N + 1));
    RectsVBT.SetN(view.rects, N);
    FOR i := 1 TO N DO RectsVBT.Color(view.rects, i, view.op) END
  END Init;

PROCEDURE SetVal (view: T; i: CARDINAL; val: Sort.Key) =
  BEGIN
    Erase(view, i);
    view.a[i] := val;
    Draw(view, i);
  END SetVal;

PROCEDURE SwapElts (view: T; i, j: CARDINAL) =
  BEGIN
    Erase(view, i);
    Erase(view, j);
    VAR t := view.a[i]; BEGIN
      view.a[i] := view.a[j];
      view.a[j] := t;
    END;
    Draw(view, i);
    Draw(view, j);
  END SwapElts;


PROCEDURE Erase (view: T; i: CARDINAL) =
  BEGIN
    RectsVBT.Erase(view.rects, i)
  END Erase;

PROCEDURE Draw (view: T; i: CARDINAL) =
  BEGIN
    view.setPosition(view.rects, i, view.a[i]);
    RectsVBT.Draw(view.rects, i);
  END Draw;


PROCEDURE DotsSetPosition (<* UNUSED *> view : T;
                                        rects: RectsVBT.T;
                                        i    : CARDINAL;
                                        val  : Sort.Key    ) =
  BEGIN
    RectsVBT.Position(rects, i, FLOAT(i) - 0.5, FLOAT(val) - 0.5,
                      FLOAT(i) + 0.5, FLOAT(val) + 0.5);
  END DotsSetPosition;


PROCEDURE ViewInit (view: T; op: PaintOp.T): T =
  BEGIN
    view.rects := NEW(RectsVBT.T).init();
    view.op := op;
    RETURN SortViewClass.T.init(view, view.rects);
  END ViewInit;

<* FATAL ColorName.NotFound *>
VAR 
  color: PaintOp.T;

PROCEDURE New (): View.T =
  BEGIN
    RETURN NEW(T).init(color)
  END New;

BEGIN
  WITH rgb = ColorName.ToRGB("Red") DO
    color := PaintOp.FromRGB(rgb.r, rgb.g, rgb.b);
  END;
  ZeusPanel.RegisterView (New, "Dots", "Sort");
END DotsView.
