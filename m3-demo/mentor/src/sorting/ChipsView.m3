(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Last modified on Tue Jul 28 00:02:36 1992 by najork*)
(*      modified on Sun Jul 19 21:08:34 1992 by mhb  *)

MODULE ChipsView;

IMPORT ChipsVBT, Sort, SortViewClass, View, ZeusPanel;

TYPE 
  T = SortViewClass.T BRANDED OBJECT
        a    : Sort.Keys;
        chips: ChipsVBT.T;
        pass, N: INTEGER;
      OVERRIDES
        oeInit     := Init;
        oeSetVal   := SetVal;
        oeSwapElts := SwapElts;
        oeStartPass:= StartPass;
      END;

PROCEDURE Init (view: T; N: CARDINAL; passes: CARDINAL) =
  BEGIN
    view.pass := 1;
    view.N := N;
    FOR i := 1 TO N DO view.a[i] := 0 END;
    ChipsVBT.Reset(view.chips, N, passes, N);
  END Init;

PROCEDURE SetVal (view: T; i: CARDINAL; val: Sort.Key) =
  BEGIN
    ChipsVBT.Set(view.chips, i, view.pass, val);
    view.a[i] := val;
  END SetVal;

PROCEDURE SwapElts (view: T; i, j: CARDINAL) =
  VAR tmp := view.a[i];
  BEGIN
    SetVal(view, i, view.a[j]);
    SetVal(view, j, tmp);
  END SwapElts;

PROCEDURE StartPass (view: T) =
  BEGIN
    INC(view.pass);
    FOR i := 1 TO view.N DO SetVal(view, i, view.a[i]) END
  END StartPass;

PROCEDURE New (): View.T =
  VAR view := NEW(T);
  BEGIN
    view.chips := NEW(ChipsVBT.T).init();
    RETURN view.init(view.chips)
  END New;

BEGIN
  ZeusPanel.RegisterView (New, "Chips", "Sort");
END ChipsView.
