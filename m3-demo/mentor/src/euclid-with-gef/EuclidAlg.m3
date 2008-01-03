(* Copyright (C) 1992, Digital Equipment Corporation             *)
(* All rights reserved.                                          *)
(* See the file COPYRIGHT for a full description.                *)
(*                                                               *)
(* Last modified on Thu Feb  9 07:53:10 PST 1995 by kalsow       *)

MODULE EuclidAlg EXPORTS Euclid;

IMPORT Algorithm, GEFView, List, Thread, View, ZeusCodeView, ZeusPanel;

TYPE
  T = GEFView.Alg BRANDED OBJECT
      OVERRIDES
        run := Run;
      END;

PROCEDURE New (): Algorithm.T =
  BEGIN
    RETURN
      NEW(
        T, codeViews :=
             List.List1(List.List2("Euclid's proof", "proof"))).init()
  END New;

CONST Exit = -1;

PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  PROCEDURE Event (name: TEXT; line: INTEGER; pause := TRUE)
    RAISES {Thread.Alerted} =
    BEGIN
      IF name # NIL THEN GEFView.ViewEvent(alg, name); END;
      IF line # Exit THEN
        ZeusCodeView.Event(alg, line);
        GEFView.ViewPause(alg);
      ELSE
        ZeusCodeView.Exit(alg);
      END;
    END Event;

  BEGIN
    GEFView.ViewInit(alg, "EuclidView.gef");
    ZeusCodeView.Event(alg, procedureName := "Proposition");
    Event(NIL, 777);
    Event(NIL, Exit, FALSE);

    ZeusCodeView.Event(alg, procedureName := "Figure");
    Event("ShowTriangle", 1);
    Event("ShowBigSquare", 2);
    Event("ShowLittleSquares", 3);
    Event("ClearSquares", Exit);

    ZeusCodeView.Event(alg, procedureName := "Squares");
    Event("ShowLabeledBigSquare", 11);
    Event("ShowLabeledLittleSquares", 12);
    Event("ClearSquares", Exit);

    ZeusCodeView.Event(alg, procedureName := "LineAL");
    Event("RedLineAL", 21);
    Event("GreenLinesBDandCE", 22);
    Event("BlackLinesABandBDandCE", Exit);

    ZeusCodeView.Event(alg, procedureName := "Shear1");
    Event("TriangleABF", 31);
    Event("ShearABFtoCBF", 32);
    Event("GreenLineBF", 33);
    Event("GreenLineCG", 34);
    Event("BlackLinesBFandCG", Exit);

    ZeusCodeView.Event(alg, procedureName := "Angles");
    Event("GreenAngleCBD", 51);
    Event("GreenAngleABF", 52);
    Event("RedAngleABC", 53);
    Event("ReplaceAnglesWithGreenCBF", 54);
    Event("RotateCBF", 55);
    Event("ClearAngle", Exit);

    ZeusCodeView.Event(alg, procedureName := "Sides");
    Event("GreenLineBF2", 41);
    Event("RotateLineBF", 42);
    Event("MoveBFtoBC", 43);
    Event("RotateLineBC", 44);
    Event("RemoveLineBC", Exit);

    ZeusCodeView.Event(alg, procedureName := "Rotate");
    Event(NIL, 61);
    Event("RotateCBFtoDBA", 62);
    Event(NIL, Exit);

    ZeusCodeView.Event(alg, procedureName := "Shear2");
    Event(NIL, 71);
    Event("ShearABDtoBDL", 72);
    Event("GreenLineBD", 73);
    Event("GreenLineAL", 74);
    Event("ClearBDLandBDandAL", Exit);

    ZeusCodeView.Event(alg, procedureName := "End1");
    Event("ShowABF", 81);
    Event("ShowBDL", 82);
    Event("ReplaceABFandBDLwithABFG", 83);
    Event("ShowBDLX", 84);
    Event(NIL, Exit);

    ZeusCodeView.Event(alg, procedureName := "Symmetry");
    Event("ShowACK", 91);
    Event("ShearRotateShearACKtoCEL", 92);
    Event("ReplaceCELwithACKH", 93);
    Event("ShowCELX", 94);
    Event(NIL, Exit);

    ZeusCodeView.Event(alg, procedureName := "Rehash");
    Event(NIL, 100);
    Event(NIL, 101);
    Event(NIL, Exit);
  END Run;

PROCEDURE NewView (): View.T =
  VAR view := NEW(GEFView.T);
  BEGIN
    RETURN view.init();
  END NewView;

<* FATAL ZeusPanel.DuplicateName *>
BEGIN
  ZeusPanel.RegisterAlg(New, "P47", "Euclid");
  ZeusPanel.RegisterView(NewView, "P47", "Euclid");
  ZeusPanel.RegisterView(GEFView.NewTranscript, "P47 Transcript View", "Euclid");
END EuclidAlg.
