(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE EuclidAlg EXPORTS Euclid;

IMPORT Algorithm, EuclidAlgClass, EuclidIE, RefList, Thread, ZeusCodeView,
       ZeusPanel;

TYPE
  T = EuclidAlgClass.T BRANDED OBJECT
      OVERRIDES
        run := Run;
      END;

PROCEDURE New (): Algorithm.T =
  BEGIN
    RETURN
      NEW(
        T, codeViews :=
             RefList.List1(RefList.List2("Euclid's proof", "proof"))).init()
  END New;


PROCEDURE Run (alg: T) RAISES {Thread.Alerted} =
  PROCEDURE At (line: INTEGER) RAISES {Thread.Alerted} =
    BEGIN ZeusCodeView.Event(alg, line); EuclidIE.Step(alg); END At;
  PROCEDURE Q4(p1, p2, p3, p4: Id; c: INTEGER) RAISES {Thread.Alerted} =
    BEGIN EuclidIE.Quad(alg, p1, p2, p3, p4, c) END Q4;
  PROCEDURE T3(p1, p2, p3: Id; c: INTEGER) RAISES {Thread.Alerted} =
    BEGIN EuclidIE.Triangle(alg, p1, p2, p3, c) END T3;
  PROCEDURE RemT3(p1, p2, p3: Id) RAISES {Thread.Alerted} =
    BEGIN EuclidIE.Remove(alg, p1, p2, p3, -1, FALSE) END RemT3;
  PROCEDURE RemAng(p1, p2, p3: Id) RAISES {Thread.Alerted} =
    BEGIN EuclidIE.Remove(alg, p1, p2, p3, -1, FALSE) END RemAng;
  PROCEDURE RemLine(p1, p2: Id; show := FALSE) RAISES {Thread.Alerted} =
    BEGIN EuclidIE.Remove(alg, p1, p2, -1, -1, show) END RemLine;
  PROCEDURE RemQ4(p1, p2, p3, p4: Id) RAISES {Thread.Alerted} =
    BEGIN EuclidIE.Remove(alg, p1, p2, p3, p4, FALSE) END RemQ4;
BEGIN
    ZeusCodeView.Event(alg, procedureName := "Proposition");
    At(777);
    ZeusCodeView.Exit(alg);

    ZeusCodeView.Event(alg, procedureName := "Figure");
    EuclidIE.SetupTriangle(alg, 3.0, 4.0); At(1);
    Q4(B, C, E, D, 1); At(2); 
    Q4(A, B, F, G, 2); 
    Q4(A, C, K, H, 2); At(3);
    RemQ4(B, C, E, D);
    RemQ4(A, B, F, G); 
    RemQ4(A, C, K, H);
    ZeusCodeView.Exit(alg);

    ZeusCodeView.Event(alg, procedureName := "Squares");
    EuclidIE.SetupSquare(alg, B, D, E, C); 
    Q4(B, C, E, D, 1); At(11); 
    EuclidIE.SetupSquare(alg, A, B, F, G);
    Q4(A, B, F, G, 2);
    EuclidIE.SetupSquare(alg, A, H, K, C); 
    Q4(A, C, K, H, 2); At(12); 
    RemQ4(B, C, E, D);
    RemQ4(A, B, F, G); 
    RemQ4(A, C, K, H);
    ZeusCodeView.Exit(alg);

    ZeusCodeView.Event(alg, procedureName := "LineAL");
    EuclidIE.LineAL(alg, FALSE); 
    EuclidIE.HighlightLine(alg, A, L, 2, TRUE); At(21);
    EuclidIE.HighlightLine(alg, B, D, 1, FALSE);
    EuclidIE.HighlightLine(alg, C, E, 1, TRUE); At(22);

    RemLine(C, E);
    RemLine(B, D);
    RemLine(A, L);
    ZeusCodeView.Exit(alg);

    ZeusCodeView.Event(alg, procedureName := "Shear1");
    T3(A, B, F, 1); At(31);
    EuclidIE.Shear(alg, B, F, A, C); At(32);
    EuclidIE.HighlightLine(alg, B, F, 1, TRUE); At(33);
    EuclidIE.HighlightLine(alg, A, C, 1, FALSE); 
    EuclidIE.HighlightLine(alg, A, G, 1, TRUE); At(34);
    RemLine(A, C);
    RemLine(A, G);
    RemLine(B, F, TRUE);
    ZeusCodeView.Exit(alg);

    ZeusCodeView.Event(alg, procedureName := "Angles");
    EuclidIE.HighlightAngle(alg, D, B, C, 1, TRUE); At(51);
    EuclidIE.HighlightAngle(alg, F, B, A, 1, TRUE); At(52);
    EuclidIE.HighlightAngle(alg, A, B, C, 2, TRUE); At(53);
    RemAng(D, B, C);
    RemAng(F, B, A);
    RemAng(A, B, C);
    EuclidIE.HighlightAngle(alg, C, B, F, 1, TRUE); At(54);
    EuclidIE.RotateAngle(alg, B, C, D, F, A); At(55);
    RemAng(C, B, F);
    ZeusCodeView.Exit(alg);

    ZeusCodeView.Event(alg, procedureName := "Sides");
    EuclidIE.HighlightLine(alg, B, F, 1, TRUE); At(41);
    EuclidIE.RotateLine(alg, B, F, A); At(42);
    RemLine(B, F);
    EuclidIE.HighlightLine(alg, B, C, 1, TRUE); At(43);
    EuclidIE.RotateLine(alg, B, C, D); At(44);
    RemLine(B, C, TRUE);
    ZeusCodeView.Exit(alg);

    ZeusCodeView.Event(alg, procedureName := "Rotate");
    RemT3(A, B, F);
    T3(B, F, C, 1); At(61);
    EuclidIE.RotateTriangle(alg, B, F, A, C, D); At(62);
    RemT3(B, F, C);
    ZeusCodeView.Exit(alg);

    ZeusCodeView.Event(alg, procedureName := "Shear2");
    T3(A, B, D, 1); At(71);
    EuclidIE.Shear(alg, B, D, A, L);  At(72);
    EuclidIE.HighlightLine(alg, B, D, 1, TRUE); At(73);
    EuclidIE.HighlightLine(alg, A, L, 1, TRUE); At(74);
    RemT3(A, B, D);
    RemLine(B, D);
    RemLine(A, L, TRUE);
    ZeusCodeView.Exit(alg);

    ZeusCodeView.Event(alg, procedureName := "End1");
    T3(A, B, F, 1); At(81);
    T3(B, D, L, 1); At(82);
    RemT3(A, B, F);
    RemT3(B, D, L);
    Q4(A, B, F, G, 1); At(83);
    Q4(B, D, L, X, 1); At(84);
    ZeusCodeView.Exit(alg);

    ZeusCodeView.Event(alg, procedureName := "Symmetry");
    T3(A, C, K, 2); At(91);
    EuclidIE.Shear(alg, C, K, A, B);
    RemT3(A, C, K);
    T3(C, K, B, 2);
    EuclidIE.RotateTriangle(alg, C, K, A, B, E);
    RemT3(C, K, B);
    T3(A, C, E, 2);
    EuclidIE.Shear(alg, C, E, A, L);  At(92);
    RemT3(A, C, E);
    Q4(A, C, K, H, 2); At(93); 
    Q4(C, E, L, X, 2); At(94);
    ZeusCodeView.Exit(alg);
    ZeusCodeView.Event(alg, procedureName := "Rehash");
    At(100);
    At(101);
    ZeusCodeView.Exit(alg);
  END Run;

BEGIN
  ZeusPanel.RegisterAlg(New, "P47", "Euclid");
END EuclidAlg.
