MODULE Main;

IMPORT LongRealPLPlot AS PL;
IMPORT LongRealTrans AS RT, LongRealVector AS V;
IMPORT IO, Fmt;


PROCEDURE TestHistogram () =
  VAR
    data  := NEW(REF ARRAY OF RT.T, 2047);
    delta := RT.TwoPi / FLOAT(NUMBER(data^), RT.T);
  BEGIN
    (* Fill up data points *)
    FOR i := 0 TO LAST(data^) DO
      data[i] := RT.Sin(FLOAT(i, RT.T) * delta);
    END;

    PL.SetFGColorDiscr(1);
    PL.PlotHistogram(data^, -1.1D0, 1.1D0, 44);
    PL.SetFGColorDiscr(2);
    PL.SetLabels(
      "#frValue", "#frFrequency", "#frProbability function of Oscillator");
  END TestHistogram;

PROCEDURE TestCurve () =
  BEGIN
    (* Initialize plplot *)
    PL.SetEnvironment(-RT.TwoPi, RT.TwoPi, -1.1D0, 1.1D0);

    VAR
      x     := NEW(REF ARRAY OF RT.T, 2047);
      y     := NEW(REF ARRAY OF RT.T, 2047);
      delta := RT.Two * RT.TwoPi / FLOAT(NUMBER(x^), RT.T);
    BEGIN
      (* Fill up data points *)
      FOR i := 0 TO LAST(x^) DO
        x[i] := FLOAT(i, RT.T) * delta - RT.TwoPi;
        y[i] := RT.Sin(x[i]);
      END;

      PL.SetFGColorDiscr(1);
      PL.PlotLines(x^, y^);
    END;

    VAR
      x           := NEW(REF ARRAY OF RT.T, 15);
      y           := NEW(REF ARRAY OF RT.T, 15);
      t    : RT.T;
      delta       := RT.TwoPi / FLOAT(NUMBER(x^), RT.T);
    BEGIN
      (* Fill up data points *)
      FOR i := 0 TO LAST(x^) DO
        t := FLOAT(i, RT.T) * delta;
        x[i] := RT.Cos(t) * 5.0D0;
        y[i] := RT.Sin(t);
        PL.SetFGColorDiscr(i + 1);
        PL.PlotPoints(SUBARRAY(x^, i, 1), SUBARRAY(y^, i, 1), i);
      END;
      (*
          PL.SetFGColorDiscr(2);
          PL.PlotPoints(x^,y^,0);
      *)
    END;

    PL.SetFGColorDiscr(3);
    PL.SetLabels("#frTime", "#frElongation", "#frHarmonic Oscillator");
  END TestCurve;

PROCEDURE TestViewPort () =
  VAR
    x       := V.ArithSeq(1000, -RT.TwoPi, RT.TwoPi / 500.0D0);
    y       := NEW(V.T, 1000);
    chr     := PL.GetCharacterHeight();
    bnd     := PL.GetBoundaries();
    ycenter := (bnd.ymin + bnd.ymax) * 0.5D0;

  CONST
    flagsX = PL.DirTileSet{PL.DirTile.LowerBorder, PL.DirTile.UpperBorder,
                           PL.DirTile.TickLabelsConv,
                           PL.DirTile.TicksMajor, PL.DirTile.TicksMinor};
    flagsY = flagsX + PL.DirTileSet{PL.DirTile.LabelBaseParallel};
    relVP  = FALSE;

  BEGIN
    PL.AdvanceSubPage();
    (*PL.StartPage();*)

    IF relVP THEN
      PL.CreateVP(0.1D0, 1.0D0, 0.05D0, 0.5D0);
    ELSE
      PL.SetVPAbsolute(bnd.xmin + chr.ht * 5.0D0, bnd.xmax,
                       bnd.ymin + chr.ht * 3.0D0, ycenter);
    END;
    PL.SetWindow(-RT.TwoPi, RT.TwoPi, -1.1D0, 1.1D0);
    PL.SetFGColorDiscr(1);
    PL.DrawBox(flagsX, 0.0D0, 0, flagsY, 0.0D0, 0);
    FOR i := FIRST(x^) TO LAST(x^) DO y[i] := RT.Sin(x[i]); END;
    PL.SetFGColorDiscr(2);
    PL.PlotLines(x^, y^);

    IF relVP THEN
      PL.CreateVP(0.1D0, 1.0D0, 0.55D0, 1.0D0);
    ELSE
      PL.SetVPAbsolute(bnd.xmin + chr.ht * 5.0D0, bnd.xmax,
                       ycenter + chr.ht * 3.0D0, bnd.ymax);
    END;
    PL.SetWindow(-RT.TwoPi, RT.TwoPi, -1.1D0, 1.1D0);
    PL.SetFGColorDiscr(1);
    PL.DrawBox(flagsX, 0.0D0, 0, flagsY, 0.0D0, 0);
    FOR i := FIRST(x^) TO LAST(x^) DO y[i] := RT.Cos(x[i]); END;
    PL.SetFGColorDiscr(2);
    PL.PlotLines(x^, y^);

    (*PL.StopPage();*)
  END TestViewPort;

BEGIN
  (* Parse and process command line arguments *)
  (*
  EVAL PLP.ParseOptions(&argc, argv, PL.Parse.Full);
  *)

  PL.SetDevice("psc");
 (* IO.Put(
    "SetOption returned " & Fmt.Int(PL.SetOption("-portrait", "")) & "\n"); *)
  PL.SetFileName("test.eps");
  (* PL.SetPageParam(0.0D0, 0.0D0, 1000, 500, 0, 0); ignored by the
     driver *)
  PL.SetOrientation(0); (* has no effect if postscript option
                                    -portrait is set *)
  PL.Init();
  PL.LoadFont(PL.CharacterSet.Extended);
  PL.SetFont(PL.FontType.Roman);
  PL.SetCharacterHeight(0.0D0, 1.0D0); (* seems to affect only the next
                                          page and only labels at the plot
                                          but not the numbers at the
                                          axes *)
  (*PL.SetSymbolHeight(0.0D0, 2.0D0);*)

  WITH hei = PL.GetCharacterHeight() DO
    IO.Put("Character height: " & Fmt.LongReal(hei.def) & " "
             & Fmt.LongReal(hei.ht) & "\n");
  END;
  TestHistogram();
  TestCurve();
  TestViewPort();
  PL.Exit();
END Main.
