MODULE TestPLPlot EXPORTS Test;
(*Copyright (c) 1996, m3na project

   Abstract: Tests for PLPlot module. *)

IMPORT PLPlot AS PL;
IMPORT LongRealTrans AS RT;

IMPORT NADefinitions AS NA;

(*=======================*)
CONST Module = "TestPLPlot.";
(*----------------------*)
PROCEDURE TestHistogram (): BOOLEAN =
  CONST ftn = Module & "TestHistogram";

  VAR
    result := TRUE;
    data   := NEW(REF ARRAY OF RT.T, 2047);
    delta  := RT.TwoPi / FLOAT(NUMBER(data^), RT.T);
  BEGIN
    Debug(1, ftn, "begin\n");

    (* Parse and process command line arguments *)
    (*
    EVAL PLP.plParseOpts(&argc, argv, PLP.PL_PARSE_FULL);
    *)

    (* Fill up data points *)
    FOR i := 0 TO LAST(data^) DO
      data[i] := RT.Sin(FLOAT(i, RT.T) * delta);
    END;

    PL.SetFGColorDiscr(1);
    PL.PlotHistogram(data^, -1.1D0, 1.1D0, 44);
    PL.SetFGColorDiscr(2);
    PL.SetLabels(
      "#frValue", "#frFrequency", "#frProbability function of Oscillator");

    RETURN result;
  END TestHistogram;
(*----------------------*)
PROCEDURE TestCurve (): BOOLEAN =
  CONST ftn = Module & "TestCurve";

  VAR result := TRUE;
  <*FATAL NA.Error*>
  BEGIN
    Debug(1, ftn, "begin\n");

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

    RETURN result;
  END TestCurve;
(*-------------------------*)
PROCEDURE TestPLPlot (): BOOLEAN =
  <*UNUSED*>
  CONST ftn = Module & "TestPLPlot";
  VAR result := TRUE;
  BEGIN
    PL.Init();
    NewLine();
    EVAL TestHistogram();
    NewLine();
    EVAL TestCurve();
    PL.Exit();
    RETURN result;
  END TestPLPlot;
(*=======================*)
BEGIN
END TestPLPlot.
