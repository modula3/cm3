MODULE TestPLPlot EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for PLPlot module.

1/1/96    <name>   Initial version

*)

IMPORT PLPlot        AS PL,
       LongRealTrans AS RT;

(*=======================*)
CONST
  Module = "TestPLPlot.";
(*----------------------*)
PROCEDURE TestHistogram():BOOLEAN=
CONST
  ftn = Module & "TestABC";

VAR
  result:=TRUE;
  data:=NEW(REF ARRAY OF RT.T,2047);
  delta:=RT.TwoPi / FLOAT (NUMBER(data^),RT.T);
BEGIN
  Debug(1,ftn,"begin\n");

  (* Parse and process command line arguments *)
  (*
  EVAL PLP.plParseOpts(&argc, argv, PLP.PL_PARSE_FULL);
  *)

  (* Initialize plplot *)
  PL.Init();

  (* Fill up data points *)
  FOR i:=0 TO LAST(data^) DO
    data[i] := RT.Sin(FLOAT(i,RT.T) * delta);
  END;

  PL.SetColorMap0(1);
  PL.Histogram(data^, -1.1D0, 1.1D0, 44);
  PL.SetColorMap0(2);
  PL.SetLabel("#frValue",
              "#frFrequency",
	            "#frPLplot Example 5 - Probability function of Oscillator");

  PL.Exit();

  RETURN result;
END TestHistogram;
(*-------------------------*)
PROCEDURE TestPLPlot():BOOLEAN=
CONST ftn = Module & "TestPLPlot";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestHistogram();
  RETURN result;
END TestPLPlot;
(*=======================*)
BEGIN
END TestPLPlot.
