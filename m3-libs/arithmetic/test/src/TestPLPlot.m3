UNSAFE MODULE TestPLPlot EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for PLPlot module.

1/1/96    <name>   Initial version

*)

IMPORT PLPlotRaw     AS PLP,
       Ctypes        AS C,
       M3toC,
       LongRealTrans AS RT;

(*=======================*)
CONST
  Module = "TestPLPlot.";
(*----------------------*)
PROCEDURE TestHistogram():BOOLEAN=
CONST
  ftn = Module & "TestABC";

CONST
  NPTS = 2047;
VAR
  result:=TRUE;
  data:=NEW(REF ARRAY OF PLP.PLFLT,NPTS);
  delta:=RT.TwoPi / FLOAT (NPTS,PLP.PLFLT);
BEGIN
  Debug(1,ftn,"begin\n");

  (* Parse and process command line arguments *)
  (*
  EVAL PLP.plParseOpts(&argc, argv, PLP.PL_PARSE_FULL);
  *)

  (* Initialize plplot *)
  PLP.plinit();

  (* Fill up data points *)
  FOR i:=0 TO LAST(data^) DO
    data[i] := RT.Sin(FLOAT(i,RT.T) * delta);
  END;

  PLP.plcol0(1);
  PLP.plhist(NPTS, data[0], -1.1D0, 1.1D0, 44, 0);
  PLP.plcol0(2);
  PLP.pllab(M3toC.CopyTtoS("#frValue"),
            M3toC.CopyTtoS("#frFrequency"),
	          M3toC.CopyTtoS("#frPLplot Example 5 - Probability function of Oscillator"));

  PLP.plend();

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
