UNSAFE MODULE PLPlot;
(*Copyright (c) 1996, m3na project

   *)

IMPORT M3toC;
(*IMPORT Ctypes AS C,*)
IMPORT LongRealBasic AS R;
IMPORT PLPlotRaw AS PL;
FROM NADefinitions IMPORT Error, Err;

<*UNUSED*>
CONST Module = "PLPlot.";
(*==========================*)

PROCEDURE Init () =
  BEGIN
    PL.plinit()
  END Init;

PROCEDURE Exit () =
  BEGIN
    PL.plend()
  END Exit;

PROCEDURE SubPlots (nx, ny: CARDINAL) =
  BEGIN
    PL.plssub(nx, ny);
  END SubPlots;

PROCEDURE SetEnvironment (xmin, xmax, ymin, ymax: R.T;
                          just                  : CARDINAL := 0;
                          axis                  : CARDINAL := 0  ) =
  BEGIN
    PL.plenv(xmin, xmax, ymin, ymax, just, axis);
  END SetEnvironment;

PROCEDURE Clear () =
  BEGIN
    PL.plclear();
  END Clear;

PROCEDURE Advance (page: INTEGER) =
  BEGIN
    PL.pladv(page);
  END Advance;

PROCEDURE SetXORMode (mode: BOOLEAN): BOOLEAN =
  VAR success: PL.PLINT;
  BEGIN
    PL.plxormod(ORD(mode), success);
    RETURN success # 0;
  END SetXORMode;

PROCEDURE SetColor0 (icol0: Map0Color) =
  BEGIN
    PL.plcol0(icol0)
  END SetColor0;

PROCEDURE SetColor1 (icol1: Map1Color) =
  BEGIN
    PL.plcol1(icol1)
  END SetColor1;

PROCEDURE SetLabel (xlabel, ylabel, tlabel: TEXT) =
  VAR
    xlabelcs := M3toC.SharedTtoS(xlabel);
    ylabelcs := M3toC.SharedTtoS(ylabel);
    tlabelcs := M3toC.SharedTtoS(tlabel);
  BEGIN
    PL.pllab(xlabelcs, ylabelcs, tlabelcs);
    M3toC.FreeSharedS(xlabel, xlabelcs);
    M3toC.FreeSharedS(ylabel, ylabelcs);
    M3toC.FreeSharedS(tlabel, tlabelcs);
  END SetLabel;


PROCEDURE PlotPoints (READONLY x, y: ARRAY OF R.T; code: CARDINAL)
  RAISES {Error} =
  BEGIN
    IF NUMBER(x) # NUMBER(y) THEN RAISE Error(Err.bad_size); END;
    PL.plpoin(NUMBER(x), x[0], y[0], code);
  END PlotPoints;

PROCEDURE PlotLines (READONLY x, y: ARRAY OF R.T) RAISES {Error} =
  BEGIN
    IF NUMBER(x) # NUMBER(y) THEN RAISE Error(Err.bad_size); END;
    PL.plline(NUMBER(x), x[0], y[0]);
  END PlotLines;

PROCEDURE Histogram (READONLY data            : ARRAY OF R.T;
                              datamin, datamax: R.T;
                              numbin          : CARDINAL;
                              oldwin          : CARDINAL       := 0) =
  BEGIN
    PL.plhist(NUMBER(data), data[0], datamin, datamax, numbin, oldwin);
  END Histogram;

(*==========================*)
BEGIN
END PLPlot.
