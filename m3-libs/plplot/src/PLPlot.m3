UNSAFE MODULE PLPlot;
(*Copyright (c) 1996, m3na project

*)

IMPORT M3toC,
       (*Ctypes AS C,*)
       LongRealBasic AS R,
       PLPlotRaw     AS PL;

<*UNUSED*> CONST Module = "PLPlot.";
(*==========================*)

PROCEDURE Init() = BEGIN PL.plinit() END Init;

PROCEDURE Exit() = BEGIN PL.plend() END Exit;

PROCEDURE SetColorMap0(icol0:ColorMap) =
  BEGIN PL.plcol0(icol0) END SetColorMap0;

PROCEDURE Histogram(READONLY data:ARRAY OF R.T;
                             datamin, datamax:R.T;
                             numbin:CARDINAL;
                             oldwin:CARDINAL:=0) =
  BEGIN
    PL.plhist(NUMBER(data),data[0],datamin,datamax,numbin,oldwin);
  END Histogram;

PROCEDURE SetLabel(xlabel, ylabel, tlabel : TEXT) =
  VAR
    xlabelcs := M3toC.SharedTtoS(xlabel);
    ylabelcs := M3toC.SharedTtoS(ylabel);
    tlabelcs := M3toC.SharedTtoS(tlabel);
  BEGIN
    PL.pllab(xlabelcs,ylabelcs,tlabelcs);
    M3toC.FreeSharedS(xlabel,xlabelcs);
    M3toC.FreeSharedS(ylabel,ylabelcs);
    M3toC.FreeSharedS(tlabel,tlabelcs);
  END SetLabel;

(*==========================*)
BEGIN
END PLPlot.
