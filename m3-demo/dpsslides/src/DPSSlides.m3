(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Tue Jan 18 14:55:55 PST 1994 by kalsow                   *)
(*      modified on Thu Nov 12 09:41:50 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

UNSAFE MODULE DPSSlides EXPORTS Main;

IMPORT DisplayList, DPS, OneSlideDLE, PagerDLE, Params, Process, Rd;
IMPORT ScaledDLWindow, Stdio, Text, Fmt, TextRd;
IMPORT Thread, TranslateDLE, Wr, WarpDLE;

(* CONST PointsPerPixel = 0.92182;  *)
(* CONST PixelsPerPoint = 1.0 / PointsPerPixel;  *)

CONST millisecond = 0.001d0;
CONST second      = 1000.0d0 * millisecond;
CONST minute      = 60.0d0 * second;
CONST hour        = 60.0d0 * minute;
CONST day         = 24.0d0 * hour;


CONST ScreenWidth = 1024; (* In pixels. *)
CONST ScreenHeight = 844; (* In pixels. *)

CONST ScreenAspectRatio = FLOAT(ScreenWidth) / FLOAT(ScreenHeight);

CONST DesiredWidth = 612.0;  (* Full 8.5 inch paper. *)
(* Margins cover unprintable edges. *)
CONST DesiredHeight = DesiredWidth / ScreenAspectRatio;

VAR GreyScaleDisplay: BOOLEAN := FALSE;
VAR scale, scale2, warp, translate, tall: BOOLEAN := FALSE;
VAR nervous: BOOLEAN := FALSE;
VAR quantized: BOOLEAN := FALSE;

PROCEDURE RdDotGetLine (r: Rd.T): Text.T RAISES {Rd.EndOfFile} = 
(* Rd.GetLine is buggy.  21aug91 *)
 <*FATAL Rd.Failure, Thread.Alerted*>
 VAR c: CHAR;
 VAR line: Text.T := "";
  BEGIN
  c := Rd.GetChar(r); (* Initial EOF propogates. *)
  IF c = '\n' THEN RETURN line; END;
  line := line & Text.FromChar (c);
  WHILE NOT Rd.EOF(r) DO
    c := Rd.GetChar(r);
    IF c = '\n' THEN RETURN line; END;
    line := line & Text.FromChar (c);
    END;
  RETURN line;
  END RdDotGetLine;
 
PROCEDURE BoxFromXYWH (x, y: REAL; w, h: REAL := 0.0): DPS.Box =
 VAR box: DPS.Box;
  BEGIN
  box.low.x := x; box.high.x := x + w;
  box.low.y := y; box.high.y := y + h;
  RETURN box;
  END BoxFromXYWH;

PROCEDURE MainProgram () =
 VAR window1: ScaledDLWindow.T; 
 VAR root: DisplayList.T; 
 VAR pager: PagerDLE.T; 
 VAR line, line2, text: Text.T;
 VAR scaleFactor: REAL;
  PROCEDURE NewPageFromReader (reader: Rd.T): DisplayList.T =
   VAR ret: DisplayList.T;
   VAR ole: OneSlideDLE.T;
   VAR wart: WarpDLE.T; 
   VAR xlate: TranslateDLE.T; 
   VAR initialY: REAL := 0.0; 
   BEGIN
    ole := NEW ( OneSlideDLE.T, 
      box := BoxFromXYWH (0.0, 0.0, DesiredWidth, DesiredHeight) );
    IF tall THEN
       ole.minimumHeight := DesiredHeight;
       ole.maximumHeight := 3.0 * DesiredHeight;
      ELSE 
       ole.minimumHeight := DesiredHeight;
       ole.maximumHeight := DesiredHeight;
      END;
    OneSlideDLE.Init (ole, window1, reader);
    ret := ole;
    IF warp THEN initialY := 390.0; END;
    IF translate THEN 
      xlate := NEW ( TranslateDLE.T, 
        translationX := 0.0, translationY := initialY, 
        fixedX := TRUE,
        onlyIfShifted := TRUE );
      xlate.Append (ret);
      ret := xlate;
      END;
    IF warp THEN 
      wart := NEW ( WarpDLE.T, 
        topWarpY := 700.0, 
        bottomWarpY := 200.0, 
        multiplierWarpY := 0.33333 );
      wart.Append (ret);
      ret := wart;
      END;
    RETURN ret;
    END NewPageFromReader;

  BEGIN
  window1 := NEW ( ScaledDLWindow.T, 
    displayList := NEW(DisplayList.R),
    alwaysNervous := nervous );
  window1.displayList.window := window1;
  window1.Create ( ScreenWidth, ScreenHeight, NOT GreyScaleDisplay, NIL );

  IF scale OR scale2 THEN
    window1.desiredWidth := DesiredWidth;
    window1.desiredHeight := DesiredHeight;
    IF quantized THEN
      window1.backgroundTransformationScaler := DPS.StandardFontPoints;
     ELSE 
      window1.backgroundTransformationScaler := 0.0;
      END;
    window1.backgroundTransformationMaintainsSimilarity := NOT scale2;
   ELSE
    scaleFactor := 0.964 * MIN ( FLOAT(ScreenWidth) / DesiredWidth, 
     FLOAT(ScreenHeight) / DesiredHeight );
    (* The constant is a heuristic. PointsPerPixel isn't quite right, *)
    (* perhaps due to window manager fudging things. *)
    DPS.SendClientTransformation ( window1,
    " " & Fmt.Real(scaleFactor) & " dup scale " );
    DPS.SendClientTransformation ( window1,
    " " & Fmt.Real(scaleFactor) & " dup scale 0.0 14.0 translate " );
    (* Translate is a cheap heuristic to fix the bottom fudge for 'fixed'. *)
    END;

  pager := NEW ( PagerDLE.T );
  root := pager;

  text := ""; (* Splitting input into pages. *)
  LOOP
    TRY
      line := RdDotGetLine (Stdio.stdin);
      IF Text.Equal(Text.Sub(line,0,2),"%!") THEN 
        line2 := RdDotGetLine (Stdio.stdin);
        WHILE NOT Text.Equal(Text.Sub(line2,0,9),"%%Trailer") DO
          line := line & " " & line2;
          line2 := RdDotGetLine (Stdio.stdin);
          END;
        END;
      IF Text.Equal (Text.Sub(line,0,5),"/page") THEN
        pager.AppendPage ( NewPageFromReader (TextRd.New(text)) );
        text := "";
       ELSIF Text.Empty(text) THEN text := line;
       ELSE text := text & "\n" & line;
        END;
     EXCEPT Rd.EndOfFile =>
      IF NOT Text.Empty(text) THEN 
        pager.AppendPage ( NewPageFromReader (TextRd.New(text)) );
        END;
      EXIT;
      END; (* of TRY *)
    END; (* of LOOP *)


 window1.displayList.Append (root);

  (* ScaledDLWindow.InstallButtons(window1); *)

  window1.Dirty (DPS.EverywhereBox, NIL);

  LOOP
    Thread.Pause (day);
    END;

  END MainProgram;  

PROCEDURE Usage () =
  <*FATAL ANY*>
  BEGIN
    Wr.PutText (Stdio.stderr, "usage: dpsslides [-xlate] [-warp] [-tall]\n");
    Process.Exit (1);
  END Usage;

PROCEDURE GetParams () = 
 VAR s, f, s2, helv: BOOLEAN := FALSE;  arg: TEXT;
  BEGIN
    FOR i := 1 TO Params.Count-1 DO
      arg := Params.Get (i);
      IF    Text.Equal (arg, "-warp")      THEN warp := TRUE;
      ELSIF Text.Equal (arg, "-tall")      THEN tall := TRUE;
      ELSIF Text.Equal (arg, "-xlate")     THEN translate := TRUE;
      ELSIF Text.Equal (arg, "-grey")      THEN GreyScaleDisplay := TRUE;
      ELSIF Text.Equal (arg, "-gray")      THEN GreyScaleDisplay := TRUE;
      ELSIF Text.Equal (arg, "-helvetica") THEN helv := TRUE;
      ELSIF Text.Equal (arg, "-Helvetica") THEN helv := TRUE;
      ELSIF Text.Equal (arg, "-nervous")   THEN nervous := TRUE;
      ELSIF Text.Equal (arg, "-fixed")     THEN f := TRUE;
      ELSIF Text.Equal (arg, "-f")         THEN f := TRUE;
      ELSIF Text.Equal (arg, "-scale")     THEN s := TRUE;
      ELSIF Text.Equal (arg, "-s")         THEN s := TRUE;
      ELSIF Text.Equal (arg, "-scale2")    THEN s2 := TRUE;
      ELSIF Text.Equal (arg, "-s2")        THEN s2 := TRUE;
      ELSE  Usage ();
      END;
    END;

    IF helv THEN DPS.SetPreferredFontName ("Helvetica"); END;

    IF f      THEN scale := FALSE; scale2 := FALSE;
    ELSIF s2  THEN scale := TRUE;  scale2 := TRUE;
    ELSIF s   THEN scale := TRUE;  scale2 := FALSE;
    ELSE           scale := TRUE;  scale2 := FALSE; (* Defaults. *)
    END;
  END GetParams;


  BEGIN  

  Thread.IncDefaultStackSize (65535 - 3000);
  GetParams ();
  MainProgram ();

  END DPSSlides.

