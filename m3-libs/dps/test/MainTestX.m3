(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Mon Feb 10 18:05:47 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)


UNSAFE MODULE MainTestX EXPORTS Main;

IMPORT ButtonDLE, DisplayList, DLWindow, DPS, DPSWindow, 
 HContainerDLE, ParseParams,
 PopupButtonDLE, ScaledDLWindow, Scan, SimpleTextDLE, Stdio, 
 Text, Text2, TextLineDLE, Thread, Time, Unique, VContainerDLE, Wr, cDPS;

CONST second = 1000000;

CONST DesiredWidth = 400;
CONST DesiredHeight = 400;

VAR GreyScaleDisplay: BOOLEAN;
VAR PlaneWindowCount: INTEGER;

VAR scale, scale2: BOOLEAN;

TYPE BoxObject = DisplayList.E OBJECT box: DPS.Box; OVERRIDES Repaint := MyRepaintBox; END; 

TYPE BlobObject = DisplayList.E OBJECT 
  hue, sat: REAL := 0.4999; bri: REAL := 1.0; 
  hot: BOOLEAN := FALSE;
  places: REF ARRAY OF DPS.Place;
  pathNameNoU, foundation: Text.T := NIL;
  pathName: Text.T := NIL; pathChars: ARRAY [0..11] OF CHAR; 
  (* Big enough for A1234567890 plus '000' *)
 OVERRIDES Repaint := MyRepaintBlob; Mouse := MyMouseBlob;
  END; 

PROCEDURE MyRepaintBlob (e: BlobObject; box: DPS.Box; only: REFANY): Text.T =
 VAR hueText, satText, briText: Text.T := "";
 VAR colorText: Text.T := "";
  BEGIN
  IF NOT DPS.BoxesIntersect (box, e.box) THEN RETURN NIL; END;
  IF GreyScaleDisplay THEN
    hueText := Text2.FromReal(e.hue);
    colorText := hueText & " " & hueText & " " & hueText & " setrgbcolor";
   ELSE 
    hueText := Text2.FromReal(e.hue);
    satText := Text2.FromReal(e.sat);
    briText := Text2.FromReal(e.bri);
    colorText := hueText & " " & satText & " " & briText & " sethsbcolor";
    END;
  RETURN DPS.GSaveAndClip(box) & e.pathNameNoU 
   & " " & colorText & " fill " & DPS.GRestore();
  END MyRepaintBlob;

PROCEDURE CalculateBlobPath (e: BlobObject; window: DPSWindow.T) =
 VAR bbox, path: Text.T := "";
  BEGIN
    bbox := " " & Text2.FromReal (e.box.low.x)
     & " " & Text2.FromReal (e.box.low.y)
     & " " & Text2.FromReal (e.box.high.x) 
     & " " & Text2.FromReal (e.box.high.y) & " setbbox ";
    (* Begining "path" with a "newpath" gives a complaint from *)
    (* the X/DPS server "typecheck: Offending Command: inufill" *)
    (* which is peculiar, as there isn't a 'inufill' anywhere in *.m3 *)
    (* rma 23mar91 *)
    path := " " & Text2.FromReal (e.places^[0].x)
     & " " & Text2.FromReal (e.places^[0].y) & " moveto ";
    FOR m := 1 TO NUMBER(e.places^)-1 DO
      path := path & " " & Text2.FromReal (e.places^[m].x)
       & " " & Text2.FromReal (e.places^[m].y) & " lineto ";
      END;
  e.pathName := Unique.Identifier(); Text2.ToArray (e.pathChars, e.pathName);
  e.pathNameNoU := Unique.Identifier();
  e.foundation := " /" & e.pathNameNoU & " { " & path & " } def ";
  window.SendFoundation (e.foundation);
  window.SendSpecialFoundation (" /" & e.pathName & " { ucache " 
   & bbox & path & " } cvlit def ");
  END CalculateBlobPath;

PROCEDURE MyRepaintBox (e: BoxObject; box: DPS.Box; only: REFANY): Text.T =
  BEGIN
  IF NOT DPS.BoxesIntersect (box, e.box) THEN RETURN NIL; END;
  RETURN " newpath " 
   & Text2.FromReal(e.box.low.x) & " " & Text2.FromReal(e.box.low.y) 
   & " moveto " 
   & Text2.FromReal(e.box.low.x) & " " & Text2.FromReal(e.box.high.y) 
   & " lineto "
   & Text2.FromReal(e.box.high.x) & " " & Text2.FromReal(e.box.high.y)
   & " lineto "
   & Text2.FromReal(e.box.high.x) & " " & Text2.FromReal(e.box.low.y) 
   & " lineto "
   & " closepath 1.0 0.0 0.0 setrgbcolor fill ";
  END MyRepaintBox;

VAR recentDownX, recentDownY: REAL;
VAR recentHue, recentSat, recentBri: REAL;

PROCEDURE MyMouseBlob (e: BlobObject; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR int: INTEGER;
 VAR newHue, newSat, newBri: REAL;
 VAR affected: DisplayList.E;
 VAR dirtyBox: DPS.Box;
  BEGIN
  IF e.hot THEN
    IF (event.clickType = DPS.ClickType.LastUp) THEN
     e.hot := FALSE; RETURN TRUE;
      END;
    IF (event.whatChanged = DPS.Button.Left)
     AND (event.clickType = DPS.ClickType.Dragging) THEN
      newHue := recentHue + (event.place.y - recentDownY) / 200.0;
      newHue := MAX (newHue, 0.0); newHue := MIN (newHue, 1.0);
      newSat := recentSat + (event.place.x - recentDownX) / 200.0;
      newSat := MAX (newSat, 0.0); newSat := MIN (newSat, 1.0);
      IF ABS(newSat - e.sat) > 0.01 OR ABS(newHue - e.hue) > 0.01 THEN
        e.hue := newHue; e.sat := newSat;
        e.Dirty (e.box, e);
        END;
      END;
    IF (event.whatChanged = DPS.Button.Middle)
     AND (event.clickType = DPS.ClickType.Dragging) THEN
      newBri := recentBri + (event.place.y - recentDownY) / 200.0;
      newBri := MAX (newBri, 0.0); newBri := MIN (newBri, 1.0);
      newSat := recentSat + (event.place.x - recentDownX) / 200.0;
      newSat := MAX (newSat, 0.0); newSat := MIN (newSat, 1.0);
      IF ABS(newBri - e.bri) > 0.01 OR ABS(newSat - e.sat) > 0.01 THEN
        e.sat := newSat; e.bri := newBri;
        e.Dirty (e.box, e);
        END;
      END;
    RETURN TRUE;
    END;
  IF NOT DPS.PlaceIsInBox (event.place, e.box) THEN RETURN FALSE; END;
  int := cDPS.xyupathhit (
   window.ctx, event.place.x, event.place.y, ADR(e.pathChars[0]) );
  IF int = 0 THEN RETURN FALSE; END;
  IF (event.whatChanged # DPS.Button.Right)
   AND (event.clickType = DPS.ClickType.FirstDown) THEN
    e.hot := TRUE;
    recentDownX := event.place.x;
    recentDownY := event.place.y;
    recentHue := e.hue;
    recentSat := e.sat;
    recentBri := e.bri;
    affected := e.MoveToLast();
    IF affected # NIL THEN
      affected.Dirty (affected.box, affected); 
      END;
    END;
  IF (event.whatChanged = DPS.Button.Right)
   AND (event.clickType = DPS.ClickType.LastUp) THEN
    dirtyBox := e.box;
    e.Remove();
    window.Dirty(dirtyBox, NIL);
    END;
  RETURN TRUE;
  END MyMouseBlob;

VAR clicks: ARRAY [0..99] OF BoxObject;
VAR clickCount: INTEGER := 0;

CONST clickDisplayWidth = 2.0;

PROCEDURE MyMouse (t: DLWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR e: BoxObject;
 VAR blob: BlobObject;
  BEGIN 
  IF DLWindow.Mouse (t, event) THEN RETURN TRUE; END;
  IF event.clickType # DPS.ClickType.FirstDown THEN RETURN FALSE; END;
  (* We do it on the down. Otherwise we process an up where the down *)
  (* occurred over a button, and the user dragged off. That not what *)
  (* the user expects. *)
  CASE event.whatChanged OF
  | DPS.Button.Left => 
    IF clickCount < 99 THEN
      clicks[clickCount] := NEW ( BoxObject,
       box := DPS.Box { 
        DPS.Place{event.place.x,event.place.y}, 
        DPS.Place{event.place.x+clickDisplayWidth,event.place.y+clickDisplayWidth} } );
      t.displayList.Append (clicks[clickCount]);
      e := clicks[clickCount];
      e.Dirty (clicks[clickCount].box, NIL);
      clickCount := clickCount + 1;
      END;
  | DPS.Button.Middle => 
    IF clickCount < 99 THEN
      clicks[clickCount] := NEW ( BoxObject,
       box := DPS.Box { 
        DPS.Place{event.place.x,event.place.y}, 
        DPS.Place{event.place.x+clickDisplayWidth,event.place.y+clickDisplayWidth} } );
      t.displayList.Append (clicks[clickCount]);
      clickCount := clickCount + 1;
      END;
    FOR m := 0 TO clickCount-1 DO
      clicks[m].Remove ();
      t.Dirty(clicks[m].box, NIL); (* Or should we do it all at once? *)
      END;
    blob := NEW ( BlobObject,
     places := NEW (REF ARRAY OF DPS.Place, clickCount),
     box := DPS.Box { 
      DPS.Place{event.place.x,event.place.y},
      DPS.Place{event.place.x+clickDisplayWidth,event.place.y+clickDisplayWidth} } );
    (* Hoping that the recent ^^ one got into the list. *)
    FOR m := 0 TO clickCount-1 DO
      blob.box.low.x := MIN (blob.box.low.x, clicks[m].box.low.x);
      blob.box.low.y := MIN (blob.box.low.y, clicks[m].box.low.y);
      blob.box.high.x := MAX (blob.box.high.x, clicks[m].box.high.x);
      blob.box.high.y := MAX (blob.box.high.y, clicks[m].box.high.y);
      blob.places^[m] := clicks[m].box.low;
      END;
    CalculateBlobPath (blob, t);
    t.displayList.Append (blob);
    clickCount := 0;
    blob.Dirty (blob.box, blob);
  | DPS.Button.Right => 
    END; (* of CASE *)
  RETURN TRUE;
  END MyMouse;

PROCEDURE FirstProc (i: PopupButtonDLE.Item) =
  BEGIN
  END FirstProc;

PROCEDURE SecondProc (i: PopupButtonDLE.Item) =
  BEGIN
  Wr.PutText (Stdio.stdout, "Called SecondProc.\n");
  END SecondProc;

PROCEDURE ThirdProc (i: PopupButtonDLE.Item) =
  BEGIN
  END ThirdProc;

PROCEDURE BoxFromXYWH (x, y: REAL; w, h: REAL := 0.0): DPS.Box =
 VAR box: DPS.Box;
  BEGIN
  box.low.x := x; box.high.x := x + w;
  box.low.y := y; box.high.y := y + h;
  RETURN box;
  END BoxFromXYWH;

PROCEDURE MainProgram () =
 VAR window1, window2, window3: ScaledDLWindow.T;
 VAR tle: TextLineDLE.E;
 VAR ste: SimpleTextDLE.E;
 VAR pbe: PopupButtonDLE.E;
 VAR vce: VContainerDLE.T;
 VAR hce: HContainerDLE.T;
  BEGIN
  window1 := NEW ( ScaledDLWindow.T, Mouse := MyMouse, displayList := NEW(DisplayList.R) );
  window1.displayList.window := window1;

  IF GreyScaleDisplay AND (PlaneWindowCount > 1) THEN
    window1.Create ( DesiredWidth, DesiredHeight, NOT GreyScaleDisplay, window1 );
   ELSE window1.Create ( DesiredWidth, DesiredHeight, NOT GreyScaleDisplay, NIL );
    END;

  IF scale OR scale2 THEN
    window1.desiredWidth := FLOAT (DesiredWidth);
    window1.desiredHeight := FLOAT (DesiredHeight);
    window1.backgroundTransformationScaler := DPS.StandardFontPoints; 
    (* Perhaps the default. *)
    window1.backgroundTransformationMaintainsSimilarity := NOT scale2;
    END;

  ste := NEW ( SimpleTextDLE.E,
   box := BoxFromXYWH (50.0, 90.0), 
   text := "This is two lines of text in\n" & "a -SimpleTextDLE- element.\n" );
  SimpleTextDLE.Init (ste, window1);
  window1.displayList.Append (ste);

  pbe := NEW ( PopupButtonDLE.E,  
   box := BoxFromXYWH (100.0, 220.0), 
   text := "Popup Test" );
  PopupButtonDLE.Init (pbe, window1);
  pbe.items := NEW (REF ARRAY OF PopupButtonDLE.Item, 3);
  pbe.items^[0] := NEW (PopupButtonDLE.Item, text := "first", Proc := FirstProc);
  pbe.items^[1] := NEW (PopupButtonDLE.Item, text := "second", Proc := SecondProc);
  pbe.items^[2] := NEW (PopupButtonDLE.Item, text := "third", Proc := ThirdProc);
  window1.displayList.Append (pbe);

  tle := NEW ( TextLineDLE.E,
   box := BoxFromXYWH ( 150.0, 55.0, 100.0 ),
   box := DPS.Box { DPS.Place {150.0, 55.0}, DPS.Place {250.0, -1.0} }, 
   text := "Hint" );
  TextLineDLE.Init (tle, window1);
  window1.displayList.Append (tle);

  tle := NEW ( TextLineDLE.E,
   box := BoxFromXYWH (250.0, 55.0), 
   text := "Sized to this" );
  TextLineDLE.Init (tle,window1);
  window1.displayList.Append (tle);

  vce := NEW ( VContainerDLE.E, box := BoxFromXYWH (300.0, 220.0) );
    tle := NEW ( TextLineDLE.E,
     box := BoxFromXYWH (0.0, 0.0), 
     text := "First Text" );
    TextLineDLE.Init (tle,window1);
    vce.Append (tle);
    tle := NEW ( TextLineDLE.E,
     box := BoxFromXYWH (0.0, 0.0), 
     text := "Second Text" );
    TextLineDLE.Init (tle, window1);
    vce.Append (tle);
  VContainerDLE.Init (vce, window1);
  IF vce.box.high.x > FLOAT(DesiredWidth) THEN
    vce.box.low.x := vce.box.low.x - (vce.box.high.x - FLOAT(DesiredWidth));
    vce.box.high.x := FLOAT(DesiredWidth);
    vce.Rearrange();
    END;
  window1.displayList.Append (vce);

  hce := NEW ( HContainerDLE.E, box := BoxFromXYWH (100.0, 260.0) );
    tle := NEW ( TextLineDLE.E,
     box := BoxFromXYWH (0.0, 0.0), 
     text := "Left Text" );
    TextLineDLE.Init (tle,window1);
    hce.Append (tle);
    tle := NEW ( TextLineDLE.E,
     box := BoxFromXYWH (0.0, 0.0), 
     text := "Right Text" );
    TextLineDLE.Init (tle,window1);
    hce.Append (tle);
  window1.displayList.Append (hce);

  IF GreyScaleDisplay AND (PlaneWindowCount > 1) THEN
    window2 := NEW ( ScaledDLWindow.T, Mouse := MyMouse, displayList := NEW(DisplayList.T) );
    window2.Create ( DesiredWidth, DesiredHeight, NOT GreyScaleDisplay, window1 );
    END;

  IF GreyScaleDisplay AND (PlaneWindowCount > 2) THEN
    window3 := NEW ( ScaledDLWindow.T, Mouse := MyMouse, displayList := NEW(DisplayList.T) );
   
    window3.Create ( DesiredWidth, DesiredHeight, NOT GreyScaleDisplay, window2 );
    END;

  ScaledDLWindow.InstallButtons(window1);

  window1.Dirty (DPS.EverywhereBox, NIL);

  LOOP
    Time.Pause (10000 * second);
    END;

  END MainProgram;

PROCEDURE GetParams () = 
  BEGIN
  TRY       
    ParseParams.BeginParsing(Stdio.stderr);                         
    IF ParseParams.KeywordPresent("-planes") THEN  
      PlaneWindowCount := ParseParams.GetNextInt (1,3); 
     ELSIF ParseParams.KeywordPresent("-p") THEN  
      PlaneWindowCount := ParseParams.GetNextInt (1,3); 
     ELSE PlaneWindowCount := 1;  
      END;                                                            
    IF ParseParams.KeywordPresent("-grey")
     OR ParseParams.KeywordPresent("-gray") 
     OR ParseParams.KeywordPresent("-g") THEN
      GreyScaleDisplay := TRUE;  
     ELSE GreyScaleDisplay := FALSE;  
      END;           
    scale := ParseParams.KeywordPresent("-scale") OR ParseParams.KeywordPresent("-s");  
    scale2 := ParseParams.KeywordPresent("-scale2") OR ParseParams.KeywordPresent("-s2");  
    ParseParams.UnparsedTail();    
    ParseParams.EndParsing();   
   EXCEPT Scan.BadFormat => RETURN;    
    END;  
  END GetParams;


  BEGIN
  (* Thread.IncDefaultStackSize (65535 - 3000); *)
  GetParams ();
  MainProgram ();
  END MainTestX.

