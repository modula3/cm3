(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:14:57 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:17 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE SlideLineDLE;

IMPORT DisplayList, DPS, DPSWindow, Linked2Tree;
IMPORT Text, Err, Fmt, wraps;

CONST aboveLeadingDefault = 8.0;
CONST heightDefault = 30.0;
CONST belowLeadingDefault = 0.0;
CONST leftLeadingDefault = 0.0;
CONST pointsDefault = 30.0;

CONST fontDescender = 2.0;

CONST PointsBump = 4.0;

CONST BumpX = 9.0; (* BumpY = 9.0; *)
(* CONST ReturnBoxSeparationY = 36.0; *)

PROCEDURE LooksLikePostscript (t: TEXT): BOOLEAN =
  BEGIN
  RETURN Text.Equal (Text.Sub(t,0,2), "%!");
  END LooksLikePostscript;

PROCEDURE PostscriptContent (t: TEXT): TEXT =
  BEGIN
  RETURN Text.Sub (t, 2, Text.Length(t)-2)
  END PostscriptContent;

PROCEDURE Repaint (e: E; box: DPS.Box; <*UNUSED*>only: REFANY := NIL): TEXT =
 VAR data: TEXT := "";
 VAR caretX: REAL;
 VAR caretBox: DPS.Box;
  BEGIN
  IF NOT DPS.BoxesIntersect (e.box, box) THEN RETURN NIL; END;
  IF e.isPostscript THEN
    data := Fmt.Real(e.box.low.x + e.leftLeading) & " "
     & Fmt.Real(e.box.low.y + e.belowLeading + fontDescender)
     & " moveto "
     & "/" & DPS.PreferredFontName() 
     & " findfont " & Fmt.Real(e.typefacePoints) & " scalefont setfont "
     & PostscriptContent(e.text);
    RETURN " gsave " & data & " grestore ";
    END;
  IF e.hasInputFocus THEN 
    data := DPS.EdgedBoxClipAndPaint (e.box) & " " 
     & Fmt.Real(e.box.low.x + e.leftLeading) & " "
     & Fmt.Real(e.box.low.y + e.belowLeading + fontDescender) & " moveto "
     & e.fontName & " setfont " & e.showPostScript;
    caretX := e.box.low.x + e.leftLeading;
    FOR k := 0 TO e.insertAfterIndex DO caretX := caretX + e.widths[k]; END;
    caretBox := e.box;
    caretBox.low.x := caretX + 0.5;
    caretBox.high.x := caretBox.low.x + 1.0;
    data := data & DPS.NewPathBox(caretBox) & " 0.0 1.0 1.0 sethsbcolor fill";
   ELSE 
    data := Fmt.Real(e.box.low.x + e.leftLeading) & " "
     & Fmt.Real(e.box.low.y + e.belowLeading + fontDescender) & " moveto "
     & e.fontName & " setfont " & e.showPostScript;
    END;
  RETURN " gsave " & data & " grestore ";
  END Repaint;

PROCEDURE Init (e: E; window: DPSWindow.T) =
  BEGIN
  IF e.initialized THEN RETURN; END;
  e.isPostscript := LooksLikePostscript (e.text);
  RecalculatePoints (e, window);
  RecalculatePostScript (e, window);
  RecalculateArea (e, window);
  e.initialized := TRUE;
  END Init;

PROCEDURE RecalculatePoints (e: E; window: DPSWindow.T) =
 VAR centipoints: INTEGER;
  BEGIN
  IF e.typefacePoints < 0.0 THEN e.typefacePoints := pointsDefault; END;
  IF e.aboveLeading < 0.0 THEN e.aboveLeading := aboveLeadingDefault; END;
  IF e.height < 0.0 THEN e.height := heightDefault; END;
  IF e.belowLeading < 0.0 THEN e.belowLeading := belowLeadingDefault; END;
  IF e.leftLeading < 0.0 THEN e.leftLeading := leftLeadingDefault; END;
   IF e.isPostscript THEN
    TRY DPS.AcquireDPSMutex();
      TRY DPS.SendNervously ( window, 
       " /points " & Fmt.Real(e.typefacePoints) & " def "
       & " /aboveLeading " & Fmt.Real(e.aboveLeading) & " def "
       & " /height " & Fmt.Real(e.height) & " def "
       & " /belowLeading " & Fmt.Real(e.belowLeading) & " def "
       & " /leftLeading " & Fmt.Real(e.leftLeading) & " def "
       & " /Helvetica findfont 10 scalefont setfont "
       & "999999 999999 moveto " 
       & PostscriptContent(e.text) & " ",
        TRUE (* regardless *), TRUE (* alreadyLocked *) );
      e.typefacePoints := wraps.FetchNumber ( window.ctx, "points", TRUE);
      e.aboveLeading := wraps.FetchNumber ( window.ctx, "aboveLeading", TRUE);
      e.height := wraps.FetchNumber ( window.ctx, "height", TRUE);
      e.belowLeading := wraps.FetchNumber ( window.ctx, "belowLeading", TRUE);
      e.leftLeading := wraps.FetchNumber ( window.ctx, "leftLeading", TRUE );
     EXCEPT DPS.BadPostScript =>
        Err.Msg ("Got invalid PostScript: ", e.text);
        e.isPostscript := FALSE; (* Vital. *)
        END;
     FINALLY DPS.ReleaseDPSMutex();
      END;
   END;
  centipoints := ROUND (e.typefacePoints * 100.0);
  e.fontName := DPS.PreferredFontName() & "-" & Fmt.Int(centipoints);
  (* Could use nonce id here ^^. *)
  window.SendFoundation ( " /" & e.fontName & " /" & DPS.PreferredFontName() 
   & " findfont " & Fmt.Real(e.typefacePoints) & " scalefont def " ); 
  (* Many times alas. *)
  END RecalculatePoints;

PROCEDURE RecalculatePostScript (e: E; <*UNUSED*> window: DPSWindow.T) =
  BEGIN
  e.showPostScript := DPS.ShowItAccentedPostScript(e.text);
  END RecalculatePostScript;

PROCEDURE RecalculateArea ( e: E; window: DPSWindow.T; 
 measureOK: BOOLEAN := FALSE ) =
 VAR textWidth: REAL;
 VAR b: DPS.Box;
  BEGIN
  IF NOT measureOK THEN
    e.widths := DPS.MeasureText (e.text, window, e.fontName);
    END;
  textWidth := 0.0;
  FOR j := 0 TO NUMBER(e.widths^)-1 DO
    textWidth := textWidth + e.widths^[j];
    END;
  (* wraps.Stringwidth (window.ctx, e.fontName, e.text, height, textWidth); *)
  b := e.box;
  b.high.x := b.low.x + e.leftLeading + textWidth; 
  b.high.y := b.low.y + e.aboveLeading + e.height + e.belowLeading;
  e.NewBox (b);
  END RecalculateArea;

PROCEDURE AfterWhich (a: REF ARRAY OF REAL; x: REAL): INTEGER =
 VAR sum: REAL := 0.0;
 VAR k: INTEGER;
  BEGIN
  FOR j := 0 TO NUMBER(a^)-1 DO
    IF x < (sum + a^[j] / 2.0) THEN (* Here, but skip accents. *)
      k := j;
      WHILE (k < NUMBER(a^)) AND (a^[k] < 0.001) DO INC (k); END;
      RETURN k - 1; 
      END;
    sum := sum + a^[j];
    END;
  RETURN NUMBER(a^) - 1;
  END AfterWhich;

PROCEDURE Mouse (e: E; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR in: BOOLEAN;
 VAR loc: INTEGER;
 VAR affected: DisplayList.E;
  BEGIN
  Init (e, window);
  in := DPS.PlaceIsInBox (event.place, e.box);
  IF in THEN
    IF event.clickType = DPS.ClickType.LastUp THEN
      loc := AfterWhich ( e.widths, 
       event.place.x - (e.box.low.x+e.leftLeading) );
      <* ASSERT loc < NUMBER(e.widths^) *>
      IF e.hasInputFocus THEN (* Maybe move insert point. *)
        IF loc # e.insertAfterIndex THEN
          e.insertAfterIndex := loc;
          e.Dirty (e.box, NIL); (* May it be not on top? *)
          END;
       ELSE (* Need to get input focus. *)
        e.insertAfterIndex := loc;
        e.GetInputFocus (NIL);
        e.hasInputFocus := TRUE;
        affected := e.MoveToLast();
        IF affected=NIL THEN affected := e; END;
        affected.Dirty (affected.box, affected);
        END;
      END;
    END;
  RETURN in;
  END Mouse;

PROCEDURE SloppyBox (box: DPS.Box): DPS.Box =
  BEGIN (* DisplayPostScript clipping is a bit buggy. *)
  box.low.x := box.low.x - 1.0;
  box.high.x := box.high.x + 1.0;
  box.low.y := box.low.y - 1.0;
  box.high.y := box.high.y + 1.0;
  RETURN box;
  END SloppyBox;

PROCEDURE BumpBoxX (box: DPS.Box; bump: REAL): DPS.Box =
  BEGIN
  box.low.x := box.low.x + bump;
  box.high.x := box.high.x + bump;
  RETURN box;
  END BumpBoxX;

(***
PROCEDURE BumpBoxY (box: DPS.Box; bump: REAL): DPS.Box =
  BEGIN
  box.low.y := box.low.y + bump;
  box.high.y := box.high.y + bump;
  RETURN box;
  END BumpBoxY;
***)

PROCEDURE WidthsDelete (w: REF ARRAY OF REAL; i: INTEGER): REF ARRAY OF REAL =
 VAR ret: REF ARRAY OF REAL;
  BEGIN
  IF NUMBER(w^) < 1 THEN RETURN w; END;
  <* ASSERT i >= 0 *>
  <* ASSERT i < NUMBER(w^) *>
  ret := NEW (REF ARRAY OF REAL, NUMBER(w^) - 1);
  FOR k := 0 TO i-1 DO ret^[k] := w^[k]; END;
  FOR k := i+1 TO NUMBER(w^)-1 DO ret^[k-1] := w^[k]; END;
  RETURN ret;
  END WidthsDelete;

PROCEDURE WidthsInsert (w: REF ARRAY OF REAL; i: INTEGER; r: REAL): REF ARRAY OF REAL =
 VAR ret: REF ARRAY OF REAL;
  BEGIN
  <* ASSERT i >= 0 *>
  <* ASSERT i <= NUMBER(w^) *>
  ret := NEW (REF ARRAY OF REAL, NUMBER(w^) + 1);
  FOR k := 0 TO i-1 DO ret^[k] := w^[k]; END;
  ret^[i] := r;
  FOR k := i TO NUMBER(w^)-1 DO ret^[k+1] := w^[k]; END;
  RETURN ret;
  END WidthsInsert;

PROCEDURE Char (e: E; window: DPSWindow.T; char: CHAR): BOOLEAN =
 VAR oldBox: DPS.Box;
 VAR parent: Linked2Tree.T;
 VAR new: E;
  BEGIN
  IF NOT e.hasInputFocus THEN RETURN FALSE; END;
  Init (e, window);
  IF e.hasInputFocus THEN 
    oldBox := e.box;
    IF ORD(char) > 127 THEN
      CASE VAL (ORD(char) - 128, CHAR) OF
      | 'l' => e.NewBox ( BumpBoxX (e.box, -BumpX) );
      | 'r' => e.NewBox ( BumpBoxX (e.box, BumpX) );
      | 'b' => e.height := e.height + PointsBump; 
               e.typefacePoints := e.typefacePoints + PointsBump; 
               RecalculatePoints (e, window);
               RecalculatePostScript (e, window);
               RecalculateArea (e, window);
      | 's' => e.height := e.height - PointsBump;
               e.typefacePoints := e.typefacePoints - PointsBump; 
               RecalculatePoints (e, window);
               RecalculatePostScript (e, window);
               RecalculateArea (e, window);
       ELSE RETURN TRUE;
        END;
      e.Dirty (DPS.BoxUnion(oldBox, e.box)); 
      RETURN TRUE;
      END;
    IF char = '\n' THEN 
      new := NEW ( E, text := e.text, box := e.box, 
       typefacePoints := e.typefacePoints, height := e.height );
      Init (new, window);
      parent := e.parent; (* See remark in Linked2Tree.i3. *)
      parent.InsertAfter (new, e); 
      new.GetInputFocus (NIL);
      new.hasInputFocus := TRUE;
      new.Dirty (new.box, new); 
      RETURN TRUE; 
      END;
    IF char = '\010' THEN
      IF Text.Length(e.text) <= 0 THEN RETURN TRUE; END;
      IF e.insertAfterIndex < 0 THEN RETURN TRUE; END;
      e.text := Text.Sub (e.text, 0, e.insertAfterIndex)
       & Text.Sub (
         e.text, 
         e.insertAfterIndex+1, 
         Text.Length(e.text) - (e.insertAfterIndex+1) );
      e.widths := WidthsDelete (e.widths, e.insertAfterIndex);
      DEC (e.insertAfterIndex);
     ELSE 
      e.text := Text.Sub (e.text, 0, e.insertAfterIndex + 1) 
       & Text.FromChar(char) 
       & Text.Sub (
         e.text, 
         e.insertAfterIndex+1,
         Text.Length(e.text) - (e.insertAfterIndex+1) );
      e.widths := WidthsInsert ( e.widths, e.insertAfterIndex + 1,
        DPS.MeasureChar (char, window, e.fontName) );
      INC (e.insertAfterIndex);
      END;
    RecalculateArea (e, window, TRUE);
    RecalculatePostScript (e, window);
    (* e.Dirty (DPS.BoxUnion(oldBox, e.box)); *)
    END;
  RETURN TRUE;
  END Char;

PROCEDURE LoseInputFocus (e: E) =
  BEGIN
  IF e.hasInputFocus THEN 
    e.hasInputFocus := FALSE; 
    e.Dirty (SloppyBox(e.box)); (* It may not be on top now! *)
    END;
  END LoseInputFocus;

  BEGIN

  END SlideLineDLE.

