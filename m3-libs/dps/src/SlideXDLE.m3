(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:47:51 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:16 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE SlideXDLE;

IMPORT DisplayList, DisplayListStack, DPS, DPSWindow, FileWr,
 Linked2Tree, PopupMenuDLE, Rd, SlideLineDLE, Text, 
 Thread, Wr, Fmt, OSError;

CONST millisecond = 0.001d0;
CONST second = 1.0d0;

CONST pointsDefault = 24.0;
CONST aboveLeadingDefault = 8.0;
CONST heightDefault = pointsDefault;
CONST belowLeadingDefault = 0.0;
(* CONST lowLeadingDefault = 0.0; *)
CONST leftLeadingDefault = 24.0;

CONST pointsDefaultTop = 30.0;
CONST aboveLeadingDefaultTop = 8.0;
CONST heightDefaultTop = pointsDefaultTop;
CONST belowLeadingDefaultTop = 16.0;
CONST leftLeadingDefaultTop = 0.0;

CONST TopMargin = 32.0;
CONST LeftMarginOfLines = 60.0; (* Alas has to factor in the boilerplate. *)

(* CONST ItemsInitiallyVisible = 1;*) (* Cannot be zero. Need to start stack. *)

CONST DecorationMarginLeft = 20.0;
CONST DecorationMarginRight = 20.0; 
CONST DecorationMarginTop = 20.0;
CONST DecorationMarginBottom = 40.0;
(* ^^ Because bottom of screen hard to see. *)

CONST backgroundPostScript = 
" 20.0 ButtonDLEDrawRoundedPath 6.0 setlinewidth 0.5 0.5 0.5 setrgbcolor stroke ";

PROCEDURE BoxFromXYWH (x, y: REAL; w, h: REAL := 0.0): DPS.Box =
 VAR box: DPS.Box;
  BEGIN
  box.low.x := x; box.high.x := x + w;
  box.low.y := y; box.high.y := y + h;
  RETURN box;
  END BoxFromXYWH;

PROCEDURE Repaint (t: T; box: DPS.Box; only: REFANY): TEXT =
 VAR bkgBox: DPS.Box;
 VAR text: TEXT := "";
  BEGIN
  bkgBox.low.x := t.box.low.x + DecorationMarginLeft; 
  bkgBox.high.x := t.box.high.x - DecorationMarginRight;
  bkgBox.low.y := t.box.low.y + DecorationMarginBottom; 
  bkgBox.high.y := t.box.high.y - DecorationMarginTop;
  IF DPS.BoxesIntersect (box, bkgBox) THEN 
    text := DPS.GSaveAndClip(box) 
     & DPS.BoxCoordsAsText (bkgBox) & backgroundPostScript & DPS.GRestore();
    END;
  RETURN text & DisplayList.Repaint (t, box, only);
  END Repaint;
 
PROCEDURE Init (t: T; window: DPSWindow.T) =
  <*FATAL Rd.Failure, Thread.Alerted*>
  BEGIN
  IF t.initialized THEN RETURN; END;
  
  (* Later compute based on content: *)
  t.box.low.y := t.box.high.y - t.maximumHeight;

  t.window := window; (* Used in capturing PostSCript. *)
  t.page := 0; t.lastPage := -1;
  t.backgroundPopup := NEW ( PopupMenuDLE.T,
   items := NEW (REF ARRAY OF PopupMenuDLE.Item, 5) );
  t.backgroundPopup.items^[0] := NEW ( PopupMenuDLE.Item, 
    text := "All Invisible", Proc := AllInvisiblePop, context := t );
  t.backgroundPopup.items^[1] := NEW ( PopupMenuDLE.Item,
    text := "Next Visible", Proc := NextVisiblePop, context := t );
  t.backgroundPopup.items^[2] := NEW (PopupMenuDLE.Item, 
    text := "All Visible", Proc := AllVisiblePop, context := t);
  t.backgroundPopup.items^[3] := NEW (PopupMenuDLE.Item, 
    text := "Toggle Mouse -> Child", Proc := ToggleMouseChildPop, context := t);
  t.backgroundPopup.items^[4] := NEW (PopupMenuDLE.Item, 
    text := "PostScript -> /tmp/slide.ps", Proc := PSPop, context := t);
  t.initialized := TRUE;

  window.SendFoundation ( " /ButtonDLEDrawRoundedPath "
   & " { /r exch def /highy exch def /highx exch def "
   & " /lowy exch def /lowx exch def "
   & " newpath lowx lowy r add moveto "
   & " lowx r add highy r sub r 180 90 arcn "
   & " highx r sub highy r sub r 90 0 arcn "
   & " highx r sub lowy r add r 0 270 arcn "
   & " lowx r add lowy r add r 270 180 arcn "
   & "  closepath } def " ); 
  END Init;

PROCEDURE RdDotGetLine (r: Rd.T): TEXT RAISES {Rd.EndOfFile} = 
(* Rd.GetLine is buggy.  21aug91 *)
 <*FATAL Rd.Failure, Thread.Alerted*>
 VAR c: CHAR;
 VAR line: TEXT := "";
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
 
PROCEDURE AddPage ( t: T; window: DPSWindow.T; 
 data: Rd.T; page: INTEGER := 999999 ) =
 VAR sle: SlideLineDLE.E; 
 VAR line: TEXT;
 VAR itemIndex: INTEGER := 0;
 VAR invisible, newInvisible: BOOLEAN := FALSE;
  BEGIN
  Init (t, window);
  IF page > (t.lastPage + 1) THEN page := t.lastPage + 1; END;
  IF page > 9 THEN RETURN; END;
  t.pages[page].fixed := NEW (DisplayList.T);
  t.pages[page].visible := NEW ( DisplayListStack.T,
    MakeChildFirst := MakeChildFirstNOP, MakeChildLast := MakeChildLastNOP );
  t.pages[page].invisible := NEW (Linked2Tree.T);
  IF (page = 0) AND (t.page = 0) THEN
    Linked2Tree.Append (t, t.pages[t.page].fixed);
    Linked2Tree.Append (t, t.pages[t.page].visible);
    END;
  LOOP
    TRY
      line := RdDotGetLine (data);
      IF Text.Equal ("/invisible", Text.Sub(line,0,10)) THEN
        invisible := TRUE;
        newInvisible := TRUE;
        IF sle # NIL THEN sle.togetherWithNext := FALSE; END;
       ELSIF Text.Equal ("/half", Text.Sub(line,0,5)) THEN
        sle := NEW ( SlideLineDLE.E,
         box := BoxFromXYWH (LeftMarginOfLines, 0.0, 0.0, 0.0), 
         typefacePoints := pointsDefault, 
         aboveLeading := aboveLeadingDefault, 
         height := heightDefault / 2.0, 
         belowLeading := belowLeadingDefault, 
         leftLeading := leftLeadingDefault, 
         text := " ",
         togetherWithNext := TRUE );
        newInvisible := FALSE;
        SlideLineDLE.Init (sle, window);
        IF NOT invisible THEN t.pages[page].visible.Append (sle); 
         ELSE t.pages[page].invisible.Append (sle);
          END;
        INC (itemIndex);
       ELSIF itemIndex < 1 THEN
        sle := NEW ( SlideLineDLE.E,
         box := BoxFromXYWH (LeftMarginOfLines, 0.0, 0.0, 0.0), 
         typefacePoints := pointsDefaultTop, 
         aboveLeading := aboveLeadingDefaultTop, 
         height := heightDefaultTop, 
         belowLeading := belowLeadingDefaultTop, 
         leftLeading := leftLeadingDefaultTop, 
         text := line,
         togetherWithNext := TRUE );
        newInvisible := FALSE;
        SlideLineDLE.Init (sle, window);
        IF NOT invisible THEN t.pages[page].visible.Append (sle); 
         ELSE t.pages[page].invisible.Append (sle);
          END;
        INC (itemIndex);
       ELSE
        sle := NEW ( SlideLineDLE.E,
         box := BoxFromXYWH (LeftMarginOfLines, 0.0, 0.0, 0.0), 
         typefacePoints := pointsDefault, 
         aboveLeading := aboveLeadingDefault, 
         height := heightDefault, 
         belowLeading := belowLeadingDefault, 
         leftLeading := leftLeadingDefault, 
         text := line,
         togetherWithNext := TRUE );
        newInvisible := FALSE;
        SlideLineDLE.Init (sle, window);
        IF NOT invisible THEN t.pages[page].visible.Append (sle); 
         ELSE t.pages[page].invisible.Append (sle);
          END;
        INC (itemIndex);
        END;
     EXCEPT Rd.EndOfFile => EXIT;
      END;
    END;
  t.pages[page].visible.Stack (t.box.high.y - TopMargin - pointsDefault);
  t.lastPage := MAX (t.lastPage, page);
  END AddPage;
 
PROCEDURE Prepend (t: T; e: Linked2Tree.E) =
  BEGIN
  Init (t, NIL);
  t.pages[t.page].fixed.Prepend (e); 
  END Prepend;
 
PROCEDURE Append (t: T; e: Linked2Tree.E) =
  BEGIN
  Init (t, NIL);
  AppendVariable (t, e); 
  END Append;
 
PROCEDURE AppendFixed (t: T; e: Linked2Tree.E) =
  BEGIN
  Init (t, NIL);
  t.pages[t.page].fixed.Append (e); 
  END AppendFixed;
 
PROCEDURE AppendVariable (t: T; e: Linked2Tree.E) =
  BEGIN
  t.pages[t.page].visible.Append (e); 
  t.pages[t.page].visible.Stack (t.box.high.y - TopMargin - pointsDefault);
  END AppendVariable;
 
PROCEDURE InsertBefore (t: T; e, before: Linked2Tree.E) =
  BEGIN
  Init (t, NIL);
  t.pages[t.page].visible.InsertBefore (e, before); 
  END InsertBefore;
 
PROCEDURE InsertAfter (t: T; e, after: Linked2Tree.E) =
  BEGIN
  Init (t, NIL);
  t.pages[t.page].visible.InsertAfter (e, after); 
  END InsertAfter;
 
PROCEDURE PSPop (p: PopupMenuDLE.Item) =
 <*FATAL OSError.E, Wr.Failure, Thread.Alerted*>
 VAR t: T;
 VAR wr: Wr.T;
 VAR currentPage: INTEGER;
  BEGIN
  t := NARROW (p.context, T);
  wr := FileWr.Open ("/tmp/slide.ps");
  Wr.PutText (wr, "%!IPS-Adobe-1.0\n");
  Wr.PutText (wr, "%%Creator: Postscript Button in SlideX\n");
  Wr.PutText (wr, "%%Title: Client Slides\n");
  Wr.PutText (wr, "%%EndComments\n");
  Wr.PutText (wr, "%%EndProlog\n\n");
  currentPage := t.page;
  FOR page := 0 TO t.lastPage DO
    NthPage (t, page);
    AllVisible (t);
    DPS.PostscriptToWriter (t.window, wr);
    Wr.PutText (wr, "\nshowpage\n\n");
    END;
  Wr.PutText (wr, "%%Trailer\n\n");
  Wr.Close(wr);
  NthPage (t, currentPage);
  END PSPop;
 
PROCEDURE ToggleMouseChildPop (p: PopupMenuDLE.Item) =
 VAR t: T;
  BEGIN
  t := NARROW (p.context, T);
  t.canMouseChildren := NOT t.canMouseChildren;
  END ToggleMouseChildPop;
 
PROCEDURE AllInvisiblePop (p: PopupMenuDLE.Item) =
  BEGIN
  AllInvisible (NARROW(p.context, T));
  END AllInvisiblePop;
 
PROCEDURE NextVisiblePop (p: PopupMenuDLE.Item) =
  BEGIN
  NextVisible (NARROW(p.context, T));
  END NextVisiblePop;
 
PROCEDURE AllVisiblePop (p: PopupMenuDLE.Item) =
  BEGIN
  AllVisible (NARROW(p.context, T));
  END AllVisiblePop;
 
PROCEDURE AllInvisible (t: T) =
 VAR cur: DisplayList.E;
  BEGIN
  t.LoseInputFocus(); 
  (* Have to kill input focus of anything moving to invisible list. *)
  LOOP
    cur := t.pages[t.page].visible.First();
    IF cur = NIL THEN EXIT; END;
    cur.Remove ();
    t.pages[t.page].invisible.Append (cur);
    END; 
  t.Dirty (t.box, NIL);
  END AllInvisible;
 
PROCEDURE NextVisible (t: T) =
 VAR cur: DisplayList.E;
  BEGIN
  LOOP
    cur := t.pages[t.page].invisible.First();
    IF cur = NIL THEN RETURN; END;
    cur.Remove ();
    t.pages[t.page].visible.Append (cur);
    TYPECASE cur OF
    | SlideLineDLE.E (sle) => 
      Reappear (sle); 
      IF NOT sle.togetherWithNext THEN EXIT; END;
     ELSE cur.Dirty (cur.box, cur); EXIT;
      END;
    END;
  END NextVisible;
 
PROCEDURE NthPage (t: T; page: INTEGER) =
  BEGIN
  IF page < 0 THEN RETURN; END;
  IF page > t.lastPage THEN RETURN; END;
  IF page = t.page THEN RETURN; END;
  t.pages[t.page].visible.Remove();
  (* t.pages[t.page].fixed.Remove(); *)
  t.page := page;
  (* Linked2Tree.Append (t, t.pages[t.page].fixed); *)
  Linked2Tree.Append (t, t.pages[t.page].visible);
  (* NOT t.Append, as that *really* appends to the visibles! *)
  (* (Or tries to.  Here, calling it recurses off the end of the stack.) *)
  t.Dirty (DPS.EverywhereBox);
  END NthPage;
 
PROCEDURE NextSomething (t: T) =
  BEGIN
  IF t.pages[t.page].invisible.First() # NIL THEN NextVisible (t);
   ELSE NthPage (t, t.page + 1);
    END;
  END NextSomething;
 
PROCEDURE AllVisible (t: T) =
 VAR cur: DisplayList.E;
  BEGIN
  LOOP
    cur := t.pages[t.page].invisible.First();
    IF cur = NIL THEN RETURN; END;
    cur.Remove ();
    t.pages[t.page].visible.Append (cur);
    TYPECASE cur OF
    | SlideLineDLE.E (sle) => Reappear (sle); 
     ELSE cur.Dirty (cur.box, cur);
      END;
    END;
  END AllVisible;
 
PROCEDURE NthVisible (t: T; n: INTEGER): DisplayList.T =
 VAR cur: DisplayList.E;
  BEGIN
  cur := t.pages[t.page].visible.First();
  WHILE n > 0 DO
    IF cur = NIL THEN RETURN NIL; END;
    cur := cur.Next();
    DEC (n);
    END;
  RETURN cur;
  END NthVisible;
 
TYPE RepaintClosure = Thread.Closure OBJECT item: SlideLineDLE.E; END; 

PROCEDURE RepaintNormallyAFterPause (rc: RepaintClosure): REFANY RAISES {} =
  BEGIN
  Thread.Pause (2.0d0 * second);
  rc.item.Dirty (rc.item.box, rc.item);
  RETURN NIL;
  END RepaintNormallyAFterPause;

PROCEDURE Reappear (item: SlideLineDLE.E) =
 CONST wait = 100.0d0 * millisecond;
 CONST delta = 0.167;
 VAR r, g, b: REAL;
 VAR ps: TEXT;
  PROCEDURE DownRed () =
    BEGIN
    r := MAX (0.0, r - delta);
    item.ImmediatePostScript ( " " 
     & Fmt.Real(r) & " " & Fmt.Real(g) 
     & " " & Fmt.Real(b) & " setrgbcolor " & ps );
    Thread.Pause (wait);
    END DownRed;
  PROCEDURE DownGB () =
    BEGIN
    g := MAX (0.0, g - delta);
    b := MAX (0.0, b - delta);
    item.ImmediatePostScript ( " " 
     & Fmt.Real(r) & " " & Fmt.Real(g) 
     & " " & Fmt.Real(b) & " setrgbcolor " & ps );
    Thread.Pause (wait);
    END DownGB;
  BEGIN
  ps :=  item.Repaint (item.box, item);
  (* May not really be on top, but we lie in the Dirty/Repaint calls. *)
  (* If really obscured, the repaints would look ugly any way. *)
  r := 1.0; g := 1.0; b := 1.0;
  WHILE g > 0.0 DO DownGB(); END;
  WHILE r > 0.0 DO DownRed(); END;
  item.ImmediatePostScript ( " 0.0 0.0 0.0 setrgbcolor " );
  (*
  EVAL Thread.Fork ( 
   NEW ( RepaintClosure,
    apply := RepaintNormallyAFterPause, 
    item := item ) );
  *)
END Reappear;

PROCEDURE MakeChildFirst (<*UNUSED*> t: T;
                          <*UNUSED*> e: Linked2Tree.E): Linked2Tree.E =
  BEGIN
  RETURN NIL;
  (* RETURN Linked2Tree.MakeChildFirst (t.visible, e); *)
  END MakeChildFirst;
  
PROCEDURE MakeChildLast (<*UNUSED*> t: T;
                         <*UNUSED*> e: Linked2Tree.E): Linked2Tree.E =
  BEGIN
  RETURN NIL;
  (* RETURN Linked2Tree.MakeChildLast (t.visible, e); *)
  END MakeChildLast;
  
PROCEDURE MakeChildFirstNOP (<*UNUSED*> t: Linked2Tree.T;
                             <*UNUSED*> e: Linked2Tree.E): Linked2Tree.E =
  BEGIN
  RETURN NIL;
  END MakeChildFirstNOP;
  
PROCEDURE MakeChildLastNOP (<*UNUSED*> t: Linked2Tree.T;
                            <*UNUSED*> e: Linked2Tree.E): Linked2Tree.E =
  BEGIN
  RETURN NIL;
  END MakeChildLastNOP;

PROCEDURE Mouse (t: T; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR e: DisplayList.E;
  BEGIN
  Init (t, window);

  IF t.canMouseChildren THEN
    e := t.Last();
    WHILE e # NIL DO 
      IF e.Mouse (window, event) THEN RETURN TRUE; END; 
      e := e.Previous(); 
      END;
    END;

  IF event.clickType # DPS.ClickType.FirstDown THEN RETURN FALSE; END;

  t.GetInputFocus ();

  IF NOT t.canMouseChildren THEN (* Allow user to highlight by pointing. *)
    e := t.pages[t.page].visible.Last(); 
    WHILE e # NIL DO 
      TYPECASE e OF
        SlideLineDLE.E(sle) =>
        SlideLineDLE.Init (sle, window);
        IF DPS.PlaceIsInBox (event.place, sle.box) THEN 
          ShortHighlight (sle);
          RETURN TRUE;
          END; (* of IF is in box *)
       ELSE (* Do nothing. What is it? *)
        END; (* of TYPECASE *)
      e := e.Previous(); 
      END; (* of WHILE *)
    END; (* of IF not can mouse *)
  
  CASE event.whatChanged OF
  | DPS.Button.Left => (* Left just gets us the input focus. *)
  | DPS.Button.Middle => t.backgroundPopup.Popup (event.place, window);
  | DPS.Button.Right => 
    END; (* of CASE *)

  RETURN TRUE;
  END Mouse;

PROCEDURE ShortHighlight (sle: SlideLineDLE.E) =
 VAR ps: TEXT;
  BEGIN
  ps :=  sle.Repaint (sle.box, sle);
  sle.ImmediatePostScript ( " 1.0 0.0 0.0 setrgbcolor " & ps );
  sle.ImmediatePostScript ( " 0.0 0.0 0.0 setrgbcolor " );
  EVAL Thread.Fork ( 
   NEW ( RepaintClosure,
    apply := RepaintNormallyAFterPause, 
    item := sle ) );
  END ShortHighlight;

PROCEDURE MaybeShortHighlight (e: DisplayList.E) =
  BEGIN
  IF e # NIL THEN
    TYPECASE e OF SlideLineDLE.E(sle) => ShortHighlight (sle);
     ELSE
      END;
    END;
  END MaybeShortHighlight;

PROCEDURE Char (e: E; window: DPSWindow.T; char: CHAR): BOOLEAN =
 VAR ee: DisplayList.E;
  BEGIN
  Init (e, window);
  ee := e.childWithInputFocus;
  IF ee # NIL THEN RETURN ee.Char (window, char); END;
  CASE char OF
  | ' ' =>  NextSomething (e);
  | 'i' =>  AllInvisible (e);
  | 'v' =>  NextVisible (e);
  | '-' =>  NthPage (e, e.page - 1);
  | '_' =>  NthPage (e, e.page - 1);
  | '+' =>  NthPage (e, e.page + 1);
  | '=' =>  NthPage (e, e.page + 1);
  | 'a' =>  AllVisible (e);
  | '1' =>  MaybeShortHighlight ( NthVisible (e, 0) );
  | '2' =>  MaybeShortHighlight ( NthVisible (e, 1) );
  | '3' =>  MaybeShortHighlight ( NthVisible (e, 2) );
  | '4' =>  MaybeShortHighlight ( NthVisible (e, 3) );
  | '5' =>  MaybeShortHighlight ( NthVisible (e, 4) );
  | '6' =>  MaybeShortHighlight ( NthVisible (e, 5) );
  | '7' =>  MaybeShortHighlight ( NthVisible (e, 6) );
  | '8' =>  MaybeShortHighlight ( NthVisible (e, 7) );
  | '9' =>  MaybeShortHighlight ( NthVisible (e, 8) );
  | '!' =>  NthPage (e, 0);
  | '@' =>  NthPage (e, 1);
  | '#' =>  NthPage (e, 2);
  | '$' =>  NthPage (e, 3);
  | '%' =>  NthPage (e, 4);
  | '^' =>  NthPage (e, 5);
  | '&' =>  NthPage (e, 6);
  | '*' =>  NthPage (e, 7);
  | '(' =>  NthPage (e, 8);
   ELSE
    END; (* of CASE *)
  RETURN FALSE;
  END Char;

(***************8
PROCEDURE Key (t: T; window: DPSWindow.T; event: DPS.KeyEvent) =
 VAR got: CHAR;
 VAR e: DisplayList.E;
  BEGIN
  Init (t, window);
  got := DPS.CharFromKey (event.key, event.modifiers);
  IF got = '\000' THEN RETURN; END;
  e := t.Last();
  WHILE e # NIL DO 
    IF e.Char (window, got) THEN RETURN; END; 
    e := e.Previous(); 
    END;
  Err.Msg ("Ignored Keystroke = ", Text.FromChar(got), ".");
  END Key;
*****************)

  BEGIN

  END SlideXDLE.

