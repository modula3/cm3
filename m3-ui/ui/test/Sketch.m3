(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Feb 24 13:49:04 PST 1992 by muller   *)
(*      modified on Tue Nov 19  0:59:35 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:55:45 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE Sketch EXPORTS Main;

IMPORT VBT, Trestle, Point, Rect, Path, ButtonVBT, PaintOp,
  Region, HVSplit, TextVBT, Axis, KeyboardKey, Pixmap, Filter, Text;

FROM VBT IMPORT ClickType;

TYPE Elem = RECORD 
    fill: BOOLEAN; 
    color: Pixmap.T;
    path: Path.T;
    width: CARDINAL
  END;

TYPE DrawVBT = VBT.Leaf OBJECT
    display: REF ARRAY OF Elem := NIL;
    m: CARDINAL := 0; (* # of active display elements *)
    path, dotPath: Path.T;
    stack: ARRAY [0..3] OF Point.T;
    n: CARDINAL := 0; (* # of active stack points *)
    txtr := Pixmap.Solid;
    fill := FALSE;
    width := 0
  OVERRIDES
    repaint := Repaint;
    reshape := Reshape;
    mouse := Mouse;
    key := Key
  END;

PROCEDURE Highlight(v: DrawVBT; READONLY clip := Rect.Full) =
  BEGIN
    VBT.Stroke(v, clip, v.path, src := v.txtr, width := v.width,
      op := PaintOp.TransparentSwap);
    FOR i := 0 TO v.n-1 DO Dot(v, v.stack[i], clip) END
  END Highlight;

PROCEDURE Dot(v: DrawVBT; READONLY p: Point.T; READONLY clip: Rect.T) =
  BEGIN
    Path.Reset(v.dotPath);
    Path.MoveTo(v.dotPath, p);
    VBT.Stroke(v, clip, v.dotPath, src := v.txtr, width := 5,
      op := PaintOp.TransparentSwap);  
  END Dot;

PROCEDURE Repaint(v: DrawVBT; READONLY rgn: Region.T) =
  BEGIN
    VBT.PaintTint(v, rgn.r, PaintOp.Bg);
    FOR i := 0 TO v.m-1 DO DisplayElem(v, rgn.r, v.display[i]) END;
    Highlight(v, rgn.r);
  END Repaint;

PROCEDURE DisplayElem(v: DrawVBT; clip: Rect.T; e: Elem) =
  BEGIN
    IF e.fill THEN
      VBT.Fill(v, clip, e.path, src := e.color)
    ELSE
      VBT.Stroke(v, clip, e.path, src := e.color, width := e.width);
    END
  END DisplayElem;

PROCEDURE Reshape(v: DrawVBT; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    InnerReshape(v, Rect.Middle(cd.new), Rect.Middle(cd.prev))
  END Reshape;
  
PROCEDURE InnerReshape(v: DrawVBT; READONLY new, old: Point.T) =
  VAR delta := Point.Sub(new, old); BEGIN
    FOR i := 0 TO v.m - 1 DO
      WITH e = v.display[i] DO
        e.path := Path.Translate(e.path, delta)
      END
    END;
    v.path := Path.Translate(v.path, delta);
    FOR i := 0 TO v.n-1 DO
      v.stack[i] := Point.Add(v.stack[i], delta)
    END;
    Repaint(v, Region.Full)
  END InnerReshape;

PROCEDURE Mouse(v: DrawVBT; READONLY cd: VBT.MouseRec) =
  BEGIN
    IF cd.clickType = ClickType.FirstDown AND v.n # NUMBER(v.stack) THEN
      Dot(v, cd.cp.pt, Rect.Full);
      v.stack[v.n] := cd.cp.pt;
      INC(v.n);
      VBT.Acquire(v, VBT.KBFocus, cd.time)
    END
  END Mouse;

PROCEDURE Key(v: DrawVBT; READONLY cd: VBT.KeyRec) =
  BEGIN
    IF cd.whatChanged = KeyboardKey.Escape AND cd.wentDown THEN
      Highlight(v);
      IF v.n = 0 THEN
        VAR elem: Elem; BEGIN
          elem.fill := v.fill;
          elem.color := v.txtr;
          elem.width := v.width;
          elem.path := v.path;
          IF v.m = NUMBER(v.display^) THEN
            VAR new := NEW(REF ARRAY OF Elem, 2 * v.m); BEGIN
              SUBARRAY(new^, 0, v.m) := v.display^;
              v.display := new
            END
          END;
          v.display[v.m] := elem;
          INC(v.m);
          DisplayElem(v, VBT.Domain(v), elem);
          v.path := NEW(Path.T)
        END
      ELSIF v.n = 1 AND NOT Path.IsClosed(v.path) THEN
        Path.LineTo(v.path, v.stack[0])
      ELSIF v.n = 2 THEN
        Path.MoveTo(v.path, v.stack[0]);
        Path.LineTo(v.path, v.stack[1])
      ELSIF v.n = 3 AND NOT Path.IsClosed(v.path) THEN
        Path.CurveTo(v.path, v.stack[0], v.stack[1], v.stack[2])
      ELSIF v.n = 4 THEN
         Path.MoveTo(v.path, v.stack[0]);
         Path.CurveTo(v.path, v.stack[1], v.stack[2], v.stack[3])
      END;
      v.n := 0;
      Highlight(v)
    END
  END Key;

PROCEDURE DoErase(<*UNUSED*>b: ButtonVBT.T; 
  <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    Path.Reset(drawVBT.path);
    drawVBT.n := 0;
    drawVBT.m := 0;
    Repaint(drawVBT, Region.Full)
  END DoErase;

PROCEDURE DoExit(<*UNUSED*>b: ButtonVBT.T; 
  <*UNUSED*>READONLY cd: VBT.MouseRec) =
  BEGIN
    Trestle.Delete(main);
  END DoExit;

PROCEDURE ToggleStroke(b: ButtonVBT.T; 
  <*UNUSED*>READONLY cd: VBT.MouseRec) =
  VAR current := TextVBT.Get(Filter.Child(b)); BEGIN
    IF Text.Equal(current, "Stroke") THEN
      TextVBT.Put(Filter.Child(b), "Fill");
      drawVBT.fill := TRUE
    ELSE
      TextVBT.Put(Filter.Child(b), "Stroke");
      drawVBT.fill := FALSE
    END
  END ToggleStroke;

PROCEDURE ToggleBlack(b: ButtonVBT.T; <*UNUSED*>READONLY cd: VBT.MouseRec) =
  VAR current := TextVBT.Get(Filter.Child(b)); BEGIN
    Highlight(drawVBT);
    IF Text.Equal(current, "Black") THEN
      TextVBT.Put(Filter.Child(b), "Gray");
      drawVBT.txtr := Pixmap.Gray
    ELSE
      TextVBT.Put(Filter.Child(b), "Black");
      drawVBT.txtr := Pixmap.Solid
    END;
    Highlight(drawVBT)
  END ToggleBlack;

PROCEDURE ToggleWidth(b: ButtonVBT.T; <*UNUSED*>READONLY cd: VBT.MouseRec) =
  VAR current := TextVBT.Get(Filter.Child(b)); BEGIN
    Highlight(drawVBT);
    IF Text.Equal(current, "0") THEN
      TextVBT.Put(Filter.Child(b), "1");
      drawVBT.width := 1
    ELSIF Text.Equal(current, "1") THEN
      TextVBT.Put(Filter.Child(b), "5");
      drawVBT.width := 5
    ELSIF Text.Equal(current, "5") THEN
      TextVBT.Put(Filter.Child(b), "0");
      drawVBT.width := 0
    END;
    Highlight(drawVBT)
  END ToggleWidth;

VAR 
  drawVBT := NEW(DrawVBT, path := NEW(Path.T), dotPath := NEW(Path.T), 
    display := NEW(REF ARRAY OF Elem, 10));
  menuBar := ButtonVBT.MenuBar(
    ButtonVBT.New(TextVBT.New("Erase"), DoErase),
    ButtonVBT.New(TextVBT.New("Exit"), DoExit),
    ButtonVBT.New(TextVBT.New("Stroke"), ToggleStroke),
    ButtonVBT.New(TextVBT.New("Black"), ToggleBlack),
    ButtonVBT.New(TextVBT.New("0"), ToggleWidth));
  main := HVSplit.Cons(Axis.T.Ver, menuBar, drawVBT, adjustable := FALSE);

<*FATAL ANY*>
BEGIN 
  Trestle.Install(main);
  Trestle.AwaitDelete(main)
END Sketch.
