(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

(* The client's window on the board. *)

MODULE Win;

IMPORT VBT, Rect, Font, Point, HighlightVBT, Color,
       KeyboardKey, Latin1Key, Text, TextSeq, Thread, NetObj, Time,
       Item, ItemList, ItemClass, TextItem, RuleItem, Do,
       View, Board, Trans, RectR, PointR, Focus,
       ItemFont, FontCache;

REVEAL T = Public BRANDED OBJECT
    parent: VBT.T;
    status := Status.Nothing;
    color := Color.Black;

    kbCount := 0;
    curText: TextItem.T;
    cursor: Point.T;
    itemFont: ItemFont.T;
    font: Font.T;
    lmargin: INTEGER;
    curStart: INTEGER;

    pointer: Point.T;
    pointer2: Point.T;

    zoomRate := 0.5;
    lastZoom: Time.T;
    zoomIndex := 0;

    moveBox: Rect.T;

    do: Do.T;

  OVERRIDES
    init := Init;
    key := Key;
    position := Position;
    misc := Misc;
  END;

(* The "parent" is the parent doublebuffered VBT returned by
   "View.T.init". It is used to compute the delta between the NW
   corners of the parent and the child. 

   The "status" gives the current action in progress.
   "color" is the color that will be acquired by any new items
   created.


   The status "Typing" implies that the user is typing the item
   "curText", unless "curText" is "NIL", which indicates that a new
   item has yet to begin.  
   "cursor" is the current position of the cursor (not to be mistaken 
   with the mouse pointer). 
   "font" is the font that will be chosen for a new item. 
   "itemFont" is the representation of "font" as an "ItemFont.T".
   "lmargin" is the "h" coordinate where the cursor will return to on 
   carriage return.
   "curStart" is the "h" coordinate of "curText"'s reference point
   in pixels; if "curText" is NIL, it is the "h" coordinate of
   "wn.cursor". 
   Regardless of the "status", "kbCount" records the difference
   between acquires and losses of "KBFocus".
   An active cursor is painted if and only if status is "Typing" and 
   "kbCount" is positive.

   The status "Selecting" implies that the user is in the process of
   selecting items.
   "pointer" stores the top-left corner of the rectangle enclosing
   selected items. "pointer" is in the Win coordinates.
   "selected" gives the list of items selected (irrespective of the
   status). 

   The status "Moving" moves the selected items. It sets and uses
   "selectedBox" --- the bouding box of all selected items.

   The status "Ruling", for drawing rules, keeps track of two
   pointers: "pointer" and  "pointer2", which mark the corners of the
   rule. 

   If "status" is "Panning" then "pointer" tracks the last position of
   the mouse pointer. The status "Dragging" is similar to "Panning".

   The status "Magnifying" and "Reducing" fork off a thread that zooms.
   The "zoomRate" controls the factor by which the scale much be
   increased / decreased per second. "lastZoom" stores the last time
   the scale was increased (or the zoom was invoked), and is used to
   determine the factor by    which the scale must be increased /
   decreased per step. "zoomIndex" ensures that a zoom thread created
   to execute one zoom does not continue to execute others.

   The status "Moving" involves moving the selected items. The
   "moveBox" is set to the the join of the bounding boxes of all
   selected items. 

   The "do" object tracks the calls to "createItems" and
   "deleteItems", and is used to undo them.

*)

PROCEDURE Init (wn: T; board: Board.T): VBT.T 
    RAISES {NetObj.Error} =
  VAR fontName := "-*-times-medium-r-*-*-*-100-*";
  BEGIN 
    wn.parent := View.T.init (wn, board);
    wn.itemFont := ItemFont.FromName (fontName);
    wn.font := FontCache.Get (fontName);
    wn.do := NEW (Do.T).init (wn);
    RETURN (wn.parent);
  END Init;


PROCEDURE PaintCursor (wn: T) =
  VAR rect := Rect.Add (VBT.BoundingBox (wn, "x", wn.font), 
                        Point.Add (wn.cursor, ParentDelta (wn)));
  BEGIN
    IF wn.kbCount > 0 THEN
      HighlightVBT.SetRect (wn, rect, 100);
    ELSE
      HighlightVBT.SetRect (wn, rect, 2);
    END;
  END PaintCursor;

PROCEDURE UnpaintCursor (wn: T) =
  BEGIN
    HighlightVBT.SetRect (wn, Rect.Empty);
  END UnpaintCursor;

PROCEDURE GetStatus (wn: T): Status =
  BEGIN RETURN wn.status END GetStatus;

PROCEDURE Nothing (wn: T) =
  BEGIN
    CASE wn.status OF
    | Status.Typing => EndTyping (wn);
    | Status.Ruling => EndRuling (wn);
    | Status.Selecting => EndSelecting (wn);
    | Status.Moving => EndMoving (wn);
    ELSE (*SKIP*)
    END;
    wn.status := Status.Nothing;
    VBT.SetCage (wn, VBT.EverywhereCage);    
  END Nothing;

PROCEDURE Typing  (wn: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Nothing (wn);
    TRY
      VBT.Acquire (wn, VBT.KBFocus, cd.time);
      INC (wn.kbCount);
      wn.status := Status.Typing;
      wn.curText := NIL;
      wn.cursor := cd.cp.pt;
      wn.lmargin := wn.cursor.h;
      wn.curStart := wn.cursor.h;
      PaintCursor (wn);
    EXCEPT
      VBT.Error => RETURN;
    END;
  END Typing;

PROCEDURE EndTyping (wn: T) =
  BEGIN
    wn.status := Status.Nothing; 
    (* above ensures that "Misc" works correctly *) 
    UnpaintCursor (wn);
    (* VBT.Release (wn, VBT.KBFocus); *)
  END EndTyping;

PROCEDURE Ruling (wn: T; READONLY cd: VBT.MouseRec) =
  BEGIN 
    Nothing (wn);
    wn.status := Status.Ruling;
    wn.pointer := cd.cp.pt;
    wn.pointer2 := cd.cp.pt;
    VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
  END Ruling;

PROCEDURE EndRuling (wn: T) =
  VAR rect := Rect.FromCorners (wn.pointer, wn.pointer2);
      it := NEW (RuleItem.T, box := Trans.RectW2B (rect, View.GetFocus (wn)),
                 color := wn.color);
      its := NEW (Item.TArray, 1);
  BEGIN
    HighlightVBT.SetRect (wn, Rect.Empty);
    its[0] := it;
    wn.do.createItems (its);
  END EndRuling;

PROCEDURE Panning (wn: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Nothing (wn);
    wn.status := Status.Panning;
    wn.pointer := cd.cp.pt;
    VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
  END Panning;

PROCEDURE Dragging (wn: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Nothing (wn);
    wn.status := Status.Dragging;
    wn.pointer := cd.cp.pt;
    VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
  END Dragging;

PROCEDURE Moving (wn: T; READONLY cd: VBT.MouseRec) =
  VAR il := View.GetSelection (wn);
      box := RectR.Empty;
  BEGIN
    Nothing (wn);
    wn.status := Status.Moving;
    wn.pointer := cd.cp.pt;
    wn.pointer2 := cd.cp.pt;
    WHILE il # NIL DO
      box := RectR.Join (box, il.head.box);
      il := il.tail;
    END;
    wn.moveBox := Trans.RectB2W (box, View.GetFocus (wn));
    VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
  END Moving;

PROCEDURE EndMoving (wn: T) =
  VAR il := View.GetSelection (wn);
      its := NEW (Item.TArray, ItemList.Length (il));
      i := 0;
      focus := View.GetFocus (wn);
      delta := Point.Sub (wn.pointer2, wn.pointer);
      displ := PointR.T{h := FLOAT (delta.h)/focus.scale,
                        v := FLOAT (delta.v)/focus.scale};
      oldBox := RectR.Empty;
  BEGIN
    WHILE il # NIL DO
      oldBox := RectR.Join (oldBox, il.head.box);
      il.head.move (displ);
      (* Ideally the items should be copied and then modified. 
         Otherwise: weird effects if the item is being painted. *)
      its [i] := il.head;
      INC (i);
      il := il.tail;
    END;
    View.ModifyItems (wn, its, FALSE, oldBox);
    HighlightVBT.SetRect (wn, Rect.Empty);
  END EndMoving;

PROCEDURE Selecting (wn: T; READONLY cd: VBT.MouseRec) =
  BEGIN 
    Nothing (wn);
    wn.status := Status.Selecting;
    wn.pointer := cd.cp.pt;
    View.SetSelectionBox (wn, Rect.Empty);
    VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
  END Selecting;

PROCEDURE EndSelecting (wn: T) =
  BEGIN
    HighlightVBT.SetRect (wn, Rect.Empty);
  END EndSelecting; 

PROCEDURE SelectItem (wn: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    View.SelectOne (wn, cd.cp.pt);
  END SelectItem;

PROCEDURE DiscardSelection  (wn: T) =
  BEGIN
    Nothing (wn);
    View.SetSelectionBox (wn, Rect.Empty);
  END DiscardSelection;

PROCEDURE DeleteSelection  (wn: T) =
  BEGIN
    Nothing (wn);
    VAR il := View.GetSelection (wn);
        its := NEW (Item.TArray, ItemList.Length (il));
        j := 0;
    BEGIN
      WHILE il # NIL DO
        its [j] := il.head;
        il := il.tail;
        INC (j);
      END;
      wn.do.deleteItems (its);
    END;
  END DeleteSelection;

PROCEDURE PasteSource (wn: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Nothing (wn);
    TRY
      TYPECASE VBT.Read (wn, VBT.Source, cd.time).toRef () OF
        TEXT (text) => 
        VAR tseq := BreakLines (text);
            its := NEW (Item.TArray, tseq.size());
            rp := Trans.PointW2B (cd.cp.pt, View.GetFocus (wn));
            bb := VBT.BoundingBox (wn, "|", wn.font); 
            lineskip := bb.south - bb.north;
            focus := View.GetFocus (wn);
            font := ItemFont.Scale (wn.itemFont, focus.scale);
        BEGIN
          FOR i := 0 TO tseq.size ()-1 DO
            its[i] := NEW (TextItem.T, rp := rp, color := wn.color,
                           font := font, text := tseq.get (i));
            AdjustTextBox (wn, its[i]);
            rp.v := rp.v + FLOAT (lineskip)/focus.scale;
          END;
          wn.do.createItems (its);
        END;
      ELSE (* skip *)
      END;
    EXCEPT
    | VBT.Error => (* SKIP *)
    END;
  END PasteSource;

PROCEDURE BreakLines (text: TEXT): TextSeq.T =
  VAR tseq := NEW (TextSeq.T).init ();
      lastNL := -1;
      nextNL: INTEGER;
      length := Text.Length (text);
  BEGIN
    REPEAT
      nextNL := Text.FindChar (text, '\n', lastNL+1);
      IF nextNL = -1 THEN nextNL := length END;
      tseq.addhi (Text.Sub (text, lastNL+1, nextNL-lastNL-1));
      lastNL := nextNL;
    UNTIL nextNL = length;
    RETURN tseq;
  END BreakLines;

(* old 
PROCEDURE AdjustTextBox (wn: T; it: TextItem.T) =
  VAR focus := View.GetFocus (wn);
      rect := Rect.Add (VBT.BoundingBox (wn, it.text, wn.font),
                        Trans.PointB2W (it.rp, focus));
      (* The above assumes that 
         wn.font = ItemFont.ToFont (it.font, focus.scale) *)
  BEGIN
    IF Rect.IsEmpty (rect) THEN 
      it.box := RectR.T {west := it.rp.h, east := it.rp.h, 
                         north := it.rp.v, south := it.rp.v};
    ELSE
      it.box := Trans.RectW2B (rect, focus);
    END;
  END AdjustTextBox;
*)


PROCEDURE AdjustTextBox (wn: T; it: TextItem.T) =
<* FATAL ItemFont.TooSmall, ItemFont.TooBig, ItemFont.Invisible *>
  VAR scale := 10.0/ItemFont.Size (it.font);
      font := ItemFont.ToFont (it.font, scale);
      bb := VBT.BoundingBox (wn, it.text, font);
      width := FLOAT (bb.east-bb.west)*1.1;
  BEGIN
    it.box := RectR.Add (RectR.T{west := FLOAT (bb.west)/scale,
                                 east := (FLOAT (bb.west)+width)/scale,
                                 north := FLOAT (bb.north)/scale,
                                 south := FLOAT (bb.south)/scale},
                         it.rp);
(*
HighlightVBT.SetRect (wn, Rect.Add (Trans.RectB2W (it.box, View.GetFocus (wn)),
                                 ParentDelta (wn)));
*)   
  END AdjustTextBox;


PROCEDURE ChangeFont (wn: T; fontName: TEXT) =
  BEGIN
    wn.itemFont := ItemFont.FromName  (fontName);
    wn.font := FontCache.Get (fontName);
    IF wn.status = Status.Typing THEN 
      wn.curText := NIL;
      wn.curStart := wn.cursor.h;
      PaintCursor (wn);
    END;
  END ChangeFont;

PROCEDURE ChangeColor (wn: T; color: Color.T) =
  BEGIN
    IF wn.status = Status.Typing THEN
      wn.curText := NIL;
      wn.curStart := wn.cursor.h;
    END;
    wn.color := color;
  END ChangeColor;

PROCEDURE Position (wn: T; READONLY cd: VBT.PositionRec) =
  BEGIN
    CASE wn.status OF
    | Status.Dragging =>
      VAR delta := Point.Sub (wn.pointer, cd.cp.pt);
      BEGIN
        wn.pointer := cd.cp.pt;
        View.ChangeOffset (wn, Trans.PointW2B (delta, View.GetFocus (wn)));
        VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
      END;
    | Status.Panning =>
      VAR delta := Point.Sub (cd.cp.pt, wn.pointer);
      BEGIN
        wn.pointer := cd.cp.pt;
        View.ChangeOffset (wn, Trans.PointW2B (delta, View.GetFocus (wn)));
        VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
      END;
    | Status.Magnifying, Status.Reducing =>
      BEGIN
        wn.pointer2 := cd.cp.pt;
        VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
      END;
    | Status.Selecting =>
      VAR rect1 := Rect.FromCorners (wn.pointer, cd.cp.pt);
          rect2 := Rect.Add (rect1, ParentDelta (wn));
      BEGIN
        HighlightVBT.SetRect (wn, rect2);
        View.SetSelectionBox (wn, rect1);
        VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
      END;
    | Status.Ruling =>
      VAR rect := Rect.Add (Rect.FromCorners (wn.pointer, cd.cp.pt),
                            ParentDelta (wn));
      BEGIN
        HighlightVBT.SetRect (wn, rect);
        wn.pointer2 := cd.cp.pt;
        VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
      END;
    | Status.Moving =>
      VAR displ := Point.Sub (cd.cp.pt, wn.pointer);
          rect := Rect.Add (wn.moveBox, Point.Add (displ, ParentDelta (wn)));
      BEGIN
        HighlightVBT.SetRect (wn, rect);
        wn.pointer2 := cd.cp.pt;
      END;
      VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
    ELSE 
      VBT.SetCage (wn, VBT.EverywhereCage);
    END;
  END Position;

PROCEDURE Key (wn: T; READONLY cd: VBT.KeyRec) =
  VAR code := cd.whatChanged;
      focus := View.GetFocus (wn);
  BEGIN
    IF NOT wn.status = Status.Typing OR 
      NOT cd.wentDown OR
      VBT.Modifier.Control IN cd.modifiers THEN 
      RETURN;
    END;

    CASE code OF
    | KeyboardKey.Return =>
      VAR bb := VBT.BoundingBox (wn, "|", wn.font); BEGIN
        wn.curText := NIL;
        INC (wn.cursor.v, bb.south - bb.north);
        wn.cursor.h := wn.lmargin;
        PaintCursor (wn);
        wn.curStart := wn.cursor.h;
      END;
    | KeyboardKey.BackSpace, KeyboardKey.Delete => 
      IF wn.curText # NIL THEN 
        VAR oldBox := wn.curText.box; BEGIN
          wn.curText.text := Text.Sub (wn.curText.text, 0, 
                               MAX (0, Text.Length(wn.curText.text)-1));
          AdjustTextBox (wn, wn.curText);
          wn.cursor.h := wn.curStart + 
                             VBT.TextWidth (wn, wn.curText.text, wn.font);
          PaintCursor (wn);
          VAR its := NEW (Item.TArray, 1); BEGIN
            its[0] := wn.curText;
            View.ModifyItems (wn, its, FALSE, oldBox);
            IF its[0] = NIL THEN 
              wn.curText := NIL;
              wn.curStart := wn.cursor.h;
            END;
          END;
        END;
      END;
    ELSE
      IF code >= 0 AND code <= Latin1Key.ydiaeresis THEN
        VAR text := Text.FromChar(VAL(code, CHAR)); 
            its := NEW (Item.TArray, 1);
        BEGIN
          IF wn.curText = NIL THEN
            wn.curText := NEW (TextItem.T, text := text, 
                            rp := Trans.PointW2B (wn.cursor, focus), 
                            font := ItemFont.Scale (wn.itemFont, focus.scale),
                            color := wn.color);
            AdjustTextBox (wn, wn.curText);
            its[0] := wn.curText;
            wn.cursor.h := wn.curStart + 
                               VBT.TextWidth (wn, wn.curText.text, wn.font);
            PaintCursor (wn);
            wn.do.createItems (its);
            (* it looks better to paint the cursor BEFORE the text *)
          ELSE
            wn.curText.text := wn.curText.text & text;
            AdjustTextBox (wn, wn.curText);
            its[0] := wn.curText;
            wn.cursor.h := wn.curStart + 
                               VBT.TextWidth (wn, wn.curText.text, wn.font);
            PaintCursor (wn);
            View.ModifyItems (wn, its, TRUE, RectR.Empty);
            IF its[0] = NIL THEN (* the old text item was deleted *)
              wn.curText := NEW (TextItem.T, text := text, 
                            rp := Trans.PointW2B (wn.cursor, focus), 
                            font := ItemFont.Scale (wn.itemFont, focus.scale),
                            color := wn.color);
              AdjustTextBox (wn, wn.curText);
              its[0] := wn.curText;
              wn.curStart := wn.cursor.h;
              PaintCursor (wn);
              wn.do.createItems (its);
            END;
          END;
        END;
      END;
    END;
  END Key;

PROCEDURE Misc (wn: T; READONLY cd: VBT.MiscRec) =
  BEGIN
    IF cd.type = VBT.TakeSelection THEN
      IF cd.selection = VBT.KBFocus AND wn.status = Status.Typing THEN
        TRY
          VBT.Acquire (wn, cd.selection, cd.time);
          INC (wn.kbCount);
          PaintCursor (wn);
        EXCEPT
          VBT.Error => (*SKIP*)
        END;
      ELSE (* SKIP *)
      END;
    ELSIF cd.type = VBT.Lost THEN
      IF cd.selection = VBT.KBFocus THEN
        DEC (wn.kbCount);
        IF wn.status = Status.Typing THEN
          PaintCursor (wn);
        END;
      END;
    END;
  END Misc;


TYPE ZoomClosure = Thread.Closure OBJECT
    wn: T;
    zoomIndex: INTEGER;
  OVERRIDES 
    apply := Zoom;
  END;

PROCEDURE Zoom (cl: ZoomClosure): REFANY =
  VAR wn := cl.wn;
      newFocus := NEW (Focus.T);
  BEGIN
    WHILE wn.status = Status.Magnifying OR wn.status = Status.Reducing DO
        IF NOT (wn.status = Status.Magnifying OR wn.status = Status.Reducing)
          OR cl.zoomIndex # wn.zoomIndex THEN
          RETURN NIL;
        END;
        VAR focus := View.GetFocus (wn);
            delta := Point.Sub (wn.pointer, wn.pointer2);
            offset := Trans.PointW2B (delta, focus);
            f: REAL;
            now := Time.Now ();
            interval := MAX (0.1, MIN (1.0, FLOAT(now-wn.lastZoom)));
            zoomFactor := 1.0 + interval*wn.zoomRate; 
        BEGIN
          wn.pointer := wn.pointer2;
          IF wn.status = Status.Magnifying THEN
            newFocus.scale := focus.scale * zoomFactor;
          ELSE
            newFocus.scale := focus.scale / zoomFactor;
          END;
          f := 1.0/focus.scale - 1.0/newFocus.scale;
          newFocus.offset := PointR.T {
                             h := offset.h + FLOAT(wn.pointer.h) * f,
                             v := offset.v + FLOAT(wn.pointer.v) * f};
          View.ChangeFocus (wn, newFocus);
          wn.lastZoom := now;
        END;
    END;
    RETURN NIL;
  END Zoom;

PROCEDURE Magnifying (wn: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Nothing (wn);
    INC (wn.zoomIndex);
    wn.status := Status.Magnifying;
    wn.pointer := cd.cp.pt;
    wn.pointer2 := cd.cp.pt;
    wn.lastZoom := Time.Now ();
    VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
    EVAL Thread.Fork (NEW (ZoomClosure, wn := wn, zoomIndex := wn.zoomIndex));
  END Magnifying; 

PROCEDURE Reducing (wn: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    Nothing (wn);
    INC (wn.zoomIndex);
    wn.status := Status.Reducing;
    wn.pointer := cd.cp.pt;
    wn.pointer2 := cd.cp.pt;
    wn.lastZoom := Time.Now ();
    VBT.SetCage (wn, VBT.CageFromPosition (cd.cp));
    EVAL Thread.Fork (NEW (ZoomClosure, wn := wn, zoomIndex := wn.zoomIndex));
  END Reducing;

PROCEDURE ChangeZoomRate (wn: T; rate: REAL) =
  BEGIN
    wn.zoomRate := rate;
  END ChangeZoomRate;

<*INLINE*> PROCEDURE ParentDelta (wn: T): Point.T =
  BEGIN
    RETURN Point.Sub (Rect.NorthWest (VBT.Domain (wn.parent)), 
                      Rect.NorthWest (VBT.Domain (wn)));
  END ParentDelta;

PROCEDURE Undo (wn: T) = 
  BEGIN
    TRY
      wn.do.undo ();
    EXCEPT
    | Do.NoInfo => 
      IF wn.reportError # NIL THEN
        wn.reportError ("No more undo information");
      END;
    END;
  END Undo;

BEGIN
END Win.

