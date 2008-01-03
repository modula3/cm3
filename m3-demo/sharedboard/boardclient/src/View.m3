(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)

MODULE View;

IMPORT VBT, Region, Rect, PaintOp, Point, DblBufferVBT,
       Thread, Atom, NetObj,
       Item, ItemClass, ItemTbl, ItemList,
       Board, ClientInfo, CallbackX, 
       Trans, Focus, RectR, PointR;

REVEAL T = Public BRANDED OBJECT
    mu: MUTEX;
    board: Board.T;
    ci: ClientInfo.T;

    focus: Focus.T;
    display: ItemTbl.T;
    selected: ItemList.T := NIL;
  OVERRIDES
    init := Init;
    repaint := Repaint;
    reshape := Reshape;
    refresh := Refresh;
    quit := Quit;
  END;

(* 
   The field "board" points to the board on which the view is
   set, and "ci" points to the "ClassInfo.T" object for this view
   maintained at the board. 
   The "focus" gives the position of the window's focus in board
   coordinates: the offset of the top-left corner and the scale.  

   The field "display" is the display list of items in "board" that are
   cached at the client;
   "selected" gives the list of items selected.
*)

<* FATAL Thread.Alerted *>

PROCEDURE Init (v: T; board: Board.T): VBT.T
  RAISES {NetObj.Error} =
  BEGIN 
    v.mu := NEW (MUTEX);
    v.board := board;
    v.ci := v.board.register (NEW (CallbackX.T).init (v));
    v.focus := NEW (Focus.T, offset := PointR.Origin, scale := 1.0);
    v.board.setScope (v.ci, RectR.Full);
    v.display := NEW (ItemTbl.Default).init ();
    RETURN NEW (DblBufferVBT.T).init (v);
  END Init;

PROCEDURE Error (v: T; text: TEXT) =
  BEGIN
    IF v.reportError # NIL THEN v.reportError (text) END;
  END Error; 

PROCEDURE Repaint (v: T; READONLY rgn: Region.T) =
  BEGIN
    LOCK v.mu DO
      v.refresh (rgn.r);
      VBT.Sync (v);
    END;
  END Repaint;

PROCEDURE Reshape (v: T; <*UNUSED *> READONLY cd: VBT.ReshapeRec) =
  BEGIN
    LOCK v.mu DO
      v.refresh (Rect.Full);
      VBT.Sync (v);
    END;
  END Reshape; 

PROCEDURE Refresh (v: T; READONLY r: Rect.T) =
  BEGIN
      VAR ir := v.display.iterate ();
          id: Item.ID;
          it: Item.T;
          rect := Rect.Meet (r, VBT.Domain (v));
          rectR := Trans.RectW2B (rect, v.focus);
      BEGIN
        IF Rect.IsEmpty (rect) THEN RETURN END;
        VBT.PaintTint (v, rect, op := PaintOp.Bg);
        WHILE ir.next (id, it) DO
          IF RectR.Overlap (it.box, rectR) THEN 
            it.paint (v, v.focus);
          END;
        END;
        HiliteList (v, v.selected);
      END;
  END Refresh; 

PROCEDURE HiliteList (v: T; il: ItemList.T) =
  BEGIN
    WHILE il # NIL DO
      il.head.hilite (v, v.focus);
      il := il.tail;
    END;
  END HiliteList; 

PROCEDURE UnhiliteList (v: T; il: ItemList.T) =
  BEGIN
    WHILE il # NIL DO
      il.head.unhilite (v, v.focus);
      il := il.tail;
    END;
  END UnhiliteList;  

PROCEDURE GetFocus (v: T): Focus.T =
  BEGIN
    RETURN v.focus;
  END GetFocus;

PROCEDURE ChangeFocus (v: T; focus: Focus.T) =
  BEGIN
    IF focus.scale = v.focus.scale THEN
      ChangeOffset (v, focus.offset);
      RETURN;
    END;
    LOCK v.mu DO
      v.focus.offset := focus.offset;
      v.focus.scale := focus.scale;
      v.refresh (Rect.Full);
      IF v.reportFocus # NIL THEN v.reportFocus (v.focus) END;
      VBT.Sync (v);
    END;
  END ChangeFocus;

PROCEDURE ChangeOffset (v: T; offset: PointR.T) =
  VAR delta := Trans.PointB2W (offset, v.focus);
      domain := VBT.Domain (v);
      overlap := Rect.Meet (domain, Rect.Sub (domain, delta));
      extra: Rect.Partition;
  BEGIN
    LOCK v.mu DO
      v.focus.offset := offset;
      VBT.Scroll (v, overlap, Point.Minus (delta));
      Rect.Factor (domain, overlap, extra, 0, 0);
      extra[2] := extra[4];
      FOR i := 0 TO 3 DO
        v.refresh (extra[i]);
      END;  
      IF v.reportFocus # NIL THEN v.reportFocus (v.focus) END;
      VBT.Sync (v);
    END;
  END ChangeOffset; 


PROCEDURE GetSelection (v: T): ItemList.T =
  BEGIN
    RETURN v.selected; 
  END GetSelection; 

PROCEDURE SelectItems (v: T; newSel: ItemList.T) =
  BEGIN
    LOCK v.mu DO
      VAR list1 := newSel; 
        list2 := v.selected; 
      BEGIN
        WHILE list1 # NIL AND list2 # NIL AND list1.head = list2.head DO
          list1 := list1.tail;
          list2 := list2.tail;
        END;
        UnhiliteList (v, list2);
        HiliteList (v, list1);
      END;
      v.selected := newSel;
      VBT.Sync (v);
    END;   
  END SelectItems;

PROCEDURE SelectOne (v: T; pt: Point.T) =
  BEGIN
    LOCK v.mu DO
      UnhiliteList (v, v.selected);
      v.selected := NIL;
      VAR ir := v.display.iterate ();
          id: Item.ID;
          it: Item.T;
          pp := Trans.PointW2B (pt, v.focus);
      BEGIN
        WHILE ir.next (id, it) DO
          IF RectR.Member (pp, it.box) THEN 
            v.selected := ItemList.List1 (it);
            EXIT;
          END;
        END;
      END;
      HiliteList (v, v.selected);
      VBT.Sync (v);
    END;
  END SelectOne;

PROCEDURE SetSelectionBox (v: T; box: Rect.T) =
  VAR newSel: ItemList.T := NIL;
  BEGIN
    LOCK v.mu DO
      IF NOT Rect.IsEmpty (box) THEN 
        VAR ir := v.display.iterate ();
            id: Item.ID;
            it: Item.T;
            rr := Trans.RectW2B (box, v.focus);
        BEGIN
          WHILE ir.next (id, it) DO
            IF RectR.Subset (it.box, rr) THEN 
              newSel := ItemList.Cons (it, newSel);
            END;
          END;
        END;
      END;
      VAR list1 := newSel; 
        list2 := v.selected; 
      BEGIN
        WHILE list1 # NIL AND list2 # NIL AND list1.head = list2.head DO
          list1 := list1.tail;
          list2 := list2.tail;
        END;
        UnhiliteList (v, list2);
        HiliteList (v, list1);
      END;
      v.selected := newSel;
      VBT.Sync (v);
    END;
  END SetSelectionBox;

PROCEDURE CreateItems (v: T; its: Item.TArray) =
  VAR ids: Item.IDArray;
  BEGIN
    IF its = NIL THEN RETURN END;
    TRY
      ids := v.board.createItems (v.ci, its);
      LOCK v.mu DO
        FOR i := FIRST (its^) TO LAST (its^) DO
          its[i].id := ids[i];
          EVAL v.display.put (ids[i], its[i]);
          its[i].paint (v, v.focus);
        END;
        VBT.Sync (v); 
      END;
    EXCEPT
      NetObj.Error (atom) => Error (v, Atom.ToText (atom.head)); 
    | Thread.Alerted => Error (v, "Thread.Alerted");
    END;
  END CreateItems;

PROCEDURE ItemsCreated (v: T; its: Item.TArray) =
  BEGIN
    IF its = NIL THEN RETURN END;
    LOCK v.mu DO
      FOR i := FIRST (its^) TO LAST (its^) DO
        EVAL v.display.put (its[i].id, its[i]);
        its[i].paint (v, v.focus);
      END;
      VBT.Sync (v); 
    END;
  END ItemsCreated; 

PROCEDURE ModifyItems (v: T; its: Item.TArray; additive: BOOLEAN;
                      oldBox: RectR.T) =
  BEGIN
    IF its = NIL THEN RETURN END;
    LOCK v.mu DO
      FOR i := FIRST (its^) TO LAST (its^) DO
        VAR tmp: Item.T; BEGIN
          IF NOT v.display.get (its[i].id, tmp) THEN
            its[i] := NIL;
          ELSE
            EVAL v.display.put (its[i].id, its[i]);
            IF additive OR NOT RectR.Overlap (its[i].box, oldBox) THEN
              IF ItemList.Member (v.selected, its[i]) THEN
                its[i].hilite (v, v.focus);
              ELSE
                its[i].paint (v, v.focus);
              END;
            END;
          END;
        END;
      END;
      IF NOT additive THEN
        v.refresh (Trans.RectB2W (oldBox, v.focus));
      END;
      VBT.Sync (v);
    END;
    TRY
      v.board.modifyItems (v.ci, its, additive);
    EXCEPT
      NetObj.Error (atom) => Error (v, Atom.ToText (atom.head)); 
    | Thread.Alerted => Error (v, "Thread.Alerted");
    END;
  END ModifyItems;

PROCEDURE ItemsModified (v: T; its: Item.TArray; additive: BOOLEAN) =
  VAR oldBox := RectR.Empty;
  BEGIN
    IF its = NIL THEN RETURN END;
    LOCK v.mu DO
      IF NOT additive THEN
        FOR i := FIRST (its^) TO LAST (its^) DO
          VAR old: Item.T; BEGIN
            IF its[i] = NIL OR NOT v.display.get (its[i].id, old) THEN
              its[i] := NIL;
            ELSE
              oldBox := RectR.Join (oldBox, old.box);
            END;
          END;
        END;
      END;
      FOR i := FIRST (its^) TO LAST (its^) DO
        IF its[i] # NIL THEN 
          EVAL v.display.put (its[i].id, its[i]);
          IF (additive OR NOT RectR.Overlap (its[i].box, oldBox)) THEN
            IF ItemList.Member (v.selected, its[i]) THEN
              its[i].hilite (v, v.focus);
            ELSE
              its[i].paint (v, v.focus);
            END;
          END;
        END;
      END;
      IF NOT additive THEN
        v.refresh (Trans.RectB2W (oldBox, v.focus));
      END;
      VBT.Sync (v);
    END;
  END ItemsModified; 

PROCEDURE DeleteItems (v: T; ids: Item.IDArray) =
  VAR oldBox := RectR.Empty;
  BEGIN
    TRY
      v.board.deleteItems (v.ci, ids);
    EXCEPT
    | NetObj.Error (atom) => Error (v, Atom.ToText (atom.head)); 
    | Thread.Alerted => Error (v, "Thread.Alerted");
    END;
    LOCK v.mu DO
      FOR i := FIRST (ids^) TO LAST (ids^) DO
        VAR old: Item.T; BEGIN
          IF v.display.delete (ids[i], old) THEN
            RemoveFromSelection (v, old);
            oldBox := RectR.Join (oldBox, old.box);
          END;
        END;      
      END;
      v.refresh (Trans.RectB2W (oldBox, v.focus));
      VBT.Sync (v);
    END;
  END DeleteItems; 

PROCEDURE ItemsDeleted (v: T; ids: Item.IDArray) =
  VAR oldBox := RectR.Empty;
  BEGIN
    IF ids = NIL THEN RETURN END;
    LOCK v.mu DO
      FOR i := FIRST (ids^) TO LAST (ids^) DO
        VAR old: Item.T; BEGIN
          IF v.display.delete (ids[i], old) THEN
            RemoveFromSelection (v, old);
            oldBox := RectR.Join (oldBox, old.box);
          END;
        END;      
      END;
      v.refresh (Trans.RectB2W (oldBox, v.focus));
      VBT.Sync (v); 
    END;
  END ItemsDeleted; 

PROCEDURE RemoveFromSelection (v: T; it: Item.T) =
  BEGIN
    IF NOT ItemList.Member (v.selected, it) THEN RETURN END;
    IF v.selected.head.id = it.id THEN
      v.selected := v.selected.tail;
    ELSE
      VAR list := v.selected; BEGIN
        WHILE list.tail.head.id # it.id DO
          list := list.tail;
        END;
        list.tail := list.tail.tail;
      END;
    END;
  END RemoveFromSelection;

PROCEDURE Quit (v: T) =
  BEGIN
    TRY
      v.board.unregister (v.ci);
    EXCEPT
      NetObj.Error (atom) => Error (v, Atom.ToText (atom.head)); 
    | Thread.Alerted => Error (v, "Thread.Alerted");
    END;
  END Quit; 

BEGIN
END View. 

