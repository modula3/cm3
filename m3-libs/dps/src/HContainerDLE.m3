(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Wed Aug 17 16:30:21 PDT 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:26 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)


MODULE HContainerDLE;

IMPORT DisplayList, DPS, DPSWindow, Linked2Tree, Err;

PROCEDURE Repaint (e: E; box: DPS.Box; only: REFANY := NIL): TEXT =
  BEGIN
  TYPECASE only OF
    NULL => RETURN DisplayList.Repaint (e, box, NIL); 
            (* Goal: paint my children. *) 
  | E(ee)=> 
      IF ee = e THEN
        RETURN DisplayList.Repaint (e, box, NIL);
       ELSE
        Err.Msg ("ee # e in HContainerDLE.Repaint");
        RETURN ee.Repaint (box, only); 
        END; 
    ELSE
      <*ASSERT FALSE*> 
    END;
  END Repaint;

PROCEDURE Initialize (e: E;  <*UNUSED*> window: DPSWindow.T) =
  BEGIN
  IF e.initialized THEN RETURN; END;
  e.initialized := TRUE;
  END Initialize;

PROCEDURE Mouse (e: E; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
  BEGIN
  Initialize(e, window);
  RETURN DisplayList.Mouse (e, window, event);
  END Mouse;

PROCEDURE Char (t: T; window: DPSWindow.T; char: CHAR): BOOLEAN =
 VAR e: E;
  BEGIN
  Initialize(t, window);
  e := t.childWithInputFocus;
  IF e#NIL THEN RETURN e.Char (window, char); END;
  RETURN FALSE;
  END Char;

PROCEDURE Prepend (t: Linked2Tree.T; e: Linked2Tree.E) =
  BEGIN
  Linked2Tree.Prepend (t, e);
  Rearrange (t);
  END Prepend;

PROCEDURE Append (t: Linked2Tree.T; e: Linked2Tree.E) =
  BEGIN
  Linked2Tree.Append (t, e);
  Rearrange (t);
  END Append;

PROCEDURE Remove (e: Linked2Tree.E) =
 VAR me: T;
  BEGIN
  me := e.parent;
  Linked2Tree.Remove (e);
  Rearrange (me);
  END Remove;

PROCEDURE MakeChildLast (t: Linked2Tree.T;
                         <*UNUSED*> e: Linked2Tree.E): Linked2Tree.E =
  BEGIN
  (* Since our children are not overlapped, we can maintain their order. *)
  (* Important as we use the ordering as the left-to-right order. *)
  RETURN t.MoveToLast();
  END MakeChildLast;

PROCEDURE Rearrange (me: T) =
 VAR ee: DisplayList.E;
 VAR x, h, w, maxH: REAL;
  BEGIN
  maxH := 0.0;
  x := me.box.low.x;
  ee := me.First();
  IF ee = NIL THEN me.box.high.x := x; RETURN; END;
  WHILE ee # NIL DO
    w := ee.box.high.x - ee.box.low.x;
    h := ee.box.high.y - ee.box.low.y;
    maxH := MAX (maxH, h);
    ee.box.low.y := me.box.low.y; ee.box.high.y := ee.box.low.y + h;
    ee.box.low.x := x; ee.box.high.x := x + w;
    x := x + w + me.separation;
    ee := ee.Next();
    END;
  me.box.high.x := x - me.separation;
  me.box.high.y := me.box.low.y + maxH;
  END Rearrange;

  BEGIN
  END HContainerDLE.

