(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Wed Aug 17 16:30:02 PDT 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:12 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE VContainerDLE;

IMPORT DisplayList, DPS, DPSWindow, Linked2Tree, Err;

PROCEDURE Repaint (e: E; box: DPS.Box; only: REFANY := NIL): TEXT =
  BEGIN
  TYPECASE only OF
    NULL => RETURN DisplayList.Repaint (e, box, NIL); (* Paint my children. *) 
  | E(ee)=> 
      IF ee = e THEN RETURN DisplayList.Repaint (e, box, NIL); 
       ELSE
        Err.Msg ("ee # e in VContainerDLE.Repaint");
        RETURN ee.Repaint (box, only); 
        END; 
    ELSE RETURN NIL;
    END;
  END Repaint;

PROCEDURE Init (e: E; <*UNUSED*> window: DPSWindow.T) =
  BEGIN
  IF e.initialized THEN RETURN; END;
  Rearrange (e);
  e.initialized := TRUE;
  END Init;

PROCEDURE Mouse (e: E; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
  BEGIN
  Init(e, window);
  RETURN DisplayList.Mouse (e, window, event);
  END Mouse;

PROCEDURE Char (t: T; window: DPSWindow.T; char: CHAR): BOOLEAN =
 VAR e: DisplayList.T;
  BEGIN
  e := t.childWithInputFocus;
  IF e # NIL THEN RETURN e.Char (window, char);END;
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
  (* Since our children are not overlapped, we can leave their order alone. *)
  (* Important, as we use the order as our bottom-to-top ordering. *)
  RETURN t.MoveToLast();
  END MakeChildLast;

PROCEDURE Rearrange (me: T) =
 VAR ee: DisplayList.E;
 VAR y, h, w, maxW: REAL;
  BEGIN
  maxW := 0.0;
  y := me.box.low.y;
  ee := me.First(); (* Forward, so up in Display PostScript. *)
  IF ee = NIL THEN me.box.high.y := y; RETURN; END;
  WHILE ee # NIL DO
    w := ee.box.high.x - ee.box.low.x;
    h := ee.box.high.y - ee.box.low.y;
    maxW := MAX (maxW, w);
    ee.box.low.x := me.box.low.x; ee.box.high.x := ee.box.low.x + w;
    ee.box.low.y := y; ee.box.high.y := y + h;
    y := y + h + me.separation;
    ee := ee.Next();
    END;
  me.box.high.y := y - me.separation;
  me.box.high.x := me.box.low.x + maxW;
  END Rearrange;

  BEGIN
  END VContainerDLE.

