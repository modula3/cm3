(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:25:16 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:29 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE DisplayList;

IMPORT DPS, DPSWindow, Fmt, Err;

PROCEDURE NewBox (e: E; box: DPS.Box) = 
(* Could just offer NewBoxToParent. This is convenient for subclassers. *)
 VAR p: T;
  BEGIN
  p := e.parent;
  IF p # NIL THEN p.NewBoxOfChild (e, box); 
   ELSE e.box := box; (* New box of root. Needs repaint? *)
    END;
  END NewBox;

PROCEDURE NewBoxToParent (e: E; box: DPS.Box) =
 VAR p: T;
  BEGIN
  p := e.parent;
  IF p # NIL THEN p.NewBoxOfChild (e, box); 
   ELSE e.box := box; (* New box of root. Needs repaint? *)
    END;
  END NewBoxToParent;

PROCEDURE NewBoxOfChild (<*UNUSED*> t: T; e: E; box: DPS.Box) =
 VAR dirt: DPS.Box;
  BEGIN
  dirt := DPS.BoxUnion (e.box, box);
  e.box := box;
  e.Dirty (dirt, NIL);
  END NewBoxOfChild;

PROCEDURE Repaint (t: T; box: DPS.Box; only: REFANY): TEXT =
 VAR him, ret: TEXT := "";
 VAR ee: E;
  BEGIN
  TYPECASE only OF
  NULL => 
    ee := t.First();
    WHILE ee # NIL DO 
      him := ee.Repaint (box, only);
      IF him # NIL THEN ret := ret & him; END;
      ee := ee.Next(); 
      END;
  | E(eeonly) => 
    (*
    ee := t.First();
    WHILE ee # NIL DO 
      him := ee.Repaint (box, NIL);
      IF him # NIL THEN ret := ret & him; END;
      ee := ee.Next(); 
      END;
    *)
    (*
    ret := eeonly.Repaint (box, only);
    *)
    ee := t.First();
    WHILE ee # NIL DO 
      IF ee = eeonly THEN RETURN ee.Repaint (box, only); END;
      ee := ee.Next(); 
      END;
    (* If 'only' not child, have to traverse tree so ancestors can warp. *)
    ee := t.First();
    WHILE ee # NIL DO 
      him := ee.Repaint (box, only);
      IF him # NIL THEN ret := ret & him; END;
      ee := ee.Next(); 
      END;
   ELSE Err.Msg ("Bad -only- in DisplayList.Repaint");
    END;
  RETURN ret;
  END Repaint;

PROCEDURE DirtyToWindow (r: R; box: DPS.Box; only: T := NIL) =
  BEGIN
  r.window.Dirty (box, only);
  END DirtyToWindow;

PROCEDURE DirtyToParent (e: E; box: DPS.Box; only: T := NIL) =
 VAR p: T;
  BEGIN
  p := e.parent;
  IF p # NIL THEN p.Dirty(box, only); END;
  END DirtyToParent;

PROCEDURE PostScriptToParent (e: E; script: TEXT) =
 VAR p: T;
  BEGIN
  p := e.parent;
  IF p # NIL THEN p.ImmediatePostScript(script); END;
  END PostScriptToParent;

PROCEDURE PostScriptToWindow (r: R; script: TEXT) =
 <*FATAL DPS.BadPostScript*>
 VAR w: DPSWindow.T;
  BEGIN
  w := r.window;
  IF w # NIL THEN w.Send(script); END;
  END PostScriptToWindow;

PROCEDURE Mouse (t: T; window: DPSWindow.T; event: DPS.MouseEvent): BOOLEAN =
 VAR e: E;
  BEGIN
  e := t.Last();
  WHILE e # NIL DO 
    IF e.Mouse (window, event) THEN RETURN TRUE; END; 
    e := e.Previous(); 
    END;
  (* Used to kill input focus if no-one accepted mouse. *)
  RETURN FALSE;
  END Mouse;

PROCEDURE CharR (r: R; window: DPSWindow.T; char: CHAR): BOOLEAN =
 VAR e: E;
  BEGIN
  e := r.childWithInputFocus;
  IF e # NIL THEN RETURN e.Char (window, char); END;
  Err.Msg ("Discarded Char: ", Fmt.Int(ORD(char)));
  RETURN FALSE;
  END CharR;

PROCEDURE CharT (r: T; window: DPSWindow.T; char: CHAR): BOOLEAN =
 VAR e: E;
  BEGIN
  e := r.childWithInputFocus;
  IF e # NIL THEN RETURN e.Char (window, char); END;
  Err.Msg ("Discarded Char: ", Fmt.Int(ORD(char)));
  RETURN FALSE;
  END CharT;

PROCEDURE Key (t: T; window: DPSWindow.T; event: DPS.KeyEvent) =
 VAR got: CHAR;
 VAR e: E;
  BEGIN
  got := DPS.CharFromKey (event.key, event.modifiers);
  IF got = '\000' THEN RETURN; END;
  e := t.Last();
  WHILE e # NIL DO 
    IF e.Char (window, got) THEN RETURN; END; 
    e := e.Previous(); 
    END;
  Err.Msg ("Ignored Keystroke = ",Fmt.Int(event.key)," = ",Fmt.Int(ORD(got)));
  END Key;

PROCEDURE GetInputFocus (t: T; e: E := NIL) =
 VAR c: E;
 VAR p: T;
  BEGIN
  IF e = NIL THEN (* Start of a call. *)
    KillAnyInputFocusDownFromHere (t);
    END;
  IF e # NIL THEN (* Recursive, internal call. *)
    c := t.childWithInputFocus;
    (*
    IF c = e THEN RETURN; END;
    IF c # NIL THEN c.LoseInputFocus (); END;
    *)
    IF (c#e) AND (c#NIL) THEN c.LoseInputFocus (); END;
    t.childWithInputFocus := e;
    END;
  p := t.parent; IF p # NIL THEN p.GetInputFocus(t); END;
  IF e = NIL THEN (* Debugging. *)
    ForceInputFocusToHere (t);
    END;
  END GetInputFocus;

PROCEDURE ForceInputFocusToHere (e: E) =
 VAR parent: E;
  BEGIN
  e.childWithInputFocus := NIL;
  WHILE e.parent # NIL DO 
    parent := e.parent; parent.childWithInputFocus := e;
    e := e.parent;
   END;
  END ForceInputFocusToHere;

PROCEDURE KillAnyInputFocusDownFromHere (e: E) =
 VAR child: E;
  BEGIN
  IF e # NIL THEN
    child := e.childWithInputFocus;
    e.LoseInputFocus();
    e.childWithInputFocus := NIL; (* In case he doesn't. *)
    KillAnyInputFocusDownFromHere (child); 
    END;
  END KillAnyInputFocusDownFromHere;

PROCEDURE LoseInputFocus (<*UNUSED*> t: T) =
  BEGIN
  END LoseInputFocus;

PROCEDURE KillInputFocus (t: T) =
 VAR e, ee: E;
  BEGIN
  e := t.childWithInputFocus;
  WHILE e # NIL DO 
    ee := e.childWithInputFocus;
    e.LoseInputFocus(); (* Is it OK to work downward? *) 
    e := ee; 
    END;
  END KillInputFocus;

  BEGIN

  END DisplayList.

