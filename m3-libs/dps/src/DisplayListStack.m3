(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 10:56:43 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:28 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE DisplayListStack;

IMPORT DisplayList, DPS, Linked2Tree;
 
PROCEDURE NewBoxOfChild (t: T; e: DisplayList.E; box: DPS.Box) =
  BEGIN
  e.box := box;
  t.Stack();
  END NewBoxOfChild;
 
PROCEDURE Prepend (t: T; e: Linked2Tree.E) =
  BEGIN
  Linked2Tree.Prepend (t, e);
  t.Stack();
  END Prepend;
 
PROCEDURE Append (t: T; e: Linked2Tree.E) =
  BEGIN
  Linked2Tree.Append (t, e);
  (* t.Stack(); *)
  EVAL StackInternal (t);
  END Append;
 
PROCEDURE InsertBefore (t: T; e, before: Linked2Tree.E) =
  BEGIN
  Linked2Tree.InsertBefore (t, e, before);
  t.Stack();
  END InsertBefore;
 
PROCEDURE InsertAfter (t: T; e, after: Linked2Tree.E) =
  BEGIN
  Linked2Tree.InsertAfter (t, e, after);
  (* t.Stack(); *)
  EVAL StackInternal (t);
  END InsertAfter;
 
PROCEDURE RemoveChild (t: T; e: Linked2Tree.E) =
  BEGIN
  Linked2Tree.RemoveChild (t, e);
  t.Stack();
  END RemoveChild;
 
PROCEDURE Stack (t: T; firstHighY: REAL := -1.0) =
 VAR dirtyBox: DPS.Box;
  BEGIN
  dirtyBox := StackInternal (t, firstHighY);
  IF dirtyBox # DPS.ZeroBox THEN t.Dirty (dirtyBox, NIL); END;
  END Stack;
 
PROCEDURE StackInternal (t: T; firstHighY: REAL := -1.0): DPS.Box =
 VAR dd: DisplayList.E;
 VAR y: REAL;
 VAR dirtyBox: DPS.Box := DPS.ZeroBox;
  BEGIN
  dd := t.First();
  IF dd = NIL THEN RETURN DPS.ZeroBox; END;
  IF firstHighY < 0.0 THEN y := dd.box.low.y; (* First one stands as-is. *)
   ELSE (* Restack everyone. *)
    y := firstHighY;
    IF dd.box.high.y # y THEN 
      IF dirtyBox = DPS.ZeroBox THEN dirtyBox := dd.box;
       ELSE dirtyBox := DPS.BoxUnion (dirtyBox, dd.box);
        END;
      dd.box.low.y := dd.box.low.y + (y - dd.box.high.y);
      dd.box.high.y := y;
      dirtyBox := DPS.BoxUnion (dirtyBox, dd.box);
      END;
    y := dd.box.low.y;
    END;
  dd := dd.Next();
  WHILE dd # NIL DO
    IF dd.box.high.y # y THEN 
      IF dirtyBox = DPS.ZeroBox THEN dirtyBox := dd.box;
       ELSE dirtyBox := DPS.BoxUnion (dirtyBox, dd.box);
        END;
      dd.box.low.y := dd.box.low.y + (y - dd.box.high.y);
      dd.box.high.y := y;
      dirtyBox := DPS.BoxUnion (dirtyBox, dd.box);
      END;
    y := dd.box.low.y;
    dd := dd.Next();
    END;
  RETURN dirtyBox
  END StackInternal;
 
  BEGIN

  END DisplayListStack.

