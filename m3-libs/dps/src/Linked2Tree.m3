(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 11:47:08 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:24 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)


MODULE Linked2Tree;

PROCEDURE Prepend (t: T; e: E) =
  BEGIN
  LOCK t DO PrependWhileLocked (t, e); END;
  END Prepend;
 
PROCEDURE PrependWhileLocked (t: T; e: E) =
  BEGIN
  <* ASSERT (e.parent = NIL) *>  e.parent := t; 
  IF t.firstChild=NIL THEN
    t.firstChild := e; t.lastChild := e;
    e.nextSibling := NIL; e.previousSibling := NIL;
   ELSE
    t.firstChild.previousSibling := e; e.nextSibling := t.firstChild;
    e.previousSibling := NIL; t.firstChild := e;
    END;
  END PrependWhileLocked;
 
PROCEDURE Append (t: T; e: E) =
  BEGIN
  LOCK t DO AppendWhileLocked (t, e); END;
  END Append;
 
PROCEDURE AppendWhileLocked (t: T; e: E) =
  BEGIN
  <* ASSERT (e.parent = NIL) *>  e.parent := t;
  IF t.firstChild=NIL THEN
    t.firstChild := e; t.lastChild := e; 
    e.nextSibling := NIL; e.previousSibling := NIL;
   ELSE
    t.lastChild.nextSibling := e; e.previousSibling := t.lastChild;
    e.nextSibling := NIL; t.lastChild := e;
    END;
  END AppendWhileLocked;
 
PROCEDURE InsertBefore (t: T; e, before: E) =
  BEGIN
  LOCK t DO InsertBeforeWhileLocked (t, e, before); END;
  END InsertBefore;
 
PROCEDURE InsertBeforeWhileLocked (t: T; e, before: E) =
  BEGIN
  IF before = NIL THEN AppendWhileLocked (t, e); RETURN; END;
  <* ASSERT (before.parent = t) *> 
  IF before.previousSibling=NIL THEN PrependWhileLocked (t, e); RETURN; END;
  <* ASSERT (e.parent = NIL) *>  e.parent := t;
  before.previousSibling.nextSibling := e;
  e.previousSibling := before.previousSibling; e.nextSibling := before;
  before.previousSibling := e;
  END InsertBeforeWhileLocked;
 
PROCEDURE InsertAfter (t: T; e, after: E) =
  BEGIN
  LOCK t DO InsertAfterWhileLocked (t, e, after); END;
  END InsertAfter;
 
PROCEDURE InsertAfterWhileLocked (t: T; e, after: E) =
  BEGIN
  IF after = NIL THEN AppendWhileLocked (t, e); RETURN; END;
  <* ASSERT (after.parent = t) *> 
  IF after.nextSibling=NIL THEN AppendWhileLocked (t, e); RETURN; END;
  <* ASSERT (e.parent = NIL) *>  e.parent := t;
  after.nextSibling.previousSibling := e;
  e.previousSibling := after; e.nextSibling := after.nextSibling;
  after.nextSibling := e;
  END InsertAfterWhileLocked;
 
PROCEDURE Remove (e: E) =
 VAR t: T;
  BEGIN
  t := e.parent;
  t.RemoveChild (e); 
  END Remove;
 
PROCEDURE RemoveChild (t: T; e: E) =
  BEGIN
  RemoveInternal (t, e); 
  END RemoveChild;
 
PROCEDURE RemoveInternal (t: T; e: E) =
  BEGIN
  LOCK t DO RemoveWhileLocked (t, e); END;
  END RemoveInternal;
 
PROCEDURE RemoveWhileLocked (t: T; e: E) =
 VAR done: BOOLEAN;
 VAR past: E;
  BEGIN
  e.parent := NIL;
  done := FALSE;
  IF t.firstChild=e THEN 
    t.firstChild := e.nextSibling; 
    IF t.firstChild#NIL THEN t.firstChild.previousSibling := NIL; END;
    done := TRUE; 
    END;
  IF t.lastChild=e THEN 
    t.lastChild := e.previousSibling; 
    IF t.lastChild#NIL THEN t.lastChild.nextSibling := NIL; END;
    done := TRUE;
    END;
  IF NOT done THEN
    past := e.nextSibling;
    e.nextSibling.previousSibling := e.previousSibling;
    e.previousSibling.nextSibling := past;
    END;
  END RemoveWhileLocked;
 
PROCEDURE First (t: T): E =
  BEGIN
  LOCK t DO RETURN t.firstChild; END;
  END First;
 
PROCEDURE Next (e: E): E =
  BEGIN (* Should lock e.parent? *)
  LOCK e DO RETURN e.nextSibling; END;
  END Next;
 
PROCEDURE Last (t: T): E =
  BEGIN
  LOCK t DO RETURN t.lastChild; END;
  END Last;
 
PROCEDURE Previous (e: E): E =
  BEGIN (* Should lock e.parent? *)
  LOCK e DO RETURN e.previousSibling; END;
  END Previous;
 
PROCEDURE InternalMoveToFirst (t: T; e: E): E =
 BEGIN
  LOCK t DO 
    IF e = t.firstChild THEN RETURN NIL; END;
    RemoveWhileLocked (t, e);
    PrependWhileLocked (t, e);
    RETURN e.parent; (* Hmmm. *)
    END;
  END InternalMoveToFirst;
 
PROCEDURE InternalMoveToLast (t: T; e: E): E =
  BEGIN
  LOCK t DO 
    IF e = t.lastChild THEN RETURN NIL; END;
    RemoveWhileLocked (t, e);
    AppendWhileLocked (t, e);
    RETURN e; 
    END;
  END InternalMoveToLast;
 
PROCEDURE MoveToFirst (e: E): E =
 VAR p: T;
  BEGIN
  p := e.parent;
  IF p=NIL THEN RETURN NIL; ELSE RETURN p.MakeChildFirst(e); END;
  END MoveToFirst;
 
PROCEDURE MakeChildFirst (t: T; e: E): E =
  BEGIN
  RETURN InternalMoveToFirst(t, e);
  END MakeChildFirst;
 
PROCEDURE MoveToLast (e: E): E =
 VAR p: T;
  BEGIN
  p := e.parent;
  IF p=NIL THEN RETURN NIL; ELSE RETURN p.MakeChildLast(e); END;
  END MoveToLast;
 
PROCEDURE MakeChildLast (t: T; e: E): E =
  BEGIN
  RETURN InternalMoveToLast(t, e);
  END MakeChildLast;
 
  BEGIN

  END Linked2Tree.

