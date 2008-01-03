(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 11:48:12 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:25 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

MODULE Linked2Tree;

IMPORT Thread;

PROCEDURE Init (t: T) =
  BEGIN
  t.firstChild := NIL;
  t.lastChild := NIL;
  t.monitor := Thread.NewMutex();
  END Init; 

PROCEDURE Prepend (t: T; e: T) =
  BEGIN
  LOCK t.monitor DO PrependWhileLocked (t, e); END;
  END Prepend;
 
PROCEDURE PrependWhileLocked (t: T; e: T) =
  BEGIN
  <* ASSERT (e.parent = NIL) *>  e.parent := t; 
  IF t.firstChild=NIL THEN
    t.firstChild := e; t.lastChild := e;
    e.nextSibling := NIL; e.previousSibling := NIL;
   ELSE
    t.firstChild.previousSibling := e; e.next := t.firstChild;
    e.previousSibling := NIL; t.firstChild := e;
    END;
  END PrependWhileLocked;
 
PROCEDURE Append (t: T; e: T) =
  BEGIN
  LOCK t.monitor DO AppendWhileLocked (t, e); END;
  END Append;
 
PROCEDURE AppendWhileLocked (t: T; e: T) =
  BEGIN
  <* ASSERT (e.parent = NIL) *>  e.parent := t;
  IF t.first=NIL THEN
    t.firstChild := e; t.lastChild := e; 
    e.nextSibling := NIL; e.previousSibling := NIL;
   ELSE
    t.last.nextSibling := e; e.previousSibling := t.lastChild;
    e.next := NIL; t.lastChild := e;
    END;
  END AppendWhileLocked;
 
PROCEDURE Remove (t: T; e: T) =
  BEGIN
  LOCK t.monitor DO RemoveWhileLocked (t, e); END;
  END Remove;
 
PROCEDURE RemoveWhileLocked (t: T; e: T) =
 VAR done: BOOLEAN;
 VAR past: E;
  BEGIN
  e.parent := NIL;
  done := FALSE;
  IF t.firstChild=e THEN 
    t.firstChild := e.next; 
    IF t.firstChild#NIL THEN t.firstChild.previousSibling := NIL; END;
    done := TRUE; 
    END;
  IF t.lastChild=e THEN 
    t.lastChild := e.previousSibling; 
    IF t.lastChild#NIL THEN t.lastChild.nextSibling := NIL; END;
    done := TRUE;
    END;
  IF NOT done THEN
    past := e.next;
    e.next.previousSibling := e.previousSibling;
    e.previousSibling.next := past;
    END;
  END RemoveWhileLocked;
 
PROCEDURE First (t: T): E =
  BEGIN
  LOCK t.monitor DO RETURN t.firstChild; END;
  END First;
 
PROCEDURE Next (t: T; e: E): E =
  BEGIN
  LOCK t.monitor DO RETURN e.nextSibling; END;
  END Next;
 
PROCEDURE Last (t: T): E =
  BEGIN
  LOCK t.monitor DO RETURN t.lastChild; END;
  END Last;
 
PROCEDURE Previous (t: T; e: E): E =
  BEGIN
  LOCK t.monitor DO RETURN e.previousSibling; END;
  END Previous;
 
PROCEDURE InternalMoveToFirst (t: T; e: E): BOOLEAN =
  BEGIN
  LOCK t.monitor DO 
    IF e = t.first THEN RETURN FALSE; END;
    RemoveWhileLocked (t, e);
    PrependWhileLocked (t, e);
    RETURN TRUE; 
    END;
  END InternalMoveToFirst;
 
PROCEDURE InternalMoveToLast (t: T; e: T): BOOLEAN =
  BEGIN
  LOCK t.monitor DO 
    IF e = t.last THEN RETURN FALSE; END;
    RemoveWhileLocked (t, e);
    AppendWhileLocked (t, e);
    RETURN TRUE; 
    END;
  END InternalMoveToLast;
 
PROCEDURE MoveToFirst (e: T): BOOLEAN =
  BEGIN
  RETURN InternalMoveToFirst(e.parent, e);
  END MoveToFirst;
 
PROCEDURE MoveToLast (e: T): BOOLEAN =
  BEGIN
  RETURN InternalMoveToLast(e.parent, e);
  END MoveToLast;
 
  BEGIN

  END Linked2List.

