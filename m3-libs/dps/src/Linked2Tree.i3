(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 11:49:09 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:25 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE Linked2Tree;  

TYPE E = MUTEX OBJECT 
  parent: E := NIL; (* Really always a T. *)
  nextSibling, previousSibling: E := NIL;
 METHODS
  Remove () := Remove;
  Next (): E := Next;
  Previous (): E := Previous;
  MoveToFirst (): E := MoveToFirst;
  MoveToLast (): E := MoveToLast;
  END;

TYPE T = E OBJECT 
  firstChild, lastChild: E := NIL;
 METHODS
  Prepend (e: E) := Prepend;
  Append (e: E) := Append;
  InsertBefore (e, before: E) := InsertBefore;
  InsertAfter (e, after: E) := InsertAfter;
  First (): E := First;
  Last (): E := Last;
  MakeChildLast (e: E): E := MakeChildLast;
  MakeChildFirst (e: E): E := MakeChildFirst;
  RemoveChild (e: E) := RemoveChild;
  END;

PROCEDURE Prepend (t: T; e: E);
PROCEDURE Append (t: T; e: E);
PROCEDURE InsertBefore (t: T; e, before: E);
PROCEDURE InsertAfter (t: T; e, after: E);
PROCEDURE Remove (e: E);

PROCEDURE First (t: T): E;
PROCEDURE Last (t: T): E;

PROCEDURE Next (e: E): E;
PROCEDURE Previous (e: E): E;

PROCEDURE MoveToFirst (e: E): E;
PROCEDURE MoveToLast (e: E): E;

PROCEDURE MakeChildLast (t: T; e: E): E;
PROCEDURE MakeChildFirst (t: T; e: E): E;
PROCEDURE RemoveChild (t: T; e: E);

  END Linked2Tree.





