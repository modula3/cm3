(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 11:49:01 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:26 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)

INTERFACE Linked2List;  

IMPORT Text;

TYPE T = OBJECT 
  monitor: MUTEX;
  first: E; last: E
 METHODS
  Prepend (e: E) := Prepend;
  Append (e: E) := Append;
  Remove (e: E) := Remove;
  First (): E := First;
  Next (e: E): E := Next;
  Last (): E := Last;
  Previous (e: E): E := Previous;
  MoveToFirst (e: E): BOOLEAN := MoveToFirst;
  MoveToLast (e: E): BOOLEAN := MoveToLast;
  END;

TYPE E = OBJECT 
  list: T;
  next: E; previous: E;
 METHODS
  MakeFirst (): BOOLEAN := MakeFirst;
  MakeLast (): BOOLEAN := MakeLast;
  END;

PROCEDURE New (): T;
PROCEDURE Init (t: T);

PROCEDURE Prepend (t: T; e: E);
PROCEDURE Append (t: T; e: E);
PROCEDURE Remove (t: T; e: E);
PROCEDURE First (t: T): E;
PROCEDURE Next (t: T; e: E): E;
PROCEDURE Last (t: T): E;
PROCEDURE Previous (t: T; e: E): E;
PROCEDURE MoveToFirst (t: T; e: E): BOOLEAN;
PROCEDURE MoveToLast (t: T; e: E): BOOLEAN;

PROCEDURE MakeFirst (e: E): BOOLEAN;
PROCEDURE MakeLast (e: E): BOOLEAN;

  END Linked2List.





