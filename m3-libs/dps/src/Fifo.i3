(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Mon Feb 10 17:52:27 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)


INTERFACE Fifo; 

IMPORT Thread; 

TYPE E = OBJECT next: E; METHODS END;

TYPE SubsumerProc = PROCEDURE (new, old: E): E;

TYPE T = OBJECT 
  mutex: MUTEX;
  condition: Thread.Condition;
  first: E; last: E;
  subsumerProc: SubsumerProc;
 METHODS
  Insert (e: E) := Insert;
  InsertMaybe (e: E) := Insert;
  RemoveOrNIL (): E:= RemoveOrNIL;
  RemoveOrWait (): E := RemoveOrWait;
  Count (): INTEGER := Count;
  Empty (): BOOLEAN := Empty;
  END;

PROCEDURE New (proc: SubsumerProc := NIL): T;

PROCEDURE Insert (t: T; e: E);
PROCEDURE RemoveOrNIL (t: T): E;
PROCEDURE RemoveOrWait (t: T): E;
PROCEDURE Count (t: T): INTEGER;
PROCEDURE Empty (t: T): BOOLEAN;

  END Fifo.





