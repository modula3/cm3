(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)

(* Last modified on Fri Jan 14 12:25:39 PST 1994 by kalsow                   *)
(*      modified on Mon Feb 10 17:52:27 PST 1992 by muller                   *)
(*      modified on Mon Feb 10 16:54:50 PST 1992 by ayers                    *)


(*UNSAFE*) MODULE Fifo;

IMPORT Thread;

PROCEDURE New (proc: SubsumerProc := NIL): T =
  BEGIN
  RETURN NEW ( T, 
             subsumerProc := proc,
             first := NIL,
             last := NIL, 
             mutex := NEW (MUTEX),
             condition := NEW (Thread.Condition) );
  END New;

PROCEDURE Insert (t: T; e: E) =
 VAR r, s: E;
  BEGIN
  LOCK t.mutex DO 
    e.next := NIL;
    IF t.first=NIL THEN
      t.first := e; t.last := e;
      Thread.Signal (t.condition);
     ELSE
      IF t.subsumerProc # NIL THEN (* Test for subsumes. *)
        r := t.subsumerProc (e, t.first);
        IF r = t.first THEN RETURN; END;
        IF r = e THEN
          e.next := t.first.next; t.first := e;
          IF e.next = NIL THEN t.last := e; END;
          RETURN; 
          END;
        s := t.first;
        WHILE s.next # NIL DO
          r := t.subsumerProc (e, s.next);
          IF r = s.next THEN RETURN; END;
          IF r = e THEN 
            e.next := s.next.next; s.next := e;
            IF e.next = NIL THEN t.last := e; END;
            RETURN; 
            END;
          s := s.next;
          END;
        END; (* of IF t.subsumerProc # NIL *)
      t.last.next := e;
      t.last := e;
     END;
    END;
  END Insert;
 
PROCEDURE Empty (t: T): BOOLEAN =
  BEGIN
  LOCK t.mutex DO RETURN t.first # NIL; END;
  END Empty;
 
PROCEDURE Count (t: T): INTEGER =
 VAR ret: INTEGER := 0;
 VAR ind: E;
  BEGIN
  LOCK t.mutex DO
    ind := t.first;
    WHILE ind # NIL DO INC (ret); ind := ind.next; END;
    RETURN ret;
    END;
  END Count;
 
PROCEDURE RemoveOrNIL (t: T): E =
 VAR ret: E;
  BEGIN
  LOCK t.mutex DO
    ret := t.first;
    IF ret # NIL THEN t.first := ret.next; END;
    RETURN ret;
    END;
  END RemoveOrNIL;
 
PROCEDURE RemoveOrWait (t: T): E =
 VAR ret: E;
  BEGIN
  LOCK t.mutex DO
    WHILE t.first = NIL DO Thread.Wait (t.mutex, t.condition); END;
    ret := t.first;
    t.first := ret.next; 
    IF t.first = NIL THEN t.last := NIL; END;
    END;
  RETURN ret;
  END RemoveOrWait;
 
  BEGIN

  END Fifo.

