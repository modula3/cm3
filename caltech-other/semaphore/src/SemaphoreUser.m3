(* $Id$ *)

MODULE SemaphoreUser EXPORTS Semaphore;
IMPORT Thread;

REVEAL 
  T = BRANDED Brand REF RECORD
    mu : MUTEX;
    s  : CARDINAL;
    c  : Thread.Condition;
  END;

PROCEDURE New() : T =
  BEGIN RETURN NEW(T, mu := NEW(MUTEX), c := NEW(Thread.Condition), s := 0) END New; 

PROCEDURE P(t : T) =
  BEGIN
    LOCK t.mu DO 
      WHILE t.s = 0 DO Thread.Wait(t.mu, t.c) END;
      DEC(t.s)
    END
  END P;

PROCEDURE V(t : T) =
  BEGIN
    LOCK t.mu DO
      INC(t.s);
      Thread.Signal(t.c)
    END
  END V;

PROCEDURE Value(t : T) : INTEGER =
  BEGIN 
    LOCK t.mu DO RETURN t.s END
  END Value;

BEGIN END SemaphoreUser.
