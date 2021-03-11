(* $Id$ *)

UNSAFE MODULE Semaphore;
FROM SemaphoreC IMPORT 
  sem_init, sem_destroy, sem_post, sem_wait, sem_getvalue, sem_alloc;
IMPORT WeakRef;
FROM Ctypes IMPORT int;

REVEAL 
  T = BRANDED Brand REF RECORD
    sem : ADDRESS;
  END;

PROCEDURE New() : T =
  BEGIN
    WITH q = sem_alloc(),
         r = sem_init(q),
         n = NEW(T, sem := q) DO
      <*ASSERT r = 0*>
      EVAL WeakRef.FromRef(n, Cleanup);
      RETURN n
    END
  END New;

PROCEDURE Cleanup(<*UNUSED*>READONLY self : WeakRef.T; ref : REFANY) =
  VAR 
    d : T := ref;
  BEGIN
    WITH r = sem_destroy(d.sem) DO <*ASSERT r = 0*> END
  END Cleanup;

PROCEDURE P(t : T) =
  BEGIN
    WITH r = sem_wait(t.sem) DO <*ASSERT r=0*> END
  END P;

PROCEDURE V(t : T) =
  VAR r : INTEGER;
  BEGIN
    REPEAT r := sem_post(t.sem) UNTIL r = 0
  END V;

PROCEDURE Value(t : T) : INTEGER =
  VAR res : int;
  BEGIN 
    WITH r = sem_getvalue(t.sem, res) DO <*ASSERT r=0*> END;
    RETURN res
  END Value;

BEGIN END Semaphore.
