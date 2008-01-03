(* Copyright (C) 1992, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Oct  6 09:32:55 PDT 1993 by sfreeman *)

MODULE Completion;

IMPORT CompletionSeq, Picture, Thread;

REVEAL
  T = Public BRANDED OBJECT
        cond     : Thread.Condition := NIL;
        count    : CARDINAL         := 0;
        next     : T                := NIL; (* for free list *)
        freeProc : Picture.FreeProc := NIL;
        freeParam: REFANY           := NIL;
      OVERRIDES
        init          := Init;
        inc           := Inc;
        dec           := Dec;
        isFree        := IsFree;
        waitUntilFree := WaitUntilFree;
      END;

PROCEDURE Init (c           : T;
                initialCount                   := 1;
                freeProc    : Picture.FreeProc := NIL;
                freeParam   : REFANY           := NIL  ): T =
  BEGIN
    IF c.cond = NIL THEN c.cond := NEW(Thread.Condition); END;
    c.count := initialCount;
    c.freeProc := freeProc;
    c.freeParam := freeParam;
    RETURN c;
  END Init;

PROCEDURE Inc (c: T) =
  BEGIN
    LOCK c DO INC(c.count); END;
  END Inc;

PROCEDURE Dec (c: T) =
  VAR signal := FALSE;
  BEGIN
    LOCK c DO
      CASE c.count OF
      | 0 => RETURN;
      | 1 => DEC(c.count); signal := TRUE;
      ELSE
        DEC(c.count);
      END;
    END;
    IF signal THEN
      IF c.freeProc # NIL THEN
        SetupCallback(c)
      ELSE
        Thread.Broadcast(c.cond);
      END;
    END;
  END Dec;

PROCEDURE IsFree (c: T): BOOLEAN =
  BEGIN
    RETURN c.count = 0;
  END IsFree;

PROCEDURE WaitUntilFree (c: T) RAISES {Thread.Alerted} =
  BEGIN
    LOCK c DO WHILE c.count > 0 DO Thread.AlertWait(c, c.cond); END; END;
  END WaitUntilFree;

PROCEDURE New (): T =
  VAR res: T := NIL;
  BEGIN
    LOCK freeMu DO
      IF free # NIL THEN res := free; free := res.next; END;
    END;
    IF res = NIL THEN res := NEW(T); END;
    RETURN res;
  END New;

PROCEDURE Dispose (c: T) =
  BEGIN
    <* ASSERT c.count = 0 *>
    LOCK freeMu DO c.next := free; free := c; END;
  END Dispose;

VAR
  freeMu    := NEW(MUTEX);
  free  : T := NIL;

(* -- callback handling -- *)
(* when we need to call the freeProc for a Completion.T, we do it in
   another thread to avoid locking problems.  This thread is started the
   first time we need to call a freeProc.  Its creation is protected by
   /mu/.  Completions waiting for their callbacks to be called are put into
   the Sequence /seq/ which is also protected by /mu/ *)
VAR
  mu                    := NEW(MUTEX);
  thread     : Thread.T := NIL;
  seq                   := NEW(CompletionSeq.T).init();
  seqNotEmpty           := NEW(Thread.Condition);

(* start the callback thread if necessary. Add the T to its input queue *)
PROCEDURE SetupCallback (c: T) =
  VAR signal := FALSE;
  BEGIN
    LOCK mu DO
      IF thread = NIL THEN
        thread := Thread.Fork(NEW(Thread.Closure, apply := Apply));
      END;
      seq.addhi(c);
      IF seq.size() = 1 THEN signal := TRUE; END;
    END;
    IF signal THEN Thread.Signal(seqNotEmpty); END;
  END SetupCallback;

(* pull Ts off the queue and call each callback and notify any other
   threads waiting for the T *)
PROCEDURE Apply (<*UNUSED*>cl: Thread.Closure): REFANY =
  VAR c: T;
  BEGIN
    LOOP
      LOCK mu DO
        WHILE seq.size() = 0 DO Thread.Wait(mu, seqNotEmpty); END;
        c := seq.remlo();
      END;

      <* ASSERT c.freeProc # NIL *>
      c.freeProc(c.freeParam);
      Thread.Broadcast(c.cond);
      Dispose(c);
    END;
  END Apply;

BEGIN
END Completion.
