(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Thu Jan 26 14:04:03 PST 1995 by kalsow     *)
(*      modified on Fri Jun 18 17:36:13 PDT 1993 by wobber     *)
(*      modified on Tue Jun  1 00:18:27 PDT 1993 by swart      *)
(*      modified on Wed Apr 21 13:13:51 PDT 1993 by mcjones    *)
(*      modified on Thu Jan 28 12:00:11 PST 1993 by mjordan    *)
(*      modified on Sat Jun 27 15:10:10 PDT 1992 by muller     *)
(*      modified on Thu Nov  2 18:18:07 1989 by gnelson        *)
(*      modified on Thu Nov  2 18:18:07 1989 by manasse        *)

MODULE AutoFlushWr;

IMPORT Wr, WrClass, Time, Thread, Process;
FROM Wr IMPORT Failure;
FROM Thread IMPORT Alerted;

(* A writer wr is *inactive* if wr.cur=wr.hi=wr.lo, and *active* otherwise.  
   Notice that if wr is inactive, c(wr) = target(wr) (by WrClass.V2) 
   and therefore can be ignored by the worker thread that does the
   flushing.  Our strategy is to keep a list containing all active
   writers.  The worker thread wakes up periodically and checks all 
   the writers on the list, flushing them if they have gone unflushed 
   for too long.  The delay between wake ups is the 
   equal to minimum delay of any active writer.  When the list 
   is empty, the worker blocks. *) 

CONST 
  Forever = LAST(LONGREAL);
  Default = 0.1d0;
  
VAR 
  mu := NEW (Thread.Mutex);
  (* mu protects the next four variables *)
  (* LL(wr) < LL(mu) for all wr: T *)
  active := NEW(T);  
  minDelay: Time.T := Forever; 
  c := NEW (Thread.Condition); (* that active is nonempty *) 
  worker: Thread.T := NIL; 

(* The comment LL(wr) < LL(mu) means that the locking level of mu 
   is greater than the locking level of any auto-flush writer.
   This means that a thread must not try to acquire a writer lock 
   while it holds mu. *)

(* The first object on the active list is a sentinal; thus the list 
   is empty if active.link = NIL.  The value of minDelay is the minimum 
   delay of any writer on the active list. *)

REVEAL
  T = Public BRANDED OBJECT
        child: Wr.T;
        delay: Time.T;
        alarm: Time.T;           (* readonly if onQ is true *)
        onQ  : BOOLEAN;          (* protected by mu *)
        link : T;                (* protected by mu *)
      OVERRIDES
        seek      := Seek;
        length    := Length;
        flush     := Flush;
        close     := Close;
        init      := Init;
      END;

(* For wr of type T, wr.child is the writer to which wr's writing 
   is forwarded.  wr.delay is the inverse of the frequency with which 
   wr will be flushed.  If wr is active, wr.alarm is the next time 
   at which wr should be flushed; otherwise wr.alarm is irrelevant.  
   The boolean wr.onQ is true iff wr is on the active list, and  
   wr.link is the link field for the active list.  The child, delay, 
   and alarm fields are protected by wr's mutex; the onQ and link 
   fields are protected by the global mu. *)

PROCEDURE Init (wr: T; ch: Wr.T; p := -1.0D0): T =
  BEGIN
    WrClass.Lock(wr);
    WrClass.Lock(ch);
    TRY
      wr.buff := ch.buff;
      wr.st := ch.st;
      wr.cur := ch.cur;
      wr.lo := ch.lo;
      wr.hi := ch.hi;
      wr.closed := ch.closed;
      wr.seekable := ch.seekable;
      wr.buffered := TRUE;
      wr.child := ch;
      wr.onQ := FALSE;
      wr.link := NIL;
    FINALLY
      WrClass.Unlock(ch);
      WrClass.Unlock(wr);
    END;
    IF p < 0.0D0 THEN wr.delay := Default ELSE wr.delay := p END;
    RETURN wr
  END Init;

PROCEDURE Seek (wr: T; n: CARDINAL) RAISES {Failure, Alerted} =
  VAR wasEmpty := FALSE;
  BEGIN
    WrClass.Lock(wr.child);
    TRY
      wr.child.cur := wr.cur;
      wr.child.seek(n);
    FINALLY
      WrClass.Unlock(wr.child);
    END;
    wr.buff := wr.child.buff;
    wr.st := wr.child.st;
    wr.cur := wr.child.cur;
    wr.lo := wr.child.lo;
    wr.hi := wr.child.hi;
    LOCK mu DO
      IF NOT wr.onQ THEN
        wr.onQ := TRUE;
        wasEmpty := active.link = NIL;
        wr.link := active.link;
        active.link := wr;
        wr.alarm := Time.Now() + wr.delay;
        IF wasEmpty THEN
          minDelay := wr.delay;
          IF worker = NIL THEN
            worker := Thread.Fork(NEW(Thread.Closure, apply := Worker))
          END
        ELSIF minDelay > wr.delay THEN
          minDelay := wr.delay;
          Thread.Alert(worker)
        END
      END
    END;
    IF wasEmpty THEN Thread.Signal(c) END
  END Seek;

PROCEDURE Flush (wr: T) RAISES {Failure, Alerted} =
  BEGIN
    WrClass.Lock(wr.child);
    TRY
      wr.child.cur := wr.cur;
      wr.child.flush();

      wr.buff := wr.child.buff;
      (* Set everything empty. *)
      wr.st := wr.child.st + wr.child.cur - wr.child.lo;
      wr.cur := wr.child.cur;
      wr.lo := wr.child.cur;
      wr.hi := wr.child.cur;
    FINALLY
      WrClass.Unlock(wr.child);
    END;
  END Flush;

PROCEDURE Length (wr: T): CARDINAL RAISES {Failure, Alerted} =
  BEGIN
    WrClass.Lock(wr.child);
    TRY
      wr.child.cur := wr.cur;
      WITH res = wr.child.length() DO
        wr.buff := wr.child.buff;
        wr.st := wr.child.st;
        wr.cur := wr.child.cur;
        wr.lo := wr.child.lo;
        wr.hi := wr.child.hi;
        RETURN res;
      END;
    FINALLY
      WrClass.Unlock(wr.child);
    END;
  END Length;

PROCEDURE Close(wr: T) RAISES {Failure, Alerted} =
  BEGIN
    WrClass.Lock(wr.child);
    TRY
      wr.child.cur := wr.cur;
      wr.child.close();
    FINALLY
      WrClass.Unlock(wr.child);
    END;
    wr.buff := NIL
  END Close;

(* The worker thread delay is the minimum delay of any active writer. 
    Each time it wakes up, it executes the following:

|     Let S be the set of active writers.
|     Set minDelay := infinity;
|     FOR wr IN S do:
|      IF wr's timer has expired THEN
|        Flush wr, remove it from the active list
|      ELSE
|        minDelay := min(minDelay, wr.delay)
|      END
|    END

    Notice that the active list can be enlarged while S is being 
    processed, since clients may be operating on writers concurrently.  
    If minDelay decreases while the worker is asleep, it is alerted. 
    *)

PROCEDURE Worker (<*UNUSED*>cl: Thread.Closure): REFANY RAISES {} =
  <*FATAL Wr.Failure, Thread.Alerted*>
  VAR
    delay, now: Time.T;
    s:     T;
    wr:    T;
  BEGIN
    LOOP
      LOCK mu DO
        WHILE active.link = NIL DO Thread.Wait(mu, c) END;
        delay := minDelay
      END;
      TRY
        Thread.AlertPause(delay)
      EXCEPT
        Thread.Alerted  => (*skip*)
      END;
      LOCK mu DO
        s := active;
        minDelay := Forever;
      END;
      now := Time.Now();
      LOOP
        (* s scans through the set called S above. *)
        LOCK mu DO
          IF s.link = NIL THEN EXIT END;
          wr := s.link;
          IF now > wr.alarm THEN
            s.link := wr.link;
            wr.onQ := FALSE;
            wr.link := NIL
          ELSE
            s := wr;
            wr := NIL;
            IF minDelay > s.delay THEN
              minDelay := s.delay
            END
          END
        END;
        IF wr # NIL THEN
          WrClass.Lock(wr);
          TRY
            IF NOT wr.closed THEN Flush(wr) END;
          FINALLY
            WrClass.Unlock(wr);
          END;
        END (* IF *);
      END
    END
  END Worker;

(* Notice that Flush(wr) cannot be called while mu is locked, since 
   this violates the locking level order.  Thus wr is taken out of 
   the lock to be flushed.  *)

PROCEDURE Shutdown () =
  VAR s, wr: T;
  <*FATAL Wr.Failure, Thread.Alerted *>
  BEGIN
    LOCK mu DO s := active; END;
    LOOP
      (* s scans through the set called S above. *)
      LOCK mu DO
        IF s.link = NIL THEN EXIT END;
        wr := s.link;
        s.link := wr.link;
        wr.onQ := FALSE;
        wr.link := NIL
      END;
      WrClass.Lock(wr);
      TRY
        IF NOT wr.closed THEN Flush(wr) END;
      FINALLY
        WrClass.Unlock(wr);
      END;
    END
  END Shutdown;

BEGIN Process.RegisterExitor(Shutdown); END AutoFlushWr.
