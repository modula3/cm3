(* $Id$ *)

MODULE SXTimer;
IMPORT XTime AS Time;
IMPORT Thread;
IMPORT SXLongReal;
IMPORT LongrealPQ;
IMPORT SXIterator;
IMPORT Fmt, Debug;

VAR DoDebug := Debug.DebugThis("SXTIMER");

REVEAL
  Private = SXLongReal.Var BRANDED OBJECT END; (* this is cool! *)

  T = Public BRANDED Brand OBJECT
    granularity : LONGREAL;
    c : CARDINAL;
  OVERRIDES
    init := Init;
    dependsOn := SXIterator.NullNull;
  END;

VAR
  c := 0;
  cMu := NEW(MUTEX);

PROCEDURE Init(t : T; granularity : LONGREAL) : T =
  BEGIN
    EVAL SXLongReal.T.init(t);
    LOCK cMu DO
      t.c := c;
      INC(c)
    END;
    t.granularity := granularity;
    t.set(Time.Now());
    Register(t);
    RETURN t
  END Init;

PROCEDURE Register(t : T) =
  BEGIN
    LOCK mu DO
      pq.insert(NEW(Elt, priority := Time.Now() + t.granularity, t := t));
      Thread.Alert(main)
    END
  END Register;

TYPE
  Elt = LongrealPQ.Elt OBJECT
    t    : T;
    sx   : SXLongReal.Var;
  END;

VAR pq := NEW(LongrealPQ.Default).init();

PROCEDURE Loop(<*UNUSED*>cl : Thread.Closure) : REFANY =
  <*FATAL LongrealPQ.Empty*>

  PROCEDURE Sleep(howMuch : LONGREAL) RAISES { Thread.Alerted } =
    BEGIN
      Thread.Release(mu);
      TRY
        Thread.AlertPause(howMuch);
      FINALLY
        Thread.Acquire(mu)
      END
    END Sleep;

  BEGIN
    LOCK mu DO
      LOOP
        TRY
          WITH now = Time.Now() DO
            WHILE pq.size() > 0 AND NARROW(pq.min(),Elt).priority < now DO
              WITH head = NARROW(pq.deleteMin(),Elt) DO 
                head.t.set(now);
                head.priority := now + head.t.granularity;

                IF DoDebug THEN 
                  Debug.Out("SXTimer.Loop ("&Fmt.Int(head.t.c)&"): set " & 
                    Fmt.LongReal(now) & " -> " & 
                    Fmt.LongReal(head.priority) & " size=" & 
                    Fmt.Int(pq.size()))
                END;
                pq.insert(head)
              END
            END;

            IF pq.size() = 0 THEN
              Sleep(1000.0d0) (* a long time *)
            ELSE
              Sleep(NARROW(pq.min(),Elt).priority-Time.Now())
            END
          END
        EXCEPT
          Thread.Alerted => (* skip *)
        END
      END
    END
  END Loop;

VAR main : Thread.T;
    mu := NEW(MUTEX);

BEGIN
  main := Thread.Fork(NEW(Thread.Closure, apply := Loop))
END SXTimer.

