(* $Id$ *)

MODULE SXTime;
IMPORT XTime AS Time, SXLongReal, Thread, SXInt;
FROM Math IMPORT log, pow;

PROCEDURE log2(x : LONGREAL) : LONGREAL = 
  BEGIN RETURN log(x) / log(2.0d0) END log2;

TYPE
  Closure = Thread.SizedClosure OBJECT
    interval, next : Time.T;
    sx : SXLongReal.Var;
    ix : SXInt.Var;
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Apply(cl : Closure) : REFANY =
  VAR i := 0;
  BEGIN
    LOOP
      WITH now = Time.Now() DO
        IF now < cl.next THEN
          WITH pause = cl.next-now-0.01d0 DO
            IF pause > 0.0d0 THEN
              Thread.Pause(pause)
            END
          END
        ELSE
          (* set the sx *)
          cl.sx.set(now);
          INC(i);
          cl.ix.set(i);

          (* and find the next wakeup time *)
          WHILE cl.next < now DO 
            cl.next := cl.next + cl.interval
          END
        END
      END
    END
  END Apply;

PROCEDURE Next(interval, offset : Time.T) : Time.T =
  BEGIN RETURN NextFrom(Time.Now(), interval, offset) END Next;

PROCEDURE NextFrom(now, interval, offset : Time.T) : Time.T =
  VAR
    steps := now/interval;
    frac := steps;
  BEGIN
    IF offset = CurrentOffset THEN
      WITH next0 = Next(interval,0.0d0),
           prev0 = next0 - interval DO
        offset := now - prev0
      END
    END;

    now := now - offset;

    (* get rid of the integer part of frac.

       problems:
       1. frac may not fit in a machine word if it is in integer format
       2. math library operations on longreals are very slow

       solution: try a quick integer op, then use a fully correct
       longreal loop 
     *)

    WITH big       = MIN(frac, FLOAT(LAST(INTEGER),LONGREAL)),
         bigInt    = FLOOR(big),
         bigIntLR  = FLOAT(bigInt, LONGREAL) DO
      frac := frac - bigIntLR
    END;


    WHILE frac >= 1.0d0 DO
      frac := frac - pow(2.0d0, FLOAT(FLOOR(log2(frac)),LONGREAL))
    END;
    WITH whole = steps - frac,
         next = whole * interval + interval DO
      RETURN next + offset
    END
  END NextFrom;

CONST StackSize = 4096;

PROCEDURE New(interval, offset : Time.T) : SXLongReal.T =
  BEGIN
    WITH sx = NEW(SXLongReal.Var).initVal(Time.Now()),
         ix = NEW(SXInt.Var).initVal(0) DO 
      EVAL Thread.Fork(NEW(Closure, 
                           interval := interval, 
                           next := Next(interval,offset),
                           sx := sx,
                           ix := ix, 
                           stackSize := StackSize));
      RETURN sx
    END
  END New;

PROCEDURE NewCounter(interval, offset : Time.T) : SXInt.T =
  BEGIN
    WITH sx = NEW(SXLongReal.Var).initVal(Time.Now()),
         ix = NEW(SXInt.Var).initVal(0) DO 
      EVAL Thread.Fork(NEW(Closure, 
                           interval := interval, 
                           next := Next(interval,offset),
                           sx := sx,
                           ix := ix, 
                           stackSize := StackSize));
      RETURN ix
    END
  END NewCounter;

BEGIN END SXTime.
