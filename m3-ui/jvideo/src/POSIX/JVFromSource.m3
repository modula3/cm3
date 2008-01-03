(* Copyright (C) 1989, 1993 Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)
(* Last modified on Wed Mar 22 18:14:12 PST 1995 by msm      *)
(*      modified on Tue Aug 17 23:32:26 PDT 1993 by sfreeman *)

MODULE JVFromSource;

IMPORT Jvs, JvsBuffer, Thread, OSError;

REVEAL
  Factory = FactoryPublic BRANDED OBJECT
            OVERRIDES
              init    := Init;
              newBuf  := NewBuf;
              make    := Make;
              destroy := DontDestroy;
            END;

  T = T_Public BRANDED OBJECT
        fac    : Factory;
        next   : T;
        lastUse: CARDINAL  := 0
      END;

VAR
  fmu := NEW(MUTEX);
  bufList: T := NIL;
  thread: Thread.T := NIL;

PROCEDURE Init (f: Factory; jvs: Jvs.T): Factory =
  BEGIN
    RETURN JvsBuffer.Factory.init(f, jvs, Jvs.BufferType.Compress);
  END Init;

PROCEDURE NewBuf (<*UNUSED*> f: Factory): JvsBuffer.T =
  BEGIN
    RETURN NEW(T);
  END NewBuf;

PROCEDURE Make (f: Factory; wait := TRUE; subtype: CARDINAL): JvsBuffer.T
  RAISES {OSError.E, Thread.Alerted} =
  VAR res, ptr: T;
  BEGIN
    LOCK fmu DO
      res := bufList;
      ptr := NIL;
      WHILE res # NIL AND res.subtype # subtype DO
        ptr := res;
        res := res.next
      END;
      IF res # NIL THEN
        IF ptr = NIL THEN
          bufList := res.next
        ELSE
          ptr.next := res.next
        END;
        res.next := NIL;
        RETURN res
      END
    END;
    RETURN FactoryPublic.make(f, wait, subtype)
  END Make;

PROCEDURE Destroy (f: Factory; buf: JvsBuffer.T)
  RAISES {OSError.E, Thread.Alerted} =
  BEGIN
    FactoryPublic.destroy(f, buf)
  END Destroy;

PROCEDURE DontDestroy (f: Factory; buf: JvsBuffer.T) =
  VAR b: T := buf;
  BEGIN
    LOCK fmu DO
      IF thread = NIL THEN
        thread := Thread.Fork(NEW(Thread.Closure, apply := Cleaner))
      END;
      b.next := bufList;
      bufList := b;
      b.fac := f
    END
  END DontDestroy;

PROCEDURE Cleaner (<* UNUSED *> cl: Thread.Closure): REFANY =
  VAR
    useless                            := 0;
    cleanse, cprev, prev, next: T := NIL;
  BEGIN
    LOOP
      cprev := NIL;
      prev := NIL;
      next := NIL;
      cleanse := NIL;
      Thread.Pause(1.0D0);
      LOCK fmu DO
        IF bufList = NIL THEN
          INC(useless);
          IF useless > 15 THEN thread := NIL; RETURN NIL END
        ELSE
          useless := 0;
          next := bufList;
          WHILE next # NIL DO
            INC(next.lastUse);
            IF next.lastUse <= 4 THEN
              cleanse := NIL
            ELSIF cleanse = NIL THEN
              cleanse := next;
              cprev := prev
            END;
            prev := next;
            next := next.next
          END;
          IF cleanse # NIL THEN
            IF cprev = NIL THEN bufList := NIL ELSE cprev.next := NIL END
          END
        END
      END;
      WHILE cleanse # NIL DO
        next := cleanse.next;
        TRY
          LOCK cleanse DO Destroy(cleanse.fac, cleanse) END
        EXCEPT
          OSError.E, Thread.Alerted =>
        END;
        cleanse := next
      END
    END
  END Cleaner;

BEGIN
END JVFromSource.
