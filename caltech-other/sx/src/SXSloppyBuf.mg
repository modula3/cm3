(* $Id$ *)

GENERIC MODULE SXSloppyBuf(SXType);
IMPORT Thread, SXSelect, SX;

REVEAL
  Spec = SXType.Var BRANDED OBJECT END;

  T = Public BRANDED Brand OBJECT
    src : SXType.T;
    last := -1;
  OVERRIDES
    init := Init;
  END;

CONST StackSize = 4096; 
(* small but not incredibly small: cl.t.set uses some stack space *)

PROCEDURE Init(t : T; src : SXType.T) : T =
  BEGIN
    EVAL SXType.Var.init(t);
    t.src := src;
    EVAL Thread.Fork(NEW(Closure, 
                         t := t, 
                         stackSize := StackSize));
    RETURN t
  END Init;

TYPE
  Closure = Thread.SizedClosure OBJECT
    t : T;
  OVERRIDES 
    apply := Apply;
  END;

PROCEDURE Apply(cl : Closure) : REFANY =
  VAR
    temp : SXType.Base;
    src := cl.t.src;
  BEGIN
    LOOP
      SX.Lock1(src);
      TRY
        WHILE cl.t.last = src.numUpdates() DO
          SXSelect.Wait1(src)
        END;
        TRY
          cl.t.last := src.numUpdates();
          temp      := src.value(); 
        EXCEPT
          SX.Uninitialized => (* skip *)
        END
      FINALLY
        SX.Unlock1(src)
      END;
      cl.t.set(temp)
    END
  END Apply;

BEGIN END SXSloppyBuf.
