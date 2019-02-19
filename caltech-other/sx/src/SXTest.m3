(* $Id$ *)

MODULE SXTest;
IMPORT Thread, SXLongReal, Debug, Fmt;
IMPORT SX, SXSelect;

TYPE 
  PClosure = Thread.Closure OBJECT
    x : SXLongReal.T;
  OVERRIDES
    apply := PApply;
  END;

  P2Closure = Thread.Closure OBJECT
    x,y : SXLongReal.T;
  OVERRIDES
    apply := P2Apply;
  END;

PROCEDURE PApply(cl : PClosure) : REFANY =
  <* FATAL SX.Uninitialized *>
  BEGIN
    SX.Lock(SX.Array { cl.x });
    TRY
      LOOP
        Debug.Out("Waiting...");
        cl.x.wait();
        Debug.Out("After wait...");
        Debug.Out("Changed: " & Fmt.LongReal(cl.x.value()))
      END
    FINALLY
      SX.Unlock(SX.Array { cl.x })
    END
  END PApply;

PROCEDURE P2Apply(cl : P2Closure) : REFANY =
  <* FATAL SX.Uninitialized *>
  BEGIN
    LOOP
      SX.Lock(SX.Array { cl.x, cl.y });
      TRY
        Debug.Out("P2 Waiting...");
        SXSelect.Wait(ARRAY OF SX.T {cl.x,cl.y});
        Debug.Out("P2 After wait...");
        Debug.Out("P2 Changed: " & Fmt.LongReal(cl.x.value()) & " "&
          Fmt.LongReal(cl.y.value()))
      FINALLY
        SX.Unlock(SX.Array { cl.x, cl.y })
      END
    END
  END P2Apply;

<*NOWARN*>PROCEDURE SumSq(a, b : LONGREAL) : LONGREAL =
  BEGIN
    RETURN a*a + b*b
  END SumSq;

PROCEDURE DoIt() =
  BEGIN
    WITH var = NARROW(NEW(SXLongReal.Var).init(),SXLongReal.Var) (*,
         sin = SXLR_LRFuncOps.UnaryFunc(var, Math.sin),
         cos = SXLR_LRFuncOps.UnaryFunc(var, Math.cos),
         sumsq = SXLR_LRFuncOps.BinaryFunc(sin,cos,SumSq) *) DO
      EVAL Thread.Fork(NEW(PClosure, x := var));

(*      EVAL Thread.Fork(NEW(PClosure, x := sin));
      EVAL Thread.Fork(NEW(PClosure, x := cos));
      EVAL Thread.Fork(NEW(PClosure, x := sumsq));
      EVAL Thread.Fork(NEW(P2Closure, x := sin, y := cos));
*)
      Thread.Pause(1.0d0); (* make sure threads are started... *)

      Debug.Out("Counting...");
      FOR i := 0 TO 1000000 DO
        var.set(FLOAT(i,LONGREAL),0.0d0)
      END;
      Debug.Out("Done counting...");

      Thread.Pause(1.0d0);

  (*    Debug.Out("Updates: var: " & Fmt.Int(var.numUpdates()) & " " &
                         "sin: " & Fmt.Int(sin.numUpdates()) & " " &
                         "cos: " & Fmt.Int(cos.numUpdates()) & " " &
                         "sumsq: " & Fmt.Int(sumsq.numUpdates()) & " " )
*)
    END
  END DoIt;

BEGIN END SXTest.
