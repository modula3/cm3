GENERIC MODULE LRScalarPll(Base, LRVectorBaseTbl);
IMPORT LRVectorRefTbl;
IMPORT LRVector;
IMPORT Thread;
IMPORT Debug;
IMPORT LRMatrix2;

REVEAL
  T = Public BRANDED Brand OBJECT
    base : Base.T;

    mu   : MUTEX;
    tbl  : LRVectorBaseTbl.T;
    open : LRVectorRefTbl.T;
    
  OVERRIDES
    init      := Init;
    eval      := Eval;
    evalHint  := EvalHint;
    clearTbls := ClearTbls;
  END;

PROCEDURE ClearTbls(t : T) =
  BEGIN
    LOCK t.mu DO
      t.tbl  := NEW(LRVectorBaseTbl.Default).init();
      t.open := NEW(LRVectorRefTbl.Default).init()
    END
  END ClearTbls;
  
PROCEDURE Init(t : T; from : Base.T) : T =
  BEGIN
    t.base := from;
    t.tbl  := NEW(LRVectorBaseTbl.Default).init();
    t.open := NEW(LRVectorRefTbl.Default).init();
    t.mu   := NEW(MUTEX);
    RETURN t
  END Init;

PROCEDURE EvalHint(t : T; pp : LRVector.T) =
  VAR
    dummyref : REFANY;
    dummylr  : Base.Result;
    cl       : Closure;
    p        : LRVector.T;
  BEGIN
    LOCK t.mu DO
      p := LRVector.Copy(pp);
      IF NOT (t.open.get(p, dummyref) OR t.tbl.get(p, dummylr)) THEN
        cl := NEW(Closure, t := t, p := p, c := NEW(Thread.Condition));
        EVAL Thread.Fork(cl);
        EVAL t.open.put(p, cl)
      END
    END
  END EvalHint;

PROCEDURE Eval(t : T; pp : LRVector.T) : Base.Result =
  VAR
    ref : REFANY;
    res : Base.Result;
    p   : LRVector.T;
  BEGIN
    LOCK t.mu DO
      p        := LRVector.Copy(pp);
    END;
    t.evalHint(p);
    LOCK t.mu DO
      IF NOT t.tbl.get(p, res) THEN
        WITH hadIt = t.open.get(p, ref),
             cl    = NARROW(ref, Closure) DO
          <*ASSERT hadIt*>
          WHILE NOT t.tbl.get(p, res) DO
            Thread.Wait(t.mu, cl.c)
          END
        END
      END
    END;
    RETURN res
  END Eval;

TYPE
  Closure = Thread.Closure OBJECT
    t : T;
    p : LRVector.T;
    c : Thread.Condition;
  OVERRIDES
    apply := Apply;
  END;

PROCEDURE Apply(cl : Closure) : REFANY =
  VAR
    ref : REFANY;
  BEGIN
    WITH res = cl.t.base.eval(cl.p) DO
      LOCK cl.t.mu DO
        WITH hadIt  = cl.t.open.delete(cl.p, ref) DO
          IF NOT hadIt THEN
            Debug.Warning(Brand & ".Apply : could not find " &
              LRMatrix2.FormatV(cl.p^))
          END;
          EVAL cl.t.tbl.put(cl.p, res)
        END
      END;
      Thread.Broadcast(cl.c)
    END;
    RETURN NIL
  END Apply;
  
BEGIN END LRScalarPll.
