MODULE Main;
IMPORT NewUOA_M3;
IMPORT LRScalarField, LRVector;
IMPORT Math, Debug;
FROM Fmt IMPORT LongReal, F, Style;
IMPORT NewUOAs;
IMPORT Powell;
IMPORT Scan, Params;
IMPORT Matrix;

TYPE
  TestFuncType = LRScalarField.Default OBJECT
    opt    : LONGREAL;
    radius : LONGREAL;
  OVERRIDES
    eval := EvalTF;
  END;
  
PROCEDURE ApplyConstraint(x : LRVector.T; r : LONGREAL) : LRVector.T =
  VAR
    res : LRVector.T;
    normsq := 0.0d0;
    q : LONGREAL;
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      normsq := normsq + x[i]*x[i]
    END;
    IF normsq < r*r THEN RETURN x END;

    (* implement the triangle wave *)
    res := NEW(LRVector.T, NUMBER(x^));

    IF normsq = 0.0d0 THEN 
      FOR i := FIRST(x^) TO LAST(x^) DO res[i] := 0.0d0 END
    ELSE
      WITH norm      = Math.sqrt(normsq),
           relnorm   = norm/r,
           relnormfl = FLOOR(relnorm),
           relnormx  = norm-FLOAT(relnormfl, LONGREAL) DO
        IF relnormfl MOD 2 = 0 THEN
          q := relnormx
        ELSE
          q := 1.0d0 - relnormx
        END;
        (* 0 <= q <= 1 *)
        FOR i := FIRST(x^) TO LAST(x^) DO
          res[i] := q * x[i] / norm
        END
      END
    END;
    RETURN res
  END ApplyConstraint;

PROCEDURE EvalTF(t : TestFuncType; xarg : LRVector.T) : LONGREAL =
  VAR
    ss := 0.0d0;
    dbg := "TestFunc";
    x := ApplyConstraint(xarg, t.radius);
  BEGIN
    (* min at 1, 1, 1, ... , 1 *)
    FOR i := FIRST(x^) TO LAST(x^) DO
      dbg := dbg & F(" %10s", LongReal(x[i], prec :=4, style := Style.Fix));
      ss := ss + Math.sqrt((t.opt - x[i])*(t.opt - x[i]))
    END;
    IF ss > 1.0d-2 THEN
      dbg := dbg & " = " & LongReal(ss, prec :=4, style := Style.Fix);
    ELSE
      dbg := dbg & " = " & LongReal(ss, prec :=4, style := Style.Sci);
    END;
    Debug.Out(dbg);
    RETURN ss
  END EvalTF;

VAR
  n := 20;
  p := NEW(LRVector.T, n);
  npt := 2*n + 1;
  rhobeg := 1.0d0;
  rhoend := 1.0d-2;
  maxfun := 1000*n;
  func := NEW(TestFuncType, opt := 1.1d0, radius := 10.0d0);
  method := Scan.Int(Params.Get(1));
  min : LONGREAL;
BEGIN
  FOR i := FIRST(p^) TO LAST(p^) DO
    p[i] := 0.0d0
  END;
  CASE method OF
    0 =>
    min := NewUOA_M3.Minimize(p, func, npt, rhobeg, rhoend, maxfun)
  |
    1 =>
    WITH res = NewUOAs.Minimize(p, func, rhobeg, rhoend) DO
      Debug.Out("NewUOAs done, " & res.message);
      min := res.f
    END
  |
    2 =>
    VAR xi := Matrix.Unit(Matrix.Dim{n, n}); BEGIN
      min := Powell.Minimize(p, xi, rhoend, func)
    END
  END;
  Debug.Out("min="& LongReal(min))
END Main.
