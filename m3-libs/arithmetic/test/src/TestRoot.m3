MODULE TestRoot EXPORTS Test;
(*Copyright (c) 1996, Harry George*)
FROM NADefinitions IMPORT Error, Err;
IMPORT Fmt,
       LongRealBasic                   AS R,
       LongRealTrans                   AS RT,
       LongRealFmtLex                  AS RF,
       LongRealComplex                 AS C,
       LongRealComplexTrans            AS CT,
       LongRealComplexFmtLex           AS CF,
       LongRealComplexPolynomial       AS CP,
       LongRealComplexPolynomialFmtLex AS CPF,
       LongRealComplexRoot             AS Rt,
       LongRealRootApproximation       AS RtA,
       LongRealFindZero                AS FZ,
       Integer32Basic                  AS I,
       Integer32Root                   AS IR,
       Integer32Polynomial             AS IP,
       Integer32PolynomialFmtLex       AS IPF,
       Thread,
       Wr;

<*FATAL Error*>
(*=======================*)
CONST Module = "TestRoot.";

(*---------------------*)
PROCEDURE TestPolyRoots (         p     : CP.T;
                         READONLY rt    : Rt.RootArray;
                                  epssqr: R.T            := RT.Eps) =
  BEGIN
    (*
      Msg(Fmt.FN("{%s,%s}\n", ARRAY OF TEXT
          {CF.Fmt(rt[0]), CF.Fmt(rt[1])}));
      Msg(Fmt.FN("0 = {%s,%s}\n", ARRAY OF TEXT
          {CF.Fmt(CP.Eval(xc,rt[0])), CF.Fmt(CP.Eval(xc,rt[1]))}));
      <*ASSERT CT.AbsSqr(CP.Eval(xc,rt[0]))<RT.Eps*>
      <*ASSERT CT.AbsSqr(CP.Eval(xc,rt[1]))<RT.Eps*>
    *)
    Msg("{");
    FOR j := 0 TO LAST(rt) DO Msg(CF.Fmt(rt[j]) & ", "); END;
    Msg("}\n");
    Msg("zero check: ");
    FOR j := 0 TO LAST(rt) DO
      (*
          Msg(CF.Fmt(rt[j]) & " ->" & CF.Fmt(CP.Eval(p,rt[j])) & ",\n");
      *)
      <*ASSERT CT.AbsSqr(CP.Eval(p,rt[j]))<epssqr*>
    END;
    Msg("\n");
  END TestPolyRoots;

(*=========================*)
(* Quadratics *)
(*=========================*)
(*-----------------------*)
PROCEDURE TestQuadratic (): BOOLEAN =

  PROCEDURE TestSingle (READONLY x: RtA.RealPolynomial2) =
    VAR
      rt: RtA.RootArray2;
      xc: CP.T;
    BEGIN
      Msg(Fmt.FN("Solve %s+%s*t+%s*t^2=0  -->  ",
                 ARRAY OF TEXT{RF.Fmt(x[0]), RF.Fmt(x[1]), RF.Fmt(x[2])}));
      rt := RtA.RealQuadratic(x);

      (*evaluate polynomial for the found roots*)
      xc := CP.New(2);
      xc[0] := C.T{x[0], R.Zero};
      xc[1] := C.T{x[1], R.Zero};
      xc[2] := C.T{x[2], R.Zero};
      TestPolyRoots(xc, rt);

      xc[0] := C.T{x[0], x[0]};
      xc[1] := C.T{x[1], x[1]};
      xc[2] := C.T{x[2], x[2]};
      Msg(
        Fmt.FN("Solve %s+%s*t+%s*t^2=0  -->  ",
               ARRAY OF TEXT{CF.Fmt(xc[0]), CF.Fmt(xc[1]), CF.Fmt(xc[2])}));
      rt := RtA.ComplexQuadratic(xc^);
      TestPolyRoots(xc, rt);

      xc[0] := C.T{x[0], x[0] + R.One};
      xc[1] := C.T{x[1], x[1] - R.One};
      xc[2] := C.T{x[2], x[2]};
      Msg(
        Fmt.FN("Solve %s+%s*t+%s*t^2=0  -->  ",
               ARRAY OF TEXT{CF.Fmt(xc[0]), CF.Fmt(xc[1]), CF.Fmt(xc[2])}));
      rt := RtA.ComplexQuadratic(xc^);
      TestPolyRoots(xc, rt);
    END TestSingle;

  CONST ftn = Module & "TestQuadratic";
  VAR result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    TestSingle(ARRAY OF R.T{-3.0D0, 2.0D0, 1.0D0});
    TestSingle(ARRAY OF R.T{-1.0D0, 0.0D0, 1.0D0});
    TestSingle(ARRAY OF R.T{1.0D0, 2.0D0, 1.0D0});
    TestSingle(ARRAY OF R.T{1.0D0, -2.0D0, 1.0D0});
    TestSingle(ARRAY OF R.T{1.0D0, 1.0D0, 1.0D0});
    TestSingle(ARRAY OF R.T{1.0D0, -1.0D0, 1.0D0});
    TestSingle(ARRAY OF R.T{1.0D0, 0.0D0, 1.0D0});

    RETURN result;
  END TestQuadratic;

PROCEDURE TestRootApproximation (): BOOLEAN =

  PROCEDURE TestSingle (READONLY rt        : Rt.RootArray;
                                 zerotolsqr: R.T            := RT.Eps;
                        roottolsqr: R.T := RT.Eps * 10.0D0) =
    VAR
      p                     := Rt.FromRoots(rt);
      art: REF Rt.RootArray;
    BEGIN
      Msg("zeros given {");
      FOR j := 0 TO LAST(rt) DO Msg(CF.Fmt(rt[j]) & ", "); END;
      Msg("}\nPolynomial: " & CPF.Fmt(p) & "\n");

      art := RtA.ComplexNewtonMaehli(p);

      Msg("compare with given zeros, tolerance: " & RF.Fmt(roottolsqr)
            & "\n");
      FOR j := 0 TO LAST(art^) DO
        VAR
          minerror      := CT.AbsSqr(C.Sub(rt[j], art[0]));
          curerror: R.T;
        BEGIN
          FOR k := 1 TO LAST(rt) DO
            curerror := CT.AbsSqr(C.Sub(rt[j], art[k]));
            (* the root finding seems to be quite imprecise, sometimes only
               the half precision of LONGREAL is reached
               Msg(CF.Fmt(C.Sub(rt[j],art[k])) & "\n"); *)
            IF minerror > curerror THEN minerror := curerror; END;
          END;
          (*
                  Msg(CF.Fmt(rt[j]) & " - next approx " & RF.Fmt(minerror) & "\n");
          *)
          <*ASSERT minerror < roottolsqr*>
        END;
      END;

      TestPolyRoots(p, art^, zerotolsqr);
    END TestSingle;

  CONST ftn = Module & "TestRootApproximation";
  VAR result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    TestSingle(Rt.RootArray{C.T{-10.0D0, R.Zero}, C.T{2.0D0, R.Zero},
                            C.T{10.0D0, R.Zero}});
    TestSingle(Rt.RootArray{C.T{-10.0D0, 1.0D0}, C.T{-10.0D0, 1.0D0},
                            C.T{10.0D0, 1.0D0}, C.T{10.0D0, 1.0D0}});
    TestSingle(Rt.RootArray{C.T{-1.0D0, 1.2D0}, C.T{-10.0D0, 3.7D0},
                            C.T{12.7D0, -5.1D0}, C.T{8.2D0, 1.0D0}});
    VAR
      rt : ARRAY [0 .. 20] OF C.T;
      pow: C.T                    := C.One;
    BEGIN
      FOR j := 0 TO LAST(rt) DO
        rt[j] := pow;
        pow := C.Scale(pow, RT.Half);
      END;
      TestSingle(rt);
    END;

    (* the killer example of Wilkinson contain the roots 1,..,20 - the
       results are very imprecise *)
    VAR
      rt: ARRAY [0 .. 20] OF C.T;
      x : C.T                    := C.Zero;
    BEGIN
      FOR j := 0 TO LAST(rt) DO rt[j] := x; x := C.Add(x, C.One); END;
      TestSingle(rt, zerotolsqr := 1.0D28, roottolsqr := 1.0D-3);
    END;

    RETURN result;
  END TestRootApproximation;

CONST
  prec3Style = RF.FmtStyle{style := Fmt.Style.Fix, prec := 3};
  prec5Style = RF.FmtStyle{style := Fmt.Style.Fix, prec := 5};

(*---------------------*)
VAR                              (*globally visible*)
  r1 := -10.0D0;
  r2 := 2.0D0;
  r3 := 10.0D0;

(*---------------------*)
PROCEDURE MyFun (x: R.T): R.T =
  BEGIN
    RETURN (x - r1) * (x - r2) * (x - r3);
  END MyFun;
(*---------------------*)
PROCEDURE MyFun2 (x: R.T): FZ.DerivativeArray2 =
  BEGIN
    RETURN FZ.DerivativeArray2{(x - r1) * (x - r2) * (x - r3),
                               (x - r2) * (x - r3) + (x - r1) * (x - r3)
                                 + (x - r1) * (x - r2)};
  END MyFun2;

(*=========================*)
(* NonLinears *)
(*=========================*)
(*----------------------*)
PROCEDURE TestBracketOut (): BOOLEAN =
  CONST
    ftn     = Module & "TestBracketOut";
    maxiter = 10;
  VAR
    result             := TRUE;
    x     : FZ.Bracket;
  BEGIN
    Debug(1, ftn, "begin\n");
    Msg("true roots: r1=" & RF.Fmt(r1) & " r2=" & RF.Fmt(r2) & " r3="
          & RF.Fmt(r3) & " maxiter=" & Fmt.Int(maxiter) & "\n");
    FOR i := 1 TO 50 DO
      x.l := 5.0D0 * FLOAT(i, R.T) - 50.0D0;
      x.r := x.l + 1.0D0;
      Msg("start at x.l=" & RF.Fmt(x.l, prec3Style) & " x.r="
            & RF.Fmt(x.r, prec3Style));
      <*FATAL ANY*>
      BEGIN
        IF FZ.BracketOut(MyFun, x, maxiter := maxiter) THEN
          Msg(" end at x.l=" & RF.Fmt(x.l, prec3Style) & " x.r="
                & RF.Fmt(x.r, prec3Style) & "\n");
        ELSE
          Msg(" not found\n");
        END;
      END;
    END;
    RETURN result;
  END TestBracketOut;
(*----------------------*)
PROCEDURE TestBracketIn (): BOOLEAN =
  CONST ftn = Module & "TestBracketIn";
  VAR
    result             := TRUE;
    x     : FZ.Bracket;
    nb    : CARDINAL   := 5;
    xb                 := NEW(REF ARRAY OF FZ.Bracket, nb);
    n     : CARDINAL;
  BEGIN
    Debug(1, ftn, "begin\n");
    Msg("true roots: r1=" & RF.Fmt(r1) & " r2=" & RF.Fmt(r2) & " r3="
          & RF.Fmt(r3) & "\n");
    x.l := -50.0D0;
    x.r := +50.0D0;
    Msg("start at x.l=" & RF.Fmt(x.l, prec3Style) & " x.r="
          & RF.Fmt(x.r, prec3Style) & " nb=" & Fmt.Int(nb) & "\n");
    FOR i := 10 TO 100 BY 10 DO
      n := i;
      Msg("n=" & Fmt.Int(n) & "\n");
      <*FATAL Error*>
      BEGIN
        nb := FZ.BracketIn(func := MyFun, x := x, n := n, xb := xb^);
        IF nb > 0 THEN
          FOR j := 0 TO nb - 1 DO
            Msg(" found  x.l=" & RF.Fmt(xb[j].l, prec3Style) & " x.r="
                  & RF.Fmt(xb[j].r, prec3Style) & "\n");
          END;
        ELSE
          Msg(" not found\n");
        END;
      END;
    END;
    RETURN result;
  END TestBracketIn;
(*----------------------*)
PROCEDURE TestBisection (): BOOLEAN =
  CONST ftn = Module & "TestBisection";
  VAR
    result                := TRUE;
    x        : FZ.Bracket;
    tol, root: R.T;
  <*FATAL Error*>
  BEGIN
    Debug(1, ftn, "begin\n");
    Msg("true roots: r1=" & RF.Fmt(r1) & " r2=" & RF.Fmt(r2) & " r3="
          & RF.Fmt(r3) & "\n");
    x.l := -1.0D0;
    x.r := 2.9D0;
    tol := 0.001D0;
    Msg("start at x.l=" & RF.Fmt(x.l, prec3Style) & " x.r="
          & RF.Fmt(x.r, prec3Style) & " tol=" & RF.Fmt(tol));
    root := FZ.Bisection(MyFun, x, tol);
    Msg(" found  root=" & RF.Fmt(root, prec3Style) & "\n");
    RETURN result;
  END TestBisection;
(*----------------------*)
PROCEDURE TestBrent (): BOOLEAN =
  CONST ftn = Module & "TestBrent";
  VAR
    result                := TRUE;
    x        : FZ.Bracket;
    tol, root: R.T;
  BEGIN
    Debug(1, ftn, "begin\n");
    Msg("true roots: r1=" & RF.Fmt(r1) & " r2=" & RF.Fmt(r2) & " r3="
          & RF.Fmt(r3) & "\n");
    x.l := -12.0D0;
    x.r := 1.0D0;
    tol := 0.001D0;
    Msg("start at x.l=" & RF.Fmt(x.l, prec3Style) & " x.r="
          & RF.Fmt(x.r, prec3Style) & " tol=" & RF.Fmt(tol));
    <*FATAL Error*>
    BEGIN
      root := FZ.Brent(MyFun, x, tol := tol);
    END;
    Msg(" found  root=" & RF.Fmt(root, prec3Style) & "\n");
    RETURN result;
  END TestBrent;
(*----------------------*)
PROCEDURE TestNewtonRaphson (): BOOLEAN =
  CONST ftn = Module & "TestNewtonRaphson";
  VAR
    result                := TRUE;
    x        : FZ.Bracket;
    tol, root: R.T;
    maxiter  : CARDINAL;
  BEGIN
    Debug(1, ftn, "begin\n");
    Msg("true roots: r1=" & RF.Fmt(r1) & " r2=" & RF.Fmt(r2) & " r3="
          & RF.Fmt(r3) & "\n");
    x.l := 6.0D0;
    x.r := 5.0D0;
    tol := 0.001D0;
    maxiter := 15;
    FOR i := 0 TO 10 DO
      x.r := x.r + 1.1D0;
      Msg("start at x.l=" & RF.Fmt(x.l, prec3Style) & " x.r="
            & RF.Fmt(x.r, prec3Style) & " tol=" & RF.Fmt(tol) & " maxiter="
            & Fmt.Int(maxiter));
      TRY
        root := FZ.NewtonRaphson(MyFun2, x, tol, maxiter);
        Msg(" found  root=" & RF.Fmt(root, prec5Style) & "\n");
      EXCEPT
      | Error (err) =>
          CASE err OF
          | Err.not_bracketed => Msg(" not bracketed\n");
          | Err.out_of_range => Msg(" jumped out\n");
          | Err.not_converging => Msg(" not converging\n");
          ELSE
            <*ASSERT FALSE*>
          END;
      ELSE
        Msg(" other error\n");
      END;
    END;
    RETURN result;
  END TestNewtonRaphson;



(****************************
(*----------------------*)

PROCEDURE TestLaguer():BOOLEAN=
CONST
  ftn = Module & "TestLaguer";
  n=4; m=n-1;
VAR
  result:=TRUE;
  p:=NEW(na.cVector,n);
  x:C.T;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & RF.Fmt(r1)
               & " r2=" & RF.Fmt(r2)
               & " r3=" & RF.Fmt(r3)
               & "\n");
  p^:=ARRAY [0..m] OF C.T
      {C.T(200.0,0.0), C.T(-100.0,0.0),
       C.T(-2.0,0.0),  C.T(1.0,0.0)};
FOR i:=0 TO 10 DO
  x:=C.T(FLOAT(i,R.T),0.5);
  Msg("start at x=" & CF.Fmt(x,prec3Style));
  TRY
    na.laguer(p,m,x);
    Msg(" found x=" & CF.Fmt(x,prec5Style)
      & "\n");
  EXCEPT
  | Error(err) => CASE err OF
                      | Err.divide_by_zero=>Msg(" divide by zero\n");
                      | Err.out_of_range=>Msg(" jumped out\n");
                      | Err.not_converging=>Msg(" not converging\n");
                      END;
  ELSE
    Msg(" other error\n");
  END;
END;
  RETURN result;
END TestLaguer;
(*----------------------*)
PROCEDURE TestZRoots():BOOLEAN=
CONST
  ftn = Module & "TestZRoots";
  n=4; m=n-1;
VAR
  result:=TRUE;
  p:=NEW(na.cVector,n);
  roots:na.cVector;
BEGIN
  Debug(1,ftn,"begin\n");
  Msg("true roots: r1=" & RF.Fmt(r1)
               & " r2=" & RF.Fmt(r2)
               & " r3=" & RF.Fmt(r3)
               & "\n");
  p^:=ARRAY [0..m] OF C.T
      {C.T(200.0,0.0), C.T(-100.0,0.0),
       C.T(-2.0,0.0),  C.T(1.0,0.0)};
  TRY
    na.zroots(p,roots,polish:=FALSE);
    Msg("\n     raw roots:");
    FOR i:=0 TO m-1 DO
      Msg(" root[" & Fmt.Int(i) & "]="
        & na.Ctext(roots[i],prec:=4));
    END;
    na.zroots(p,roots,polish:=TRUE);
    Msg("\npolished roots:");
    FOR i:=0 TO m-1 DO
      Msg(" root[" & Fmt.Int(i) & "]="
        & na.Ctext(roots[i],prec:=4));
    END;
  EXCEPT
  | Error(err) => CASE err OF
                      | Err.divide_by_zero=>Msg(" divide by zero\n");
                      | Err.out_of_range=>Msg(" jumped out\n");
                      | Err.not_converging=>Msg(" not converging\n");
                      END;
  ELSE
    Msg(" other error\n");
  END;

  RETURN result;
END TestZRoots;
******************************)
PROCEDURE WritePowerSeq (s: REF IR.PowerSumSeq) =
  BEGIN
    FOR j := 0 TO LAST(s^) DO Msg(Fmt.Int(s[j]) & ", "); END;
    Msg("\n");
  END WritePowerSeq;

<*FATAL Thread.Alerted, Wr.Failure*>
(*-----------------------*)
PROCEDURE TestPowerSeq (): BOOLEAN =

  PROCEDURE TestPowerPoly (READONLY x: IR.PowerSumSeq) =
    VAR
      p: IR.T;
      y: REF IR.PowerSumSeq;
    <*FATAL Error*>
    BEGIN
      p := IR.FromPowerSumSeq(x);
      Msg(IPF.Fmt(p) & "\n");
      y := IR.ToPowerSumSeq(p, NUMBER(x));
      <*ASSERT NUMBER(x)=NUMBER(y^)*>
      FOR j := 0 TO LAST(y^) DO
        Msg(Fmt.Int(y[j]) & ", ");
        <*ASSERT x[j]=y[j]*>
      END;
      Msg("\n");
    END TestPowerPoly;

  PROCEDURE TestPolyPower (x: IR.T) =
    VAR
      y: IR.T;
      s: REF IR.PowerSumSeq;
    <*FATAL Error*>
    BEGIN
      s := IR.ToPowerSumSeq(x, LAST(x^));
      WritePowerSeq(s);
      y := IR.FromPowerSumSeq(s^);
      Msg(IPF.Fmt(y) & "\n");
      <*ASSERT IR.Equal(x,y)*>
    END TestPolyPower;

  CONST ftn = Module & "TestPowerSeq";
  VAR result := TRUE;

  BEGIN
    Debug(1, ftn, "begin\n");
    TestPowerPoly(IR.PowerSumSeq{0, 2, 0, 2, 0, 2, 0, 2, 0, 2, 0, 2});
    TestPowerPoly(IR.PowerSumSeq{2, 2, 2, 2, 2, 2});
    TestPowerPoly(IR.PowerSumSeq{3, 3, 3, 3, 3, 3});
    TestPowerPoly(IR.PowerSumSeq{-2, 2, -2, 2});
    TestPowerPoly(IR.PowerSumSeq{-1, -1, -1, -1, -1, -1, -1});
    TestPowerPoly(IR.PowerSumSeq{-2, -2, -2, -2, -2, -2, -2});
    TestPowerPoly(IR.PowerSumSeq{4, 8, 16, 32, 64});

    VAR p: IR.T;
    BEGIN
      p := IR.New(7);
      p^ := IR.TBody{1, 1, 1, 1, 1, 1, 1, 1};
      TestPolyPower(p);
      p^ := IR.TBody{729, 243, 81, 27, 9, 3, 1, 1};
      TestPolyPower(p);
      p := IR.New(2);
      p^ := IR.TBody{4, 4, 1};
      TestPolyPower(p);
    END;

    RETURN result;
  END TestPowerSeq;
(*----------------------*)
PROCEDURE TestRootOp (): BOOLEAN =

  PROCEDURE TestSingle (READONLY rootx, rooty: ARRAY OF I.T;
                                 op          : PROCEDURE (x, y: I.T): I.T;
                                 opr         : PROCEDURE (x, y: IR.T): IR.T) =
    VAR x, y, z: IR.T;
    BEGIN
      x := IR.FromRoots(rootx);
      y := IR.FromRoots(rooty);
      z := opr(x, y);
      (*
          Msg("x-poly " & IPF.Fmt(x) & "\t");
          Msg("y-poly " & IPF.Fmt(y) & "\n");
          Msg("z-poly " & IPF.Fmt(z) & "\n");
          WritePowerSeq(IR.ToPowerSumSeq(x,LAST(z^)));
          WritePowerSeq(IR.ToPowerSumSeq(y,LAST(z^)));
          WritePowerSeq(IR.ToPowerSumSeq(z,LAST(z^)));
          Msg("test zeroes ");
      *)
      FOR j := 0 TO LAST(rootx) DO
        FOR k := 0 TO LAST(rooty) DO
          (*
                  Msg(Fmt.Int(j) & "," & Fmt.Int(k) & "  ");
          *)
          <*ASSERT I.IsZero(IP.Eval(z,op(rootx[j],rooty[k])))*>
        END;
      END;
      (*
          Msg("\n");
      *)
    END TestSingle;

  PROCEDURE TestPower (READONLY root: ARRAY OF I.T; n: I.T; ) =
    VAR x, z: IR.T;
    BEGIN
      x := IR.FromRoots(root);
      z := IR.PowN(x, n);
      (*
          Msg("x-poly " & IPF.Fmt(x) & "\t");
          Msg("pow-poly " & IPF.Fmt(z) & "\n");
          WritePowerSeq(IR.ToPowerSumSeq(x,LAST(z^)));
          WritePowerSeq(IR.ToPowerSumSeq(z,LAST(z^)));
          Msg("test zeroes ");
      *)
      FOR j := 0 TO LAST(root) DO
        VAR pow: I.T := I.One;
        BEGIN
          FOR k := 0 TO n - 1 DO pow := I.Mul(pow, root[j]); END;
          (*
                  Msg(Fmt.Int(j) & "  ");
          *)
          <*ASSERT I.IsZero(IP.Eval(z,pow))*>
        END;
      END;
      (*
          Msg("\n");
      *)
    END TestPower;

  PROCEDURE NewOneChain (len: CARDINAL; c: I.T): IR.T =
    VAR x: IR.T;
    BEGIN
      x := IR.New(len);
      FOR l := 0 TO len - 1 DO x[l] := 1; END;
      x[len] := c;
      RETURN x;
    END NewOneChain;

  CONST ftn = Module & "TestRootOp";
  VAR result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    TestSingle(ARRAY OF I.T{2}, ARRAY OF I.T{3}, I.Mul, IR.Mul);
    TestSingle(ARRAY OF I.T{1, 1}, ARRAY OF I.T{3}, I.Mul, IR.Mul);
    TestSingle(ARRAY OF I.T{1, 1, 1, 1}, ARRAY OF I.T{3}, I.Mul, IR.Mul);
    TestSingle(ARRAY OF I.T{1, 1}, ARRAY OF I.T{2, 2}, I.Mul, IR.Mul);
    TestSingle(ARRAY OF I.T{1, -1, 2}, ARRAY OF I.T{0, 2}, I.Mul, IR.Mul);
    TestSingle(ARRAY OF I.T{1, -1, 2}, ARRAY OF I.T{1, -1}, I.Mul, IR.Mul);
    TestSingle(ARRAY OF I.T{2, 2}, ARRAY OF I.T{3, 3}, I.Mul, IR.Mul);

    TestSingle(ARRAY OF I.T{2}, ARRAY OF I.T{3}, I.Add, IR.Add);
    TestSingle(ARRAY OF I.T{2}, ARRAY OF I.T{3, 0}, I.Add, IR.Add);
    TestSingle(ARRAY OF I.T{1, 1}, ARRAY OF I.T{3}, I.Add, IR.Add);
    TestSingle(ARRAY OF I.T{1, 1, 1, 1}, ARRAY OF I.T{3}, I.Add, IR.Add);
    TestSingle(ARRAY OF I.T{1, 1}, ARRAY OF I.T{2, 2}, I.Add, IR.Add);
    TestSingle(ARRAY OF I.T{1, -1, 2}, ARRAY OF I.T{0, 2}, I.Add, IR.Add);
    TestSingle(ARRAY OF I.T{1, -1, 2}, ARRAY OF I.T{1, -1}, I.Add, IR.Add);
    TestSingle(ARRAY OF I.T{2, 2}, ARRAY OF I.T{3, 3}, I.Add, IR.Add);

    VAR x, y, z: IR.T;
    BEGIN
      FOR j := 1 TO 7 DO
        x := NewOneChain(j, 2);
        (*Msg("x"&IPF.Fmt(x));*)
        FOR k := 1 TO 7 DO
          y := NewOneChain(k, 3);
          (*Msg("y"&IPF.Fmt(y));*)
          IF j + k < 8 THEN      (*otherwise internal overflow*)
            z := IR.Mul(x, y);
            (*
                      Msg(Fmt.FN("%s,%s - %s * %s = %s\n", ARRAY OF TEXT
                        {Fmt.Int(j), Fmt.Int(k), IPF.Fmt(x), IPF.Fmt(y), IPF.Fmt(z)}));
            *)
          END;
          IF j + k < 7 THEN      (*otherwise internal overflow*)
            z := IR.Add(x, y);
            (*
                      Msg(Fmt.FN("%s,%s - %s + %s = %s\n", ARRAY OF TEXT
                        {Fmt.Int(j), Fmt.Int(k), IPF.Fmt(x), IPF.Fmt(y), IPF.Fmt(z)}));
            *)
          END;
        END;
      END;
    END;

    Msg(
      "(1+6t)^4 " & IPF.Fmt(IR.FromRoots(ARRAY OF I.T{6, 6, 6, 6})) & "\t");
    VAR x, y, z: IR.T;
    BEGIN
      x := IR.New(2);
      x^ := IR.TBody{1, -4, 4};
      y := IR.New(2);
      y^ := IR.TBody{1, -6, 9};
      z := IR.Mul(x, y);
      Msg(IPF.Fmt(z) & "\n");

      x := IR.New(2);
      x^ := IR.TBody{1, -5, 6};  (* 1/2, 1/3 *)
      (* y:=IR.New(2); y^:=IR.TBody{1,-6,5}; (* 1, 1/5 - 5^2 6^2*) *)
      y := IR.New(3);
      y^ := IR.TBody{-1, 7, -11, 5}; (* 1, 1, 1/5 - 5^4 6^3 *)
      (* y:=IR.New(4); y^:=IR.TBody{1,-8,18,-16,5}; (* 1, 1, 1, 1/5 - 5^6
         6^4*) *)
      (*
        (2,2) -> (2,2)
        (2,3) -> (4,3)
        (2,4) -> (6,4)
      *)
      z := IR.Mul(x, y);
      (*
          Msg(IPF.Fmt(z) & "\n");
      *)

      x := IR.New(1);
      x^ := IR.TBody{-1, 3};     (* 1/3 *)
      (* x:=IR.New(2); x^:=IR.TBody{1,-8,15}; (* 1/3, 1/5 *) *)
      y := IR.New(2);
      y^ := IR.TBody{1, -9, 14}; (* 1/2, 1/7*)
      (*7^2 3^2 2^2 5^2*)
      (* y:=IR.New(3); y^:=IR.TBody{-1,20,-113,154}; (* 1/2, 1/7, 1/11*) *)
      (* y:=IR.New(3); y^:=IR.TBody{-1,11,-32,28}; (* 1/2, 1/2, 1/7*) *)
      (* y:=IR.New(3); y^:=IR.TBody{0,1,-9,14}; (* 1/2, 1/7*) *)
      z := IR.Mul(x, y);
      (*
          Msg(IPF.Fmt(z) & "\n");
      *)
    END;

    TestPower(ARRAY OF I.T{1, 1, 1}, 3);
    TestPower(ARRAY OF I.T{2, 2}, 2);
    TestPower(ARRAY OF I.T{2, 2, 2, 2}, 1);
    TestPower(ARRAY OF I.T{2, 2, 2, 2}, 2);
    TestPower(ARRAY OF I.T{2, 2, 2, 2}, 3);
    TestPower(ARRAY OF I.T{3, 2, 1}, 2);
    TestPower(ARRAY OF I.T{3, 2, 1}, 3);

    VAR x, z: IR.T;
    BEGIN
      (*
          x:=IR.New(2); x^:=IR.TBody{1,-4,4};
      *)
      FOR j := 1 TO 7 DO
        x := NewOneChain(j, 2);
        FOR k := 1 TO 4 DO
          z := IR.PowN(x, k);
          (*
                  Msg(Fmt.FN("%s^%s = %s\n",
                             ARRAY OF TEXT{IPF.Fmt(x), Fmt.Int(k), IPF.Fmt(z)}));
          *)
          <*ASSERT IP.Equal(z,IR.PowNSlow(x,k))*>
        END;
      END;
    END;

    RETURN result;
  END TestRootOp;

(*-------------------------*)
PROCEDURE TestRoot (): BOOLEAN =
  <*UNUSED*>
  CONST ftn = Module & "TestRoot";
  VAR result := TRUE;
  BEGIN
    NewLine();
    EVAL TestQuadratic();
    NewLine();
    EVAL TestRootApproximation();
    NewLine();
    EVAL TestBracketOut();
    NewLine();
    EVAL TestBracketIn();
    NewLine();
    EVAL TestBisection();
    NewLine();
    EVAL TestBrent();
    NewLine();
    EVAL TestNewtonRaphson();
    NewLine();
    EVAL TestPowerSeq();
    NewLine();
    EVAL TestRootOp();

    RETURN result;
  END TestRoot;
(*=======================*)
BEGIN
END TestRoot.
