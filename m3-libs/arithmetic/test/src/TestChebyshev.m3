MODULE TestChebyshev EXPORTS Test;
(*Copyright (c) 1995, Harry George
Abstract: Test driver for Modula-3 rendition of
          Numerical Recipes in C, 1992.

12/27/95  Harry George   Initial version: Ch 5

*)

IMPORT Fmt;
IMPORT LongRealBasic AS R,
       LongRealTrans AS RT,
       LongRealChebyPolynomialFast AS CP,
       xUtils;

<*FATAL xUtils.Error *>

(*=======================*)
CONST Module = "TestChebyshev.";
(*=======================*)
(*-----------------------*)
PROCEDURE TestEulerSum():BOOLEAN=
CONST
  ftn = Module & "TestEulerSum";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  RETURN result;
END TestEulerSum;
(*-----------------------*)
PROCEDURE TestCheby():BOOLEAN=
CONST
  ftn = Module & "TestCheby";
VAR
  cheby:=CP.Expand(RT.Sin,20);
  chebm,
  chebd,
  chebi:CP.T;
  monomial:=CP.New(5);
  m:CARDINAL;
  x,y1,y2,y3:R.T;
  dc:R.T:=R.Zero;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  m:=CP.FindUpperExp(cheby);
  Msg("m=" & Fmt.Int(m) & "\n");
  chebm:=CP.Abort(cheby,m);
  monomial^:=ARRAY OF R.T{R.Zero,R.Zero,R.Zero,R.Zero,R.Zero,R.One};
  FOR i:=1 TO 9 DO
    x:=-R.One+FLOAT(0.2,R.T)*FLOAT(i,R.T);
    y1:=RT.Sin(x);
    y2:=CP.Eval(cheby,x);
    Msg("sin(" & Fmt.LongReal(x,prec:=2)
      & ")= " & Fmt.LongReal(y1,prec:=4)
      & ", cheby:"  & Fmt.LongReal(y2,prec:=4)
      & ", mono: " & Fmt.LongReal(CP.Eval(monomial,x),prec:=4)
      & "\n");
    <*ASSERT ABS(y1-y2) < 0.00001D0 *>
  END;

  Msg("Doing derivatives and integrals\n");
  chebd:=CP.Derive(cheby);
  m:=CP.FindUpperExp(chebd);
  chebd:=CP.Abort(chebd,m);
  Msg("chebd = D(cheby), m=" & Fmt.Int(m) & "\n");

  chebi:=CP.Integrate(cheby);
  m:=CP.FindUpperExp(chebi);
  chebi:=CP.Abort(chebi,m);
  Msg("chebi = Integral(cheby), m=" & Fmt.Int(m) & "\n");

  dc:=-RT.Cos(R.Zero)(*R.One*)-CP.Eval(chebi,R.Zero);
(*
  chebi:=CP.Abort(chebi,CP.FindUpperExp(chebi));
  chebd:=CP.Abort(chebd,CP.FindUpperExp(chebd));
*)
  FOR i:=1 TO 9 DO
    x:=-R.One+FLOAT(0.2,R.T)*FLOAT(i,R.T);
    y1:=RT.Cos(x);
    y2:=CP.Eval(chebd,x);
    y3:=CP.Eval(chebi,x)+dc;
    Msg(Fmt.LongReal(x,prec:=2)
      & " -> cos: " & Fmt.LongReal(y1,prec:=4)
      & ", d sin: " & Fmt.LongReal(y2,prec:=4)
      & ", St sin: " & Fmt.LongReal(y3,prec:=4)
      & "\n");
    <*ASSERT ABS(y1-y2) < 0.00001D0 *>
    <*ASSERT ABS(y1+y3) < 0.00001D0 *>
  END;

  RETURN result;
END TestCheby;
(*-----------------------*)
PROCEDURE TestChebyshev():BOOLEAN=
<*UNUSED*>
CONST
  ftn = Module & "TestChebyshev";
BEGIN
  NewLine(); EVAL TestEulerSum();
  NewLine(); EVAL TestCheby();
  RETURN TRUE;
END TestChebyshev;
(*=======================*)
BEGIN
END TestChebyshev.
