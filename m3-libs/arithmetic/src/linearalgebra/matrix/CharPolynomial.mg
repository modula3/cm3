GENERIC MODULE CharPolynomial(R,Rt,M);
(*
Abstract: Characteristic polynomial of a matrix

*)

IMPORT Arithmetic AS Arith;

<*UNUSED*> CONST Module = "CharPolynomial.";

(*-----------------*)
(*it is trace(x^n) = lambda[1]^n+...+lambda[n]^n
  thus we get sequence of power sums if we compute
  the trace of successive powers of x*)
PROCEDURE CharPolynomial(x:M.T):Rt.T RAISES{Arith.Error}=
VAR
  p:REF Rt.PowerSumSeq;
  pow:M.T;
BEGIN
  IF NUMBER(x^)#NUMBER(x[0]) THEN
    RAISE Arith.Error(NEW(Arith.ErrorSizeMismatch).init());
  END;
  p:=NEW(REF Rt.PowerSumSeq,NUMBER(x^));
  pow:=x;
  p[0]:=M.Trace(pow);
  FOR j:=1 TO LAST(p^) DO
    pow:=M.Mul(pow,x);
    p[j]:=M.Trace(pow);
  END;
  RETURN Rt.FromPowerSumSeq(p^);
END CharPolynomial;

(*-----------------*)
PROCEDURE CompanionMatrix(x:Rt.T):M.T RAISES{Arith.Error}=
VAR
  y:M.T;
BEGIN
  IF NOT R.Equal(x[LAST(x^)],R.One) THEN
    RAISE Arith.Error(NEW(Arith.ErrorIndivisible).init());
  END;
  y:=M.New(LAST(x^),LAST(x^));
  FOR j:=0 TO LAST (y^)-1 DO
    y[j,j+1] := R.One;
  END;
  FOR j:=0 TO LAST (y[0]) DO
    y[LAST(y^),j] := R.Neg(x[j]);
  END;
  RETURN y;
END CompanionMatrix;

(*-----------------*)
BEGIN
END CharPolynomial.
