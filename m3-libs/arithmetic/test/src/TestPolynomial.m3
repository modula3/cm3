MODULE TestPolynomial EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for Poly module.

2/4/96    Harry George   Initial version

*)

IMPORT (*IO,Wr,*)Fmt,
       LongRealBasic  AS R,
       LongRealFmtLex AS RF,
       LongRealPolynomial       AS P,
       LongRealPolynomialFmtLex AS PF;
(*=======================*)
CONST
  Module = "TestPolynomial.";

(*=======================*)
TYPE
  Poly4 = ARRAY [0..3] OF R.T;
  Poly5 = ARRAY [0..4] OF R.T;
(*-----------------------*)
<*FATAL ANY*>
PROCEDURE TestAdd():BOOLEAN=
CONST
  ftn = Module & "TestAdd";
VAR
  u:=NEW(P.T,5);
  v:=NEW(P.T,4);
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  Msg("u="   & PF.Fmt(u) & "\n");
  Msg("v="   & PF.Fmt(v) & "\n");
  Msg("u+v=" & PF.Fmt(P.Add(u,v)) & "\n");
  Msg("v+u=" & PF.Fmt(P.Add(v,u)) & "\n");
  <*ASSERT P.Equal(P.Add(u,v),P.Add(v,u))*>
  RETURN result;
END TestAdd;
(*-----------------------*)
PROCEDURE TestSub():BOOLEAN=
CONST
  ftn = Module & "TestSub";
VAR
  u:=NEW(P.T,5);
  v:=NEW(P.T,4);
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  Msg("u="   & PF.Fmt(u) & "\n");
  Msg("v="   & PF.Fmt(v) & "\n");
  Msg("u-v=" & PF.Fmt(P.Sub(u,v)) & "\n");
  Msg("v-u=" & PF.Fmt(P.Sub(v,u)) & "\n");
  <*ASSERT P.Equal(P.Add(u,P.Neg(v)),P.Sub(u,v))*>
  <*ASSERT P.Equal(P.Add(v,P.Sub(u,v)),u)*>
  RETURN result;
END TestSub;
(*-----------------------*)
PROCEDURE TestMul():BOOLEAN=
CONST
  ftn = Module & "TestMul";
VAR
  u:=NEW(P.T,5);
  v:=NEW(P.T,4);
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  Msg("u="   & PF.Fmt(u) & "\n");
  Msg("v="   & PF.Fmt(v) & "\n");
  Msg("u*v=" & PF.Fmt(P.Mul(u,v)) & "\n");
  Msg("v*u=" & PF.Fmt(P.Mul(v,u)) & "\n");
  <*ASSERT P.Equal(P.Mul(u,v),P.Mul(v,u))*>
  RETURN result;
END TestMul;
(*-----------------------*)
PROCEDURE TestDiv():BOOLEAN=
CONST
  ftn = Module & "TestDiv";
VAR
  u:=NEW(P.T,5);
  v:=NEW(P.T,4);
  qr:P.QuotRem;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  Msg("u="   & PF.Fmt(u) & "\n");
  Msg("v="   & PF.Fmt(v) & "\n");
  Msg("u/v="); qr:=P.DivMod(u,v);
      Msg(PF.Fmt(qr.quot) & " rem=" & PF.Fmt(qr.rem) & "\n");
  <*ASSERT NUMBER(qr.rem^)<NUMBER(v^)*>
  <*ASSERT P.Equal(P.Add(P.Mul(v,qr.quot),qr.rem),u)*>
  Msg("v/u="); qr:=P.DivMod(v,u);
      Msg(PF.Fmt(qr.quot) & " rem=" & PF.Fmt(qr.rem) & "\n");
  <*ASSERT NUMBER(qr.rem^)<NUMBER(u^)*>
  <*ASSERT P.Equal(P.Add(P.Mul(u,qr.quot),qr.rem),v)*>
  RETURN result;
END TestDiv;
(*-----------------------*)
PROCEDURE TestDeriv():BOOLEAN=
CONST
  ftn = Module & "TestDeriv";
  nd  = 4;
VAR
  p:ARRAY [0..nd] OF P.T;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  p[0]:=P.FromArray(Poly5{1.0D0,-1.0D0,-2.0D0,3.0D0,1.0D0});
  FOR i:=0 TO nd-1 DO
    p[i+1]:=P.Derive(p[i]);
  END;
  Msg("p="   & PF.Fmt(p[0]) & "\n");
  FOR j:=0 TO 9 DO
    VAR
      x:=FLOAT(j,R.T);
      pd:=P.EvalDerivative(p[0],x,nd+1);
    BEGIN
      Msg("x=" & RF.Fmt(x));
      FOR i:=0 TO nd DO
        Msg(" d"& Fmt.Int(i) & "=" & RF.Fmt(pd[i]));
        <*ASSERT pd[i]=P.Eval(p[i],x)*>
      END;
    END;
    Msg("\n");
  END;
  RETURN result;
END TestDeriv;
(*-------------------------*)
PROCEDURE TestPolynomial():BOOLEAN=
<*UNUSED*> CONST ftn = Module & "TestPolynomial";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestAdd();
  NewLine(); EVAL TestSub();
  NewLine(); EVAL TestMul();
  NewLine(); EVAL TestDiv();
  NewLine(); EVAL TestDeriv();
  RETURN result;
END TestPolynomial;
(*=======================*)
BEGIN
END TestPolynomial.
