MODULE TestPoly EXPORTS Test;
(*Copyright (c) 1996, m3na project
Abstract:  TestS for Poly module.

2/4/96    Harry George   Initial version

*)

IMPORT IO,Wr,Fmt;
FROM xReal64 IMPORT REAL64;
IMPORT xReal64 AS R, xPoly AS P;
(*=======================*)
CONST
  Module = "TestPoly.";

(*----------------------*)
PROCEDURE TestABC():BOOLEAN=
CONST
  ftn = Module & "TestABC";
VAR
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");

  RETURN result;   
END TestABC;
(*=======================*)
TYPE
  Poly4 = ARRAY [0..3] OF REAL64;
  Poly5 = ARRAY [0..4] OF REAL64;
(*-----------------------*)
PROCEDURE TestAdd():BOOLEAN=
CONST
  ftn = Module & "TestAdd";
VAR
  u:=NEW(P.Poly,5);
  v:=NEW(P.Poly,4);
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  Msg("u="   & P.fmt(u) & "\n");
  Msg("v="   & P.fmt(v) & "\n");
  Msg("u+v=" & P.fmt(P.add(u,v)) & "\n");          
  Msg("v+u=" & P.fmt(P.add(v,u)) & "\n");          
  RETURN result;
END TestAdd;
(*-----------------------*)
PROCEDURE TestSub():BOOLEAN=
CONST
  ftn = Module & "TestSub";
VAR
  u:=NEW(P.Poly,5);
  v:=NEW(P.Poly,4);
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  Msg("u="   & P.fmt(u) & "\n");
  Msg("v="   & P.fmt(v) & "\n");
  Msg("u-v=" & P.fmt(P.sub(u,v)) & "\n");          
  Msg("v-u=" & P.fmt(P.sub(v,u)) & "\n");          
  RETURN result;
END TestSub;
(*-----------------------*)
PROCEDURE TestMul():BOOLEAN=
CONST
  ftn = Module & "TestMul";
VAR
  u:=NEW(P.Poly,5);
  v:=NEW(P.Poly,4);
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  Msg("u="   & P.fmt(u) & "\n");
  Msg("v="   & P.fmt(v) & "\n");
  Msg("u*v=" & P.fmt(P.mul(u,v)) & "\n");          
  Msg("v*u=" & P.fmt(P.mul(v,u)) & "\n");          
  RETURN result;
END TestMul;
(*-----------------------*)
PROCEDURE TestDiv():BOOLEAN=
CONST
  ftn = Module & "TestDiv";
VAR
  u:=NEW(P.Poly,5);
  v:=NEW(P.Poly,4);
  q,r:P.Poly;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  Msg("u="   & P.fmt(u) & "\n");
  Msg("v="   & P.fmt(v) & "\n");
  Msg("u/v="); P.div(u,v,q,r);
      Msg(P.fmt(q) & " rem=" & P.fmt(r) & "\n");          
  Msg("v/u="); P.div(v,u,q,r);
      Msg(P.fmt(q) & " rem=" & P.fmt(r) & "\n");          
  RETURN result;
END TestDiv;
(*-----------------------*)
PROCEDURE TestDeriv():BOOLEAN=
CONST
  ftn = Module & "TestDeriv";
VAR
  p:=NEW(P.Poly,5);
  nd:=3;
  pd:=NEW(R.Array,nd+1);
  x:=1.0D0;
  result:=TRUE;
BEGIN
  Debug(1,ftn,"begin\n");
  p^:=Poly5{1.0D0,1.0D0,1.0D0,1.0D0,1.0D0};
  Msg("p="   & P.fmt(p) & "\n");
  FOR j:=1 TO 5 DO
    x:=FLOAT(j,REAL64);
    Msg("x=" & R.fmt(x));
    P.deriv(p,x,pd,nd);
    FOR i:=0 TO nd DO
      Msg(" d"& Fmt.Int(i) & "=" & R.fmt(pd[i]));
    END;
    Msg("\n");
  END;
  RETURN result;
END TestDeriv;
(*-------------------------*)
PROCEDURE TestPoly():BOOLEAN=
CONST ftn = Module & "Testoly";
VAR result:=TRUE;
BEGIN
  NewLine(); EVAL TestAdd();
  NewLine(); EVAL TestSub();
  NewLine(); EVAL TestMul();
  NewLine(); EVAL TestDiv();
  NewLine(); EVAL TestDeriv();
  RETURN result;
END TestPoly;
(*=======================*)
BEGIN
END TestPoly.
