MODULE tPoly EXPORTS test;
(*Copyright (c) 1996, m3na project
Abstract:  Tests for Poly module.

2/4/96    Harry George   Initial version

*)

IMPORT IO,Wr,Fmt;
FROM xReal64 IMPORT REAL64;
IMPORT xReal64 AS R, xPoly AS P;
(*=======================*)
CONST
  Module = "tPoly.";

(*----------------------*)
PROCEDURE test_ABC():BOOLEAN=
CONST
  ftn = Module & "test_ABC";
VAR
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");

  RETURN result;   
END test_ABC;
(*=======================*)
TYPE
  Poly4 = ARRAY [0..3] OF REAL64;
  Poly5 = ARRAY [0..4] OF REAL64;
(*-----------------------*)
PROCEDURE test_add():BOOLEAN=
CONST
  ftn = Module & "test_add";
VAR
  u:=NEW(P.Poly,5);
  v:=NEW(P.Poly,4);
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  msg("u="   & P.fmt(u) & "\n");
  msg("v="   & P.fmt(v) & "\n");
  msg("u+v=" & P.fmt(P.add(u,v)) & "\n");          
  msg("v+u=" & P.fmt(P.add(v,u)) & "\n");          
  RETURN result;
END test_add;
(*-----------------------*)
PROCEDURE test_sub():BOOLEAN=
CONST
  ftn = Module & "test_sub";
VAR
  u:=NEW(P.Poly,5);
  v:=NEW(P.Poly,4);
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  msg("u="   & P.fmt(u) & "\n");
  msg("v="   & P.fmt(v) & "\n");
  msg("u-v=" & P.fmt(P.sub(u,v)) & "\n");          
  msg("v-u=" & P.fmt(P.sub(v,u)) & "\n");          
  RETURN result;
END test_sub;
(*-----------------------*)
PROCEDURE test_mul():BOOLEAN=
CONST
  ftn = Module & "test_mul";
VAR
  u:=NEW(P.Poly,5);
  v:=NEW(P.Poly,4);
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  msg("u="   & P.fmt(u) & "\n");
  msg("v="   & P.fmt(v) & "\n");
  msg("u*v=" & P.fmt(P.mul(u,v)) & "\n");          
  msg("v*u=" & P.fmt(P.mul(v,u)) & "\n");          
  RETURN result;
END test_mul;
(*-----------------------*)
PROCEDURE test_div():BOOLEAN=
CONST
  ftn = Module & "test_div";
VAR
  u:=NEW(P.Poly,5);
  v:=NEW(P.Poly,4);
  q,r:P.Poly;
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  u^:=Poly5{1.0D0,2.0D0,3.0D0,4.0D0,5.0D0};
  v^:=Poly4{0.1D0,0.2D0,0.3D0,0.4D0};
  msg("u="   & P.fmt(u) & "\n");
  msg("v="   & P.fmt(v) & "\n");
  msg("u/v="); P.div(u,v,q,r);
      msg(P.fmt(q) & " rem=" & P.fmt(r) & "\n");          
  msg("v/u="); P.div(v,u,q,r);
      msg(P.fmt(q) & " rem=" & P.fmt(r) & "\n");          
  RETURN result;
END test_div;
(*-----------------------*)
PROCEDURE test_deriv():BOOLEAN=
CONST
  ftn = Module & "test_deriv";
VAR
  p:=NEW(P.Poly,5);
  nd:=3;
  pd:=NEW(R.Array,nd+1);
  x:=1.0D0;
  result:=TRUE;
BEGIN
  debug(1,ftn,"begin\n");
  p^:=Poly5{1.0D0,1.0D0,1.0D0,1.0D0,1.0D0};
  msg("p="   & P.fmt(p) & "\n");
  FOR j:=1 TO 5 DO
    x:=FLOAT(j,REAL64);
    msg("x=" & R.fmt(x));
    P.deriv(p,x,pd,nd);
    FOR i:=0 TO nd DO
      msg(" d"& Fmt.Int(i) & "=" & R.fmt(pd[i]));
    END;
    msg("\n");
  END;
  RETURN result;
END test_deriv;
(*-------------------------*)
PROCEDURE test_Poly():BOOLEAN=
CONST ftn = Module & "test_oly";
VAR result:=TRUE;
BEGIN
  newline(); EVAL test_add();
  newline(); EVAL test_sub();
  newline(); EVAL test_mul();
  newline(); EVAL test_div();
  newline(); EVAL test_deriv();
  RETURN result;
END test_Poly;
(*=======================*)
BEGIN
END tPoly.
