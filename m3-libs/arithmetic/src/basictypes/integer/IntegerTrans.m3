MODULE IntegerTrans;
(*Arithmetic for Modula-3, see doc for details

Abstract: Integers

2/17/96  Harry George    Initial version
*)

IMPORT Word;

CONST Module = "IntegerTrans.";
(*==========================*)

(*============================*)
(* Integer Approximations     *)
(*============================*)
(*----------------------*)
PROCEDURE SqRt(N:[0..1073741823]):CARDINAL=
(*returns integer sqrt of n*)
(*from P. Heinrich, "A Fast Integer Square Root",
Dr. Dobbs, Apr 1996, pp 113-114*)
<*UNUSED*>
CONST ftn = Module & "sqrt";
VAR
  u,v,u2,n:Word.T;
  vbit:CARDINAL;
BEGIN
  (*---check quick victory---*)
  IF N<2 THEN RETURN N; END;

  (*---find highest bit---*)
  u:=N; vbit:=0;
  LOOP
    u:=Word.RightShift(u,2);
    IF u=0 THEN EXIT; END;
    INC(vbit);
  END;

  (*---use vbit to make v---*)
  v:=Word.LeftShift(1,vbit);

  (*---u is approx v---*)
  u:=v;
  (*---u^2 is shifted twice as far as u---*)
  u2:=Word.LeftShift(u,vbit);

  (*---move vbit toward 0, recalculating u as we go---*)
  WHILE vbit>0 DO
    DEC(vbit);
    v:=Word.RightShift(v,1);

    (*---build the current n---*)
    (*---1. get v*(2u+v)---*)
    n:=Word.LeftShift(u+u+v,vbit);
    (*---2. add the u^2 term---*)
    INC(n,u2);

    (*---are we big enough yet?---*)
    IF n<=N THEN
      (*---new u is (u+v)---*)
      INC(u,v);
      (*---current best estimate of u^2---*)
      u2:=n;
    END;
  END;
  RETURN u;
END SqRt;

(*============================*)
(* CORDIC Functions           *)
(*============================*)

(*----------------*)
PROCEDURE SinCos(theta:Cordic;    (*given this angle*)
                  VAR s,c:INTEGER)= (*return sin and cos*)
<*UNUSED*>
CONST
  ftn = Module & "sin_cos";

CONST
  AtanTbl = ARRAY [0..CordicBits] OF CARDINAL{
    32768,19344,10221,5188,2604,1303,652,326,163,81,41,20,10,5,3,1,1};
  n1=FIRST(AtanTbl); nn=LAST(AtanTbl);

  Quad0Boundary = CordicBase*0;
  Quad1Boundary = CordicBase*1;
  Quad2Boundary = CordicBase*2;
  Quad3Boundary = CordicBase*3;
  Quad4Boundary = CordicBase*4;

VAR
  z:INTEGER;
  x:INTEGER:=39796;  (*initialize here to overcome the expansion*)
  y:INTEGER:=0;
  xtmp:INTEGER;
  quadrant:[1..4];
BEGIN
  (*---find quadrant---*)
  IF    theta < Quad1Boundary THEN
    quadrant:=1; z:=theta - Quad0Boundary;
  ELSIF theta < Quad2Boundary THEN
    quadrant:=2; z:=Quad2Boundary - theta;
  ELSIF theta < Quad3Boundary THEN
    quadrant:=3; z:=theta - Quad2Boundary;
  ELSE (*known to be < Quad4Boundary, due to typechecking*)
    quadrant:=4; z:=Quad4Boundary - theta;
  END;

  (*---negate z so we can go toward 0---*)
  z:=-z;
  (*---compute---*)
  FOR i:=n1 TO nn DO
    IF z<0 THEN (*counter-clockwise*)
      xtmp:=x - Word.RightShift(y,i);
      y   :=y + Word.RightShift(x,i);
      x   :=xtmp;
      INC(z,AtanTbl[i]);
    ELSE        (*clockwise*)
      xtmp:=x + Word.RightShift(y,i);
      y   :=y - Word.RightShift(x,i);
      x   :=xtmp;
      DEC(z,AtanTbl[i]);
    END;
  END;

  (*---resolve quadrants---*)
  CASE quadrant OF
  | 1 => c:= x; s:= y;
  | 2 => c:=-x; s:= y;
  | 3 => c:=-x; s:=-y;
  | 4 => c:= x; s:=-y;
  END;
END SinCos;

(*==========================*)
BEGIN
END IntegerTrans.
