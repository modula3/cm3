MODULE RandomBayesDurham;
(*Copyright (c) 1996, m3na project

Abstract: <describe>

1/1/96  <name>    Initial version
*)
IMPORT LongRealBasic AS R;

FROM RandomBasic IMPORT Min,Max;
IMPORT RandomRep, RandomBasic;

CONST Module = "RandomBayesDurham.";
(*==========================*)
(*---------------------*)
REVEAL T = RandomBasic.T BRANDED OBJECT
    z1:INTEGER;
  OVERRIDES
    engine:=Engine;
  END;

PROCEDURE New(seed:[1..LAST(INTEGER)]:=1;
                 ):T =
VAR
  t:=NEW(T);
BEGIN
  t.z1:=seed;
  EVAL t.engine();
  RETURN t;
END New;

PROCEDURE Engine(SELF:T):R.T=
<*UNUSED*> CONST ftn= Module & "Engine";
CONST
  (*use Park/Miller alternatives (NR92, pg279)*)
  m = 2147483647;
  m_recip = R.One / FLOAT(m,R.T);
  a = 48271;
  q = 44488;
  r = 3399;

VAR
  z1,tmp:INTEGER;
  result:R.T;
BEGIN
  z1:=SELF.z1;
  tmp:=z1 DIV q;
  z1:=a*(z1-tmp*q)-r*tmp;
  IF z1 < 0 THEN INC(z1,m) END;

  (*---convert and check for out of bounds---*)
  result:= FLOAT(z1,R.T) * m_recip;
  IF result < Min THEN
    result:=Min;
  ELSIF result > Max THEN
    result:=Max;
  END;

  (*---save data and close---*)
  SELF.z1:=z1;
  RETURN result;
END Engine;

(*==========================*)
BEGIN
END RandomBayesDurham.
