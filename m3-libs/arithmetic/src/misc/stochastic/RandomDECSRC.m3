MODULE RandomDECSRC;
(*Copyright (c) 1996, m3na project

Abstract: <describe>

1/1/96  <name>    Initial version
*)
IMPORT LongRealBasic AS R;
IMPORT Random;

FROM RandomBasic IMPORT Min,Max;
IMPORT RandomRep, RandomBasic;

CONST Module = "RandomDECSRC.";
(*==========================*)
(*---------------------*)
REVEAL T = RandomBasic.T BRANDED OBJECT
    rand:Random.T;
  OVERRIDES
    engine:=Engine;
  END;

PROCEDURE New():T =
VAR
  t:=NEW(T);
BEGIN
  t.rand:=NEW(Random.Default).init();
  RETURN t;
END New;

PROCEDURE Engine(SELF:RandomBasic.T):R.T=
<*UNUSED*> CONST ftn = Module & "DECSRC";
VAR
  t:=NARROW(SELF,T);
  result:R.T;
BEGIN
  result:=t.rand.longreal();
  IF result < Min THEN
    result:=Min;
  ELSIF result > Max THEN
    result:=Max;
  END;
  RETURN result;
END Engine;

(*==========================*)
BEGIN
END RandomDECSRC.
