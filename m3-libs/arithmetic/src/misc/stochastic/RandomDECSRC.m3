MODULE RandomDECSRC;
(*Copyright (c) 1996, m3na project

Abstract: <describe>

1/1/96  <name>    Initial version
*)
IMPORT LongRealBasic AS R,
       Word AS W;
IMPORT Random;

FROM RandomBasic IMPORT Min,Max;
IMPORT RandomRep, RandomBasic;

CONST Module = "RandomDECSRC.";
(*==========================*)
(*---------------------*)
REVEAL T = RandomBasic.T BRANDED OBJECT
    rand:Random.T;
  OVERRIDES
    generateBoolean:=GenerateBoolean;
    generateWord:=GenerateWord;
    generateReal:=GenerateReal;
  END;

PROCEDURE New():T =
VAR
  t:=NEW(T);
BEGIN
  t.rand:=NEW(Random.Default).init();
  RETURN t;
END New;

PROCEDURE GenerateBoolean(SELF:T):BOOLEAN=
BEGIN
  RETURN SELF.rand.boolean();
END GenerateBoolean;

PROCEDURE GenerateWord(SELF:T):W.T=
BEGIN
  RETURN VAL(SELF.rand.integer(),W.T);
END GenerateWord;

PROCEDURE GenerateReal(SELF:T):R.T=
BEGIN
  RETURN MIN(Max,MAX(Min,SELF.rand.longreal()));
END GenerateReal;

(*==========================*)
BEGIN
END RandomDECSRC.
