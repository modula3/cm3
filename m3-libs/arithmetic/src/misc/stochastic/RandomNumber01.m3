MODULE RandomNumber01;
(*Copyright (c) 1996, m3na project

Abstract: <describe>

1/1/96  <name>    Initial version
*)
IMPORT LongRealBasic AS R;
IMPORT Random;

FROM RandomBasic IMPORT T,Min,Max;
IMPORT RandomRep;
FROM RandomRep IMPORT TableSize;

CONST Module = "RandomNumber01.";
(*==========================*)
(*---------------------*)
REVEAL DECSRC = T BRANDED OBJECT
    rand:Random.T;
  OVERRIDES
    init:=DECSRC_init;
    engine:=DECSRC_engine;
  END;

PROCEDURE DECSRC_init(SELF:T;
                   <*UNUSED*> seed:[FIRST(INTEGER)..-1]:=-1
                       ):T =
VAR
  t:=NARROW(SELF,DECSRC);
BEGIN
  t.rand:=NEW(Random.Default).init();
  RETURN t;
END DECSRC_init;

PROCEDURE DECSRC_engine(SELF:T):R.T=
<*UNUSED*> CONST ftn = Module & "DECSRC";
VAR
  t:=NARROW(SELF,DECSRC);
  result:R.T;
BEGIN
  result:=t.rand.longreal();
  IF result < Min THEN
    result:=Min;
  ELSIF result > Max THEN
    result:=Max;
  END;
  RETURN result;
END DECSRC_engine;

(*------------------*)
REVEAL ran0 = T BRANDED OBJECT
  OVERRIDES
    init:=ran0_init;
    engine:=ran0_engine;
  END;

PROCEDURE ran0_init(SELF:T;
                    seed:[FIRST(INTEGER)..-1]:=-1
                       ):T =
VAR
  t:=NARROW(SELF,ran0);
BEGIN
  t.z1:=-seed;
  t.start:=TRUE;
  EVAL t.engine();
  RETURN t;
END ran0_init;

PROCEDURE ran0_engine(SELF:ran0):R.T=
<*UNUSED*> CONST ftn= Module & "ran0_engine";
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
END ran0_engine;


(*-----------------------------------*)
(*------------------*)
REVEAL ran1 = T BRANDED OBJECT
  OVERRIDES
    init:=ran1_init;
    engine:=ran1_engine;
  END;

PROCEDURE ran1_init(SELF:T;
                    seed:[FIRST(INTEGER)..-1]:=-1
                       ):T =
VAR
  t:=NARROW(SELF,ran1);
BEGIN
  t.z1:=-seed;
  t.start:=TRUE;
  EVAL t.engine();
  RETURN t;
END ran1_init;

PROCEDURE ran1_engine(SELF:ran1):R.T=
<*UNUSED*> CONST ftn= Module & "ran1_engine";
CONST
  m = 2147483647;
  m_recip = R.One / FLOAT(m,R.T);
  a = 48271;
  q = 44488;
  r  = 3399;

VAR
  z1,table_z,tmp,ndx:INTEGER;
  result:R.T;
BEGIN
  IF SELF.start THEN
      z1:=SELF.z1;
      FOR i:=FIRST(SELF.table) TO LAST (SELF.table) DO
        tmp:=z1 DIV q;
        z1:=a*(z1-tmp*q)-r*tmp;
        IF z1 < 0 THEN INC(z1,m) END;
        SELF.table[i]:=z1;
      END;
      SELF.z1:=z1;
      SELF.table_z:=SELF.table[2];
      SELF.start:=FALSE;
      RETURN R.Zero;
  END;

  (*---get the raw result---*)
  z1:=SELF.z1;
  tmp:=z1 DIV q;
  z1:=a*(z1-tmp*q)-r*tmp;
  IF z1 < 0 THEN INC(z1,m) END;

  (*---do the shuffle---*)
  table_z:=SELF.table_z;
  ndx:=table_z - (table_z DIV TableSize) * TableSize;
  table_z:=SELF.table[ndx];
  SELF.table[ndx]:=z1;

  (*---convert and check for out of bounds---*)
  result:= FLOAT(table_z,R.T) * m_recip;
  IF result < Min THEN
    result:=Min;
  ELSIF result > Max THEN
    result:=Max;
  END;

  (*---save data and close---*)
  SELF.z1:=z1;
  SELF.table_z:=table_z;
  RETURN result;
END ran1_engine;


(*==========================*)
BEGIN
END RandomNumber01.
