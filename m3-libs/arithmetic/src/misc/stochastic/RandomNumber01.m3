MODULE RandomNumber01;
(*Copyright (c) 1996, m3na project

Abstract: <describe>

1/1/96  <name>    Initial version
*)
IMPORT xReal64 AS R;
FROM xReal64 IMPORT REAL64;
IMPORT Random;

FROM RandomBasic IMPORT RandomGen,Min,Max;
IMPORT RandomRep;
FROM RandomRep IMPORT TableSize;

CONST Module = "RandomNumber01.";
(*==========================*)
(*---------------------*)
REVEAL DECSRC = RandomGen BRANDED OBJECT
    rand:Random.T;
  OVERRIDES
    init:=DECSRC_init;
    engine:=DECSRC_engine;
  END;

PROCEDURE DECSRC_init(self:RandomGen;
                   <*UNUSED*> seed:[FIRST(INTEGER)..-1]:=-1
                       ):RandomGen =
VAR
  t:=NARROW(self,DECSRC);
BEGIN
  t.rand:=NEW(Random.Default).init();
  RETURN t;
END DECSRC_init;

PROCEDURE DECSRC_engine(self:RandomGen):REAL64=
CONST ftn = Module & "DECSRC";
VAR
  t:=NARROW(self,DECSRC);
  result:REAL64;
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
REVEAL ran0 = RandomGen BRANDED OBJECT
  OVERRIDES
    init:=ran0_init;
    engine:=ran0_engine;
  END;

PROCEDURE ran0_init(self:RandomGen;
                    seed:[FIRST(INTEGER)..-1]:=-1
                       ):RandomGen =
VAR
  t:=NARROW(self,ran0);
BEGIN
  t.z1:=-seed;
  t.start:=TRUE;
  EVAL t.engine();
  RETURN t;
END ran0_init;

PROCEDURE ran0_engine(self:ran0):REAL64=
CONST
  ftn= Module & "ran0_engine";
  (*use Park/Miller alternatives (NR92, pg279)*)
  m = 2147483647;
  m_recip = R.One / FLOAT(m,R.T);
  a = 48271;
  q = 44488;
  r = 3399;

VAR
  z1,tmp:INTEGER;
  result:REAL64;
BEGIN
  z1:=self.z1;
  tmp:=z1 DIV q;
  z1:=a*(z1-tmp*q)-r*tmp;
  IF z1 < 0 THEN INC(z1,m) END;

  (*---convert and check for out of bounds---*)
  result:= FLOAT(z1,REAL64) * m_recip;
  IF result < Min THEN
    result:=Min;
  ELSIF result > Max THEN
    result:=Max;
  END;

  (*---save data and close---*)
  self.z1:=z1;
  RETURN result;
END ran0_engine;


(*-----------------------------------*)
(*------------------*)
REVEAL ran1 = RandomGen BRANDED OBJECT
  OVERRIDES
    init:=ran1_init;
    engine:=ran1_engine;
  END;

PROCEDURE ran1_init(self:RandomGen;
                    seed:[FIRST(INTEGER)..-1]:=-1
                       ):RandomGen =
VAR
  t:=NARROW(self,ran1);
BEGIN
  t.z1:=-seed;
  t.start:=TRUE;
  EVAL t.engine();
  RETURN t;
END ran1_init;

PROCEDURE ran1_engine(self:ran1):REAL64=
CONST
  ftn= Module & "ran1_engine";
  m = 2147483647;
  m_recip = R.One / FLOAT(m,R.T);
  a = 48271;
  q = 44488;
  r  = 3399;

VAR
  z1,table_z,tmp,ndx:INTEGER;
  result:REAL64;
BEGIN
  IF self.start THEN
      z1:=self.z1;
      FOR i:=FIRST(self.table) TO LAST (self.table) DO
        tmp:=z1 DIV q;
        z1:=a*(z1-tmp*q)-r*tmp;
        IF z1 < 0 THEN INC(z1,m) END;
        self.table[i]:=z1;
      END;
      self.z1:=z1;
      self.table_z:=self.table[2];
      self.start:=FALSE;
      RETURN R.Zero;
  END;

  (*---get the raw result---*)
  z1:=self.z1;
  tmp:=z1 DIV q;
  z1:=a*(z1-tmp*q)-r*tmp;
  IF z1 < 0 THEN INC(z1,m) END;

  (*---do the shuffle---*)
  table_z:=self.table_z;
  ndx:=table_z - (table_z DIV TableSize) * TableSize;
  table_z:=self.table[ndx];
  self.table[ndx]:=z1;

  (*---convert and check for out of bounds---*)
  result:= FLOAT(table_z,REAL64) * m_recip;
  IF result < Min THEN
    result:=Min;
  ELSIF result > Max THEN
    result:=Max;
  END;

  (*---save data and close---*)
  self.z1:=z1;
  self.table_z:=table_z;
  RETURN result;
END ran1_engine;


(*==========================*)
BEGIN
END RandomNumber01.
