MODULE RandomParkMiller;
(*Arithmetic for Modula-3, see doc for details

Abstract: <describe>

1/1/96  <name>    Initial version
*)
IMPORT LongRealBasic AS R;

FROM RandomBasic IMPORT Min,Max;
IMPORT RandomRep, RandomBasic;

CONST Module = "RandomParkMiller.";
(*==========================*)

(*-----------------------------------*)
(*------------------*)
CONST
  TableSize = 32; (*for Bayes-Durham shuffle*)

REVEAL T = TPublic BRANDED OBJECT
    z1,table_z:INTEGER;
    table:ARRAY[0..TableSize-1] OF INTEGER;
  OVERRIDES
    init:=Init;
    engine:=Engine;
  END;

CONST
  m = 2147483647;
  m_recip = R.One / FLOAT(m,R.T);
  a = 48271;
  q = 44488;
  r  = 3399;

PROCEDURE Init(SELF:T;seed:[1..LAST(INTEGER)]:=1;
                 ):T =
VAR
  z1,tmp:INTEGER;
BEGIN
  SELF.z1:=seed;
  z1:=SELF.z1;
  FOR i:=FIRST(SELF.table) TO LAST (SELF.table) DO
    tmp:=z1 DIV q;
    z1:=a*(z1-tmp*q)-r*tmp;
    IF z1 < 0 THEN INC(z1,m) END;
    SELF.table[i]:=z1;
  END;
  SELF.z1:=z1;
  SELF.table_z:=SELF.table[2];
  RETURN SELF;
END Init;

PROCEDURE Engine(SELF:T):R.T=
<*UNUSED*> CONST ftn= Module & "Engine";

VAR
  z1,table_z,tmp,ndx:INTEGER;
  result:R.T;
BEGIN
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
END Engine;


(*==========================*)
BEGIN
END RandomParkMiller.
