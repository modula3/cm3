MODULE BigIntegerBasic;
(*Copyright (c) 1996, m3na project

Abstract: Integers of arbitrary size

Daniel Beer
*)

FROM xUtils IMPORT Error, Err;
IMPORT BigIntegerRep AS Rep;

<*UNUSED*> CONST Module = "BigIntegerBasic.";
(*==========================*)

<*UNUSED*>
PROCEDURE FastCopy (READONLY x : T) : T =
VAR
  y := T{NEW(Value,NUMBER(x.data^)),x.size,x.sign};

BEGIN
  y.data^ := x.data^;
  RETURN y;
END FastCopy;

PROCEDURE Copy (READONLY x : T) : T =
VAR
  y := T{NEW(Value,x.size),x.size,x.sign};

BEGIN
  y.data^ := SUBARRAY(x.data^,0,NUMBER(y.data^));
  RETURN y;
END Copy;


(*
PROCEDURE SetZero (VAR r : LIST OF T) =
VAR j : INTEGER;
BEGIN
  FOR j:=0 TO r'MAX DO
    r[j].size := 0;
    r[j].sign := FALSE;
  END;
END SetZero;

PROCEDURE SetOne (VAR r : LIST OF T) =
VAR j : INTEGER;
BEGIN
  FOR j:=0 TO r'MAX DO
    IF r[j].data=NIL THEN
      r[j].data'RANGE := 1;
      memPool.NewPooled (r[j].data);
    END;

    r[j].data[0] := 1;
    r[j].size    := 1;
    r[j].sign    := FALSE;
  END;
END SetOne;
*)

PROCEDURE FromInteger (x : INTEGER) : T =
VAR
  data : Value;
BEGIN
  IF x=0 THEN
    (*we cannot return Zero because we use FromInteger to initialize Zero*)
    RETURN T{NIL,0,FALSE};
  ELSE
    data := NEW(Value,1);
    data[0] := ABS(x);
    RETURN T{data,1,x<0};
  END;
END FromInteger;



(*signed arithmetic*)

PROCEDURE Add (READONLY x, y : T) : T =
VAR
  z : T;

BEGIN
  IF x.sign = y.sign THEN
    z := Rep.AddU (x, y);
    z.sign := x.sign;
  ELSE
    CASE Rep.CompareU (x, y) OF
    |  1 =>
      z := Rep.SubU (x, y);
      z.sign := x.sign;
    | -1 =>
      z := Rep.SubU (y, x);
      z.sign := y.sign;
    |  0 =>
      z := Zero;
    END;
  END;
  RETURN z;
END Add;

PROCEDURE Sub (READONLY x, y : T) : T =
VAR
  z : T;

BEGIN
  IF x.sign # y.sign THEN
    z := Rep.AddU (x, y);
    z.sign := x.sign;
  ELSE
    CASE Rep.CompareU (x, y) OF
    | 1 =>
      z := Rep.SubU (x, y);
      z.sign := x.sign;
    | -1 =>
      z := Rep.SubU (y, x);
      z.sign := NOT x.sign;
    | 0 =>
      z := Zero;
    END;
  END;
  RETURN z;
END Sub;

PROCEDURE Neg (READONLY x : T) : T =
VAR
  y := x;
BEGIN
  y.sign := NOT x.sign;
  RETURN y;
END Neg;

PROCEDURE Conj (READONLY x : T) : T =
BEGIN
  RETURN x;
END Conj;


PROCEDURE IsZero (READONLY x : T) : BOOLEAN =
BEGIN
  RETURN x.size=0;
  (*RETURN x.size=0 OR x.size=1 AND x.data[0]=0;*)
END IsZero;

PROCEDURE Compare (READONLY x, y : T) : [-1..1] =
BEGIN
  IF x.sign # y.sign THEN
    IF x.sign THEN
      RETURN -1
    ELSE
      RETURN 1
    END
  ELSE
    IF x.sign THEN
      RETURN Rep.CompareU (y, x)
    ELSE
      RETURN Rep.CompareU (x, y)
    END
  END;
END Compare;

PROCEDURE Equal   (READONLY x, y : T) : BOOLEAN =
BEGIN
  IF x.sign # y.sign OR x.size # y.size THEN
    RETURN FALSE
  ELSE
    FOR j:=x.size-1 TO 0 BY -1 DO
      IF x.data[j] # y.data[j] THEN
        RETURN FALSE
      END;
    END;
    RETURN TRUE;
  END;
END Equal;




PROCEDURE Mul (READONLY x, y : T) : T =
VAR
  z : T;

BEGIN
  z := Rep.MulU(x,y);
  z.sign := x.sign # y.sign;
  RETURN z;
END Mul;



PROCEDURE Div (READONLY x, y : T) : T RAISES {Error} =
VAR
  q, r : T;

BEGIN
  q := Rep.DivModU (x, y, r);
  q.sign := x.sign#y.sign;
  (*IF NOT Equal(r,Zero) THEN*)
  IF r.size # 0 THEN
    RAISE Error(Err.indivisible);
  END;
  RETURN q;
END Div;

PROCEDURE DivMod (READONLY x, y : T; VAR r : T) : T RAISES {Error} =
VAR
  q : T;

BEGIN
  q := Rep.DivModU (x, y, r);
  r.sign := y.sign;
  q.sign := x.sign#y.sign;
  IF q.sign THEN (*means x.sign#y.sign*)
    r := Rep.SubU (y, r);
  END;
  RETURN q;
END DivMod;

PROCEDURE Mod (READONLY x, y : T) : T RAISES {Error} =
VAR
  r : T;

BEGIN
  EVAL DivMod (x, y, r);
  RETURN r;
END Mod;

(*==========================*)
BEGIN
  Zero     := FromInteger(0);
  One      := FromInteger(1);
  MinusOne := FromInteger(-1);

(*  billion := FromInteger (1000000000);  *)
END BigIntegerBasic.
