GENERIC MODULE VectorFast(R);
(*
Abstract:

6/6/87    hgeorge
          Initial version.

2/11/89   hgeorge
          To work with generic matrices.

11/20/94  Harry George
          Converted to Modula3 dynamic arrays.

12/18/95  Harry George
          ...and back to fully instantiated for REAL32.

1/27/96   Harry George
          Converted to OO format, and R.T

2/17/96   Harry George   Converted from OO to ADT format
*)
FROM NADefinitions IMPORT Error,Err;

<*UNUSED*> CONST Module = "VectorFast.";

(*-----------------*)
<*INLINE*>
PROCEDURE AssertEqualSize(
                 x,y:T) RAISES {Error}=
BEGIN
  IF NUMBER(x^) # NUMBER(y^) THEN
    RAISE Error(Err.bad_size);
  END;
END AssertEqualSize;

(*---------------------*)
PROCEDURE IsZero(x:T):BOOLEAN =
BEGIN
  FOR i:=FIRST(x^) TO LAST(x^) DO
    IF x[i] # R.Zero THEN
      RETURN FALSE;
    END
  END;
  RETURN TRUE;
END IsZero;

(*---------------------*)
PROCEDURE Equal(x,y:T):BOOLEAN RAISES {Error} =
BEGIN
  AssertEqualSize(x,y);
  FOR i:=FIRST(x^) TO LAST(x^) DO
    IF x[i] # y[i] THEN
      RETURN FALSE;
    END
  END;
  RETURN TRUE;
END Equal;

(*-----------------*)
PROCEDURE Add(
                 x,y:T):T RAISES {Error}=
VAR
  z:T;
BEGIN
  AssertEqualSize(x,y);
  z:=NEW(T,NUMBER(x^));
  FOR i:=FIRST(x^) TO LAST(x^) DO
    z[i]:=x[i]+y[i];
  END;
  RETURN z;
END Add;

(*-----------------*)
PROCEDURE Sub(
               x,y:T):T RAISES {Error}=
VAR
  z:T;
BEGIN
  AssertEqualSize(x,y);
  z:=NEW(T,NUMBER(x^));
  FOR i:=FIRST(x^) TO LAST(x^) DO
    z[i]:=x[i]-y[i];
  END;
  RETURN z;
END Sub;

(*---------------------*)
PROCEDURE Neg(x:T):T =    (*return -x *)
VAR
  y:=NEW(T,NUMBER(x^));
BEGIN
  FOR i:=FIRST(x^) TO LAST(x^) DO
    y[i] := -x[i];
  END;
  RETURN y;
END Neg;


(*-----------------*)
PROCEDURE Scale(
                 x:T; y:R.T):T=
VAR
  z:=NEW(T,NUMBER(x^));
BEGIN
  FOR i:=FIRST(x^) TO LAST(x^) DO
    z[i]:=x[i]*y;
  END;
  RETURN z;
END Scale;


(*-----------------*)
PROCEDURE Inner(
                x,y:T):R.T RAISES {Error}=
VAR
  sum:R.T;
BEGIN
  AssertEqualSize(x,y);
  sum:=R.Zero;
  FOR i:=FIRST(x^) TO LAST(x^) DO
    sum:=sum+x[i]*y[i];
  END;
  RETURN sum;
END Inner;

(*-----------------*)
(*
PROCEDURE Cross(
                x,y:T):T RAISES {Error}=
BEGIN
  RAISE Error(Err.not_implemented);
END Cross;
*)

PROCEDURE Sum(READONLY x:TBody):R.T=
  VAR
    sum:=R.Zero;
  BEGIN
    FOR i:=FIRST(x) TO LAST(x) DO
      sum:=sum+x[i];
    END;
    RETURN sum;
  END Sum;

PROCEDURE Max(READONLY x:TBody):R.T=
  VAR
    max:=R.NegInf;
  BEGIN
    FOR i:=FIRST(x) TO LAST(x) DO
      max:=MAX(max,x[i]);
    END;
    RETURN max;
  END Max;

PROCEDURE Min(READONLY x:TBody):R.T=
  VAR
    min:=R.PosInf;
  BEGIN
    FOR i:=FIRST(x) TO LAST(x) DO
      min:=MIN(min,x[i]);
    END;
    RETURN min;
  END Min;


(*-----------------*)
PROCEDURE ArithSeq(num:CARDINAL;from:R.T;by:R.T):T=
VAR
  x:=NEW(T,num);
BEGIN
  FOR j:=0 TO num-1 DO
    x[j] := from;
    IF j<num-1 THEN
      from := from+by;
    END;
  END;
  RETURN x;
END ArithSeq;

(*-----------------*)
PROCEDURE GeomSeq(num:CARDINAL;from:R.T;by:R.T):T=
VAR
  x:=NEW(T,num);
BEGIN
  FOR j:=0 TO num-1 DO
    x[j] := from;
    IF j<num-1 THEN
      from := from*by;
    END;
  END;
  RETURN x;
END GeomSeq;



(*-----------------*)
BEGIN
END VectorFast.
