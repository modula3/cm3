GENERIC MODULE VectorBasic(R);
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
FROM xUtils IMPORT Error,Err;

<*UNUSED*> CONST Module = "VectorBasic.";

(*-----------------*)
PROCEDURE New(  n:CARDINAL):T =
BEGIN
  RETURN NEW(T,n);
END New;
(*-----------------*)
PROCEDURE Copy(  x:T):T =
VAR
  n:=NUMBER(x^);
  z:=NEW(T,n);
BEGIN
  z^:=x^;
  RETURN z;
END Copy;


(*-----------------*)
<*INLINE*>
PROCEDURE AssertEqualSize( 
                 x,y:T) RAISES {Error}=
BEGIN
  IF NUMBER(x^) # NUMBER(y^) THEN
    RAISE Error(Err.bad_size);
  END;
END AssertEqualSize;

(*-----------------*)
PROCEDURE Add( 
                 x,y:T):T RAISES {Error}=
VAR
  z:T;
BEGIN
  AssertEqualSize(x,y);
  z:=NEW(T,NUMBER(x^));
  FOR i:=FIRST(x^) TO LAST(x^) DO
    z[i]:=R.Add(x[i],y[i]);
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
    z[i]:=R.Sub(x[i],y[i]);
  END;
  RETURN z;
END Sub;

(*---------------------*)
PROCEDURE Equal(x,y:T):BOOLEAN RAISES {Error} =
BEGIN
  AssertEqualSize(x,y);
  FOR i:=FIRST(x^) TO LAST(y^) DO
    IF NOT R.Equal(x[i],y[i]) THEN
      RETURN FALSE;
    END
  END;
  RETURN TRUE;
END Equal;


(*-----------------*)
PROCEDURE Scale( 
                 x:T; factor:R.T)=
BEGIN
  FOR i:=FIRST(x^) TO LAST(x^) DO
    x[i]:=R.Mul(x[i],factor);
  END;
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
    sum:=R.Add(sum,R.Mul(R.Conj(x[i]),y[i]));
  END;
  RETURN sum;
END Inner;

(*-----------------*)
(*
PROCEDURE Cross( 
                x,y:T):T RAISES {Error}=
(*return cross product of x and y*)
BEGIN
  RAISE Error(Err.not_implemented);
END Cross;
*)

(*-----------------*)
BEGIN
END VectorBasic.
