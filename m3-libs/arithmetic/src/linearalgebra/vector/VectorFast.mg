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
FROM xUtils IMPORT Error,Err;

<*UNUSED*> CONST Module = "VectorFast.";

(*-----------------*)
PROCEDURE New(  n:CARDINAL):T =
BEGIN
  RETURN NEW(T,n);
END New;
(*-----------------*)
PROCEDURE Copy(  x:T):T =
VAR
  n:=NUMBER(x^);
  y:=NEW(T,n);
BEGIN
  y^:=x^;
  RETURN y;
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
PROCEDURE Equal(x,y:T):BOOLEAN RAISES {Error} =
BEGIN
  AssertEqualSize(x,y);
  FOR i:=FIRST(x^) TO LAST(y^) DO
    IF x[i] # y[i] THEN
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
    x[i]:=x[i]*factor;
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

(*-----------------*)
BEGIN
END VectorFast.
