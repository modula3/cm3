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
PROCEDURE Copy(  v:T):T =
VAR
  n:=NUMBER(v^);
  tmp:=NEW(T,n);
BEGIN
  tmp^:=v^;
  RETURN tmp;
END Copy;

(*-----------------*)
(*
PROCEDURE Zero( 
                v:T)=
(*set all zeros*)
BEGIN
  FOR i:=FIRST(v^) TO LAST(v^) DO
    v[i]:=R.Zero;
  END;
END Zero;
*)


(*-----------------*)
<*INLINE*>
PROCEDURE AssertEqualSize( 
                 v1,v2:T) RAISES {Error}=
BEGIN
  IF NUMBER(v1^) # NUMBER(v2^) THEN
    RAISE Error(Err.bad_size);
  END;
END AssertEqualSize;

(*-----------------*)
PROCEDURE Add( 
                 v1,v2:T):T RAISES {Error}=
(*v1:=v1+v2*)
VAR
  tmp:T;
BEGIN
  AssertEqualSize(v1,v2);
  tmp:=NEW(T,NUMBER(v1^));
  FOR i:=FIRST(v1^) TO LAST(v1^) DO
    tmp[i]:=v1[i]+v2[i];
  END;
  RETURN tmp;
END Add;

(*-----------------*)
PROCEDURE Sub( 
               v1,v2:T):T RAISES {Error}=
(*v1:=v1-v2*)
VAR
  tmp:T;
BEGIN
  AssertEqualSize(v1,v2);
  tmp:=NEW(T,NUMBER(v1^));
  FOR i:=FIRST(v1^) TO LAST(v1^) DO
    tmp[i]:=v1[i]-v2[i];
  END;
  RETURN tmp;
END Sub;

(*---------------------*)
PROCEDURE Equal(v1,v2:T):BOOLEAN RAISES {Error} =
BEGIN
  AssertEqualSize(v1,v2);
  FOR i:=FIRST(v1^) TO LAST(v2^) DO
    IF v1[i] # v2[i] THEN
      RETURN FALSE;
    END
  END;
  RETURN TRUE;
END Equal;


(*-----------------*)
PROCEDURE Scale( 
                 v:T; factor:R.T)=
(*Scale v by factor*)
BEGIN
  FOR i:=FIRST(v^) TO LAST(v^) DO
    v[i]:=v[i]*factor;
  END;
END Scale;


(*-----------------*)
PROCEDURE Inner( 
                v1,v2:T):R.T RAISES {Error}=
(*return Dot product of v1 and v2*)
VAR
  sum:R.T;
BEGIN
  AssertEqualSize(v1,v2);
  sum:=R.Zero;
  FOR i:=FIRST(v1^) TO LAST(v1^) DO
    sum:=sum+v1[i]*v2[i];
  END;
  RETURN sum;
END Inner;

(*-----------------*)
(*
PROCEDURE Cross( 
                v1,v2:T):T RAISES {Error}=
(*return cross product of v1 and v2*)
BEGIN
  RAISE Error(Err.not_implemented);
END Cross;
*)

(*-----------------*)
BEGIN
END VectorFast.
