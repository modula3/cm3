GENERIC MODULE VectorTrans(Ct,R,Rt);
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

<*UNUSED*> CONST Module = "VectorTrans.";

(*-----------------*)
PROCEDURE Norm1( 
                v:T):R.T=
VAR
  sum:R.T;
BEGIN
  sum:=R.Zero;
  FOR i:=FIRST(v^) TO LAST(v^) DO
    sum:=R.Add(sum,Ct.Abs(v[i]));
  END;
  RETURN sum;
END Norm1;

(*-----------------*)
PROCEDURE Norm2( 
                v:T):R.T=
VAR
  sum:R.T;
BEGIN
  sum:=R.Zero;
  FOR i:=FIRST(v^) TO LAST(v^) DO
    sum:=R.Add(sum,Ct.AbsSqr(v[i]));
  END;
  RETURN Rt.SqRt(sum);
END Norm2;

(*-----------------*)
PROCEDURE NormInf( 
                v:T):R.T=
VAR
  max,abs:R.T;
BEGIN
  max:=R.Zero;
  FOR i:=FIRST(v^) TO LAST(v^) DO
    abs:=Ct.Abs(v[i]);
    IF R.Compare(max,abs)<0 THEN
      max:=abs;
    END;
  END;
  RETURN max;
END NormInf;


(*-----------------*)
BEGIN
END VectorTrans.
