GENERIC MODULE VectorTrans(R,RT,CT);
(*
Abstract:

*)

<*UNUSED*> CONST Module = "VectorTrans.";

(*-----------------*)
PROCEDURE Norm1(
                x:T):R.T=
VAR
  sum:R.T;
BEGIN
  sum:=R.Zero;
  FOR i:=FIRST(x^) TO LAST(x^) DO
    sum:=R.Add(sum,CT.Abs(x[i]));
  END;
  RETURN sum;
END Norm1;

(*-----------------*)
PROCEDURE Norm2(
                x:T):R.T=
VAR
  sum:R.T;
BEGIN
  sum:=R.Zero;
  FOR i:=FIRST(x^) TO LAST(x^) DO
    sum:=R.Add(sum,CT.AbsSqr(x[i]));
  END;
  RETURN RT.SqRt(sum);
END Norm2;

(*-----------------*)
PROCEDURE NormInf(
                x:T):R.T=
VAR
  max,abs:R.T;
BEGIN
  max:=R.Zero;
  FOR i:=FIRST(x^) TO LAST(x^) DO
    abs:=CT.Abs(x[i]);
    IF R.Compare(max,abs)<0 THEN
      max:=abs;
    END;
  END;
  RETURN max;
END NormInf;


(*-----------------*)
BEGIN
END VectorTrans.
