GENERIC MODULE MatrixTrans(M,Eigen,CT,R,RT);
(*
Abstract:

*)

FROM NADefinitions IMPORT Error;

<*UNUSED*> CONST Module = "MatrixTrans.";

(*-----------------*)
PROCEDURE Norm1(
                x:T):R.T=
VAR
  max,sum:R.T;
BEGIN
  max:=R.Zero;
  FOR j:=FIRST(x[0]) TO LAST(x[0]) DO
    sum:=R.Zero;
    FOR i:=FIRST(x^) TO LAST(x^) DO
      sum:=R.Add(sum,CT.Abs(x[i,j]));
    END;
    max:=MAX(max,sum);
  END;
  RETURN max;
END Norm1;

(*-----------------*)
PROCEDURE Norm2(
                x:T):R.T RAISES {Error}=
VAR
  v:REF M.TRow;
  specrad:=Eigen.SquareMethod(M.Mul(M.Adjungate(x),x),v);
BEGIN
  RETURN RT.SqRt(specrad);
END Norm2;

(*-----------------*)
PROCEDURE NormInf(
                x:T):R.T=
VAR
  max,sum:R.T;
BEGIN
  max:=R.Zero;
  FOR i:=FIRST(x^) TO LAST(x^) DO
    sum:=R.Zero;
    FOR j:=FIRST(x[0]) TO LAST(x[0]) DO
      sum:=R.Add(sum,CT.Abs(x[i,j]));
    END;
    max:=MAX(max,sum);
  END;
  RETURN max;
END NormInf;


(*-----------------*)
BEGIN
END MatrixTrans.
