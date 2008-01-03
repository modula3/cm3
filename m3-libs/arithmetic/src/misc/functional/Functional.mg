GENERIC MODULE Functional(R, RT, V, VT, M, FD, LA);
(* Arithmetic for Modula-3, see doc for details *)

IMPORT Arithmetic AS Arith;

<* UNUSED *>
CONST
  Module = "Functional.";


PROCEDURE EvalCentralDiff2 (f: Func; x, dx: V.T; ): FD.T
  RAISES {Arith.Error} =
  VAR
    der := FD.T{
             f(x), NEW(V.T, NUMBER(x^)), NEW(M.T, NUMBER(x^), NUMBER(x^))};
    xn := V.Copy(x);             (* we will modify this copy - this can
                                    cause unexpected results if the
                                    function f stores the pointer to x1 *)
    dxi: R.T;
  BEGIN
    FOR i := FIRST(x^) TO LAST(x^) DO
      VAR y0, y1: R.T;
      BEGIN
        dxi := dx[i];
        xn[i] := x[i] - dxi;
        y0 := f(xn);
        xn[i] := x[i] + dxi;
        y1 := f(xn);
        der.first[i] := (y1 - y0) / (dxi * R.Two);
        der.second[i, i] := (y1 + y0 - R.Two * der.zeroth) / (dxi * dxi);
      END;
      FOR j := i + 1 TO LAST(x^) DO
        VAR
          y00, y01, y10, y11, y: R.T;
          dxj                        := dx[j];
        BEGIN
          xn[i] := x[i] - dxi;
          xn[j] := x[j] - dxj;
          y00 := f(xn);
          xn[j] := x[j] + dxj;
          y01 := f(xn);
          xn[i] := x[i] + dxi;
          y11 := f(xn);
          xn[j] := x[j] - dxj;
          y10 := f(xn);
          xn[j] := x[j];
          y := (y00 - y01 - y10 + y11) / (dxi * dxj * R.Two * R.Two);
          der.second[i, j] := y;
          der.second[j, i] := y;
        END;
      END;
      xn[i] := x[i];
    END;
    RETURN der;
  END EvalCentralDiff2;

PROCEDURE FindStationaryPoint
  (f: FuncDeriv2; x: V.T; tol: R.T; maxiter: CARDINAL; ): V.T
  RAISES {Arith.Error} =
  BEGIN
    FOR iter := 0 TO maxiter - 1 DO
      VAR der := f(x);
      BEGIN
        IF VT.Norm1(der.first) <= tol * RT.Abs(der.zeroth) THEN
          RETURN x;
        END;
        x := V.Sub(x, LA.LeastSquares(der.second, ARRAY OF V.T{der.first})[
                        0].x);
      END;
    END;
    RAISE Arith.Error(NEW(Arith.ErrorNoConvergence).init());
  END FindStationaryPoint;


BEGIN
END Functional.
