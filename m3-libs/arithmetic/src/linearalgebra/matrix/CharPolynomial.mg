GENERIC MODULE CharPolynomial(R, Rt, M);
(* Arithmetic for Modula-3, see doc for details *)

IMPORT Arithmetic AS Arith;

<* UNUSED *>
CONST
  Module = "CharPolynomial.";

(*it is trace(x^n) = lambda[1]^n+...+lambda[n]^n thus we get sequence of
   power sums if we compute the trace of successive powers of x*)
PROCEDURE CharPolynomial (x: M.T): Rt.T =
  BEGIN
    <* ASSERT NUMBER(x^) = NUMBER(x[0]), "Matrix must be square form!" *>
    VAR
      p   := NEW(REF Rt.PowerSumSeq, NUMBER(x^));
      pow := x;
    <* FATAL Arith.Error *>      (*Rt.FromPowerSumSeq can't fail, all
                                    divisions can be performed because if
                                    the polynomial coefficients are in the
                                    same ring as the matrix coefficients.*)
    BEGIN
      p[0] := M.Trace(pow);
      FOR j := 1 TO LAST(p^) DO
        pow := M.Mul(pow, x);
        p[j] := M.Trace(pow);
      END;
      RETURN Rt.FromPowerSumSeq(p^);
    END;
  END CharPolynomial;

PROCEDURE CompanionMatrix (x: Rt.T): M.T =
  BEGIN
    <* ASSERT R.Equal(x[LAST(x^)], R.One),
                "The leading coefficient of the polynomial must be one." *>
    WITH y = M.New(LAST(x^), LAST(x^)) DO
      FOR j := 0 TO LAST(y^) - 1 DO y[j, j + 1] := R.One; END;
      FOR j := 0 TO LAST(y[0]) DO y[LAST(y^), j] := R.Neg(x[j]); END;
      RETURN y;
    END;
  END CompanionMatrix;

BEGIN
END CharPolynomial.
