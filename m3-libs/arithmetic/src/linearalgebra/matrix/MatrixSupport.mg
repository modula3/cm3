GENERIC MODULE MatrixSupport(R);
(* Arithmetic for Modula-3, see doc for details *)

<* INLINE *>
PROCEDURE AssertEqualWidth (n, m: CARDINAL; ) =
  BEGIN
    <* ASSERT n = m, "Width or height of operands don't match." *>
  END AssertEqualWidth;


PROCEDURE NewZero (m, n: CARDINAL; ): T =
  VAR z := NEW(T, m, n);
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO
      FOR j := FIRST(z[i]) TO LAST(z[i]) DO z[i, j] := R.Zero; END;
    END;
    RETURN z;
  END NewZero;

PROCEDURE NewOne (n: CARDINAL; ): T =
  VAR z := NEW(T, n, n);
  BEGIN
    FOR i := FIRST(z^) TO LAST(z^) DO
      z[i, i] := R.One;
      FOR j := FIRST(z[i]) TO i - 1 DO
        z[i, j] := R.Zero;
        z[j, i] := R.Zero;
      END;
    END;
    RETURN z;
  END NewOne;



PROCEDURE Transpose (x: T; ): T =
  VAR z := NEW(T, NUMBER(x[0]), NUMBER(x^));
  BEGIN
    FOR i := FIRST(x[0]) TO LAST(x[0]) DO
      FOR j := FIRST(x^) TO LAST(x^) DO z[i, j] := x[j, i]; END;
    END;
    RETURN z;
  END Transpose;

(* Given the matrix of all weights of clows of length l compute the weight
   matrix for all clows of length (l+1).  Take the result of 'newClow' as
   diagonal and the result of 'extendClow' as lower triangle of the weight
   matrix.

   Note that only the lower triangle of 'cl0' is used.  Is is somewhat
   waste of space, but saving space would lead to more complicated (time
   inefficient) array handling *)
PROCEDURE LongerClow (x: T; cl0: T; ): T =
  VAR
    cl1 := NEW(T, NUMBER(x^), NUMBER(x[0]));
    sum := R.Zero;
  BEGIN
    (* Compute the weights of all clow sequences where the last clow is
       closed and a new one is started. *)
    cl1[0, 0] := R.Zero;
    FOR i := FIRST(x^) TO LAST(x^) - 1 DO
      FOR j := i TO LAST(x^) DO
        sum := R.Sub(sum, R.Mul(x[i, j], cl0[j, i]));
      END;
      cl1[i + 1, i + 1] := sum;
    END;

    (* Compute the weights of all clow sequences where the last (open) clow
       is extended by a new arc.  This is essentially a multiplication of
       the matrix 'x' with the lower triangular matrix 'cl0' where the
       result is restricted to a lower triangular matrix without the
       diagonal. *)
    FOR i := FIRST(x^) + 1 TO LAST(x^) DO
      FOR j := 0 TO i - 1 DO
        sum := R.Zero;
        FOR k := j TO LAST(x^) DO
          sum := R.Add(sum, R.Mul(x[i, k], cl0[k, j]));
        END;
        cl1[i, j] := sum;
      END;
    END;

    RETURN cl1;
  END LongerClow;

(* Compute the determinant with about n^4 multiplications without division
   according to the clow decomposition algorithm presented by Günter Rote:
   "Division-Free Algorithms for the Determinant and the Pfaffian:
   Algebraic and Combinatorial Approaches." *)
PROCEDURE Determinant (x: T; ): R.T =
  VAR
    y   := NewOne(NUMBER(x^));
    sum := R.Zero;

  BEGIN
    AssertEqualWidth(NUMBER(x^), NUMBER(x[0]));

    FOR i := 1 TO LAST(x^) DO y := LongerClow(x, y); END;

    (* This is equal to the first part of LongerClow. *)
    FOR i := FIRST(x^) TO LAST(x^) DO
      FOR j := i TO LAST(x^) DO
        sum := R.Sub(sum, R.Mul(x[i, j], y[j, i]));
      END;
    END;
    IF NUMBER(x^) MOD 2 = 0 THEN RETURN sum; ELSE RETURN R.Neg(sum); END;
  END Determinant;

BEGIN
END MatrixSupport.
