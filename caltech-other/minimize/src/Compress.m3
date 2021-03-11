(* $Id$ *)

MODULE Compress;
IMPORT Bracket;
IMPORT LRScalarField;
IMPORT LRVector;
IMPORT LRFunction;

CONST Tol = 2.0d-8;

(* mutex and therewith protected globals *)
VAR mu := NEW(MUTEX);
VAR pcom, xicom : LRVector.T; 
VAR nrfunc : LRScalarField.T;

PROCEDURE F1Dim(x : LONGREAL) : LONGREAL =
  VAR
    xt := NEW(LRVector.T, NUMBER(pcom^));
  BEGIN
    FOR j := FIRST(pcom^) TO LAST(pcom^) DO xt[j] := pcom[j] + x*xicom[j] END;
    RETURN nrfunc.eval(xt);
  END F1Dim;

PROCEDURE LinMin(VAR p : LRVector.T; (* initial and final point *)
                 VAR xi : LRVector.T; (* search direction, 
                                            replaced with change in p *)
                 func : LRScalarField.T) : LONGREAL (* returns min. value *) =
  VAR
    xmin : LONGREAL;
    bracket := Bracket.Trio { 0.0d0, 1.0d0, 2.0d0 };
    fret : LONGREAL;
  BEGIN
    LOCK mu DO
      pcom := NEW(LRVector.T, NUMBER(p^));
      xicom := NEW(LRVector.T, NUMBER(p^));
      nrfunc := func;

      <* ASSERT NUMBER(p^) = NUMBER(xi^) AND FIRST(p^) = 0 AND FIRST(xi^) = 0 *>
      FOR j := FIRST(p^) TO LAST(p^) DO
        pcom[j] := p[j]; xicom[j] := xi[j]
      END;
      EVAL Bracket.Initial(bracket, NEW(LRFunction.Default).wrap(F1Dim));
      fret := Bracket.Brent(bracket, NEW(LRFunction.Default).wrap(F1Dim), Tol, xmin);
      FOR j := FIRST(p^) TO LAST(p^) DO
        xi[j] := xi[j] * xmin;
        p[j] := p[j] + xi[j];
      END
    END; (* LOCK mu ... *)
    RETURN fret
  END LinMin;

BEGIN END Compress.
