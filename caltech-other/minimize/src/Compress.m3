(* $Id: Compress.m3,v 1.3 2001/10/10 07:39:55 mika Exp $ *)

MODULE Compress;
IMPORT Bracket;
IMPORT LRScalarField;
IMPORT LRVector;
IMPORT LRFunction;

TYPE
  Func = LRFunction.Default OBJECT
    pcom, xicom : LRVector.T;
    nrfunc      : LRScalarField.T;
  OVERRIDES
    eval     := EvalF1;
    evalHint := EvalHintF1;
  END;

PROCEDURE EvalF1(f : Func; x : LONGREAL) : LONGREAL =
  VAR
    xt := NEW(LRVector.T, NUMBER(f.pcom^));
  BEGIN
    FOR j := FIRST(f.pcom^) TO LAST(f.pcom^) DO
      xt[j] := f.pcom[j] + x*f.xicom[j]
    END;
    RETURN f.nrfunc.eval(xt);
  END EvalF1;
  
PROCEDURE EvalHintF1(f : Func; x : LONGREAL) =
  VAR
    xt := NEW(LRVector.T, NUMBER(f.pcom^));
  BEGIN
    FOR j := FIRST(f.pcom^) TO LAST(f.pcom^) DO
      xt[j] := f.pcom[j] + x*f.xicom[j]
    END;
    f.nrfunc.evalHint(xt);
  END EvalHintF1;
  
PROCEDURE LinMin(p     : LRVector.T; (* initial and final point *)
                 xi    : LRVector.T; (* search direction, 
                                     replaced with change in p *)
                 func  : LRScalarField.T;
                 scale : LONGREAL;
                 tol   : LONGREAL) : LONGREAL (* returns min. value *) =

    
  VAR
    xmin : LONGREAL;
    bracket := Bracket.Trio { -scale, 0.0d0, +scale };
    fret : LONGREAL;
    pcom, xicom : LRVector.T;
    f : Func;

  BEGIN
    <* ASSERT NUMBER(p^) = NUMBER(xi^) AND FIRST(p^) = 0 AND FIRST(xi^) = 0 *>

    pcom  := LRVector.Copy(p);
    xicom := LRVector.Copy(xi);

    f := NEW(Func, pcom := pcom, xicom := xicom, nrfunc := func);
    
    EVAL Bracket.Initial(bracket, f);
    
    fret := Bracket.Brent(bracket, f, tol, xmin);
    
    FOR j := FIRST(p^) TO LAST(p^) DO
      xi[j] := xi[j] * xmin;
      p[j]  := p[j] + xi[j];
    END;
    RETURN fret
  END LinMin;

BEGIN END Compress.
