MODULE TestMatchWavelet;

IMPORT LongRealBasic AS R;
IMPORT Integer32IntegerPower AS IIntPow;

IMPORT LongRealVectorFast AS V;

IMPORT LongRealMatrixLapack AS LA;

IMPORT LongRealSignal AS S;
IMPORT LongRealSignalIntegerPower AS SIntPow;

IMPORT LongRealRefinableFunc AS Refn;
IMPORT LongRealBSplineWavelet AS BSpl;

IMPORT LongRealSignalFmtLex AS SF;
IMPORT LongRealWaveletPlot AS WP;
IMPORT PLPlot AS PL;
IMPORT IO, Fmt, Wr, Thread;
(*IMPORT NADefinitions AS NA;*)

PROCEDURE MatchPattern (target                               : S.T;
                        levels, smooth, vanishing, translates: CARDINAL) =
  (*The degree of freedom, i.e.  the number of parameters to minimize for,
     is 2*translates*)
  VAR
    hdual := BSpl.GeneratorMask(smooth);
    gdual := BSpl.WaveletMask(smooth, vanishing);
    vancore := SIntPow.Power(NEW(S.T).fromArray(
                               ARRAY OF R.T{0.5D0, -0.5D0}), vanishing);
    phivan := Refn.Refine(vancore, hdual, levels);
    psi    := Refn.Refine(gdual, hdual, levels);

    twopow := IIntPow.Power(2, levels);
    first  := MIN(psi.getFirst(), phivan.getFirst() - twopow * translates);
    last := MAX(
              psi.getLast(), phivan.getLast() + twopow * (translates - 1));
    size := last - first + 1;

    targetvec := V.New(size);
    basis     := NEW(REF ARRAY OF V.T, 2 * translates + 1);

    coef: LA.LS;

  BEGIN
    target.clipToArray(first, targetvec^);
    basis[LAST(basis^)] := psi.clipToVector(first, size);
    FOR j := -translates TO translates - 1 DO
      basis[j + translates] :=
        phivan.clipToVector(first - twopow * j, size);
    END;
    coef := LA.LeastSquaresGen();
  END MatchPattern;

PROCEDURE Test () =
  BEGIN
    CASE 0 OF
    | 0 => MatchPattern();
    ELSE
      <*ASSERT FALSE*>
    END;
  END Test;

BEGIN
END TestMatchWavelet.
