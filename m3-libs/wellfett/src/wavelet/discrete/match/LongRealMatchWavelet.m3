MODULE LongRealMatchWavelet;

IMPORT Integer32IntegerPower AS IIntPow;

IMPORT LongRealVector AS V;

IMPORT LongRealMatrix AS M;
IMPORT LongRealMatrixLapack AS LA;

IMPORT LongRealSignal AS S;

IMPORT LongRealRefinableFunc AS Refn;

(*
IMPORT LongRealFmtLex AS RF;
IMPORT LongRealVectorFmtLex AS VF;
IMPORT LongRealSignalFmtLex AS SF;
IMPORT IO, Fmt, Wr, Thread;
*)

IMPORT NADefinitions AS NA;

PROCEDURE MatchPattern (target                                : S.T;
                        refineMask, generatorMask, waveletMask: S.T;
                        numLevels                             : CARDINAL;
                        firstTranslate                        : INTEGER;
                        numTranslates                         : CARDINAL; ):
  SimpleApprox =
  VAR
    generator := Refn.Refine(generatorMask, refineMask, numLevels);
    wavelet   := Refn.Refine(waveletMask, refineMask, numLevels);

    lastTranslate := firstTranslate + numTranslates - 1;

    twonit := IIntPow.MulPower(2, 2, numLevels);
    first := MIN(wavelet.getFirst(),
                 generator.getFirst() + twonit * firstTranslate);
    last := MAX(wavelet.getLast(),
                generator.getLast() + twonit * lastTranslate);
    size := last - first + 1;

    targetVec := V.New(size);
    basis     := M.New(numTranslates + 1, size);

  <*FATAL NA.Error*>
  BEGIN
    wavelet.clipToArray(first, basis[LAST(basis^)]);
    FOR j := firstTranslate TO lastTranslate DO
      generator.clipToArray(first - twonit * j, basis[j - firstTranslate]);
    END;

    target.clipToArray(first, targetVec^);

    (*
    IO.Put(Fmt.FN("normal matrix %s, right hand side %s\n",
                  ARRAY OF
                    TEXT{MF.Fmt(M.MulMMA(basis)),
                         VF.Fmt(M.MulV(basis, targetvec))}));
    *)

    VAR
      coef := LA.LeastSquares(basis, ARRAY OF V.T{targetVec},
                              flags := LA.LSFlagSet{LA.LSFlag.transposed})[
                0];
      approx := M.MulTV(basis, coef.x);

    BEGIN
      (*
      <*FATAL Thread.Alerted, Wr.Failure*>
      IO.Put(Fmt.FN("numTranslates %s, size %s, residuum %s, %s\n",
                    ARRAY OF
                      TEXT{Fmt.Int(numTranslates), Fmt.Int(size),
                           RF.Fmt(coef.res), VF.Fmt(coef.x)}));
      *)
      (*
      IO.Put(Fmt.FN("residuum - %s\n", ARRAY OF TEXT{RF.Fmt(coef.res)}));
      *)

      RETURN SimpleApprox{
               NEW(S.T).fromArray(
                 SUBARRAY(coef.x^, 0, numTranslates), firstTranslate),
               coef.x[LAST(coef.x^)], NEW(S.T).fromVector(approx, first),
               basis, targetVec};
    END;
  END MatchPattern;


BEGIN
END LongRealMatchWavelet.
