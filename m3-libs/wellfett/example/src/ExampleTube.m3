MODULE ExampleTube;

IMPORT LongRealBasic                          AS R,
       LongRealTrans                          AS RT,
       LongRealVector                         AS V,
       LongRealVectorTrans                    AS VT,
       LongRealMatrix                         AS M,
       LongRealSignal                         AS S,
       LongRealScaledSignal                   AS ScaledSignal,
       LongRealSignalIntegerPower             AS SIntPow,
       LongRealMatrixFmtLex                   AS MF,
       LongRealSignalFmtLex                   AS SF,
       LongRealDyadicFilterBank               AS Bank,
       LongRealVanillaDyadicFilterBank        AS VBank,
       LongRealDyadicDiscreteWaveletTransform AS DWT,
       LongRealBSplineWavelet                 AS BSpline,
       LongRealWaveletMatchBasis              AS WMBasis,
       LongRealWaveletMatchSmooth             AS WaveletMatchSmooth;

FROM ExampleMEG IMPORT PlotReal;
IMPORT LongRealPLPlot AS PL, LongRealDWTPlot AS DWTPlot;

(*
IMPORT LongRealPLPlotStream     AS Stream,
       LongRealPLPlotFrame      AS Frame,
       LongRealPLPlotFigure     AS Figure,
       LongRealPLPlotLineStyle  AS LineStyle,
       LongRealScaledSignalPlot AS ScaledSignalPlot;
*)

IMPORT Pathname, IO, Fmt, Rd, FileRd, DosToUnixRd, Wr, Lex;
IMPORT Thread, OSError, FloatMode, Atom, AtomList, RdUtils;
IMPORT Arithmetic, Range;




TYPE VectorPair = RECORD x, y: V.T;  END;

PROCEDURE LoadVector (filename: Pathname.T; ): VectorPair
  RAISES {OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted} =
  BEGIN
    TRY
      WITH rd  = DosToUnixRd.New(FileRd.Open(filename)),
           mat = MF.Lex(rd)                               DO
        (* mat := MF.Lex(rd,MF.LexStyle{rowTerm:='\r',matTerm:='\n'}); *)
        (* IO.Put(MF.Fmt(mat) & "\n"); *)
        Rd.Close(rd);
        RETURN VectorPair{M.GetColumn(mat, 0), M.GetColumn(mat, 1)};
      END;
    EXCEPT
    | OSError.E (item) =>
        RAISE OSError.E(AtomList.Cons(
                          Atom.FromText("On opening " & filename & "\n"),
                          item));
    END;
  END LoadVector;


(* similar to Signal.slim, should be separated from this module *)
PROCEDURE Strip (x: S.T; tol: LONGREAL; ): S.T =
  BEGIN
    WITH data = x.getData()^ DO
      VAR
        begin: CARDINAL := FIRST(data);
        end  : CARDINAL := LAST(data);
      BEGIN
        tol := tol * VT.NormInf(x.getData());
        WHILE end >= 0 AND ABS(data[end]) <= tol DO DEC(end); END;
        WHILE begin <= end AND ABS(data[begin]) <= tol DO INC(begin); END;
        RETURN NEW(S.T).fromArray(SUBARRAY(data, begin, end + 1 - begin),
                                  x.getFirst() + begin);
      END;
    END;
  END Strip;



PROCEDURE Normalise (x: S.T; ): S.T =
  BEGIN
    RETURN x.scale(R.One / VT.Norm2(x.getData()));
  END Normalise;


(**
  Match a wavelet with a pattern,
  display the basis functions, the pattern is re-composed of,
  apply the wavelet transform with respect to matched wavelet to the pattern.

  translate - shift of the pattern in order to give an optimal wavelet match
  dualSmooth, primalSmooth - number of smoothness factors in dual and primal generator
     (order of the convolutional B-Spline factor)
  dualVanishing - number of vanishing moments of the dual wavelet,
     it must be  dualVanishing <= primalSmooth
  refine - number of levels of refinement,
     represents a unit size of 2^refine,
     the (refine)th level of the DWT gives the matches with the pattern
  liftSupport - extent of the lifting filter
*)
PROCEDURE DWTFilter (fileName                               : Pathname.T;
                     translate                              : INTEGER;
                     dualSmooth, primalSmooth, dualVanishing: CARDINAL;
                     refine                                 : CARDINAL;
                     liftSupport                            : Range.T;    )
  RAISES {OSError.E, FloatMode.Trap, Lex.Error, Rd.Failure, Thread.Alerted} =
  CONST
    numLevels = 5;               (* number of levels of decomposition in
                                    the wavelet transform *)
    dualAnalysis = TRUE;

  CONST
    (* Path = "D:\\tube"; *)
    Path = "/data1/tube";

  VAR
    filePath   := Pathname.Join(Path, fileName, "dat");
    xy         := LoadVector(filePath);
    resolution := (xy.x[0] - xy.x[LAST(xy.x^)]) / FLOAT(LAST(xy.x^), R.T);
    clip       := NEW(S.T).fromVector(xy.y);
    (* clip := NEW(S.T).fromArray(SUBARRAY(xy.y^, 0, 2000)); *)

    bank, revBank: Bank.Pair;

  BEGIN
    IO.Put("size of pattern: " & Fmt.Int(clip.getNumber()) & "\n");

    PL.Init();
    TRY
      IF FALSE THEN
        (* CDF-wavelet base *)
        bank[0] := Bank.TBody{BSpline.GeneratorMask(4).scale(RT.SqRtTwo),
                              BSpline.WaveletMask(4, 2).scale(RT.SqRtTwo)};
        bank[1] := Bank.DualToPrimal(bank[0]);

      ELSE
        <* FATAL Wr.Failure *>
        VAR
          peak               := NEW(S.T).fromVector(xy.y, translate);
          peakClip           := Strip(peak, 1.0D-2);
          peakClipNormalised := Normalise(peak); (* peakClip *)
        BEGIN
          EVAL peakClip;
          (* Ignore the clipped pattern.  There is no need to clip, because
             the wavelet is only matched, where the wavelet can be. *)
          IO.Put("size of stripped pattern: "
                   & Fmt.Int(peakClip.getNumber()) & "\n");

          (* matched wavelet base *)
          PL.StartPage();
          PlotReal(
            ARRAY OF ScaledSignal.T{ScaledSignal.T{peakClip, resolution}});
          PL.StopPage();

          bank :=
            WaveletMatchSmooth.BSplineWithPattern(
              peakClipNormalised, dualSmooth, primalSmooth, dualVanishing,
              liftSupport, refine, R.Zero, (* 1.0D-10,*)
              WaveletMatchSmooth.FlagSet{WaveletMatchSmooth.Flag.Plot},
              WaveletMatchSmooth.Options{maxIter := 0});
          (* Note that the dual filters are the matched ones. *)
          bank := VBank.ScaleSqRtTwo(bank);
          (* For correlation the filter must be flipped *)
          revBank := VBank.Reverse(bank);

          IO.Put("Dual lowpass: " & SF.Fmt(bank[0, 0]) & "\n");
          IO.Put("Primal lowpass: " & SF.Fmt(bank[1, 0]) & "\n");
        END;
      END;

      VAR
        wt   : DWT.T;
        recon: S.T;
        vanishing := SIntPow.MulPower(S.One, WMBasis.vanishingAtom,
                                      primalSmooth - dualVanishing);
        revVanishing := vanishing.reverse();

      BEGIN
        IF dualAnalysis THEN
          wt :=
            DWT.HighBandOversampledFromSignal(clip, revBank[1], numLevels);
        ELSE
          wt := DWT.HighBandUpConvolve(
                  DWT.FromSignal(clip, bank[0], numLevels), vanishing);
        END;

        PL.StartPage();
        DWTPlot.HighBandOversampledDWT(wt, clip, resolution);
        PL.StopPage();

        (* process the wavelet transform *)
        PL.StartPage();
        wt.high[0] := wt.high[0].scale(1.0D-2);
        (* wt.high[1] := S.Zero; *)
        (* wt.high[2] := wt.high[2].scale(1.0D-1); *)
        wt.low := wt.low.scale(1.0D-3);
        DWTPlot.HighBandOversampledDWT(wt, clip, resolution);
        PL.StopPage();

        IF dualAnalysis THEN
          recon := DWT.ToSignal(DWT.HighBandConvolveDown(wt, revVanishing),
                                revBank[0]);
        ELSE
          recon := DWT.HighBandOversampledToSignal(wt, bank[1]);
        END;
        PL.StartPage();
        PL.SetSubWindows(1, 1);
        PlotReal(
          ARRAY OF
            ScaledSignal.T{
            ScaledSignal.T{clip, resolution},
            ScaledSignal.T{
              recon.clip(clip.getFirst(), clip.getNumber()), resolution}});
        PlotReal(
          ARRAY OF
            ScaledSignal.T{
            (* ScaledSignal.T{clip.clip(clip.getFirst(), 500),
               resolution},*)
            ScaledSignal.T{recon.clip(clip.getFirst(), 500), resolution}});
        PL.StopPage();
      END;

    EXCEPT
    | Arithmetic.Error (error) =>
        IO.Put("Error:\n");
        IO.Put(Atom.ToText(error.head) & "\n");
    END;
    PL.Exit();
  END DWTFilter;



PROCEDURE Run () =
  <* FATAL ANY *>
  BEGIN
    TRY
      CASE 1 OF
      | 0 =>
          DWTFilter(
            "GT_firstBackwall", (*translate*) -145, (*moments*) 4, 6,
            6, (*refine*) 3, (*lifting filter range*) Range.New(-3, 15));
      | 1 =>
          DWTFilter(
            "EMUS_firstBackwall", (*translate*) -277, (*moments*) 4, 6,
            6, (*refine*) 2, (*lifting filter range*) Range.New(-1, 9));
      ELSE
        <* ASSERT FALSE *>
      END;
    EXCEPT
    | OSError.E (item) => IO.Put(RdUtils.FailureText(item));
    | Lex.Error => IO.Put("syntax error in data file\n");
    END;
  END Run;

BEGIN
END ExampleTube.
