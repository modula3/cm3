MODULE TestTex EXPORTS Test;
(*Arithmetic for Modula-3, see doc for details

   Abstract: Tests for Tex module. *)

IMPORT FileWr, Wr, Fmt, Process;
IMPORT Thread, OSError;
IMPORT TempFiles AS Tmp;

IMPORT LongRealBasic             AS R,
       LongRealComplex           AS C,
       LongRealPolar             AS Polar,
       BigInteger                AS B,
       BigIntegerFraction        AS Fr,
       LongRealVector            AS V,
       LongRealMatrix            AS M,
       LongRealComplexPolynomial AS P,

       LongRealFmtLex                  AS RF,
       LongRealComplexFmtLex           AS CF,
       LongRealPolarFmtLex             AS PolarF,
       BigIntegerFmtLex                AS BF,
       BigIntegerFractionFmtLex        AS FrF,
       LongRealVectorFmtLex            AS VF,
       LongRealMatrixFmtLex            AS MF,
       LongRealComplexPolynomialFmtLex AS PF;

IMPORT Arithmetic AS Arith;

(*=======================*)
CONST Module = "TestTex.";
(*----------------------*)
PROCEDURE TestTexVector (): BOOLEAN =
  CONST
    ftn      = Module & "TestTexVector";
    filename = "test";
  VAR
    result := TRUE;
    out    := FileWr.Open(filename & ".tex");

  <*FATAL OSError.E, Thread.Alerted, Wr.Failure, Arith.Error *>
  BEGIN
    Debug(1, ftn, "begin\n");

    Tmp.Note(filename & ".tex");

    Wr.PutText(out, "\\documentclass[a4paper]{article}\n");
    Wr.PutText(out, "\\begin{document}\n");

    CONST y = ARRAY OF R.T{1.3D0, -0.4D0, -0.2D0, 3.6D0, -2.3D0};
    VAR
      x := V.FromArray(ARRAY OF R.T{1.0D0, 1.5D0, -0.3D0, 0.7D0, -2.3D0});
      A := M.New(NUMBER(x^), NUMBER(x^));
    BEGIN
      FOR i := 0 TO LAST(y) DO
        FOR j := 0 TO LAST(y) DO A[i, j] := y[ABS(i - j)]; END;
      END;

      Wr.PutText(out, "$$\n");
      Wr.PutText(out, RF.Tex(V.Inner(x, M.MulV(A, x))) & "\n");
      Wr.PutText(
        out,
        "=" & VF.Tex(x, style := VF.TexStyle{flags := VF.TexFlagSet{}}));
      Wr.PutText(out, "\\cdot" & MF.Tex(A));
      Wr.PutText(
        out, "\\cdot" & VF.Tex(x,
                               style := VF.TexStyle{
                                          flags := VF.TexFlagSet{
                                                     VF.TexFlag.vertical}}));
      Wr.PutText(out, "$$\n");
    END;

    CONST
      y = ARRAY OF
            C.T{
            C.T{0.0D20, 0.0D0}, C.T{1.0D20, 0.0D0}, C.T{3.0D23, -4.0D23},
            C.T{-1.0D23, -1.0D23}, C.T{0.0D0, 2.5D0}};
    BEGIN
      Wr.PutText(out, "\\begin{eqnarray*}\n");
      FOR j := 0 TO LAST(y) DO
        Wr.PutText(out, CF.Tex(y[j]) & "&=&");
        Wr.PutText(out, PolarF.Tex(Polar.FromComplex(y[j])) & "\\\\\n");
      END;
      Wr.PutText(out, "\\end{eqnarray*}\n");
    END;

    VAR
      (*
          x := B.FromInteger(12345679);
          y := x;
      *)
      x       := B.FromInteger(987654321);
      y       := B.FromInteger(1608040201);
      billard := B.FromInteger(1000000000);
    BEGIN
      Wr.PutText(out, "$$\n");
      Wr.PutText(out, "\\begin{array}{rcrcl}\n");
      FOR j := 0 TO 20 DO
        Wr.PutText(out, FrF.Tex(Fr.T{y, x}) & "&=&");
        Wr.PutText(
          out, FrF.Tex(
                 Fr.T{y, x},
                 style := FrF.TexStyle{
                            flags := FrF.TexFlagSet{FrF.TexFlag.fraction}})
                 & "&=&");
        <*FATAL Arith.Error*>
        VAR qr := B.DivMod(y, x);
        BEGIN
          Wr.PutText(
            out, BF.Tex(qr.quot) & "."
                   & Fmt.Pad(
                       BF.Tex(B.DivMod(B.Mul(billard, qr.rem), x).rem), 9,
                       '0') & "\\\\\n");
        END;
        VAR z := B.Add(x, y);
        BEGIN
          x := y;
          y := z;
        END;
      END;
      Wr.PutText(out, "\\end{array}\n");
      Wr.PutText(out, "$$\n");
    END;

    VAR y := NEW(REF ARRAY OF P.T, 4);
    TYPE Flag = PF.TexFlag;
    BEGIN
      y^ :=
        ARRAY OF
          P.T{P.FromArray(ARRAY OF C.T{}),
              P.FromArray(ARRAY OF C.T{C.T{0.0D20, 0.0D0}}),
              P.FromArray(ARRAY OF
                            C.T{C.T{0.0D20, 0.5D0}, C.T{3.0D23, -4.0D23},
                                C.T{1.0D0, 0.0D0}}),
              P.FromArray(ARRAY OF
                            C.T{C.T{-1.0D0, 0.0D0}, C.Zero, C.Zero,
                                C.T{0.0D0, 1.0D0}})};
      Wr.PutText(out, "\\begin{eqnarray*}\n");
      FOR j := 0 TO LAST(y^) DO
        Wr.PutText(out, "\\lefteqn{" & PF.Tex(y[j]) & ":}\\\\\n");
        Wr.PutText(out, PF.Tex(y[j], style := PF.TexStyle{
                                                flags := PF.TexFlagSet{
                                                           Flag.powerSum}})
                          & "&=&");
        Wr.PutText(
          out, PF.Tex(y[j], style :=
                              PF.TexStyle{
                                flags := PF.TexFlagSet{
                                           Flag.powerSum, Flag.simplePower,
                                           Flag.omitZero, Flag.reverse}})
                 & "\\\\\n");
      END;
      Wr.PutText(out, "\\end{eqnarray*}\n");
    END;

    Wr.PutText(out, "\\end{document}\n");

    Wr.Close(out);

    Tmp.Note(filename & ".log");
    Tmp.Note(filename & ".aux");
    Tmp.Note(filename & ".dvi");
    EVAL Process.Wait(Process.Create("latex", ARRAY OF TEXT{filename}));
    EVAL Process.Wait(Process.Create("xdvi", ARRAY OF TEXT{filename}));

    RETURN result;
  END TestTexVector;
(*-------------------------*)
PROCEDURE TestTex (): BOOLEAN =
  <*UNUSED*>
  CONST ftn = Module & "TestTex";
  VAR result := TRUE;
  BEGIN
    NewLine();
    EVAL TestTexVector();
    RETURN result;
  END TestTex;
(*=======================*)
BEGIN
END TestTex.
