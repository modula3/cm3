MODULE TestFmtLex EXPORTS Test;
(*Copyright (c) 1996, m3na project*)

(*Abstract: Tests for FmtLex module.*)

IMPORT LongRealBasic AS R;
IMPORT LongRealVector AS V;
IMPORT LongRealVectorRep AS VS;
IMPORT LongRealMatrix AS M;

IMPORT LongRealFmtLex AS RF;
IMPORT LongRealVectorFmtLex AS VF;
IMPORT LongRealMatrixFmtLex AS MF;

IMPORT IO, Fmt, TextRd;


IMPORT Rd, Wr, TextWr, Thread;
IMPORT Fmt AS F;
IMPORT Lex AS L;
IMPORT FloatMode;
IMPORT FmtLexSupport AS FSup;

(*=======================*)
CONST Module = "TestFmtLex.";


(*----------------------*)
PROCEDURE TestMatrixIO (): BOOLEAN =
  CONST ftn = Module & "TestMatrixIO";
  CONST
    mat = ARRAY OF
            ARRAY [0 .. 2] OF R.T{
            ARRAY [0 .. 2] OF R.T{1.0D0, 0.5D0, 0.7D0},
            ARRAY [0 .. 2] OF R.T{10.0D0, 5.3D0, 1.7D0},
            ARRAY [0 .. 2] OF R.T{1.0D3, 0.9D0, 2.7D0}};
  VAR
    result := TRUE;
    text   := "";

  BEGIN
    Debug(1, ftn, "begin\n");

    FOR i := FIRST(mat) TO LAST(mat) DO
      FOR j := FIRST(mat[0]) TO LAST(mat[0]) DO
        text := text & RF.Fmt(mat[i, j]);
        IF j < LAST(mat[0]) THEN text := text & " "; END;
      END;
      text := text & "\n";
    END;
    IO.Put(text);

    VAR
      rd     := TextRd.New(text);
      newMat := MF.Lex(rd);
    BEGIN
      IO.Put(MF.Fmt(newMat) & "\n");
      (* <*ASSERT MS.Equal(mat,newMat^)*> *)
      <*ASSERT M.Equal(M.FromArray(mat),newMat)*>
    END;

    RETURN result;
  END TestMatrixIO;
(*-------------------------*)
PROCEDURE TestFmtLex (): BOOLEAN =
  CONST ftn = Module & "TestFmtLex";
  VAR result := TRUE;
  BEGIN
    NewLine();
    EVAL TestMatrixIO();
    RETURN result;
  END TestFmtLex;
(*=======================*)
BEGIN
END TestFmtLex.
