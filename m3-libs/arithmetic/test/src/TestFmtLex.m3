MODULE TestFmtLex EXPORTS Test;
(*Arithmetic for Modula-3, see doc for details*)

(*Abstract: Tests for FmtLex module.*)

IMPORT LongRealBasic        AS R,
       LongRealMatrix       AS M,
       LongRealFmtLex       AS RF,
       LongRealMatrixFmtLex AS MF;

IMPORT IO, Lex, TextRd;

IMPORT Rd, Wr, Thread;
IMPORT FloatMode;
IMPORT NADefinitions AS NA;

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

  <*FATAL FloatMode.Trap, Lex.Error, Rd.Failure, Wr.Failure,
          Thread.Alerted, NA.Error*>
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
      Rd.Close(rd);
    END;

    RETURN result;
  END TestMatrixIO;
(*-------------------------*)
PROCEDURE TestFmtLex (): BOOLEAN =
  <*UNUSED*>
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
