MODULE TestMatrix EXPORTS Test;
(* Arithmetic for Modula-3, see doc for details

   Abstract:  Tests for LongRealMatrix module. *)

(* ToDo: automatically test laws like commutativity, associativity,
         distributivity, (A*B)^T = B^T * A^T *)

IMPORT LongRealBasic        AS R,
       LongRealVector       AS V,
       LongRealVectorFmtLex AS VF,
       LongRealMatrix       AS M,
       LongRealMatrixFmtLex AS MF;

(*=======================*)
CONST Module = "TestMatrix.";

(*----------------------*)
<*FATAL ANY*>
PROCEDURE TestMatrixBasic (): BOOLEAN =
  CONST
    ftn = Module & "TestMatrixBasic";
    m   = 3;
    n   = 3;
    v3  = ARRAY [0 .. 2] OF R.T{1.0D0, 2.0D0, 3.0D0};
  VAR
    result      := TRUE;
    m1          := M.New(m, n);
    m2    : M.T;
    b           := V.New(n);
  BEGIN
    Debug(1, ftn, "begin\n");

    m1[0] := v3;
    m1[1] := v3;
    m1[2] := v3;
    Msg("m1=\n" & MF.Fmt(m1) & "\n");

    m2 := M.Copy(m1);
    Msg("m2:=copy(m1)=\n" & MF.Fmt(m2) & "\n");

    b^ := v3;
    b := M.MulV(m1, b);
    Msg("m1*v3=\n" & VF.Fmt(b) & "\n");

    Msg("m1+m2=\n" & MF.Fmt(M.Add(m1, m2)) & "\n");
    Msg("m1-m2=\n" & MF.Fmt(M.Sub(m1, m2)) & "\n");
    Msg("m1*m2=\n" & MF.Fmt(M.Mul(m1, m2)) & "\n");

    Msg("m1^T=\n" & MF.Fmt(M.Transpose(m1)) & "\n");

    RETURN result;
  END TestMatrixBasic;

(*----------------------*)
<*FATAL ANY*>
PROCEDURE TestConstruct (): BOOLEAN =
  CONST ftn = Module & "TestConstruct";
  VAR
    result := TRUE;
    b      := V.FromArray(ARRAY OF R.T{1.0D0, 2.0D0, 3.0D0});
    bmr    := M.RowFromVector(b);
    bmc    := M.ColumnFromVector(b);
    bmd    := M.DiagonalFromVector(b);
    big := M.FromMatrixArray(
             ARRAY OF
               ARRAY OF M.T{
               ARRAY [0 .. 1] OF M.T{bmd, bmc},
               ARRAY [0 .. 1] OF M.T{bmr, M.FromScalar(-1.0D0)}});
  BEGIN
    Debug(1, ftn, "begin\n");

    Msg("Row(b)=\n" & MF.Fmt(bmr) & "\n");
    Msg("Column(b)=\n" & MF.Fmt(bmc) & "\n");
    <*ASSERT M.Equal(bmc,M.Transpose(bmr))*>
    <*ASSERT M.Equal(M.Transpose(bmc),bmr)*>

    Msg("Diagonal(b)=\n" & MF.Fmt(bmd) & "\n");
    Msg("Composed \n" & MF.Fmt(big) & "\n");
    <*ASSERT M.Equal(M.Transpose(big),big)*>
    (*
      Msg("Composed \n" & MF.Fmt(M.FromMatrixArray(
        ARRAY OF ARRAY OF M.T{
          ARRAY OF M.T{bmd,bmc},
          ARRAY OF M.T{bmr,M.FromScalar(-1.0D0)}
        })) & "\n");
    *)

    RETURN result;
  END TestConstruct;

(*-------------------------*)
PROCEDURE TestMatrix (): BOOLEAN =
  <*UNUSED*>
  CONST ftn = Module & "TestMatrix";
  VAR result := TRUE;
  BEGIN
    NewLine();
    EVAL TestMatrixBasic();
    NewLine();
    EVAL TestConstruct();
    RETURN result;
  END TestMatrix;
(*=======================*)
BEGIN
END TestMatrix.
