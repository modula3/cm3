MODULE TestSLE EXPORTS Test;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Test driver for LongRealMatrixDecomposition (simultaneous
   linear equations) *)

IMPORT Arithmetic AS Arith;
IMPORT LongRealBasic               AS R,
       LongRealTrans               AS RT,
       LongRealFmtLex              AS RF,
       LongRealVector              AS V,
       LongRealMatrix              AS M,
       LongRealVectorFmtLex        AS VF,
       LongRealMatrixFmtLex        AS MF,
       LongRealMatrixDecomposition AS MD,
       LongRealVectorTrans         AS VT,
       LongRealMatrixTrans         AS MT,
       RandomBayesDurham           AS Rand,
       Fmt;

CONST Module = "TestSLE.";
<* FATAL ANY *>

  (*=====================================*)
TYPE
  M3x3 = ARRAY [0 .. 2] OF ARRAY [0 .. 2] OF R.T;
  V3 = ARRAY [0 .. 2] OF R.T;

VAR rand := NEW(Rand.T).init();

(*---------------------*)
(*---------------------*)
PROCEDURE BuildAX (VAR (*OUT*) A, C, D       : M.T;
                   VAR (*OUT*) knownX, foundX: V.T;
                               size          : CARDINAL := 3) =
  <* UNUSED *>
  CONST
    ftn = Module & "BuildAX";
  VAR
    n  := size;
    n1 := 0;
    nn := n - 1;
  BEGIN
    A := NEW(M.T, n, n);
    C := NEW(M.T, n, n);
    D := NEW(M.T, n, n);
    knownX := NEW(V.T, n);       (*X is an nx1 matrix*)
    foundX := NEW(V.T, n);

    FOR i := n1 TO nn DO
      FOR j := n1 TO nn DO A[i, j] := rand.uniform(0.0d0, 9.99d0); END;
    END (*for*);

    FOR i := n1 TO nn DO
      knownX[i] := rand.uniform(0.0d0, 9.99d0);
    END (*for*);

  END BuildAX;
(*---------------------*)
PROCEDURE BuildB (VAR (*OUT*) B: V.T; A: M.T; knownX: V.T) =
  <* UNUSED *>
  CONST
    ftn = Module & "BuildB";
  BEGIN
    B := M.MulV(A, knownX);
  END BuildB;
(*---------------------*)
PROCEDURE BuildData (VAR (*OUT*) A, C, D          : M.T;
                     VAR (*OUT*) B, knownX, foundX: V.T;
                                 size             : CARDINAL := 3) =
  <* UNUSED *>
  CONST
    ftn = Module & "BuildData";
  BEGIN
    BuildAX(A, C, D, knownX, foundX, size);
    BuildB(B, A, knownX);
  END BuildData;
(*--------------------*)
PROCEDURE TestBacksub (): BOOLEAN =
  CONST ftn = Module & "TestBacksub";
  VAR
    size      := 4;
    A   : M.T;
    B   : V.T;
    C   : M.T;
    D   : M.T;

    knownX: V.T;                 (*X is an nx1 matrix*)
    foundX: V.T;

    result := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    BuildAX(A, C, D, knownX, foundX, size);
    (*---zero out lower triangle---*)
    FOR row := 0 TO LAST(A^) DO
      FOR col := 0 TO row - 1 DO A[row, col] := R.Zero; END;
    END;
    BuildB(B, A, knownX);

    Msg("A=" & MF.Fmt(A));
    Msg("B=" & VF.Fmt(B));
    MD.BackSubst(A, x := foundX, b := B);
    Msg("knownX=" & VF.Fmt(knownX));
    Msg("foundX=" & VF.Fmt(foundX));

    <* ASSERT VT.NormInf(V.Sub(foundX, knownX))
                < RT.Eps * 10.0D0 * VT.NormInf(knownX) *>

    RETURN result;
  END TestBacksub;
(*--------------------*)
PROCEDURE TestHouseholder (): BOOLEAN =
  CONST ftn = Module & "TestHouseholder";
  VAR
    A: M.T;
    B: V.T;
    C: M.T;
    D: M.T;

    knownX: V.T;                 (*X is an nx1 matrix*)
    foundX: V.T;

    result := TRUE;

  BEGIN
    Debug(1, ftn, "begin\n");
    BuildData(A, C, D, B, knownX, foundX, 4);

    Msg("A=" & MF.Fmt(A));
    MD.HouseHolderD(A);
    Msg("HouseHolder(A)=" & MF.Fmt(A));
    RETURN result;
  END TestHouseholder;
(*--------------------*)
PROCEDURE TestTridiag (): BOOLEAN =
  CONST
    ftn = Module & "TestTridiag";
    n   = 3;
    n1  = 0;
    nn  = n - 1;
  VAR
    A      := NEW(M.T, n, n);
    knownX := NEW(R.Array, n);
    foundX := NEW(R.Array, n);
    t := MD.Tridiagonals{NEW(R.Array, n), NEW(R.Array, n), NEW(R.Array, n)};
    r     : R.Array;
    result          := TRUE;
  BEGIN
    Debug(1, ftn, "begin\n");
    A^ := M3x3{V3{1.0d0, 1.0d0, 0.0d0}, V3{0.5d0, 2.0d0, 1.0d0},
               V3{0.0d0, 0.5d0, 3.0d0}};
    FOR i := n1 TO nn DO
      IF i > n1 THEN t.a[i] := A[i, i - 1]; END;
      t.b[i] := A[i, i];
      IF i < nn THEN t.c[i] := A[i, i + 1]; END;
    END;
    Msg("A=" & MF.Fmt(A));
    knownX^ := V3{1.0d0, 2.0d0, 3.0d0};
    Msg("knownX=" & VF.Fmt(knownX));
    r := M.MulV(A, knownX);
    Msg("r=     " & VF.Fmt(r));

    MD.SolveTridiagonal(t, r, foundX);
    Msg("foundX=" & VF.Fmt(foundX));

    <* ASSERT VT.NormInf(V.Sub(foundX, knownX))
                < RT.Eps * VT.NormInf(knownX) *>

    RETURN result;
  END TestTridiag;

(*--------- LU factor ---------*)
PROCEDURE TestLU (): BOOLEAN RAISES {} =
  CONST ftn = Module & "TestLU";
  VAR
    U1 := M.FromArray(M.TBody{V.TBody{1.0D0, 2.3D0, -0.3D0, 0.7D0},
                              V.TBody{0.0D0, -1.2D0, -0.5D0, 0.1D0},
                              V.TBody{0.0D0, 0.0D0, -0.3D0, 0.7D0},
                              V.TBody{0.0D0, 0.0D0, 0.0D0, 0.4D0}});
    Ms := ARRAY [0 .. 3] OF
            M.T{U1, M.Transpose(U1),
                M.FromArray(M.TBody{V.TBody{1.0D0, 2.3D0, -0.3D0, 0.7D0},
                                    V.TBody{0.3D0, -1.2D0, -0.5D0, 0.1D0},
                                    V.TBody{0.0D0, 2.3D0, -0.3D0, 0.7D0},
                                    V.TBody{0.0D0, 0.0D0, -0.1D0, 0.4D0}}),
                M.FromArray(M.TBody{V.TBody{1.0D0, 0.0D0, 0.0D0, 0.0D0},
                                    V.TBody{0.0D0, 0.0D0, 0.0D0, 0.1D0},
                                    V.TBody{0.0D0, 0.8D0, 0.0D0, 0.0D0},
                                    V.TBody{0.0D0, 0.0D0, -0.1D0, 0.0D0}})};

    knownX := V.FromArray(V.TBody{0.7D0, 1.3D0, -0.2D0, 0.3D0}); (*X is an
                                                                    nx1
                                                                    matrix*)

  BEGIN
    Debug(1, ftn, "begin\n");

    FOR i := FIRST(Ms) TO LAST(Ms) DO
      TRY
        VAR
          A      := Ms[i];
          lu     := MD.LUFactor(A);
          b      := M.MulV(A, knownX);
          foundX := MD.LUBackSubst(lu, b);
          det    := MD.LUDet(lu);
          AI     := MD.LUInverse(lu);

        BEGIN
          Msg("Factorize " & MF.Fmt(A) & "\n");
          Msg("LU factors: sign =" & Fmt.Int(lu.sign) & "\n");
          Msg("L = " & MF.Fmt(lu.L) & "\n");
          Msg("U = " & MF.Fmt(lu.U) & "\n");
          Msg("det = " & RF.Fmt(det) & "\n");
          Msg("Inverse(A) =" & MF.Fmt(AI));
          Msg("Inverse(A)*A =" & MF.Fmt(M.Mul(AI,A)));

          Msg("knownX = " & VF.Fmt(knownX));
          Msg("foundX = " & VF.Fmt(foundX));

          (* check solution of the equation system *)
          <* ASSERT VT.NormInf(V.Sub(foundX, knownX))
                      < RT.Eps * 10.0D0 * VT.NormInf(knownX) *>
          (* verify the matrix inversion *)
          <* ASSERT MT.Norm1(M.Sub(M.Mul(AI, A), M.NewOne(NUMBER(A^))))
                      < RT.Eps * 10.0D0 * MT.Norm1(A) *>
        END;

      EXCEPT
      | Arith.Error (err) =>
          Msg("LU fails, error code " & Fmt.Int(TYPECODE(err)) & "\n");
          RETURN FALSE;
      END;
    END;

    RETURN TRUE;
  END TestLU;
(*------------------------*)
PROCEDURE TestSLE (): BOOLEAN =
  BEGIN
    NewLine();
    EVAL TestBacksub();
    NewLine();
    EVAL TestHouseholder();
    NewLine();
    EVAL TestTridiag();
    NewLine();
    EVAL TestLU();
    RETURN TRUE;
  END TestSLE;
(*=======================*)
BEGIN
END TestSLE.
