MODULE TestEigenSystem EXPORTS Test;
(* Arithmetic for Modula-3, see doc for details

   Abstract: Test driver for Eigensystem *)

IMPORT LongRealEigenSystem      AS EigenSys,
       LongRealCharPolynomial   AS CP,
       LongRealMatrix           AS M,
       LongRealVector           AS V,
       LongRealPolynomial       AS P,
       LongRealMatrixFmtLex     AS MF,
       LongRealPolynomialFmtLex AS PF;
IMPORT Wr, IO, Thread, Fmt, Arithmetic AS Arith;

<* FATAL Wr.Failure, Thread.Alerted *>


CONST
  Module = "TestEigenSystem.";

PROCEDURE Print2 (d, e: V.T; READONLY dWR, eWR: V.TBody; ) =
  BEGIN
    IO.Put(Fmt.Pad("i", 4) & Fmt.Pad("Diagonal", 18)
             & Fmt.Pad("Sub-Diagonal", 18) & "\n");
    FOR i := FIRST(d^) TO LAST(d^) DO
      IO.Put(Fmt.Pad(Fmt.Int(i), 2)
               & Fmt.Pad(Fmt.LongReal(d[i], Fmt.Style.Sci, 12), 19)
               & Fmt.Pad(Fmt.LongReal(e[i], Fmt.Style.Sci, 12), 19) & "\n");
    END;                         (* for *)
    IO.Put("\nComparison with Wilkinson-Reinsch:\n");
    FOR i := FIRST(d^) TO LAST(d^) DO
      IO.Put(
        Fmt.Pad(Fmt.Int(i), 4) & Fmt.Pad(Fmt.LongReal(d[i] - dWR[i]), 18)
          & Fmt.Pad(Fmt.LongReal(e[i] - eWR[i]), 18) & "\n");
    END;                         (* for *)

  END Print2;

PROCEDURE Print3 (d, e, e2: V.T; READONLY dWR, eWR, e2WR: V.TBody; ) =
  BEGIN
    IO.Put(Fmt.Pad("i", 4) & Fmt.Pad("Diagonal", 18)
             & Fmt.Pad("Sub-Diagonal", 18) & Fmt.Pad("Sub-Diagonal**2", 18)
             & "\n");
    FOR i := FIRST(d^) TO LAST(d^) DO
      IO.Put(
        Fmt.Pad(Fmt.Int(i), 2)
          & Fmt.Pad(Fmt.LongReal(d[i], Fmt.Style.Sci, 12), 19)
          & Fmt.Pad(Fmt.LongReal(e[i], Fmt.Style.Sci, 12), 19)
          & Fmt.Pad(Fmt.LongReal(e2[i], Fmt.Style.Sci, 12), 19) & "\n");
    END;                         (* for *)
    IO.Put("\nComparison with Wilkinson-Reinsch:\n");
    FOR i := FIRST(d^) TO LAST(d^) DO
      IO.Put(
        Fmt.Pad(Fmt.Int(i), 4) & Fmt.Pad(Fmt.LongReal(d[i] - dWR[i]), 18)
          & Fmt.Pad(Fmt.LongReal(e[i] - eWR[i]), 18)
          & Fmt.Pad(Fmt.LongReal(e2[i] - e2WR[i]), 18) & "\n");
    END;                         (* for *)

  END Print3;


PROCEDURE RunTql1 (VAR d, e: V.T; ) =
  <* FATAL Arith.Error *>
  BEGIN
    EigenSys.Tql1(d, e);
    IO.Put("Eigenvalues\n");
    FOR i := FIRST(d^) TO LAST(d^) DO
      IO.Put(Fmt.Pad(Fmt.LongReal(d[i], prec := 12), 20) & "\n");
    END;                         (* for *)
  END RunTql1;

PROCEDURE RunTql2 (VAR d, e: V.T; VAR z: M.T; ) =
  <* FATAL Arith.Error *>
  BEGIN
    EigenSys.Tql2(d, e, z);
    IO.Put("Eigenvalues\n");
    FOR i := FIRST(d^) TO LAST(d^) DO
      IO.Put(Fmt.Pad(Fmt.LongReal(d[i], prec := 12), 20) & "\n");
    END;                         (* for *)
  END RunTql2;

PROCEDURE RunTred1 (a: M.T; READONLY dWR, eWR, e2WR: V.TBody; ) =
  VAR
    z  := M.Copy(a);
    d  := NEW(V.T, NUMBER(a^));
    e  := NEW(V.T, NUMBER(a^));
    e2 := NEW(V.T, NUMBER(a^));
  BEGIN
    EigenSys.Tred1(NUMBER(d^), z, d, e, e2);
    Print3(d, e, e2, dWR, eWR, e2WR);
    RunTql1(d, e);
  END RunTred1;

PROCEDURE RunTred2 (a: M.T; READONLY dWR, eWR: V.TBody; ) =
  VAR
    aLocal := M.Copy(a);
    dWR2   := V.FromArray(dWR);
    d      := NEW(V.T, NUMBER(a^));
    e      := NEW(V.T, NUMBER(a^));
  BEGIN
    EigenSys.Tred2(NUMBER(d^), aLocal, d, e);
    dWR2[1] := -dWR2[1];
    Print2(d, e, dWR2^, eWR);
    dWR2[1] := -dWR2[1];
    IO.Put("Transformation matrix\n");
    FOR i := FIRST(a^) TO LAST(a^) DO
      FOR j := FIRST(a[0]) TO LAST(a[0]) DO
        IO.Put(Fmt.Pad(Fmt.LongReal(aLocal[i, j], prec := 10), 15));
      END;                       (* for *)
      IO.Put("\n");
    END;                         (* for *)
    RunTql2(d, e, aLocal);
  END RunTred2;



PROCEDURE RunTestA () =
  CONST
    aWR = M.TBody{M.TRow{10.0D0, 1.0D0, 2.0D0, 3.0D0, 4.0D0},
                  M.TRow{1.0D0, 9.0D0, -1.0D0, 2.0D0, -3.0D0},
                  M.TRow{2.0D0, -1.0D0, 7.0D0, 3.0D0, -5.0D0},
                  M.TRow{3.0D0, 2.0D0, 3.0D0, 12.0D0, -1.0D0},
                  M.TRow{4.0D0, -3.0D0, -5.0D0, -1.0D0, 15.0D0}};
    eWR = V.TBody{0.0d0, 7.49484677741D-1, -4.49626820120D0,
                  -2.15704099085D0, 7.14142842854D0};
    dWR = V.TBody{9.29520217754D0, 1.16267115569D1, 1.09604392078D1,
                  6.11764705885D0, 1.5D1};
    e2WR = V.TBody{0.0D0, 5.61727282169D-1, 2.02164277371D1,
                   4.65282583621D0, 5.1D1};

  VAR a := NEW(M.T, 5, 5);
  BEGIN
    a^ := aWR;

    IO.Put("Test: Tred1\n");
    FOR i := FIRST(a^) TO LAST(a^) DO
      FOR j := FIRST(a[0]) TO LAST(a[0]) DO
        IO.Put(Fmt.Pad(Fmt.LongReal(a[i, j]), 12));
      END;                       (* for *)
      IO.Put("\n");
    END;                         (* for *)
    RunTred1(a, dWR, eWR, e2WR);

    IO.Put("Test: Tred2\n");
    FOR i := FIRST(a^) TO LAST(a^) DO
      FOR j := FIRST(a[0]) TO LAST(a[0]) DO
        IO.Put(Fmt.Pad(Fmt.LongReal(a[i, j]), 12));
      END;                       (* for *)
      IO.Put("\n");
    END;                         (* for *)
    RunTred2(a, dWR, eWR);

  END RunTestA;

PROCEDURE TestCharPolynomial () =
  VAR
    p, cp: P.T;
    m    : M.T;
  BEGIN
    p := P.New(10);
    (*
    FOR j:=0 TO LAST(p^)-1 DO
      p[j]:=FLOAT(j,R.T);
    END;
    p[LAST(p^)]:=R.One;
    *)
    p^ := P.TBody{0.0D0, 1.0D0, -3.0D0, 4.0D0, 7.0D0, -2.0D0, 4.0D0, 3.0D0,
                  -1.0D0, 5.0D0, 1.0D0};
    Msg(Fmt.FN("Polynomial %s\n", ARRAY OF TEXT{PF.Fmt(p)}));
    m := CP.CompanionMatrix(p);
    Msg(Fmt.FN("Companion %s\n",
               ARRAY OF TEXT{MF.Fmt(m, MF.FmtStyle{width := 4})}));
    cp := CP.CharPolynomial(m);
    Msg(
      Fmt.FN("Characteristic polynomial %s\n", ARRAY OF TEXT{PF.Fmt(cp)}));
    <* ASSERT P.Equal(p, cp) *>
  END TestCharPolynomial;


PROCEDURE TestEigenSystem (): BOOLEAN =
  <* UNUSED *>
  CONST
    ftn = Module & "TestEigenSystem";
  VAR result := TRUE;
  BEGIN
    NewLine();
    RunTestA();
    NewLine();
    TestCharPolynomial();
    RETURN result;
  END TestEigenSystem;


BEGIN
END TestEigenSystem.
