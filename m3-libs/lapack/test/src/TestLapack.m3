MODULE TestLapack;

IMPORT Fmt, Wr, Thread;
IMPORT LongRealBasic         AS R,
       LongRealVector        AS V,
       LongRealComplexVector AS CV,
       (*LongRealMatrix AS M,*)
       (*LongRealEigenSystem AS ES,*)
       LongRealMatrixLapack        AS ES,
       LongRealMatrixLapack        AS LA,
       LongRealCharPolynomial      AS MCP,
       LongRealComplexFmtLex       AS CF,
       LongRealVectorFmtLex        AS VF,
       LongRealComplexVectorFmtLex AS CVF,
       LongRealMatrixFmtLex        AS MF,
       LongRealComplexRoot         AS Root;
IMPORT Arithmetic;

IMPORT UnitTest, UnitTestList, UnitTestAtom, UnitTestNumeric;


<* FATAL Arithmetic.Error, Thread.Alerted, Wr.Failure *>


PROCEDURE CheckMachine (SELF: UnitTestAtom.T; ) =
  BEGIN
    FOR j := FIRST(LA.MachParam) TO LAST(LA.MachParam) DO
      (* messages should be restricted to failures of test cases *)
      SELF.message(Fmt.FN("%s: %s\n",
                          ARRAY OF
                            TEXT{Fmt.Int(ORD(j)),
                                 Fmt.LongReal(LA.GetMachineParameter(j))}));
    END;
  END CheckMachine;

PROCEDURE CheckEigensystem (SELF: UnitTestNumeric.T; ) =
  VAR
    poly := V.FromArray(ARRAY OF R.T{-1.0D0, 0.0D0, 0.0D0, 1.0D0});
    (* poly:=V.FromArray(ARRAY OF R.T{1.0D0,3.0D0,3.0D0,1.0D0}) *)
    A := MCP.CompanionMatrix(poly);
    (*
    A:=NEW(REF ARRAY OF ARRAY OF LONGREAL,3,3);
    *)
    ev     : ES.EV;
    newPoly: CV.T;

  BEGIN
    IF FALSE THEN SELF.message(MF.Fmt(A) & "\n"); END;

    ev := ES.EigenValues(A, flags := ES.EVFlagSet{ES.EVFlag.SchurVectors});

    IF FALSE THEN
      SELF.message(MF.Fmt(ev.upperTri) & "\n");
      SELF.message(MF.Fmt(ev.schur) & "\n");

      FOR j := 0 TO LAST(ev.eigenvalues^) DO
        SELF.message(
          Fmt.FN("%s: %s\n",
                 ARRAY OF TEXT{Fmt.Int(j), CF.Fmt(ev.eigenvalues[j])}));
      END;
    END;

    newPoly := Root.FromRoots(ev.eigenvalues^);
    FOR j := 0 TO LAST(newPoly^) DO
      IF NOT (SELF.scalarMatch(newPoly[j].re, poly[j], 1.0D-15)
                AND SELF.scalarMatch(newPoly[j].im, R.Zero, 1.0D-15)) THEN
        SELF.message(Fmt.FN("Polynomials %s and %s differ.",
                            ARRAY OF TEXT{VF.Fmt(poly), CVF.Fmt(newPoly)}));
      END;
    END;
  END CheckEigensystem;


PROCEDURE Test (): UnitTest.T =
  BEGIN
    RETURN
      NEW(UnitTestList.T).init(
        "LAPACK wrappers",
        ARRAY OF
          UnitTest.T{NEW(UnitTestNumeric.T, test := CheckMachine).init(
                       "Machine parameters"),
                     NEW(UnitTestNumeric.T, test := CheckEigensystem).init(
                       "Eigensystem")});
  END Test;

BEGIN
END TestLapack.
