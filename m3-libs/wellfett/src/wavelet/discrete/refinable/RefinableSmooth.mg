GENERIC MODULE RefinableSmooth(R, RT, C, CT, V, VS, VT, CVT, M, Eigen, S,
                               Refn, BSpl);

IMPORT Arithmetic;
IMPORT IntBiList;


PROCEDURE SobolevNonSmooth (x: S.T; ): R.T RAISES {Arithmetic.Error} =
  BEGIN
    RETURN R.Half * (-R.One - RT.Lb(SpectralRadius(x.autocorrelate())));
  END SobolevNonSmooth;

PROCEDURE Eigenvalues (mask: S.T; ): Eigen.EV RAISES {Arithmetic.Error} =
  BEGIN
    RETURN Eigen.EigenValues(Refn.TransitionMatrix(mask));
  END Eigenvalues;

PROCEDURE SpectralRadius (mask: S.T; ): R.T RAISES {Arithmetic.Error} =
  BEGIN
    (*
    IO.Put("TransitionSpecRad "&Fmt.Int(ncall)&"\n");
    INC(ncall);
    *)
    RETURN CVT.NormInf(Eigenvalues(mask).eigenvalues);
  END SpectralRadius;


TYPE
  MatrixElem = RECORD
                 enX, enY: IntBiList.Node;
                 value   : R.T;
               END;

PROCEDURE FindMinMatrix
  (READONLY mat: M.TBody; enabledX, enabledY: IntBiList.T; ): MatrixElem =
  VAR
    result  : MatrixElem;
    enX, enY: IntBiList.Node;
  BEGIN
    result.value := R.PosInf;
    enX := enabledX.getlo();
    WHILE enX # NIL DO
      enY := enabledY.getlo();
      WHILE enY # NIL DO
        IF result.value
             > mat[enabledX.getvalue(enX), enabledY.getvalue(enY)] THEN
          result.value :=
            mat[enabledX.getvalue(enX), enabledY.getvalue(enY)];
          result.enX := enX;
          result.enY := enY;
        END;
        enY := enabledY.getnext(enY);
      END;
      enX := enabledX.getnext(enX);
    END;
    RETURN result;
  END FindMinMatrix;

(* Compute a kind of distance of a given eigenspectrum to the one of the
   transfer matrix of the B-Spline of corresponding order *)
PROCEDURE EigenDistBSpline (specX: REF ARRAY OF C.T; ): R.T =
  VAR
    enabledX, enabledY := NEW(IntBiList.T).init();
    specY              := NEW(V.T, NUMBER(specX^));
    distMatrix         := NEW(M.T, NUMBER(specX^), NUMBER(specX^));
  BEGIN
    VAR eigY := R.One;
    BEGIN
      FOR i := FIRST(specX^) TO LAST(specX^) DO
        EVAL enabledX.addhi(i);
        EVAL enabledY.addhi(i);
        eigY := eigY / R.Two;
        specY[i] := eigY;
      END;
      specY[LAST(specX^)] := R.Two * eigY;
    END;
    (*
    IO.Put(Fmt.FN("compute distance between spectra\n%s%s\n",
                  ARRAY OF TEXT{CVF.Fmt(specX), VF.Fmt(specY)}));
    *)
    FOR i := FIRST(specX^) TO LAST(specX^) DO
      FOR j := FIRST(specX^) TO LAST(specX^) DO
        distMatrix[i, j] :=
          CT.AbsSqr(C.T{specX[i].re - specY[j], specX[i].im});
      END;
    END;
    VAR sum := R.Zero;
    BEGIN
      FOR i := FIRST(specX^) TO LAST(specX^) DO
        WITH min = FindMinMatrix(distMatrix^, enabledX, enabledY) DO
          sum := sum + min.value;
          enabledX.rem(min.enX);
          enabledY.rem(min.enY);
        END;
      END;
      RETURN sum;
    END;
  END EigenDistBSpline;


CONST
  Three  = FLOAT(3.0, R.T);
  Six    = FLOAT(6.0, R.T);
  Twelve = FLOAT(12.0, R.T);

PROCEDURE ComputeSSE (READONLY y: ARRAY [0 .. 2] OF R.T; ): R.T =
  VAR
    p1  := VS.Sum(y);
    p2  := VS.Inner(y, y);
    p12 := p1 * p1;
    dif := Three * p2 - p12;
  BEGIN
    RETURN dif * dif + R.Two * p12 * p12;
  END ComputeSSE;

PROCEDURE ComputeDSSE (READONLY y: ARRAY [0 .. 2] OF R.T; ): V.T =
  VAR
    p1  := VS.Sum(y);
    p2  := VS.Inner(y, y);
    p12 := p1 * p1;
    p1d := Twelve * (p1 * (p12 - p2));
    p2d := Six * (Three * p2 - p12)
             * R.Two (* because p2' contains 2a'a and so on *);
    z := V.New(NUMBER(y));
  BEGIN
    FOR i := 0 TO LAST(y) DO z[i] := p1d + y[i] * p2d; END;
    RETURN z;
  END ComputeDSSE;

PROCEDURE ComputeDDSSE (READONLY y: ARRAY [0 .. 2] OF R.T; ): M.T =
  (* derived with mathematica *)
  VAR
    p1 := VS.Sum(y);
    p2 := VS.Inner(y, y);

    z := M.New(NUMBER(y), NUMBER(y));
  BEGIN
    FOR i := 0 TO LAST(y) DO
      FOR j := i TO LAST(y) DO
        z[i, j] := (p1 + y[i] - y[j]) * (p1 - y[i] + y[j]);
        z[j, i] := z[i, j];
      END;
      z[i, i] := z[i, i] + (Three * y[i] - R.Two * p1) * y[i] + p2;
    END;
    RETURN M.Scale(z, R.Two * Twelve);
  END ComputeDDSSE;

PROCEDURE SquareSmoothEstimate (x: S.T; ): R.T =
  VAR hsums := x.wrapCyclic(3);

  BEGIN
    (*
    IO.Put(Fmt.FN("Compute SSE of %s, sum %s\n",ARRAY OF TEXT{VF.Fmt(hsums),RF.Fmt(V.Sum(hsums^))}));
    *)
    RETURN ComputeSSE(hsums^);
  END SquareSmoothEstimate;

PROCEDURE BSpline (x: S.T; ): R.T RAISES {Arithmetic.Error} =
  BEGIN
    RETURN EigenDistBSpline(Eigenvalues(x).eigenvalues);
  END BSpline;

PROCEDURE Binomial (x: S.T; ): R.T =
  VAR
    bsplinemask := BSpl.GeneratorMask(x.getNumber() - 1).translate(
                     x.getFirst());
  BEGIN
    (*
    IO.Put(Fmt.FN("%s-%s, %s-%s\n",
                  ARRAY OF
                    TEXT{Fmt.Int(hprimal.getFirst()),
                         Fmt.Int(hprimal.getLast()),
                         Fmt.Int(bsplinemask.getFirst()),
                         Fmt.Int(bsplinemask.getLast())}));
    *)
    RETURN -ABS(bsplinemask.inner(x));
  END Binomial;

PROCEDURE Frobenius (x: S.T; ): R.T =
  VAR
    hh              := x.autocorrelate();
    frob            := R.Zero;
    alter: [0 .. 1] := 1;
    n               := hh.getLast();
  BEGIN
    (* because of the symmetry we can content ourselves with the half
       autocorrelated mask *)
    FOR i := hh.getFirst() TO -1 DO
      WITH val = hh.getValue(i) DO
        frob := frob + FLOAT(n + alter, R.T) * val * val;
        alter := 1 - alter;
      END;
    END;
    WITH val = hh.getValue(0) DO
      frob := R.Two * frob + FLOAT(n + alter, R.T) * val * val;
    END;
    (*
    IO.Put(Fmt.FN("Frobenius norm %s =?= %s\n",
                  ARRAY OF TEXT{RF.Fmt(frob0), RF.Fmt(frob)}));
    *)
    RETURN frob;
  END Frobenius;

PROCEDURE SimplifiedFrobenius (x: S.T; ): R.T =
  VAR hh := x.autocorrelate();
  BEGIN
    (* the computation could be sped-up because 'hh' is symmetric *)
    RETURN hh.inner(hh);
  END SimplifiedFrobenius;

PROCEDURE SumNorm (x: S.T; ): R.T =
  VAR hh := x.autocorrelate();
  BEGIN
    (* the computation could be sped-up because 'hh' is symmetric *)
    RETURN VT.Norm1(hh.getData());
  END SumNorm;

BEGIN
END RefinableSmooth.
