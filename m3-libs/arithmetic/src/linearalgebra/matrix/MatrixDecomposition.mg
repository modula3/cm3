GENERIC MODULE MatrixDecomposition(R, RT, V, M);
(*Arithmetic for Modula-3, see doc for details *)

IMPORT Arithmetic AS Arith;
FROM RT IMPORT Tiny, Eps;

CONST Module = "MatrixDecomposition";

PROCEDURE AssertSquareForm (READONLY x: M.TBody; ) =
  BEGIN
    <* ASSERT NUMBER(x) = NUMBER(x[0]), "Matrix must have square form." *>
  END AssertSquareForm;


(*==========================*)
(* Triangluar Matrices *)
(*==========================*)
(**
A triangular matrix A is of the form:
| a11 a12 a13 a14
| 0   a22 a23 a24
| 0   0   a33 a34
| 0   0   0   a44

A x = b can be solved for b by back substitution
*)
PROCEDURE BackSubst (A: M.T; x, b: V.T) RAISES {Arith.Error} =

  VAR
    m  := NUMBER(A^);
    m1 := FIRST(A^);
    mm := LAST(A^);
    n  := NUMBER(A[0]);

  BEGIN
    <* ASSERT m = n AND NUMBER(x^) = n AND NUMBER(b^) = n,
                "Matrix and vector sizes must match." *>

    FOR row := mm TO m1 BY -1 DO
      IF ABS(b[row]) < Tiny THEN
        RAISE Arith.Error(NEW(Arith.ErrorDivisionByZero).init());
      END;
      WITH tmp = b[row] DO
        FOR col := row + 1 TO mm DO tmp := tmp - A[row, col] * x[col]; END;
        x[row] := tmp / A[row, row];
      END;
    END;
  END BackSubst;

(* Tridiagonal Matrices *)
(*------------------------*)
PROCEDURE HouseHolderD (A: M.T) = (*nxn*)
  (*Convert A to tridiagonal form (destroying original A)*)
  VAR
    n                                 := NUMBER(A^);
    n1                                := FIRST(A^);
    nn                                := LAST(A^);
    u                                 := NEW(V.T, n);
    t                                 := NEW(M.T, n, n);
    sum, rootsum, w, h, uau, b23: R.T;

  BEGIN
    AssertSquareForm(A^);

    FOR row := n1 TO nn - 2 DO
      sum := R.Zero;
      FOR i := n1 TO nn DO
        u[i] := R.Zero;
        IF i > row + 1 THEN u[i] := A[i, row]; END;
        IF i > row THEN sum := sum + A[i, row] * A[i, row]; END;
      END;
      w := R.One;
      IF A[row + 1, row] < R.Zero THEN w := -R.One; END;
      rootsum := RT.SqRt(sum);
      h := sum + ABS(A[row + 1, row]) * rootsum;
      u[row + 1] := A[row + 1, row] + rootsum * w;
      uau := R.Zero;
      FOR i := n1 TO nn DO
        FOR j := n1 TO nn DO
          uau := uau + u[i] * A[i, j] * u[j];
          IF (i <= row) AND (j <= row) THEN
            t[i, j] := A[i, j];
          ELSIF (j = row) AND (i >= row + 2) THEN
            t[i, j] := R.Zero;
          ELSE
            b23 := R.Zero;
            FOR k := n1 TO nn DO
              b23 := b23 - (u[i] * A[k, j] + A[i, k] * u[j]) * u[k];
            END;
            t[i, j] := A[i, j] + b23 / h;
          END;
        END;                     (*for j*)
      END;                       (*for i*)
      uau := uau / h / h;
      FOR i := n1 TO nn DO
        FOR j := n1 TO nn DO
          A[i, j] := t[i, j] + uau * u[i] * u[j];
          IF ABS(A[i, j]) < Eps THEN A[i, j] := R.Zero; END;
        END;
      END;
    END;                         (*for row*)
  END HouseHolderD;

(*---------------------*)
PROCEDURE SplitTridiagonal (A: M.T; ): Tridiagonals =

  VAR
    n  := NUMBER(A^);
    n1 := FIRST(A^);
    nn := LAST(A^);
    a  := NEW(V.T, n);
    b  := NEW(V.T, n);
    c  := NEW(V.T, n);

  BEGIN
    AssertSquareForm(A^);

    a[n1] := R.Zero;
    b[n1] := A[n1, n1];
    c[n1] := A[n1, n1 + 1];
    FOR i := n1 + 1 TO nn - 1 DO
      a[i] := A[i, i - 1];
      b[i] := A[i, i];
      c[i] := A[i, i + 1];
    END;
    a[nn] := A[nn, nn - 1];
    b[nn] := A[nn, nn];
    c[nn] := R.Zero;

    RETURN Tridiagonals{a, b, c};
  END SplitTridiagonal;
(*-----------------------*)
PROCEDURE SolveTridiagonal (t: Tridiagonals; r: V.T; VAR u: V.T)
  RAISES {Arith.Error} =
  (**Given tridiagonal matrix A, with diagonals a,b,c:
  |  b1 c1  0    ...
  |  a2 b2 c2    ...
  |   0 a3 b3 c3 ...
  |              ...
  |                 aN-1 bN-1 cN-1
  |                  0   aN   bN
  |  Solve for u in A*u=r
  *)
  <* UNUSED *>
  CONST
    ftn = Module & "SolveTriDiag";
  VAR
    den: R.T;
    a        := t.a;
    b        := t.b;
    c        := t.c;
    n        := NUMBER(r^);
    n1       := FIRST(r^);
    nn       := LAST(r^);
    d        := NEW(V.T, n);

  BEGIN
    (*---check preconditions---*)
    <* ASSERT NUMBER(a^) = n AND NUMBER(b^) = n AND NUMBER(c^) = n,
                "Diagonals must have the same size." *>
    IF ABS(b[n1]) < Tiny THEN
      RAISE Arith.Error(NEW(Arith.ErrorAlmostZero).init());
    END;

    (*---first row---*)
    den := b[n1];
    u[n1] := r[n1] / den;
    d[n1] := c[n1] / den;

    (*---work forward---*)
    FOR i := n1 + 1 TO nn - 1 DO
      den := b[i] - a[i] * d[i - 1];
      IF ABS(den) < Tiny THEN
        RAISE Arith.Error(NEW(Arith.ErrorDivisionByZero).init());
      END;
      u[i] := (r[i] - a[i] * u[i - 1]) / den;
      d[i] := c[i] / den;
    END;

    (*---last row---*)
    den := b[nn] - a[nn] * d[nn - 1];
    u[nn] := (r[nn] - a[nn] * u[nn - 1]) / den;

    (*---work backward---*)
    FOR i := nn - 1 TO n1 BY -1 DO u[i] := u[i] - d[i] * u[i + 1]; END;
  END SolveTridiagonal;

(* nxn Matrices *)
(**A general nxn real matrix A is of the form
| a11 a12 a13
| a21 a22 a23
| a31 a32 a33

A x = b can be solved for x by Gaussian Elimination and
backsubstitution
*)
(**
PROCEDURE GaussElim(A:  M.T;
                    x,b:V.T;
                    pivot:BOOLEAN:=TRUE
                    ) RAISES {Arith.Error}=
(*Generally, we need to pivot to assure division by the largest
coeff.  However, sometimes we already know the matrix is in
the correct form and can avoid pivoting.  In that case, set
pivot:=FALSE
*)
VAR
  m:=NUMBER(A^);    m1:=FIRST(A^);   mm:=LAST(A^);
  n:=NUMBER(A[0]);  n1:=FIRST(A[0]); nn:=LAST(A[0]);
  tmp:R.T;
  pndx:CARDINAL;
BEGIN
<*ASSERT m=n AND NUMBER(x^)=n AND NUMBER(b^)=n, "Matrix and vector sizes must match."*>

  FOR row:=n1 TO nn-1 DO
    IF pivot THEN
      (*---look for max scale---*)
      rmax:=row; max:=ABS(x[row]/A[row,row]);
      FOR col:=row TO nn DO
        IF ABS(A[row,col]) >  max THEN
          rmax:=col; max:=ABS(x[row]/A[row,col]);
        END;
      END;
      (*---pivot---*)
      tmprow^:=A[

END GaussElim;
*)

(* Non-destructive LU factoring *)

(* This routine recycles the results of LUFactorD in order to avoid writing
   a separate routine with its own bugs :-) *)
PROCEDURE LUFactor (Aorig: M.T; ): LUFactors RAISES {Arith.Error} =
  <* UNUSED *>
  CONST
    ftn = "LUFactor";

  VAR
    A                := M.Copy(Aorig);
    index            := NEW(REF IndexArray, NUMBER(A^));
    sign : [-1 .. 1];
    L, U             := M.NewZero(NUMBER(A^), NUMBER(A[0]));
  BEGIN
    LUFactorD(A^, index^, sign);
    FOR j := FIRST(A^) TO LAST(A^) DO
      SUBARRAY(L[j], 0, j) := SUBARRAY(A[j], 0, j);
      L[j, j] := R.One;
      WITH l = NUMBER(U[j]) - j DO
        SUBARRAY(U[j], j, l) := SUBARRAY(A[j], j, l);
      END;
    END;
    RETURN LUFactors{L, U, index, sign};
  END LUFactor;

PROCEDURE LUBackSubst (LU: LUFactors; b: V.T; ): V.T =
  <* UNUSED *>
  CONST
    ftn = "LUBackSubst";
  VAR B := V.Copy(b);
  BEGIN
    LUBackSubstSep(LU.L^, LU.U^, B^, LU.index^);
    RETURN B;
  END LUBackSubst;

PROCEDURE LUInverse (LU: LUFactors; ): M.T =
  <* UNUSED *>
  CONST
    ftn = "LUInverse";
  VAR
    n         := NUMBER(LU.index^);
    unit      := V.NewZero(n);
    B   : M.T;

  BEGIN
    <* ASSERT n = NUMBER(LU.L^) AND n = NUMBER(LU.L[0])
                AND n = NUMBER(LU.U^) AND n = NUMBER(LU.U[0]),
                "Matrix and vector sizes must match." *>

    B := M.New(n, n);
    FOR i := 0 TO n - 1 DO
      unit[i] := R.One;
      B[i] := LUBackSubst(LU, unit)^;
      unit[i] := R.Zero;
    END;
    RETURN M.Transpose(B);
  END LUInverse;

PROCEDURE Inverse (Aorig: M.T; ): M.T RAISES {Arith.Error} =
  <* UNUSED *>
  CONST
    ftn = "LUInverse";
  VAR
    n                     := NUMBER(A^);
    A                     := M.Transpose(Aorig);
    B    : M.T;
    index: REF IndexArray;
    sign : [-1 .. 1];

  BEGIN
    AssertSquareForm(A^);

    B := M.NewOne(n);
    index := NEW(REF IndexArray, n);
    LUFactorD(A^, index^, sign);
    FOR i := 0 TO LAST(B^) DO LUBackSubstD(A^, B[i], index^); END;
    RETURN B;
  END Inverse;

PROCEDURE LUDet (LU: LUFactors; ): R.T =
  <* UNUSED *>
  CONST
    ftn = "LUDet";
  VAR
    (*---set sign due to row switching---*)
    prod := FLOAT(LU.sign, R.T);
    A    := LU.U;
  BEGIN
    (*---could do more checking here to assure LU form---*)

    (*---compute value---*)
    FOR i := 0 TO LAST(A[0]) DO prod := prod * A[i, i]; END (* for *);
    RETURN prod;
  END LUDet;



(* Destructive LU factoring *)
PROCEDURE LUFactorD (VAR A    : M.TBody;
                     VAR index: IndexArray;
                     VAR d    : [-1 .. 1];  ) RAISES {Arith.Error} =
  <* UNUSED *>
  CONST
    ftn = "LUFactorD";
  VAR
    imax                    := 0;
    sum, dum, max, tmp: R.T;
    Af                      := FIRST(A);
    Al                      := LAST(A);
    m1                      := LAST(A); (*num rows*)
    n1                      := LAST(A[0]); (*num cols*)
    n2                      := LAST(index);
    scale                   := NEW(V.T, n1 + 1);
    tmprow                  := NEW(V.T, n1 + 1);

  BEGIN
    <* ASSERT m1 = n1 AND m1 = n2,
                "Matrix and permutation vector sizes must match." *>

    (*---track the row switching parity via d---*)
    d := 1;

    (*---find max for scaling in each row---*)
    FOR i := Af TO Al DO
      max := R.Zero;
      FOR j := Af TO Al DO
        tmp := ABS(A[i, j]);
        IF tmp > max THEN max := tmp; END (* if *);
      END (* for *);
      IF max = R.Zero THEN
        RAISE Arith.Error(NEW(Arith.ErrorDivisionByZero).init());
      ELSE
        scale[i] := R.One / max;
      END (* if *);
    END (* for *);

    (*---loop over columns---*)
    FOR j := Af TO Al DO
      (*---compute beta---*)
      IF j > Af THEN
        FOR i := Af TO j - 1 DO
          sum := A[i, j];
          IF i > Af THEN
            FOR k := Af TO i - 1 DO
              sum := sum - A[i, k] * A[k, j];
            END (* for *);
            A[i, j] := sum;
          END (* if *);
        END (* for *);
      END (* if *);

      (*---compute alpha---*)
      max := R.Zero;
      FOR i := j TO Al DO
        sum := A[i, j];
        IF j > Af THEN
          FOR k := Af TO j - 1 DO
            sum := sum - A[i, k] * A[k, j];
          END (* for *);
          A[i, j] := sum;
        END (* if *);

        (*---is this a better pivot?---*)
        dum := scale[i] * ABS(sum);
        IF dum > max THEN imax := i; max := dum; END (* if *);
      END (* for j to n*);

      (*---exchange rows?---*)
      IF j # imax THEN
        (*swap rows*)
        tmprow^ := A[imax];
        A[imax] := A[j];
        A[j] := tmprow^;
        d := -d;                 (*fix parity*)
        scale[imax] := scale[j]; (*fix scale*)
      END (* if *);

      (*---set the index for this row---*)
      index[j] := imax;

      (*---divide by pivot---*)
      IF j # Al THEN
        IF A[j, j] = R.Zero THEN A[j, j] := Tiny; END (* if *);
        dum := R.One / A[j, j];
        FOR i := j + 1 TO Al DO A[i, j] := A[i, j] * dum; END (* for *);
      END (* if *);
    END (* for next column*);

    (*---last item---*)
    IF A[Al, Al] = R.Zero THEN A[Al, Al] := Tiny; END (* if *);
  END LUFactorD;

(*-----------------*)
PROCEDURE LUBackSubstD (VAR      A    : M.TBody;
                        VAR      B    : V.TBody;
                        READONLY index: IndexArray) =
  BEGIN
    LUBackSubstSep(A, A, B, index);
  END LUBackSubstD;

PROCEDURE LUBackSubstSep (VAR      A, U : M.TBody;
                          VAR      B    : V.TBody;
                          READONLY index: IndexArray) =
  <* UNUSED *>
  CONST
    ftn = "LUBackSubstSep";
  VAR
    Af                             := FIRST(A);
    Al                             := LAST(A);
    m1                             := LAST(A); (*num rows*)
    n1                             := LAST(A[0]); (*num cols*)
    m2                             := LAST(B); (*num rows*)
    ii, ip: [-1 .. LAST(CARDINAL)];
    sum   : R.T;

  BEGIN
    <* ASSERT m1 = n1 AND m1 = m2, "Matrix and vector sizes must match." *>

    (*---find first non-zero---*)
    ii := Af - 1;                (*marker for first non-zero coeff*)
    FOR i := Af TO Al DO
      ip := index[i];
      sum := B[ip];
      B[ip] := B[i];
      IF ii # Af - 1 THEN
        FOR j := ii TO i - 1 DO sum := sum - A[i, j] * B[j]; END (* for *);
      ELSIF NOT sum = R.Zero THEN
        ii := i;
      END (* if *);
      B[i] := sum;
    END (* for *);

    (*---work through on column basis---*)
    FOR i := Al TO Af BY -1 DO
      sum := B[i];
      IF i < Al THEN
        FOR j := i + 1 TO Al DO sum := sum - U[i, j] * B[j]; END (* for *);
      END (* if *);
      B[i] := sum / U[i, i];
    END (* for *);
  END LUBackSubstSep;
(*-----------------*)
PROCEDURE LUInverseD (VAR A: M.TBody; READONLY index: IndexArray): M.T =
  <* UNUSED *>
  CONST
    ftn = "LUInverse";
  VAR
    n      := NUMBER(A);
    B: M.T;

  BEGIN
    AssertSquareForm(A);

    B := M.NewOne(n);

    FOR i := 0 TO LAST(B^) DO LUBackSubstD(A, B[i], index); END (* for *);
    RETURN B;
  END LUInverseD;




PROCEDURE Cholesky (A: M.T): CholeskyResult =
  VAR
    L := M.NewOne(NUMBER(A^));
    D := V.New(NUMBER(A^));
  BEGIN
    FOR i := FIRST(A^) TO LAST(A^) DO
      WITH d = D[i] DO
        d := A[i, i];
        FOR k := FIRST(A^) TO i - 1 DO
          d := d - L[i, k] * L[i, k] * D[k];
        END;
        FOR j := i + 1 TO LAST(A^) DO
          WITH l = L[j, i] DO
            l := A[j, i];
            FOR m := FIRST(A^) TO i - 1 DO
              l := l - L[j, m] * L[i, m] * D[m];
            END;
            l := l / d;
          END;
        END;
      END;
    END;
    RETURN CholeskyResult{L, D};
  END Cholesky;


(* Singular Value Decomposition*)

(*
(*----------------------*)
PROCEDURE SVDGolub(
           A:M.T;         (*mxn matrix*)
           b:V.T;         (*nx1 col matrix for each set of *)
           rhs:CARDINAL;       (*number of right hand sides*)
           matU:BOOLEAN;       (*make U in the decomposition*)
           matV:BOOLEAN;       (*make V in the decomposition*)
           VAR U,V,W:M.T  (*decomposition products*)
           ) RAISES {Arith.Error}=
(*Do SVD via Golub and Reinsch *)
BEGIN

END SVDGolub;

(*------------------------*)
PROCEDURE SVDChan(
           A:M.T;         (*mxn matrix*)
           b:V.T;         (*nx1 col matrix*)
           rhs:CARDINAL;       (*number of right hand sides*)
           matU:BOOLEAN;       (*make U in the decomposition*)
           matV:BOOLEAN;       (*make V in the decomposition*)
           VAR U,V,W:M.T  (*decomposition products*)
           ) RAISES {Arith.Error}=
(*Do SVD via T. Chan's ACM algorithm 581*)
BEGIN
END SVDChan;

(*-----------------------*)
PROCEDURE SVDSolve(U,V,W:M.T; (*decomposition*)
                    b:V.T;     (*rightside*)
                    VAR x:V.T  (*result*)
                   ) RAISES {Arith.Error}=


BEGIN
END SVDSolve;
*)
(*-----------------*)
BEGIN
END MatrixDecomposition.
