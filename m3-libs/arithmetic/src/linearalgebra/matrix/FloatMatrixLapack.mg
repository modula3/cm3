GENERIC MODULE FloatMatrixLapack(R, C, V, VR, CV, M, LA);
(*Arithmetic for Modula-3, see doc for details

   Abstract: <describe> *)

FROM NADefinitions IMPORT Error, Err;

<*UNUSED*>
CONST Module = "FloatMatrixLapack.";
(*==========================*)

PROCEDURE EigenValues (A: M.T; flags := EVFlagSet{}): EV RAISES {Error} =
  VAR
    eigRe            := V.New(NUMBER(A^));
    eigIm            := V.New(NUMBER(A^));
    job    : CHAR;
    sdim   : INTEGER;
    work             := V.New(MAX(1, 3 * NUMBER(A^)));
    bwork            := NEW(REF ARRAY OF BOOLEAN, NUMBER(A^));
    success: INTEGER;
    result : EV;
  BEGIN
    IF NUMBER(A^) # NUMBER(A[0]) THEN RAISE Error(Err.bad_size); END;

    result.eigenvalues := CV.New(NUMBER(A^));

    IF EVFlag.schurVectors IN flags THEN
      job := 'V';
      result.schur := M.New(NUMBER(A^), NUMBER(A[0]));
    ELSE
      job := 'N';
      result.schur := M.New(1, 1);
    END;

    A := M.Transpose(A);
    LA.GEES(job, 'N', NIL, NUMBER(A^), A[0, 0], NUMBER(A^), sdim, eigRe[0],
            eigIm[0], result.schur[0, 0], NUMBER(result.schur^), work[0],
            NUMBER(work^), bwork[0], success);

    (*A finer error analysis is possible!  In some cases partial results
       should be returned*)
    IF success < 0 THEN
      RAISE Error(Err.b1_too_small); (*nonsense :-)*)
    ELSIF success > 0 THEN
      RAISE Error(Err.not_converging);
    END;

    result.upperTri := M.Transpose(A);

    FOR j := 0 TO LAST(eigRe^) DO
      result.eigenvalues[j] := C.T{eigRe[j], eigIm[j]};
    END;

    RETURN result;
  END EigenValues;

PROCEDURE LeastSquares (A: M.T; READONLY B: ARRAY OF V.T; flags: LSFlagSet):
  REF ARRAY OF LS RAISES {Error} =
  VAR
    ls    := NEW(REF ARRAY OF LS, NUMBER(B));
    minmn := MIN(NUMBER(A^), NUMBER(A[0]));
    maxmn := MAX(NUMBER(A^), NUMBER(A[0]));
    X     := M.New(NUMBER(B), maxmn);
    Atmp := M.Copy(A);           (*The information returned in Atmp is too
                                    much implementation specific and is
                                    ignored for now until one finds a
                                    sophisticated way of utilizing it*)
    bsize  : CARDINAL;           (*size of a target vector from 'b'*)
    xsize  : CARDINAL;           (*size of a solution vector*)
    success: INTEGER;
    trans  : CHAR;
    work := NEW(REF ARRAY OF R.T, MAX(1, minmn + MAX(minmn, NUMBER(B))));
  BEGIN
    IF LSFlag.transposed IN flags THEN
      bsize := NUMBER(A[0]);
      xsize := NUMBER(A^);
      trans := 'N';              (*we have to think inverse because FORTRAN
                                    always considers the matrices to be
                                    transposed*)
    ELSE
      bsize := NUMBER(A^);
      xsize := NUMBER(A[0]);
      trans := 'T';
    END;

    FOR j := 0 TO LAST(X^) DO
      IF NUMBER(X[j]) # bsize THEN RAISE Error(Err.bad_size); END;
      SUBARRAY(X[j], 0, bsize) := B[j]^;
    END;

    LA.GELS(trans, NUMBER(Atmp[0]), NUMBER(Atmp^), NUMBER(X^), Atmp[0, 0],
            NUMBER(Atmp[0]), X[0, 0], NUMBER(X[0]), work[0], NUMBER(work^),
            success);

    IF success < 0 THEN
      RAISE Error(Err.not_converging); (*nonsense :-)*)
    END;

    FOR j := 0 TO LAST(X^) DO
      ls[j].x := V.FromArray(SUBARRAY(X[j], 0, xsize));
      ls[j].res := VR.Inner(SUBARRAY(X[j], xsize, maxmn - xsize),
                            SUBARRAY(X[j], xsize, maxmn - xsize));
    END;

    RETURN ls;
  END LeastSquares;

PROCEDURE GetMachineParameter (param: MachParam): R.T =
  CONST
    chars = ARRAY MachParam OF
              CHAR{'E', 'S', 'B', 'P', 'N', 'R', 'M', 'U', 'L', 'O'};
  BEGIN
    RETURN LA.LAMCH(chars[param]);
  END GetMachineParameter;


(*==========================*)
BEGIN
END FloatMatrixLapack.
