GENERIC MODULE FloatMatrixLapack(R, C, V, CV, M, LA);
(*Copyright (c) 1996, m3na project

   Abstract: <describe> *)

FROM NADefinitions IMPORT Error, Err;

<*UNUSED*>
CONST Module = "FloatMatrixLapack.";
(*==========================*)

PROCEDURE EigenValuesGen (A: M.T; flags := EVGenFlagSet{}): EV
  RAISES {Error} =
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

    IF EVGenFlag.schurVectors IN flags THEN
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
  END EigenValuesGen;


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
