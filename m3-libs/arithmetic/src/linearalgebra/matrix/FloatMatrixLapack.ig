GENERIC INTERFACE FloatMatrixLapack(R, V, CV, M);
(*Copyright (c) 1996, m3na project

   Abstract: High level interfaces to LAPACK

   These wrapper are prefered over the raw interfaces.

   *)

FROM NADefinitions IMPORT Error;

(*==========================*)

TYPE
  EVGenFlag = {schurVectors};
  EVGenFlagSet = SET OF EVGenFlag;

  EV = RECORD
         eigenvalues: CV.T;
         upperTri   : M.T;
         schur      : M.T;       (*initalized if schurVector flag is set*)
       END;

PROCEDURE EigenValuesGen (A: M.T; flags := EVGenFlagSet{}): EV
  RAISES {Error};
(*Compute all eigenvalues of matrix A.  This is a wrapper for SGEES and
   DGEES. *)


TYPE
  LSGenFlag = {transposed};
  LSGenFlagSet = SET OF LSGenFlag;

  LS = RECORD
         x  : V.T;               (*solution vector*)
         res: R.T;               (*square of the minimal distance*)
       END;

PROCEDURE LeastSquaresGen (         A    : M.T;
                           READONLY B    : ARRAY OF V.T;
                                    flags                 := LSGenFlagSet{}):
  REF ARRAY OF LS RAISES {Error};
(*For each j compute x with minimal norm ||A*x-B[j]||.  This is a wrapper
   for SGELS and DGELS. *)

TYPE
  MachParam = {eps, sfmin, base, epsbase, t, rnd, emin, rmin, emax, rmax};

PROCEDURE GetMachineParameter (param: MachParam): R.T;

(*==========================*)
END FloatMatrixLapack.
