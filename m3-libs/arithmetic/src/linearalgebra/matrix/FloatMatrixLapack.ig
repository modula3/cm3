GENERIC INTERFACE FloatMatrixLapack(R, CV, M);
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

(*wrapper for SGEES and DGEES *)
PROCEDURE EigenValuesGen (A: M.T; flags := EVGenFlagSet{}): EV
  RAISES {Error};


TYPE
  MachParam = {eps, sfmin, base, epsbase, t, rnd, emin, rmin, emax, rmax};

PROCEDURE GetMachineParameter (param: MachParam): R.T;

(*==========================*)
END FloatMatrixLapack.
