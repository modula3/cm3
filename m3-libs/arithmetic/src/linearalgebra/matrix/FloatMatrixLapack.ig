GENERIC INTERFACE FloatMatrixLapack(R,CV,M);
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

(*
PROCEDURE GetMachineParameter (READONLY cmach: CHAR): R.T;
*)

(*==========================*)
END FloatMatrixLapack.
