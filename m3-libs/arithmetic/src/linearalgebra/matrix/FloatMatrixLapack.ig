GENERIC INTERFACE FloatMatrixLapack(R, V, CV, M);
(* Arithmetic for Modula-3, see doc for details

   Abstract: High level interfaces to LAPACK

   These wrapper should be prefered to the raw interfaces. *)

FROM Arithmetic IMPORT Error;



TYPE
  EVFlag = {SchurVectors};
  EVFlagSet = SET OF EVFlag;

  EV = RECORD
         eigenvalues: CV.T;
         upperTri   : M.T;
         schur: M.T;             (* initalized if schurVector flag is
                                    set *)
       END;

PROCEDURE EigenValues (A: M.T; flags := EVFlagSet{}; ): EV RAISES {Error};
(* Compute all eigenvalues of matrix A.  This is a wrapper for SGEES and
   DGEES. *)


TYPE
  LSFlag = {Transposed};
  LSFlagSet = SET OF LSFlag;

  LS = RECORD
         x  : V.T;               (* solution vector *)
         res: R.T;               (* square of the minimal distance *)
       END;

PROCEDURE LeastSquares
  (A: M.T; READONLY B: ARRAY OF V.T; flags := LSFlagSet{}; ):
  REF ARRAY OF LS RAISES {Error};
(* For each j compute x with minimal norm ||A*x-B[j]||.  This is a wrapper
   for SGELS and DGELS. *)

TYPE
  MachParam = {Eps, SFMin, Base, EpsBase, T, Rnd, EMin, RMin, EMax, RMax};

PROCEDURE GetMachineParameter (param: MachParam; ): R.T;


END FloatMatrixLapack.
