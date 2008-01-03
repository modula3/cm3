GENERIC INTERFACE FloatMatrixLapack(R, V, M, MD, Eig);
(* Arithmetic for Modula-3, see doc for details

   Abstract: High level interfaces to LAPACK

   These wrapper should be prefered to the raw interfaces. *)

FROM Arithmetic IMPORT Error;



TYPE
  EVFlag = Eig.EVFlag;
  EVFlagSet = Eig.EVFlagSet;
  EV = Eig.EV;


PROCEDURE EigenValues (A: M.T; flags := EVFlagSet{}; ): EV RAISES {Error};
(* Compute all eigenvalues of matrix A.  This is a wrapper for SGEES and
   DGEES. *)


TYPE
  LSFlag = MD.LSFlag;
  LSFlagSet = MD.LSFlagSet;
  LS = MD.LS;

PROCEDURE LeastSquares
  (A: M.T; READONLY B: ARRAY OF V.T; flags := LSFlagSet{}; ):
  REF ARRAY OF LS RAISES {Error};
(* For each j compute x with minimal norm ||A*x-B[j]||.  This is a wrapper
   for SGELS and DGELS. *)

TYPE
  MachParam = {Eps, SFMin, Base, EpsBase, T, Rnd, EMin, RMin, EMax, RMax};

PROCEDURE GetMachineParameter (param: MachParam; ): R.T;


END FloatMatrixLapack.
