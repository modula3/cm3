(* Copyright (c) 2000, 2008 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
GENERIC INTERFACE LU2(M);
FROM Matrix IMPORT Singular;

PROCEDURE BackSubstitute(READONLY m : M.M; 
                         READONLY indx : REF ARRAY OF INTEGER; 
                         VAR b : M.V);

PROCEDURE BackSubstitute2(READONLY m : M.M; 
                         READONLY indx : REF ARRAY OF INTEGER; 
                         VAR b : M.V;
                         VAR bout : M.V);
  
PROCEDURE DecomposeR(VAR m : M.M; 
                     VAR vv : M.V;
                     indx : REF ARRAY OF INTEGER; 
                     VAR d : M.Base) RAISES { Singular };
  (* non-allocating version of Decompose.  Call with vv as 
     scratch space with NUMBER(m^) members *)

END LU2.
