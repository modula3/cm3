(* Copyright (c) 2000 Mika Nystrom.  All Rights Reserved. *)
(* $Id$ *)
INTERFACE LU;
IMPORT Matrix;

PROCEDURE Decompose(m : Matrix.T; 
                    indx : REF ARRAY OF INTEGER; 
                    VAR d : LONGREAL) RAISES { Matrix.Singular };


PROCEDURE BackSubstitute(READONLY m : Matrix.T; 
                           READONLY indx : REF ARRAY OF INTEGER; 
                           b : Matrix.Vector);

PROCEDURE BackSubstituteArray(READONLY m : Matrix.T; 
                           READONLY indx : REF ARRAY OF INTEGER; 
                           VAR b : ARRAY OF LONGREAL);

PROCEDURE DecomposeR(m : Matrix.T; 
                    vv : Matrix.Vector;
                    indx : REF ARRAY OF INTEGER; 
                    VAR d : LONGREAL) RAISES { Matrix.Singular };
  (* non-allocating version of Decompose.  Call with vv as 
     scratch space with NUMBER(m^) members *)


END LU.
