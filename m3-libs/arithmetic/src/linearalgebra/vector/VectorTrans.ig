GENERIC INTERFACE VectorTrans(R, V);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Vector math *)


TYPE T = V.T;

PROCEDURE Norm1 (x: T; ): R.T;   (* Sum norm *)
PROCEDURE Norm2 (x: T; ): R.T;   (* Euclidean norm *)
PROCEDURE Norm2Sqr (x: T; ): R.T; (* Square of Euclidean norm *)
PROCEDURE NormInf (x: T; ): R.T; (* Maximum norm *)

END VectorTrans.
