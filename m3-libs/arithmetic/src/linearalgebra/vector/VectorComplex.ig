GENERIC INTERFACE VectorComplex(V, CV);
(* Arithmetic for Modula-3, see doc for details *)


TYPE T = RECORD re, im: V.T;  END;

PROCEDURE Split (READONLY x: CV.TBody; ): T;
(* Split a vector of complex numbers into two vectors of real numbers. *)

PROCEDURE Merge (READONLY re, im: V.TBody; ): CV.T;
(* Merge two vectors of real numbers into one vector of complex numbers. *)

END VectorComplex.
