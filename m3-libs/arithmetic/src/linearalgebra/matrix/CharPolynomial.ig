GENERIC INTERFACE CharPolynomial(Rt, M);
(* Arithmetic for Modula-3, see doc for details

   Abstract: Characteristic polynomial of a matrix *)


PROCEDURE CharPolynomial (x: M.T; ): Rt.T;
(* return the characteristic polynomial of the square matrix x needs about
   n^4 operations but works on rings, too*)

PROCEDURE CompanionMatrix (x: Rt.T; ): M.T;
(* return a matrix which has x as characteristic polynomial, the leading
   coefficient must be one*)

END CharPolynomial.
