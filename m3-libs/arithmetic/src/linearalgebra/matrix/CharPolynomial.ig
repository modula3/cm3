GENERIC INTERFACE CharPolynomial(Rt,M);
(*Arithmetic for Modula-3, see doc for details

Abstract: Characteristic polynomial of a matrix

*)
FROM Arithmetic IMPORT Error;
(*==========================*)
(*-----------------*)

PROCEDURE CharPolynomial(x:M.T):Rt.T RAISES{Error};
(*return the characteristic polynomial of the matrix x
  needs about n^4 operations but works on rings, too*)

PROCEDURE CompanionMatrix(x:Rt.T):M.T RAISES{Error};
(*return a matrix which has x as characteristic polynomial
  raises Err.indivisible if the leading coefficient is not one*)

(*==========================*)
END CharPolynomial.
