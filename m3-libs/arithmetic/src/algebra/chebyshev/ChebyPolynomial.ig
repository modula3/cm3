GENERIC INTERFACE ChebyPolynomial(P, CP);
(*Copyright (c) 1995, Harry George

   Abstract: Chebyshev polynomials and approximation of functions by sums
   of chebyshev polynomials *)

CONST Brand = "Cheby" & P.Brand;

TYPE
  TBody = P.TBody;
  T = P.T;
  Ftn = CP.Ftn;

CONST
  New  = P.New;
  Copy = P.Copy;

  IsZero = P.IsZero;
  Equal  = P.Equal;

  Add   = P.Add;
  Sub   = P.Sub;
  Neg   = P.Neg;
  Scale = P.Scale;

  Expand       = CP.Expand;
  FindUpperExp = CP.FindUpperExp;
  Abort        = CP.Abort;
  Eval         = CP.Eval;
  Derive       = CP.Derive;
  Integrate    = CP.Integrate;

(*========================*)
END ChebyPolynomial.
