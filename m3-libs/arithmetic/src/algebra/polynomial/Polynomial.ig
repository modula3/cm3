GENERIC INTERFACE Polynomial(R, V, PI);
(*Copyright (c) 1996, m3na project *)

(*==========================*)

CONST Brand = R.Brand & "Polynomial";

TYPE
  (*interpretation is: a[0] + a[1]*xi + a[2]* xi^2...a[n]*xi^n *)
  (*text form is: T4{a0,a1,a2,a3} *)
  TBody = PI.TBody;
  T = PI.T;
  QuotRem = PI.QuotRem;

(**
   It's not possible to obtain a pointer to a constant array.
   We can not turn T from a reference type to an array type,
   because the return type must be compatible to the input types.

CONST
  Zero    =  TBody{R.Zero};
  One     =  TBody{R.One};
**)

VAR
  Zero: T;                       (*CONST*)
  One : T;                       (*CONST*)

PROCEDURE New (degree: CARDINAL): T; (*make a poly for a0..an*)

CONST
  FromArray = V.FromArray;
  Copy      = V.Copy;

  IsZero  = PI.IsZero;
  Equal   = PI.Equal;
  Compare = PI.Compare;

  Add = PI.Add;
  Sub = PI.Sub;
  Neg = V.Neg;

  Scale = V.Scale;

  Mul    = PI.Mul;
  Div    = PI.Div;
  Mod    = PI.Mod;
  DivMod = PI.DivMod;

  Eval           = PI.Eval;
  Derive         = PI.Derive;
  EvalDerivative = PI.EvalDerivative;
  Compose        = PI.Compose;

(*==========================*)
END Polynomial.
