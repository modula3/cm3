GENERIC MODULE FunctionalDeriv2(R, V, M);
(* Copyright (c) 1996, m3na project *)

IMPORT NADefinitions AS NA;

<*UNUSED*>
CONST Module = "FunctionalDeriv2.";
(*==========================*)

PROCEDURE Add (READONLY x, y: T): T RAISES {NA.Error} =
  BEGIN
    RETURN T{zeroth := x.zeroth + y.zeroth, first :=
             V.Add(x.first, y.first), second := M.Add(x.second, y.second)};
  END Add;

PROCEDURE Scale (READONLY x: T; y: R.T): T =
  BEGIN
    RETURN T{zeroth := x.zeroth * y, first := V.Scale(x.first, y),
             second := M.Scale(x.second, y)};
  END Scale;

(*==========================*)
BEGIN
END FunctionalDeriv2.
