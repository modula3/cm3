GENERIC MODULE FunctionalDeriv2(R, V, M);
(* Arithmetic for Modula-3, see doc for details *)

<* UNUSED *>
CONST
  Module = "FunctionalDeriv2.";
(*==========================*)

PROCEDURE Add (READONLY x, y: T): T =
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
