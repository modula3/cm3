GENERIC MODULE Polynomial(R);
(*Copyright (c) 1995, Harry George*)

<*UNUSED*>
CONST Module = "Polynomial.";

(*--------------------*)
PROCEDURE New (n: CARDINAL): T =
  BEGIN
    RETURN NEW(T, n + 1);
  END New;

(*==========================*)
BEGIN
  Zero := NEW(T, 1);
  Zero[0] := R.Zero;
  One := NEW(T, 1);
  One[0] := R.One;
END Polynomial.
