INTERFACE Interpolation;
(* author: thielema *)

TYPE T = {Constant, Linear, Cubic};

CONST NumOfValues = ARRAY T OF CARDINAL{1, 2, 4};

PROCEDURE Linear (READONLY x: ARRAY [0 .. 1] OF LONGREAL; t: LONGREAL; ):
  LONGREAL;

PROCEDURE Cubic (READONLY x: ARRAY [-1 .. 2] OF LONGREAL; t: LONGREAL; ):
  LONGREAL;

END Interpolation.
