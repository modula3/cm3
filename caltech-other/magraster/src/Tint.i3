(* $Id$ *)

INTERFACE Tint;

EXCEPTION ParseError;

TYPE Channel = { R, G, B, alpha };
TYPE TintBase = LONGREAL;
TYPE T = ARRAY Channel OF TintBase;

PROCEDURE Parse(from : TEXT) : T RAISES { ParseError };

PROCEDURE Format(tint : T) : TEXT;

PROCEDURE Gamma(READONLY tint : T; gamma : LONGREAL) : T;

PROCEDURE Add(READONLY a, b : T) : T;

PROCEDURE Scale(READONLY a : T; by : LONGREAL) : T;

CONST Delims = ", \t"; (* delimiters allowed in a tint spec *)
CONST Brand = "Tint";
CONST White = T { 1.0d0, 1.0d0, 1.0d0, 1.0d0 };
CONST Black = T { 0.0d0, 0.0d0, 0.0d0, 0.0d0 };

END Tint.
