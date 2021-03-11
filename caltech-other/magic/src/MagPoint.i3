(* $Id$ *)

INTERFACE MagPoint;
IMPORT Word;
IMPORT MagCoord AS Coord;
IMPORT MagDir;

TYPE
  T = RECORD x, y : Coord.T; END;

CONST
  Brand = "MagPoint";

PROCEDURE Equal(a, b : T) : BOOLEAN;
 
PROCEDURE Hash(a : T) : Word.T;

(* new bbox coords *)
PROCEDURE Max(a, b : T) : T;

PROCEDURE Min(a, b : T) : T;

(* return 0 if a = b, -1 if a < b, 0 if unordered, 1 if a > b *)
(* Order DOES NOT establish a total order. *)
(* or does it? *)
PROCEDURE Order(a, b : T) : [-1..1];

PROCEDURE Format(READONLY a : T) : TEXT;

PROCEDURE Shift(READONLY a, by : T) : T;

CONST DirDelta  = ARRAY MagDir.T OF T 
                        { T {  0,  1 }   (* N *),
                          T {  1,  0 }   (* E *),
                          T { -1,  0 }   (* W *),
                          T {  0, -1 }   (* S *) };

END MagPoint.
