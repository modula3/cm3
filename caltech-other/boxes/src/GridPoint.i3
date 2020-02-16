(* $Id$ *)
INTERFACE GridPoint;
IMPORT Word;

TYPE 
  T = RECORD
    x, y : INTEGER; l : Layer; 
  END;

  (* This eventually needs to change to be set at runtime *)
  (* also need to change the cost matrix in Cost.i3 *)
  Layer = [ 0 .. 5 ];

PROCEDURE Equal(READONLY a , b : T) : BOOLEAN;
PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Format(READONLY p : T) : TEXT;
(* format for printing *)

EXCEPTION ParseError;
PROCEDURE Parse(t : TEXT) : T RAISES { ParseError };
(* inverse of Format *)

CONST Brand = "GridPoint";

PROCEDURE ComparX(READONLY a, b : T) : [-1..1];
PROCEDURE ComparY(READONLY a, b : T) : [-1..1];
PROCEDURE ComparL(READONLY a, b : T) : [-1..1];

TYPE Comparer = PROCEDURE(READONLY a, b : T) : [-1..1];

CONST Compare : Comparer = NIL;

END GridPoint.
