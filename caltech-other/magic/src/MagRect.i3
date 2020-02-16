(* $Id$ *)

INTERFACE MagRect;
IMPORT MagPoint;
IMPORT Word;
IMPORT TextReader; 
IMPORT Wr, Thread;
IMPORT MagDir;

TYPE T = RECORD ll, ur : MagPoint.T END;

CONST
  Brand = "MagRect";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;

PROCEDURE Hash(READONLY a : T): Word.T;

PROCEDURE Union(READONLY a, b : T) : T;

PROCEDURE IsProper(a : T) : BOOLEAN;

(* returns TRUE if they have intersection, FALSE else *) 
(* note that a zero-sized intersection is legal and different from a NULL *) 
(* intersection *)
PROCEDURE Intersection(READONLY a, b : T; VAR intersection : T) : BOOLEAN;

PROCEDURE IntersectionOrAbort(READONLY a, b : T) : T;

EXCEPTION
  ParseError;

PROCEDURE ParseFromReader(reader : TextReader.T; (* OUT *) VAR res : T) RAISES { ParseError };
PROCEDURE Parse(coord : TEXT) : T RAISES { ParseError };

(* check if two MagRect.T's overlap.  *)
(* Two MagRect.T's overlap if they have an Intersection *)
(* (see above) -- even if that Intersection is zero-sized *)
PROCEDURE Overlap(READONLY a, b : T) : BOOLEAN;

(* shift a MagRect.T into the coordinate system rooted at by *)
(* (i.e., the shift is the negative of by.x, by.y) *)
PROCEDURE Shift(READONLY a : T; by : MagPoint.T) : T;

(* build a proper rect from two points *)
PROCEDURE FromPoints(READONLY a, b : MagPoint.T) : T;

(* format for debugging... *)
PROCEDURE Format(READONLY a : T) : TEXT;

(* format for a .mag file... *)
PROCEDURE FormatForMagic(READONLY a : T) : TEXT;
PROCEDURE WriteToWrForMagic(wr : Wr.T; r : T) RAISES { Wr.Failure, Thread.Alerted };

(* Manhattan distance from a to b *)
PROCEDURE Distance(READONLY a, b : T) : CARDINAL;

(* clip a to the frustrum to; returns TRUE if the result is nonzero *)
PROCEDURE Clip(READONLY a, to : T; VAR res : T) : BOOLEAN;

(* a little example rect... *)
CONST OriginSquare = T { MagPoint.T { 0, 0 }, MagPoint.T { 1, 1 } };

PROCEDURE Bloat(READONLY a : T; dir : MagDir.T; by : CARDINAL) : T;
PROCEDURE Bloats(READONLY a : T; dirs : SET OF MagDir.T; by : CARDINAL) : T;

END MagRect.



