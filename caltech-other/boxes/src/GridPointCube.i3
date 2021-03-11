(* $Id$ *)

INTERFACE GridPointCube;
IMPORT Word, GridPoint;

TYPE T = RECORD ll, ur : GridPoint.T END;
(* A GridPointCube.T represents a coordinate-aligned 3D-box, up to and 
   INCLUDING ll and ur *)

CONST Brand = "GridPointCube";

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN;
PROCEDURE Hash(READONLY a : T) : Word.T;

PROCEDURE Inside(READONLY box : T; READONLY p : GridPoint.T) : BOOLEAN;
(* is p contained in box? *)

END GridPointCube.
