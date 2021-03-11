(* $Id$ *)

MODULE GridPointCube;
IMPORT Word, GridPoint;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN =
  BEGIN RETURN a = b END Equal;

PROCEDURE Hash(READONLY a : T) : Word.T = 
  BEGIN RETURN Word.Plus(GridPoint.Hash(a.ll), GridPoint.Hash(a.ur)) END Hash;

PROCEDURE Inside(READONLY box : T; READONLY p : GridPoint.T) : BOOLEAN =
  BEGIN
    RETURN (p.x >= box.ll.x AND p.x <= box.ur.x) AND
           (p.y >= box.ll.y AND p.y <= box.ur.y) AND
           (p.l >= box.ll.l AND p.l <= box.ur.l) 
  END Inside;

BEGIN END GridPointCube.
