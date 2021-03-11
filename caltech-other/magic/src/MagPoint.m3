(* $Id$ *)

MODULE MagPoint;
IMPORT Word;
IMPORT Fmt;

PROCEDURE Shift(READONLY a, b : T) : T =
  BEGIN  RETURN T { a.x + b.x, a.y + b.y } END Shift;

PROCEDURE Hash(a : T) : Word.T = BEGIN RETURN Word.Plus(a.x,a.y) END Hash;

PROCEDURE Equal(a, b : T) : BOOLEAN = 
  BEGIN RETURN a.x = b.x AND a.y = b.y END Equal;

PROCEDURE Max(a, b : T) : T =
  VAR res := T { x := MAX(a.x, b.x), y := MAX(a.y,b.y) }; BEGIN 
    RETURN res 
  END Max;

PROCEDURE Min(a, b : T) : T =
  VAR res := T { x := MIN(a.x, b.x), y := MIN(a.y,b.y) }; BEGIN 
    RETURN res 
  END Min;

PROCEDURE Order( a, b : T) : [-1..1] = 
  BEGIN
    IF Equal(a,b) THEN RETURN 0 
    ELSIF a.x <= b.x AND a.y <= b.y THEN RETURN -1
    ELSIF a.x >= b.x AND a.y >= b.y THEN RETURN 1
    ELSE RETURN 0
    END
  END Order;

PROCEDURE Format(READONLY a : T) : TEXT =
  BEGIN RETURN "(" & Fmt.Int(a.x) & "," & Fmt.Int(a.y) & ")" END Format;

BEGIN END MagPoint.
