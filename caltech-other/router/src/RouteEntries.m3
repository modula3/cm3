(* $Id$ *)

MODULE RouteEntries;
FROM EndPointStatus IMPORT Dir, DirName;

PROCEDURE Equal(READONLY a, b : T) : BOOLEAN = 
  BEGIN RETURN a = b END Equal;

PROCEDURE Format(READONLY a : T) : TEXT =
  VAR
    res := "";
  BEGIN
    FOR i := FIRST(Dir) TO LAST(Dir) DO
      IF i IN a THEN res := res & DirName[i] END
    END;
    RETURN res
  END Format;

BEGIN 
END RouteEntries.
