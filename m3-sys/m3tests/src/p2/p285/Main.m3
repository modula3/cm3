(* This used to work.
 * PackedVars work has it not currently compiling.
 *)
MODULE Main;
IMPORT RTIO;

TYPE Point = ARRAY [0..0] OF INTEGER;
     (* Point = RECORD a: INTEGER END; this works *)
     PointsArray = ARRAY [0..0] OF Point;

PROCEDURE F2(): Point =
BEGIN
  RETURN Point{123};
END F2;

PROCEDURE F1(): PointsArray =
BEGIN
  RETURN PointsArray{F2()};
END F1;

BEGIN
 RTIO.PutInt(F1()[0][0]);
 RTIO.Flush();
END Main.
