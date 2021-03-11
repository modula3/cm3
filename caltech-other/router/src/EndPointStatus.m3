(* $Id$ *)
MODULE EndPointStatus;
IMPORT GridPoint;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a = b END Equal;

PROCEDURE Unitize(x : INTEGER) : INTEGER =
  BEGIN
    IF x < 0 THEN RETURN -1 ELSIF x > 0 THEN RETURN 1 ELSE RETURN 0 END
  END Unitize;

PROCEDURE EndPointDir(READONLY end, prev : GridPoint.T) : Dir =
  VAR
    dx := prev.x - end.x;
    dy := prev.y - end.y;
    dl := prev.l - end.l;
    step := Step { Unitize(dx), Unitize(dy), Unitize(dl) };
  BEGIN
    FOR i := FIRST(Dir) TO LAST(Dir) DO
      IF step = DirStep[i] THEN RETURN i END
    END;
    <* ASSERT FALSE *>
  END EndPointDir;

PROCEDURE Neighbor(READONLY a : GridPoint.T; dir : Dir; 
                   VAR res : GridPoint.T) : BOOLEAN =
  BEGIN
    res.x := a.x + DirStep[dir].dx;
    res.y := a.y + DirStep[dir].dy;
    res.l := MAX(MIN(a.l + DirStep[dir].dl, LAST(GridPoint.Layer)), 
                 FIRST(GridPoint.Layer));

    <* ASSERT res = a OR EndPointDir(a,res) = dir *>

    RETURN res # a
  END Neighbor;

BEGIN END EndPointStatus.



