MODULE MagRectUtils;
IMPORT MagPoint;
IMPORT Rect;

PROCEDURE FromRect(r: Rect.T): T =
  BEGIN
    RETURN T{MagPoint.T{r.west, -r.south},
             MagPoint.T{r.east, -r.north}};
  END FromRect;

PROCEDURE ToRect(mr: T): Rect.T =
  BEGIN
    RETURN Rect.T{mr.ll.x, mr.ur.x,
                  -mr.ur.y, -mr.ll.y};
  END ToRect;

BEGIN
END MagRectUtils.
