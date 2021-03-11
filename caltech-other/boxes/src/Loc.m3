MODULE Loc;
IMPORT Debug,Fmt,GridPoint;

VAR DoDebug := Debug.DebugThis("Loc");

PROCEDURE Compare(READONLY a, b : T) : [-1..1] =
  VAR
    aCost, bCost : INTEGER;
  BEGIN
    <* ASSERT a.remains # -1 *>
    <* ASSERT b.remains # -1 *>
    aCost := a.minCost + a.remains;
    bCost := b.minCost + b.remains;

    IF DoDebug THEN
      Debug.Out("Loc.Compare: a: " & GridPoint.Format(a.point) & " remains: " &
        Fmt.Int(a.remains) & " cost: " & Fmt.Int(aCost));
      Debug.Out("Loc.Compare: b: " & GridPoint.Format(b.point) & " remains: " &
        Fmt.Int(b.remains) & " cost: " & Fmt.Int(bCost));
    END;

    IF aCost < bCost THEN RETURN -1
    ELSIF aCost > bCost THEN RETURN 1
    ELSIF a.remains < b.remains THEN RETURN -1
    ELSIF a.remains > b.remains THEN RETURN 1
    ELSIF a.point.x < b.point.x THEN RETURN -1
    ELSIF a.point.x > b.point.x THEN RETURN 1
    ELSE RETURN 0 (* can this happen? *)
    END
  END Compare;

BEGIN END Loc.
