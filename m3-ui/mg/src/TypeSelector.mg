(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Steve Glassman and Mick Jordan                         *)
(* Last modified on Tue Nov  1 09:31:55 PST 1994 by kalsow   *)
(*      modified on Tue Jun  9 19:36:02 1992 by steveg       *)

GENERIC MODULE TypeSelector(Type);

IMPORT
  MG, MGV, R2, R2Box, VBT;

TYPE
  SelectorIter = MG.GroupIterator OBJECT
    closest: MG.T := NIL;
    dist   : REAL := 999999999.0;
    pos: R2.T;
  OVERRIDES
    proc := SelectorProc;
  END;

PROCEDURE Closest (<* UNUSED *>          s  : MGV.Selector;
                                                 v  : MGV.V;
                                        READONLY pos: R2.T;
                           <* UNUSED *> READONLY cd : VBT.MouseRec): MG.T =
  VAR iter := NEW(SelectorIter, v := v, pos := pos);
  BEGIN
    EVAL v.displayList.iterate(iter, TRUE, FALSE);
    RETURN iter.closest;
  END Closest;

PROCEDURE SelectorProc (iter: SelectorIter; t: MG.T): BOOLEAN =
  VAR
    dx, dy, dist: REAL;
    pos                            := iter.pos;
    bounds: R2Box.T;
  BEGIN
    TYPECASE t OF
    | Type.T =>
      bounds := t.appearance.boundingBox(t, iter.v);
      IF pos[0] < bounds[0].lo THEN
        dx := bounds[0].lo - pos[0]
      ELSIF pos[0] > bounds[0].hi THEN
        dx := pos[0] - bounds[0].hi
      ELSE
        dx := 0.0;
      END;
      IF pos[1] < bounds[1].lo THEN
        dy := bounds[1].lo - pos[1]
      ELSIF pos[1] > bounds[1].hi THEN
        dy := pos[1] - bounds[1].hi
      ELSE
        dy := 0.0;
      END;
      IF dx = 0.0 AND dy = 0.00 THEN
        iter.closest := t;
        iter.dist := 0.0;
        RETURN FALSE
      ELSE
        dist := dx * dx + dy * dy;
        IF dist < iter.dist THEN iter.closest := t; iter.dist := dist; END;
        RETURN TRUE;
      END;
    ELSE
      RETURN TRUE
    END;
  END SelectorProc;

BEGIN
  closest := NEW(MGV.Selector, select := Closest);
END TypeSelector.
