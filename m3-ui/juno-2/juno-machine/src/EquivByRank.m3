(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 31 09:33:50 PST 1994 by heydon                   *)

MODULE Equiv;

(* This implementation of disjoint sets uses the path-compression and
   union-by-rank heuristics described in "Introduction to Algorithms":
   Cormen, Leiserson, Rivest, 1st Ed., pp.446-449 *)

REVEAL
  T = Public BRANDED "EquivByRank.T" OBJECT
    parent: T;
    rank: CARDINAL;
  OVERRIDES
    init  := Init;
    find  := Find;
    union := Union;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    self.parent := self;
    self.rank := 0;
    RETURN self
  END Init;

PROCEDURE Find(self: T): T =
  BEGIN
    IF self # self.parent THEN self.parent := Find(self.parent) END;
    RETURN self.parent
  END Find;

PROCEDURE Union(x: T; y: T): T =
  BEGIN
    <* ASSERT x.parent = x AND y.parent = y *>
    IF x = y THEN RETURN x END;
    IF x.rank < y.rank THEN
      x.parent := y;
      RETURN y
    ELSE
      y.parent := x;
      IF x.rank = y.rank THEN INC(x.rank) END;
      RETURN x
    END
  END Union;

BEGIN
END Equiv.
