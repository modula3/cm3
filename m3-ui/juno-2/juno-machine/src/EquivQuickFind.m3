(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Oct 31 09:34:02 PST 1994 by heydon                   *)

MODULE Equiv;

(* This implementation of disjoint sets uses the quick-find algorithm,
   in which all trees have depth at most 1. Hence, the Find operation
   takes constant time, but the Union operation takes time
   proportional to the size of the smaller set being merged. *)

REVEAL
  T = Public BRANDED "EquivQuickFind.T" OBJECT
    size: CARDINAL;
    link: T;
  OVERRIDES
    init  := Init;
    union := Union;
  END;

PROCEDURE Init(self: T): T =
  BEGIN
    self.root := self;
    self.size := 1;
    self.link := self;
    RETURN self
  END Init;

PROCEDURE Union(x: T; y: T): T =
  BEGIN
    <* ASSERT x.root = x AND y.root = y *>
    IF x = y THEN RETURN x END;
    IF x.size < y.size THEN
      VAR t := x; BEGIN x := y; y := t; END
    END;
    VAR p := y; BEGIN
      REPEAT
        p.root := x;
        p := p.link
      UNTIL p = y
    END;
    VAR t := y.link; BEGIN y.link := x.link; x.link := t END;
    INC(x.size, y.size);
    RETURN x
  END Union;

BEGIN
END Equiv.
