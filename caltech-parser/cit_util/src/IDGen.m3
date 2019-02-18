MODULE IDGen;
IMPORT Rect;
IMPORT Point;
IMPORT Region;


REVEAL
  Low = LowPublic BRANDED OBJECT
    fr: Region.T;   (* "(p,0)" in region iff "p" free. *)
  OVERRIDES
    init                := InitLow;
    alloc               := AllocLow;
    free                := FreeLow;
  END;

PROCEDURE InitLow(self: Low): Low =
  BEGIN
    self.fr := Region.FromRect(Rect.T{0,LAST(INTEGER),0,1});
    RETURN self;
  END InitLow; 

PROCEDURE AllocLow(self: Low; force:=-1): ID =
  VAR
    res: ID;
  BEGIN
    IF force>=0 THEN
      res := force;
    ELSIF NOT AllocLow1(self, 0, LAST(INTEGER), res) THEN
      <* ASSERT FALSE *>
      (* out of IDs *)
    END;
    self.fr := Region.Difference(self.fr, Region.FromPoint(Point.T{res,0}));
    RETURN res;
  END AllocLow;

(* TODO: isn't there a Region.BoundingBox or something?
   That would be much more efficient, probably. *)
PROCEDURE AllocLow1(self: Low; lo, hi: ID; VAR res: ID): BOOLEAN =
  BEGIN
    <* ASSERT lo # hi *>
    WITH mid = (lo + hi) DIV 2,
         r1  = Rect.T{lo, mid, 0, 1},
         r2  = Rect.T{mid, hi, 0, 1} DO
      IF Region.OverlapRect(r1, self.fr) THEN
        IF mid = lo+1 THEN
          res := lo;
          RETURN TRUE;
        END;
        RETURN AllocLow1(self, lo, mid, res);
      ELSIF Region.OverlapRect(r2, self.fr) THEN
        IF hi = mid+1 THEN
          res := mid;
          RETURN TRUE;
        END;
        RETURN AllocLow1(self, mid, hi, res);
      ELSE
        RETURN FALSE;
      END;
    END;
  END AllocLow1;

PROCEDURE FreeLow(self: Low; id: ID) =
  VAR
    p := Point.T{id, 0};
  BEGIN
    <* ASSERT NOT Region.Member(p, self.fr) *>
    (* not allocated *)

    self.fr := Region.JoinRect(Rect.FromPoint(p), self.fr);
  END FreeLow; 



REVEAL
  Unique = UniquePublic BRANDED OBJECT
    cur: ID;
  OVERRIDES
    init                := InitUnique;
    alloc               := AllocUnique;
    free                := FreeUnique;
  END;

PROCEDURE InitUnique(self: Unique): Unique =
  BEGIN
    self.cur := 0;
    RETURN self;
  END InitUnique; 

PROCEDURE AllocUnique(self: Unique; force:=-1): ID =
  BEGIN
    WITH res = self.cur DO
      IF force >= 0 THEN
        res := MAX(res, force);
        RETURN force;
      END;
      INC(self.cur);
      RETURN res;
    END;
  END AllocUnique;

PROCEDURE FreeUnique(<*UNUSED*>self: Unique; <*UNUSED*>id: ID) =
  BEGIN
  END FreeUnique;

BEGIN
END IDGen.
