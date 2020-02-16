(* $Id$ *)

MODULE RectSet;
IMPORT MagLayerRectSet AS Spec;
IMPORT MagLayerRect AS Rect;
IMPORT MagRect;
IMPORT Word;

REVEAL
  T = Public BRANDED Brand OBJECT OVERRIDES
    clip := Clip;
    keepOverlapping := KeepOverlapping;
    copy := Copy;
    union := Union;
    intersection := Intersection;
    diff := Diff;
  END;

PROCEDURE Copy(self : T) : Spec.T = 
  VAR
    res := NEW(T).init(self.size());
    iter := self.iterate();
    r : Rect.T;
  BEGIN
    WHILE iter.next(r) DO EVAL res.insert(r) END;
    RETURN res
  END Copy;

(* we code these methods in terms of copy and the "D" variants of      *)
(* the methods.  If we didn't do this, then the supertype's union etc. *)
(* methods would return an object of the supertype and not a RectSet.T *)

(* by the way, this coding means that a subtype need only override copy() *)

(* a different approach would be to simply inherit from the Set.T rather *)
(* than from the SetDef.T, but then we'd have to supply our own *)
(* implementation of the set itself *)

PROCEDURE Intersection(self : T; with : Spec.T) : Spec.T =
  VAR
    res := with.copy();
  BEGIN 
    RETURN res.intersectionD(self) 
  END Intersection;

PROCEDURE Union(self : T;  with : Spec.T) : Spec.T =
  BEGIN RETURN with.copy().unionD(self) END Union;

PROCEDURE Diff(self : T; with : Spec.T) : Spec.T =
  BEGIN RETURN self.copy().diffD(with) END Diff;

(* the stuff we really wanted to specify follows. *)

PROCEDURE Distance(READONLY a, b : T) : LONGREAL =
  VAR
    aIter := a.iterate();
    aRect : Rect.T;
    minDistance := LAST(LONGREAL);
  BEGIN
    (* this is inefficient.  We should really prune a against the *)
    (* bounding box of the b set . *)
    WHILE aIter.next(aRect) DO
      VAR
        bIter := b.iterate();
        bRect : Rect.T;
      BEGIN
        WHILE bIter.next(bRect) DO
          WITH thisDist = FLOAT(Rect.Distance(aRect, bRect),LONGREAL) DO
            IF thisDist < minDistance THEN
              minDistance := thisDist
            END
          END (* WITH *)
        END (* WHILE *)
      END (* VAR BEGIN *)
    END; (* WHILE *)
    RETURN minDistance
  END Distance;

PROCEDURE Clip(a : T; READONLY against : MagRect.T) : T =
  VAR
    res := NEW(T).init();
    iter := a.iterate();
    rect : Rect.T;
    clipRect : MagRect.T;
  BEGIN
    WHILE iter.next(rect) DO
      IF MagRect.Intersection(rect.rect, against, clipRect) THEN
        EVAL res.insert(Rect.T { clipRect, rect.layer }) 
      END
    END;
    RETURN res
  END Clip;

PROCEDURE KeepOverlapping(a : T; READONLY against : MagRect.T) : T =
  VAR
    res := NEW(T).init();
    iter := a.iterate();
    rect : Rect.T;
    clipRect : MagRect.T;
  BEGIN
    WHILE iter.next(rect) DO
      IF MagRect.Intersection(rect.rect, against, clipRect) THEN
        EVAL res.insert(Rect.T { rect.rect, rect.layer }) 
      END
    END;
    RETURN res
  END KeepOverlapping;

PROCEDURE Equal(a, b : T) : BOOLEAN = BEGIN RETURN a.equal(b) END Equal;

PROCEDURE Hash(a : T) : Word.T = 
  VAR
    iter := a.iterate();
    e : Rect.T;
    hash := 0;
  BEGIN 
    WHILE iter.next(e) DO hash := Word.Plus(hash, Rect.Hash(e)) END;
    RETURN hash
  END Hash;

BEGIN END RectSet.
