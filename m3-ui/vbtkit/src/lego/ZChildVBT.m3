(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Oct 25 15:12:38 PDT 1995 by mhb                      *)
(*      modified on Wed Mar  3 01:06:48 PST 1993 by meehan                   *)
(*      modified on Tue Jun 16 13:07:59 PDT 1992 by muller                   *)
(*      modified on Fri Mar 27 02:58:45 1992 by steveg                       *)

MODULE ZChildVBT;

IMPORT Axis, FilterClass, HighlightVBT, Interval, 
       Point, Rect, Split, VBT, VBTClass, ZSplit;

TYPE
  HotSpot = Location;
  Coord = OBJECT END;
  AbsCoord = Coord BRANDED OBJECT x, y: REAL END;
  RelCoord = Coord BRANDED OBJECT x, y: REAL END;

  At = OBJECT END;
  ByPt = At OBJECT hot: HotSpot; pt: Coord; END;
  ByEdges = At OBJECT nw, se: Coord; END;


REVEAL
  T = Public BRANDED OBJECT
        open: BOOLEAN; (* the "Open" property *)
        at: At; (* the "At" property *)
        touched:=FALSE; (* whether user has changed its position *)
        shaperSet:=FALSE; (* has a reshape control been set? *)
        shaper: ZSplit.ReshapeControl; (* control reshaping *)
        useAt:=TRUE; (* use the "At" info for position *)
        size: ARRAY Axis.T OF INTEGER; (* width and height set by user *)
        sizeMM: ARRAY Axis.T OF REAL;  (* width/height in millimeters *)
      OVERRIDES
        shape         := Shape;
        rescreen      := Rescreen;
        init          := Init;
        initFromEdges := InitFromEdges;
      END;

EXCEPTION BadPercentage;

PROCEDURE Init (z   : T;
                ch  : VBT.T;
                h, v          := 0.5;
                loc           := Location.Center;
                type          := CoordType.Scaled;
                shaper        : ZSplit.ReshapeControl := NIL;
                open          := TRUE              ): T =
  <* FATAL BadPercentage *>
  BEGIN
    EVAL HighlightVBT.T.init (z, ch);
    z.open := open;
    IF type = CoordType.Absolute THEN
      z.at := NEW (ByPt, hot := loc, pt := NEW (AbsCoord, x := h, y := v))
    ELSIF Pct (h) AND Pct (v) THEN
      z.at := NEW (ByPt, hot := loc, pt := NEW (RelCoord, x := h, y := v))
    ELSE
      RAISE BadPercentage
    END;
    IF shaper = NIL THEN
      z.shaper := Scaled
    ELSE
      z.shaper := shaper
    END;
    RETURN z
  END Init;

PROCEDURE InitFromEdges (v         : T;
                         ch        : VBT.T;
                         w, e, n, s: REAL;
                         type                := CoordType.Absolute;
                         shaper        : ZSplit.ReshapeControl := NIL;
                         open                := TRUE                ): T =
  <* FATAL BadPercentage *>
  BEGIN
    EVAL HighlightVBT.T.init (v, ch);
    v.open := open;
    IF type = CoordType.Absolute THEN
      v.at := NEW (ByEdges, 
                   nw := NEW (AbsCoord, x := w, y := n),
                   se := NEW (AbsCoord, x := e, y := s))
    ELSIF Pct (w) AND Pct (e) AND Pct (n) AND Pct (s) THEN
      v.at := NEW (ByEdges,
                   nw := NEW (RelCoord, x := w, y := n),
                   se := NEW (RelCoord, x := e, y := s))
    ELSE
      RAISE BadPercentage
    END;
    IF shaper = NIL THEN
      v.shaper := Scaled
    ELSE
      v.shaper := shaper
    END;
    RETURN v;
  END InitFromEdges;

PROCEDURE Pct (x: REAL): BOOLEAN =
  BEGIN
    RETURN 0.00 <= x AND x <= 1.00
  END Pct;

PROCEDURE Shape (v: T; ax: Axis.T; n: CARDINAL): VBT.SizeRange =
  VAR sr := VBTClass.GetShape (v.ch, ax, n);
  BEGIN
    IF NOT v.useAt AND v.size[ax] # 0 THEN  
      sr.pref := MIN (MAX (sr.lo, v.size [ax]), sr.hi - 1)
(*
      sr.pref := v.size [ax];
      sr.lo := MIN (sr.lo, v.size[ax]);
      sr.hi := MAX (sr.hi, v.size[ax] + 1);
*)
    END;
    RETURN sr;
  END Shape;

PROCEDURE Rescreen (v: T; READONLY cd: VBT.RescreenRec) =
  BEGIN
    IF NOT v.useAt THEN
      v.size [Axis.T.Hor] :=
        ROUND (VBT.MMToPixels (v, v.sizeMM [Axis.T.Hor], Axis.T.Hor));
      v.size [Axis.T.Ver] :=
        ROUND (VBT.MMToPixels (v, v.sizeMM [Axis.T.Ver], Axis.T.Ver));
    END;
    HighlightVBT.T.rescreen (v, cd);
  END Rescreen;

PROCEDURE Moved (vbt: VBT.T) =
  BEGIN
    TYPECASE vbt OF T (v) => 
      v.touched := TRUE 
    ELSE 
    END
  END Moved;

PROCEDURE Grew (vbt: VBT.T; w, h: INTEGER) =
  BEGIN
    TYPECASE vbt OF T(v) =>
      v.touched := TRUE;
      RecordSize (v, Rect.FromSize (w, h));
    ELSE 
    END
  END Grew;

PROCEDURE InitiallyMapped (vbt: VBT.T): BOOLEAN =
  BEGIN
    TYPECASE vbt OF T (v) => 
      RETURN v.open
    ELSE 
      RETURN TRUE 
    END
  END InitiallyMapped;

PROCEDURE Pop (vbt: VBT.T; forcePlace: BOOLEAN := FALSE) =
  VAR
    zsplit := VBT.Parent(vbt);
    vDom   := ZSplit.GetDomain(vbt);
    zDom   := ZSplit.GetParentDomain(zsplit);
  BEGIN
    IF forcePlace OR Rect.IsEmpty(vDom) OR NOT Rect.Overlap(vDom, zDom)
      THEN
      (* it's not visible, so put it in standard place *)
      Inserted(vbt);
    END;
    ZSplit.Lift(vbt, ZSplit.Altitude.Top);
    ZSplit.Map(vbt);
  END Pop;

PROCEDURE Inserted (vbt: VBT.T) =
  VAR zDom, vDom: Rect.T;
  BEGIN
    zDom := VBT.Domain (VBT.Parent (vbt));
    TYPECASE vbt OF
    | T (v) =>
      vDom := GetZRect (zDom, v);
      IF NOT v.shaperSet THEN
        ZSplit.SetReshapeControl (v, ZChild);
        v.shaperSet := TRUE
      END;
      v.touched := FALSE;
    ELSE
    END;
    ZSplit.Move (vbt, vDom);
  END Inserted;

PROCEDURE ZChildReshape (<* UNUSED *> self: ZSplit.ReshapeControl;
                                      ch  : VBT.T;
                         READONLY oldParentDomain: Rect.T;
                         READONLY newParentDomain: Rect.T;
                         READONLY oldChildDomain : Rect.T  ): Rect.T =
  <*FATAL Split.NotAChild*>
  VAR v: T := ch; r: Rect.T;
  BEGIN
    IF Split.Succ (VBT.Parent (v), v) = NIL THEN
      (* background child *)
      RETURN newParentDomain
    END;
    IF v.touched THEN
      (* chain the NW *)
      r := Rect.Move (oldChildDomain,
             Point.Sub (Rect.NorthWest (newParentDomain),
                        Rect.NorthWest (oldParentDomain)))
    ELSIF v.useAt THEN
      (* get initial placement *)
      r := GetZRect (newParentDomain, v);
    ELSE
      (* do what the client specified in the init call *)
      r := v.shaper.apply (v, 
              oldParentDomain, newParentDomain, oldChildDomain);
    END;
    RecordSize (v, r);
    RETURN r
  END ZChildReshape;

<*INLINE*>
PROCEDURE Scale(num, den, lo, hi, idelta, odelta: INTEGER): Interval.T =
  (* Scale lo+delta and hi+delta by num/den, and return the resulting interval
     shifted by odelta *)
  BEGIN
    RETURN Interval.FromBounds(
      ((lo+idelta)*num + den DIV 2) DIV den + odelta,
      ((hi+idelta)*num + den DIV 2) DIV den + odelta)
  END Scale; 

PROCEDURE DoScaledReshape(
    READONLY op, np, oc: Rect.T;
    fixedH, fixedV: BOOLEAN): Rect.T =
  VAR hor, ver: Interval.T;
  BEGIN
    IF Rect.IsEmpty(op) THEN RETURN oc END;
    hor := Scale(Rect.HorSize(np), Rect.HorSize(op), 
      oc.west, oc.east, -op.west, np.west);
    IF fixedH THEN 
      hor := Interval.Center (Interval.FromSize(Rect.HorSize(oc)), 
                              Interval.Middle (hor));
    END;
    ver := Scale(Rect.VerSize(np), Rect.VerSize(op), 
      oc.north, oc.south, -op.north, np.north);
    IF fixedV THEN 
      ver := Interval.Center (Interval.FromSize(Rect.VerSize(oc)), 
                              Interval.Middle (ver));
    END;
    RETURN Rect.FromIntervals(hor, ver)
  END DoScaledReshape;

PROCEDURE ScaledReshape(
    <* UNUSED *> self: ZSplit.ReshapeControl;
    <* UNUSED *> ch: VBT.T; 
    READONLY op, np, oc: Rect.T): Rect.T =
  BEGIN
    RETURN DoScaledReshape (op, np, oc, FALSE, FALSE)
  END ScaledReshape;

PROCEDURE ScaledHReshape(
    <* UNUSED *> self: ZSplit.ReshapeControl;
    <* UNUSED *> ch: VBT.T; 
    READONLY op, np, oc: Rect.T): Rect.T =
  BEGIN
    RETURN DoScaledReshape (op, np, oc, TRUE, FALSE)
  END ScaledHReshape;

PROCEDURE ScaledVReshape(
    <* UNUSED *> self: ZSplit.ReshapeControl;
    <* UNUSED *> ch: VBT.T; 
    READONLY op, np, oc: Rect.T): Rect.T =
  BEGIN
    RETURN DoScaledReshape (op, np, oc, FALSE, TRUE)
  END ScaledVReshape;

PROCEDURE ScaledHVReshape(
    <* UNUSED *> self: ZSplit.ReshapeControl;
    <* UNUSED *> ch: VBT.T; 
    READONLY op, np, oc: Rect.T): Rect.T =
  BEGIN
    RETURN DoScaledReshape (op, np, oc, TRUE, TRUE)
  END ScaledHVReshape;

PROCEDURE RecordSize (v: T; READONLY r: Rect.T) =
  BEGIN
   IF NOT Rect.IsEmpty (r) THEN 
    v.size [Axis.T.Hor] := Rect.HorSize(r);
    v.size [Axis.T.Ver] := Rect.VerSize(r);
    v.sizeMM [Axis.T.Hor] :=
          FLOAT (v.size[Axis.T.Hor]) / VBT.MMToPixels (v, 1.0, Axis.T.Hor);
    v.sizeMM [Axis.T.Ver] :=
          FLOAT (v.size[Axis.T.Ver]) / VBT.MMToPixels (v, 1.0, Axis.T.Ver);
   END
  END RecordSize;

PROCEDURE GetZRect (dom: Rect.T; ch: T): Rect.T =
  VAR p: Point.T; r: Rect.T; pref: Rect.T;
  PROCEDURE map (pct: REAL; low, high: INTEGER): INTEGER =
    BEGIN
      RETURN low + ROUND (FLOAT (high - low) * pct);
    END map;
  PROCEDURE offset (mm: REAL; ax: Axis.T): INTEGER =
    BEGIN
      RETURN ROUND (VBT.MMToPixels (ch, mm, ax))
    END offset;
  BEGIN
    IF Rect.IsEmpty (dom) THEN
      RETURN Rect.Empty
    ELSE
      WITH sh = VBTClass.GetShapes (ch, FALSE) DO
        pref := Rect.FromSize (sh[Axis.T.Hor].pref, sh[Axis.T.Ver].pref)
      END;
      TYPECASE ch.at OF

      | ByPt (atPt) =>
          TYPECASE atPt.pt OF
          | AbsCoord (ac) =>
              p.h := dom.west + offset (ac.x, Axis.T.Hor);
              p.v := dom.north + offset (ac.y, Axis.T.Ver)
          | RelCoord (rc) =>
              p.h := map (rc.x, dom.west, dom.east);
              p.v := map (rc.y, dom.north, dom.south)
          ELSE <* ASSERT FALSE *>
          END;
          r := PlaceRect (pref, p, atPt.hot);
  
      | ByEdges (atEdges) =>

        TYPECASE atEdges.nw OF
        | AbsCoord (ac) =>
            r.west := dom.west + offset (ac.x, Axis.T.Hor);
            r.north := dom.north + offset (ac.y, Axis.T.Ver)
        | RelCoord (rc) =>
            r.west := map (rc.x, dom.west, dom.east);
            r.north := map (rc.y, dom.north, dom.south)
        ELSE <* ASSERT FALSE *>
        END;
        TYPECASE atEdges.se OF
        | AbsCoord (ac) =>
            r.east := dom.west + offset (ac.x, Axis.T.Hor);
            r.south := dom.north + offset (ac.y, Axis.T.Ver)
        | RelCoord (rc) =>
            r.east := map (rc.x, dom.west, dom.east);
            r.south := map (rc.y, dom.north, dom.south)
        ELSE <* ASSERT FALSE *>
        END; 

      ELSE <* ASSERT FALSE *>
      END;
    END;
    IF Rect.Subset (r, dom) THEN 
      ch.useAt := FALSE
    ELSE
      r := Project (r, dom)
    END;
    RETURN r;
  END GetZRect;

PROCEDURE PlaceRect(r: Rect.T; p: Point.T; hot: HotSpot): Rect.T=
  (* Given a rectangle assumed to have its NW corner at the origin, return a
     rectangle that is placed relative to point p as specified by reference.
     That is to say, depending on reference, its center or one of its corners
     will be placed at p. *)
  VAR
    offh, offv: INTEGER;
  BEGIN
    CASE hot OF
    | HotSpot.Center =>
      RETURN Rect.Center(r, p);
    | HotSpot.NW =>
      offh := p.h;
      offv := p.v;
    | HotSpot.NE =>
      offh := p.h - Rect.HorSize(r);
      offv := p.v;
    | HotSpot.SW =>
      offh := p.h;
      offv := p.v - Rect.VerSize(r);
    | HotSpot.SE =>
      offh := p.h - Rect.HorSize(r);
      offv := p.v - Rect.VerSize(r);
    END;
    RETURN Rect.MoveHV(r, offh, offv);
  END PlaceRect;

PROCEDURE Project (r, dom: Rect.T): Rect.T =
  (* Return a rect that is congruent to r, offset to be sure that
     its northwest corner is always visible. *)
  VAR
    offset := Point.T {h := MAX (0, dom.west - r.west),
                       v := MAX (0, dom.north - r.north)};
  BEGIN
    RETURN Rect.Move (r, offset);
  END Project;


VAR
  ZChild  := NEW (ZSplit.ReshapeControl, apply := ZChildReshape);

BEGIN
  Scaled  := NEW (ZSplit.ReshapeControl, apply := ScaledReshape);
  ScaledHFixed  := NEW (ZSplit.ReshapeControl, apply := ScaledHReshape);
  ScaledVFixed  := NEW (ZSplit.ReshapeControl, apply := ScaledVReshape);
  ScaledHVFixed  := NEW (ZSplit.ReshapeControl, apply := ScaledHVReshape);
END ZChildVBT.
