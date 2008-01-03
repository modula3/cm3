(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* VBT for selecting the view area of an image *)

(* Last modified on Mon Mar 27 12:51:23 PST 1995 by birrell   *)

MODULE ViewAreaVBT EXPORTS ViewAreaVBT;

IMPORT Images, ImageVBT, PaintOp, Point, Rect, Region, Thread, VBT;

TYPE Tracking = { None, NW, SE };

REVEAL T = Public BRANDED OBJECT
    r: Rect.T;
    tracking :=Tracking.None;
    trackStart: Point.T;
  OVERRIDES
    init := Init;
    hit := Hit;
    setSelected := SetSelected;
    getSelected := GetSelected;
    mouse := Mouse;
    position := Position;
    reshape := Reshape;
    paint := Paint;
  END;

CONST
  SelWidth = 1;
  HotRectWidth = 8;

PROCEDURE NWRect(r: Rect.T): Rect.T =
  (* Returns the NW hot-spot rectangle of selection rectangle "r" *)
  VAR
    origin := Rect.NorthWest(r);
    other := Point.MoveHV(origin, HotRectWidth, HotRectWidth);
  BEGIN
    RETURN Rect.FromCorners(origin, other)
  END NWRect;

PROCEDURE SERect(r: Rect.T): Rect.T =
  (* Returns the SE hot-spot rectangle of selection rectangle "r" *)
  VAR
    origin := Rect.SouthEast(r);
    other := Point.MoveHV(origin, -HotRectWidth, -HotRectWidth);
  BEGIN
    RETURN Rect.FromCorners(other, origin)
  END SERect;

PROCEDURE Init(v: T; pm: Images.T; bg: PaintOp.T; r: Rect.T): T =
  VAR res: T;
  BEGIN
    res := ImageVBT.T.init(v, pm, bg);
    v.setSelected(r);
    RETURN res
  END Init;

PROCEDURE Hit(<*UNUSED*>v: T; <*UNUSED*>time: VBT.TimeStamp) =
  BEGIN
  END Hit;

PROCEDURE GetSelected(v: T): Rect.T =
  VAR
    imageDomain := v.get().domain(v);
    domain := VBT.Domain(v);
    imageDelta := Point.Sub(Rect.Middle(domain), Rect.Middle(imageDomain));
  BEGIN
    RETURN Rect.Meet(Rect.Sub(v.r, imageDelta), imageDomain)
  END GetSelected;

PROCEDURE SetSelected(v: T; r: Rect.T) =
  VAR
    imageDomain := v.get().domain(v);
    domain := VBT.Domain(v);
    imageDelta := Point.Sub(Rect.Middle(domain), Rect.Middle(imageDomain));
  BEGIN
    v.r := Rect.Add(r, imageDelta);
    v.repaint(Region.Full);
  END SetSelected;

PROCEDURE Mouse(v: T; READONLY cd: VBT.MouseRec) =
    (* LL = VBT.mu *)
  VAR
    domain := VBT.Domain(v);
    clippedSel := Rect.Meet(v.r, domain);
  BEGIN
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      IF Rect.Member(cd.cp.pt, NWRect(clippedSel)) THEN
        v.tracking := Tracking.NW;
      ELSIF Rect.Member(cd.cp.pt, SERect(clippedSel)) THEN
        v.tracking := Tracking.SE;
      END;
      IF v.tracking # Tracking.None THEN
        v.trackStart := cd.cp.pt;
        VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE));
      END;
    ELSIF cd.clickType = VBT.ClickType.LastUp THEN
      IF v.tracking # Tracking.None THEN
        v.tracking := Tracking.None;
        v.hit(cd.time);
      END;
    END;
  END Mouse;

PROCEDURE Position(v: T; READONLY cd: VBT.PositionRec) =
    (* LL = VBT.mu *)
  VAR
    domain := VBT.Domain(v);
    clippedSel := Rect.Meet(v.r, domain);
    delta: Point.T;
    newR: Rect.T;
  BEGIN
    IF v.tracking # Tracking.None THEN
      IF NOT cd.cp.offScreen THEN
        delta := Point.Sub(cd.cp.pt, v.trackStart);
        IF delta # Point.Origin THEN
          v.trackStart := Rect.Project(domain, cd.cp.pt);
          IF v.tracking = Tracking.NW THEN
            newR := Rect.FromCorners(
                        Rect.Project(domain,
                                     Point.Add(Rect.NorthWest(clippedSel),
                                               delta)),
                        Rect.SouthEast(clippedSel));
          ELSIF v.tracking = Tracking.SE THEN
            newR := Rect.FromCorners(
                        Rect.NorthWest(clippedSel),
                        Rect.Project(domain,
                                     Point.Add(Rect.SouthEast(clippedSel),
                                               delta)));
          END;
          IF Rect.HorSize(newR) > 2 * HotRectWidth AND
            Rect.VerSize(newR) > 2 * HotRectWidth THEN
            v.r := newR;
            v.repaint(Region.Full);
          END;
        END;
      END;
      VBT.SetCage(v, VBT.CageFromPosition(cd.cp, TRUE));
    END;
  END Position;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) =
  BEGIN
    v.r := Rect.Add(v.r, Point.Sub(Rect.Middle(cd.new), Rect.Middle(cd.prev)));
    ImageVBT.T.reshape(v, cd);
  END Reshape;

PROCEDURE Paint(v: T; READONLY rgn: Region.T)
               RAISES { Thread.Alerted, Images.Error } =
    (* LL = mu.v *)
  VAR
    domain := VBT.Domain(v);
    clippedSel := Rect.Meet(v.r, domain);
    selRgn: Region.T;
  BEGIN
    IF NOT Rect.IsEmpty(domain) THEN
      selRgn := Region.FromRect(clippedSel);
      selRgn := Region.Difference(selRgn,
                             Region.FromRect(Rect.Inset(clippedSel, SelWidth)));
      selRgn := Region.JoinRect(NWRect(clippedSel), selRgn);
      selRgn := Region.JoinRect(SERect(clippedSel), selRgn);
      ImageVBT.T.paint(v, Region.Difference(rgn, selRgn));
      VBT.PaintRegion(v, selRgn, PaintOp.Fg);
    END;
  END Paint;


BEGIN

END ViewAreaVBT.
