(* Copyright (c) 2000 California Institute of Technology *)
(* All rights reserved. See the file COPYRIGHT for a full description. *)
(* $Id: DCPaneVBT.m3,v 1.2 2001-09-19 15:30:31 wagner Exp $ *)

MODULE DCPaneVBT;
IMPORT Axis;
IMPORT VBTTextBounder;
IMPORT PaneVBT;
IMPORT Rd;
IMPORT Region;
IMPORT Point;
IMPORT VBTDrawContext;
IMPORT Transform;
IMPORT TransformOther;
IMPORT PaintOp;
IMPORT Rect;
IMPORT VBT;
IMPORT VBTClass;
IMPORT KeyboardKey;
IMPORT Wr;
IMPORT PSDrawContext;
IMPORT RegionDrawContext;
IMPORT PaneManOp;

IMPORT Pointe;
IMPORT Fmt;
FROM Debug IMPORT S;

REVEAL
  T = Public BRANDED "DCPaneVBT" OBJECT
    m: MUTEX;
    transform: Transform.T;
    lastPt: Point.T := Nowhere;
    rect: Rect.T := Nothing;
    centered := FALSE;
  OVERRIDES
    init := Init;
    key := Key;
    write := Write;
    repaint := Repaint;
    mouse := Mouse;
    position := Position;
    shape := Shape;
    getTransformFrom := GetTransformFrom;
    getTransform := GetTransform;
  END;

CONST
  DebugLevel = 10;
  Nowhere = Point.T{LAST(INTEGER),0};
  Nothing = Rect.T{LAST(INTEGER),0,0,0};

PROCEDURE Init(self: T;
               <*UNUSED*> rd: Rd.T;
               <*UNUSED*> pm: PaneManOp.T): PaneVBT.T =
  BEGIN
    S("called DCPaneVBT.T.init", DebugLevel);
    <* ASSERT NOT self.centered *>
    self.m := NEW(MUTEX);
    self.transform := Transform.Translate(256.0, 256.0,
                                          Transform.IsoScale(1.0));
    RETURN self;
  END Init;

PROCEDURE ScrollPoint(self: T; p: Point.T) =
  VAR
    r := VBT.Domain(self);
    exposed := Region.Difference(
                   Region.FromRect(r),
                   Region.FromRect(
                       Rect.Move(r, p)));
  BEGIN
    self.transform := Transform.Compose(self.transform,
                                        Transform.FromPoint(p));
    VBT.Scroll(self, r, p);
    UnlockedRepaint(self, exposed); (* VBT.ForceRepaint isn't working right *)
    VBT.Sync(self);
  END ScrollPoint;

PROCEDURE Scroll(self: T; h, v: INTEGER) =
  BEGIN
    LOCK self.m DO
      ScrollPoint(self, Point.T{h, v});
    END;
  END Scroll;

PROCEDURE Scale(self: T; f: REAL) =
  BEGIN
    LOCK self.m DO
      VAR
        r := VBT.Domain(self);
        p := Rect.Middle(r);
      BEGIN
        self.transform := Transform.Compose(self.transform,
                                          TransformOther.IsoScaleAbout(f, p));
        VBT.ForceRepaint(self, Region.Full);
      END;
    END;
  END Scale;

PROCEDURE Key(self: T; READONLY key: VBT.KeyRec) =
  CONST
    Zoom = 1.414;
  VAR
    r := VBT.Domain(self);
    trans := (r.east-r.west) DIV 4;
  BEGIN
    IF key.wentDown THEN
      CASE key.whatChanged OF
      | ORD('-'), ORD('_') => Scale(self, 1.0/Zoom);
      | ORD('='), ORD('+') => Scale(self, Zoom);
      | KeyboardKey.Up => Scroll(self, 0, trans);
      | KeyboardKey.Down => Scroll(self, 0, -trans);
      | KeyboardKey.Left => Scroll(self, trans, 0);
      | KeyboardKey.Right => Scroll(self, -trans, 0);
      ELSE
      END;
    END;
  END Key;

PROCEDURE Mouse(self: T; READONLY cd: VBT.MouseRec) =
  BEGIN
    CASE cd.clickType OF
    | VBT.ClickType.FirstDown =>
      VBT.SetCage(self, VBT.CageFromPosition(cd.cp));
      LOCK self.m DO
        self.lastPt := cd.cp.pt;
      END;
    | VBT.ClickType.LastUp =>
      VBT.SetCage(self, VBT.EverywhereCage);
      LOCK self.m DO
        self.lastPt := Nowhere;
      END;
    ELSE
    END;
    Public.mouse(self, cd);
  END Mouse;

PROCEDURE Position(self: T; READONLY cd: VBT.PositionRec) =
  BEGIN
    LOCK self.m DO
      IF self.lastPt # Nowhere THEN
        ScrollPoint(self, Point.Sub(cd.cp.pt, self.lastPt));
        self.lastPt := cd.cp.pt;
        VBT.SetCage(self, VBT.CageFromPosition(cd.cp));
      END;
    END;
  END Position;

PROCEDURE RectContains(a, b: Rect.T): BOOLEAN =
  BEGIN
    RETURN Rect.Equal(Rect.Meet(a,b),b);
  END RectContains;

PROCEDURE DoubleCheckFit(self: T) =
  VAR
    r := VBT.Domain(self);
  BEGIN
    GetRect(self);
    IF NOT RectContains(r, self.rect) THEN
      S("text sizing must have broken the fit. Try again.", DebugLevel);
      self.centered := FALSE;
      Size(self);
      Center(self);
      IF NOT RectContains(r, self.rect) THEN
        S("still not right. Oh well.", DebugLevel);
      END;
    END;
  END DoubleCheckFit;

PROCEDURE UnlockedRepaint(self: T; READONLY rgn: Region.T) =
  VAR
    dc := NEW(VBTDrawContext.T).init(self, PaintOp.Bg);
  BEGIN
    IF NOT self.centered THEN
      S("will do size-center thingy", DebugLevel);
      Size(self);
      Center(self);
      DoubleCheckFit(self);
    ELSE
      S("will NOT size-center. self.centered=TRUE.", DebugLevel);
    END;
    dc.setClip(rgn);
    dc.setTransform(self.transform);
    dc.erase();
    self.paint(dc);
  END UnlockedRepaint; 

PROCEDURE Repaint(self: T; READONLY rgn: Region.T) =
  BEGIN
    (*
      <* ASSERT debug # Region.Full *>
      IF debug # Region.Empty THEN
      VBT.PaintRegion(self, TransformOther.ApplyToRegion(self.transform, debug));
      END;
      *)
    LOCK self.m DO
      UnlockedRepaint(self, rgn);
    END;
  END Repaint;

PROCEDURE Write(self: T; wr: Wr.T) =
  VAR
    dc := NEW(PSDrawContext.T).init();
  BEGIN
    LOCK self.m DO
      EVAL dc.preTransform(self.transform);
      self.paint(dc);
      dc.write(wr, "Test");
    END;
  END Write;

(* VAR
  debug := Region.Empty; *)

PROCEDURE GetRect(self: T) =
  BEGIN
    IF self.rect = Nothing THEN
      VAR
        tb := NEW(VBTTextBounder.T).init(self);
        region := NEW(RegionDrawContext.T).init(tb);
      BEGIN
        region.setTransform(self.transform);
        self.paint(region);
        (* IF debug # Region.Empty THEN
           debug := region.toRegion();
           END; *)
        self.rect := region.toRegion().r;
        self.rect := Rect.Inset(self.rect, -6);
      END;      
    END;
  END GetRect;

PROCEDURE Center(self: T) =
  BEGIN
    GetRect(self);
    VAR
      offset := Point.Sub(Rect.NorthWest(VBT.Domain(self)),
                          Rect.NorthWest(self.rect));
    BEGIN
      S("offset: " & Pointe.Format(offset), DebugLevel);
      self.transform := Transform.Compose(self.transform,
         Transform.FromPoint(offset));
      (* VBT.ForceRepaint(self, Region.Full); *)
      self.centered := TRUE;
    END;
    self.rect := Nothing;
  END Center;

PROCEDURE GetTransformFrom(self, other: T) =
  BEGIN
    LOCK self.m DO
      LOCK other.m DO
        self.transform := other.transform;
        DoNotCenter(self);
      END;
    END;
  END GetTransformFrom;

PROCEDURE DoNotCenter(self: T) =
  BEGIN
    S("told not to center.", DebugLevel);
    self.centered := TRUE;
  END DoNotCenter;

PROCEDURE Size(self: T) =
  VAR
    r := VBT.Domain(self);
  BEGIN
    GetRect(self);
    VAR
      targetH := Rect.Size(Axis.T.Hor, r);
      actualH := Rect.Size(Axis.T.Hor, self.rect);
      scaleH := FLOAT(targetH)/FLOAT(actualH);
      targetV := Rect.Size(Axis.T.Ver, r);
      actualV := Rect.Size(Axis.T.Ver, self.rect);
      scaleV := FLOAT(targetV)/FLOAT(actualV);
    BEGIN
      self.transform := Transform.Compose(self.transform,
        Transform.IsoScale(MIN(scaleH, scaleV)));
    END;
    self.rect := Nothing;
  END Size;

PROCEDURE Shape(self: T; axis: Axis.T; n: CARDINAL): VBT.SizeRange =
  BEGIN
    LOCK self.m DO
      GetRect(self);
      VAR
        size := Rect.Size(axis, self.rect);
        other := Rect.Size(Axis.Other[axis], self.rect);
      BEGIN
        IF n # 0 THEN
          size := (size * n) DIV other;
        END;
        S("shape: n = " & Fmt.Int(n) & ",  size = " & Fmt.Int(size) & 
          ",  axis = " & ARRAY Axis.T OF TEXT{"H","V"}[axis]);
        RETURN VBT.SizeRange{16, size, 8192};
      END;
    END;
  END Shape;

PROCEDURE GetTransform(self: T): Transform.T =
  BEGIN
    LOCK self.m DO
      RETURN self.transform;
    END;
  END GetTransform;

BEGIN
END DCPaneVBT.
