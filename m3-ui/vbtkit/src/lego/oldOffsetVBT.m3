(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Mon Sep 11 14:47:13 PDT 1995 by steveg   *)
(*      modified on Thu Jul 27 12:28:29 PDT 1995 by mhb      *)
(*      modified on Tue Apr 18 13:28:50 PDT 1995 by rustan   *)
(*      modified on Thu May  7 11:45:44 PDT 1992 by harrison *)
(*      modified on Mon Feb 24 13:55:18 PST 1992 by muller   *)
(*      modified on Sun Nov 10 19:22:25 PST 1991 by gnelson  *)
(*      modified on Wed Sep 11 15:47:06 PDT 1991 by msm      *)
<*PRAGMA LL*>

MODULE OffsetVBT;

IMPORT Axis, Filter, FilterClass, MouseSplit, PaintOp, Pixmap, Point,
       VBTClass, VBT, Rect, Region, Trestle, Batch, BatchUtil, ScrnCursor,
       ScrnPixmap, VBTRep;

REVEAL 
  T = TPublic BRANDED OBJECT 
    delta: Point.T;
    (* child coord + delta = parent coord. *)
    (* v.delta is protected both by v and by VBT.mu.
       (If v is disconnected, these locks are not necessary). *)
    screen: VBT.ScreenID := -1;
    north, west: REAL;
    bg: PaintOp.T;

    (* from MouseSplit.MouseRef *)
    (* All fields protected by VBT.mu; mouseFocus, current, cache, and
      tracking are also protected by self *)
    mouseFocus: VBT.T := NIL;
    current: VBT.T := NIL;
    (* the child containing the last reported position of the
       cursor, or NIL if this position was not over any child. *)
    cache: VBT.Cage := VBT.GoneCage;
    (* If meth is the mouseRef of the VBT c and meth.cache.inOut =
       {FALSE} then for all points p in meth.cache.rect, Locate(c, p,
       ...)  returns meth.current.  Otherwise meth.cache.inOut = {TRUE},
       and the last position received by the parent was gone.  In any
       case, if meth.cache is non-empty then it contains the last
       position received by the parent.  *)
    tracking: BOOLEAN := FALSE;
    (* TRUE if some child other than current or the mouseFocus
       has a cage that does not contain GoneCage. *)
  OVERRIDES
    (* VBT down methods *)
    reshape := Reshape;
    repaint := Repaint;
    rescreen := Rescreen;
    mouse := Mouse;
    position := Position;
    redisplay := Redisplay;
    shape := Shape;
    locate := Locate;

    (* VBT up methods *)
    setcage := Setcage;
    setcursor := Setcursor;
    paintbatch := PaintBatch;
    capture := Capture;
    screenOf := ScreenOf;
    newShape := NewShape;

    (* split methods *)
    beChild := BeChild;
    init := Init;

    (* ScrollVBT methods *)
    move := Move;
  END;

PROCEDURE Init (v: T; ch: VBT.T; north, west: REAL; bg: PaintOp.T): T =
  BEGIN
    LOCK v DO
      v.north := north;
      v.west := west;
      v.delta := Rect.NorthWest(v.domain);
      v.bg := bg;
      IF v.st # NIL THEN
        v.delta :=
          Point.Sub(
            v.delta, Point.T{ROUND(VBT.MMToPixels(v, west, Axis.T.Hor)),
                             ROUND(VBT.MMToPixels(v, north, Axis.T.Ver))});
      END;
    END;
    EVAL Filter.Replace(v, ch);
    RETURN v
  END Init;

PROCEDURE BeChild(v: T; ch: VBT.T) RAISES {} =
  BEGIN
    Filter.T.beChild(v, ch); 
    VBTClass.ClearShortCircuit(ch)
  END BeChild;
    
PROCEDURE Repaint (prnt: T; READONLY rgn: Region.T) RAISES {} =
  BEGIN                          (* LL = VBT.mu *)
    IF prnt.ch # NIL THEN
      VBTClass.Repaint(prnt.ch, Region.Sub(rgn, prnt.delta));
      PaintWhite(prnt, rgn.r)
    END
  END Repaint;

PROCEDURE PaintWhite (prnt: T; READONLY r: Rect.T) =
  VAR a: Rect.Partition;
  BEGIN
    IF prnt.ch # NIL THEN
      Rect.Factor(r, Rect.Move(prnt.ch.domain, prnt.delta), a, 0, 0);
      a[2] := a[4];
      VBT.PolyTexture(
        prnt, SUBARRAY(a, 0, 4), prnt.bg, Pixmap.Solid, Point.Origin)
    END
  END PaintWhite;

PROCEDURE Rescreen (prnt: T; READONLY cd: VBT.RescreenRec) =
  VAR screen := Trestle.ScreenOf(prnt, Point.Origin).id;
  BEGIN
    LOCK prnt DO prnt.screen := screen END;
    VBT.Split.rescreen(prnt, cd);
    VBT.Mark(prnt);
  END Rescreen;

PROCEDURE Redisplay (prnt: T) =
  BEGIN
    IF prnt.ch # NIL THEN
      (* reshape child to preferred size *)
      WITH szs = VBTClass.GetShapes(prnt.ch),
           dom = Rect.FromSize(szs[Axis.T.Hor].pref, szs[Axis.T.Ver].pref) DO
        IF NOT Rect.Equal(prnt.ch.domain, dom) THEN
          VBTClass.Reshape(prnt.ch, dom, Rect.Empty);
          PaintWhite(prnt, VBT.Domain(prnt));
          InvalidateCache(prnt);
        END;
      END;
    END;
  END Redisplay;

PROCEDURE Reshape (prnt: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  VAR
    delta, deltaP: Point.T;
    chbad        : Region.T;
    saved        : Rect.T;
  BEGIN
    IF prnt.ch # NIL THEN
      IF cd.marked OR Rect.IsEmpty(cd.prev) THEN Redisplay(prnt); END;
      delta :=
        Point.Sub(
          Rect.NorthWest(cd.new),
          Point.T{ROUND(VBT.MMToPixels(prnt, prnt.west, Axis.T.Hor)),
                  ROUND(VBT.MMToPixels(prnt, prnt.north, Axis.T.Ver))});
      deltaP := Point.Sub(delta, prnt.delta);
      saved := Rect.Meet(Rect.Move(cd.saved, deltaP), cd.new);
      LOCK prnt.ch DO
        VBT.Scroll(prnt, saved, deltaP);
        prnt.delta := delta;
      END;
      chbad := Region.Difference(
                 Region.FromRect(prnt.domain), Region.FromRect(saved));
      Repaint(prnt, chbad);
    END;
  END Reshape;

PROCEDURE Shape (<* UNUSED *> prnt: T;
                 <* UNUSED *> ax  : Axis.T;
                 <* UNUSED *> n   : CARDINAL): VBT.SizeRange =
  BEGIN
    RETURN VBT.DefaultShape;
  END Shape;

PROCEDURE ScreenOf(
    prnt: T;
    <*UNUSED*> ch: VBT.T;
    READONLY pt: Point.T):
    Trestle.ScreenOfRec
    RAISES {} =
  BEGIN (* LL=ch *)
    RETURN Trestle.ScreenOf(prnt, Point.Add(pt, prnt.delta));
  END ScreenOf;

PROCEDURE Capture(
    prnt: T; 
    <*UNUSED*> ch: VBT.T; 
    READONLY rect: Rect.T;
    VAR (*out*) br: Region.T)
    : ScrnPixmap.T RAISES {} =
  VAR res: ScrnPixmap.T;
  BEGIN (* LL = ch *)
    res := VBT.Capture(prnt, Rect.Add(rect, prnt.delta), br);
    br := Region.Sub(br, prnt.delta);
    RETURN res
  END Capture;

PROCEDURE PaintBatch(prnt: T; <*UNUSED*> ch: VBT.T; ba: Batch.T) RAISES {} =
  BEGIN
    BatchUtil.Clip(ba);
    BatchUtil.Translate(ba, prnt.delta);
    VBTClass.PaintBatch(prnt, ba)
  END PaintBatch;

PROCEDURE NewShape(prnt: T; <* UNUSED *> ch: VBT.T) =
  BEGIN
    VBTRep.Mark(prnt);
  END NewShape;

PROCEDURE Move (prnt: T; north, west: REAL) =
  VAR
    delta, deltaP: Point.T;
    chbad        : Region.T;
  BEGIN
    LOCK prnt DO prnt.north := north; prnt.west := west; END;
    IF prnt.ch # NIL THEN
      delta := Point.Sub(
                 Rect.NorthWest(prnt.domain),
                 Point.T{ROUND(VBT.MMToPixels(prnt, west, Axis.T.Hor)),
                         ROUND(VBT.MMToPixels(prnt, north, Axis.T.Ver))});
      deltaP := Point.Sub(delta, prnt.delta);
      LOCK prnt.ch DO
        VBT.Scroll(prnt, prnt.domain, deltaP);
        prnt.delta := delta;
      END;
      chbad :=
        Region.Difference(Region.FromRect(prnt.domain),
                          Region.FromRect(Rect.Add(prnt.domain, deltaP)));
      Repaint(prnt, chbad);
    END
  END Move;

(* FROM MouseSplit *)

PROCEDURE Setcursor(v: T; ch: VBT.T) RAISES {} =
  VAR cs: ScrnCursor.T; BEGIN (* LL=ch *)
    LOCK v DO
      WITH r = v DO
        IF ch # r.mouseFocus AND
            (ch # r.current OR r.mouseFocus # NIL) 
        THEN
          RETURN
        END
      END
    END;
    cs := ch.getcursor();
    LOCK v DO
      WITH r = v DO
        IF (ch = r.mouseFocus OR 
            ch = r.current AND r.mouseFocus = NIL) 
        THEN
          SetCursor2(v, cs)
        END
      END
    END
  END Setcursor;

<*INLINE*> PROCEDURE SetCursor2(v: T; cs: ScrnCursor.T) RAISES {} =
  BEGIN (* LL=v *)
    IF cs # v.effectiveCursor THEN
      v.effectiveCursor := cs; 
      IF v.parent # NIL THEN v.parent.setcursor(v) END
    END
  END SetCursor2;

<*INLINE*> PROCEDURE SetCursor3(v: T; ch: VBT.T) RAISES {} =
  BEGIN
    IF ch # NIL THEN
      LOCK ch DO
        VAR cs := ch.getcursor(); BEGIN
          LOCK v DO SetCursor2(v, cs) END
        END
      END
    ELSE 
      LOCK v DO SetCursor2(v, ScrnCursor.DontCare) END
    END
  END SetCursor3;

(* Cage setting depends on the following invariants:

   (R1) v's cage is contained in the intersection of its children's 
        cages.  This guarantees that v will get a position whenever 
        any child is owed one.

   (R2) v's cage is contained in v.cache.  This guarantees that v will 
        get a position whenever "current" should be changed.

   (R3) v.tracking OR for each ch # v.mouseFocus AND ch # v.current,
        ch.cage contains GoneCage.  

    When the parent receives a position its cage is set arbitrarily,
    so the invariants are destroyed.  It sets its cage to satisfy R2,
    and then delivers positions to its children.  The SetCages which
    they do in response to the positions reestablish R1 and R3 before
    the parent Position returns.  *)

PROCEDURE Setcage (v: T; ch: VBT.T) RAISES {} =
  VAR cg := VBTClass.Cage(ch);
  BEGIN                          (*LL=ch*)
    LOCK v DO
      IF cg.rect # Rect.Full AND v.screen = cg.screen THEN
        cg.rect := Rect.Move(cg.rect, v.delta)
      END;
      WITH r          = v,
           notCurrent = (ch # r.current) DO
        IF NOT (notCurrent IN cg.inOut) THEN cg := VBT.EmptyCage END;
        cg.inOut := VBT.InOut{FALSE, TRUE};
        IF notCurrent AND (ch # r.mouseFocus)
             AND ((NOT Rect.Equal(cg.rect, Rect.Full))
                    OR (cg.screen # VBT.AllScreens)) THEN
          r.tracking := TRUE
        END;
        VBTClass.SetCage(v, cg);
      END
    END
  END Setcage;

PROCEDURE Position(v: T; READONLY cdIn: VBT.PositionRec)
  RAISES {} =
  VAR
    current, mouseFocus, newCurrent: VBT.T := NIL;
    cd := cdIn;
    goneCd: VBT.PositionRec;
    changed := TRUE;
    newCache: VBT.Cage;
    tracking := FALSE;
  BEGIN (* LL = VBT.mu *)
    IF v.screen # cd.cp.screen AND NOT cd.cp.offScreen THEN
      LOCK v DO
        v.screen := cd.cp.screen
      END;
    END;
    WITH r = v DO
        current := r.current;
        mouseFocus := r.mouseFocus;
        tracking := r.tracking;
      IF cd.cp.gone THEN
        changed := (current # NIL) OR (FALSE IN r.cache.inOut);
        newCurrent := NIL;
        newCache := VBT.GoneCage;
        VBT.SetCage(v, newCache)
      ELSIF  NOT VBT.Outside(cd.cp, r.cache) THEN
        changed := FALSE;
        newCurrent := current;
        VBT.SetCage(v, r.cache)
      ELSE
        newCurrent := v.locate(cd.cp.pt, newCache.rect);
        IF newCurrent # NIL THEN
          newCache.rect := Rect.Meet(newCache.rect, newCurrent.domain)
        ELSE
          newCache.rect := Rect.Meet(newCache.rect, v.domain)
        END;
        newCache.inOut := VBT.InOut{FALSE};
        newCache.screen := cd.cp.screen;
        VBT.SetCage(v, newCache)
      END;
      IF changed OR tracking THEN
        LOCK v DO
          r.current := newCurrent;
          r.cache := newCache;
          r.tracking := FALSE;
        END
      END
    END;
    IF NOT cd.cp.offScreen THEN
      cd.cp.pt := Point.Sub(cd.cp.pt, v.delta);
    END;
    goneCd := cd;
    goneCd.cp.gone := TRUE;
    IF current # newCurrent THEN
      (* possibly deliver "gone" to old current;
         possibly change cursors. *)
      IF current # NIL AND current # mouseFocus THEN
        VBTClass.Position(current, goneCd)
      END;
      IF mouseFocus = NIL THEN SetCursor3(v, newCurrent) END
    END;
    IF mouseFocus # NIL AND mouseFocus # newCurrent THEN
      VBTClass.Position(mouseFocus, goneCd)
    END;
    IF tracking THEN
      VAR ch := v.succ(NIL); BEGIN
        WHILE ch # NIL DO
          IF ch # mouseFocus AND ch # current AND ch # newCurrent THEN
            VBTClass.Position(ch, goneCd)
          END;
          ch := v.succ(ch)
        END
      END
    END;
    IF newCurrent # NIL THEN
      VBTClass.Position(newCurrent, cd)
    END
  END Position;

PROCEDURE BecomeMF(v: T; mf: VBT.T) =
  BEGIN
    LOCK v DO
      v.mouseFocus := mf
    END;
    IF mf # NIL THEN
      SetCursor3(v, mf)
    ELSE
      SetCursor3(v, v.current)
    END
  END BecomeMF;

PROCEDURE Mouse (v: T; READONLY cdIn: VBT.MouseRec) RAISES {} =
  VAR
    ch    : VBT.T;
    junk  : Rect.T;
    goneCd: VBT.MouseRec;
    cd                   := cdIn;
  BEGIN
    (* Set ch to the child containing the position of cd. *)
    WITH r = v DO
      IF cd.cp.gone THEN
        ch := NIL
      ELSIF (FALSE IN r.cache.inOut)
              AND Rect.Member(cd.cp.pt, r.cache.rect) THEN
        ch := r.current
      ELSE
        ch := v.locate(cd.cp.pt, junk)
      END;
      IF NOT cd.cp.offScreen THEN
        cd.cp.pt := Point.Sub(cd.cp.pt, v.delta)
      END;
      (* Deliver the mouse code. *)
      IF ch # NIL THEN VBTClass.Mouse(ch, cd) END;
      (* Possibly deliver cd to the mouseFocus *)
      IF r.mouseFocus # NIL AND r.mouseFocus # ch THEN
        goneCd := cd;
        goneCd.cp.gone := TRUE;
        VBTClass.Mouse(r.mouseFocus, goneCd)
      END
    END;
    (* reset the mouseFocus *)
    IF cd.clickType = VBT.ClickType.FirstDown THEN
      BecomeMF(v, ch)
    ELSIF cd.clickType = VBT.ClickType.LastUp THEN
      BecomeMF(v, NIL)
    END
  END Mouse;

PROCEDURE InvalidateCache(v: T) =
  BEGIN
    LOCK v DO
      WITH r = v DO
        IF (FALSE IN r.cache.inOut) THEN
           r.cache.rect := Rect.Empty;
           VBTClass.SetCage(v, r.cache)
        END
      END
    END
  END InvalidateCache;

PROCEDURE Locate (v: T; READONLY pt: Point.T; VAR (*OUT*) rect: Rect.T):
  VBT.T RAISES {} =
  VAR ch := v.succ(NIL);
  BEGIN
    rect := Rect.Full;
    WHILE ch # NIL DO
      (* (pt IN rect), rect doesn't intersect the domain of any predecessor
         of ch *)
      WITH r = Rect.Add(ch.domain, v.delta) DO
        IF r.west >= r.east THEN (* skip *)
        ELSIF pt.v < r.north THEN
          IF rect.south > r.north THEN rect.south := r.north END
        ELSIF pt.v >= r.south THEN
          IF rect.north < r.south THEN rect.north := r.south END
        ELSIF pt.h < r.west THEN
          IF rect.east > r.west THEN rect.east := r.west END
        ELSIF pt.h >= r.east THEN
          IF rect.west < r.east THEN rect.west := r.east END
        ELSE
          rect := Rect.Meet(rect, r);
          RETURN ch
        END;
        ch := v.succ(ch)
      END
    END;
    RETURN NIL
  END Locate;

BEGIN
END OffsetVBT.
