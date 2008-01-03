(* Copyright 1990 Digital Equipment Corporation. *)
(* Distributed only by permission. *)

(* Lectern: a user interface for viewing documents stored as images *)
(* VBT for viewing images *)

(* Last modified on Mon Mar 27 13:28:26 PST 1995 by birrell   *)

MODULE ImageVBT EXPORTS ImageVBT;

IMPORT
  Axis,
  Images,
  Math,
  PaintOp, Pixmap, Point,
  Rect, Region,
  Thread, Time,
  VBT, VBTClass;

REVEAL T = Public BRANDED OBJECT
    (* Locking: client-defined state is protected by .imageMu+.readMu so it
       can be read under either lock but writing requires both.  Other state
       is protected only by .imageMu.
       The Paint subroutine does not require or acquire .imageMu; this
       allows a concurrent call of .put while painting, which will cancel an
       asynchronous paint.
       Also .get and .getDelta acquire only .readMu, so they can be
       called from the paint method (handy for sub-classes). *)
    imageMu: MUTEX := NIL;
    readMu: MUTEX := NIL;
    (* fields protected by imageMu+readMu: *)
    pm: Images.T;                     (* The pixmap to be displayed *)
    bg: PaintOp.T := PaintOp.Bg;      (* painting areas outside the pixmap *)
    shadow := FALSE;                  (* whether to paint a drop-shadow *)
    paintChunk := 0;                  (* for pm.paint *)
    delta: Point.T;                   (* Offset to pixmap's origin *)
    highlight := Region.Empty;        (* highlight after moveTo *)
    (* fields protected by imageMu: *)
    unhighlighter: Thread.T := NIL;   (* will remove .highlight *)
    redisplayer: Thread.T := NIL;     (* asynchronous painting thread *)
    willRedisplay := FALSE;           (* A forked thread will redisplay *)
    redisplaying := FALSE;            (* Forked thread is writing pixels *)
    redisplayDone: Thread.Condition := NIL; (* end of forked thread *)
  OVERRIDES
    init := Init;
    put := Put;
    get := Get;
    getDelta := GetDelta;
    moveTo := MoveTo;
    redisplay := Redisplay;
    repaint := Repaint;
    shape := Shape;
    flash := Flash;
    paint := Paint;
  END;


(* *)
(* Internal subroutines *)
(* *)

CONST
  ShadowSize = 2; (* thickness of drop-shadow *)
  HighlightSize = 2; (* thickness of highlight added by MoveTo *)

PROCEDURE PaintShadow(v: VBT.Leaf; domain, imageDomainInV: Rect.T) =
    (* LL <= VBT.mu *)
    (* Paint a drop-shadow around imageDomainInV, iff it fits into domain *)
  VAR
    imDomainPlusShadow := Rect.Change(imageDomainInV,
                                      -2, 2+ShadowSize,
                                      -2, 2+ShadowSize);
  BEGIN
    IF Rect.Subset(imDomainPlusShadow, domain) THEN
      WITH
        top = Rect.T{west := imageDomainInV.west-1,
                     east := imageDomainInV.east+1,
                     north := imageDomainInV.north-1,
                     south := imageDomainInV.north},
        right1 = Rect.T{west := top.east-1,
                        east := top.east,
                        north := top.south,
                        south := top.north+ShadowSize},
        right2 = Rect.T{west := right1.west,
                        east := right1.east+ShadowSize,
                        north := right1.south,
                        south := imageDomainInV.south},
        left = Rect.T{west := top.west,
                      east := top.west+1,
                      north := top.south,
                      south := right2.south},
        bottom1 = Rect.T{west := left.west,
                         east := left.west+ShadowSize,
                         north := left.south,
                         south := left.south+1},
        bottom2 = Rect.T{west := bottom1.east,
                         east := right2.east,
                         north := bottom1.north,
                         south := bottom1.south+ShadowSize} DO
        VBT.PaintTint(v, top, PaintOp.Fg);
        VBT.PaintTint(v, right1, PaintOp.Fg);
        VBT.PaintTint(v, right2, PaintOp.Fg);
        VBT.PaintTint(v, bottom1, PaintOp.Fg);
        VBT.PaintTint(v, bottom2, PaintOp.Fg);
        VBT.PaintTint(v, left, PaintOp.Fg);
      END;
    END;
  END PaintShadow;

PROCEDURE Paint(v: T; READONLY rgn: Region.T)
               RAISES { Thread.Alerted, Images.Error } =
    (* LL <= v.imageMu *)
    (* Paint image into rgn in v, with image's center offset delta from v's *)
  VAR
    pm: Images.T;
    bg: PaintOp.T;
    delta: Point.T;
    paintChunk: INTEGER;
    domain := VBT.Domain(v);
    bounds: Rect.T;
    imageDelta: Point.T;
    imageDomainInV: Rect.T;
    rects := Region.ToRects(rgn);
    a: Rect.Partition;
  BEGIN
    IF NOT Rect.IsEmpty(domain) THEN
      LOCK v.readMu DO
        pm := v.pm;
        bg := v.bg;
        delta := v.delta;
        paintChunk := v.paintChunk;
      END;
      bounds := pm.domain(v);
      imageDelta := Point.Add(delta,
                              Point.Sub(Rect.Middle(domain),
                                        Rect.Middle(bounds)));
      imageDomainInV := Rect.Move(bounds, imageDelta);
      FOR i := 0 TO LAST(rects^) DO
        Rect.Factor(Rect.Meet(domain, rects[i]), imageDomainInV, a, 0, 0);
        pm.paint(v, a[2], imageDelta, paintChunk);
        a[2] := Rect.Empty;
        VBT.PolyTexture(v, a, bg, Pixmap.Solid)
      END;
      LOCK v.readMu DO
        IF v.shadow THEN PaintShadow(v, domain, imageDomainInV) END;
        IF NOT Region.IsEmpty(v.highlight) THEN
          VBT.PaintRegion(v, Region.Meet(rgn, v.highlight), PaintOp.Swap);
        END;
      END;
    END;
  END Paint;

TYPE UnhilighterClosure = Thread.Closure OBJECT
    v: T;
    expiry: Time.T;
  OVERRIDES
    apply := ForkedUnhighlighter;
  END;

PROCEDURE ForkedUnhighlighter(self: UnhilighterClosure): REFANY =
    (* LL = 0 *)
  BEGIN
    TRY
      Thread.AlertPause(self.expiry-Time.Now());
      LOCK self.v.imageMu DO
        (* It's possible that the highlight changed before we acquired the
           lock *)
        IF self.v.unhighlighter = Thread.Self() THEN
          VBT.ForceRepaint(self.v, self.v.highlight);
          LOCK self.v.readMu DO
            self.v.highlight := Region.Empty;
          END;
        END;
      END;
    EXCEPT Thread.Alerted =>
    END;
    LOCK self.v.imageMu DO
      IF self.v.unhighlighter = Thread.Self() THEN
        self.v.unhighlighter := NIL;
      END;
    END;
    RETURN NIL;
  END ForkedUnhighlighter;

TYPE RedisplayerClosure = Thread.Closure OBJECT
    v: T;
  OVERRIDES
    apply := ForkedRedisplay;
  END;

PROCEDURE ForkedRedisplay(self: RedisplayerClosure): REFANY =
    (* LL = 0 *)
  BEGIN
    LOCK self.v.imageMu DO
      WHILE self.v.redisplaying DO
        Thread.Wait(self.v.imageMu, self.v.redisplayDone);
      END;
      self.v.willRedisplay := FALSE;
      self.v.redisplaying := TRUE;
    END;
    TRY
      Thread.AlertPause(0.050D0); (* allow other processing to go first *)
      self.v.paint(Region.Full);
    EXCEPT
    | Thread.Alerted => VBT.Mark(self.v);
    | Images.Error =>
    END;
    LOCK self.v.imageMu DO
      self.v.redisplaying := FALSE;
      Thread.Broadcast(self.v.redisplayDone);
      IF self.v.redisplayer = Thread.Self() THEN self.v.redisplayer := NIL END;
    END;
    RETURN NIL;
  END ForkedRedisplay;


(* *)
(* Exported methods *)
(* *)

PROCEDURE Init(v: T; pm: Images.T; bg: PaintOp.T;
               shadow := FALSE; paintChunk := 0): T =
    (* LL < v.imageMu *)
  BEGIN
    IF v.imageMu = NIL THEN
      v.imageMu := NEW(MUTEX);
      v.readMu := NEW(MUTEX);
      v.redisplayDone := NEW(Thread.Condition);
    END;
    LOCK v.imageMu DO
      LOCK v.readMu DO
        v.delta := Point.T{0,0};
        v.pm := pm;
        v.bg := bg;
        v.shadow := shadow;
        v.paintChunk := paintChunk;
      END;
    END;
    RETURN v
  END Init;

PROCEDURE Put(v: T; pm: Images.T; bg: PaintOp.T;
              shadow := FALSE; paintChunk := 0) =
    (* LL = VBT.mu *)
  VAR oldBounds: Rect.T;
  BEGIN
    LOCK v.imageMu DO
      IF v.redisplayer # NIL THEN Thread.Alert(v.redisplayer) END;
      oldBounds := v.pm.domain(v);
      LOCK v.readMu DO
        v.pm := pm;
        v.bg := bg;
        v.shadow := shadow;
        v.paintChunk := paintChunk;
      END;
      IF NOT Rect.Equal(oldBounds, v.pm.domain(v)) THEN VBT.NewShape(v) END;
      VBT.Mark(v);
    END;
  END Put;

PROCEDURE Get(v: T): Images.T =
    (* LL < v.readMu *)
  BEGIN
    LOCK v.readMu DO RETURN v.pm END;
  END Get;

PROCEDURE GetDelta(v: T): Point.T =
    (* LL < v.readMu *)
  BEGIN
    LOCK v.readMu DO RETURN v.delta END;
  END GetDelta;

PROCEDURE MoveTo(v: T; delta: Point.T;
                 pixelRate := 0.0; highlighting := FALSE) =
    (* LL = VBT.mu *)
  CONST TimePerStep = 0.033D0; (* ideal time for each step: 30 frames/second *)
  VAR
    dom: Rect.T;
    distance: LONGREAL;
    finish: Time.T;
    movement: Point.T;
    nonHighlight: Rect.T;
  PROCEDURE MoveBy(amount: Point.T) RAISES { Thread.Alerted, Images.Error } =
    VAR badR := Region.Difference(Region.FromRect(dom),
                                  Region.FromRect(Rect.Add(dom, amount)));
    BEGIN
      IF amount # Point.Origin THEN
        VBT.Scroll(v, dom, amount);
        LOCK v.readMu DO
          v.delta := Point.Add(v.delta, amount);
          IF NOT Region.IsEmpty(v.highlight) THEN
            v.highlight := Region.Add(v.highlight, amount);
          END;
        END;
        v.paint(badR);
        VBT.Sync(v, FALSE); (* flush to process boundary *)
      END;
    END MoveBy;
  BEGIN
    LOCK v.imageMu DO
      dom := VBT.Domain(v);
      movement := Point.Sub(delta, v.delta);
      TRY
        IF highlighting THEN
          VAR oldHighlight := v.highlight;
          BEGIN
            IF NOT Region.IsEmpty(oldHighlight) THEN (* repaint it *)
              LOCK v.readMu DO v.highlight := Region.Empty END;
              v.paint(oldHighlight);
            END;
          END;
          IF v.unhighlighter # NIL THEN (* get rid of old unhighlighter *)
            Thread.Alert(v.unhighlighter);
            v.unhighlighter := NIL;
          END;
          nonHighlight := dom;
          IF movement.h > 0 THEN INC(nonHighlight.west, HighlightSize) END;
          IF movement.v > 0 THEN INC(nonHighlight.north, HighlightSize) END;
          IF movement.h < 0 THEN DEC(nonHighlight.east, HighlightSize) END;
          IF movement.v < 0 THEN DEC(nonHighlight.south, HighlightSize) END;
          LOCK v.readMu DO
            v.highlight := Region.Difference(Region.FromRect(dom),
                                             Region.FromRect(nonHighlight));
          END;
          VBT.PaintRegion(v, v.highlight, PaintOp.Swap);
        END;
        IF VBT.IsMarked(v) OR v.willRedisplay OR Rect.IsEmpty(dom) THEN
          (* Forked thread will repaint everything in due course *)
          LOCK v.readMu DO v.delta := delta END;
        ELSIF v.redisplaying THEN
          (* Forked thread is mangling pixels; schedule a redisplay *)
          IF v.delta # delta THEN
            LOCK v.readMu DO v.delta := delta END;
            VBT.Mark(v);
          END;
        ELSE
          distance := Math.sqrt(FLOAT(Point.DistSquare(delta, v.delta),
                                      LONGREAL));
          finish := Time.Now() + distance * FLOAT(pixelRate, LONGREAL);
          WHILE v.delta # delta DO
            WITH
              stepStart = Time.Now(),
              remainingSteps = MAX(1, FLOOR((finish-stepStart)/TimePerStep)) DO
              MoveBy(Point.Div(Point.Sub(delta, v.delta), remainingSteps));
              Thread.AlertPause(stepStart+TimePerStep-Time.Now());
            END;
          END;
        END;
      EXCEPT
      | Thread.Alerted => LOCK v.readMu DO v.delta := delta END; VBT.Mark(v);
      | Images.Error => (* ignore; there aren't any pixels *)
      END;
      IF highlighting THEN
        (* Start the unhighlighting timer after we've finished scrolling *)
        v.unhighlighter := Thread.Fork(NEW(UnhilighterClosure,
                                       v := v,
                                       expiry := Time.Now() + 0.500D0));
      END;
    END;
  END MoveTo;

PROCEDURE Redisplay(v: T) =
    (* LL = VBT.mu *)
  BEGIN
    LOCK v.imageMu DO
      IF NOT v.willRedisplay THEN
        IF v.redisplayer # NIL THEN Thread.Alert(v.redisplayer) END;
        v.redisplayer := Thread.Fork(NEW(RedisplayerClosure, v := v));
        v.willRedisplay := TRUE;
      END;
    END;
  END Redisplay;

PROCEDURE Repaint(v: T; READONLY rgn: Region.T) =
    (* LL = mu.v *)
  BEGIN
    LOCK v.imageMu DO
      IF v.willRedisplay THEN
        (* Forked thread will repaint everything in due course *)
      ELSIF v.redisplaying THEN
        (* Forked thread is mangling pixels; schedule a redisplay *)
        VBT.Mark(v);
        IF v.redisplayer # NIL THEN Thread.Alert(v.redisplayer) END;
      ELSE
        TRY
          v.paint(rgn);
        EXCEPT
        | Thread.Alerted => VBT.Mark(v); Thread.Alert(Thread.Self());
        | Images.Error => (* ignore *)
        END;
      END;
    END;
  END Repaint;

PROCEDURE Shape(v: T; ax: Axis.T; <*UNUSED*> n: CARDINAL): VBT.SizeRange =
    (* LL = VBT.mu.v *)
  VAR sr: VBT.SizeRange;
  BEGIN
    LOCK v.imageMu DO
      sr.pref := Rect.Size(ax, v.pm.domain(v));
      sr.lo := sr.pref;
      sr.hi := sr.pref+1;
    END;
    RETURN sr
  END Shape;

PROCEDURE Flash(v: T) =
    (* LL = VBT.mu *)
  BEGIN
    LOCK v.imageMu DO
      WHILE v.willRedisplay OR v.redisplaying DO
        Thread.Wait(v.imageMu, v.redisplayDone);
      END;
      VBT.PaintTint(v, Rect.Full, PaintOp.Swap);
      VBT.Sync(v);
      Thread.Pause(0.2D0);
      VBT.PaintTint(v, Rect.Full, PaintOp.Swap);
      VBT.Sync(v);
    END;
  END Flash;

BEGIN

END ImageVBT.
