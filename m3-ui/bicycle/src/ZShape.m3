(* Copyright (C) 1992, Digital Equipment Corporation.               *)
(* All rights reserved *)
(* See the file COPYRIGHT for a full description *)
(* ZShape.m3, coded Fri Oct 31 11:24:53 1986 by cgn *)
<*PRAGMA LL*>

(* Last modified on Wed Feb 26 18:58:49 1992 by msm     *)
(*      modified on Sun Nov 10 18:14:50 PST 1991 by gnelson *)
(*      modified on Fri Feb  2 14:08:01 PST 1990 by glassman *)


UNSAFE MODULE ZShape;

(* Unsafe when it traverses paint batches. *)

IMPORT VBT, Rect, Split, ProperSplit, Point, PolyRegion, PaintPrivate,
Region, Batch, BatchUtil, BatchRep, Axis, ScrnPixmap, Interval,
VBTTuning, VBTClass, Word, VBTRep;

FROM PaintPrivate IMPORT PaintCommand;

REVEAL
  T = ZSplit.T BRANDED OBJECT
    (* Protection level VBT.mu *)
  OVERRIDES
    beChild := BeChild;
    repaint := Repaint;
    reshape := Reshape;
    redisplay := Redisplay;
  END;

TYPE 
  ZChild = ProperSplit.Child OBJECT
    (* Protection level VBT.mu *)
    shapeChanged, mapped := FALSE;
    (* The mapped bit is set if the child is mapped. *)
    (* zc.upRef.shapeChanged = TRUE implies that zc's newshape method
       has been called and therefore its shape method will be called
       in order to possibly change the dimensions of the child.  *)
    dom: ZDom := NIL;
    (* If zc.upRef.dom is non-NIL, then zc.upRef.dom.r is the 
       rectangle to which zc will be reshaped the next time
       zc.parent is redisplayed non-empty and zc is mapped. Also, 
       zc.upRef.dom.checked is set if the domain has been
       clipped into zc's shape range. *)
    reshapeControl: ZSplit.ReshapeControl := NIL;
    (* Protection level VBT.mu + ch *)
    clip: ZClip := NIL;
    (* If clip = NIL, this child is unobscured; otherwise
        clip.rgn is the child's visible region, and
        clip.cache is a subset of clip.rgn. *)
  END;
  Child = ZSplit.Child OBJECT
    reg := Region.Empty;
    translation := TRUE;
    regionControl: RegionControl := NIL;
    (* If zc.upRef.reg is Empty, the child is rectangular.  Otherwise,
       zc.upRef.reg is the region desired by the child that corresponds
       either to its dom or to its domain when dom is NIL. *)
  END;
  ZClip = REF RECORD cache: Rect.T := Rect.Empty; rgn: Region.T END;
  ZDom = REF RECORD r: Rect.T; checked, replacement := FALSE END;
  
PROCEDURE New(
    bg: VBT.T := NIL;
    saveBits := FALSE;
    parlim: INTEGER := -1): T = 
  BEGIN
    RETURN Be(NEW(T), bg, saveBits, parlim)
  END New;

PROCEDURE BeChild(v: T; ch: VBT.T) RAISES {} =
VAR  ur: Child;
  BEGIN
    IF ch.upRef = NIL THEN 
      ur := NEW(Child); 
      ch.upRef := ur
    ELSE
      ur := ch.upRef 
    END;
    ZSplit.T.beChild(v, ch)
  END BeChild;

PROCEDURE Repaint(v: T; READONLY rg: Region.T) RAISES {} =
  VAR ch := v.succ(NIL); rgn := rg; ur: Child;
  BEGIN
    WHILE (ch # NIL) AND NOT Region.IsEmpty(rgn) DO
      WITH ur = NARROW(ch.upRef, Child) DO
        IF Region.OverlapRect(ch.domain, rgn) THEN
	  ur := ch.upRef;
	  IF ur.clip = NIL THEN
            VBTClass.Repaint(ch, Region.MeetRect(ch.domain, rgn));
            rgn := Region.Difference(rgn, Region.FromRect(ch.domain))
	  ELSIF Region.Overlap(ur.clip.rgn, rgn) THEN
	    VBTClass.Repaint(ch, Region.Meet(ur.clip.rgn, rgn));
	    rgn := Region.Difference(rgn, ur.clip.rgn)
	  END
        END
      END;
      ch := v.succ(ch)
    END
  END Repaint;

PROCEDURE Reshape(v: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
  VAR ch: VBT.T; prev, old: Rect.T;
  BEGIN
    IF Rect.IsEmpty(cd.new) THEN
      v.oldDom := NEW(REF Rect.T);
      v.oldDom^ := cd.prev
    ELSIF v.oldDom = NIL THEN 
      old := cd.prev 
    ELSE
      old := v.oldDom^;
      v.oldDom := NIL
    END;
    IF NOT Rect.IsEmpty(cd.new) AND NOT Rect.Equal(cd.new, old) THEN
      ch := v.succ(NIL);
      WHILE ch # NIL DO
        WITH ur = NARROW(ch.upRef, Child) DO
          RememberDomain(ch, ur);
          prev := ur.dom.r;
          ur.dom.r := ur.reshapeControl.apply(ch, old, cd.new, prev);
          IF ur.shapeChanged THEN
            ur.dom.checked := FALSE;
            ur.shapeChanged := FALSE;
            VBTClass.ClearNewShape(ch)
          ELSIF ur.dom.checked THEN 
            ur.dom.checked := Congruent(prev, ur.dom.r);
	    IF ur.dom.checked THEN
	      IF ur.regionControl # NIL THEN
	        ur.reg := ur.regionControl.apply(ch, ur.dom.r)
	      END
	    END
          END 
        END;
        ch := v.succ(ch)
      END
    END;
    IF Congruent(cd.new, cd.prev) THEN
      Redisplay2(v, TRUE, TRUE, cd.saved,
        Point.Sub(Rect.NorthWest(cd.new), Rect.NorthWest(cd.prev)))
    ELSE
      Redisplay2(v, TRUE, FALSE, cd.saved, Point.Origin)
    END
  END Reshape;

PROCEDURE Redisplay(v: T) RAISES {} =
  BEGIN Redisplay2(v, FALSE, FALSE, v.domain, Point.Origin) END Redisplay;
  
 TYPE
   ChildRec = RECORD ch: VBT.T; ur: Child; clip: Clip; winner: BOOLEAN END;
 
PROCEDURE Redisplay2(v: T; inReshape, translation: BOOLEAN; READONLY
saved: Rect.T; READONLY delta: Point.T)
  RAISES {} =
  VAR ch := v.succ(NIL); numch := 0;
    a1: ARRAY [0..9] OF ChildRec;
    a2: REF ARRAY OF ChildRec;
    replacement := FALSE;
  BEGIN
    VBTClass.LocateChanged(v);
    IF Rect.IsEmpty(v.domain) THEN
      WHILE ch # NIL DO
        WITH ur = NARROW(ch.upRef, Child) DO
          RememberDomain(ch, ur);
          IF ur.clip # NIL THEN LOCK ch DO ur.clip := NIL END END
        END; 
        IF NOT Rect.IsEmpty(ch.domain) THEN
          VBTClass.Reshape(ch, Rect.Empty, Rect.Empty)
        END;
        ch := v.succ(ch)
      END;
      v.affected := PolyRegion.Empty;
      RETURN
    END;
    translation := translation AND Rect.IsEmpty(v.affected.r);
    (* Check domains, expand affected, and blow away unmapped windows *)
    WHILE ch # NIL DO
      WITH ur = NARROW(ch.upRef, Child) DO
        IF NOT ur.mapped THEN
          IF NOT Rect.IsEmpty(ch.domain) THEN
            translation := FALSE;
            RememberDomain(ch, ur);
            VAR oldDom := ch.domain; BEGIN
              VBTClass.Reshape(ch, Rect.Empty, Rect.Empty);
              IF NOT inReshape THEN
                IF ur.clip # NIL THEN
                  PolyRegion.JoinRgn(v.affected, ur.clip.rgn);
                  LOCK ch DO ur.clip := NIL END
                ELSE
                  PolyRegion.JoinRect(v.affected, oldDom)
                END
              END
            END
          END
        ELSE
          IF (ur.dom # NIL) OR ur.shapeChanged THEN
            Move2(ch, ur, GetDomain(ch));
            ur.shapeChanged := FALSE;
          END;
          IF inReshape THEN
            IF translation THEN
              IF ur.dom = NIL THEN
                translation := Point.Equal(delta, Point.Origin)
		  AND ur.translation;
              ELSE
                translation := Rect.Equal(Rect.Add(ch.domain, delta),
                 ur.dom.r) AND ur.translation;
              END
            END
          ELSIF (ur.dom # NIL) THEN
            IF ur.dom.replacement THEN
              replacement := TRUE
            ELSE
              PolyRegion.JoinRgn(v.affected,
                Region.SymmetricDifference(
                  Region.FromRect(ur.dom.r), Region.FromRect(ch.domain)))
            END
          END;
	  ur.translation := TRUE;
          IF (ur.dom # NIL) AND Rect.IsEmpty(ur.dom.r) THEN
            ur.dom := NIL;
            VBTClass.Reshape(ch, Rect.Empty, Rect.Empty);
            IF ur.clip # NIL THEN LOCK ch DO ur.clip := NIL END END
          ELSE
            INC(numch)
          END
        END
      END;
      ch := v.succ(ch)
    END;
    IF inReshape OR replacement OR NOT Rect.IsEmpty(v.affected.r) THEN 
      IF numch <= NUMBER(a1) THEN
        Redisplay3(v, a1, inReshape, translation, saved, delta)
      ELSE
        a2 := NEW(REF ARRAY OF ChildRec, numch);
        Redisplay3(v, a2^, inReshape, translation, saved, delta)
      END
    END
  END Redisplay2;

PROCEDURE ComputeClip(
  READONLY affected: Region.T; 
  VAR covered: PolyRegion.T;
  READONLY dom, pdom: Rect.T; 
  inReshape: BOOLEAN;
  oclip: Clip): Clip = 
  VAR cl, oc: Region.T; obs := PolyRegion.OverlapRect(covered, dom);
  BEGIN
    IF NOT obs AND Rect.Subset(dom, pdom) AND 
      ((oclip = NIL) OR inReshape OR Region.SubsetRect(dom, affected))
    THEN
      PolyRegion.JoinRect(covered, dom);
      RETURN NIL
    ELSE
      WITH ndom = Rect.Meet(dom, pdom) DO
        IF inReshape THEN
          cl := PolyRegion.Complement(covered, Region.FromRect(ndom))
        ELSE
          WITH af = Region.MeetRect(ndom, affected) DO
            IF obs THEN
              cl := PolyRegion.Complement(covered, af)
            ELSE
              cl := af
            END;
            IF NOT RegionEqRect(ndom, af) THEN
              IF oclip = NIL THEN
                oc := Region.FromRect(ndom)
              ELSE
                oc := Region.MeetRect(ndom, oclip.rgn)
              END;
              cl := Region.Join(cl, Region.Difference(oc, af))
            END
          END
        END;
        PolyRegion.JoinRect(covered, ndom)
      END;
      IF RegionEqRect(dom, cl) THEN
        RETURN NIL
      ELSIF Region.IsEmpty(cl) THEN
        RETURN EmptyClip
      ELSIF (oclip # NIL) AND Region.Equal(oclip.rgn, cl) THEN
        RETURN oclip
      ELSE
        RETURN NEW(Clip, rgn := cl)
      END
    END
  END ComputeClip;

<*INLINE*> PROCEDURE RegionEqRect(
    READONLY rect: Rect.T; 
    READONLY rgn: Region.T): BOOLEAN =
  BEGIN
    RETURN (rgn.p = NIL) AND Rect.Equal(rect, rgn.r)
  END RegionEqRect;
              
PROCEDURE ApplyClip(
  v: T; 
  VAR el: ChildRec; 
  READONLY dom: Rect.T;
  inReshape: BOOLEAN; 
  READONLY saved: Rect.T; 
  VAR secure:PolyRegion.T) =
  VAR nc: Clip;
  BEGIN
    WITH ur = el.ur DO
      IF ur.dom = NIL THEN
        (* set ch's clip to be meet of old and new clip, to
           prevent it from painting on windows that we are
           going to reshape. *)
        IF (el.clip = NIL) OR (ur.clip = EmptyClip) 
           OR (ur.clip = el.clip) THEN
          nc := ur.clip
        ELSIF (ur.clip = NIL) OR (el.clip = EmptyClip) THEN
          nc := el.clip
        ELSE
          nc := NEW(Clip, rgn := Region.Meet(el.clip.rgn, ur.clip.rgn))
        END;
        IF inReshape AND (nc = NIL) AND NOT Rect.Subset(dom, saved) THEN
          nc := NEW(Clip, rgn := Region.FromRect(Rect.Meet(dom, saved)))
        ELSIF inReshape AND (nc # NIL) AND 
              NOT Rect.Subset(nc.rgn.r, saved) THEN
          nc := NEW(Clip, rgn := Region.MeetRect(saved, nc.rgn))
        END;
        el.winner := FALSE  
      ELSIF v.saveBits AND (el.clip = NIL) AND (ur.clip = NIL)
        AND NOT Rect.IsEmpty(el.ch.domain) AND
        NOT PolyRegion.OverlapRect(secure, el.ch.domain) THEN
        el.winner := TRUE;
        PolyRegion.JoinRect(secure, dom);
        nc := NIL
      ELSE
        el.winner := FALSE;
        nc := EmptyClip;
      END;
      (* ch.clip := nc *)
      IF ur.clip # nc THEN
        LOCK el.ch DO 
          ur.clip := nc;
          VBTClass.ClearShortCircuit(el.ch)
        END
      END
    END
  END ApplyClip;
              
PROCEDURE Redisplay3(v: T; VAR a: ARRAY OF ChildRec; inReshape, translation:
  BOOLEAN; READONLY saved: Rect.T; READONLY delta: Point.T) =
  VAR ch := v.succ(NIL);
    covered := PolyRegion.Empty;
    secure := PolyRegion.Empty;
    affected, br: Region.T; nch := 0;
  BEGIN
    IF NOT inReshape THEN
      affected := PolyRegion.ToRegion(v.affected)
    END;
    v.affected := PolyRegion.Empty;
    (* Compute new regions. Find movers that don't get old domain, and 
       throttle them; also restrict painting on windows that get more 
       obscured. *)
    WHILE ch # NIL DO
      WITH ur = NARROW(ch.upRef, Child), nd = Domain(ch, ur) DO
        IF ur.mapped AND (inReshape OR (ur.dom # NIL) OR 
            Region.OverlapRect(nd, affected))
        THEN
          WITH el = a[nch] DO
            IF translation THEN
              IF (ur.clip = NIL) OR (ur.clip = EmptyClip) OR
                  Point.Equal(delta, Point.Origin) THEN
                el.clip := ur.clip
              ELSE
                el.clip := NEW(Clip, rgn := Region.Add(ur.clip.rgn, delta),
                  cache := Rect.Add(ur.clip.cache, delta))
              END
            ELSE
              el.clip := 
                ComputeClip(affected, covered, nd, v.domain, inReshape, 
		  ur.clip)
            END;
            IF (ur.dom # NIL) OR (el.clip # ur.clip) OR
               (inReshape AND NOT Rect.Subset(nd, saved)) THEN
              el.ch := ch;
              el.ur := ur;
              INC(nch);
              ApplyClip(v, el, nd, inReshape, saved, secure)
            END
          END
        END
      END;
      ch := v.succ(ch)
    END;
    (* Move the ones that get old domain *)
    IF v.saveBits THEN
      FOR i := 0 TO nch - 1 DO
        WITH el = a[i] DO
          IF el.winner THEN
            VBTClass.Reshape(el.ch, el.ur.dom.r, saved);
            el.ur.dom := NIL
          END
        END
      END
    END;
    (* Deliver badrects and move the rest of the children *)
    FOR i := 0 TO nch - 1 DO
      WITH el = a[i] DO
        IF NOT el.winner THEN
          IF (el.ur.dom = NIL) AND (el.ur.clip # el.clip) THEN
            IF el.clip = NIL THEN
	      IF el.ch.regionControl = NIL THEN
                br := Region.Difference(
                  Region.FromRect(el.ch.domain), el.ur.clip.rgn)
	      ELSE
	        br := Region.Difference(el.ch.reg, el.ur.clip.rgn)
            ELSE
              br := Region.Difference(el.clip.rgn, el.ur.clip.rgn)
            END;
            LOCK el.ch DO
              el.ur.clip := el.clip;
              VBTClass.ForceRepaint(el.ch, br, FALSE)
            END;
            VBTClass.Repaint(el.ch, Region.Empty)
          ELSIF el.ur.dom # NIL THEN
            LOCK el.ch DO el.ur.clip := el.clip END;
            VBTClass.Reshape(el.ch, el.ur.dom.r, Rect.Empty);
            el.ur.dom := NIL
          END
        END
      END
    END
  END Redisplay3;

PROCEDURE GetDomain(ch: VBT.T): Region.T =
  <*FATAL Split.NotAChild*>
  VAR lastChild := Split.Succ(ch.parent,ch) = NIL;
  BEGIN
    WITH ur = NARROW(ch.upRef, Child), r = Domain(ch, ur) DO
      IF ur.shapeChanged OR (ur.dom # NIL) AND NOT ur.dom.checked THEN
        WITH 
          s = VBTClass.GetShapes(ch, ur.shapeChanged),
          hor = s[Axis.T.Hor], ver = s[Axis.T.Ver]
        DO
          IF ur.shapeChanged AND NOT lastChild THEN
	    IF ur.regionControl = NIL THEN
              RETURN Rect.FromCorner(Rect.NorthWest(r.r), hor.pref, ver.pref)
	    ELSE
	      RETURN ur.regionControl.apply(ch,
	        Rect.FromCorner(Rect.NorthWest(r.r), hor.pref, ver.pref))
          ELSE
            WITH hsize= Rect.HorSize(r.r), vsize = Rect.VerSize(r.r),  
              width = MIN(hor.hi-1, MAX(hor.lo, hsize)),
              height = MIN(ver.hi-1, MAX(ver.lo, vsize))
            DO
              IF (width = hsize) AND (height = vsize) OR lastChild THEN
                IF ur.dom # NIL THEN ur.dom.checked := TRUE END;
                RETURN r
              END;
	      IF ur.regionControl = NIL THEN
                RETURN Rect.FromCorner(Rect.NorthWest(r.r), hor.pref, ver.pref)
	      ELSE
	        RETURN ur.regionControl.apply(ch,
	          Rect.FromCorner(Rect.NorthWest(r.r), hor.pref, ver.pref))
            END
          END
        END
      ELSE
        RETURN r
      END
    END
  END GetDomain;

<*INLINE*> PROCEDURE Domain(ch: VBT.T; ur: Child): Region.T =
  (* ur = ch.upRef. LL = VBT.mu*)
  VAR rg: Region.T; r: Rect.T;
  BEGIN
    IF ur.dom = NIL THEN r := ch.domain ELSE r := ur.dom.r END;
    IF ur.regionControl = NIL THEN RETURN r END;
    IF NOT ur.checked THEN
      rg := ur.regionControl.apply(ch, r);
      ur.translation := 
        Region.Equal(rg, 
	  Region.Move(ur.reg, 
	    Point.Sub(Rect.NorthWest(rg.r), Rect.NorthWest(ur.reg.r))));
      ur.reg := rg
    END;
    RETURN ur.reg
  END Domain;

<*INLINE*> PROCEDURE Congruent(READONLY r1, r2: Rect.T): BOOLEAN =
  BEGIN 
    RETURN 
      Rect.HorSize(r1) = Rect.HorSize(r2) AND
      Rect.VerSize(r1) = Rect.VerSize(r2) 
  END Congruent;  
    
PROCEDURE SetReshapeControl(
    ch: VBT.T;
    rc: ReshapeControl) =
  BEGIN
    WITH ur = NARROW(ch.upRef, Child) DO
      ur.reshapeControl := rc
    END
  END SetReshapeControl;

BEGIN
END ZShape.
