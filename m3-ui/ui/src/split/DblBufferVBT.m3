(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Mon Jan 15 13:09:17 PST 1996 by heydon                   *)
(*      modified on Fri Jul  8 17:10:10 PDT 1994 by msm                      *)
<* PRAGMA LL *>

MODULE DblBufferVBT;

IMPORT Filter, FilterClass, VBTClass, VBT, VBTRep, Point, Rect, Region,
  Trestle, TrestleComm, InstalledVBT, Batch, BatchUtil, ScrnPixmap,
  MouseSplit, DblBufferUtil, PaintOp, HighlightVBT;

(* A "DblBufferVBT.T" "v" is implemented by creating a VBT "offscreen(v)" that
   is installed off-screen. The "paintbatch" method is overridden to forward
   paint batches to "offscreen(v)". The operation "VBT.Sync(v)" updates the
   on-screen VBT from the portion of "offscreen(v)" that has changed since the
   last update.

   Because "offscreen(v)" is installed off screen, the northwest corner of its
   domain has the coordinates "(0, 0)". The "reshape" method arranges for the
   double-buffer's child's domain to agree with the domain of "offscreen(v)".

   The double-buffer maintains a vector "delta(v)", which is the difference
   between the parent's and child's coordinate systems. In practice, "delta(v)
   = Rect.Northwest(VBT.Domain(v))".

   The double-buffer maintains two rectangles "screenDiff(v)" and
   "savedDiff(v)" that satisfy the following invariants:

|    I1: (A p: offscreen(v)(p) # screen(v)(p) => p IN screenDiff(v))
|    I2: (A p: offscreen(v)(p) # savedBuff(v)(p) => p IN savedDiff(v))

   savedBuff(v) = NIL represents the state where the saved buffer is all
   background.

   If either "offScreen(v)" or "savedBuff(v)" are non-NIL, their domains are
   congruent to "v"'s domain.
*)

REVEAL 
  T = Filter.T BRANDED OBJECT 
    <* LL >= { VBT.mu.SELF, SELF } *>
    delta := Point.Origin;          (* child coord + delta = parent coord. *)
    screenId: VBT.ScreenID := -1;
    <* LL >= { SELF } *>
    offScreen, savedBuff: VBT.T := NIL;
    screenDiff, savedDiff: Rect.T;            (* both in child coordinates *)
  OVERRIDES
    (* split methods *)
    <* LL >= {VBT.mu, SELF, ch} *>
    beChild := BeChild;

    (* VBT down methods *)
    <* LL.sup = VBT.mu.SELF *>
    reshape := Reshape;
    repaint := Repaint;
    rescreen := Rescreen;
    <* LL.sup = VBT.mu *>
    mouse := Mouse;
    position := Position;

    (* VBT up methods *)
    <* LL.sup = ch *>
    setcage := SetCage;
    setcursor := SetCursor;
    paintbatch := PaintBatch;
    sync := Sync;
    capture := Capture;
    screenOf := ScreenOf;
  END;

VAR showSyncRect := FALSE;
(* For debugging: when "showSyncRect" is true, a highlight rectangle is drawn
   to frame the rectangle that is copied when the on-screen VBT is updated
   from the off-screen VBT. *)

(* Split Method Implementations -------------------------------------------- *)

PROCEDURE BeChild(v: T; ch: VBT.T) RAISES {} =
  <* LL >= {VBT.mu, v, ch} *>
  BEGIN
    Filter.T.beChild(v, ch); 
    VBTClass.ClearShortCircuit(ch)
  END BeChild;

(* Down Method Implementations --------------------------------------------- *)

(* In the down direction, argument points and rectangles must be translated
   from the parent's coordinate system to the child's coordinate system. This
   is accomplished by subtracting "delta(prnt)". *)

PROCEDURE Reshape(prnt: T; READONLY cd: VBT.ReshapeRec) RAISES {} =
(* Create and install a new off-screen VBT with "prnt"'s width and height, set
   the new value "delta(prnt)", and recursively reshape the child so it has
   the same domain as "offscreen(prnt)". *)
  <* LL.sup = VBT.mu.prnt *>
  VAR
    child := prnt.ch;
    delta := Rect.NorthWest(cd.new);
  BEGIN
    LOCK prnt DO prnt.delta := delta END;
    IF Rect.Congruent(cd.new, cd.prev)
       AND NOT Rect.IsEmpty(cd.new) THEN
      VAR offScreen: VBT.T; BEGIN
        LOCK prnt DO offScreen := prnt.offScreen END;
        PaintVBTtoVBT(prnt, cd.new, offScreen, delta);
        LOCK prnt DO prnt.screenDiff := Rect.Empty END
      END
    ELSE
      ReshapeOffScreen(prnt);
      ReshapeSavedBuff(prnt);
      IF child # NIL THEN
        VBTClass.Reshape(child, Rect.Sub(cd.new, delta), Rect.Empty)
      END
    END
  END Reshape;

PROCEDURE Repaint(prnt: T; READONLY rgn: Region.T) RAISES {} =
(* Merge "rgn" with "screenDiff(prnt)", and then update the on-screen VBT. *)
  <* LL.sup = VBT.mu.prnt *>
  BEGIN
    LOCK prnt DO
      prnt.screenDiff :=
        Rect.Join(prnt.screenDiff, Rect.Sub(rgn.r, prnt.delta))
    END;
    Update(prnt)
  END Repaint;

PROCEDURE Rescreen(prnt: T; READONLY cd: VBT.RescreenRec) =
(* Cache the current screen-id, and then call the parent type's "rescreen"
   method. *)
  <* LL.sup = VBT.mu.prnt *>
  VAR screen := Trestle.ScreenOf(prnt, Point.Origin); BEGIN
    LOCK prnt DO prnt.screenId := screen.id END;
    Filter.T.rescreen(prnt, cd)
  END Rescreen;

PROCEDURE Mouse(prnt: T; READONLY cd: VBT.MouseRec) RAISES {} =
(* If "NOT cd.cp.gone", invoke the parent type's "mouse" method with the mouse
   location translated by "-delta(prnt)". *)
  <* LL.sup = VBT.mu *>
  VAR cdP: VBT.MouseRec; child := prnt.ch; BEGIN
    IF prnt.ch # NIL THEN
      cdP := cd;
      IF NOT cd.cp.gone THEN
        cdP.cp.pt := Point.Sub(cdP.cp.pt, prnt.delta)
      END;
      VBTClass.Mouse(child, cdP)
    END
  END Mouse;

PROCEDURE Position(prnt: T; READONLY cd: VBT.PositionRec) RAISES {} =
(* If "NOT cd.cp.offScreen", invoke the parent type's "position" method with
   the mouse location translated by "-delta(prnt)". *)
  <* LL.sup = VBT.mu *>
  VAR cdP: VBT.PositionRec; child := prnt.ch; BEGIN
    IF prnt.ch # NIL THEN
      cdP := cd;
      IF NOT cd.cp.offScreen THEN
        cdP.cp.pt := Point.Sub(cd.cp.pt, prnt.delta)
      END;
      VBTClass.Position(child, cdP)
    END
  END Position;

(* Up Method Implementations ----------------------------------------------- *)

(* In the up direction, argument points and rectangles must be translated
   from the child's coordinate system to the parent's coordinate system. This
   is accomplished by adding "delta(prnt)". *)

PROCEDURE SetCage(prnt: T; ch: VBT.T) RAISES {} =
(* If the child "ch"'s cage is non-trivial and refers to the same screen as
   that of its parent "prnt", then translate the cage to parent coordinates
   and recursively propagate the message up the VBT tree. *)
  <* LL.sup = ch *>
  VAR cg := VBTClass.Cage(ch); BEGIN
    LOCK prnt DO
      IF cg.rect # Rect.Full AND prnt.screenId = cg.screen THEN 
        cg.rect := Rect.Add(cg.rect, prnt.delta)
      END;
      VBTClass.SetCage(prnt, cg)
    END
  END SetCage;

PROCEDURE SetCursor(prnt: T; ch: VBT.T) RAISES {} =
  VAR cs := ch.getcursor();
  BEGIN (* LL=ch *)
    LOCK prnt DO
      IF cs # prnt.effectiveCursor THEN
        prnt.effectiveCursor := cs;
        IF prnt.parent # NIL THEN prnt.parent.setcursor(prnt) END
      END
    END
  END SetCursor;

PROCEDURE PaintBatch(prnt: T; <*UNUSED*> ch: VBT.T; ba: Batch.T) RAISES {} =
(* Merge "screenDiff(prnt)" with a bounding box of the painting commands "ba";
   then forward the paint batch to "offscreen(prnt)". *)
  <* LL.sup = ch *>
  VAR offScreen: VBT.T; clip: Rect.T; BEGIN
    DblBufferUtil.Tighten(ba);
    clip := BatchUtil.GetClip(ba);
    LOCK prnt DO
      offScreen := prnt.offScreen;
      prnt.screenDiff := Rect.Join(prnt.screenDiff, clip);
      prnt.savedDiff := Rect.Join(prnt.savedDiff, clip)
    END;
    VBTClass.PaintBatch(offScreen, ba)
  END PaintBatch;

PROCEDURE Sync(prnt: T; <*UNUSED*> ch: VBT.T; wait: BOOLEAN) =
(* Update the on-screen VBT from "offscreen(prnt)". *)
  <* LL.sup = ch *>
  BEGIN
    Update(prnt, wait)
  END Sync;

PROCEDURE Capture(
    prnt: T; 
    <*UNUSED*> ch: VBT.T; 
    READONLY rect: Rect.T;
    VAR (*OUT*) br: Region.T)
    : ScrnPixmap.T RAISES {} =
(* The rectangle "rect" is in "ch"'s coordinate system. Capture the rectangle
   "rect" from the VBT "offscreen(prnt)". *)
  <* LL.sup = ch *>
  VAR offScreen: VBT.T; BEGIN
    LOCK prnt DO offScreen := prnt.offScreen END;
    RETURN VBT.Capture(offScreen, rect, br)
  END Capture;

PROCEDURE ScreenOf(
    prnt: T;
    <*UNUSED*> ch: VBT.T;
    READONLY pt: Point.T)
    : Trestle.ScreenOfRec RAISES {} =
(* The point "pt" is in "ch"'s coordinate system. Recurse on "prnt" with the
   point "pt" translated by "delta(prnt)". *)
  <* LL.sup = ch *>
  VAR delta: Point.T; BEGIN
    LOCK prnt DO delta := prnt.delta END;
    RETURN Trestle.ScreenOf(prnt, Point.Add(pt, delta));
  END ScreenOf;

PROCEDURE ForceBatches(v: VBT.T): T =
(* Force the paint batches of all ancestors of "v" up to a VBT of type "T",
   and return that VBT. *)
  <* LL.sup < v *>
  BEGIN
    WHILE NOT ISTYPE(v, T) DO
      LOCK v DO VBTRep.ForceBatch(v) END;
      v := VBT.Parent(v)
    END;
    <* ASSERT v # NIL *>
    RETURN v
  END ForceBatches;

PROCEDURE ClearSaved2(v: T) =
  <* LL.sup < v *>
  VAR offScreen, savedBuff: VBT.T; BEGIN
    (* discard existing buffer (if any) *)
    LOCK v DO
      offScreen := v.offScreen;
      savedBuff := v.savedBuff
    END;
    IF savedBuff # NIL THEN
      Trestle.Delete(savedBuff);
      VBT.Discard(savedBuff)
    END;
    VAR savedDiff: Rect.T; BEGIN
      IF offScreen # NIL
        THEN savedDiff := VBT.Domain(offScreen)
        ELSE savedDiff := Rect.Full
      END;
      LOCK v DO
        v.savedBuff := NIL;
        v.savedDiff := savedDiff
      END
    END
  END ClearSaved2;

PROCEDURE ClearSaved(v: VBT.T) =
  <* LL.sup < v *>
  BEGIN
    WHILE NOT ISTYPE(v, T) DO v := VBT.Parent(v) END;
    <* ASSERT v # NIL *>
    ClearSaved2(v)
  END ClearSaved;

PROCEDURE Save(v: VBT.T) =
  <* LL.sup < v *>
  VAR
    db: T := ForceBatches(v);
    offscreen, savedBuff: VBT.T;
    savedDiff: Rect.T;
  BEGIN
    (* create a savedBuff if necessary *)
    LOCK db DO
      offscreen := db.offScreen;
      savedBuff := db.savedBuff;
      savedDiff := db.savedDiff
    END;
    IF offscreen = NIL THEN RETURN END;
    IF savedBuff = NIL THEN
      savedBuff := InstallOffscreen(db)
    END;
    PaintVBTtoVBT(savedBuff, savedDiff, offscreen);
    LOCK db DO
      db.savedBuff := savedBuff;
      db.savedDiff := Rect.Empty
    END
  END Save;

PROCEDURE Restore(v: VBT.T) =
  <* LL.sup < v *>
  VAR
    db: T := ForceBatches(v);
    offscreen, savedBuff: VBT.T;
    savedDiff: Rect.T;
  BEGIN
    (* create a savedBuff if necessary *)
    LOCK db DO
      offscreen := db.offScreen;
      savedBuff := db.savedBuff;
      savedDiff := db.savedDiff
    END;
    IF offscreen = NIL THEN RETURN END;
    IF savedBuff = NIL
      THEN VBT.PaintTint(offscreen, savedDiff, PaintOp.Bg)
      ELSE PaintVBTtoVBT(offscreen, savedDiff, savedBuff)
    END;
    LOCK db DO
      db.screenDiff := Rect.Join(db.screenDiff, savedDiff);
      db.savedDiff := Rect.Empty
    END
  END Restore;

(* Create/capture the off-screen VBT --------------------------------------- *)

PROCEDURE InstallOffscreen(v: T): VBT.T =
(* Install and return a new offscreen VBT whose domain is
   congruent to "v"'s. *)
  VAR
    offScreen := NEW(VBT.Leaf);
    dom := VBT.Domain(v);
    tso := Trestle.ScreenOf(v, Point.Origin);
    trsl := tso.trsl;
    stInstall := VBT.ScreenTypeOf(InstalledVBT.Child(v));
    st := VBT.ScreenTypeOf(v);
  <* FATAL TrestleComm.Failure *>
  BEGIN
    IF trsl # NIL AND st # NIL THEN
      (* Install a Filter above "offScreen" so that it can have a ScreenType
         that Trestle likes in the case where "v" has an "unusual" screen
         type (i.e.  there is a scale filter) *)
      WITH filter = NEW(Filter.T).init(offScreen) DO
        Trestle.Attach(filter, trsl);
        Trestle.InstallOffscreen(
          filter, dom.east - dom.west, dom.south - dom.north, stInstall);
        IF filter.st # st THEN
          (* duke it out with trestle to set the screen type and domain *)
          VBTClass.Rescreen(offScreen, st);
          VBTClass.Reshape(offScreen, filter.domain, Rect.Empty);
        END
      END
    END;
    RETURN offScreen
  END InstallOffscreen;

PROCEDURE ReshapeOffScreen(v: T) =
(* Initialize "v"'s off-screen VBT. The VBT installed offscreen is a
   "Filter.T" containing a "VBT.Leaf". The field "v.offScreen" is set to the
   leaf. The offscreen VBT is created with the same width and height as "v".
   This procedure also has the side-effect of initializing "v.screenDiff"  and
   "v.savedDiff" to the full domain of the off-screen VBT. *)
  <* LL.sup = VBT.mu.v *>
  VAR offScreen: VBT.T; BEGIN
    (* Delete and discard the current off-screen VBT (if any) *)
    LOCK v DO
      offScreen := v.offScreen;
      v.offScreen := NIL
    END;
    IF offScreen # NIL THEN
      Trestle.Delete(offScreen);
      VBT.Discard(offScreen)
    END;
    offScreen := InstallOffscreen(v);
    LOCK v DO
      v.offScreen := offScreen;
      v.screenDiff := VBT.Domain(offScreen);
      v.savedDiff := v.screenDiff
    END
  END ReshapeOffScreen;

PROCEDURE ReshapeSavedBuff(v: T) =
  VAR new, old: VBT.T; meet: Rect.T; BEGIN
    LOCK v DO old := v.savedBuff END;
    IF old = NIL THEN RETURN END;
    new := InstallOffscreen(v);
    (* copy the common areas *)
    meet := Rect.Meet(VBT.Domain(old), VBT.Domain(new));
    PaintVBTtoVBT(new, meet, old);
    (* fill the remaining area with background *)
    VAR a: Rect.Partition; BEGIN
      Rect.Factor(VBT.Domain(new), meet, (*out*) a, 0, 0);
      a[2] := a[4];
      FOR i := 0 TO 3 DO
        VBT.PaintTint(new, a[i], PaintOp.Bg)
      END
    END;
    (* discard the old buffer *)
    Trestle.Delete(old);
    VBT.Discard(old);
    LOCK v DO v.savedBuff := new END
  END ReshapeSavedBuff;

PROCEDURE PaintVBTtoVBT(to: VBT.T; clip: Rect.T;
  from: VBT.T; delta := Point.Origin; wait := TRUE) =
<* LL.sup < to *>
  VAR dummy: Region.T; pixmap: ScrnPixmap.T; BEGIN
    pixmap := VBT.Capture(from, Rect.Sub(clip, delta), (*OUT*) dummy);
    IF pixmap # NIL THEN
      IF dummy = Region.Empty THEN
        VBT.PaintScrnPixmap(to, src := pixmap, delta := delta);
        VBT.Sync(to, wait)
      END;
      <* FATAL TrestleComm.Failure *>
      BEGIN pixmap.free() END
    END
  END PaintVBTtoVBT;

PROCEDURE LogBadRectArea(<*UNUSED*> area: INTEGER) =
(* This procedure exists soley to log the area of the rectangle copied from
   the off-screen VBT to the on-screen VBT at update time. *)
  BEGIN END LogBadRectArea;

PROCEDURE Update(v: T; wait := TRUE) =
(* Update "v" from "offscreen(v)", and set "screenDiff(v)" to "Rect.Empty". *)
  <* LL.sup < v *>
  VAR screenDiff: Rect.T; offScreen: VBT.T; delta: Point.T; BEGIN
    LOCK v DO
      screenDiff := v.screenDiff;
      offScreen := v.offScreen;
      delta := v.delta
    END;
    IF offScreen # NIL AND screenDiff # Rect.Empty THEN
      VAR transScreenDiff := Rect.Add(screenDiff, delta); BEGIN
      	PaintVBTtoVBT(v, transScreenDiff, offScreen, delta, wait);
      	IF showSyncRect THEN
          HighlightVBT.SetRect(v, transScreenDiff);
          VBT.Sync(v)
      	END
      END;
      LogBadRectArea(Rect.HorSize(screenDiff) * Rect.VerSize(screenDiff));
      LOCK v DO v.screenDiff := Rect.Empty END
    END
  END Update;

BEGIN
END DblBufferVBT.
