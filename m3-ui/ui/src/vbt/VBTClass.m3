(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* by Steve Glassman, Mark Manasse and Greg Nelson           *)
(* Last modified on Tue Jan 31 10:10:59 PST 1995 by kalsow   *)
(*      modified on Wed Jul 28 13:27:42 PDT 1993 by sfreeman *)
(*      modified on Mon May 24 19:14:48 PDT 1993 by msm      *)
(*      modified on Fri Apr  2 11:28:22 PST 1993 by steveg   *)
(*      modified on Mon Feb 24 13:58:38 PST 1992 by muller   *)
(*      modified on Mon Dec 30 18:11:15 PST 1991 by gnelson  *)

<*PRAGMA LL*>

UNSAFE MODULE VBTClass;

IMPORT VBT, VBTRep, Word, Rect, Region, PropertyV, Scheduler, Thread, BatchRep, 
Batch, BatchUtil, Cursor, Axis, ScrnPixmap, Point, Trestle, MouseSplit,
VBTTuning;

FROM VBTRep IMPORT Prop, Props;
FROM BatchUtil IMPORT ClipState;

REVEAL VBT.Split = MouseSplit.Public BRANDED OBJECT
  OVERRIDES
    beChild := BeChildDefault;
    pred := PredDefault;
    nth := NthDefault;
    index := IndexDefault;
    locate := LocateDefault;
    setcage := MouseSplit.Setcage;
    setcursor := MouseSplit.Setcursor;
    paintbatch := PaintBatchDefault;
    sync := SyncDefault;
    capture := CaptureDefault;
    screenOf := ScreenOfDefault;
    newShape := NewShapeDefault;
    acquire := AcquireDefault;
    release := ReleaseDefault;
    put := PutDefault;
    forge := ForgeDefault;
    readUp := ReadDefault;
    writeUp := WriteDefault;
    mouse := MouseSplit.Mouse;
    position := MouseSplit.Position;
    discard := DiscardDefault;
    rescreen := RescreenDefault;
    repaint := RepaintDefault;
    misc := MiscCodeDefault;
    getcursor := MouseSplit.Getcursor;
    replace := DoCrash;
    insert := DoCrash;
    move := DoCrash
  END;

PROCEDURE LocateChanged(v: VBT.Split) =
  BEGIN MouseSplit.InvalidateCache(v) END LocateChanged;
  
PROCEDURE DoCrash(<*UNUSED*>v: VBT.Split; <*UNUSED*>ch, new: VBT.T) =
  BEGIN Crash() END DoCrash;

PROCEDURE BeChildDefault(v: VBT.Split; ch: VBT.T) RAISES {} =
  BEGIN (* LL = VBT.mu & v & ch *)
    IF (ch.parent # NIL) OR (ch.st # v.st) THEN Crash() END;
    ch.parent := v;
    VBTRep.Mark(v)
  END BeChildDefault;

PROCEDURE Cage(v: VBT.T): VBT.Cage RAISES {} =
  BEGIN (* LL >= v *)
    CASE v.cageType OF
      VBTCageType.Gone => RETURN VBT.GoneCage
    | VBTCageType.Everywhere => RETURN VBT.EverywhereCage
    | VBTCageType.Rectangle =>
        IF v.miscRef # NIL THEN
          RETURN v.miscRef.cage
        ELSE
          RETURN VBTRep.EmptyCage
        END
    END
  END Cage;
        
PROCEDURE CageType(v: VBT.T): VBTCageType RAISES {} =
  BEGIN (* LL = v *)
    RETURN v.cageType
  END CageType;

PROCEDURE GetCursor(v: VBT.T): Cursor.T RAISES {} =
  BEGIN (* LL = v *)
    RETURN v.cursor
  END GetCursor;

PROCEDURE SetShortCircuit(v: VBT.T) RAISES {} =
  BEGIN (* LL = v *)
    v.props := v.props + Props{Prop.ShortCircuit}
  END SetShortCircuit;

PROCEDURE ClearShortCircuit(v: VBT.T) RAISES {} =
  BEGIN (* LL = v *)
    v.props := v.props - Props{Prop.ShortCircuit}
  END ClearShortCircuit;

PROCEDURE PutProp(v: VBT.T; ref: REFANY) RAISES {} =
  BEGIN (* LL = v *)
    PropertyV.Put(v.propset, ref)
  END PutProp;

PROCEDURE GetProp(v: VBT.T; tc: INTEGER): REFANY RAISES {} =
  BEGIN (* LL = v *)
    RETURN PropertyV.Get(v.propset, tc)
  END GetProp;

PROCEDURE RemProp(v: VBT.T; tc: INTEGER) RAISES {} =
  BEGIN (* LL = v *)
    PropertyV.Remove(v.propset, tc)
  END RemProp;

PROCEDURE Reshape(
    v: VBT.T;
    READONLY new, saved: Rect.T) RAISES {} =
  VAR cd: VBT.ReshapeRec; emptyNew, emptyPrev: BOOLEAN;
  BEGIN (* LL = v's share of VBT.mu *)
    emptyNew := new.west >= new.east;
    emptyPrev := v.domain.west >= v.domain.east;
    IF emptyNew AND emptyPrev THEN RETURN END;
    cd.prev := v.domain;
    cd.new := new;
    LOCK v DO
      cd.marked := Prop.Marked IN v.props;
      v.props := v.props - Props{Prop.Marked,Prop.BlockNewShape};
      IF emptyNew OR emptyPrev OR v.batch # NIL THEN
        VBTRep.CancelBatch(v);
        cd.saved := Rect.Empty
      ELSE
        cd.saved := Rect.Meet(saved, v.domain);
        IF NOT Rect.IsEmpty(cd.saved) THEN
          v.props := v.props + Props{Prop.Reshaping}
        END
      END;
      IF v.parent # NIL THEN
        IF Prop.Reshaping IN v.props THEN
          VBTRep.CreateMisc(v);
          v.miscRef.oldDomain := cd.saved
        ELSIF v.miscRef # NIL THEN
          v.miscRef.oldDomain := Rect.Empty;
          v.miscRef.badRgn := Region.Empty;
          VBTRep.CheckMisc(v)
        END
      END;
      v.domain := new;
      v.props := v.props + Props{Prop.Covered}
    END;
    v.reshape(cd);
    LOCK v DO
      IF v.batch # NIL THEN VBTRep.ForceBatch(v) END;
      IF Prop.Reshaping IN v.props THEN
        IF v.miscRef # NIL THEN
          v.miscRef.badRgn := Region.MeetRect(new, v.miscRef.badRgn);
          v.miscRef.oldDomain := Rect.Empty;
          VBTRep.CheckMisc(v)
        END
      END;
      v.props := v.props - Props{Prop.Covered, Prop.Reshaping};
    END
  END Reshape;
  
PROCEDURE Rescreen(v: VBT.T; st: VBT.ScreenType) RAISES {} =
  VAR cd: VBT.RescreenRec; emptyPrev: BOOLEAN;
  BEGIN (* LL = v's share of VBT.mu *)
    emptyPrev := v.domain.west >= v.domain.east;
    IF emptyPrev AND v.st = st THEN RETURN END;
    cd.prev := v.domain;
    cd.st := st;
    LOCK v DO
      VBTRep.CancelBatch(v);
      cd.marked := Prop.Marked IN v.props;
      v.props := v.props - Props{Prop.Marked,Prop.BlockNewShape};
      v.domain := Rect.Empty;
      v.st := st;
      IF v.miscRef # NIL THEN
        v.miscRef.oldDomain := Rect.Empty;
        v.miscRef.badRgn := Region.Empty;
        VBTRep.CheckMisc(v)
      END
    END;
    v.rescreen(cd);
    LOCK v DO IF v.parent # NIL THEN v.parent.setcursor(v) END END
  END Rescreen;

PROCEDURE Repaint(v: VBT.T; READONLY badR: Region.T) RAISES {} =
  VAR br: Region.T; seqno: Word.T; seqnoValid: BOOLEAN;
  BEGIN
    LOCK v DO
      IF v.parent = NIL THEN RETURN END;
      br := Region.MeetRect(v.domain, badR);
      seqnoValid := v.miscRef # NIL;
      IF seqnoValid THEN
        seqno := v.miscRef.rpseqno;
        br := Region.Join(v.miscRef.badRgn, br)
      END;
      v.props := v.props + Props{Prop.Covered}
    END;
    IF NOT Region.IsEmpty(br) THEN v.repaint(br) END;
    LOCK v DO
      IF v.batch # NIL THEN VBTRep.ForceBatch(v) END;
      v.props := v.props - Props{Prop.Covered};
      IF seqnoValid THEN
        IF v.miscRef # NIL AND seqno = v.miscRef.rpseqno THEN
          v.miscRef.badRgn := Region.Empty
        END;
        VBTRep.CheckMisc(v)
      END
    END
  END Repaint;
  
PROCEDURE Position(v: VBT.T; READONLY cd: VBT.PositionRec) RAISES {} =
  VAR escaped: BOOLEAN;
  BEGIN
    LOCK v DO
      IF v.cageType = VBTCageType.Everywhere THEN
        escaped := FALSE
      ELSIF v.cageType = VBTCageType.Gone THEN
        escaped := NOT cd.cp.gone
      ELSE
        escaped := v.miscRef = NIL OR VBT.Outside(cd.cp, v.miscRef.cage);
        IF escaped AND (v.miscRef # NIL) THEN
          v.miscRef.cage := VBTRep.EmptyCage;
          VBTRep.CheckMisc(v)
        END
      END;
      v.props := v.props - Props{Prop.EscapePending};
      IF NOT escaped THEN 
        IF v.parent # NIL THEN v.parent.setcage(v) END;
        RETURN
      END;
      v.props := v.props + Props{Prop.Covered, Prop.CageCovered};
      v.cageType := VBTCageType.Everywhere
    END;
    v.position(cd);
    LOCK v DO
      IF v.parent # NIL THEN v.parent.setcage(v) END;
      IF v.batch # NIL THEN VBTRep.ForceBatch(v) END;
      v.props := v.props - Props{Prop.Covered, Prop.CageCovered};
    END
  END Position;

PROCEDURE Key(v: VBT.T; READONLY cd: VBT.KeyRec) RAISES {} =
  BEGIN
    LOCK v DO v.props := v.props + Props{Prop.Covered} END;
    v.key(cd);
    LOCK v DO
      IF v.batch # NIL THEN VBTRep.ForceBatch(v) END;
      v.props := v.props - Props{Prop.Covered}
    END
  END Key;

PROCEDURE Mouse(v: VBT.T; READONLY cd: VBT.MouseRec) RAISES {} =
  BEGIN
    LOCK v DO v.props := v.props + Props{Prop.Covered} END;
    v.mouse(cd);
    LOCK v DO
      IF v.batch # NIL THEN VBTRep.ForceBatch(v) END;
      v.props := v.props - Props{Prop.Covered}
    END
  END Mouse;

PROCEDURE Misc(v: VBT.T; READONLY cd: VBT.MiscRec) RAISES {} =
  BEGIN
    LOCK v DO v.props := v.props + Props{Prop.Covered} END;
    v.misc(cd);
    LOCK v DO
      IF v.batch # NIL THEN VBTRep.ForceBatch(v) END;
      v.props := v.props - Props{Prop.Covered}
    END
  END Misc;

TYPE EscapeClosure = 
  Thread.SizedClosure OBJECT 
    v: VBT.T 
  OVERRIDES 
    apply := NotifyEscape
  END;

PROCEDURE ForceEscape (v: VBT.T) =
  BEGIN
    IF NOT Prop.EscapePending IN v.props
         AND v.cageType = VBTCageType.Rectangle THEN
      v.props := v.props + Props{Prop.EscapePending};
      IF NOT Prop.EscapeCovered IN v.props THEN
        v.props := v.props + Props{Prop.EscapeCovered};
        EVAL Thread.Fork(NEW(EscapeClosure, v := v, stackSize := 20000))
      END
    END
  END ForceEscape;

CONST
  Gone = VBT.PositionRec{VBT.CursorPosition{Point.Origin, 0, TRUE, FALSE},
                         time := 0, modifiers := VBT.Modifiers{}};

PROCEDURE NotifyEscape (cl: EscapeClosure): REFANY =
  VAR b: BOOLEAN;
  BEGIN
    (* EVAL ThreadFriends.SetPriority(TPFriends.PriIOLow) *)
    LOOP
      Scheduler.Yield();
      LOCK VBT.mu DO
        LOCK cl.v DO
          b := Prop.EscapePending IN cl.v.props;
          IF b THEN
            cl.v.props := cl.v.props - Props{Prop.EscapePending}
          ELSE
            cl.v.props := cl.v.props - Props{Prop.EscapeCovered};
            RETURN NIL
          END
        END;
        Position(cl.v, Gone)
      END
    END
  END NotifyEscape;

TYPE RepaintClosure = 
  Thread.SizedClosure OBJECT 
    v: VBT.T 
  OVERRIDES 
    apply := NotifyRepaint
  END;
  
PROCEDURE ForceRepaint(v: VBT.T; READONLY rgn: Region.T; 
  deliver := TRUE) RAISES {} =
  VAR br: Region.T;
  BEGIN
    IF (v.parent = NIL) OR  
      Region.IsEmpty(rgn) AND ((NOT deliver) OR (v.miscRef = NIL)) THEN 
      RETURN 
    END;
    br := Region.MeetRect(v.domain, rgn);
    IF Prop.Reshaping IN v.props THEN
      br := Region.Join(br, Region.MeetRect(v.miscRef.oldDomain, rgn))
    END;
    IF NOT Region.IsEmpty(br) THEN
      VBTRep.CreateMisc(v);
      v.miscRef.badRgn := Region.Join(v.miscRef.badRgn, br);
      INC(v.miscRef.rpseqno)
    END;
    IF deliver AND NOT (Prop.RepaintPending IN v.props) THEN
      v.props := v.props + Props{Prop.RepaintPending};
      EVAL Thread.Fork(NEW(RepaintClosure, v := v, stackSize := 20000))
    END
  END ForceRepaint;

PROCEDURE NotifyRepaint(cl: RepaintClosure): REFANY RAISES {} =
  BEGIN
    (* EVAL ThreadFriends.SetPriority(TPFriends.PriIOLow) *)
    LOCK VBT.mu DO
      LOCK cl.v DO cl.v.props := cl.v.props - Props{Prop.RepaintPending} END;
      Repaint(cl.v, Region.Empty)
    END;
    RETURN NIL
  END NotifyRepaint;

PROCEDURE Redisplay(v: VBT.T) RAISES {} =
  BEGIN
    LOCK v DO
      IF NOT Prop.Marked IN v.props THEN RETURN END;
      v.props := v.props - Props{Prop.Marked} + Props{Prop.Covered}
    END;
    v.redisplay();
    LOCK v DO
      IF v.batch # NIL THEN VBTRep.ForceBatch(v) END;
      v.props := v.props - Props{Prop.Covered}
    END
  END Redisplay;

PROCEDURE GetShape(v: VBT.T; ax: Axis.T; n: CARDINAL; 
  clearNewShape := TRUE): VBT.SizeRange RAISES {} =
  VAR sz := v.shape(ax, n);
  BEGIN
    IF clearNewShape THEN ClearNewShape(v) ELSE UnblockNewShape(v) END;
    IF (sz.lo <= sz.pref) AND (sz.pref < sz.hi) THEN RETURN sz END;
    Crash(); <*ASSERT FALSE*>
  END GetShape;

PROCEDURE GetShapes(v: VBT.T; clearNewShape := TRUE):
  ARRAY Axis.T OF VBT.SizeRange =
  VAR res: ARRAY Axis.T OF VBT.SizeRange; ax := v.axisOrder();
  BEGIN
    res[ax] := GetShape(v, ax, 0, clearNewShape);
    res[Axis.Other[ax]] := GetShape(v, Axis.Other[ax], res[ax].pref, FALSE);
    RETURN res
  END GetShapes;
  
PROCEDURE Detach(v: VBT.T) RAISES {} =
  BEGIN
    LOCK v DO
      VBTRep.DestroyMisc(v);
      v.parent := NIL;
      v.upRef := NIL;
      ClearShortCircuit(v);
      ForceEscape(v)
    END;
    Reshape(v, Rect.Empty, Rect.Empty)
  END Detach;

(* Procedures for activating the up methods of a VBT. *)

PROCEDURE SetCage(v: VBT.T; READONLY cgP: VBT.Cage) RAISES {} =
  VAR oldType := v.cageType; cg: VBT.Cage;
  BEGIN (* LL = v *)
    IF oldType = VBTCageType.Everywhere THEN
      cg := cgP
    ELSIF oldType = VBTCageType.Gone THEN
      cg.rect := cgP.rect;
      cg.screen := cgP.screen;
      cg.inOut := cgP.inOut * VBT.InOut{TRUE};
      IF cg.inOut = VBT.InOut{} THEN
        cg := VBT.EmptyCage
      END
    ELSIF v.miscRef = NIL THEN
      cg := VBTRep.EmptyCage
    ELSE
      cg.rect := Rect.Meet(cgP.rect, v.miscRef.cage.rect);
      cg.inOut := cgP.inOut * v.miscRef.cage.inOut;
      IF Rect.IsEmpty(cg.rect) OR cg.inOut = VBT.InOut{} THEN
        cg := VBT.EmptyCage
      ELSIF cgP.screen = VBT.AllScreens THEN
        cg.screen := v.miscRef.cage.screen
      ELSIF v.miscRef.cage.screen = VBT.AllScreens THEN
        cg.screen := cgP.screen
      ELSIF cgP.screen = v.miscRef.cage.screen THEN
        cg.screen := cgP.screen
      ELSE
        cg := VBTRep.EmptyCage
      END
    END;
    IF (cg.screen # VBT.AllScreens) OR NOT (TRUE IN cg.inOut) OR
      NOT Rect.Equal(cg.rect, Rect.Full) THEN
      v.cageType := VBTCageType.Rectangle
    ELSIF (FALSE IN cg.inOut) THEN
      v.cageType := VBTCageType.Everywhere
    ELSE
      v.cageType := VBTCageType.Gone
    END;
    IF v.parent = NIL THEN
      IF NOT (TRUE IN cg.inOut) THEN
        ForceEscape(v)
      END;
      RETURN
    END;
    IF v.cageType = oldType AND
       ((v.cageType # VBTCageType.Rectangle) OR
         (v.miscRef = NIL) AND Rect.IsEmpty(cg.rect) OR
         (v.miscRef # NIL) AND EqualCage(cg, v.miscRef.cage)) THEN
       RETURN
    END;
    IF (v.cageType = VBTCageType.Rectangle) AND NOT Rect.IsEmpty(cg.rect) THEN 
      VBTRep.CreateMisc(v);
      v.miscRef.cage := cg
    ELSIF v.miscRef # NIL THEN
      v.miscRef.cage := VBTRep.EmptyCage;
      IF oldType = VBTCageType.Rectangle THEN VBTRep.CheckMisc(v) END
    END;
    IF NOT (Prop.CageCovered IN v.props) THEN
      v.parent.setcage(v)
    END
  END SetCage;

PROCEDURE EqualCage(READONLY a, b: VBT.Cage): BOOLEAN =
  BEGIN
    IF Rect.IsEmpty(a.rect) AND Rect.IsEmpty(b.rect) THEN RETURN TRUE END;
    RETURN a.inOut = b.inOut AND a.screen = b.screen AND 
      Rect.Equal(a.rect, b.rect)
  END EqualCage;

PROCEDURE SetCursor(v: VBT.T; cs: Cursor.T) RAISES {} =
  BEGIN
    IF v.cursor.cs # cs.cs THEN
      v.cursor := cs;
      IF v.parent # NIL THEN v.parent.setcursor(v) END
    END
  END SetCursor;

VAR combineLimit := VBTTuning.CombineLimit;

PROCEDURE PaintBatch(v: VBT.T; VAR ba: Batch.T) RAISES {} =
  VAR w: VBT.T; clipP: Rect.T; clipPed: BOOLEAN; lenb: INTEGER;
  CONST
    CombineProps =
      VBTRep.AllProps - 
        Props{Prop.Covered, Prop.OnQ, Prop.ExcessBegins, Prop.Combiner};
    CoveredProps = 
      VBTRep.AllProps - Props{Prop.Covered, Prop.OnQ, Prop.ExcessBegins};

  PROCEDURE OKToMerge(): BOOLEAN =
    BEGIN
      IF v.batch # NIL THEN
        WITH b = v.batch^ DO
          IF b.firstSingle # b.next THEN RETURN FALSE END;
          BatchUtil.Tighten(v.batch);
          IF NOT Rect.Subset(b.clip, v.domain) THEN RETURN FALSE END
        END
      END;
      IF clipPed THEN ba.clip := clipP; clipPed := FALSE END;
      BatchUtil.Tighten(ba);
      IF Rect.Subset(ba.clip, v.domain) THEN
        RETURN TRUE
      ELSE
        clipP := ba.clip;
        clipPed := TRUE;
        ba.clip := Rect.Meet(ba.clip, v.domain);
        ba.clipped := ClipState.Unclipped;
        RETURN FALSE
      END
    END OKToMerge;

  BEGIN
    IF ba = NIL THEN RETURN END;
    lenb := ba.next - ADR(ba.b[0]);
    IF lenb = 0 THEN Batch.Free(ba); RETURN END;
    Thread.Acquire(v);
    TRY (* AIRLOCK v DO *)
      WITH vdom = v.domain, clip = ba.clip DO
        clipPed :=
          (clip.west < vdom.west) OR (clip.east > vdom.east)
          OR (clip.north < vdom.north) OR (clip.south > vdom.south);
        IF clipPed THEN
          ba.clipped := ClipState.Unclipped;
          clipP := clip;
          clip := Rect.Meet(clipP, vdom)
        END 
      END;
      (* clipPed == clipP is defined to be the original value of clip.
         In any case, clip is now a subset of the domain of v *)
      IF ba.clip.west >= ba.clip.east THEN
        IF Prop.Reshaping IN v.props THEN
          IF NOT clipPed THEN clipP := ba.clip END;
          VBTRep.ExpandBadRect(v, clipP, ba)
        END;
        Batch.Free(ba);
        RETURN
      END;
      LOOP (* LL = v *)
        IF ((v.remaining >= lenb)
            OR (lenb < combineLimit) AND 
                NOT (v.props <= CombineProps))
           AND (NOT (Prop.Reshaping IN v.props) OR OKToMerge()) THEN
          (* Deposit batch onto v. *)
          IF (v.batch # NIL) AND (v.remaining < lenb) THEN
            VBTRep.ForceBatch(v)
          END;
          IF v.batch = NIL THEN
            v.batch := ba;
            ba.firstSingle := ba.next;
            ba.excessBegins := 0;
            v.remaining := NUMBER(ba.b^) * BYTESIZE(Word.T) - lenb;
            ba := NIL
          ELSE
            (* Now v's batch is present and has room for the batch *)
            VBTRep.ExtendBatch(v, ba)
          END;
          IF v.props <= CoveredProps THEN VBTRep.Enqueue(v) END;
          EXIT
        END;
        IF v.batch # NIL THEN VBTRep.ForceBatch(v) END;
        IF (Prop.Reshaping IN v.props) OR NOT Rect.IsEmpty(ba.scrollSource) THEN
          IF NOT clipPed THEN clipP := ba.clip; clipPed := TRUE END;
          VBTRep.ExpandBadRect(v, clipP, ba);
          clipPed := FALSE;
          IF (ba.clipped = ClipState.Unclipped)
             OR NOT Rect.Subset(ba.clip, v.domain) THEN
            ba.clip := Rect.Meet(ba.clip, v.domain);
            ba.clipped := ClipState.Unclipped
          END
        END;
        IF v.parent = NIL THEN 
          Batch.Free(ba); 
          EXIT
        ELSIF NOT Prop.ShortCircuit IN v.props THEN
          v.parent.paintbatch(v, ba);
          ba := NIL;
          EXIT
        END;
        w := v.parent;
        Thread.Acquire(w);
        Thread.Release(v);
        v := w
      END
    FINALLY
      Thread.Release(v)
    END
  END PaintBatch;

PROCEDURE GetBadRegion(v: VBT.T): Region.T =
  (* LL = v *)
  BEGIN
    IF v.parent = NIL THEN 
      RETURN Region.Empty
    ELSE
      IF v.batch # NIL THEN VBTRep.ForceBatch(v) END;
      IF v.miscRef = NIL THEN 
        RETURN Region.Empty
      ELSE 
        RETURN v.miscRef.badRgn
      END
    END
  END GetBadRegion;

PROCEDURE Acquire(v: VBT.T; s: VBT.Selection; t: VBT.TimeStamp)
  RAISES {VBT.Error} =
  BEGIN 
    WITH p = v.parent DO
      IF p = NIL THEN RAISE VBT.Error(VBT.ErrorCode.Uninstalled) END;
      p.acquire(v, v, s, t)
    END
  END Acquire;

PROCEDURE Release(v: VBT.T; s: VBT.Selection) RAISES {} =
  BEGIN 
    WITH p = v.parent DO
      IF p # NIL THEN p.release(v, v, s) END
    END
  END Release;

PROCEDURE Put(
  v:      VBT.T;
  s:      VBT.Selection;
  t:      VBT.TimeStamp;
  type:   VBT.MiscCodeType;
  READONLY detail: VBT.MiscCodeDetail
  ) RAISES {VBT.Error} =
  BEGIN
    WITH p = v.parent DO
      IF p # NIL THEN
        p.put(v, v, s, t, type, detail)
      ELSE
        RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
      END
    END
  END Put;

PROCEDURE Forge(v: VBT.T; type: VBT.MiscCodeType; READONLY detail: VBT.MiscCodeDetail)
  RAISES {VBT.Error} =
  BEGIN
    WITH p = v.parent DO
        IF p # NIL THEN p.forge(v, v, type, detail)
      ELSE
        RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
      END
    END
  END Forge;

PROCEDURE HasNewShape(v: VBT.T): BOOLEAN =
  BEGIN
    LOCK v DO RETURN Prop.HasNewShape IN v.props END
  END HasNewShape;

PROCEDURE ClearNewShape(v: VBT.T) =
  BEGIN
    LOCK v DO 
      v.props := v.props - 
        Props{Prop.HasNewShape,Prop.BlockNewShape} 
    END
  END ClearNewShape;

PROCEDURE UnblockNewShape(v: VBT.T) =
  BEGIN
    LOCK v DO 
      v.props := v.props - Props{Prop.BlockNewShape} 
    END
  END UnblockNewShape;

(* The default split methods. *)

PROCEDURE PredDefault(v: VBT.Split; ch: VBT.T): VBT.T RAISES {} =
  VAR res: VBT.T := NIL; next := v.succ(res);
  BEGIN
    WHILE next # NIL AND next # ch DO
      res := next;
      next := v.succ(res)
    END;
    IF next # ch THEN Crash() END;
    RETURN res
  END PredDefault;

PROCEDURE NthDefault(v: VBT.Split; n: CARDINAL): VBT.T RAISES {} =
  VAR ch := v.succ(NIL);
  BEGIN
    WHILE ch # NIL AND n # 0 DO DEC(n); ch := v.succ(ch) END;
    RETURN ch
  END NthDefault;

PROCEDURE IndexDefault(v: VBT.Split; ch: VBT.T): CARDINAL RAISES {} =
  VAR res := 0; chP := v.succ(NIL);
  BEGIN
    WHILE chP # ch AND chP # NIL DO
      INC(res);
      chP := v.succ(chP)
    END;
    IF chP # ch THEN Crash() END;
    RETURN res
  END IndexDefault;

PROCEDURE LocateDefault(
  v: VBT.Split; 
  READONLY pt: Point.T; 
  VAR (*OUT*) rect: Rect.T): VBT.T RAISES {} =
  VAR ch := v.succ(NIL);
  BEGIN
    rect := Rect.Full;
    WHILE ch # NIL DO
      (* (pt IN rect), rect doesn't intersect the domain of any 
         predecessor of ch *)
      WITH r = ch.domain DO
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
  END LocateDefault;

(* Default VBT down methods for splits. *)

PROCEDURE DiscardDefault(v: VBT.Split) RAISES {} =
  VAR ch := v.succ(NIL); 
  BEGIN 
    WHILE ch # NIL DO
      v.replace(ch, NIL);
      Detach(ch);
      VBT.Discard(ch);
      ch := v.succ(NIL)
    END;
    v.mouseRef := NIL
  END DiscardDefault;
  
PROCEDURE RescreenDefault(v: VBT.Split; <*UNUSED*>READONLY cd: VBT.RescreenRec) 
  RAISES {} =
  VAR ch := v.succ(NIL); 
  BEGIN
    WHILE ch # NIL DO
      Rescreen(ch, v.st);
      ch := v.succ(ch)
    END
  END RescreenDefault;

PROCEDURE RepaintDefault(v: VBT.Split; READONLY rgn: Region.T) 
  RAISES {} =
  VAR ch := v.succ(NIL); 
  BEGIN
    WHILE ch # NIL DO
      Repaint(ch, rgn);
      ch := v.succ(ch)
    END
  END RepaintDefault;

PROCEDURE MiscCodeDefault(v: VBT.Split; READONLY cd: VBT.MiscRec) RAISES {} =
  VAR ch := v.succ(NIL); 
  BEGIN
    WHILE ch # NIL DO
      Misc(ch, cd);
      ch := v.succ(ch)
    END
  END MiscCodeDefault;

(* The default up methods. *)

(* The following method defaults recurse on their parent in
   and obvious way. *)
    
PROCEDURE NewShapeDefault(v: VBT.Split; <*UNUSED*>ch: VBT.T) RAISES {} =
  BEGIN 
    VBT.NewShape(v) 
  END NewShapeDefault;

PROCEDURE PaintBatchDefault(v: VBT.Split; ch: VBT.T; b: Batch.T) RAISES {} =
  BEGIN
    PaintBatch(v, b);
    SetShortCircuit(ch);
  END PaintBatchDefault;

PROCEDURE SyncDefault(v: VBT.Split; ch: VBT.T; wait := TRUE) RAISES {} =
  BEGIN
    LOCK v DO
      Thread.Release(ch);
      IF v.batch # NIL THEN VBTRep.ForceBatch(v) END;
      WITH p = v.parent DO
        IF p # NIL THEN p.sync(v, wait) END
      END
    END;
    Thread.Acquire(ch)
  END SyncDefault;

PROCEDURE CaptureDefault(
  v: VBT.Split; 
  <*UNUSED*>ch: VBT.T; 
  READONLY rect: Rect.T; 
  VAR (*out*) br: Region.T): 
  ScrnPixmap.T RAISES {} =
  BEGIN 
    RETURN VBT.Capture(v, rect, br) 
  END CaptureDefault;

PROCEDURE ScreenOfDefault(
    v: VBT.Split; 
    <*UNUSED*>ch: VBT.T;
    READONLY pt: Point.T): 
    Trestle.ScreenOfRec
    RAISES {} =
  BEGIN 
    RETURN Trestle.ScreenOf(v, pt) 
  END ScreenOfDefault;

PROCEDURE AcquireDefault(
  v: VBT.Split; 
  <*UNUSED*>ch: VBT.T; w: VBT.T; 
  s: VBT.Selection; 
  ts: VBT.TimeStamp) 
  RAISES {VBT.Error} =
  BEGIN (* LL < v *)
    LOCK v DO
      IF v.parent = NIL THEN 
        RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
      ELSE
        v.parent.acquire(v, w, s, ts)
      END
    END
  END AcquireDefault;

PROCEDURE ReleaseDefault(
  v: VBT.Split; 
  <*UNUSED*>ch: VBT.T; w: VBT.T; 
  s: VBT.Selection)
  RAISES {}  =
  BEGIN (* LL < v *)
    LOCK v DO
      IF v.parent # NIL THEN 
        v.parent.release(v, w, s)
      END
    END
  END ReleaseDefault;

PROCEDURE PutDefault(
  v: VBT.Split; 
  <*UNUSED*>ch: VBT.T; w: VBT.T; 
  s: VBT.Selection; 
  ts: VBT.TimeStamp; 
  type: VBT.MiscCodeType;
  READONLY detail: ARRAY [0..1] OF INTEGER)
  RAISES {VBT.Error}  =
  BEGIN (* LL < v *)
    LOCK v DO
      IF v.parent = NIL THEN 
        RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
      ELSE
        v.parent.put(v, w, s, ts, type, detail)
      END
    END
  END PutDefault;

PROCEDURE ForgeDefault(
  v: VBT.Split; 
  <*UNUSED*>ch: VBT.T; w: VBT.T; 
  type: VBT.MiscCodeType;
  READONLY detail: ARRAY [0..1] OF INTEGER)
  RAISES {VBT.Error} =
  BEGIN (* LL < v *)
    LOCK v DO
      IF v.parent = NIL THEN 
        RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
      ELSE
        v.parent.forge(v, w, type, detail)
      END
    END
  END ForgeDefault;

PROCEDURE ReadDefault(
  v: VBT.Split; 
  <*UNUSED*>ch: VBT.T; w: VBT.T; 
  s: VBT.Selection; 
  ts: VBT.TimeStamp; 
  tc: CARDINAL)
  : VBT.Value RAISES {VBT.Error}  =
  VAR p: VBT.Split;
  BEGIN 
    LOCK v DO p := v.parent END;
    IF p = NIL THEN
      RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
    ELSE
      RETURN p.readUp(v, w, s, ts, tc)
    END 
  END ReadDefault;

PROCEDURE WriteDefault(
  v: VBT.Split; 
  <*UNUSED*>ch: VBT.T; w: VBT.T; 
  s: VBT.Selection; 
  ts: VBT.TimeStamp; 
  val: VBT.Value;
  tc: CARDINAL) 
  RAISES {VBT.Error} =
  VAR p: VBT.Split;
  BEGIN 
    LOCK v DO p := v.parent END;
    IF p = NIL THEN
      RAISE VBT.Error(VBT.ErrorCode.Uninstalled)
    ELSE
      p.writeUp(v, w, s, ts, val, tc)
    END 
  END WriteDefault;

EXCEPTION FatalError;

PROCEDURE Crash() =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;

BEGIN
END VBTClass.
