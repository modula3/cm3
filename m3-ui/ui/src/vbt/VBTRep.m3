(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* VBTRep.def, code Sun Aug 11 12:18:43 1985 by Greg Nelson *)
(* Last modified on Tue Jan 31 10:11:21 PST 1995 by kalsow   *)
(*      modified on Fri Jul  8 17:10:26 PDT 1994 by msm      *)
(*      modified on Mon Oct  4 11:38:30 PDT 1993 by sfreeman *)
(*      modified on Thu Apr 29 11:14:04 PDT 1993 by mjordan *)
(*      modified on Fri Apr  2 17:31:31 PST 1993 by steveg  *)
(*      modified on Mon Feb 24 13:59:01 PST 1992 by muller  *)
(*      modified on Tue Oct 22 22:40:17 PDT 1991 by gnelson *)


<*PRAGMA LL*>

UNSAFE MODULE VBTRep;

IMPORT Batch, BatchRep, BatchUtil, PaintPrivate, PlttFrnds, Point, Rect,
       Region, ScrnCursor, Thread, VBT, VBTClass, Word, Cstring, Axis,
       VBTTuning, Palette, ScrnPixmap, ScrnFont, ScrnPaintOp, PaintOp,
       Font, Pixmap, Cursor, Completion, PictureRep, PaintExt;

REVEAL
  VBT.T = VBT.Public BRANDED OBJECT
          OVERRIDES
            reshape   := ReshapeCrash;
            repaint   := RepaintCrash;
            rescreen  := RescreenCrash;
            mouse     := MouseCrash;
            key       := KeyCodeCrash;
            position  := PositionCrash;
            misc      := MiscCodeCrash;
            shape     := ShapeCrash;
            read      := ReadCrash;
            write     := WriteCrash;
            redisplay := RedisplayCrash;
            discard   := DiscardCrash;
          END;

REVEAL VBT.ScreenType = STPub BRANDED OBJECT
  OVERRIDES
    opApply := OpApply;
    pixmapApply := PixmapApply;
    cursorApply := CursorApply;
    fontApply := FontApply
  END;

PROCEDURE CopyBytes(src, dst: ADDRESS; n: INTEGER) =
  BEGIN
    EVAL Cstring.memcpy(dst, src, n)
  END CopyBytes;

PROCEDURE OpApply (st: VBT.ScreenType; cl: Palette.OpClosure; op: PaintOp.T):
  ScrnPaintOp.T =
  BEGIN
    IF cl = NIL THEN
      RETURN st.op.builtIn(op.op)
    ELSE
      RETURN cl.apply(st)
    END
  END OpApply;

PROCEDURE PixmapApply (st: VBT.ScreenType;
                       cl: Palette.PixmapClosure;
                       pm: Pixmap.T               ): ScrnPixmap.T =
  BEGIN
    IF cl = NIL THEN
      RETURN st.pixmap.builtIn(pm.pm)
    ELSE
      RETURN cl.apply(st)
    END
  END PixmapApply;

PROCEDURE CursorApply (st: VBT.ScreenType;
                       cl: Palette.CursorClosure;
                       cs: Cursor.T               ): ScrnCursor.T =
  BEGIN
    IF cl = NIL THEN
      RETURN st.cursor.builtIn(cs.cs)
    ELSE
      RETURN cl.apply(st)
    END
  END CursorApply;

PROCEDURE FontApply (st: VBT.ScreenType;
                     cl: Palette.FontClosure;
                     ft: Font.T               ): ScrnFont.T =
  BEGIN
    IF cl = NIL THEN
      RETURN st.font.builtIn(ft.fnt)
    ELSE
      RETURN cl.apply(st)
    END
  END FontApply;

VAR
  mu := NEW(MUTEX);
  avail: MiscRef := NIL;
    (* mu protects all access to avail *)

PROCEDURE CheckMisc(v: VBT.T) =
  VAR miscRef := v.miscRef;
  BEGIN 
    IF (miscRef # NIL) AND (miscRef.badRgn.r.west >= miscRef.badRgn.r.east)
         (* i.e. Rect.IsEmpty(miscRef.badRgn.r) *)
       AND (miscRef.oldDomain.west >= miscRef.oldDomain.east)
         (* i.e. Rect.IsEmpty(miscRef.oldDomain) *)
       AND (miscRef.cage.rect.west >= miscRef.cage.rect.east)
    THEN
      LOCK mu DO
        miscRef.link := avail;
        avail := miscRef
      END;
      v.miscRef := NIL
    END
  END CheckMisc;

PROCEDURE CreateMisc(v: VBT.T) =
  BEGIN 
    IF v.miscRef = NIL THEN
      LOCK mu DO
        IF avail # NIL THEN
          v.miscRef  := avail;
          avail := avail.link
        ELSE
          v.miscRef := NEW(MiscRef)
        END
      END;
      v.miscRef.rpseqno := 0
    END
  END CreateMisc;

PROCEDURE DestroyMisc(v: VBT.T) =
  BEGIN
    v.miscRef := NIL;
    v.props := v.props - Props{Prop.Reshaping};
  END DestroyMisc;

PROCEDURE NewBatch(v: VBT.T; len: INTEGER := -1) =
  BEGIN
    IF v.batch # NIL THEN ForceBatch(v) END;
    v.batch := Batch.New(len);
    v.batch.excessBegins := 0;
    v.batch.firstSingle := v.batch.next;
    v.remaining := NUMBER(v.batch.b^) * ADRSIZE(Word.T)
  END NewBatch;

PROCEDURE MergeBatch(
         batch:    Batch.T;
         middle:   ADDRESS;
READONLY clipP:    Rect.T;
         clippedP: BatchUtil.ClipState;
   VALUE scrollSourceP: Rect.T) =
  VAR
    first:  ADDRESS;
    len:    INTEGER;
  BEGIN
    IF middle = batch.next THEN RETURN END;
    first := ADR(batch.b[0]);
    IF middle = first THEN
      batch.clip := clipP;
      batch.clipped := clippedP;
      batch.scrollSource := scrollSourceP;
      RETURN
    END;
    len := (middle - first) DIV ADRSIZE(Word.T);
    IF batch.clipped = BatchUtil.ClipState.Unclipped THEN
      batch.clip := BatchRep.ClipSubAndTighten(batch.clip, 
        batch.b^, 0, len, batch.scrollSource);
      batch.clipped := BatchUtil.ClipState.Tight
    END;
    IF clippedP = BatchUtil.ClipState.Unclipped THEN
      batch.clip := Rect.Join(batch.clip, 
         BatchRep.ClipSubAndTighten(clipP, 
           batch.b^, len, (batch.next - middle) DIV ADRSIZE(Word.T),
           scrollSourceP))
        (* clippedP is now tight, so don't downgrade clipped *)
    ELSE
      batch.clip := Rect.Join(batch.clip, clipP);
        (* downgrade clipped if needed *)
      IF clippedP = BatchUtil.ClipState.Clipped THEN 
        batch.clipped := BatchUtil.ClipState.Clipped 
      END
    END;
    batch.scrollSource := Rect.Join(batch.scrollSource, scrollSourceP)
  END MergeBatch;

PROCEDURE ExpandBR(
                v:    VBT.T;
  READONLY      rect: Rect.T;
  VAR (*INOUT*) br:   Region.T
  ): BOOLEAN =
  VAR
    a: Rect.Partition;
    b := FALSE;
      (* expand br by (rect \ v^.domain) intersect v^.oldDomain. Return TRUE
         if the empty rectangle would have been expanded. LL = MuP(v). *)
  BEGIN
    IF NOT Rect.Subset(rect, v.domain) AND (v.miscRef # NIL) AND
       Rect.Overlap(rect, v.miscRef.oldDomain)
    THEN
      Rect.Factor(rect, v.domain, a, 0, 0);
      FOR i := 0 TO 4 DO
        IF (i # 2) AND Rect.Overlap(a[i], v.miscRef.oldDomain) THEN
          b  := TRUE;
          br := Region.JoinRect(Rect.Meet(a[i], v.miscRef.oldDomain), br)
        END
      END
    END;
    RETURN b
  END ExpandBR;

PROCEDURE ScrollBR(
                v:     VBT.T;
  VAR (*INOUT*) clip:  Rect.T;
  READONLY      delta: Point.T;
  VAR (*INOUT*) br:    Region.T
  ): BOOLEAN =
  (* Return TRUE if scrolling by delta into clip uses any bits (a) outside
     v's screen or (b) in v's badRgn or in br whose translation by delta
     isn't in v's badRgn. br gets a rectangle that covers the translation of
     all such bits and contains the original br. clip may be reduced by
     dropping regions whose corresponding source are such bits. LL = muP(v).
     *)
  VAR
    a, aP: Rect.Partition;
    b:     BOOLEAN;
    dom, oldDom: Rect.T;
    brP, brPP: Region.T;
  BEGIN
    IF v.miscRef # NIL THEN
      brP := Region.Join(br, v.miscRef.badRgn)
    ELSE
      brP := br
    END;
    IF NOT Region.IsEmpty(brP) THEN
      brPP := Region.MeetRect(clip, Region.Add(brP, delta));
      b    := NOT Region.Subset(brPP, brP);
      IF b THEN br := Region.Join(br, brPP) END
    ELSE
      b := FALSE
    END;
    (* Now the bad rectangle is smeared, but we haven't yet checked for
       the scrolling of bits outside the domain. *)
    dom := Rect.Move(v.domain, delta);
    IF Prop.Reshaping IN v.props THEN
      IF v.miscRef # NIL THEN
        oldDom := Rect.Move(v.miscRef.oldDomain, delta)
      ELSE
        oldDom := Rect.Empty
      END
    END;
    IF NOT Rect.Subset(clip, dom) AND
       NOT ((Prop.Reshaping IN v.props) AND Rect.Subset(clip, oldDom))
    THEN
      (* probably using bits not in the domain as source; must expand br *)
      Rect.Factor(clip, dom, a, 0, 0);
      FOR i := 0 TO 4 DO
        IF (i # 2) AND NOT Rect.IsEmpty(a[i]) THEN
          IF Prop.Reshaping IN v.props THEN
            Rect.Factor(a[i], oldDom, aP, 0, 0);
            FOR j := 0 TO 4 DO
              IF (j # 2) AND NOT Rect.IsEmpty(aP[j]) THEN
                b  := TRUE;
                br := Region.JoinRect(aP[j], br)
              END
            END
          ELSE
            b  := TRUE;
            br := Region.JoinRect(a[i], br)
          END
        END
      END;
      IF Prop.Reshaping IN v.props THEN
        clip := Rect.Meet(clip, Rect.Join(dom, oldDom))
      ELSE
        clip := Rect.Meet(clip, dom)
      END
    END;
    RETURN b
  END ScrollBR;

PROCEDURE ExpandBadRect(w: VBT.T; READONLY clp: Rect.T; ba: Batch.T) =
  (* Expand w's bad rectangle for the given paint batch. clp is the original
     clipping rectangle for ba, before intersection with w.domain. The
     expansion is caused by (a) using out-of-domain bits as source (b)
     painting into the old domain (c) scrolling an existing bad rectangle. LL
     = w. *)
  VAR
    b, brPres: BOOLEAN;
    cptr: PaintPrivate.CommandPtr;
    start, end, len: INTEGER;
    br: Region.T;
  BEGIN
    br := Region.Empty;
    b  := FALSE;
    IF Prop.Reshaping IN w.props THEN b := ExpandBR(w, clp, br) END;
    brPres := (w.miscRef # NIL) AND 
      (w.miscRef.badRgn.r.west < w.miscRef.badRgn.r.east);
    IF NOT Rect.IsEmpty(ba.scrollSource) THEN
      start  := 0;
      end := (ba.next - ADR(ba.b[0])) DIV ADRSIZE(Word.T);
      WHILE start # end DO
        cptr := LOOPHOLE(ADR(ba.b[start]), PaintPrivate.CommandPtr);
        len := PaintPrivate.CommandLength(cptr);
        INC(start, len);
        IF cptr.command = PaintPrivate.PaintCommand.ScrollCom THEN
          WITH scptr = LOOPHOLE(cptr, PaintPrivate.ScrollPtr) DO
            IF ba.clipped = BatchUtil.ClipState.Unclipped THEN
              scptr.clip := Rect.Meet(ba.clip, scptr.clip)
            END;
            IF brPres OR b OR
               NOT Rect.Subset(scptr.clip, Rect.Move(w.domain, scptr.delta))
            THEN
              IF ScrollBR(w, scptr.clip, scptr.delta, br) THEN b := TRUE END
            END
          END
        END
      END
    END;
    IF b THEN VBTClass.ForceRepaint(w, br) END
  END ExpandBadRect;

PROCEDURE ForceBatch(v: VBT.T) =
  VAR clipP: Rect.T; clipPed: BOOLEAN;
  BEGIN 
    IF v.parent = NIL THEN CancelBatch(v) END;
    IF v.batch = NIL THEN RETURN END;
    v.remaining := 0;
    v.props := v.props - Props{Prop.ExcessBegins};
    IF v.batch.next = ADR(v.batch.b[0]) THEN Batch.Free(v.batch); RETURN END;
    IF v.batch.firstSingle = ADR(v.batch.b[0]) THEN
      v.batch.clipped := BatchUtil.ClipState.Unclipped;
      IF Prop.Reshaping IN v.props THEN
        v.batch.clip := Rect.Join(v.domain, v.miscRef.oldDomain);
        BatchUtil.Tighten(v.batch);
        WITH dom = v.domain, clip = v.batch.clip DO
          clipPed :=
            (clip.west < dom.west) OR (clip.east > dom.east)
            OR (clip.north < dom.north) OR (clip.south > dom.south);
          IF clipPed THEN
            v.batch.clipped := BatchUtil.ClipState.Unclipped;
            clipP := clip;
            clip := Rect.Meet(clipP, dom)
          END
        END
      ELSE
        v.batch.clip := v.domain;
        clipPed := FALSE
      END
    ELSE
      IF v.batch.firstSingle # v.batch.next THEN
        IF Prop.Reshaping IN v.props THEN
          MergeBatch(v.batch, v.batch.firstSingle,
            Rect.Join(v.domain, v.miscRef.oldDomain), 
            BatchUtil.ClipState.Unclipped, v.batch.scrollSource)
        ELSE
          MergeBatch(v.batch, v.batch.firstSingle, v.domain,
            BatchUtil.ClipState.Unclipped, v.batch.scrollSource)
        END
      END;
      WITH dom = v.domain, clip = v.batch.clip DO
        clipPed :=
          (clip.west < dom.west) OR (clip.east > dom.east)
          OR (clip.north < dom.north) OR (clip.south > dom.south);
        IF clipPed THEN
          v.batch.clipped := BatchUtil.ClipState.Unclipped;
          clipP := clip;
          clip := Rect.Meet(clipP, v.domain)
        END
      END
    END;
    IF (Prop.Reshaping IN v.props) OR 
       NOT Rect.IsEmpty(v.batch.scrollSource) THEN
      IF NOT clipPed THEN clipP := v.batch.clip END;
      ExpandBadRect(v, clipP, v.batch)
    END;
    IF v.batch.clip.west >= v.batch.clip.east THEN 
      Batch.Free(v.batch); 
      RETURN 
    END;
    IF Prop.ShortCircuit IN v.props THEN
      VBTClass.PaintBatch(v.parent, v.batch);
    ELSE
      TRY
        v.parent.paintbatch(v, v.batch);
      FINALLY
        v.batch := NIL
      END
    END
  END ForceBatch;

PROCEDURE CancelBatch(v: VBT.T) =
  BEGIN
    IF v.batch # NIL THEN
      Batch.Free(v.batch);
      v.remaining := 0;
      v.props := v.props - Props{Prop.ExcessBegins}
    END
  END CancelBatch;

TYPE MMEntry = REF RECORD v: VBT.T; mmLink: MMEntry END;

VAR
  qmu := NEW(MUTEX);
  qc := NEW(Thread.Condition);
  todo, qhead, qtail: MMEntry := NIL;
  qth: INTEGER := 0; (* Number of MeterMaid threads not waiting on qc *)
  numWorkers: INTEGER := 0; (* current size of crew *)

VAR (* const *)
  maxWorkers: INTEGER := 1; (* := number of processors on the system *)

PROCEDURE Enqueue(v: VBT.T) =
  VAR mustSignal: BOOLEAN;
  BEGIN
    v.props := v.props + Props{Prop.OnQ};
    LOCK qmu DO
      mustSignal := (qhead = NIL) AND (qth = 0);
      WITH ve = NEW(MMEntry, v := v, mmLink := NIL) DO
        IF qhead = NIL THEN qhead := ve ELSE qtail.mmLink := ve END;
        qtail := ve
      END
    END;
    IF mustSignal THEN Thread.Signal(qc) END
  END Enqueue;

PROCEDURE MeterMaid(<*UNUSED*>self: Thread.Closure): REFANY RAISES {} =
  VAR w, v: MMEntry := NIL; mustSignal: BOOLEAN;
  BEGIN
    (* IF 0 = ThreadFriends.SetPriority(TPFriends.PriIOLow) THEN END; *)
    LOCK qmu DO INC(qth) END;
    LOOP
      LOCK qmu DO
        IF v # NIL THEN todo := v; v := NIL END;
        DEC(qth);
        WHILE ((qhead = NIL) OR (qth # 0)) AND (todo = NIL) DO
          Thread.Wait(qmu, qc)
        END;
        INC(qth);
        IF todo = NIL THEN
          v := qhead;
          qhead := NIL;
          qtail := NIL
        ELSE
          w := todo;
          todo := todo.mmLink;
          w.mmLink := NIL;
          mustSignal := todo # NIL;
          IF mustSignal AND (qth = numWorkers)
             AND (numWorkers < maxWorkers) THEN
            EVAL Thread.Fork(NEW(Thread.SizedClosure, apply := MeterMaid, 
              stackSize := 20000));
            INC(numWorkers)
          END
        END
      END;
      IF v # NIL THEN
        Thread.Pause(batchLatency)
      ELSE
        IF mustSignal THEN Thread.Signal(qc) END;
        LOCK w.v DO
          IF w.v.batch # NIL THEN ForceBatch(w.v) END;
          w.v.props := w.v.props - Props{Prop.OnQ}
        END;
        w := NIL
      END
    END
  END MeterMaid;

VAR batchLatency := FLOAT(VBTTuning.BatchLatency, LONGREAL) / 1000000.0D0;

PROCEDURE AxisOrderDefault(<*UNUSED*> v: VBT.Prefix): Axis.T =
  BEGIN RETURN Axis.T.Hor END AxisOrderDefault;

TYPE
  CursorClosure = Thread.Closure OBJECT
                    st: VBT.ScreenType;
                    v : VBT.Prefix;
                    cs: Cursor.T
                  OVERRIDES
                    apply  := CursorResolver
                  END;

PROCEDURE CursorResolver (self: CursorClosure): REFANY =
  BEGIN
    EVAL Palette.ResolveCursor(self.st, self.cs);
    LOCK self.v DO
      IF self.v.parent # NIL THEN self.v.parent.setcursor(self.v) END
    END;
    RETURN NIL
  END CursorResolver;

PROCEDURE GetcursorDefault (v: VBT.Prefix): ScrnCursor.T =
  BEGIN
    IF v.st # NIL THEN
      VAR res: ScrnCursor.T := NIL;
      BEGIN
        IF v.cursor.cs < NUMBER(v.st.cursors^) THEN
          res := v.st.cursors[v.cursor.cs]
        END;
        IF res # NIL AND res # PlttFrnds.noCursor THEN
          RETURN res
        ELSE
          EVAL Thread.Fork(
                 NEW(CursorClosure, st := v.st, v := v, cs := v.cursor));
          RETURN ScrnCursor.DontCare
        END
      END
    ELSE
      RETURN ScrnCursor.DontCare
    END
  END GetcursorDefault;

PROCEDURE ExtendBatch (v: VBT.T; VAR ba: Batch.T) =
  (* Extend v's batchP to include the painting operations in ba, and free
     ba.  It is assumed that v has a non-empty batchP which has room for
     the extension. *)
  VAR
    newlen: INTEGER;
    middle: ADDRESS;
  BEGIN
    WITH vb = v.batch DO
      IF vb.firstSingle # vb.next THEN
        IF vb.firstSingle = ADR(vb.b[0]) THEN
          vb.clip := v.domain;
          vb.clipped := BatchUtil.ClipState.Unclipped
        ELSE
          MergeBatch(vb, vb.firstSingle, v.domain,
                     BatchUtil.ClipState.Unclipped, vb.scrollSource)
        END
      END;
      WITH newStart = ADR(ba.b[0]) DO
        newlen := ba.next - newStart;
        CopyBytes(newStart, vb.next, newlen)
      END;
      middle := vb.next;
      INC(vb.next, newlen);
      vb.firstSingle := vb.next;
      DEC(v.remaining, newlen);
      MergeBatch(vb, middle, ba.clip, ba.clipped, ba.scrollSource);
      vb.containsPicture := vb.containsPicture OR ba.containsPicture;
      IF ba.containsPicture THEN
        PictureRep.IncrementBatch(ba); (* increment /ba/ because it will be
                                         decremented by the Free *)
      END;
    END;
    Batch.Free(ba)
  END ExtendBatch;

TYPE 
  RedisplayRec = RECORD v: VBT.T; depth: INTEGER END;
  RedisplayList = REF ARRAY OF RedisplayRec;

VAR
  rdList: RedisplayList := NIL;
  rdCount, rdCoverage := 0;
  rdMu := NEW(MUTEX);
  (* all access to rdList is controlled by rdMu. The
     locking level of muRedisplayList is greater than the locking level
     of all VBT's. *)
  rdClosure := NEW(Thread.SizedClosure, stackSize := 20000, apply := RdApply);
  
CONST
  InitialRdSize = 8;
  
PROCEDURE Mark(v: VBT.T) RAISES {} =
  BEGIN 
    IF NOT Prop.Marked IN v.props THEN
      v.props := v.props + Props{Prop.Marked};
      IF v.st = NIL THEN RETURN END;
      LOCK rdMu DO
        IF rdList = NIL THEN
          rdList := NEW(RedisplayList, InitialRdSize);
          rdCount := 0
        ELSIF (rdCount = NUMBER(rdList^)) THEN
          WITH new = NEW(RedisplayList, 2 * rdCount) DO
            SUBARRAY(new^, 0, rdCount) := rdList^;
            rdList := new
          END
        END;
        rdList[rdCount].v := v;
        INC(rdCount);
        IF rdCoverage = 0 THEN
          INC(rdCoverage);
          EVAL Thread.Fork(rdClosure)
        END
      END
    END
  END Mark;

PROCEDURE CoverRedisplay() = 
  BEGIN
    LOCK rdMu DO INC(rdCoverage) END 
  END CoverRedisplay;

PROCEDURE UncoverRedisplay() = 
  VAR zero: BOOLEAN; BEGIN
    LOCK rdMu DO DEC(rdCoverage); zero := rdCoverage = 0 END;
    IF zero THEN Redisplay() END
  END UncoverRedisplay;
  
PROCEDURE RdApply(<*UNUSED*> self: Thread.Closure): REFANY RAISES {} =
  BEGIN
    LOCK VBT.mu DO UncoverRedisplay() END; RETURN NIL
  END RdApply;

TYPE
  DepthArray = REF ARRAY OF RECORD n := 0 END;

PROCEDURE Redisplay() RAISES {} =
  VAR
    list: RedisplayList;
    dcount: DepthArray;
    n: INTEGER;
  BEGIN 
    LOOP
      list := GetRedisplayList();
      IF list = NIL THEN RETURN END;
      dcount := NEW(DepthArray, 10);
      n := 0;
      WHILE n # NUMBER(list^) AND list[n].v # NIL DO
        WITH d = list[n].depth DO
          IF d > LAST(dcount^) THEN
            WITH new = NEW(DepthArray, 2 * d) DO
              SUBARRAY(new^, 0, NUMBER(dcount^)) := dcount^;
              dcount := new
            END
          END;
          INC(dcount[d].n)
        END;
        INC(n)
      END;
      (* n = number of VBTs in list *)
      (* All d: dcount[d].n = # VBTs in list with depth d. *)
      IF n = 0 THEN RETURN END;
      FOR d := 1 TO LAST(dcount^) DO 
        INC(dcount[d].n, dcount[d-1].n)
      END;
      (* All d: dcount[d].n = # VBTs in list with depth at most d. *)
      WITH v = NEW(REF ARRAY OF VBT.T, n) DO
        FOR i := 0 TO n - 1 DO
          v[dcount[list[i].depth - 1].n] := list[i].v;
          INC(dcount[list[i].depth - 1].n);
          (* All d: dcount[d-1].n = # VBTs in list with depth < d,
             or with depth = d that have been copied into v. *)
        END;
        (* v has all the VBTs in the list and is sorted by depth *)
        FOR i := 0 TO n - 1 DO
          IF Prop.Marked IN v[i].props THEN
            VBTClass.Redisplay(v[i])
          END
        END
      END
    END
  END Redisplay;

PROCEDURE GetRedisplayList(): RedisplayList =
  VAR res: RedisplayList;
  BEGIN
    LOCK rdMu DO
      res := rdList;
      FOR i := 0 TO rdCount-1 DO
        res[i].depth := Depth(res[i].v)
      END;
      rdList := NIL;
      rdCount := 0;
      RETURN res;
    END;
  END GetRedisplayList;

PROCEDURE Depth(v: VBT.T): INTEGER =
    VAR res := 0;
  BEGIN
    WHILE v # NIL DO INC(res); v := v.parent END;
    RETURN res
  END Depth;

PROCEDURE MaxRepeat(v: VBT.T): CARDINAL =
  BEGIN (* LL = v *)
    RETURN v.remaining DIV ADRSIZE(PaintPrivate.CommandRec)
  END MaxRepeat;

PROCEDURE PaintRepeat( v: VBT.T; READONLY clip: ARRAY OF Rect.T) =
  VAR pRep: PaintPrivate.RepeatPtr;
  BEGIN 
    IF NUMBER(clip) = 0 THEN RETURN END;
    IF NUMBER(clip) > MaxRepeat(v) THEN Crash() END;
    pRep := v.batch.next;
    FOR i := 0 TO LAST(clip) DO
      pRep.command := PaintPrivate.PaintCommand.RepeatCom;
      pRep.clip := clip[i];
      pRep := pRep + ADRSIZE(PaintPrivate.CommandRec)
    END;
    v.batch.next := pRep;
    DEC(v.remaining, NUMBER(clip) * ADRSIZE(PaintPrivate.CommandRec));
  END PaintRepeat;
  
PROCEDURE PaintSingle (         v   : VBT.T;
                       READONLY clip: Rect.T;
                                com : PaintPrivate.CommandPtr) =
  BEGIN
    WITH len  = PaintPrivate.CommandLength(com),
         lenb = len * ADRSIZE(Word.T)            DO
      IF v.remaining < lenb THEN NewBatch(v, len) END;
      CopyBytes(com, v.batch.next, lenb);
      com := v.batch.next;
      WITH command = com.command DO
        CASE command OF
        | PaintPrivate.PaintCommand.TextCom =>
            WITH t = LOOPHOLE(com, PaintPrivate.TextPtr) DO
              IF NOT Rect.Subset(com.clip, clip) THEN
                t.props :=
                  t.props + PaintPrivate.Props{PaintPrivate.Prop.Clipped}
              END
            END;
        | PaintPrivate.PaintCommand.ExtensionCom =>
            WITH op = LOOPHOLE(com, PaintExt.PicturePtr) DO
              IF op.ext.subCommand = PaintExt.PictureCommand THEN
                LOOPHOLE(op.completion, Completion.T).inc();
                (* see PaintExt.i3 *)
                BatchUtil.SetPicture(v.batch);
                (* make sure picture flag is propagate up... *)
              END;
            END;
        ELSE                     (* skip *)
        END;
      END;
      com.clip := clip;
      INC(v.batch.next, lenb);
      DEC(v.remaining, lenb)
    END
  END PaintSingle;
  
PROCEDURE Scroll (         v   : VBT.T;
                  READONLY clip: Rect.T;
                           com : PaintPrivate.ScrollPtr) =
  BEGIN
    IF v.remaining < ADRSIZE(PaintPrivate.ScrollRec) THEN
      NewBatch(
        v, PaintPrivate.ComSize[PaintPrivate.PaintCommand.ScrollCom])
    END;
    v.batch.scrollSource :=
      Rect.Join(v.batch.scrollSource, Rect.Move(clip, com.delta));
    PaintSingle(v, clip, LOOPHOLE(com, PaintPrivate.CommandPtr));
  END Scroll;
  
PROCEDURE MouseCrash (<*UNUSED*>          v : VBT.T;
                      <*UNUSED*> READONLY cd: VBT.MouseRec) =
  BEGIN
    Crash()
  END MouseCrash;

PROCEDURE PositionCrash (<*UNUSED*>          v : VBT.T;
                         <*UNUSED*> READONLY cd: VBT.PositionRec) =
  BEGIN
    Crash()
  END PositionCrash;

PROCEDURE ReadCrash (<*UNUSED*> v : VBT.T;
                     <*UNUSED*> s : VBT.Selection;
                     <*UNUSED*> tc: CARDINAL       ): VBT.Value =
  BEGIN
    Crash();  <*ASSERT FALSE*>
  END ReadCrash;

PROCEDURE WriteCrash(<*UNUSED*> v: VBT.T;
                     <*UNUSED*> s: VBT.Selection;
                     <*UNUSED*> val: VBT.Value;
                     <*UNUSED*> tc: CARDINAL) =
  BEGIN
    Crash()
  END WriteCrash;

PROCEDURE KeyCodeCrash(<*UNUSED*> v: VBT.T;
                       <*UNUSED*> READONLY cd: VBT.KeyRec) =
  BEGIN
    Crash()
  END KeyCodeCrash;

PROCEDURE MiscCodeCrash(<*UNUSED*> v: VBT.T;
                        <*UNUSED*> READONLY cd: VBT.MiscRec) =
  BEGIN
    Crash()
  END MiscCodeCrash;

PROCEDURE ReshapeCrash(<*UNUSED*> v: VBT.T;
                       <*UNUSED*> READONLY cd: VBT.ReshapeRec) =
  BEGIN
    Crash()
  END ReshapeCrash;

PROCEDURE RepaintCrash(<*UNUSED*> v: VBT.T;
                       <*UNUSED*> READONLY rgn: Region.T) =
  BEGIN
    Crash()
  END RepaintCrash;

PROCEDURE RescreenCrash(<*UNUSED*> v: VBT.T;
                        <*UNUSED*> READONLY cd: VBT.RescreenRec) =
  BEGIN
    Crash()
  END RescreenCrash;

PROCEDURE RedisplayCrash(<*UNUSED*> v: VBT.T) =
  BEGIN
    Crash()
  END RedisplayCrash;

PROCEDURE DiscardCrash(<*UNUSED*> v: VBT.T) =
  BEGIN
    Crash()
  END DiscardCrash;

PROCEDURE ShapeCrash(<*UNUSED*> v: VBT.T;
                     <*UNUSED*> ax: Axis.T;
                     <*UNUSED*> n: CARDINAL): VBT.SizeRange =
  BEGIN
    Crash();  <*ASSERT FALSE*>
  END ShapeCrash;

EXCEPTION FatalError;

PROCEDURE Crash () =
  <*FATAL FatalError*>
  BEGIN
    RAISE FatalError
  END Crash;

BEGIN
  LOCK qmu DO
    IF numWorkers = 0 THEN
      EVAL Thread.Fork(NEW(Thread.SizedClosure, apply := MeterMaid, 
        stackSize := 20000));
      INC(numWorkers)
    END
  END
END VBTRep.
