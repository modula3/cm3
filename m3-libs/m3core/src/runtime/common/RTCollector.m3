(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* Portions Copyright 1996-2000, Critical Mass, Inc.         *)
(* See file COPYRIGHT-CMASS for details.                     *)
(*                                                           *)
(*| Last modified on Sat Nov 19 09:37:57 PST 1994 by kalsow  *)
(*|      modified on Fri Aug  5 14:04:35 PDT 1994 by jdd     *)
(*|      modified on Wed Jun  2 15:00:17 PDT 1993 by muller  *)
(*|      modified on Wed Apr 21 13:14:37 PDT 1993 by mcjones *)
(*|      modified on Wed Mar 10 11:01:47 PST 1993 by mjordan *)

UNSAFE MODULE RTCollector EXPORTS RTCollector, RTCollectorSRC,
                                  RTHeapRep, RTWeakRef, RTHooks;

IMPORT RT0, RTHeapEvent, RTHeapMap, RTIO;
IMPORT RTMisc, RTOS, RTParams, RTPerfTool, RTProcess, RTType;
IMPORT Word, Thread, RTThread;
IMPORT TextLiteral AS TextLit, RTLinker, Time;

FROM RT0 IMPORT Typecode, TypeDefn;
TYPE TK = RT0.TypeKind;

(* The allocator/garbage collector for the traced heap is an adaptation of
   the algorithm presented in the WRL Research Report 88/2, ``Compacting
   Garbage Collection with Ambiguous Roots'', by Joel F.  Bartlett; see
   this report for a detailed presentation.  John DeTreville modified it to
   be incremental, generational, and VM-synchronized.

   The allocator/collector for the untraced heap is simply malloc/free. *)

(* Much of the code below incorrectly assumes no difference between ADRSIZE
   and BYTESIZE. *)

(* In the following procedures, "RTType.Get(tc)" will fail if "tc" is not
   proper. *)

(*** RTCollector ***)

PROCEDURE Disable () =
  BEGIN
    TRY
      RTOS.LockHeap();
      FinishGC();
      INC(disableCount);
      partialCollectionNext := FALSE;
    FINALLY
      RTOS.UnlockHeap();
    END;
    IF perfOn THEN PerfAllow(); END;
  END Disable;

PROCEDURE Enable () =
  BEGIN
    TRY
      RTOS.LockHeap();
      DEC(disableCount);
      CollectEnough();
    FINALLY
      RTOS.UnlockHeap();
      IF perfOn THEN PerfAllow(); END;
    END;
  END Enable;

PROCEDURE DisableMotion () =
  BEGIN
    TRY
      RTOS.LockHeap();
      INC(disableMotionCount);
    FINALLY
      RTOS.UnlockHeap();
    END;
    IF perfOn THEN PerfAllow(); END;
  END DisableMotion;

PROCEDURE EnableMotion () =
  BEGIN
    TRY
      RTOS.LockHeap();
      DEC(disableMotionCount);
      CollectEnough();
    FINALLY
      RTOS.UnlockHeap();
      IF perfOn THEN PerfAllow(); END;
    END;
  END EnableMotion;

PROCEDURE Collect () =
  BEGIN
    TRY
      RTOS.LockHeap();
      FinishGC();
      StartGC();
      FinishGC();
    FINALLY
      RTOS.UnlockHeap();
    END;
  END Collect;

(*** RTCollectorSRC ***)

(* StartCollection starts a total collection, if none is in progress and if
   collection and motion are enabled. *)

PROCEDURE StartCollection () =
  BEGIN
    TRY
      RTOS.LockHeap();
      StartGC();
    FINALLY
      RTOS.UnlockHeap();
    END;
  END StartCollection;

(* FinishCollection finishes the current collection, if one is on
   progress. *)

PROCEDURE FinishCollection () =
  BEGIN
    TRY
      RTOS.LockHeap();
      FinishGC();
    FINALLY
      RTOS.UnlockHeap();
    END;
  END FinishCollection;

(* StartBackgroundCollection starts the background thread, if not already
   started *)

VAR startedBackground := FALSE;

PROCEDURE StartBackgroundCollection () =
  VAR start := FALSE; 
  BEGIN
    TRY
      RTOS.LockHeap();
      IF NOT startedBackground THEN
        start := TRUE;
        startedBackground := TRUE;
      END;
    FINALLY
      RTOS.UnlockHeap();
    END;
    IF start THEN
      EVAL Thread.Fork(NEW(Thread.Closure, apply := BackgroundThread));
    END;
  END StartBackgroundCollection;

(* StartForegroundCollection starts the foreground thread, if not already
   started *)

VAR startedForeground := FALSE;

PROCEDURE StartForegroundCollection () =
  VAR start := FALSE;
  BEGIN
    TRY
      RTOS.LockHeap();
      IF NOT startedForeground THEN
        start := TRUE;
        startedForeground := TRUE;
      END;
    FINALLY
      RTOS.UnlockHeap();
    END;
    IF start THEN
      EVAL Thread.Fork(NEW(Thread.Closure, apply := ForegroundThread));
    END;
  END StartForegroundCollection;

PROCEDURE DisableVM() =
BEGIN
  (* This function does nothing and exists solely for source compatibility
   * with older cm3. *)
END DisableVM;

(* ------------------------------- low-level allocation and collection *)

(* We assume that references (values of the types ADDRESS and REFANY) are
   the addresses of addressable locations and that locations with
   successive addresses are contiguous (that is, if a points to a
   n-locations referent then these n locations are at addresses a, a+1,
   ..., a+n-1).

   The memory is viewed as a collection of pages.  Each page has a number
   that identifies it, based on the addresses that are part of this page:
   page p contains the addresses p * BytesPerPage to (p+1) * BytesPerPage -
   1.

   The page size must be a multiple of the header size (see below).  Given
   our conventions about page boundaries, this implies that the first
   location of a page is properly aligned for a Header. *)

(* The array desc and the global variables p0, and p1 describe the pages
   that are part of the traced heap.  Either p0 and p1 are equal to Nil and
   no pages are allocated; or both are valid pages and page p is allocated
   iff

|          p0 <= p < p1
|      AND desc[p - p0] >= 0

   NUMBER (desc) must be equal to p1 - p0 if there are allocated pages.
   Index i in desc correspond to page i + p0; that is p0 is the number of
   the first page available in desc, and it must be in [p0 ..  p1) if there
   are allocated pages. *)

VAR desc: UNTRACED REF ARRAY OF BITS 8 FOR [-1..1];
    (* < 0 => unallocated
       = 0 => first page in a block
       > 0 => continued page in a block *)

PROCEDURE FirstPage (p: Page): Page =
  BEGIN
    IF p < p0 OR p >= p1 OR desc[p - p0] < 0 THEN RETURN Nil END;
    IF desc[p - p0] = 0 THEN RETURN p END;
    WHILE desc[p - p0] > 0 DO DEC(p) END;
    <*ASSERT desc[p - p0] = 0*>
    RETURN p;
  END FirstPage;

(* We keep the number of allocated pages in a global variable; it should
   satify the invariant:

|     allocatedPages = sigma (i = p0, p1-1,
|                              space [i - p0] # Unallocated)
|                                  if there are allocated pages,
|                      = 0 otherwise. *)

(* Each referent is immediately preceded by a header that describes the
   type of the referent.  In the user world, this header is not visible;
   that is, a REFANY is the address of the referent, not the address of the
   header.

   Each referent is immediately followed by padding space so the combined
   size referent size + padding is a multiple of the header size.
   Actually, the low level routines are given a data size which is the sum
   of the referent size and padding size and assume this data size is a
   multiple of the header size.

   With this padding, addresses of headers and referent will always be
   multiple of ADRSIZE (Header).

   The combination of header/referent/padding space is called a "heap
   object".  The size of a heap object is the size of the header, plus the
   size of the referent, plus the size of the padding.  The alignment of a
   heap object is the greatest of the alignment of header and the alignment
   of the referent.

   We make the following assumptions:

   - alignment of headers is such what the addressable location following
   any properly aligned header is properly aligned for the type ADDRESS;
   and, for every referent: referent adrSize + padding adrSize >= ADRSIZE
   (ADDRESS)

   [During the garbage collection, we move heap objects.  But we need to
   keep the forwarding information somewhere.  This condition ensures that
   we can store the new address of the referent in the first word of the
   old referent.]

   - the pages are aligned more strictly than the headers (this means that
   the page size is a multiple of the header alignment).

   [We can put a header at the beginning of a page] *)

TYPE
  RefReferent = ADDRESS;

PROCEDURE HeaderOf (r: RefReferent): RefHeader =
  BEGIN
    RETURN LOOPHOLE(r - ADRSIZE(Header), RefHeader);
  END HeaderOf;

(* If a page is allocated, it can be normal or continued.  In the first
   case, there is a heap object just at the beginning of the page and
   others following.  The second case occurs when a heap object was too
   large to fit on a page: it starts at the beginning of a normal page and
   overflows on contiguous continued pages.  Whatever space is left on the
   last continued page is never used for another object or filler.  In
   other words, all the headers are on normal pages.

   Heap objects do not need to be adjacent.  Indeed, alignment constraints
   would make it difficult to ensure that property.  Filler objects may
   appear before objects to align them, or after the last object on a
   normal page to fill the page. *)

(* We need to be able to determine the size of a referent during
   collection; here is a functions to do just that.  It must be called with
   a non-nil pointer to the Header of a heap object that is there (has not
   been moved). *)

PROCEDURE TextLitSize (h: RefHeader): CARDINAL =
  VAR
    txt := LOOPHOLE (h + ADRSIZE(Header), TextLiteral);
    len : INTEGER := txt.cnt;
  BEGIN
    IF (len >= 0)
      THEN INC (len); (* null CHAR *)
      ELSE len := ( - len + 1 (*null WIDECHAR*) ) 
                  * BYTESIZE ( WIDECHAR ) ; 
    END;
    RETURN ADR (txt.buf[len]) - LOOPHOLE (txt, ADDRESS);
  END TextLitSize;

PROCEDURE OpenArraySize (h: RefHeader; adef: RT0.ArrayTypeDefn): CARDINAL =
(* The referent is an open array; it has the following layout:
|     pointer to the elements (ADDRESS)
|     size 1
|     ....
|     size n
|     optional padding
|     elements
|     ....
   where n is the number of open dimensions (given by the definition)
   and each size is the number of elements along the dimension. *)

  VAR
    res: INTEGER;
    sizes: UNTRACED REF INTEGER := h + ADRSIZE(Header) + ADRSIZE(ADDRESS);
                                                         (* ^ elt pointer*)
  BEGIN
    res := 1;
    FOR i := 0 TO adef.nDimensions - 1 DO
      res := res * sizes^;
      INC(sizes, ADRSIZE(sizes^));
    END;
    res := res * adef.elementSize;
    res := RTMisc.Upper(res + adef.common.dataSize, BYTESIZE(Header));
    RETURN res;
  END OpenArraySize;

PROCEDURE ReferentSize (h: RefHeader): CARDINAL =
  VAR
    res: INTEGER;
    tc: Typecode := h.typecode;
    def: TypeDefn;
  BEGIN
    IF tc = Fill_1_type THEN RETURN 0; END;

    IF tc = Fill_N_type THEN
      res := LOOPHOLE(h + ADRSIZE(Header), UNTRACED REF INTEGER)^;
      RETURN res - BYTESIZE(Header);
    END;

    IF tc = RT0.TextLitTypecode THEN RETURN TextLitSize(h) END;

    def := RTType.Get (tc);

    IF (def.kind # ORD (TK.Array)) THEN
      (* the typecell datasize tells the truth *)
      RETURN def.dataSize;
    END;

    (* Otherwise, the referent is an open array *)
    RETURN OpenArraySize(h, LOOPHOLE(def, RT0.ArrayTypeDefn));
  END ReferentSize;

(* The convention about page numbering allows for a simple conversion from
   an address to the number of the page in which it is, as well as from a
   page number to the first address is contains: *)

PROCEDURE ReferentToPage (r: RefReferent): Page =
  (* VAR p: INTEGER := LOOPHOLE(r, INTEGER) DIV BytesPerPage; *)
  VAR p: INTEGER := Word.RightShift (LOOPHOLE(r, INTEGER), LogBytesPerPage);
  BEGIN
    RETURN p;
  END ReferentToPage;

PROCEDURE RefToPage (r: RefPage): Page =
  (* VAR p: INTEGER := LOOPHOLE(r, INTEGER) DIV BytesPerPage; *)
  VAR p: INTEGER := Word.RightShift (LOOPHOLE(r, INTEGER), LogBytesPerPage);
  BEGIN
    RETURN p;
  END RefToPage;

PROCEDURE AddressToPage (r: ADDRESS): RefPage =
  (* VAR p: INTEGER := LOOPHOLE(r, INTEGER) DIV BytesPerPage; *)
  VAR p: INTEGER := Word.RightShift (LOOPHOLE(r, INTEGER), LogBytesPerPage);
  BEGIN
    p := FirstPage(p);
    RETURN PageToRef(p);
  END AddressToPage;

PROCEDURE PageToRef (p: Page): RefPage =
  BEGIN
    RETURN LOOPHOLE(p * BytesPerPage, RefPage);
  END PageToRef;

(* To move a heap object to the new space, modifying the original reference
   to it *)

TYPE Mover = RTHeapMap.Visitor OBJECT OVERRIDES apply := Move END;

PROCEDURE Move (<*UNUSED*> self: Mover;  cp: ADDRESS) =
  VAR
    refref := LOOPHOLE(cp, UNTRACED REF RefReferent);
    ref    := refref^;
    p      : INTEGER;
    hdr    : RefHeader;
    page   : RefPage;
  BEGIN
    IF ref = NIL THEN RETURN END;
    (* ignore low-bit tagged pseudo-references *)
    IF Word.And (LOOPHOLE(ref, Word.T), 1) # 0 THEN RETURN END;

    (* INLINE: hdr := HeaderOf(ref); *)
    hdr := LOOPHOLE(ref - ADRSIZE(Header), RefHeader);
    IF hdr.typecode = RT0.TextLitTypecode THEN RETURN END;
    IF hdr.forwarded THEN
      (* if already moved, just update the reference *)
      refref^ := LOOPHOLE(ref, UNTRACED REF RefReferent)^;
      RETURN;
    END;

    (* INLINE: p := ReferentToPage(ref); *)
    p := Word.RightShift (LOOPHOLE(ref, INTEGER), LogBytesPerPage);
    <*ASSERT desc[p - p0] = 0*>

    (* INLINE: page := PageToRef(p); *)
    page := LOOPHOLE(p * BytesPerPage, RefPage);

    IF page.desc.space # Space.Previous THEN RETURN END;

    IF page.nb > 1 THEN
      (* if this is a large object, just promote the pages *)
      WITH def = RTType.Get(hdr.typecode) DO
        IF (def.gc_map = NIL) AND (def.kind # ORD(TK.Obj))
          THEN PromotePage(page, PromoteReason.LargePure);
          ELSE PromotePage(page, PromoteReason.LargeImpure);
        END;
      END;
      RETURN;
    END;

    (* otherwise, move the object *)
    VAR
      def      := RTType.Get(hdr.typecode);
      dataSize := ReferentSize(hdr);
      np       : RefReferent;
    BEGIN
      IF (def.gc_map = NIL) AND (def.kind # ORD(TK.Obj)) THEN
        np := AllocCopy(dataSize, def.dataAlignment, pureCopy);
        IF (np = NIL) THEN
          (* promote as if large so we can bail out gracefully *)
          PromotePage(page, PromoteReason.LargePure);
          RETURN;
        END;
        WITH nh = HeaderOf(np) DO
          RTMisc.Copy(hdr, nh, BYTESIZE(Header) + dataSize);
          <*ASSERT NOT nh.gray*>
          nh.dirty := TRUE;
        END;
      ELSE
        np := AllocCopy(dataSize, def.dataAlignment, impureCopy);
        IF (np = NIL) THEN
          (* promote as if large so we can bail out gracefully *)
          PromotePage(page, PromoteReason.LargeImpure);
          RETURN;
        END;
        WITH nh = HeaderOf(np) DO
          RTMisc.Copy(hdr, nh, BYTESIZE(Header) + dataSize);
          nh.gray := TRUE;
          nh.dirty := FALSE;
        END;
      END;
      IF def.kind = ORD (TK.Array) THEN
        (* open array: update the internal pointer *)
        LOOPHOLE(np, UNTRACED REF ADDRESS)^ := np + def.dataSize;
      END;
      hdr.forwarded := TRUE;
      LOOPHOLE(ref, UNTRACED REF RefReferent)^ := np;
      refref^ := np;
    END;
  END Move;

(* Determines whether a REF has yet been moved into the new space.  Follows
   the logic in "Move".*)

PROCEDURE Moved (ref: RefReferent): BOOLEAN =
  VAR
    p   : INTEGER;
    hdr : RefHeader;
    page: RefPage;
  BEGIN
    IF ref = NIL THEN RETURN TRUE; END;
    (* ignore low-bit tagged pseudo-references *)
    IF Word.And (LOOPHOLE(ref, Word.T), 1) # 0 THEN RETURN TRUE END;

    (* INLINE: hdr := HeaderOf(ref); *)
    hdr := LOOPHOLE(ref - ADRSIZE(Header), RefHeader);
    IF hdr.typecode = RT0.TextLitTypecode THEN RETURN TRUE END;

    (* INLINE: p := ReferentToPage(ref); *)
    p := Word.RightShift (LOOPHOLE(ref, INTEGER), LogBytesPerPage);
    <*ASSERT desc[p - p0] = 0*>

    (* INLINE: page := page := PageToRef(p); *)
    page := LOOPHOLE(p * BytesPerPage, RefPage);

    (* check the space *)
    IF page.desc.space # Space.Previous THEN RETURN TRUE END;

    (* check the forwarded bit *)
    RETURN hdr.forwarded;
  END Moved;

(* When an allocated page is referenced by the stack, we have to move it to
   the next space and insert it in the list of promoted pages.  In the case
   where the page is actually part of a group of pages for a big referent,
   we have to promote all these pages to the new space, but only the first
   one needs to be inserted in the queue, as it is the only one containing
   referent headers.

   This routine is passed to the Threads implementation.  It is called for
   each stack, where start and stop are the addresses of the first and last
   word of the stack under consideration. *)

PROCEDURE NoteStackLocations (start, stop: ADDRESS) =
  VAR fp : UNTRACED REF ADDRESS := start;
  BEGIN
    IF NOT (start < stop) THEN RETURN END;
    stop := stop - ADRSIZE (ADDRESS); (* so we don't overrun the valid addresses *)
    WHILE fp <= stop DO               (* with the memory read on the next line.  *)
      WITH page = AddressToPage(fp^) DO
        IF page # NIL AND page.desc.space = Space.Previous THEN
          IF page.desc.pure
            THEN PromotePage(page, PromoteReason.AmbiguousPure);
            ELSE PromotePage(page, PromoteReason.AmbiguousImpure);
          END;
        END;
      END;
      INC(fp, RTThread.PointerAlignment);
    END;
  END NoteStackLocations;

TYPE
  PromoteReason = {
    OldClean, OldPure, OldImpure,
    LargePure, LargeImpure,
    AmbiguousPure, AmbiguousImpure
  };

VAR promoteGeneration: Generation;

PROCEDURE PromotePage (page: RefPage;  r: PromoteReason) =
  BEGIN
    WITH d = page.desc DO
      d.generation := promoteGeneration;
      d.space := Space.Current;
      CASE r OF
      | PromoteReason.OldClean =>
        (* no need to scan *)
        <*ASSERT NOT d.pure*>
        d.note := Note.OlderGeneration;
        d.pure := FALSE; d.gray := FALSE; d.clean := TRUE;
      | PromoteReason.OldPure =>
        d.note := Note.OlderGeneration;
        d.pure := TRUE;  d.gray := FALSE; d.clean := FALSE;
      | PromoteReason.OldImpure =>
        d.note := Note.OlderGeneration;
        d.pure := FALSE; d.gray := TRUE;  d.clean := TRUE;
        GrayBetween(page + ADRSIZE(PageHdr), page + BytesPerPage, r);
        PushPage(page);
      | PromoteReason.LargePure =>
        d.note := Note.Large;
        d.pure := TRUE;  d.gray := FALSE; d.clean := FALSE;
      | PromoteReason.LargeImpure =>
        d.note := Note.Large;
        d.pure := FALSE; d.gray := TRUE;  d.clean := TRUE;
        GrayBetween(page + ADRSIZE(PageHdr), page + BytesPerPage, r);
        PushPage(page);
      | PromoteReason.AmbiguousPure =>
        d.note := Note.AmbiguousRoot;
        d.pure := TRUE;  d.gray := FALSE; d.clean := FALSE;
      | PromoteReason.AmbiguousImpure =>
        d.note := Note.AmbiguousRoot;
        d.pure := FALSE; d.gray := TRUE;  d.clean := FALSE;
        GrayBetween(page + ADRSIZE(PageHdr), page + BytesPerPage, r);
      END;
      INC (n_promoted, page.nb);
      IF perfOn THEN PerfChange(page); END;
    END;
  END PromotePage;

PROCEDURE GrayBetween (h, he: RefHeader; r: PromoteReason) =
  BEGIN
    <* ASSERT h < he *>
    <* ASSERT Word.And (LOOPHOLE (h, INTEGER), 3) = 0 *>
    REPEAT
      VAR hdr := h^;
      BEGIN
        <* ASSERT NOT hdr.forwarded *>
        IF hdr # FillHeader1 AND hdr # FillHeaderN THEN
          IF r # PromoteReason.OldImpure OR hdr.dirty THEN
            hdr.dirty := FALSE;
            hdr.gray := TRUE;
            h^ := hdr;
          END;
        END;
      END;
      INC(h, ADRSIZE(Header) + ReferentSize(h));
    UNTIL h >= he;
  END GrayBetween;

PROCEDURE FlushThreadState (VAR thread: ThreadState) =
  BEGIN
    WITH pool = thread.pool DO
      pool.next := NIL;
      pool.limit := NIL;
      pool.page := NIL;
    END;
  END FlushThreadState;

PROCEDURE InsertFiller (start: RefHeader; n: INTEGER) =
  BEGIN
    IF n = 0 THEN
      (* nothing to do *)
    ELSIF n = ADRSIZE(Header) THEN
      start^ := FillHeader1;
    ELSIF n >= ADRSIZE(Header) + ADRSIZE(INTEGER) THEN
      start^ := FillHeaderN;
      LOOPHOLE(start + ADRSIZE(Header), UNTRACED REF INTEGER)^ := n;
    ELSE
      <* ASSERT FALSE *>
    END;
  END InsertFiller;

TYPE CollectorState = {Zero, One, Two, Three, Four, Five};

VAR collectorState := CollectorState.Zero;

VAR
  threshold := ARRAY [0 .. 1] OF REAL {FLOAT(InitialPages DIV 4), 1.0};
(* start a collection as soon as current space reaches threshold[0] /
   threshold[1] pages; the initial value is 1/4 InitialPages *)

VAR
  partialCollection: BOOLEAN;    (* whether the collection in progress is
                                    partial, involving only the newer
                                    generation *)
  partialCollectionNext: BOOLEAN := FALSE; (* whether the next collection
                                              should be partial *)

VAR collectorOn: BOOLEAN := FALSE;

VAR
  signalBackground := FALSE;     (* should signal background collector
                                    thread *)
  signalWeak := FALSE;           (* should signal weak cleaner thread *)

PROCEDURE CollectEnough () =
  VAR t0, t1: Time.T;
  BEGIN
    IF collectorOn THEN RETURN END;
    IF NOT Behind() THEN RETURN END;
    TRY
      CollectorOn();
      t0 := Time.Now();
      IF incremental AND RTLinker.incremental THEN
        REPEAT CollectSome();
        UNTIL NOT Behind() OR collectorState = CollectorState.Zero;
      ELSE
        WHILE collectorState = CollectorState.Zero DO CollectSome(); END;
        REPEAT CollectSome(); UNTIL collectorState = CollectorState.Zero;
      END;
    FINALLY
      t1 := Time.Now();
      cycleCost := cycleCost + (t1 - t0);
      CollectorOff();
      IF tsIndex >= 0 THEN
        tStamps[tsIndex] := t0; INC(tsIndex);
        tStamps[tsIndex] := t1; INC(tsIndex);
      END;
    END;
  END CollectEnough;

PROCEDURE Behind (): BOOLEAN =
  BEGIN
    IF disableCount + disableMotionCount > 0
         AND collectorState = CollectorState.Zero THEN
      RETURN FALSE;
    END;
    IF foregroundWaiting THEN
      RTOS.BroadcastHeap();
      RETURN FALSE;
    END;
    IF collectorState = CollectorState.Zero THEN
      RETURN FLOAT(n_new + n_copied + n_promoted) * threshold[1] >= threshold[0];
    ELSE
      RETURN FLOAT(n_new) * gcRatio >= FLOAT(n_copied);
    END;
  END Behind;

PROCEDURE CollectorOn () =
  (* LL >= RTOS.LockHeap *)
  BEGIN
    <* ASSERT NOT collectorOn *>
    collectorOn := TRUE;
    IF impureCopy.page # NIL THEN
      <*ASSERT impureCopy.page.desc.gray*>
    END;
  END CollectorOn;

PROCEDURE CollectorOff () =
  (* LL >= RTOS.LockHeap *)
  BEGIN
    <* ASSERT collectorOn *>

    IF impureCopy.page # NIL THEN
      <*ASSERT impureCopy.page.desc.gray*>
    END;

    collectorOn := FALSE;

    IF signalBackground OR signalWeak THEN
      signalBackground := FALSE;
      signalWeak := FALSE;
      RTOS.BroadcastHeap();
    END;
  END CollectorOff;

PROCEDURE CollectSome () =
  BEGIN
    <* ASSERT disableCount = 0 *>
    CASE collectorState OF
    | CollectorState.Zero => CollectSomeInStateZero();
    | CollectorState.One => CollectSomeInStateOne();
    | CollectorState.Two => CollectSomeInStateTwo();
    | CollectorState.Three => CollectSomeInStateThree();
    | CollectorState.Four => CollectSomeInStateFour();
    | CollectorState.Five => CollectSomeInStateFive();
    END;
  END CollectSome;

(* Start a collection *)

VAR
  mover      : Mover    := NIL;
  cycleCost  : Time.T   := 0.0D0;(* running cost of current cycle *)
  cycleLength: CARDINAL := 1;    (* current planned cycle length *)
  cycleL     : CARDINAL := 0;    (* length of current cycle, so far *)
  cycleNews  : CARDINAL;         (* the number of new pages this cycle *)
  minPrefixAvgCost: Time.T;      (* minimum average cost for a prefix of
                                    this cycle *)
  minCycleL  : CARDINAL;         (* the length of that prefix *)
  n_promoted : CARDINAL := 0;    (* # of pages promoted this cycle *)
  n_new      : CARDINAL := 0;	 (* # of pages allocated this cycle *)
  n_copied   : CARDINAL := 0;	 (* # of pages copied this cycle *)

PROCEDURE CollectSomeInStateZero () =
  BEGIN
    RTThread.SuspendOthers ();

    <* ASSERT disableCount + disableMotionCount = 0 *>
    (* compute some costs relative to previous collection *)
    INC(cycleNews, n_new);
    VAR prefixAvgCost := cycleCost / FLOAT(cycleNews, Time.T);
    BEGIN
      IF prefixAvgCost < minPrefixAvgCost THEN
        minPrefixAvgCost := prefixAvgCost;
        minCycleL := cycleL;
      END;
    END;

    (* make generational decisions *)
    IF generational AND RTLinker.generational THEN
      promoteGeneration := Generation.Older;
      partialCollection := partialCollectionNext AND cycleL < cycleLength;
      IF NOT partialCollection THEN
        IF minCycleL = cycleLength THEN
          cycleLength := cycleLength + 1;
        ELSE
          cycleLength := MAX(cycleLength - 1, 1);
        END;
      END;
    ELSE
      promoteGeneration := Generation.Younger;
      partialCollection := FALSE;
    END;
    partialCollectionNext := TRUE;

    IF partialCollection THEN
      INC(cycleL);
    ELSE
      cycleL := 1;
      cycleCost := 0.0D0;
      cycleNews := 0;
      minPrefixAvgCost := LAST(Time.T);
      minCycleL := 0;
    END;

    InvokeMonitors (before := TRUE);

    IF perfOn THEN PerfBegin(); END;

    IF (partialCollection) THEN
      INC(minorCollections);
    ELSE
      INC(majorCollections);
    END;

    (* flip spaces; newspace becomes oldspace *)
    FOR p := p0 TO p1 - 1 DO
      IF desc[p - p0] = 0 THEN
        WITH page = PageToRef(p), d = page.desc DO
          IF d.space = Space.Current THEN
            d.space := Space.Previous;
            IF perfOn THEN PerfChange(page) END;
          END;
        END;
      END;
    END;

    IF perfOn THEN PerfFlip(); END;

    (* The 'new' nextSpace is empty *)
    n_new := 0;
    n_copied := 0;
    n_promoted := 0;

    (* Conservatively scan the stacks for possible pointers. *)
    (* Note: we must scan thread stacks before promoting old
       pages, because we want to make sure that old, impure, dirty
       pages referenced by threads are marked as ambiguous roots.
       Otherwise, these pages won't get cleaned before we return. *)
    RTThread.ProcessStacks(NoteStackLocations);
    (* Now, nothing in previous space is referenced by a thread. *)

    (* Promote any remaining "old" pages and unprotect everything else *)
    FOR p := p0 TO p1 - 1 DO
      IF desc[p - p0] = 0 THEN
        WITH page = PageToRef(p), d = page.desc DO
          IF d.space = Space.Previous THEN
            IF d.generation = Generation.Older THEN
              IF partialCollection THEN
                IF d.clean
                  THEN PromotePage(page, PromoteReason.OldClean);
                ELSIF d.pure
                  THEN PromotePage(page, PromoteReason.OldPure);
                  ELSE PromotePage(page, PromoteReason.OldImpure);
                END;
              END;
            ELSE
              <*ASSERT NOT d.clean*>
            END;
          END;
        END;
      END;
    END;
    (* Now, nothing in the previous space is clean or in the older
       generation. *)

    mover := NEW (Mover);  (* get one in the new space *)

    (* VAR/READONLY and WITH allow programmers to generate interior pointers
       to heap objects that are not true traced references.  These interior
       pointers can be held only on the stack or in registers.  Accesses
       through those pointers to reference fields will not be mediated by the
       read barrier, which means a mutator could load a white reference unless
       we do something about it.  To prevent mutators loading white references
       in this way, whenever a VAR or WITH is used to create an interior
       pointer in a program we run the read barrier on the reference from
       which that pointer is created, to make sure the target of the reference
       is black -- i.e., that it contains no white references.  This will
       prevent mutators from ever loading a white reference.  Thus, we must
       preserve the invariant after initiating GC that all stacks contain only
       black references (i.e., that they refer only to black pages).  We do
       that here by processing the pinned pages (i.e., promoted as directly
       reachable from the stacks/registers) and cleaning them to make them
       black.

       A similar problem holds for dirty pages and the generational collector.
       Since mutators holding interior pointers can freely store references
       into objects in the heap without running the write barrier, we run the
       write barrier on VAR parameters and WITH where the value is assigned in
       the body of the WITH.  The barrier makes sure that the objects for
       which the interior pointers are derived are marked as (potentially)
       dirty.  We must preserve this invariant after initiating GC to make
       sure such pages are left dirty.

       In both cases, CleanPage will do the right thing based on the page
       descriptors (clean/dirty, Older/Younger).
    *)

    FOR p := p0 TO p1 - 1 DO
      IF desc[p - p0] = 0 THEN
        WITH page = PageToRef(p), d = page.desc DO
          IF d.space = Space.Current THEN
            IF d.gray AND d.note = Note.AmbiguousRoot THEN
              <*ASSERT NOT d.clean*>
              CleanPage(page);
            END
          END
        END
      END
    END;

    (* Scan the global variables for possible pointers *)
    RTHeapMap.WalkGlobals (mover);

    IF perfOn THEN PerfPromotedRoots(); END;

    collectorState := CollectorState.One;
    IF backgroundWaiting THEN signalBackground := TRUE; END;

    RTThread.ResumeOthers ();
  END CollectSomeInStateZero;

(* Clean gray nodes *)

PROCEDURE CollectSomeInStateOne () =
  BEGIN
    IF NOT CopySome() THEN collectorState := CollectorState.Two; END;
    IF backgroundWaiting THEN signalBackground := TRUE; END;
  END CollectSomeInStateOne;

(* Walk weakly-referenced nodes to determine order in which to do cleanup,
   then cleanup gray nodes.  This should be broken down into parts, since
   it may be a lengthy operation. *)

PROCEDURE CollectSomeInStateTwo () =
  BEGIN
    PreHandleWeakRefs();
    collectorState := CollectorState.Three;
    IF backgroundWaiting THEN signalBackground := TRUE; END;
  END CollectSomeInStateTwo;

(* Clean gray nodes *)

PROCEDURE CollectSomeInStateThree () =
  BEGIN
    (* recursively copy all objects reachable from promoted objects.  marks
       "marka" and "markb" are cleared when objects move to the new
       space. *)
    IF NOT CopySome() THEN
      PostHandleWeakRefs();      (* must be called with no gray objects *)
      signalWeak := TRUE;
      collectorState := CollectorState.Four;
    END;
    IF backgroundWaiting THEN signalBackground := TRUE; END;
  END CollectSomeInStateThree;

(* Clean gray nodes *)

PROCEDURE CollectSomeInStateFour () =
  BEGIN
    IF NOT CopySome() THEN collectorState := CollectorState.Five; END;
    IF backgroundWaiting THEN signalBackground := TRUE; END;
  END CollectSomeInStateFour;

PROCEDURE CollectSomeInStateFive () =
  BEGIN
    (* free all oldspace pages; oldspace becomes freespace *)
    FOR p := p0 TO p1 - 1 DO
      IF desc[p - p0] = 0 THEN
        WITH page = PageToRef(p), d = page.desc DO
          IF d.space = Space.Previous THEN
            d.space := Space.Free;
            IF perfOn THEN PerfChange(page); END;
          END;
        END;
      END;
    END;

    RebuildFreelist();

    (* fill the rest of the current copy pages *)
    IF impureCopy.page # NIL THEN
      WITH page = impureCopy.page, d = page.desc DO
        d.gray := FALSE;
        IF d.clean THEN
          IF d.generation = Generation.Older THEN
            <* ASSERT d.note # Note.AmbiguousRoot *>
            <* ASSERT d.space = Space.Current *>
          ELSE
            d.clean := FALSE;
          END;
        END;
        IF perfOn THEN PerfChange(page); END;
      END;
    END;
    <*ASSERT impureCopyStack = NIL*>

    pureCopy.page  := NIL;    impureCopy.page  := NIL;
    pureCopy.next  := NIL;    impureCopy.next  := NIL;
    pureCopy.limit := NIL;    impureCopy.limit := NIL;

    IF perfOn THEN PerfEnd(); END;

    InvokeMonitors(before := FALSE);

    VAR n_survivors := FLOAT(n_copied + n_promoted);
    BEGIN
      IF partialCollection THEN
        partialCollectionNext := n_survivors * threshold[1] < threshold[0];
      ELSE
        threshold[0] := n_survivors * (gcRatio + 1.0);
        threshold[1] := gcRatio;
        partialCollectionNext := TRUE;
      END;
    END;

    collectorState := CollectorState.Zero;
  END CollectSomeInStateFive;

(* CopySome attempts to make progress toward cleaning the new space.  It
   returns FALSE iff there was no more work to do.

   It operates by cleaning the current copy page.  It may also clean some
   number of pages on the stack.  When it returns, there is a new copy
   page. *)

(* NOTE: Any copying or cleaning may consume free pages which may trigger
   a heap expansion.  Therefore, pointers to the page descriptors
   (ie. "WITH pd = desc[p - p0]") MUST NOT be saved across "CopySome",
   "CleanPage", or "CleanBetween" calls. *)

VAR impureCopyStack: RefPage := NIL;

PROCEDURE PushPage (page: RefPage) =
  BEGIN
    page.desc.link := RefToPage(impureCopyStack);
    impureCopyStack := page;
  END PushPage;

PROCEDURE PopPage (): RefPage =
  VAR page := impureCopyStack;
  BEGIN
    IF page # NIL THEN
      impureCopyStack := PageToRef(page.desc.link);
    END;
    RETURN page;
  END PopPage;

PROCEDURE CopySome (): BOOLEAN =
  VAR
    originalPage  := impureCopy.page;
    originalLimit := impureCopy.limit;
    cleanTo       := originalPage + ADRSIZE(PageHdr);
  BEGIN
    LOOP
      IF cleanTo < impureCopy.next THEN
        VAR ptr := impureCopy.next;
        BEGIN
          CleanBetween(cleanTo, ptr, originalPage.desc.clean);
          cleanTo := ptr;
        END;
      ELSE
        WITH page = PopPage() DO
          IF page = NIL THEN RETURN FALSE END;
          IF page.desc.gray THEN CleanPage(page) END;
        END;
      END;
      IF impureCopy.page # originalPage THEN EXIT; END;
    END;

    IF originalPage # NIL THEN
      (* originalPage is now in the stack; mark it not gray *)
      CleanBetween(cleanTo, originalLimit, originalPage.desc.clean);
      CleanDesc(originalPage.desc);
      IF perfOn THEN PerfChange(originalPage); END;
    END;

    RETURN TRUE;
  END CopySome;

PROCEDURE CleanPage (page: RefPage) =
  BEGIN
    <*ASSERT NOT page.desc.pure*>
    CleanBetween(page + ADRSIZE(PageHdr), page + BytesPerPage, page.desc.clean);
    CleanDesc(page.desc);
    IF perfOn THEN PerfChange(page); END;
  END CleanPage;

PROCEDURE CleanDesc (VAR d: Desc) =
  BEGIN
    d.gray := FALSE;
    IF d.clean THEN
      IF d.generation = Generation.Older THEN
        <*ASSERT d.note # Note.AmbiguousRoot*>
        <*ASSERT d.space = Space.Current*>
      ELSE
        d.clean := FALSE;
      END;
    END;
  END CleanDesc;

PROCEDURE CleanBetween (h, he: RefHeader; clean: BOOLEAN) =
  BEGIN
    <* ASSERT h < he *>
    <* ASSERT Word.And (LOOPHOLE (h, INTEGER), 3) = 0 *>
    REPEAT
      VAR hdr := h^;
      BEGIN
        IF hdr # FillHeader1 AND hdr # FillHeaderN THEN
          <* ASSERT NOT hdr.forwarded *>
          IF hdr.gray THEN
            <*ASSERT NOT hdr.dirty*>
            hdr.marka := FALSE;
            hdr.markb := FALSE;
            h^ := hdr;                   (* FIXME: perhaps omit store here?? *)
            RTHeapMap.WalkRef (h, mover);
            hdr.gray := FALSE;
          END;
          hdr.dirty := NOT clean;
          h^ := hdr;
        END;
      END;
      INC(h, ADRSIZE(Header) + ReferentSize(h));
    UNTIL h >= he;
  END CleanBetween;

(* We maintain a list in weakTable, starting at weakLive0, of weak refs and
   the objects they reference.  This table is not considered a root.  When
   HandleWeakRefs is entered, any object mentioned in that list is a
   candidate for cleanup.

   First, we determine which weakly-referenced objects with non-NIL
   cleanups ("WRNNC objects") are reachable from other WRNNC objects, by
   walking the old space.  All such WRNNC objects are copied to new space,
   and all the objects they reference.

   All the weakly-referenced objects left in the old space can then be
   scheduled for cleanup; we move them from the list starting at weakLive0
   to the list starting at weakDead0 in weakTable.  A separate thread runs
   WeakCleaner, which does the calls to the procedures.

   Note that the refs in weakTable must be updated to point to new
   space. *)

(* PreHandleWeakRefs walks the weakly-references structures in old-space,
   deciding on a cleanup order. *)

PROCEDURE PreHandleWeakRefs () =
  VAR s: Stacker;
  BEGIN
    (* get ready to allocate on a new page (take this out!) *)
    pureCopy.next  := NIL; impureCopy.next  := NIL;
    pureCopy.limit := NIL; impureCopy.limit := NIL;

    (* allocate a stack on the side for walking the old space *)
    s := InitStack();
    (* iterate over the weak refs to walk the old space *)
    VAR i := weakLive0;
    BEGIN
      WHILE i # -1 DO
        (* here, all old-space WRNNC objects that have already been scanned
           have marka set, as do all old-space objects reachable from them;
           all old-space WRNNC objects that were reachable from other
           already-scanned WRNNC objects have been promoted to the new
           space. *)
        WITH entry = weakTable[i] DO
          IF entry.p # NIL AND NOT Moved(entry.r) THEN
            (* we haven't seen this WRNNC object before *)
            VAR header := HeaderOf(LOOPHOLE(entry.r, ADDRESS));
            BEGIN
              IF NOT header.marka THEN
                <* ASSERT NOT header.markb *>
                (* visit all old-space objects reachable from it; promote
                   all other old-space WRNNC objects reachable from it;
                   promote all old-space objects reachable from it that
                   have "marka" set.  mark all visited nodes with
                   "markb". *)
                WeakWalk1(s, entry.r);
                <* ASSERT NOT header.marka *>
                <* ASSERT header.markb *>
                (* then change all "markb" to "marka" *)
                WeakWalk2(s, entry.r);
                <* ASSERT header.marka *>
                <* ASSERT NOT header.markb *>
              END;
            END;
          END;
          i := entry.next;
        END;
      END;
    END;
  END PreHandleWeakRefs;

(* WeakWalk1 starts at a WRNNC object and visits all objects in old space
   reachable from it, using "markb" to keep from visiting them more than
   once.  All other WRNNC objects visited are promoted, as are all objects
   already visited from other WRNNC objects. *)

PROCEDURE WeakWalk1 (s: Stacker; ref: RefReferent) =
  VAR ref0 := ref;
  BEGIN
    <* ASSERT s.empty() *>
    LOOP
      IF NOT Moved(ref) THEN
        VAR header := HeaderOf(ref);
        BEGIN
          IF header.marka THEN
            <* ASSERT NOT header.markb *>
            Move(NIL, ADR(ref));
          ELSIF NOT header.markb THEN
            IF header.weak AND ref # ref0 THEN
              Move(NIL, ADR(ref));
            ELSE
              header.markb := TRUE;
              RTHeapMap.WalkRef (header, s);
            END;
          END;
        END;
      END;
      IF s.empty() THEN EXIT; END;
      ref := s.pop();
    END;
  END WeakWalk1;

(* WeakWalk2 starts at a WRNNC objects and visits all objects in the old
   space that are reachable from it, changing "markb" to "marka" *)

PROCEDURE WeakWalk2 (s: Stacker;  ref: RefReferent) =
  BEGIN
    <* ASSERT s.empty() *>
    LOOP
      IF NOT Moved(ref) THEN
        VAR header := HeaderOf(ref);
        BEGIN
          IF header.markb THEN
            header.markb := FALSE;
            header.marka := TRUE;
            RTHeapMap.WalkRef (header, s);
          END;
        END;
      END;
      IF s.empty() THEN EXIT; END;
      ref := s.pop();
    END;
  END WeakWalk2;

PROCEDURE PostHandleWeakRefs () =
  BEGIN
    (* move to a new page (take this out!) *)
    pureCopy.next  := NIL; impureCopy.next  := NIL;
    pureCopy.limit := NIL; impureCopy.limit := NIL;

    (* iterate over all weak refs.  if the object hasn't been promoted,
       schedule a cleanup *)
    VAR
      i        := weakLive0;
      previous := -1;
    BEGIN
      WHILE i # -1 DO
        WITH entry = weakTable[i] DO
          IF Moved(entry.r) THEN
            (* no cleanup this time; note new address *)
            Move(NIL, ADR(entry.r));
            previous := i;
            i := entry.next;
          ELSE
            (* the weak ref is dead; there are no cleanups *)
            VAR header := HeaderOf(LOOPHOLE(entry.r, ADDRESS));
            BEGIN
              header.weak := FALSE;
            END;
            (* move the entry from the weakLive0 list into the weakDead0 or
               weakFree0 list *)
            VAR next := entry.next;
            BEGIN
              IF previous = -1 THEN
                weakLive0 := next;
              ELSE
                weakTable[previous].next := next;
              END;
              entry.t.a := -1;   (* keep ToRef from succeeding *)
              IF entry.p # NIL THEN
                entry.next := weakDead0;
                weakDead0 := i;
              ELSE
                entry.next := weakFree0;
                weakFree0 := i;
              END;
              i := next;
            END;
          END;
        END;
      END;
    END;
    (* for all entries on the weakDead0 list, including those just placed
       there, note the new address *)
    VAR i := weakDead0;
    BEGIN
      WHILE i # -1 DO
        WITH entry = weakTable[i] DO
          <* ASSERT entry.t.a = -1 *>
          Move(NIL, ADR(entry.r));
          i := entry.next;
        END;
      END;
    END;
    (* finally, check for objects with final cleanup enabled *)
    VAR
      i        := weakFinal0;
      previous := -1;
    BEGIN
      WHILE i # -1 DO
        WITH entry = weakTable[i] DO
          IF Moved(entry.r) THEN
            (* no cleanup this time; note new address *)
            Move(NIL, ADR(entry.r));
            previous := i;
            i := entry.next;
          ELSE
            (* call the cleanup procedure *)
            LOOPHOLE(entry.p, PROCEDURE (p: REFANY))(
              LOOPHOLE(entry.r, REFANY));
            (* take the entry off the weakFinal0 list and put it on the
               weakFree0 list; on to the next entry *)
            VAR next := entry.next;
            BEGIN
              IF previous = -1 THEN
                weakFinal0 := next;
              ELSE
                weakTable[previous].next := next;
              END;
              entry.next := weakFree0;
              weakFree0 := i;
              i := next;
            END;
          END;
        END;
      END;
    END;
  END PostHandleWeakRefs;

(* The stack for walking the old space is maintained on the heap in the new
   space. *)

TYPE
  Stacker = RTHeapMap.Visitor OBJECT
    data : REF ARRAY OF RefReferent;
    x0   : UNTRACED REF RefReferent;
    x1   : UNTRACED REF RefReferent;
    xA   : UNTRACED REF RefReferent;
    xN   : CARDINAL;
  METHODS
    empty (): BOOLEAN     := StackEmpty;
    pop   (): RefReferent := PopStack;
  OVERRIDES
    apply := PushStack;
  END;

(* InitStack allocates an initial stack of 100 elements. *)

PROCEDURE InitStack (): Stacker =
  VAR s := NEW (Stacker);
  BEGIN
    s.data := NEW(REF ARRAY OF RefReferent, 100);
    s.xN   := NUMBER (s.data^);
    s.x0   := ADR(s.data[0]);
    s.x1   := s.x0 + s.xN * ADRSIZE(RefReferent);
    s.xA   := s.x0;
    RETURN s;
  END InitStack;

(* PushStack pushes an object onto the stack, growing it if necessary. *)

PROCEDURE PushStack (s: Stacker;  cp: ADDRESS) =
  VAR ref: RefReferent := LOOPHOLE(cp, UNTRACED REF RefReferent)^;
  BEGIN
    IF ref # NIL THEN
      IF s.xA = s.x1 THEN ExpandStack (s); END;
      s.xA^ := ref;
      INC(s.xA, ADRSIZE(RefReferent));
    END;
  END PushStack;

PROCEDURE ExpandStack (s: Stacker) =
  VAR
    newStackN := 2 * s.xN;
    newStack: REF ARRAY OF RefReferent := NEW(REF ARRAY OF RefReferent,
                                                  newStackN);
  BEGIN
    SUBARRAY(newStack^, 0, s.xN) := SUBARRAY(s.data^, 0, s.xN);
    s.x0   := ADR(newStack^[0]);
    s.xA   := s.x0 + s.xN * ADRSIZE(RefReferent);
    s.x1   := s.x0 + newStackN * ADRSIZE(RefReferent);
    s.data := newStack;
    s.xN   := newStackN;
  END ExpandStack;

(* PopStack pops an object off the stack. *)

PROCEDURE PopStack (s: Stacker): RefReferent =
  BEGIN
    DEC(s.xA, ADRSIZE(RefReferent));
    RETURN s.xA^;
  END PopStack;

(* StackEmpty tells if the stack is empty. *)

PROCEDURE StackEmpty (s: Stacker): BOOLEAN =
  BEGIN
    RETURN s.xA = s.x0;
  END StackEmpty;

PROCEDURE AllocCopy (dataSize, dataAlignment: CARDINAL;
                     VAR pool: AllocPool): RefReferent =
  (* Allocates space from "pool" in the traced heap. *)
  (* LL >= RTOS.LockHeap *)
  VAR
    res       := pool.next + ADRSIZE(Header);
    cur_align := Word.And(LOOPHOLE(res, INTEGER), MaxAlignMask);
    alignment := align[cur_align, dataAlignment];
    nextPtr   := res + (alignment + dataSize);
  BEGIN
    IF nextPtr > pool.limit THEN
      (* not enough space left in the pool, take the long route *)
      res := NIL;  nextPtr := NIL;  (* in case of GC... *)
      RETURN LongAlloc (dataSize, dataAlignment, pool);
    END;

    (* Align the referent *)
    IF alignment # 0 THEN
      InsertFiller(pool.next, alignment);
      pool.next := pool.next + alignment;
      res := pool.next + ADRSIZE(Header);
    END;

    pool.next := nextPtr;
    RETURN res;
  END AllocCopy;

PROCEDURE LongAlloc (dataSize, dataAlignment: CARDINAL;
                     VAR pool: AllocPool): RefReferent =
  (* LL >= RTOS.LockHeap *)
  VAR
    n_bytes :=
        RTMisc.Upper(ADRSIZE(PageHdr) + ADRSIZE(Header), dataAlignment) +
        dataSize;
    n_pages := (n_bytes + BytesPerPage - 1) DIV BytesPerPage;
    res      : RefReferent;
    filePage : RefPage;
    newPage  : RefPage;
    newPtr   : ADDRESS;
    newLimit : ADDRESS;
    notAfter : SET OF Note;
  BEGIN
    CASE pool.note OF
    | Note.Allocated =>
      notAfter := SET OF Note{Note.Copied};
    | Note.Copied =>
      notAfter := SET OF Note{Note.Allocated};
    END;

    (* get a block of "n_pages" contiguous, free pages; just what we need! *)
    newPage  := FindFreePages (n_pages, notAfter);
    newPtr   := newPage + ADRSIZE(PageHdr);
    newLimit := newPage + AdrPerPage;
    IF (newPage = NIL) THEN RETURN NIL; END;

    <*ASSERT newPage.nb = n_pages*>
    RTMisc.Zero(newPage, n_pages * BytesPerPage);

    (* mark the new pages *)
    CASE pool.note OF
    | Note.Allocated =>
      newPage.desc := Desc{space := Space.Current,
                           generation := Generation.Younger,
                           pure := pool.pure,
                           note := pool.note,
                           gray := FALSE,
                           clean := FALSE,
                           locked := FALSE};
      INC(n_new, n_pages);
    | Note.Copied =>
      newPage.desc := Desc{space := Space.Current,
                           generation := promoteGeneration,
                           pure := pool.pure,
                           note := pool.note,
                           gray := NOT pool.pure,
                           clean := NOT pool.pure,
                           locked := FALSE};
      INC(n_copied, n_pages);
    END;
    newPage.nb := n_pages;
    IF perfOn THEN PerfChange (newPage); END;

    (* maybe we have to insert a filler to align this thing *)
    res := RTMisc.Align(newPtr + ADRSIZE(Header), dataAlignment);
    InsertFiller(newPtr, res - ADRSIZE(Header) - newPtr);

    (* allocate the object from the new page *)
    newPtr := LOOPHOLE(res + dataSize, RefHeader);

    (* decide whether to use the new page or the current pool page
       for further allocations *)
    IF n_pages # 1 THEN
      (* file this page *)
      filePage := newPage;
    ELSIF newLimit - newPtr > pool.limit - pool.next THEN
      (* more space remains on the new page *)
      filePage := pool.page;
      pool.next  := newPtr;
      pool.limit := newLimit;
      pool.page  := newPage;
    ELSE (* more space remains on the existing pool page *)
      filePage := newPage;
    END;

    (* file the page *)
    IF filePage # NIL AND NOT pool.pure AND pool.note = Note.Copied THEN
      PushPage(filePage);
    END;

    RETURN res;
  END LongAlloc;

(*--------------------------------------------------*)

VAR
  backgroundWaiting   := FALSE;

(* The background thread may be present or not.  If it is present, it
   speeds collection asynchronously.  Because it makes progress slowly, it
   should impose only a small overhead when the mutator is running, but
   quickly complete a collection if the collector pauses. *)

PROCEDURE BackgroundThread (<* UNUSED *> closure: Thread.Closure): REFANY =
  VAR t0, t1: Time.T;
  BEGIN
    LOOP
      TRY
        RTOS.LockHeap();
        WHILE collectorState = CollectorState.Zero DO
          backgroundWaiting := TRUE;
          RTOS.WaitHeap();
          backgroundWaiting := FALSE;
        END;
        CollectorOn();
        t0 := Time.Now();
        CollectSome();
      FINALLY
        t1 := Time.Now();
        cycleCost := cycleCost + (t1 - t0);
        CollectorOff();
        RTOS.UnlockHeap();
      END;
      Thread.Pause(1.0d0);
    END;
  END BackgroundThread;

VAR foregroundWaiting := FALSE;

(* The foreground thread may be present or not.  If it is present, it
   collects asynchronously. *)

PROCEDURE ForegroundThread (<* UNUSED *> closure: Thread.Closure): REFANY =
  BEGIN
    TRY
      RTOS.LockHeap();
      LOOP
        foregroundWaiting := TRUE;
        RTOS.WaitHeap();
        foregroundWaiting := FALSE;
        CollectEnough();
      END;
    FINALLY
      RTOS.UnlockHeap();
    END;
  END ForegroundThread;

(* --------------------------------------------------------- collector *)

PROCEDURE StartGC () =
  VAR t0, t1: Time.T;
  BEGIN
    TRY
      CollectorOn();
      t0 := Time.Now();
      IF collectorState = CollectorState.Zero
        AND disableCount + disableMotionCount = 0 THEN
        partialCollectionNext := FALSE;
        REPEAT CollectSome(); UNTIL collectorState # CollectorState.Zero;
        IF NOT (incremental AND RTLinker.incremental) THEN
          REPEAT CollectSome(); UNTIL collectorState = CollectorState.Zero;
        END;
      END;
    FINALLY
      t1 := Time.Now();
      cycleCost := cycleCost + (t1 - t0);
      CollectorOff();
    END;
  END StartGC;

PROCEDURE FinishGC () =
  VAR t0, t1: Time.T;
  BEGIN
    TRY
      CollectorOn();
      t0 := Time.Now();
      WHILE collectorState # CollectorState.Zero DO CollectSome(); END;
    FINALLY
      t1 := Time.Now();
      cycleCost := cycleCost + (t1 - t0);
      CollectorOff();
    END;
  END FinishGC;

PROCEDURE Crash (): BOOLEAN =
  BEGIN
    RTOS.LockHeap();        (* left incremented *)

    IF collectorState = CollectorState.Zero THEN
      (* no collection in progress *)
      collectorOn := TRUE;       (* left on *)
      RETURN TRUE;
    ELSIF NOT collectorOn THEN
      collectorOn := TRUE;             (* left on *)
      (* finish collection *)
      WHILE collectorState # CollectorState.Zero DO CollectSome(); END;
      RETURN TRUE;
    ELSE
      collectorOn := TRUE;       (* left on *)
      RETURN FALSE;
    END;
  END Crash;

(* --------------------------------------------------------- debugging *)

VAR
  cleanCheck, refCheck: RTHeapMap.Visitor;

PROCEDURE InstallSanityCheck () =
  BEGIN
    RegisterMonitor(
      NEW(MonitorClosure, before := Before, after := After));
    IF (refCheck = NIL) THEN
      cleanCheck := NEW (RTHeapMap.Visitor, apply := CleanOlderRefSanityCheck);
      refCheck := NEW (RTHeapMap.Visitor, apply := RefSanityCheck);
    END;
  END InstallSanityCheck;

(* SanityCheck checks the heap for correctness when no collection is in
   progress. *)

CONST Before = SanityCheck; (* already suspended *)

PROCEDURE After (self: MonitorClosure) =
  BEGIN
    RTThread.SuspendOthers();
    SanityCheck (self);
    RTThread.ResumeOthers();
  END After;

PROCEDURE SanityCheck (<*UNUSED*> self: MonitorClosure) =
  VAR p := p0;
  BEGIN
    WHILE p < p1 DO
      IF desc[p - p0] < 0 THEN INC(p);
      ELSE
        WITH page = PageToRef(p), d = page.desc DO
          CASE d.space OF
          | Space.Unallocated, Space.Previous => <*ASSERT FALSE*>
          | Space.Current =>
            <*ASSERT NOT d.gray*>
            IF d.clean THEN
              <*ASSERT d.generation = Generation.Older*>
            END;
            (* visit the objects on the page *)
            VAR
              h : RefHeader := page + ADRSIZE(PageHdr);
              he: RefHeader := page + BytesPerPage;
            BEGIN
              WHILE h < he DO
                (* check the references in the object *)
                <* ASSERT NOT h.gray *>
                IF d.clean THEN
                  <*ASSERT NOT h.dirty*>
                  RTHeapMap.WalkRef (h, cleanCheck);
                ELSE
                  RTHeapMap.WalkRef (h, refCheck);
                END;
                INC(h, ADRSIZE(Header) + ReferentSize(h));
              END;
              IF h > he THEN
                <* ASSERT page.nb > 1 *>
              ELSE
                <* ASSERT page.nb = 1 *>
              END;
            END;
            VAR pp := p + 1;
            BEGIN
              LOOP
                IF FirstPage(pp) # p THEN EXIT END;
                INC(pp);
              END;
              <*ASSERT page.nb = pp - p*>
            END;
            INC(p, page.nb);
          | Space.Free =>
            VAR pp := p + 1;
            BEGIN
              LOOP
                IF FirstPage(pp) # p THEN EXIT END;
                INC(pp);
              END;
              <*ASSERT page.nb = pp - p*>
            END;
            INC(p, page.nb);
          END;
        END;
      END;
    END;
    <* ASSERT p = p1 *>
  END SanityCheck;

PROCEDURE RefSanityCheck (<*UNUSED*>v: RTHeapMap.Visitor;  cp  : ADDRESS) =
  VAR ref := LOOPHOLE(cp, UNTRACED REF RefReferent)^;
  BEGIN
    IF ref # NIL THEN
      VAR
        h  := HeaderOf(ref);
        tc := h.typecode;
      BEGIN
        (* the compiler generates Text.T that are not in the traced heap *)
        IF tc # RT0.TextLitTypecode THEN
          WITH p = ReferentToPage(ref), d = PageToRef(p).desc DO
            <*ASSERT d.space = Space.Current*>
          END;
          <* ASSERT (0 <= tc AND tc <= RTType.MaxTypecode())
                      OR tc = Fill_1_type
                      OR tc = Fill_N_type *>
        END;
      END;
    END;
  END RefSanityCheck;

PROCEDURE CleanOlderRefSanityCheck (<*UNUSED*> v: RTHeapMap.Visitor;
                                    cp: ADDRESS) =
  VAR ref := LOOPHOLE(cp, UNTRACED REF RefReferent)^;
  BEGIN
    IF ref # NIL THEN
      VAR
        h  := HeaderOf(ref);
        tc := h.typecode;
      BEGIN
        (* the compiler generates Text.T that are not in the traced heap *)
        IF tc # RT0.TextLitTypecode THEN
          WITH p = ReferentToPage(ref), d = PageToRef(p).desc DO
            <* ASSERT d.space = Space.Current *>
            <* ASSERT d.generation = Generation.Older *>
          END;
          <* ASSERT (0 <= tc AND tc <= RTType.MaxTypecode())
                      OR tc = Fill_1_type
                      OR tc = Fill_N_type *>
        END;
      END;
    END;
  END CleanOlderRefSanityCheck;

<*UNUSED*>
PROCEDURE P(p: Page; b: BOOLEAN): BOOLEAN =
  BEGIN
    IF NOT b THEN PrintDesc(p) END;
    RETURN b;
  END P;

PROCEDURE PrintDesc(p: Page) =
  VAR
    page := PageToRef(p);
    nb := page.nb;
    d := page.desc;
  BEGIN
    RTIO.PutText("p0="); RTIO.PutInt(p0);
    RTIO.PutText(" p="); RTIO.PutInt(p);
    RTIO.PutText(" nb="); RTIO.PutInt(nb);
    RTIO.PutText(" p1="); RTIO.PutInt(p1);
    RTIO.PutChar('\n');

    RTIO.PutText("addr="); RTIO.PutAddr(page); RTIO.PutChar('\n');
    RTIO.PutText("space=");
    CASE d.space OF
    | Space.Unallocated => RTIO.PutText("Free");
    | Space.Free        => RTIO.PutText("Free");
    | Space.Previous    => RTIO.PutText("Previous");
    | Space.Current     => RTIO.PutText("Current");
    END;
    RTIO.PutChar('\n');

    RTIO.PutText("generation=");
    CASE d.generation OF
    | Generation.Older   => RTIO.PutText("Older");
    | Generation.Younger => RTIO.PutText("Younger");
    END;
    RTIO.PutChar('\n');

    RTIO.PutText("pure="); RTIO.PutInt(ORD(d.pure)); RTIO.PutChar('\n');

    RTIO.PutText("note=");
    CASE d.note OF
    | Note.OlderGeneration => RTIO.PutText("OlderGeneration");
    | Note.AmbiguousRoot   => RTIO.PutText("AmbiguousRoot");
    | Note.Large           => RTIO.PutText("Large");
    | Note.Frozen          => RTIO.PutText("Frozen");
    | Note.Allocated       => RTIO.PutText("Allocated");
    | Note.Copied          => RTIO.PutText("Copied");
    END;
    RTIO.PutChar('\n');

    RTIO.PutText("gray="); RTIO.PutInt(ORD(d.gray)); RTIO.PutChar('\n');
    RTIO.PutText("clean="); RTIO.PutInt(ORD(d.clean)); RTIO.PutChar('\n');
    RTIO.PutChar('\n'); RTIO.Flush();
  END PrintDesc;

(* ----------------------------------------------------------------------- *)

PROCEDURE VisitAllRefs (v: RefVisitor) =
  VAR
    tc: Typecode;
    h, he: RefHeader;
    size: INTEGER;
  BEGIN
    TRY
      Disable();
      RTThread.SuspendOthers();
      FOR p := p0 TO p1 - 1 DO
        IF desc[p - p0] = 0 THEN
          WITH page = PageToRef(p), d = page.desc DO
            IF d.space = Space.Current THEN
              h := page + ADRSIZE(PageHdr);
              he := page + BytesPerPage;
              WHILE h < he DO
                size := ReferentSize(h);
                tc := h.typecode;
                IF tc # Fill_1_type AND tc # Fill_N_type THEN
                  IF NOT v.visit(tc, LOOPHOLE(h + ADRSIZE(Header), REFANY),
                                 size) THEN
                    RETURN;
                  END;
                END;
                INC(h, ADRSIZE(Header) + size);
              END;
            END;
          END;
        END;
      END;
    FINALLY
      RTThread.ResumeOthers();
      Enable();
    END;
  END VisitAllRefs;

TYPE
  CountClosure = MonitorClosure OBJECT
                   tcs    : REF ARRAY OF Typecode;
                   counts : REF ARRAY OF CARDINAL;
                   visitor: RefVisitor;
                 OVERRIDES
                   after := CountRefsForTypecodes;
                 END;

TYPE
  CountAllClosure = MonitorClosure OBJECT
                      counts : REF ARRAY OF CARDINAL;
                      visitor: RefVisitor;
                    OVERRIDES
                      after := CountRefsForAllTypecodes;
                    END;

TYPE
  CountVisitor =
    RefVisitor OBJECT cl: CountClosure OVERRIDES visit := One; END;

  CountAllVisitor =
    RefVisitor OBJECT cl: CountAllClosure OVERRIDES visit := All; END;

PROCEDURE One (           self: CountVisitor;
                          tc  : Typecode;
               <*UNUSED*> r   : REFANY;
               <*UNUSED*> size: CARDINAL      ): BOOLEAN =
  BEGIN
    FOR i := FIRST(self.cl.tcs^) TO LAST(self.cl.tcs^) DO
      IF self.cl.tcs[i] = tc THEN INC(self.cl.counts[i]); RETURN TRUE; END;
    END;
    RETURN TRUE;
  END One;

PROCEDURE All (           self: CountAllVisitor;
                          tc  : Typecode;
               <*UNUSED*> r   : REFANY;
               <*UNUSED*> size: CARDINAL         ): BOOLEAN =
  BEGIN
    INC(self.cl.counts[tc]);
    RETURN TRUE;
  END All;

PROCEDURE CountRefsForTypecodes (cl: CountClosure) =
  BEGIN
    FOR i := FIRST(cl.counts^) TO LAST(cl.counts^) DO
      cl.counts[i] := 0;
    END;
    VisitAllRefs(cl.visitor);
    FOR i := FIRST(cl.tcs^) TO LAST(cl.tcs^) DO
      RTIO.PutText("count[");
      RTIO.PutInt(cl.tcs[i]);
      RTIO.PutText("] = ");
      RTIO.PutInt(cl.counts[i]);
      IF i # LAST(cl.tcs^) THEN RTIO.PutText(",  "); END;
    END;
    RTIO.PutText("\n");
    RTIO.Flush();
  END CountRefsForTypecodes;

PROCEDURE CountRefsForAllTypecodes (cl: CountAllClosure) =
  BEGIN
    FOR i := FIRST(cl.counts^) TO LAST(cl.counts^) DO
      cl.counts[i] := 0;
    END;
    VisitAllRefs(cl.visitor);
    FOR i := FIRST(cl.counts^) TO LAST(cl.counts^) DO
      IF cl.counts[i] > 1 THEN
        RTIO.PutInt(i);
        RTIO.PutText(": ");
        RTIO.PutInt(cl.counts[i]);
        IF i # LAST(cl.counts^) THEN RTIO.PutText(", "); END;
      END;
    END;
    RTIO.PutText("\n");
    RTIO.Flush();
  END CountRefsForAllTypecodes;

(* ---------------------------------------------------- showheap hooks *)

VAR
  perfW  : RTPerfTool.Handle;
  perfOn : BOOLEAN := FALSE;

CONST
  EventSize = (BITSIZE(RTHeapEvent.T) + BITSIZE(CHAR) - 1) DIV BITSIZE(CHAR);

PROCEDURE PerfStart () =
  BEGIN
    IF RTPerfTool.Start("showheap", perfW) THEN
      perfOn := TRUE;
      RTProcess.RegisterExitor(PerfStop);
      IF p1 > p0 THEN PerfGrow(p0, p1 - p0) END;

      FOR p := p0 TO p1 - 1 DO
        IF desc[p - p0] = 0 THEN
          WITH page = PageToRef(p) DO PerfChange(page) END;
        END;
      END;
    END;
  END PerfStart;

PROCEDURE PerfFlip () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Flip};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfFlip;

PROCEDURE PerfPromotedRoots () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Roots};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfPromotedRoots;

PROCEDURE PerfStop () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Bye};
  BEGIN
    (* UNSAFE, but needed to prevent deadlock if we're crashing! *)
    EVAL RTPerfTool.Send (perfW, ADR(e), EventSize);
    RTPerfTool.Close (perfW);
  END PerfStop;

PROCEDURE PerfAllow (<*UNUSED*> n: INTEGER := 0) =
  VAR
    e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Off, nb :=
                       disableCount + disableMotionCount};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfAllow;

PROCEDURE PerfBegin () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Begin};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfBegin;

PROCEDURE PerfEnd () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.End};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfEnd;

PROCEDURE PerfChange (page: RefPage) =
  VAR
    e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Change,
                       first := RefToPage(page),
                       nb := page.nb, desc := page.desc};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfChange;

PROCEDURE PerfGrow (firstNew: Page; nb: CARDINAL) =
  VAR
    e := RTHeapEvent.T{
           kind := RTHeapEvent.Kind.Grow, first := firstNew, nb := nb};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfGrow;

(* ----------------------------------------------------------------------- *)

(* RTWeakRef *)

(* weakTable contains four singly-linked lists: for entries in use (rooted
   at index weakLive0), entries with final cleanup (at weakFinal0), dead
   entries awaiting cleanup (at weakDead0), and free entries (at
   weakFree0).

   Entries in use contain the weak ref, the REF, and the procedure.  The
   "a" field of the weak ref is the index in the table; this speeds lookup.
   The "b" field is a unique value, taken from a 32-bit counter.

   Dead entries contain the same fields, but the "a" field of the weak ref
   is set to -1 to keep lookups from succeeding.  When the cleanup
   procedure is to be called, the original weak ref can still be
   reconstructed, since the "a" field was the index. *)

VAR
  weakTable: UNTRACED REF ARRAY OF WeakEntry; (* allocated in "Init" *)
             (* := NEW(UNTRACED REF ARRAY OF WeakEntry, 0); *)
  weakLive0  := -1;              (* the root of the in-use list *)
  weakFinal0 := -1;              (* the root of the thread-cleanup list *)
  weakDead0  := -1;              (* the root of the dead list *)
  weakFree0  := -1;              (* the root of the free list *)

TYPE
  Int32 = BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff];
  WeakRefAB = RECORD
                a: Int32;
                b: Int32;
              END;
  WeakEntry = RECORD
                t: WeakRefAB;    (* the weak ref, if well-formed *)
                r: RefReferent;  (* the traced reference *)
                p: ADDRESS;      (* a WeakRefCleanUpProc or a PROCEDURE(r:
                                    REFANY) *)
                next: INTEGER;   (* the next entry on the list *)
              END;

(* This is WeakRef.FromRef, which returns a new weak ref for an object. *)

VAR startedWeakCleaner := FALSE;

PROCEDURE WeakRefFromRef (r: REFANY; p: WeakRefCleanUpProc := NIL): WeakRef =
  VAR
    start := FALSE;
    result: WeakRef;
  BEGIN
    <* ASSERT r # NIL *>
    TRY
      RTOS.LockHeap();
      (* create a WeakCleaner thread the first time through *)
      IF p # NIL AND NOT startedWeakCleaner THEN
        start := TRUE;
        startedWeakCleaner := TRUE;
      END;
      (* if necessary, expand weakTable *)
      IF weakFree0 = -1 THEN ExpandWeakTable(); END;
      IF p # NIL THEN
        (* mark the object as having a weak ref with non-nil cleanup *)
        VAR header := HeaderOf(LOOPHOLE(r, ADDRESS));
        BEGIN
          <* ASSERT NOT header^.weak *>
          header^.weak := TRUE;
        END;
      END;
      (* allocate a new entry *)
      VAR i := weakFree0;
      BEGIN
        weakFree0 := weakTable[i].next;
        (* generate a new weak ref *)
        VAR t := WeakRefAB{a := i, b := Word.Plus(weakTable[i].t.b, 1)};
        BEGIN
          <* ASSERT t.b # 0 *>
          (* set up the entry *)
          weakTable[i] :=
            WeakEntry{t := t, r := LOOPHOLE(r, RefReferent), p :=
                      LOOPHOLE(p, ADDRESS), next := weakLive0};
          weakLive0 := i;
          result := LOOPHOLE(t, WeakRef);
        END;
      END;
    FINALLY
      RTOS.UnlockHeap();
    END;
    IF start THEN
      EVAL Thread.Fork(NEW(Thread.Closure, apply := WeakCleaner));
    END;
    RETURN result;
  END WeakRefFromRef;

PROCEDURE ExpandWeakTable () =
  VAR
    newTable := NEW(UNTRACED REF ARRAY OF WeakEntry,
                    2 * NUMBER(weakTable^) + 1);
  BEGIN
    SUBARRAY(newTable^, 0, NUMBER(weakTable^)) := weakTable^;
    FOR i := NUMBER(weakTable^) TO NUMBER(newTable^) - 1 DO
      WITH entry = newTable[i] DO
        entry.t.b := 0;
        entry.next := weakFree0;
        weakFree0 := i;
      END;
    END;
    DISPOSE(weakTable);
    weakTable := newTable;
  END ExpandWeakTable;

(* This is WeakRef.ToRef, which inverts FromRef *)

PROCEDURE WeakRefToRef (READONLY t: WeakRef): REFANY =
  VAR ab: WeakRefAB;  r: REFANY := NIL;  t0, t1: Time.T;
  BEGIN
    LOOPHOLE (ab, WeakRef) := t;
    TRY
      RTOS.LockHeap();
      (* if the weak ref is not dead, we know the index *)
      WITH entry = weakTable[ab.a] DO
        (* check the weak ref there *)
        IF entry.t = ab THEN
          <* ASSERT entry.r # NIL *>
          IF collectorState # CollectorState.Zero THEN
            VAR p := ReferentToPage(entry.r);
            BEGIN
              <* ASSERT p # Nil *>
              IF PageToRef(p).desc.space = Space.Previous THEN
                TRY
                  CollectorOn();
                  t0 := Time.Now();
                  Move(NIL, ADR(entry.r));
                FINALLY
                  t1 := Time.Now();
                  cycleCost := cycleCost + (t1 - t0);
                  CollectorOff();
                END;
              END;
            END;
          END;
          r := LOOPHOLE(ADR(entry.r), UNTRACED REF REFANY)^;
        END;
      END;
    FINALLY
      RTOS.UnlockHeap();
    END;
    RETURN r;
  END WeakRefToRef;

(* This is RTHeapRep.RegisterFinalCleanup, which registers final cleanup
   for a heap object. *)

PROCEDURE RegisterFinalCleanup (r: REFANY; p: PROCEDURE (r: REFANY)) =
  BEGIN
    <* ASSERT r # NIL *>
    <* ASSERT p # NIL *>
    TRY
      RTOS.LockHeap();
      (* if necessary, expand weakTable *)
      IF weakFree0 = -1 THEN ExpandWeakTable(); END;
      (* allocate a new entry *)
      VAR i := weakFree0;
      BEGIN
        weakFree0 := weakTable[i].next;
        (* set up the entry, without a weak ref *)
        weakTable[i].r := LOOPHOLE(r, RefReferent);
        weakTable[i].p := LOOPHOLE(p, ADDRESS);
        weakTable[i].next := weakFinal0;
        weakFinal0 := i;
      END;
    FINALLY
      RTOS.UnlockHeap();
    END;
  END RegisterFinalCleanup;

(* WeakCleaner waits for entries to be placed on the dead list, then cleans
   them up and puts them on the free list. *)

PROCEDURE WeakCleaner (<*UNUSED*> closure: Thread.Closure): REFANY =
  VAR
    i   : INTEGER;
    copy: WeakEntry;
    t0, t1: Time.T;
  BEGIN
    LOOP
      TRY
        RTOS.LockHeap();
        (* get an entry to handle.  copy its contents, then put it on the
           free list. *)
        WHILE weakDead0 = -1 DO RTOS.WaitHeap() END;
        i := weakDead0;
        WITH entry = weakTable[i] DO
          <* ASSERT entry.t.a = -1 *>
          TRY
            CollectorOn();
            t0 := Time.Now();
            Move(NIL, ADR(entry.r));
          FINALLY
            t1 := Time.Now();
            cycleCost := cycleCost + (t1 - t0);
            CollectorOff();
          END;
          copy := entry;
          weakDead0 := entry.next;
          entry.next := weakFree0;
          weakFree0 := i;
        END;
      FINALLY
        RTOS.UnlockHeap();
      END;
      (* call the registered procedure.  note that collections are
         allowed; the copy is kept on the stack so the object won't be
         freed during the call. *)
      IF copy.p # NIL THEN
        LOOPHOLE(copy.p, WeakRefCleanUpProc)(
            LOOPHOLE(WeakRefAB{a := i, b := copy.t.b}, WeakRef),
            LOOPHOLE(ADR(copy.r), UNTRACED REF REFANY)^);
      END;
      copy.r := NIL;           (* to help conservative collector *)
    END;
  END WeakCleaner;

(*------------------------------------------------------ barrier support --*)

PROCEDURE CheckLoadTracedRef (ref: REFANY) =
  (* Load a heap reference 'ref' from a global or heap variable.
     The fast-path inline guard for this operation has already noticed that the
     target of the reference was gray.  We now scan the target object to make it
     black, so that it is guaranteed to contain no white references.
     This preserves the strong tricolor invariant (no pointers from black to
     white) where the mutator and the objects directly referred to from the
     mutator are black. *)
  VAR p := Word.RightShift (LOOPHOLE(ref, Word.T), LogBytesPerPage);
  BEGIN
    INC(checkLoadTracedRef);		 (* race, so only approximate *)
    WITH h = HeaderOf (LOOPHOLE(ref, RefReferent)), page = PageToRef(p) DO
      <*ASSERT h.typecode # RT0.TextLitTypecode*>
      TRY
        RTOS.LockHeap();
        CollectorOn();
        (* just clean this object *)
        CleanBetween (h, h + ADRSIZE(Header), page.desc.clean);
      FINALLY
        CollectorOff();
        RTOS.UnlockHeap();
      END;
    END;
  END CheckLoadTracedRef;

PROCEDURE CheckStoreTraced (dst: REFANY) =
  (* Store some reference into a target object 'dst'.
     The fast-path inline guard for this operation has already noticed that the
     target object was not dirty.  We now record that the target object and the
     page in which it resides is dirty. *)
  VAR p := Word.RightShift (LOOPHOLE(dst, Word.T), LogBytesPerPage);
  BEGIN
    INC(checkStoreTraced);		 (* race, so only approximate *)
    WITH h = HeaderOf (LOOPHOLE(dst, RefReferent)), page = PageToRef(p) DO
      TRY
        RTOS.LockHeap();
        <*ASSERT h.typecode # RT0.TextLitTypecode*>
        <*ASSERT NOT h.gray*>
        WITH d = page.desc DO
          IF h.dirty THEN
            <*ASSERT NOT d.clean*>
          ELSE
            h.dirty := TRUE;
            IF d.clean THEN
              d.clean := FALSE;
              IF perfOn THEN PerfChange(page); END;
            END;
          END;
        END;
      FINALLY
        RTOS.UnlockHeap();
      END;
    END;
    RETURN;
  END CheckStoreTraced;

(* ----------------------------------------------------------------------- *)

(* The inner-loop collector action is to pick a gray page and completely
   clean it (i.e., make its referents at least gray, so that the page
   becomes black).  The current gray page, "impureCopy.page" is
   distinguished; it's the page that newly gray objects are copied to.

   To improve locality of reference in the new space, we keep the set of
   gray pages as a stack.  This helps approximate a depth-first copy to
   newspace.  The current page is not a member of the stack, but will
   become one when it becomes full.  The current page is always the page
   that contains "pool.next".

   To reduce page faults, we separate the "pure" copy pages (those whose
   objects contain no REFs) from the "impure" ones (those with REFs).  Only
   impure pages become gray, since pure pages can have no REFs into the old
   space (since they have no REFs at all). *)

(* ----------------------------------------------------------------------- *)

(****** Page-level allocator ******)

(* The freelist is sorted by blocksize, linked through the first page in
   each block, using the "link" field in the "desc" array.  Page allocation
   is best-fit.  For elements of the same blocksize, they are sorted by
   page number, to make the showheap display more easily readable, and to
   slightly reduce fragmentation. *)

(* FindFreePages allocates a run of "n" free pages, which we would prefer
   not be near pages in the current space with notes in notAfter.  The
   allocator can thus be used to separate pages with different notes, since
   they will have different lifetimes.  This is a concern only when
   incremental and generational collection are combined. *)

PROCEDURE FindFreePages (n: INTEGER; notAfter: Notes): RefPage =
  VAR p: RefPage;
  BEGIN
    IF collectorState = CollectorState.Zero THEN
      p := AllocateFreePagesFromBlock(n, Notes{}, TRUE);
      IF p # NIL THEN RETURN p; END;
    ELSE
      p := AllocateFreePagesFromBlock(n, notAfter, TRUE);
      IF p # NIL THEN RETURN p; END;
      p := AllocateFreePagesFromBlock(n, Notes{}, FALSE);
      IF p # NIL THEN RETURN p; END;
    END;
    IF NOT GrowHeap(n) THEN RETURN NIL; END;
    p := AllocateFreePagesFromBlock(n, Notes{}, TRUE);
    RETURN p;
  END FindFreePages;

VAR free: Page;                  (* the head of the freelist *)

(* AllocateFreePagesFromBlock finds the first block large enough to satisfy
   the request.  "notAfter" is the set of page notes in the current space
   that the block allocated from must not immediately follow; this is used
   to separate Note.Allocated pages from Note.Copied pages.  If "front" is
   TRUE, the pages will be allocated from the beginning of the block, else
   from the end; this is also used to separate Note.Allocated Pages from
   Note.Copied pages.  If the block is bigger than the request, the
   remainder is left at the right point in the freelist.  If no block
   exists, Nil is returned. *)

PROCEDURE AllocateFreePagesFromBlock (n       : INTEGER;
                                      notAfter: Notes;
                                      front   : BOOLEAN      ): RefPage =
  VAR
    p                   := free;
    prevP: RefPage      := NIL;
    prevLength          := 0;
    page: RefPage;
    length: INTEGER;
  BEGIN
    LOOP
      IF p = Nil THEN RETURN NIL; END;
      page := PageToRef(p);
      length := page.nb;
      IF length >= n THEN
        WITH pp = FirstPage(p - 1) DO
          IF pp = Nil THEN EXIT END;
          WITH d = PageToRef(pp).desc DO
            IF NOT d.space = Space.Current THEN EXIT END;
            IF NOT d.note IN notAfter THEN EXIT END;
          END;
        END;
      END;
      prevP := page;
      prevLength := length;
      p := page.desc.link;
    END;
    IF length = n THEN
      IF prevP = NIL THEN
        free := page.desc.link;
      ELSE
        prevP.desc.link := page.desc.link;
      END;
      RETURN page;
    ELSE
      VAR
        newP, fragP: Page;
        fragLength := length - n;
      BEGIN
        IF front THEN
          newP := p;
          fragP := p + n;
          WITH page = PageToRef(newP) DO
            <*ASSERT page.desc.space = Space.Free*>
            page.nb := n;
          END;
          FOR i := 1 TO n - 1 DO
            <*ASSERT desc[newP + i - p0] > 0*>
          END;
          IF fragLength > 0 THEN
            desc[fragP - p0] := 0;
            WITH page = PageToRef(fragP) DO
              page.desc.space := Space.Free;
              page.nb := fragLength;
            END;
            FOR i := 1 TO fragLength - 1 DO
              <*ASSERT desc[fragP + i - p0] > 0*>
            END;
          END;
        ELSE
          newP := p + fragLength;
          fragP := p;
          desc[newP - p0] := 0;
          WITH page = PageToRef(newP) DO
            page.desc.space := Space.Free;
            page.nb := n;
          END;
          FOR i := 1 TO n - 1 DO
            <*ASSERT desc[newP + i - p0] > 0*>
          END;
          IF fragLength > 0 THEN
            WITH page = PageToRef(fragP) DO
              <*ASSERT page.desc.space = Space.Free*>
              page.nb := fragLength;
            END;
            FOR i := 1 TO fragLength - 1 DO
              <*ASSERT desc[fragP + i - p0] > 0*>
            END;
          END;
        END;
        IF fragLength > prevLength THEN
          IF prevP = NIL THEN
            free := fragP;
          ELSE
            prevP.desc.link := fragP;
          END;
          PageToRef(fragP).desc.link := page.desc.link;
        ELSE
          IF prevP = NIL THEN
            free := page.desc.link;
          ELSE
            prevP.desc.link := page.desc.link;
          END;
          VAR
            pp              := free;
            prevPP: RefPage := NIL;
          BEGIN
            LOOP
              IF pp = Nil THEN EXIT; END;
              WITH page = PageToRef(pp), length = page.nb DO
                IF length > fragLength
                     OR (length = fragLength AND pp > fragP) THEN
                  EXIT;
                END;
                prevPP := page;
                pp := page.desc.link;
              END;
            END;
            PageToRef(fragP).desc.link := pp;
            IF prevPP = NIL THEN
              free := fragP;
            ELSE
              prevPP.desc.link := fragP;
            END;
          END;
        END;
        RETURN PageToRef(newP);
      END;
    END;
  END AllocateFreePagesFromBlock;

(* RebuildFreelist rebuilds the free list, from the "desc" array.  It first
   links all free blocks into the free list, then it sorts the free list.
   The sort used is insertion sort, which is quadratic in the number of
   different block sizes, but only linear in the number of pages. *)

PROCEDURE RebuildFreelist () =
  BEGIN
    VAR
      prevP: RefPage := NIL;
      prevFree := FALSE;
    BEGIN
      (* link together the first pages of all free blocks *)
      FOR p := p0 TO p1 - 1 DO
        IF desc[p - p0] = 0 THEN
          WITH page = PageToRef(p), space = page.desc.space DO
            IF space = Space.Free THEN
              IF prevFree THEN
                <*ASSERT prevP # NIL*>
                desc[p - p0] := 1;
                FOR i := 1 TO page.nb - 1 DO
                  <*ASSERT desc[p + i - p0] > 0*>
                END;
                INC(prevP.nb, page.nb);
              ELSE
                IF prevP = NIL THEN
                  free := p;
                ELSE
                  prevP.desc.link := p;
                END;
                prevP := page;
                prevFree := TRUE;
              END;
            ELSE
              prevFree := FALSE;
            END;
          END;
        ELSE
          prevFree := FALSE;
        END;
      END;
      IF prevP = NIL THEN
        free := Nil;
      ELSE
        prevP.desc.link := Nil;
      END;
    END;
    (* sort them, using insertion sort *)
    VAR
      n     := 1;                (* smallest block size *)
      p     := free;             (* start of sublist we're examining *)
      prevP : RefPage := NIL;    (* element before sublist *)
    BEGIN
      LOOP
        VAR
          excess     := Nil;
          prevExcess := Nil;
        BEGIN
          (* separate off blocks over "n" long into excess list *)
          WHILE p # Nil DO
            WITH page = PageToRef(p), length = page.nb DO
              <* ASSERT length >= n *>
              IF length > n THEN
                IF prevExcess = Nil THEN
                  excess := p;
                ELSE
                  PageToRef(prevExcess).desc.link := p;
                END;
                IF prevP = NIL THEN
                  free := page.desc.link;
                ELSE
                  prevP.desc.link := page.desc.link;
                END;
                prevExcess := p;
              ELSE
                prevP := page;
              END;
              p := page.desc.link;
            END;
          END;
          (* maybe done *)
          IF excess = Nil THEN EXIT; END;
          <* ASSERT prevExcess # Nil *>
          (* link longer blocks onto end *)
          IF prevP = NIL THEN
            free := excess;
          ELSE
            prevP.desc.link := excess;
          END;
          PageToRef(prevExcess).desc.link := Nil;
          p := excess;
        END;
        (* find smallest element size of remaining bocks *)
        n := LAST(CARDINAL);
        VAR pp := p;
        BEGIN
          REPEAT
            WITH page = PageToRef(pp), length = page.nb DO
              IF length < n THEN n := length; END;
              pp := page.desc.link;
            END;
          UNTIL pp = Nil;
        END;
      END;
    END;
  END RebuildFreelist;

(* GrowHeap adds a block of at least "MinNewPages" free pages to the heap,
   and links it into the free list. *)

(* "MinNewPages" is the minimum number of pages by which to grow the heap.
   Setting it higher reduces the number of system calls; setting it lower
   keeps the heap a little smaller. *)

VAR fragment0, fragment1: ADDRESS := NIL;

CONST
  MB = 16_100000;
  MinNewFactor = 0.2;			 (* grow the heap by at least 20% *)

  InitialPages = 4;			 (* 4 * 64K = 256K *)
  MinNewPages  = 4;			 (* 4 * 64K = 256K *)

VAR
  heap_stats := FALSE;
  total_heap := 0;

PROCEDURE GrowHeap (pp: INTEGER): BOOLEAN =
  VAR
    newChunk    : ADDRESS;
    newSideSpan : INTEGER;
    firstNewPage: Page;
    lastNewPage : Page;
    newP0       : Page;
    newP1       : Page;
  BEGIN
    IF max_heap >= 0 AND total_heap > max_heap THEN
      RETURN FALSE;  (* heap is already too large *)
    END;
    IF allocatedPages = 0 THEN
      pp := MAX(pp, InitialPages);
    ELSE
      pp := MAX(pp, MinNewPages);
      pp := MAX(pp, CEILING(FLOAT(allocatedPages) * MinNewFactor));
    END;
    VAR bytes := (pp + 1) * BytesPerPage;
    BEGIN
      IF max_heap >= 0 THEN
        bytes := MIN (bytes, max_heap - total_heap);
        IF (bytes <= 0) THEN RETURN FALSE; END;
      END;
      newChunk := RTOS.GetMemory(bytes);
      INC (total_heap, bytes);
      IF heap_stats THEN
        RTIO.PutText ("Grow (");
        RTIO.PutHex  (bytes);
        RTIO.PutText (") => ");
        RTIO.PutAddr (newChunk);
        RTIO.PutText ("   total: ");
        RTIO.PutInt  (total_heap DIV MB);
        RTIO.PutText (".");
        RTIO.PutInt  ((total_heap MOD MB) DIV (MB DIV 10));
        RTIO.PutText ("M");
      END;
      IF newChunk = NIL OR newChunk = LOOPHOLE(-1, ADDRESS) THEN
        RETURN FALSE;
      END;
      IF fragment1 = newChunk THEN
        newChunk := fragment0;
        bytes := bytes + (fragment1 - fragment0);
      END;
      VAR excess := Word.Mod(-LOOPHOLE(newChunk, INTEGER), BytesPerPage);
      BEGIN
        INC(newChunk, excess);
        DEC(bytes, excess);
      END;
      VAR pages := bytes DIV BytesPerPage;
      BEGIN
        firstNewPage := Word.RightShift(LOOPHOLE(newChunk, INTEGER),
                                        LogBytesPerPage);
        lastNewPage := firstNewPage + pages - 1;
        fragment0 :=
          LOOPHOLE((firstNewPage + pages) * BytesPerPage, ADDRESS);
        fragment1 := newChunk + bytes;
      END;
    END;
    (* determine the new boundaries of the allocated pages *)
    IF p0 = Nil THEN
      newP0 := firstNewPage;
      newP1 := lastNewPage + 1;
    ELSIF firstNewPage < p0 THEN
      newP0 := firstNewPage;
      newP1 := p1;
    ELSIF p1 <= lastNewPage THEN
      newP0 := p0;
      newP1 := lastNewPage + 1;
    ELSE
      newP0 := p0;
      newP1 := p1;
    END;
    (* extend the side arrays if necessary *)
    newSideSpan := newP1 - newP0;
    IF desc = NIL OR newSideSpan # NUMBER(desc^) THEN
      WITH newDesc = NEW(UNTRACED REF ARRAY OF BITS 8 FOR [-1..1],
                         newSideSpan) DO
        IF desc # NIL THEN
          FOR i := FIRST(desc^) TO LAST(desc^) DO
            newDesc[i + p0 - newP0] := desc[i];
          END;
          FOR i := p1 TO firstNewPage - 1 DO
            newDesc[i - newP0] := -1;	 (* Space.Unallocated *)
          END;
          FOR i := lastNewPage + 1 TO p0 - 1 DO
            newDesc[i - newP0] := -1;	 (* Space.Unallocated *)
          END;
          DISPOSE(desc);
        END;
        desc := newDesc;
      END;
    END;
    p0 := newP0;
    p1 := newP1;
    IF heap_stats THEN
      VAR
        span    := (p1 - p0) * BytesPerPage;
        density := ROUND (FLOAT(total_heap) * 100.0 / FLOAT (span));
      BEGIN
        RTIO.PutText ("   span: ");
        RTIO.PutInt  (span DIV MB);
        RTIO.PutText (".");
        RTIO.PutInt  ((span MOD MB) DIV (MB DIV 10));
        RTIO.PutText ("M");
        RTIO.PutText ("   density: ");
        RTIO.PutInt  (density);
        RTIO.PutText ("%\n");
        RTIO.Flush ();
      END;
    END;
    desc[firstNewPage - p0] := 0;
    FOR i := 1 TO lastNewPage - firstNewPage DO
      desc[firstNewPage + i - p0] := 1;
    END;
    WITH page = PageToRef(firstNewPage) DO
      page.desc.space := Space.Free;
      page.nb := lastNewPage - firstNewPage + 1;
    END;
    IF perfOn THEN
      PerfGrow(firstNewPage, lastNewPage - firstNewPage + 1);
    END;
    INC(allocatedPages, lastNewPage - firstNewPage + 1);
    RebuildFreelist();
    RETURN TRUE;
  END GrowHeap;

VAR
  minorCollections := 0;                 (* the number of minor GCs begun *)
  majorCollections := 0;		 (* the number of major GCs begun *)
  checkLoadTracedRef := 0;
  checkStoreTraced := 0;
  tStamps: ARRAY [0..1048575] OF Time.T;
  tsIndex := -1;
  tStart: Time.T;

PROCEDURE StartBench() =
  BEGIN
    majorCollections := 0;
    minorCollections := 0;
    checkLoadTracedRef := 0;
    checkStoreTraced := 0;
    tsIndex := 0;
    tStart := Time.Now();
  END StartBench;

PROCEDURE FinishBench() =
  VAR
    tEnd    := Time.Now();
    span    := (p1 - p0) * BytesPerPage;
    density := ROUND (FLOAT(total_heap) * 100.0 / FLOAT (span));
  BEGIN
    RTIO.PutText("\nBEGIN\n");
    FOR i := 0 TO tsIndex-1 BY 2 DO
      RTIO.PutInt(TRUNC((tStamps[i+0] - tStart) * 1.0D6));
      RTIO.PutChar(' ');
      RTIO.PutInt(TRUNC((tStamps[i+1] - tStart) * 1.0D6));
      RTIO.PutChar('\n');
    END;
    tsIndex := -1;
    RTIO.PutInt(TRUNC((tEnd - tStart) * 1.0D6));
    RTIO.PutChar(' ');
    RTIO.PutInt(TRUNC((tEnd - tStart) * 1.0D6));
    RTIO.PutText("\nEND\n");

    RTIO.PutText("\nCollections: ");
    RTIO.PutInt(minorCollections + majorCollections);
    RTIO.PutText(" ("); RTIO.PutInt(majorCollections); RTIO.PutText(" full, ");
    RTIO.PutInt(minorCollections); RTIO.PutText(" partial)\n");

    RTIO.PutText("Slow path inc barriers: ");
    RTIO.PutInt(checkLoadTracedRef);
    RTIO.PutText("\nSlow path gen barriers: ");
    RTIO.PutInt(checkStoreTraced);

    RTIO.PutText ("\nTotal heap: ");
    RTIO.PutInt  (total_heap DIV MB);
    RTIO.PutText (".");
    RTIO.PutInt  ((total_heap MOD MB) DIV (MB DIV 10));
    RTIO.PutText ("M");
    RTIO.PutText ("   span: ");
    RTIO.PutInt  (span DIV MB);
    RTIO.PutText (".");
    RTIO.PutInt  ((span MOD MB) DIV (MB DIV 10));
    RTIO.PutText ("M");
    RTIO.PutText ("   density: ");
    RTIO.PutInt  (density);
    RTIO.PutText ("%\n");
    RTIO.Flush ();
  END FinishBench;

(*** INITIALIZATION ***)

PROCEDURE AtForkChild() =
  BEGIN
    (* There are no other threads (so synchronisation is unnecessary) *)
    startedForeground := FALSE;
    startedBackground := FALSE;
    startedWeakCleaner := FALSE;
  END AtForkChild;

PROCEDURE Init () =
  BEGIN
    WITH r = RTProcess.RegisterForkHandlers(NIL, NIL, AtForkChild) DO
      <* ASSERT r = 0 *>
    END;
    IF RTParams.IsPresent("paranoidgc") THEN InstallSanityCheck(); END;
    IF RTParams.IsPresent("nogc") THEN disableCount := 1; END;
    IF RTParams.IsPresent("noincremental") THEN incremental := FALSE; END;
    IF RTParams.IsPresent("nogenerational") THEN generational := FALSE; END;
    IF RTParams.IsPresent("heapstats") THEN heap_stats := TRUE; END;
    PerfStart();
  END Init;

BEGIN
  <*ASSERT LOOPHOLE(0, ADDRESS) = NIL*>

  weakTable := NEW(UNTRACED REF ARRAY OF WeakEntry, 0);

  (* initialize the alignment array *)
  FOR i := FIRST(align) TO LAST(align) DO
    FOR j := FIRST(align[0]) TO LAST(align[0]) DO
      align[i, j] := RTMisc.Upper(i, j) - i;
    END;
  END;
END RTCollector.
