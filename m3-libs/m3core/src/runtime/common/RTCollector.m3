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
                                  RTHeapRep, RTWeakRef;

IMPORT RT0, RTHeapEvent, RTHeapDep, RTHeapMap, RTIO, RTMachine, RTVM;
IMPORT RTMisc, RTOS, RTParams, RTPerfTool, RTProcess, RTType;
IMPORT Word, Cstdlib, Thread, ThreadF, RuntimeError;
IMPORT TextLiteral;

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
    RTOS.LockHeap();
    BEGIN
      FinishVM();
      INC(disableCount);
      partialCollectionNext := FALSE;
    END;
    RTOS.UnlockHeap();
    IF perfOn THEN PerfAllow(); END;
  END Disable;

PROCEDURE Enable () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      DEC(disableCount);
      CollectEnough();
    END;
    RTOS.UnlockHeap();
    IF perfOn THEN PerfAllow(); END;
  END Enable;

PROCEDURE DisableMotion () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      INC(disableMotionCount);
    END;
    RTOS.UnlockHeap();
    IF perfOn THEN PerfAllow(); END;
  END DisableMotion;

PROCEDURE EnableMotion () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      DEC(disableMotionCount);
      CollectEnough();
    END;
    RTOS.UnlockHeap();
    IF perfOn THEN PerfAllow(); END;
  END EnableMotion;

PROCEDURE Collect () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      FinishGC();
      StartGC();
      FinishGC();
    END;
    RTOS.UnlockHeap();
  END Collect;

(*** RTCollectorSRC ***)

(* StartCollection starts a total collection, if none is in progress and if
   collection and motion are enabled. *)

PROCEDURE StartCollection () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      CollectorOn();
      IF collectorState = CollectorState.Zero
           AND disableCount + disableMotionCount = 0 THEN
        partialCollectionNext := FALSE;
        REPEAT CollectSome(); UNTIL collectorState # CollectorState.Zero;
        IF NOT (incremental AND RTHeapDep.VM AND disableVMCount = 0) THEN
          REPEAT CollectSome(); UNTIL collectorState = CollectorState.Zero;
        END;
      END;
      CollectorOff();
    END;
    RTOS.UnlockHeap();
  END StartCollection;

(* FinishCollection finishes the current collection, if one is on
   progress. *)

PROCEDURE FinishCollection () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      CollectorOn();
      WHILE collectorState # CollectorState.Zero DO CollectSome(); END;
      CollectorOff();
    END;
    RTOS.UnlockHeap();
  END FinishCollection;

(* DisableVM disables the use of VM protection.  While VM protection is
   disabled, no objects on the heap will be protected.*)

PROCEDURE DisableVM () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      FinishVM();
      INC(disableVMCount);
    END;
    RTOS.UnlockHeap();
  END DisableVM;

(* EnableVM reenables the use of VM protection if EnableVM has been called
   as many times as DisableVM.  It is a checked runtime error to call
   EnableVM more times than DisableVM. *)

PROCEDURE EnableVM () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      DEC(disableVMCount);
      CollectEnough();
    END;
    RTOS.UnlockHeap();
  END EnableVM;

(* FinishVM is equivalent to DisableVM{}; EnableVM().  FinishVM unprotects
   all heap pages, and is intended for use from the debugger. *)

PROCEDURE FinishVM () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      FinishGC();
      CollectorOn();
      (* no gray pages now; only protected pages are in older generation *)
      FOR p := p0 TO p1 - 1 DO
        IF desc[p - p0].protected THEN Protect(p, Mode.ReadWrite); END;
      END;
      CollectorOff();
    END;
    RTOS.UnlockHeap();
  END FinishVM;

(* StartBackgroundCollection starts the background thread, if not already
   started *)

VAR startedBackground := FALSE;

PROCEDURE StartBackgroundCollection () =
  VAR start := FALSE;
  BEGIN
    RTOS.LockHeap();
    BEGIN
      IF NOT startedBackground THEN
        start := TRUE;
        startedBackground := TRUE;
      END;
    END;
    RTOS.UnlockHeap();
    IF start THEN
      EVAL Thread.Fork(NEW(Thread.Closure, apply := BackgroundThread));
    END;
  END StartBackgroundCollection;

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
|      AND desc[p - p0] != Unallocated

   NUMBER (desc) must be equal to p1 - p0 if there are allocated pages.
   Index i in desc correspond to page i + p0; that is p0 is the number of
   the first page available in desc, and it must be in [p0 ..  p1) if there
   are allocated pages. *)

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

(* We need to be able to determine the size of an referent during
   collection; here is a functions to do just that.  It must be called with
   a non-nil pointer to the Header of a heap object that is there (has not
   been moved). *)

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

    IF tc = RT0.TextLitTypecode THEN
      VAR
        txt := LOOPHOLE (h + ADRSIZE(Header), TextLiteral.T);
        len : INTEGER := txt.cnt;
      BEGIN
        IF (len >= 0)
          THEN INC (len); (* null CHAR *)
          ELSE len := 2 (*null WIDECHAR*) - len - len;
        END;
        RETURN ADR (txt.buf[len]) - LOOPHOLE (txt, ADDRESS);
      END;
    END;

    def := RTType.Get (tc);

    IF (def.kind # ORD (TK.Array)) THEN
      (* the typecell datasize tells the truth *)
      RETURN def.dataSize;
    END;

(* Otherwise, the referent is an open array; it has the following layout:
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
      adef := LOOPHOLE (def, RT0.ArrayTypeDefn);
      sizes: UNTRACED REF INTEGER := h + ADRSIZE(Header) + ADRSIZE(ADDRESS);
                                                           (* ^ elt pointer*)
    BEGIN
      res := 1;
      FOR i := 0 TO adef.nDimensions - 1 DO
        res := res * sizes^;
        INC(sizes, ADRSIZE(sizes^));
      END;
      res := res * adef.elementSize;
    END;
    res := RTMisc.Upper(res + def.dataSize, BYTESIZE(Header));
    RETURN res;
  END ReferentSize;

(* The convention about page numbering allows for a simple conversion from
   an address to the number of the page in which it is, as well as from a
   page number to the first address is contains: *)

PROCEDURE ReferentToPage (r: RefReferent): Page =
  (* VAR p: INTEGER := LOOPHOLE(r, INTEGER) DIV BytesPerPage; *)
  VAR p: INTEGER := Word.RightShift (LOOPHOLE(r, INTEGER), LogBytesPerPage);
  BEGIN
    IF p < p0 OR p >= p1 OR desc[p - p0].space = Space.Unallocated
      THEN RETURN Nil;
      ELSE RETURN p;
    END;
  END ReferentToPage;

PROCEDURE HeaderToPage (r: RefHeader): Page =
  (* VAR p: INTEGER := LOOPHOLE(r, INTEGER) DIV BytesPerPage; *)
  VAR p: INTEGER := Word.RightShift (LOOPHOLE(r, INTEGER), LogBytesPerPage);
  BEGIN
    IF p < p0 OR p >= p1 OR desc[p - p0].space = Space.Unallocated
      THEN RETURN Nil;
      ELSE RETURN p;
    END;
  END HeaderToPage;

PROCEDURE PageToHeader (p: Page): RefHeader =
  BEGIN
    RETURN LOOPHOLE(p * BytesPerPage, RefHeader);
  END PageToHeader;

PROCEDURE PageToAddress (p: Page): ADDRESS =
  BEGIN
    RETURN LOOPHOLE(p * BytesPerPage, ADDRESS);
  END PageToAddress;


(* To move a heap object to the new space, modifying the original reference
   to it *)

TYPE Mover = RTHeapMap.Visitor OBJECT OVERRIDES apply := Move END;

PROCEDURE Move (<*UNUSED*> self: Mover;  cp: ADDRESS) =
  VAR
    refref := LOOPHOLE(cp, UNTRACED REF RefReferent);
    ref    := refref^;
    p      : INTEGER;
    hdr    : RefHeader;
  BEGIN
    IF ref = NIL THEN RETURN; END;

    (* INLINE: p := ReferentToPage(ref); *)
    p := Word.RightShift (LOOPHOLE(ref, INTEGER), LogBytesPerPage);
    IF p < p0 OR p >= p1 OR desc[p - p0].space # Space.Previous THEN RETURN; END;

    (* INLINE: hdr := HeaderOf(ref); *)
    hdr := LOOPHOLE(ref - ADRSIZE(Header), RefHeader);

    IF hdr.forwarded THEN
      (* if already moved, just update the reference *)
      refref^ := LOOPHOLE(ref, UNTRACED REF RefReferent)^;
      RETURN;
    END;

    IF p + 1 < p1 AND desc[p - p0 + 1].continued THEN
      (* if this is a large object, just promote the pages *)
      VAR def := RTType.Get (hdr.typecode); BEGIN
        IF (def.gc_map = NIL) AND (def.kind # ORD(TK.Obj))
          THEN PromotePage(p, PromoteReason.LargePure);
          ELSE PromotePage(p, PromoteReason.LargeImpure);
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
      IF (def.gc_map # NIL) OR (def.kind = ORD(TK.Obj))
        THEN np := AllocTraced(dataSize, def.dataAlignment, impureCopy);
        ELSE np := AllocTraced(dataSize, def.dataAlignment, pureCopy);
      END;
      IF (np = NIL) THEN
        RAISE RuntimeError.E (RuntimeError.T.OutOfMemory);
      END;
      RTMisc.Copy(hdr, HeaderOf(np), BYTESIZE(Header) + dataSize);
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
  BEGIN
    IF ref = NIL THEN RETURN TRUE; END;

    (* check the space *)
    (* INLINE: p := ReferentToPage(ref); *)
    p := Word.RightShift (LOOPHOLE(ref, INTEGER), LogBytesPerPage);
    IF p < p0 OR p >= p1 OR desc[p - p0].space # Space.Previous THEN
      RETURN TRUE;
    END;

    (* check the forwarded bit *)
    (* INLINE: hdr := HeaderOf(ref); *)
    hdr := LOOPHOLE(ref - ADRSIZE(Header), RefHeader);
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

VAR
  heap_min, heap_max: ADDRESS;
  (* Must be set before calling NoteStackLocations *)

PROCEDURE NoteStackLocations (start, stop: ADDRESS) =
  CONST Reason = ARRAY BOOLEAN OF PromoteReason {
          PromoteReason.AmbiguousImpure, PromoteReason.AmbiguousPure };
  VAR
    fp : ADDRESS := start;
    p  : ADDRESS;
    pp : Page;
  BEGIN
    stop := stop - ADRSIZE (ADDRESS); (* so we don't overrun the valid addresses *)
    WHILE fp <= stop DO               (* with the memory read on the next line.  *)
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF heap_min <= p AND p < heap_max THEN
        pp := Word.RightShift (LOOPHOLE(p, INTEGER), LogBytesPerPage);
        WITH pd = desc[pp - p0] DO
          IF pd.space = Space.Previous THEN
            IF pd.continued THEN pp := FirstPage(pp); END;
            PromotePage(pp, Reason[pd.pure]);
          END;
        END;
      END;
      INC(fp, RTMachine.PointerAlignment);
    END;
  END NoteStackLocations;

TYPE
  PromoteReason = {
    OldProtected, OldPure, OldImpure,
    LargePure, LargeImpure,
    AmbiguousPure, AmbiguousImpure
  };

CONST
  PromotedPageNeedsScan = ARRAY PromoteReason OF BOOLEAN {
    FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE
  };

  PromoteDesc = ARRAY PromoteReason OF Desc {
    (* OldProtected *)
    Desc{ Space.Current, Generation.Older, pure := FALSE, gray := FALSE,
      note := Note.OlderGeneration, protected := TRUE, continued := FALSE },

    (* OldPure *)
    Desc{ Space.Current, Generation.Older, pure := TRUE, gray := FALSE,
      note := Note.OlderGeneration, protected := FALSE, continued := FALSE },

    (* OldImpure *)
    Desc{ Space.Current, Generation.Older, pure := FALSE, gray := TRUE,
      note := Note.OlderGeneration, protected := FALSE, continued := FALSE },

    (* LargePure *)
    Desc{ Space.Current, Generation.Older, pure := TRUE, gray := FALSE,
      note := Note.Large, protected := FALSE, continued := FALSE },

    (* LargeImpure *)
    Desc{ Space.Current, Generation.Older, pure := FALSE, gray := TRUE,
      note := Note.Large, protected := FALSE, continued := FALSE },

    (* AmbiguousPure *)
    Desc{ Space.Current, Generation.Older, pure := TRUE, gray := FALSE,
      note := Note.AmbiguousRoot, protected := FALSE, continued := FALSE },

    (* AmbiguousImpure *)
    Desc{ Space.Current, Generation.Older, pure := FALSE, gray := TRUE,
      note := Note.AmbiguousRoot, protected := FALSE, continued := FALSE }
  };

PROCEDURE PromotePage (p: Page;  r: PromoteReason) =
  VAR
    d := PromoteDesc [r];
    n_pages := PageCount(p);
  BEGIN
    d.generation := pureCopy.desc.generation;

    WITH pd = desc[p - p0] DO
      <* ASSERT pd.space = Space.Previous *>
      <* ASSERT NOT pd.continued*>

      IF pd.protected # d.protected THEN
        <*ASSERT NOT d.protected*>
        Protect(p, Mode.ReadWrite);
      END;

      pd := d;

      IF PromotedPageNeedsScan[r] THEN
        pd.link := impureCopy.stack;
        impureCopy.stack := p;
      END;
    END;

    IF n_pages > 1 THEN
      d.continued := TRUE;
      FOR pp := p + 1 TO p + n_pages - 1 DO desc[pp - p0] := d; END;
    END;

    INC (n_promoted, n_pages);
    IF perfOn THEN PerfChange(p, n_pages); END;
  END PromotePage;

PROCEDURE FillPool (VAR p: AllocPool) =
  BEGIN
    InsertFiller(p.next, p.limit - p.next);
    p.next  := NIL;
    p.limit := NIL;
  END FillPool;

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
  threshold := ARRAY [0 .. 1] OF
                 REAL{FLOAT(InitialBytes DIV 4 DIV BytesPerPage - 1), 1.0};
(* start a collection as soon as current space reaches threshold[0] /
   threshold[1] pages; the initial value is 64KB *)

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
  BEGIN
    IF collectorOn THEN RETURN; END;
    IF Behind() THEN
      CollectorOn();
      IF incremental AND RTHeapDep.VM AND disableVMCount = 0 THEN
        REPEAT CollectSome(); UNTIL NOT Behind();
      ELSE
        WHILE collectorState = CollectorState.Zero DO CollectSome(); END;
        REPEAT CollectSome(); UNTIL collectorState = CollectorState.Zero;
      END;
      CollectorOff();
    END;
  END CollectEnough;

PROCEDURE Behind (): BOOLEAN =
  VAR n_new, n_copied: INTEGER;
  BEGIN
    IF disableCount + disableMotionCount > 0
         AND collectorState = CollectorState.Zero THEN
      RETURN FALSE;
    END;
    n_new := newPool.n_small + newPool.n_big;
    n_copied := pureCopy.n_small + pureCopy.n_big
                 + impureCopy.n_small + impureCopy.n_big;
    IF collectorState = CollectorState.Zero THEN
      RETURN FLOAT(n_new + n_copied + n_promoted) * threshold[1] >= threshold[0];
    ELSE
      RETURN FLOAT(n_new) * gcRatio >= FLOAT(n_copied);
    END;
  END Behind;

VAR timeUsedOnEntry: REAL;       (* time used when entered collector *)

PROCEDURE CollectorOn () =
  (* LL >= RTOS.HeapLock *)
  BEGIN
    <* ASSERT NOT collectorOn *>
    collectorOn := TRUE;

    ThreadF.SuspendOthers ();
    (* If the collector is unprotecting pages and moving stuff around,
       other threads cannot be running!  -- 7/16/96 WKK *)

    IF RTHeapDep.VM THEN timeUsedOnEntry := RTHeapDep.TimeUsed(); END;

    IF impureCopy.page # Nil THEN
      WITH pd = desc[impureCopy.page - p0] DO
        <* ASSERT pd.gray *>
        <* ASSERT pd.protected *>
      END;
      Protect(impureCopy.page, Mode.ReadWrite);
    END;
  END CollectorOn;

PROCEDURE CollectorOff () =
  (* LL >= RTOS.HeapLock *)
  BEGIN
    <* ASSERT collectorOn *>

    IF impureCopy.page # Nil THEN
      WITH pd = desc[impureCopy.page - p0] DO
        <* ASSERT pd.gray *>
        <* ASSERT NOT pd.protected *>
      END;
      Protect(impureCopy.page, Mode.NoAccess);
    END;

    VAR p := impureCopy.stack; BEGIN
      WHILE p # Nil DO
        IF desc[p - p0].gray AND NOT desc[p - p0].protected THEN
          Protect(p, Mode.NoAccess);
        END;
        p := desc[p - p0].link;
      END;
    END;

    ThreadF.ResumeOthers ();

    collectorOn := FALSE;

    IF signalBackground OR signalWeak THEN
      signalBackground := FALSE;
      signalWeak := FALSE;
      RTOS.BroadcastHeap();
    END;

    IF RTHeapDep.VM THEN
      cycleCost := cycleCost + (RTHeapDep.TimeUsed() - timeUsedOnEntry);
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
  cycleCost  : REAL     := 0.0;  (* running cost of current cycle *)
  cycleLength: CARDINAL := 1;    (* current planned cycle length *)
  cycleL     : CARDINAL := 0;    (* length of current cycle, so far *)
  cycleNews  : CARDINAL;         (* the number of new pages this cycle *)
  minPrefixAvgCost: REAL;        (* minimum average cost for a prefix of
                                    this cycle *)
  minCycleL  : CARDINAL;         (* the length of that prefix *)
  n_promoted : CARDINAL := 0;    (* # of pages promoted this cycle *)

PROCEDURE CollectSomeInStateZero () =
  BEGIN
    <* ASSERT disableCount + disableMotionCount = 0 *>
    (* compute some costs relative to previous collection *)
    INC(cycleNews, newPool.n_small + newPool.n_big);
    VAR prefixAvgCost := cycleCost / FLOAT(cycleNews);
    BEGIN
      IF prefixAvgCost < minPrefixAvgCost THEN
        minPrefixAvgCost := prefixAvgCost;
        minCycleL := cycleL;
      END;
    END;

    (* make generational decisions *)
    IF generational AND RTHeapDep.VM AND disableVMCount = 0 THEN
      pureCopy.desc.generation   := Generation.Older;
      impureCopy.desc.generation := Generation.Older;
      partialCollection := partialCollectionNext AND cycleL < cycleLength;
      IF NOT partialCollection THEN
        IF minCycleL = cycleLength THEN
          cycleLength := cycleLength + 1;
        ELSE
          cycleLength := MAX(cycleLength - 1, 1);
        END;
      END;
    ELSE
      pureCopy.desc.generation   := Generation.Younger;
      impureCopy.desc.generation := Generation.Younger;
      partialCollection := FALSE;
    END;
    partialCollectionNext := TRUE;

    IF partialCollection THEN
      INC(cycleL);
    ELSE
      cycleL := 1;
      cycleCost := 0.0;
      cycleNews := 0;
      minPrefixAvgCost := LAST(REAL);
      minCycleL := 0;
    END;

    InvokeMonitors (before := TRUE);

    IF perfOn THEN PerfBegin(); END;

    (* fill the rest of the current page *)
    FillPool(newPool);
    newPool.page  := Nil;
    newPool.stack := Nil;

    INC(collections);

    (* flip spaces; newspace becomes oldspace *)
    FOR p := p0 TO p1 - 1 DO
      WITH pd = desc[p - p0] DO
        IF pd.space = Space.Current THEN
          pd.space := Space.Previous;
          IF perfOn THEN PerfChange(p, 1); END;
        END;
      END;
    END;

    IF perfOn THEN PerfFlip(); END;

    (* The 'new' nextSpace is empty *)
    newPool.n_small    := 0;   newPool.n_big    := 0;
    pureCopy.n_small   := 0;   pureCopy.n_big   := 0;
    impureCopy.n_small := 0;   impureCopy.n_big := 0;
    n_promoted := 0;

    (* Conservatively scan the stacks for possible pointers. *)
    (* Note: we must scan thread stacks before promoting old
       pages, because we want to make sure that old, impure, unprotected
       pages referenced by threads are marked as ambiguous roots.
       Otherwise, these pages won't get cleaned by "FinishThreadPages". *)
    heap_min := PageToAddress (p0);
    heap_max := PageToAddress (p1);
    ThreadF.ProcessStacks(NoteStackLocations);
    (* Now, nothing in previous space is referenced by a thread. *)

    (* Promote any remaining "old" pages and unprotect everything else *)
    FOR p := p0 TO p1 - 1 DO
      WITH pd = desc[p - p0] DO
        IF pd.space = Space.Previous AND NOT pd.continued THEN
          IF pd.generation = Generation.Older THEN
            IF partialCollection THEN
              IF pd.protected THEN
                <* ASSERT NOT pd.pure *>
                PromotePage(p, PromoteReason.OldProtected);
              ELSIF pd.pure THEN
                PromotePage(p, PromoteReason.OldPure);
              ELSE
                PromotePage(p, PromoteReason.OldImpure);
              END;
            ELSE
              IF pd.protected THEN Protect(p, Mode.ReadWrite); END;
            END;
          ELSE
            <* ASSERT NOT pd.protected *>
          END;
        END;
      END;
    END;
    (* Now, nothing in the previous space is protected or in the older
       generation. *)

    mover := NEW (Mover);  (* get one in the new space *)

    (* On some systems (ie Win32) the system call wrappers are not atomic
       with respect to the collector, so it's possible that this collection
       started after a thread had validated its system call parameters but
       before the system call completed.  On those systems, we must ensure
       that the heap pages referenced by threads remain unprotected after
       the collection begins. *)
    IF RTVM.VMHeap() AND NOT RTVM.AtomicWrappers() THEN
      FinishThreadPages ();
    END;

    (* Scan the global variables for possible pointers *)
    RTHeapMap.WalkGlobals (mover);

    IF perfOn THEN PerfPromotedRoots(); END;

    collectorState := CollectorState.One;
    IF backgroundWaiting THEN signalBackground := TRUE; END;
  END CollectSomeInStateZero;

PROCEDURE FinishThreadPages () =
  (* Clean any pages referenced from the threads. *)
  VAR
    p      : Page := impureCopy.stack;
    next_p : Page;
  BEGIN
    WHILE (p # Nil) DO
      WITH pd = desc[p - p0] DO
        next_p := pd.link;
        IF pd.gray AND pd.note = Note.AmbiguousRoot THEN
          CleanPage(p, thread_page := TRUE);
        END;
      END;
      p := next_p;
    END;
  END FinishThreadPages;

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
    FOR i := 0 TO p1 - p0 - 1 DO
      IF desc[i].space = Space.Previous THEN
        desc[i].space := Space.Free;
        desc[i].continued := FALSE;
        <* ASSERT NOT desc[i].protected *>
        IF perfOn THEN PerfChange(p0 + i, 1); END;
      END;
    END;

    RebuildFreelist();

    (* fill the rest of the current copy pages *)
    FillPool(pureCopy);  FillPool(impureCopy);

    IF impureCopy.page # Nil THEN
      WITH pd = desc[impureCopy.page - p0] DO
        pd.gray := FALSE;
        IF pd.generation = Generation.Older THEN
          <* ASSERT pd.space = Space.Current *>
          Protect(impureCopy.page, Mode.ReadOnly);
        END;
      END;
      IF perfOn THEN PerfChange(impureCopy.page, 1); END;
      impureCopy.page := Nil;
    END;
    <* ASSERT impureCopy.stack = Nil *>

    pureCopy.page  := Nil;    impureCopy.page  := Nil;
    pureCopy.stack := Nil;    impureCopy.stack := Nil;
    pureCopy.next  := NIL;    impureCopy.next  := NIL;
    pureCopy.limit := NIL;    impureCopy.limit := NIL;

    IF perfOn THEN PerfEnd(); END;

    InvokeMonitors(before := FALSE);

    VAR n_survivors := FLOAT(pureCopy.n_small + pureCopy.n_big
                           + impureCopy.n_small + impureCopy.n_big
                           + n_promoted);
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

PROCEDURE CopySome (): BOOLEAN =
  VAR
    originalPage  := impureCopy.page;
    originalLimit := impureCopy.limit;
    cleanTo       := PageToHeader(impureCopy.page);
  BEGIN
    LOOP
      IF cleanTo < impureCopy.next THEN
        VAR ptr := impureCopy.next;
        BEGIN
          CleanBetween(cleanTo, ptr);
          cleanTo := ptr;
        END;
      ELSE
        IF impureCopy.stack = Nil THEN RETURN FALSE; END;
        VAR p := impureCopy.stack;
        BEGIN
          WITH pd = desc[p - p0] DO
            impureCopy.stack := pd.link;
            <* ASSERT NOT pd.pure *>
            IF pd.gray THEN CleanPage(p); END;
          END;
        END;
      END;
      IF impureCopy.page # originalPage THEN EXIT; END;
    END;
    CleanBetween(cleanTo, originalLimit);

    (* originalPage is now in the stack; mark it not gray *)
    IF originalPage # Nil THEN
      WITH pd = desc[originalPage - p0] DO
        pd.gray := FALSE;
        IF pd.generation = Generation.Older THEN
          <* ASSERT pd.space = Space.Current *>
          Protect(originalPage, Mode.ReadOnly);
        END;
      END;
      IF perfOn THEN PerfChange(originalPage, 1); END;
    END;

    RETURN TRUE;
  END CopySome;

PROCEDURE CleanPage (p: Page;  thread_page := FALSE) =
  VAR hdr := PageToHeader(p);
  BEGIN
    IF desc[p - p0].protected THEN Protect(p, Mode.ReadWrite); END;
    CleanBetween(hdr, hdr + BytesPerPage);
    FOR i := 0 TO PageCount(p) - 1 DO
      desc[p + i - p0].gray := FALSE;
    END;
    IF NOT thread_page AND desc[p - p0].generation = Generation.Older THEN
      <* ASSERT desc[p - p0].space = Space.Current *>
      Protect(p, Mode.ReadOnly);
    END;
    IF perfOn THEN PerfChange(p, PageCount(p)); END;
  END CleanPage;

PROCEDURE CleanBetween (h, he: RefHeader) =
  BEGIN
    WHILE h < he DO
      <* ASSERT Word.And (LOOPHOLE (h, INTEGER), 3) = 0 *>
      <* ASSERT NOT h.forwarded *>
      h.marka := FALSE;
      h.markb := FALSE;
      RTHeapMap.WalkRef (h, mover);
      INC(h, ADRSIZE(Header) + ReferentSize(h));
    END;
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
    FillPool(pureCopy);  FillPool(impureCopy);

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
    FillPool(pureCopy);  FillPool(impureCopy);

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

(* Allocate space in the untraced heap *)

PROCEDURE AllocUntraced (size: INTEGER): ADDRESS =
  VAR res: ADDRESS;
  BEGIN
    RTOS.LockHeap();
      res := Cstdlib.malloc(size);
    RTOS.UnlockHeap();
    RETURN res;
  END AllocUntraced;

(* Allocate space in the traced heap for NEW or collector copies *)

PROCEDURE AllocTraced (dataSize, dataAlignment: CARDINAL;
                       VAR pool: AllocPool): RefReferent =
  (* Allocates space from "pool" in the traced heap. *)
  (* LL >= RTOS.LockHeap *)
  VAR
    res       : ADDRESS := pool.next + ADRSIZE(Header);
    cur_align : INTEGER := Word.And(LOOPHOLE(res, INTEGER), MaxAlignMask);
    alignment : INTEGER := align[cur_align, dataAlignment];
    nextPtr   : ADDRESS := res + (alignment + dataSize);
  BEGIN
    IF nextPtr > pool.limit THEN
      (* not enough space left in the pool, take the long route *)
      res := NIL;  nextPtr := NIL;  (* in case of GC during LongAlloc... *)
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
  END AllocTraced;

PROCEDURE LongAlloc (dataSize, dataAlignment: CARDINAL;
                     VAR pool: AllocPool): RefReferent =
  (* LL >= RTOS.HeapLock *)
  VAR
    n_bytes  := RTMisc.Upper(ADRSIZE(Header), dataAlignment) + dataSize;
    n_pages  := (n_bytes + BytesPerPage - 1) DIV BytesPerPage;
    res      : RefReferent;
    filePage : Page;
    newPage  : Page;
    newPtr   : ADDRESS;
    newLimit : ADDRESS;
  BEGIN
    (* make sure the collector gets a chance to keep up with NEW... *)
    IF pool.desc.note = Note.Allocated THEN CollectEnough (); END;

    (* get a block of "n_pages" contiguous, free pages; just what we need! *)
    newPage  := FindFreePages (n_pages, pool.notAfter);
    newPtr   := LOOPHOLE (newPage * AdrPerPage, ADDRESS);
    newLimit := LOOPHOLE (newPtr  + AdrPerPage, ADDRESS);
    IF (newPage = Nil) THEN RETURN NIL; END;

    (* maybe we have to insert a filler to align this thing *)
    res := RTMisc.Align(newPtr + ADRSIZE(Header), dataAlignment);
    InsertFiller(newPtr, res - ADRSIZE(Header) - newPtr);

    (* allocate the object from the new page *)
    newPtr := LOOPHOLE(res + dataSize, RefHeader);

    (* mark the new pages *)
    desc[newPage - p0] := pool.desc;
    IF n_pages = 1 THEN
      INC (pool.n_small);
    ELSE
      INC (pool.n_big, n_pages);
      pool.desc.continued := TRUE;
      FOR i := 1 TO n_pages - 1 DO
        desc[newPage + i - p0] := pool.desc;
      END;
      pool.desc.continued := FALSE;
    END;
    IF perfOn THEN PerfChange (newPage, n_pages); END;

    (* decide whether to use the new page or the current pool page
       for further allocations *)
    IF n_pages # 1 THEN
      (* file this page *)
      filePage := newPage;
    ELSIF newLimit - newPtr > pool.limit - pool.next THEN
      (* more space remains on the new page *)
      filePage := pool.page;
      IF pool.page # Nil THEN FillPool(pool); END;
      pool.next  := newPtr;
      pool.limit := newLimit;
      pool.page  := newPage;
    ELSE (* more space remains on the existing pool page *)
      filePage := newPage;
      InsertFiller(newPtr, newLimit - newPtr);
    END;

    IF filePage # Nil THEN
      desc[filePage - p0].link := pool.stack;
      pool.stack := filePage;
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
  BEGIN
    LOOP
      backgroundWaiting := TRUE; (* no locks, unfortunately *)
      WHILE collectorState = CollectorState.Zero DO RTOS.WaitHeap(); END;
      backgroundWaiting := FALSE;
      WHILE collectorState # CollectorState.Zero DO
        RTOS.LockHeap();
        BEGIN
          IF collectorState # CollectorState.Zero THEN
            CollectorOn();
            CollectSome();
            CollectorOff();
          END;
        END;
        RTOS.UnlockHeap();
        Thread.Pause(1.0d0);       (* one second *)
      END;
    END;
  END BackgroundThread;


(* --------------------------------------------------------- collector *)

PROCEDURE StartGC () =
  BEGIN
    StartCollection();
  END StartGC;

PROCEDURE FinishGC () =
  BEGIN
    FinishCollection();
  END FinishGC;

PROCEDURE Crash (): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    RTOS.LockHeap();        (* left incremented *)

    IF collectorState = CollectorState.Zero THEN
      (* no collection in progress *)
      collectorOn := TRUE;       (* left on *)
      result := TRUE;
    ELSIF NOT collectorOn THEN
      CollectorOn();             (* left on *)
      (* finish collection *)
      WHILE collectorState # CollectorState.Zero DO CollectSome(); END;
      result := TRUE;
    ELSE
      collectorOn := TRUE;       (* left on *)
      result := FALSE;
    END;

    (* unprotect all pages *)
    FOR p := p0 TO p1 - 1 DO
      IF desc[p - p0].protected THEN Protect(p, Mode.ReadWrite); END;
    END;

    RETURN result;
  END Crash;

(* --------------------------------------------------------- debugging *)

VAR
  protectedCheck, refCheck: RTHeapMap.Visitor;
  
PROCEDURE InstallSanityCheck () =
  BEGIN
    RegisterMonitor(
      NEW(MonitorClosure, before := SanityCheck, after := SanityCheck));
    IF (refCheck = NIL) THEN
      protectedCheck := NEW (RTHeapMap.Visitor,
                             apply := ProtectedOlderRefSanityCheck);
      refCheck := NEW (RTHeapMap.Visitor, apply := RefSanityCheck);
    END;
  END InstallSanityCheck;

(* SanityCheck checks the heap for correctness when no collection is in
   progress. *)

PROCEDURE SanityCheck (<*UNUSED*> self: MonitorClosure) =
  VAR p := p0;
  BEGIN
    WHILE p < p1 DO
      CASE desc[p - p0].space OF
      | Space.Unallocated => INC(p);
      | Space.Previous =>        <* ASSERT FALSE *>
      | Space.Current =>
          <* ASSERT NOT desc[p - p0].gray *>
          <* ASSERT NOT desc[p - p0].continued *>
          IF desc[p - p0].protected THEN
            <* ASSERT desc[p - p0].generation = Generation.Older *>
          END;
          (* visit the objects on the page *)
          VAR
            h  := PageToHeader(p);
            he := PageToHeader(p + 1);
          BEGIN
            IF p = newPool.page THEN he := newPool.next; END;
            WHILE h < he DO
              (* check the references in the object *)
              IF desc[p - p0].protected THEN
                RTHeapMap.WalkRef (h, protectedCheck);
              ELSE
                RTHeapMap.WalkRef (h, refCheck);
              END;
              INC(h, ADRSIZE(Header) + ReferentSize(h));
            END;
            IF h > he THEN
              <* ASSERT HeaderToPage(h - 1) = p + PageCount(p) - 1 *>
            ELSE
              <* ASSERT PageCount(p) = 1 *>
            END;
          END;
          VAR
            n := PageCount(p);
            d := desc[p - p0];
          BEGIN
            d.continued := TRUE;
            d.link := Nil;
            LOOP
              INC(p);
              DEC(n);
              IF n = 0 THEN EXIT; END;
              VAR dd := desc[p - p0];
              BEGIN
                dd.link := Nil;
                <* ASSERT dd = d *>
              END;
            END;
          END;
      | Space.Free =>
          <* ASSERT NOT desc[p - p0].continued *>
          INC(p);
      END;
    END;
    <* ASSERT p = p1 *>
  END SanityCheck;

PROCEDURE RefSanityCheck (<*UNUSED*>v: RTHeapMap.Visitor;  cp  : ADDRESS) =
  VAR ref := LOOPHOLE(cp, REF RefReferent)^;
  BEGIN
    IF ref # NIL THEN
      VAR
        p  := ReferentToPage(ref);
        h  := HeaderOf(ref);
        tc := h.typecode;
      BEGIN
        IF p0 <= p AND p < p1 THEN
          <* ASSERT desc[p - p0].space = Space.Current *>
          <* ASSERT NOT desc[p - p0].continued *>
          <* ASSERT (0 < tc AND tc <= RTType.MaxTypecode())
                      OR tc = Fill_1_type
                      OR tc = Fill_N_type *>
        ELSE
          (* the compiler generates Text.T that are not in the traced
             heap *)
          <* ASSERT tc = 1 *>
        END;
      END;
    END;
  END RefSanityCheck;

PROCEDURE ProtectedOlderRefSanityCheck (<*UNUSED*> v  : RTHeapMap.Visitor;
                                                   cp : ADDRESS) =
  VAR ref := LOOPHOLE(cp, REF RefReferent)^;
  BEGIN
    IF ref # NIL THEN
      VAR
        p  := ReferentToPage(ref);
        h  := HeaderOf(ref);
        tc := h.typecode;
      BEGIN
        IF p0 <= p AND p < p1 THEN
          <* ASSERT desc[p - p0].space = Space.Current *>
          <* ASSERT desc[p - p0].generation = Generation.Older *>
          <* ASSERT NOT desc[p - p0].continued *>
          <* ASSERT (0 < tc AND tc < RTType.MaxTypecode())
                      OR tc = Fill_1_type
                      OR tc = Fill_N_type *>
        ELSE
          (* the compiler generates Text.T that are not in the traced
             heap *)
          <* ASSERT tc = 1 *>
        END;
      END;
    END;
  END ProtectedOlderRefSanityCheck;

(* ----------------------------------------------------------------------- *)

PROCEDURE VisitAllRefs (v: RefVisitor) =
  VAR tc: Typecode;
  BEGIN
    TRY
      Disable();
      FOR p := p0 TO p1 - 1 DO
        IF desc[p - p0].space = Space.Current
             AND NOT desc[p - p0].continued THEN
          VAR
            h             := PageToHeader(p);
            he            := PageToHeader(p + 1);
            size: INTEGER;
          BEGIN
            WHILE h < he AND (p # newPool.page OR h < newPool.next) DO
              size := ReferentSize(h);
              tc := h.typecode;
              IF tc # Fill_1_type AND tc # Fill_N_type THEN
                IF NOT v.visit(
                         tc, LOOPHOLE(h + ADRSIZE(Header), REFANY), size) THEN
                  RETURN;
                END;
              END;
              INC(h, ADRSIZE(Header) + size);
            END;
          END;
        END;
      END;
    FINALLY
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
  VAR i, j: Page;
  BEGIN
    IF RTPerfTool.Start("showheap", perfW) THEN
      perfOn := TRUE;
      RTProcess.RegisterExitor(PerfStop);
      PerfGrow(p0, p1 - p0);

      i := p0;
      WHILE i # Nil AND i < p1 DO
        j := i + 1;
        WHILE j < p1 AND desc[j - p0].continued DO INC(j); END;
        IF desc[i - p0].space # Space.Free THEN PerfChange(i, j - i); END;
        i := j;
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

PROCEDURE PerfChange (first: Page; nb: CARDINAL) =
  VAR
    e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Change, first := first,
                       nb := nb, desc := desc[first - p0]};
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
    start           := FALSE;
    result: WeakRef;
  BEGIN
    <* ASSERT r # NIL *>
    RTOS.LockHeap();
    BEGIN
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
    END;
    RTOS.UnlockHeap();
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
    weakTable := newTable;
  END ExpandWeakTable;

(* This is WeakRef.ToRef, which inverts FromRef *)

PROCEDURE WeakRefToRef (READONLY t: WeakRef): REFANY =
  VAR ab: WeakRefAB;  r: REFANY := NIL;
  BEGIN
    LOOPHOLE (ab, WeakRef) := t;
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
            IF desc[p - p0].space = Space.Previous THEN
              CollectorOn();
              Move(NIL, ADR(entry.r));
              CollectorOff();
            END;
          END;
        END;
        r := LOOPHOLE(entry.r, REFANY);
      END;
    END;
    RTOS.UnlockHeap();
    RETURN r;
  END WeakRefToRef;

(* This is RTHeapRef.RegisterFinalCleanup, which registers final cleanup
   for a heap object. *)

PROCEDURE RegisterFinalCleanup (r: REFANY; p: PROCEDURE (r: REFANY)) =
  BEGIN
    <* ASSERT r # NIL *>
    <* ASSERT p # NIL *>
    RTOS.LockHeap();
    BEGIN
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
    END;
    RTOS.UnlockHeap();
  END RegisterFinalCleanup;

(* WeakCleaner waits for entries to be placed on the dead list, then cleans
   them up and puts them on the free list. *)

PROCEDURE WeakCleaner (<*UNUSED*> closure: Thread.Closure): REFANY =
  VAR
    i   : INTEGER;
    copy: WeakEntry;
  BEGIN
    LOOP
      (* get an entry to handle.  copy its contents, then put it on the
         free list. *)
      WHILE weakDead0 = -1 DO RTOS.WaitHeap(); END;
      RTOS.LockHeap();
      IF weakDead0 = -1 THEN
        RTOS.UnlockHeap();
      ELSE
        i := weakDead0;
        WITH entry = weakTable[i] DO
          <* ASSERT entry.t.a = -1 *>
          CollectorOn();
          Move(NIL, ADR(entry.r));
          CollectorOff();
          copy := entry;
          weakDead0 := entry.next;
          entry.next := weakFree0;
          weakFree0 := i;
        END;
        RTOS.UnlockHeap();
        (* call the registered procedure.  note that collections are
           allowed; the copy is kept on the stack so the object won't be
           freed during the call. *)
        IF copy.p # NIL THEN
          LOOPHOLE(copy.p, WeakRefCleanUpProc)(
            LOOPHOLE(WeakRefAB{a := i, b := copy.t.b}, WeakRef),
            LOOPHOLE(copy.r, REFANY));
        END;
        copy.r := NIL;           (* to help conservative collector *)
      END;
    END;
  END WeakCleaner;

(* ----------------------------------------------------------------------- *)

PROCEDURE FirstPage (p: Page): Page =
  VAR s: Space;
  BEGIN
    IF (desc[p-p0].continued) THEN
      s := desc[p - p0].space;
      WHILE desc[p - p0].continued DO DEC(p); END;
      <*ASSERT desc[p - p0].space = s *>
    END;
    RETURN p;
  END FirstPage;

PROCEDURE PageCount (p: Page): CARDINAL =
  VAR n := 0;
  BEGIN
    <* ASSERT NOT desc[p - p0].continued *>
    REPEAT INC(p); INC(n); UNTIL p >= p1 OR NOT desc[p - p0].continued;
    RETURN n;
  END PageCount;

(* ----------------------------------------------------------------------- *)

TYPE Mode = { ReadWrite, ReadOnly, NoAccess };

PROCEDURE Protect (p: Page;  m: Mode) =
  CONST
    Readable = ARRAY Mode OF BOOLEAN { TRUE, TRUE, FALSE };
    Writable = ARRAY Mode OF BOOLEAN { TRUE, FALSE, FALSE };
  VAR
    n_pages   := PageCount(p);
    protected := (m # Mode.ReadWrite);
  BEGIN
    <* ASSERT collectorOn OR (m = Mode.ReadWrite) *>
    <* ASSERT RTHeapDep.VM *>
    RTHeapDep.Protect(p, n_pages, Readable[m], Writable[m]);
    FOR i := 0 TO n_pages - 1 DO desc[p + i - p0].protected := protected; END;
    IF perfOn THEN PerfChange(p, n_pages); END;
  END Protect;

PROCEDURE Fault (addr: ADDRESS): BOOLEAN =
  VAR p : INTEGER := Word.RightShift (LOOPHOLE(addr, INTEGER), LogBytesPerPage);
  BEGIN
    <* ASSERT RTHeapDep.VM *>
    RTOS.LockHeap();

    (* remember the time spent handling faults *)
    cycleCost := cycleCost + RTHeapDep.VMFaultTime(); 

    IF (p < p0) OR (p1 <= p) THEN
      RTOS.UnlockHeap();
      RETURN FALSE; (* not in heap *)
    END;

    WITH pd = desc[p - p0] DO

      IF pd.space = Space.Unallocated THEN
        RTOS.UnlockHeap();
        RETURN FALSE;              (* not in heap *)
      END;

      IF NOT pd.protected THEN
        RTOS.UnlockHeap();
        RETURN TRUE;               (* was protected, but not any more *)
      END;

      <* ASSERT NOT pd.pure *>

      IF pd.gray THEN
        CollectorOn();
        IF p # impureCopy.page THEN
          CleanPage(FirstPage(p));
        ELSIF CopySome() THEN
          (* we cleaned the impureCopy page, but still have more to clean *)
          <* ASSERT NOT desc[p - p0].gray *>
        ELSIF desc[p - p0].gray THEN
          <* ASSERT p = impureCopy.page AND impureCopy.stack = Nil *>
          (* We just finished the collection!! *)
          FillPool(impureCopy);
          impureCopy.page  := Nil;
          impureCopy.stack := Nil;
          FOR i := 0 TO PageCount(p) - 1 DO
            desc[p + i - p0].gray := FALSE;
          END;
          IF desc[p - p0].generation = Generation.Older THEN
            <* ASSERT desc[p - p0].space = Space.Current *>
            Protect(p, Mode.ReadOnly);
          END;
          IF perfOn THEN PerfChange(p, 1); END;
        END;
        CollectorOff();
      ELSIF NOT pd.continued THEN
        <* ASSERT pd.generation = Generation.Older *>
        Protect(p, Mode.ReadWrite);
      ELSE
        p := FirstPage(p);
        <* ASSERT desc[p - p0].generation = Generation.Older *>
        Protect(p, Mode.ReadWrite);
      END;

    END; (* WITH *)

    RTOS.UnlockHeap();
    RETURN TRUE;        (* was protected, protection cleared *)
  END Fault;

(* ----------------------------------------------------------------------- *)

(* The inner-loop collector action is to pick a gray page and completely
   clean it (i.e., make its referents at least gray, so that the page
   becomes black).  The current gray page, "impureCopy.page" is
   distinguished; it's the page that newly gray objects are copied to.

   To improve locality of reference in the new space, we keep the set of
   gray pages as a stack.  This helps approximate a depth-first copy to
   newspace.  The current page is not a member of the stack, but will
   become one when it becomes full.  The current page is always the page
   that contains "newPool.next".

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

PROCEDURE FindFreePages (n: INTEGER; notAfter: Notes): Page =
  VAR p: Page;
  BEGIN
    IF collectorState = CollectorState.Zero THEN
      p := AllocateFreePagesFromBlock(n, Notes{}, TRUE);
      IF p # Nil THEN RETURN p; END;
    ELSE
      p := AllocateFreePagesFromBlock(n, notAfter, TRUE);
      IF p # Nil THEN RETURN p; END;
      p := AllocateFreePagesFromBlock(n, Notes{}, FALSE);
      IF p # Nil THEN RETURN p; END;
    END;
    IF NOT GrowHeap(n) THEN RETURN Nil; END;
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
                                      front   : BOOLEAN      ): Page =
  VAR
    p                   := free;
    prevP               := Nil;
    prevLength          := 0;
    length    : INTEGER;
  BEGIN
    LOOP
      IF p = Nil THEN RETURN Nil; END;
      length := FreeLength(p);
      IF length >= n
           AND NOT (p > p0 AND desc[(p - 1) - p0].space = Space.Current
                      AND desc[(p - 1) - p0].note IN notAfter) THEN
        EXIT;
      END;
      prevP := p;
      prevLength := length;
      p := desc[p - p0].link;
    END;
    IF length = n THEN
      IF prevP = Nil THEN
        free := desc[p - p0].link;
      ELSE
        desc[prevP - p0].link := desc[p - p0].link;
      END;
      RETURN p;
    ELSE
      VAR
        newP, fragP: Page;
        fragLength : CARDINAL;
      BEGIN
        IF front THEN
          newP := p;
          fragP := p + n;
        ELSE
          newP := p + length - n;
          fragP := p;
        END;
        fragLength := length - n;
        IF fragLength > prevLength THEN
          IF prevP = Nil THEN
            free := fragP;
          ELSE
            desc[prevP - p0].link := fragP;
          END;
          desc[fragP - p0].link := desc[p - p0].link;
        ELSE
          IF prevP = Nil THEN
            free := desc[p - p0].link;
          ELSE
            desc[prevP - p0].link := desc[p - p0].link;
          END;
          VAR
            pp     := free;
            prevPP := Nil;
          BEGIN
            LOOP
              IF pp = Nil THEN EXIT; END;
              VAR length := FreeLength(pp);
              BEGIN
                IF length > fragLength
                     OR (length = fragLength AND pp > fragP) THEN
                  EXIT;
                END;
              END;
              prevPP := pp;
              pp := desc[pp - p0].link;
            END;
            desc[fragP - p0].link := pp;
            IF prevPP = Nil THEN
              free := fragP;
            ELSE
              desc[prevPP - p0].link := fragP;
            END;
          END;
        END;
        RETURN newP;
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
      prevP     := Nil;
      prevSpace := Space.Unallocated;
    BEGIN
      (* link together the first pages of all free blocks *)
      FOR p := p0 TO p1 - 1 DO
        VAR space := desc[p - p0].space;
        BEGIN
          IF space = Space.Free AND prevSpace # Space.Free THEN
            IF prevP = Nil THEN
              free := p;
            ELSE
              desc[prevP - p0].link := p;
            END;
            prevP := p;
          END;
          prevSpace := space;
        END;
      END;
      IF prevP = Nil THEN
        free := Nil;
      ELSE
        desc[prevP - p0].link := Nil;
      END;
    END;
    (* sort them, using insertion sort *)
    VAR
      n     := 1;                (* smallest block size *)
      p     := free;             (* start of sublist we're examining *)
      prevP := Nil;              (* element before sublist *)
    BEGIN
      LOOP
        VAR
          excess     := Nil;
          prevExcess := Nil;
        BEGIN
          (* separate off blocks over "n" long into excess list *)
          WHILE p # Nil DO
            VAR length := FreeLength(p);
            BEGIN
              <* ASSERT length >= n *>
              IF length > n THEN
                IF prevExcess = Nil THEN
                  excess := p;
                ELSE
                  desc[prevExcess - p0].link := p;
                END;
                IF prevP = Nil THEN
                  free := desc[p - p0].link;
                ELSE
                  desc[prevP - p0].link := desc[p - p0].link;
                END;
                prevExcess := p;
              ELSE
                prevP := p;
              END;
            END;
            p := desc[p - p0].link;
          END;
          (* maybe done *)
          IF excess = Nil THEN EXIT; END;
          <* ASSERT prevExcess # Nil *>
          (* link longer blocks onto end *)
          IF prevP = Nil THEN
            free := excess;
          ELSE
            desc[prevP - p0].link := excess;
          END;
          desc[prevExcess - p0].link := Nil;
          p := excess;
        END;
        (* find smallest element size of remaining bocks *)
        n := LAST(CARDINAL);
        VAR pp := p;
        BEGIN
          REPEAT
            VAR length := FreeLength(pp);
            BEGIN
              IF length < n THEN n := length; END;
            END;
            pp := desc[pp - p0].link;
          UNTIL pp = Nil;
        END;
      END;
    END;
  END RebuildFreelist;

(* FreeLength returns the number of free pages starting at page p. *)

PROCEDURE FreeLength (p: Page): CARDINAL =
  BEGIN
    <* ASSERT desc[p - p0].space = Space.Free *>
    VAR pp := p + 1;
    BEGIN
      LOOP
        IF pp >= p1 THEN EXIT; END;
        IF desc[pp - p0].space # Space.Free THEN EXIT; END;
        INC(pp);
      END;
      RETURN pp - p;
    END;
  END FreeLength;

(* GrowHeap adds a block of at least "MinNewPages" free pages to the heap,
   and links it into the free list. *)

(* "MinNewBytes" is the minimum number of bytes by which to grow the heap.
   Setting it higher reduces the number of system calls; setting it lower
   keeps the heap a little smaller. *)

VAR fragment0, fragment1: ADDRESS := NIL;

CONST
  InitialBytes = 262144;         (* initial heap size is 256K *)
  MinNewBytes  = 262144;         (* grow the heap by at least 256K *)
  MinNewFactor = 0.2;            (* grow the heap by at least 20% *)

  InitialPages = (InitialBytes + BytesPerPage - 1) DIV BytesPerPage;
  MinNewPages  = (MinNewBytes  + BytesPerPage - 1) DIV BytesPerPage;

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
        RTIO.PutInt  (total_heap DIV 1000000);
        RTIO.PutText (".");
        RTIO.PutInt  ((total_heap MOD 1000000) DIV 100000);
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
                                        LogBytesPerPage;
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
      WITH newDesc = NEW(UNTRACED REF ARRAY OF Desc, newSideSpan) DO
        IF desc # NIL THEN
          FOR i := FIRST(desc^) TO LAST(desc^) DO
            newDesc[i + p0 - newP0] := desc[i];
          END;
          FOR i := p1 TO firstNewPage - 1 DO
            newDesc[i - newP0].space := Space.Unallocated;
          END;
          FOR i := lastNewPage + 1 TO p0 - 1 DO
            newDesc[i - newP0].space := Space.Unallocated;
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
        RTIO.PutInt  (span DIV 1000000);
        RTIO.PutText (".");
        RTIO.PutInt  ((span MOD 1000000) DIV 100000);
        RTIO.PutText ("M");
        RTIO.PutText ("   density: ");
        RTIO.PutInt  (density);
        RTIO.PutText ("%\n");
        RTIO.Flush ();
      END;
    END;
    FOR i := firstNewPage TO lastNewPage DO
      desc[i - p0].space := Space.Free;
    END;
    IF perfOn THEN
      PerfGrow(firstNewPage, lastNewPage - firstNewPage + 1);
    END;
    INC(allocatedPages, lastNewPage - firstNewPage + 1);
    RebuildFreelist();
    RETURN TRUE;
  END GrowHeap;

(*** INITIALIZATION ***)

CONST MaxAlignment  = 8;
CONST MaxAlignMask  = 2_0111;     (* bit mask to capture MaxAlignment *)
TYPE  MaxAlignRange = [0 .. MaxAlignment - 1];

VAR align: ARRAY MaxAlignRange, [1 .. MaxAlignment] OF CARDINAL;
(* align[i,j] == RTMisc.Align (i, j) - i *)

PROCEDURE Init () =
  BEGIN
    weakTable := NEW(UNTRACED REF ARRAY OF WeakEntry, 0);

    (* initialize the alignment array *)
    FOR i := FIRST(align) TO LAST(align) DO
      FOR j := FIRST(align[0]) TO LAST(align[0]) DO
        align[i, j] := RTMisc.Upper(i, j) - i;
      END;
    END;
  END Init;

BEGIN
  IF RTParams.IsPresent("nogc") THEN disableCount := 1; END;
  IF RTParams.IsPresent("novm") THEN disableVMCount := 1; END;
  IF RTParams.IsPresent("noincremental") THEN incremental := FALSE; END;
  IF RTParams.IsPresent("nogenerational") THEN generational := FALSE; END;
  IF RTParams.IsPresent("paranoidgc") THEN InstallSanityCheck(); END;
  IF RTParams.IsPresent("heapstats") THEN heap_stats := TRUE; END;
  PerfStart();
END RTCollector.
