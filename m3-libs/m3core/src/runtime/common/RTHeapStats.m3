(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri May  5 08:22:31 PDT 1995 by kalsow     *)

(* RTThread.ProcessStacks calls its argument once for each area of memory
   holding a thread's stack or saved registers.  How many areas a thread has
   depends on the thread package (on Linux, one; on Darwin, the stack plus,
   for a stopped thread, its registers), so each area is reported on its own
   as "T.n", rather than grouped into threads. *)

UNSAFE MODULE RTHeapStats;

IMPORT RT0, RTCollector, RTModule, RTIO, RTHeapMap, RTHeapRep, RTMisc;
IMPORT RTOS, RTType, RTTypeSRC, RTProcedure, RTProcedureSRC; 
IMPORT RTStack, RTThread, Word, Text;
FROM RTIO IMPORT PutInt, PutAddr, PutText;

TYPE
  Info = RECORD
    module    : RT0.ModulePtr;
    thread_id : INTEGER;
    location  : ADDRESS;
    ref       : ADDRESS;
    n_objects : INTEGER;
    n_bytes   : INTEGER;
  END;

TYPE
  InfoSet = RECORD
    count : INTEGER;
    info  : ARRAY [0..19] OF Info;
  END;

TYPE
  ThreadInfo = RECORD   (* one area of a thread's stack or registers *)
    id          : INTEGER;
    stack_start : ADDRESS;
    stack_stop  : ADDRESS;
    dump        : BOOLEAN;
  END;

TYPE
  VisitStack = ARRAY [0..10000] OF ADDRESS;

CONST
  MapGrain = 2 * BYTESIZE (RT0.RefHeader);  (* = 1 bit in the map *)
  MapBitsPerHeapPage = RTHeapRep.BytesPerPage DIV MapGrain;
  MapWordsPerHeapPage = MapBitsPerHeapPage DIV BITSIZE (Word.T);

VAR
  units       : InfoSet;
  unit_roots  : InfoSet;
  stacks      : InfoSet;
  stack_roots : InfoSet;
  stack_pages : InfoSet;
  map         : UNTRACED REF ARRAY OF Word.T;
  heap_min    : ADDRESS;
  heap_max    : ADDRESS;
  n_types     : INTEGER;
  visit       : Info;
  visit_stack : UNTRACED REF VisitStack;
  top_of_stack: INTEGER;
  n_overflows : INTEGER;
  last_alloc  : ADDRESS;
  outerVisitor: RTHeapMap.Visitor := NIL;
  innerVisitor: RTHeapMap.Visitor := NIL;
  rootVisitor : RTHeapMap.Visitor := NIL;
  self_id     : INTEGER;
  n_threads   : INTEGER;
  threads     : ARRAY [0..199] OF ThreadInfo;

PROCEDURE ReportReachable () =
  CONST MByte = 1024 * 1024;
  BEGIN
    (* initialize the globals *)
    units.count       := 0;
    unit_roots.count  := 0;
    stacks.count      := 0;
    stack_roots.count := 0;
    stack_pages.count := 0;
    top_of_stack      := 0;
    n_overflows       := 0;
    n_threads         := 0;

    (* freeze the world *)
    RTCollector.Disable ();
    RTOS.LockHeap (); (* freeze the heap *)

    (* Allocate space for the stats before stopping the other threads: the
       untraced NEWs and DISPOSEs call calloc and free, and a thread stopped
       inside malloc holds malloc's lock.  With the heap locked and the
       collector disabled the heap cannot change under us, so the map,
       allocated last, still covers [p0 .. p1) below. *)
    outerVisitor := NEW (RTHeapMap.Visitor, apply := Visit);
    innerVisitor := NEW (RTHeapMap.Visitor, apply := InnerVisit);
    rootVisitor  := NEW (RTHeapMap.Visitor, apply := VisitRoot);
    last_alloc   := LOOPHOLE (NEW (REF INTEGER), ADDRESS);
    visit_stack  := NEW (UNTRACED REF VisitStack);
    map := NEW (UNTRACED REF ARRAY OF Word.T,
                 (RTHeapRep.p1 - RTHeapRep.p0) * MapWordsPerHeapPage);

    (* DumpStack maps PCs to procedures, and the first FromPC call builds
       the procedure table with calloc; build it now. *)
    VAR proc: RTProcedure.Proc;  file, name: RTProcedureSRC.Name;
    BEGIN
      RTProcedureSRC.FromPC (NIL, proc, file, name);
    END;

    RTThread.SuspendOthers ();

    (* capture the heap limits *)
    heap_min  := LOOPHOLE (RTHeapRep.p0 * RTHeapRep.BytesPerPage, ADDRESS);
    heap_max  := LOOPHOLE (RTHeapRep.p1 * RTHeapRep.BytesPerPage, ADDRESS);
    n_types   := RTType.MaxTypecode () + 1;

    PutText ("\nHEAP: ");
    PutAddr (heap_min);
    PutText (" .. ");
    PutAddr (heap_max);
    PutText (" => ");
    PutInt ((heap_max - heap_min) DIV MByte);
    PutText (".");
    PutInt ((heap_max - heap_min) * 10 DIV MByte MOD 10);
    PutText (" Mbytes\n");

    (* capture the thread info *)
    GetThreads ();

    FOR i := 0 TO RTModule.Count() - 1 DO GetUnitStats (i); END;
    FOR i := 0 TO RTModule.Count() - 1 DO GetUnitRootStats (i); END;
    FOR i := 0 TO n_threads-1          DO GetThreadStats (threads[i]); END;
    FOR i := 0 TO n_threads-1          DO GetThreadRootStats (threads[i]); END;
    FOR i := 0 TO n_threads-1          DO GetThreadPageStats (threads[i]); END;

    IF (n_overflows > 0) THEN
      PutText ("  ** warning: ");
      PutInt (n_overflows);
      PutText (" paths, longer than ");
      PutInt (NUMBER (VisitStack));
      PutText (" REFs, were truncated.\n");
    END;

    ReportUnits ();
    ReportUnitRoots ();

    ReportStacks ();
    ReportStackRoots ();
    ReportStackPages ();

    DumpStacks ();
    RTIO.Flush ();

    (* thaw the world *)
    RTThread.ResumeOthers ();
    DISPOSE (visit_stack);
    DISPOSE (map);
    RTOS.UnlockHeap (); (* unfreeze the heap *)
    RTCollector.Enable ();
  END ReportReachable;

(*------------------------------------------------------------ REF visits ---*)

PROCEDURE ResetVisitCounts () =
  BEGIN
    visit.n_objects := 0;
    visit.n_bytes   := 0;
    top_of_stack    := 0;
    RTMisc.Zero (ADR (map[0]), BYTESIZE (map^));
  END ResetVisitCounts;

PROCEDURE AddVisit (VAR s: InfoSet) =
  VAR n: INTEGER;
  BEGIN
    (* if the set isn't full, make room for this visit *)
    IF (s.count < NUMBER (s.info)) THEN
      s.info[s.count].n_bytes := -1;
      INC (s.count);
    END;

    (* find where to insert this visit *)
    n := s.count-1;
    WHILE (n >= 0) AND (s.info[n].n_bytes < visit.n_bytes) DO
      IF (n < LAST(s.info)) THEN s.info[n+1] := s.info[n]; END;
      DEC (n);
    END;
    INC (n);

    (* insert the new root *)
    IF (n < s.count) THEN  s.info[n] := visit;  END;
  END AddVisit;

PROCEDURE Visit (<*UNUSED*> self: RTHeapMap.Visitor;  loc: ADDRESS) =
  BEGIN
    InnerVisit (NIL, loc);
    WHILE (top_of_stack > 0) DO
      DEC (top_of_stack);
      RTHeapMap.WalkRef (visit_stack[top_of_stack], innerVisitor);
    END;
  END Visit;

(* TRUE if the object header at "h" is on an allocated page of the current
   space, so that reading it is safe and it may be an object.  The range
   [heap_min .. heap_max) is not enough: it spans everything from the first
   heap page to the last, including the gaps between separately mapped heap
   chunks, which may be unmapped. *)
PROCEDURE InHeap (h: ADDRESS): BOOLEAN =
  VAR page := RTHeapRep.AddressToPage (h);
  BEGIN
    RETURN page # NIL AND page.desc.space = RTHeapRep.Space.Current;
  END InHeap;

PROCEDURE InnerVisit (<*UNUSED*> self: RTHeapMap.Visitor;  loc: ADDRESS) =
  CONST Mask = ADRSIZE (RT0.RefHeader) - 1; (* assume it's 2^k-1 for some k *)
  VAR ptr : UNTRACED REF ADDRESS := loc;
  VAR ref : ADDRESS := ptr^;
  VAR header: RTHeapMap.ObjectPtr;
  VAR cell, word, bit, mask, typecode: INTEGER;
  BEGIN
    header := ref - ADRSIZE (RT0.RefHeader);
    IF (heap_min <= ref) AND (ref < heap_max)
      AND (Word.And (LOOPHOLE(ref, INTEGER), Mask) = 0)
      AND InHeap (header) THEN
      typecode := RT0.GetTypecode (header);
      IF (0 < typecode) AND (typecode < n_types) AND
         (typecode # RT0.TextLitTypecode) THEN
        cell := (ref - heap_min) DIV MapGrain;
        word := cell DIV BITSIZE (Word.T);
        bit  := cell - word * BITSIZE (Word.T);
        mask := Word.LeftShift (1, bit);
        IF (Word.And (mask, map[word]) = 0) THEN
          (* this is a new ref... *)
          map[word] := Word.Or (mask, map[word]);
          INC (visit.n_objects);
          INC (visit.n_bytes,
               RTHeapRep.ReferentSize(header) + BYTESIZE (RT0.RefHeader));
          IF (top_of_stack < NUMBER (VisitStack)) THEN
            visit_stack [top_of_stack] := header;
            INC (top_of_stack);
          ELSE
            INC (n_overflows);
          END;
        END;
      END;
    END;
  END InnerVisit;

PROCEDURE TypeName (ref: ADDRESS): TEXT =
  CONST Mask = ADRSIZE (RT0.RefHeader) - 1; (* assume it's 2^k-1 for some k *)
  VAR header: RTHeapMap.ObjectPtr;
  VAR typecode: INTEGER;
  BEGIN
    header := ref - ADRSIZE (RT0.RefHeader);
    IF (Word.And (LOOPHOLE (header, INTEGER), Mask) = 0) (* => aligned *)
      AND (heap_min <= ref) AND (ref < heap_max) AND InHeap (header) THEN
      typecode := RT0.GetTypecode (header);
      IF (0 < typecode) AND (typecode <= n_types) THEN
        RETURN RTTypeSRC.TypecodeName (typecode);
      END;
    END;
    RETURN "?";
  END TypeName;

(*----------------------------------------------------------------- units ---*)

PROCEDURE GetUnitStats (n: CARDINAL) =
  BEGIN
    visit.module     := RTModule.Get (n);
    visit.thread_id  := -1;
    visit.location   := NIL;
    visit.ref        := NIL;
    ResetVisitCounts ();
    RTHeapMap.WalkModuleGlobals (outerVisitor, n);
    AddVisit (units);
  END GetUnitStats;

PROCEDURE GetUnitRootStats (n: CARDINAL) =
  BEGIN
    visit.module     := RTModule.Get (n);
    visit.thread_id  := -1;
    visit.location   := NIL;
    visit.ref        := NIL;
    RTHeapMap.WalkModuleGlobals (rootVisitor, n);
  END GetUnitRootStats;

PROCEDURE VisitRoot (<*UNUSED*> self: RTHeapMap.Visitor;  root: ADDRESS) =
  VAR p: UNTRACED REF ADDRESS := root;
  BEGIN
    visit.location := root;
    visit.ref      := p^;
    ResetVisitCounts ();
    Visit (NIL, root);
    AddVisit (unit_roots);
  END VisitRoot;

(*--------------------------------------------------------------- threads ---*)

VAR mark_addr: ADDRESS;

PROCEDURE GetThreads () =
  VAR i: INTEGER;
  BEGIN
    self_id := -1;
    mark_addr := ADR (i);
    RTThread.ProcessStacks (GetThread);
    RTIO.PutText ("Thread stack and register areas: ");
    RTIO.PutInt (n_threads);
    IF (n_threads > NUMBER (threads)) THEN
      RTIO.PutText ("  (");
      RTIO.PutInt (n_threads - NUMBER (threads));
      RTIO.PutText (" ignored)");
      n_threads := NUMBER (threads);
    END;
    RTIO.PutChar ('\n');
  END GetThreads;

PROCEDURE GetThread (start, stop: ADDRESS) =
  BEGIN
    IF (start <= mark_addr) AND (mark_addr <= stop) THEN
      self_id := n_threads;
    END;
    IF (n_threads < NUMBER (threads)) THEN
      WITH z = threads[n_threads] DO
        z.id := n_threads;
        z.dump := FALSE;
        z.stack_start := start;
        z.stack_stop  := stop;
      END;
    END;
    INC (n_threads);
  END GetThread;

PROCEDURE GetThreadStats (READONLY ti: ThreadInfo) =
  BEGIN
    visit.module     := NIL;
    visit.thread_id  := ti.id;
    visit.location   := NIL;
    visit.ref        := NIL;

    ResetVisitCounts ();
    ScanPages (ti.stack_start, ti.stack_stop);
    AddVisit (stacks);
  END GetThreadStats;

PROCEDURE LastWord (start, stop: ADDRESS): ADDRESS =
  (* The address of the last whole word in [start .. stop), the range a
     thread's stack or registers occupy; below "start" if there is none.
     As in RTCollector.NoteStackLocations, so the scans never read past
     "stop". *)
  BEGIN
    IF stop - start < ADRSIZE (ADDRESS) THEN RETURN start - 1; END;
    RETURN stop - ADRSIZE (ADDRESS);
  END LastWord;

PROCEDURE ObjectContaining (page: RTHeapRep.RefPage;  a: ADDRESS): ADDRESS =
  (* The REF of the object on the block starting at "page" whose header or
     body contains "a", or NIL if "a" lies in no object there (in the page
     header, say).  Walks the block from its start, as VisitPage does. *)
  VAR
    h : RTHeapRep.RefHeader := page + ADRSIZE(RTHeapRep.PageHdr);
    he: RTHeapRep.RefHeader := page + page.nb * RTHeapRep.BytesPerPage;
    next: RTHeapRep.RefHeader;
  BEGIN
    IF a < h OR a >= he THEN RETURN NIL; END;
    WHILE h < he DO
      next := h + ADRSIZE(RTHeapRep.Header) + RTHeapRep.ReferentSize (h);
      IF a < next THEN RETURN h + ADRSIZE(RTHeapRep.Header); END;
      h := next;
    END;
    RETURN NIL;
  END ObjectContaining;

PROCEDURE ScanPages (start, stop: ADDRESS) =
  VAR fp: UNTRACED REF ADDRESS := start;
  VAR last := LastWord (start, stop);
  BEGIN
    (* scan the stack or registers *)
    WHILE fp <= last DO
      WITH page = RTHeapRep.AddressToPage(fp^), d = page.desc DO
        IF page # NIL AND d.space = RTHeapRep.Space.Current THEN
          VisitPage(page);
        END;
      END;
      INC(fp, RTThread.PointerAlignment);
    END;
  END ScanPages;

PROCEDURE GetThreadRootStats (READONLY ti: ThreadInfo) =
  BEGIN
    visit.module     := NIL;
    visit.thread_id  := ti.id;
    visit.location   := NIL;
    visit.ref        := NIL;
    ScanThreadRoots (ti.stack_start, ti.stack_stop);
  END GetThreadRootStats;

PROCEDURE ScanThreadRoots (start, stop: ADDRESS) =
  VAR fp: UNTRACED REF ADDRESS := start;
  VAR last := LastWord (start, stop);
  VAR ref: ADDRESS;
  BEGIN
    WHILE fp <= last DO
      WITH page = RTHeapRep.AddressToPage(fp^), d = page.desc DO
        IF page # NIL AND d.space = RTHeapRep.Space.Current THEN
          (* A stack or register word is only an ambiguous root: it may
             point into the middle of an object, or at no object at all.
             Visit the object that contains it, if there is one, rather
             than treating the word as the object's REF. *)
          ref := ObjectContaining (page, fp^);
          IF ref # NIL THEN
            visit.location := fp;
            visit.ref := ref;
            ResetVisitCounts ();
            Visit (NIL, ADR (ref));
            AddVisit (stack_roots);
          END;
        END;
      END;
      INC(fp, RTThread.PointerAlignment);
    END;
  END ScanThreadRoots;

PROCEDURE GetThreadPageStats (READONLY ti: ThreadInfo) =
  BEGIN
    visit.module     := NIL;
    visit.thread_id  := ti.id;
    visit.location   := NIL;
    visit.ref        := NIL;
    ScanThreadPageRoots (ti.stack_start, ti.stack_stop);
  END GetThreadPageStats;

PROCEDURE ScanThreadPageRoots (start, stop: ADDRESS) =
  VAR fp: UNTRACED REF ADDRESS := start;
  VAR last := LastWord (start, stop);
  BEGIN
    WHILE fp <= last DO
      WITH page = RTHeapRep.AddressToPage(fp^), d = page.desc DO
        IF page # NIL AND d.space = RTHeapRep.Space.Current THEN
          visit.location := fp;
          visit.ref := fp^;
          ResetVisitCounts ();
          VisitPage (page);
          AddVisit (stack_pages);
        END;
      END;
      INC(fp, RTThread.PointerAlignment);
    END;
  END ScanThreadPageRoots;

PROCEDURE VisitPage (page: RTHeapRep.RefPage) =
  VAR
    h : RTHeapRep.RefHeader := page + ADRSIZE(RTHeapRep.PageHdr);
    he: RTHeapRep.RefHeader := page + RTHeapRep.BytesPerPage;
    ref: ADDRESS;
  BEGIN
    (* visit each object on the page *)
    WHILE h < he DO
      ref := h + ADRSIZE (RTHeapRep.Header);
      Visit (NIL, ADR (ref));
      INC (h, ADRSIZE(RTHeapRep.Header) + RTHeapRep.ReferentSize (h));
    END;
  END VisitPage;

(*--------------------------------------------------------------- reports ---*)

PROCEDURE ReportUnits () =
  BEGIN
    PutText ("\nModule globals:\n");
    PutText (" # objects   # bytes  unit\n");
    PutText (" ---------  --------  -----------------\n");
    FOR i := 0 TO units.count-1 DO
      WITH m = units.info[i] DO
        IF (m.n_bytes > 0) THEN
          PutInt (m.n_objects, 10);
          PutInt (m.n_bytes, 10);
          PutText ("  ");
          PutStr  (PathTail (m.module.file));
          PutText ("\n");
        END;
      END;
    END;
  END ReportUnits;

PROCEDURE ReportUnitRoots () =
  BEGIN
    PutText ("\nGlobal variable roots:\n");
    PutText (" # objects   # bytes         ref type                location\n");
    PutText (" ---------  --------  ---------- -----------------   ------------------------\n");
    FOR i := 0 TO unit_roots.count-1 DO
      WITH r = unit_roots.info[i] DO
        IF (r.n_bytes > 0) THEN
          PutInt  (r.n_objects, 10);
          PutInt  (r.n_bytes, 10);
          PutText ("  ");
          PutAddr (r.ref);
          PutText (" ");
          PadText (TypeName (r.ref), 18);
          PutText ("  ");
          PutStr  (PathTail (r.module.file));
          PutText (" + ");
          PutInt (r.location - r.module);
          PutText ("\n");
        END;
      END;
    END;
  END ReportUnitRoots;

PROCEDURE ReportStacks () =
  BEGIN
    PutText ("\nThread stacks (conservative page scan):\n");
    PutText (" # objects   # bytes  thread  [stack bounds]\n");
    PutText (" ---------  --------  -------------------------------\n");
    FOR i := 0 TO stacks.count-1 DO
      WITH t = stacks.info[i] DO
        IF (t.n_bytes > 0) THEN
          PutInt (t.n_objects, 10);
          PutInt (t.n_bytes, 10);
          PutText ("  T.");
          PutInt (t.thread_id, 1);
          PutText ("  [");
          PutAddr (threads[t.thread_id].stack_start);
          PutText (" .. ");
          PutAddr (threads[t.thread_id].stack_stop);
          PutText ("]\n");
          threads[t.thread_id].dump := TRUE;
        END;
      END;
    END;
  END ReportStacks;

PROCEDURE ReportStackRoots () =
  BEGIN
    PutText ("\nThread stack roots (optimistic):\n");
    ReportStackInfo (stack_roots);
  END ReportStackRoots;

PROCEDURE ReportStackPages () =
  BEGIN
    PutText ("\nThread stack roots (conservative page scan):\n");
    ReportStackInfo (stack_pages);
  END ReportStackPages;

PROCEDURE ReportStackInfo (READONLY s: InfoSet) =
  BEGIN
    PutText (" # objects   # bytes         ref type                location\n");
    PutText (" ---------  --------  ---------- -----------------   ------------------------\n");
    FOR i := 0 TO s.count-1 DO
      WITH r = s.info[i] DO
        IF (r.n_bytes > 0) THEN
          PutInt  (r.n_objects, 10);
          PutInt  (r.n_bytes, 10);
          PutText ("  ");
          PutAddr (r.ref);
          PutText (" ");
          PadText (TypeName (r.ref), 18);
          PutText ("  ");
          IF (r.location # NIL) THEN
            PutText ("sp+");
            PutInt  (r.location - threads[r.thread_id].stack_start);
          ELSE
            PutText ("register");
          END;
          PutText (" in T.");
          PutInt  (r.thread_id);
          PutText ("\n");
          threads[r.thread_id].dump := TRUE;
        END;
      END;
    END;
  END ReportStackInfo;

(*----------------------------------------------------------- stack dumps ---*)

VAR
  conservative_cutoff : INTEGER;
  optimistic_cutoff   : INTEGER;

PROCEDURE DumpStacks () =
  BEGIN
    conservative_cutoff := MAX (MinInfoBytes (stack_pages) DIV 2, 1024);
    optimistic_cutoff   := MAX (MinInfoBytes (stack_roots) DIV 2, 1024);
    PutText ("\n-------------------------------------------------------\n");
    PutText ("Thread stack dumps with references that reach\nat least:\n");
    PutInt  (optimistic_cutoff, 10);
    PutText (" bytes under the optimistic scan or\n");
    PutInt  (conservative_cutoff, 10);
    PutText (" bytes under the conservative scan.\n\n");

    FOR i := 0 TO n_threads-1 DO
      IF threads[i].dump THEN DumpStack (threads[i]); END;
    END;
  END DumpStacks;

PROCEDURE MinInfoBytes (READONLY s: InfoSet): INTEGER =
  VAR x := LAST (INTEGER);
  BEGIN
    FOR i := 0 TO s.count-1 DO
      x := MIN (x, s.info[i].n_bytes);
    END;
    RETURN x;
  END MinInfoBytes;

PROCEDURE DumpStack (READONLY ti: ThreadInfo) =
  CONST Max_proc = 4096;  (* good enough for 99% of the procedures *)
  VAR
    fp: UNTRACED REF ADDRESS; p, last: ADDRESS;
    cons_cnt, cons_bytes : INTEGER;
    opt_cnt, opt_bytes   : INTEGER;
    have_frames : BOOLEAN;
    cur, prev: RTStack.Frame;
    proc_start: RTProcedure.Proc;
    file, proc_name: RTProcedureSRC.Name;
  BEGIN
    (* Frames are known only for our own stack: no thread package records
       a stopped thread's registers in a form RTStack.GetThreadFrame takes,
       and an area here may not hold registers at all. *)
    have_frames := (ti.id = self_id) AND RTStack.Has_walker ();
    IF have_frames THEN
      RTStack.CurrentFrame (cur);
      IF cur.pc = NIL THEN have_frames := FALSE; END;
    END;

    PutText ("-------------------------------------------------\n");
    PutText ("Thread T.");
    PutInt  (ti.id, 1);
    PutText ("  stack [");
    PutAddr (ti.stack_start);
    PutText (" .. ");
    PutAddr (ti.stack_stop);
    PutText ("]\n\n");
    PutText (" stack    optimistic    conservative\n");
    PutText ("offset  #objs # bytes  #objs # bytes  ref\n");
    PutText ("------  ----- -------  ----- -------  ------------\n");
    fp := ti.stack_start;
    last := LastWord (ti.stack_start, ti.stack_stop);
    WHILE (fp <= last) DO

      IF have_frames THEN
        WHILE (cur.sp <= fp) AND (cur.pc # NIL) DO
          RTProcedureSRC.FromPC (cur.pc, proc_start, file, proc_name);
          IF (proc_start # NIL) AND (cur.pc - proc_start < Max_proc) THEN
            PutInt  (cur.sp - ti.stack_start, 5);
            PutText ("                                 --> ");
            PutStr  (proc_name);
            IF (cur.pc # proc_start) THEN
              PutText (" + ");
              PutInt (cur.pc - proc_start);
            END;
            IF (file # NIL) THEN
              PutText (" in ");
              PutStr (PathTail (file));
            END;
            PutText ("\n");
          END;
          RTStack.PreviousFrame (cur, prev);
          cur := prev;
        END;
      END;

      p := fp^;
      WITH page = RTHeapRep.AddressToPage(p), d = page.desc DO
        IF page # NIL AND d.space = RTHeapRep.Space.Current THEN
          IF d.space = RTHeapRep.Space.Current THEN
            visit.location := fp;
            visit.ref := p;

            (* make the conservative scan *)
            ResetVisitCounts ();
            VisitPage (page);
            cons_bytes := visit.n_bytes;
            cons_cnt   := visit.n_objects;

            (* make the optimistic scan *)
            ResetVisitCounts ();
            Visit (NIL, fp);
            opt_bytes := visit.n_bytes;
            opt_cnt   := visit.n_objects;

            IF (cons_bytes >= conservative_cutoff)
              OR (opt_bytes >= optimistic_cutoff) THEN
              (* report this ref! *)
              PutInt  (fp - ti.stack_start, 5);
              PutInt  (opt_cnt, 8);
              PutInt  (opt_bytes, 8);
              PutInt  (cons_cnt, 7);
              PutInt  (cons_bytes, 8);
              PutText ("  ");
              PutAddr (p);
              PutText (" ");
              PutText (TypeName (p));
              PutText ("\n");
            END;
          END;
        END;
      END;

      IF NOT have_frames THEN
        (* maybe this stack element is a PC we should print *)
        RTProcedureSRC.FromPC (p, proc_start, file, proc_name);
        IF (proc_start # NIL) AND (p - proc_start < Max_proc) THEN
          PutInt  (fp - ti.stack_start, 5);
          PutText ("                                 [PC ");
          PutAddr (p);
          PutText ("] ");
          PutStr  (proc_name);
          IF (p # proc_start) THEN
            PutText (" + ");
            PutInt (p - proc_start);
          END;
          IF (file # NIL) THEN
            PutText (" in ");
            PutStr (PathTail (file));
          END;
          PutText ("\n");
        END;
      END;

      INC (fp, RTThread.PointerAlignment);
    END;
  END DumpStack;

     
(*--------------------------------------------------------- low-level I/O ---*)

PROCEDURE PathTail (a: ADDRESS): ADDRESS =
  VAR p0 : UNTRACED REF CHAR := a;  p := p0;
  BEGIN
    IF (p0 = NIL) THEN RETURN NIL END;
    WHILE (p^ # '\000') DO
      IF (p^ = '/') THEN p0 := p + ADRSIZE (p^); END;
      INC (p, ADRSIZE (p^));
    END;
    RETURN p0;
  END PathTail;

PROCEDURE PutStr (s: ADDRESS) =
  BEGIN
    IF (s = NIL) THEN RETURN END;
    RTIO.PutString (s);
  END PutStr;

PROCEDURE PadText (t: TEXT;  width := 0) =
  VAR len := Text.Length (t);
  BEGIN
    RTIO.PutText (t);
    WHILE (len < width) DO
      RTIO.PutChar (' ');
      INC (len);
    END;
  END PadText;

BEGIN
END RTHeapStats.
