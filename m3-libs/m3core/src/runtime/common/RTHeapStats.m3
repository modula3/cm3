(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri May  5 08:22:31 PDT 1995 by kalsow     *)

(* The code below makes the following NASTY assumption:
      ThreadF.ProcessStacks calls its argument twice for
      each thread -- the first time for the stack, the
      second time for its registers. *)

UNSAFE MODULE RTHeapStats;

IMPORT RT0, RT0u, RTCollector, RTModule, RTIO, RTHeapMap, RTHeapRep, RTMisc;
IMPORT RTOS, RTType, RTTypeSRC, RTProcedure, RTProcedureSRC, RTMachine; 
IMPORT RTStack, ThreadF, Word, Text;
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
  ThreadInfo = RECORD
    id          : INTEGER;
    stack_start : ADDRESS;
    stack_stop  : ADDRESS;
    reg_start   : ADDRESS;
    reg_stop    : ADDRESS;
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
    (* allocate space for the stats *)
    outerVisitor := NEW (RTHeapMap.Visitor, apply := Visit);
    innerVisitor := NEW (RTHeapMap.Visitor, apply := InnerVisit);
    rootVisitor  := NEW (RTHeapMap.Visitor, apply := VisitRoot);
    visit_stack  := NEW (UNTRACED REF VisitStack);
    map := NEW (UNTRACED REF ARRAY OF Word.T,
                 (RTHeapRep.p1 - RTHeapRep.p0) * MapWordsPerHeapPage);

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

    (* capture the heap limits *)
    heap_min  := LOOPHOLE (RTHeapRep.p0 * RTHeapRep.BytesPerPage, ADDRESS);
    heap_max  := LOOPHOLE (RTHeapRep.p1 * RTHeapRep.BytesPerPage, ADDRESS);

    PutText ("\nHEAP: ");
    PutAddr (heap_min);
    PutText (" .. ");
    PutAddr (heap_max);
    PutText (" => ");
    PutInt ((heap_max - heap_min) DIV MByte);
    PutText (".");
    PutInt ((heap_max - heap_min) * 10 DIV MByte MOD 10);
    PutText (" Mbytes\n");

    (* find the edge of the new space *)
    last_alloc := LOOPHOLE (NEW (REF INTEGER), ADDRESS);

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

PROCEDURE InnerVisit (<*UNUSED*> self: RTHeapMap.Visitor;  loc: ADDRESS) =
  CONST Mask = ADRSIZE (RT0.RefHeader) - 1; (* assume it's 2^k-1 for some k *)
  VAR ptr : UNTRACED REF ADDRESS := loc;
  VAR ref : ADDRESS := ptr^;
  VAR header: RTHeapMap.ObjectPtr;
  VAR cell, word, bit, mask, typecode: INTEGER;
  BEGIN
    header := ref - ADRSIZE (RT0.RefHeader);
    IF (heap_min <= ref) AND (ref < heap_max)
      AND (Word.And (LOOPHOLE(ref, INTEGER), Mask) = 0) THEN
      typecode := header.typecode;
      IF (0 < typecode) AND (typecode < RT0u.nTypes) THEN
        cell := (ref - heap_min) DIV MapGrain;
        word := cell DIV BITSIZE (Word.T);
        bit  := cell - word * BITSIZE (Word.T);
        mask := Word.LeftShift (1, bit);
        IF (Word.And (mask, map[word]) = 0) THEN
          (* this is a new ref... *)
          map[word] := Word.Or (mask, map[word]);
          INC (visit.n_objects);
          INC (visit.n_bytes, DataSize (header) + BYTESIZE (RT0.RefHeader));
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

PROCEDURE DataSize (h: RTHeapMap.ObjectPtr): CARDINAL =
  VAR
    res : INTEGER;
    tc  : RT0.Typecode := h.typecode;
    def : RT0.TypeDefn;
  BEGIN
    IF tc = RTHeapRep.Fill_1_type THEN RETURN 0; END;
    IF tc = RTHeapRep.Fill_N_type THEN
      res := LOOPHOLE(h + ADRSIZE(RT0.RefHeader), UNTRACED REF INTEGER)^;
      RETURN res - BYTESIZE(RT0.RefHeader);
    END;
    def := RTType.Get (tc);
    IF def.nDimensions = 0 THEN
      (* the typecell datasize tells the truth *)
      RETURN def.dataSize;
    END;
    (* ELSE, the referent is an open array; it has the following layout:
|         pointer to the elements (ADDRESS)
|         size 1
|         ....
|         size n
|         optional padding
|         elements
|         ....
       where n is the number of open dimensions (given by the definition)
       and each size is the number of elements along the dimension *)
    VAR
      sizes: UNTRACED REF INTEGER := h + ADRSIZE(RT0.RefHeader)
                                       + ADRSIZE(ADDRESS);  (* ^ elt pointer*)
    BEGIN
      res := 1;
      FOR i := 0 TO def.nDimensions - 1 DO
        res := res * sizes^;
        INC(sizes, ADRSIZE(sizes^));
      END;
      res := res * def.elementSize;
    END;
    res := RTMisc.Upper(res + def.dataSize, BYTESIZE(RT0.RefHeader));
    RETURN res;
  END DataSize;

PROCEDURE TypeName (ref: ADDRESS): TEXT =
  CONST Mask = ADRSIZE (RT0.RefHeader) - 1; (* assume it's 2^k-1 for some k *)
  VAR header: RTHeapMap.ObjectPtr;
  VAR typecode: INTEGER;
  BEGIN
    header := ref - ADRSIZE (RT0.RefHeader);
    IF (Word.And (LOOPHOLE (header, INTEGER), Mask) = 0) (* => aligned *)
      AND (heap_min <= ref) AND (ref < heap_max) THEN
      typecode := header.typecode;
      IF (0 < typecode) AND (typecode < RT0u.nTypes) THEN
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

VAR is_registers: BOOLEAN := FALSE;
    mark_addr: ADDRESS;

PROCEDURE GetThreads () =
  VAR i: INTEGER;
  BEGIN
    self_id := -1;
    mark_addr := ADR (i);
    ThreadF.ProcessStacks (GetThread);
    RTIO.PutText ("Threads: ");
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
        IF is_registers
          THEN z.reg_start   := start;  z.reg_stop   := stop;
          ELSE z.stack_start := start;  z.stack_stop := stop;
        END;
      END;
    END;
    IF (is_registers) THEN INC (n_threads); END;
    is_registers := NOT is_registers;
  END GetThread;

PROCEDURE GetThreadStats (READONLY ti: ThreadInfo) =
  BEGIN
    visit.module     := NIL;
    visit.thread_id  := ti.id;
    visit.location   := NIL;
    visit.ref        := NIL;

    ResetVisitCounts ();
    ScanPages (ti.stack_start, ti.stack_stop);
    ScanPages (ti.reg_start,   ti.reg_stop);
    AddVisit (stacks);
  END GetThreadStats;

PROCEDURE ScanPages (start, stop: ADDRESS) =
  VAR fp := start;  p: ADDRESS;  page: INTEGER;
  BEGIN
    (* scan the stack or registers *)
    WHILE fp <= stop DO
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF heap_min <= p AND p < heap_max THEN
        page := (p - heap_min) DIV RTHeapRep.BytesPerPage;
        IF RTHeapRep.desc[page].space = RTHeapRep.Space.Current THEN
          VisitPage (page);
        END;
      END;
      INC(fp, RTMachine.PointerAlignment);
    END;
  END ScanPages;

PROCEDURE GetThreadRootStats (READONLY ti: ThreadInfo) =
  BEGIN
    visit.module     := NIL;
    visit.thread_id  := ti.id;
    visit.location   := NIL;
    visit.ref        := NIL;
    ScanThreadRoots (ti.stack_start, ti.stack_stop, on_stack := TRUE);
    ScanThreadRoots (ti.reg_start,   ti.reg_stop,   on_stack := FALSE);
  END GetThreadRootStats;

PROCEDURE ScanThreadRoots (start, stop: ADDRESS;  on_stack: BOOLEAN) =
  VAR fp := start;  p: ADDRESS;  page: INTEGER;
  BEGIN
    WHILE fp <= stop DO
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF heap_min <= p AND p < heap_max THEN
        page := (p - heap_min) DIV RTHeapRep.BytesPerPage;
        IF RTHeapRep.desc[page].space = RTHeapRep.Space.Current THEN
          IF on_stack
            THEN visit.location := fp;
            ELSE visit.location := NIL;
          END;
          visit.ref := p;
          ResetVisitCounts ();
          Visit (NIL, fp);
          AddVisit (stack_roots);
        END;
      END;
      INC(fp, RTMachine.PointerAlignment);
    END;
  END ScanThreadRoots;

PROCEDURE GetThreadPageStats (READONLY ti: ThreadInfo) =
  BEGIN
    visit.module     := NIL;
    visit.thread_id  := ti.id;
    visit.location   := NIL;
    visit.ref        := NIL;
    ScanThreadPageRoots (ti.stack_start, ti.stack_stop, on_stack := TRUE);
    ScanThreadPageRoots (ti.reg_start,   ti.reg_stop,   on_stack := FALSE);
  END GetThreadPageStats;

PROCEDURE ScanThreadPageRoots (start, stop: ADDRESS;  on_stack: BOOLEAN) =
  VAR fp := start;  p: ADDRESS;  page: INTEGER;
  BEGIN
    WHILE fp <= stop DO
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF heap_min <= p AND p < heap_max THEN
        page := (p - heap_min) DIV RTHeapRep.BytesPerPage;
        IF RTHeapRep.desc[page].space = RTHeapRep.Space.Current THEN
          IF on_stack
            THEN visit.location := fp;
            ELSE visit.location := NIL;
          END;
          visit.ref := p;
          ResetVisitCounts ();
          VisitPage (page);
          AddVisit (stack_pages);
        END;
      END;
      INC(fp, RTMachine.PointerAlignment);
    END;
  END ScanThreadPageRoots;

PROCEDURE VisitPage (page: INTEGER) =
  VAR start, stop: ADDRESS;  h: RTHeapMap.ObjectPtr;  ref: ADDRESS;
  BEGIN
    (* find the address limits of this "page" *)
    WHILE (page > 0)
      AND (RTHeapRep.desc[page].space = RTHeapRep.Space.Current)
      AND (RTHeapRep.desc[page].continued) DO
      DEC (page);
    END;
    start := heap_min + page * RTHeapRep.BytesPerPage;

    (*****
    REPEAT
      INC (page);
    UNTIL (page >= RTHeapRep.p1-RTHeapRep.p0)
       OR (RTHeapRep.desc[page].space # RTHeapRep.Space.Current)
       OR (NOT RTHeapRep.desc[page].continued);
    stop := heap_min + page * RTHeapRep.BytesPerPage;
    *****
    ***** multi-page objects never share their pages
    *****   => only need to start scanning on their first page
    *****      - Michel Dagenais, 6/17/96
    ****)
    stop := start + RTHeapRep.BytesPerPage;

    IF (start <= last_alloc) AND (last_alloc < stop) THEN
      (* we're on the allocator's partial page... *)
      stop := last_alloc;
    END;

    (* visit each object on the page *)
    h := start;
    WHILE (h < stop) AND (h.typecode # 0) DO
      ref := h + ADRSIZE (RT0.RefHeader);
      Visit (NIL, ADR (ref));
      INC (h, DataSize (h) + ADRSIZE (RT0.RefHeader));
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
    fp, p: ADDRESS;
    page : INTEGER;
    cons_cnt, cons_bytes : INTEGER;
    opt_cnt, opt_bytes   : INTEGER;
    have_frames : BOOLEAN;
    cur, prev: RTStack.Frame;
    proc_start: RTProcedure.Proc;
    file, proc_name: RTProcedureSRC.Name;
  BEGIN
    have_frames := RTStack.Has_walker;
    IF RTStack.Has_walker THEN
      IF (ti.id = self_id) THEN
        RTStack.CurrentFrame (cur);
      ELSE
        RTStack.GetThreadFrame (cur, ti.reg_start, ti.reg_stop - ti.reg_start);
      END;
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
    WHILE (fp <= ti.stack_stop) DO

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

      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF heap_min <= p AND p < heap_max THEN
        page := (p - heap_min) DIV RTHeapRep.BytesPerPage;
        IF RTHeapRep.desc[page].space = RTHeapRep.Space.Current THEN
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

      INC (fp, RTMachine.PointerAlignment);
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
