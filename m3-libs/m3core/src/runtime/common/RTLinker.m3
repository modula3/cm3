(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Sat Nov 19 09:26:45 PST 1994 by kalsow     *)
(*      modified on Wed Jun  2 15:22:58 PDT 1993 by muller     *)

(* The linker generates an inital direct call to this module's
   main body.  All Modula-3 code reached from here. *)

UNSAFE MODULE RTLinker EXPORTS RTLinker, RTModule;

IMPORT Cstdlib, Cstring;
IMPORT RT0, RTParams, RTDebug, RTHeapRep, RTCollectorSRC;
IMPORT RTTypeSRC, RTSignal, RTThread, RTHeapInfo, RTLinkerX, 
       RTIO, Word;

VAR
  traceInit   := FALSE;
  init_done   := FALSE;
  n_modules   := 0;
  n_fixed     := 0;
  max_modules := 0;
  modules     : ADDRESS;  (* UNTRACED REF ARRAY [0..] OF RT0.ModulePtr; *)

PROCEDURE InitRuntime (p_argc: INTEGER;  p_argv, p_envp, p_instance: ADDRESS) =
  (*Note: This procedure is called BEFORE any modules are linked! *)
  BEGIN
    IF init_done THEN RETURN; END;
    init_done := TRUE;

    (* make sure we can at least reference our own interface variables! *)
    FixImports (RTLinkerX.RTLinker_M3 (0));

    (* expose the global environment *)
    argc := p_argc;
    argv := p_argv;
    envp := GetEnvironmentStrings (p_envp);
    instance := p_instance;

    (* PrintModule(RTLinkerX.RTHooks_M3(0)); *)

    (* initialize the rest of the modules we'll be calling *)
    AddUnit (RTLinkerX.RTLinker_I3);  (* myself! *)
    AddUnit (RTLinkerX.RT0_I3);
    AddUnit (RTLinkerX.RTSignal_I3);
    AddUnit (RTLinkerX.RTParams_I3);
    AddUnit (RTLinkerX.RTDebug_I3);
    AddUnit (RTLinkerX.RTError_I3);
    AddUnit (RTLinkerX.RTHeapRep_I3);
    AddUnit (RTLinkerX.RTThread_I3);
    AddUnit (RTLinkerX.RTHeapInfo_I3);
    AddUnit (RTLinkerX.RTIO_I3);
    AddUnit (RTLinkerX.RTCollectorSRC_I3);
    AddUnit (RTLinkerX.Word_I3);

    (* finally, initialize the runtime. *)
    RTSignal.InstallHandlers ();
    RTParams.Init ();
    RTThread.Init ();
    RTHeapRep.Init ();
    RTDebug.Init ();
    RTHeapInfo.Init ();
    IF RTParams.IsPresent("tracelinker") THEN
      traceInit := TRUE;
    END;

    AddUnit (RTLinkerX.RTDebug_M3);
    AddUnit (RTLinkerX.RTError_M3);
    AddUnit (RTLinkerX.RTType_M3);
    AddUnit (RTLinkerX.RTPacking_M3);
    AddUnit (RTLinkerX.RTTipe_M3);
    AddUnit (RTLinkerX.RTException_M3);
  END InitRuntime;

PROCEDURE FixImports (m: RT0.ModulePtr) =
  VAR imp: RT0.ImportPtr;
  BEGIN
    IF (m = NIL) THEN RETURN; END;
    TraceModule("FixImports: ", m);
    imp := m.imports;
    WHILE (imp # NIL) DO
      IF (imp.import = NIL) THEN  imp.import := imp.binder (0);  END;
      imp := imp.next;
    END;
  END FixImports;

(*-------------------------------------------------------- module linking ---*)

CONST
  LS_Initial = 0;
  LS_Linked  = 1;
  LS_TypesOK = 2;
  LS_Ready   = 3;
  LS_Stacked = 4;  (* LS_Stacked+n => init_stack[n] holds the init info *)

PROCEDURE AddUnitI (m: RT0.ModulePtr) =
  BEGIN
    IF (m = NIL) THEN RETURN END;
    TraceModule("AddUnitI: ", m);
    IF generational AND (Word.And(m.gc_flags, RT0.GC_gen) = 0) THEN
      generational := FALSE;
      IF RTCollectorSRC.generational THEN
        RTCollectorSRC.FinishCollection ();
      END;
    END;
    IF incremental AND (Word.And(m.gc_flags, RT0.GC_inc) = 0) THEN
      incremental := FALSE;
      IF RTCollectorSRC.incremental THEN
        RTCollectorSRC.FinishCollection ();
      END;
    END;
    IF (m.link_state = LS_Initial) THEN FindModules (m);  END;
    IF (m.link_state = LS_Linked)  THEN FixTypes ();      END;
    IF (m.link_state = LS_TypesOK) THEN RunMainBody (m);  END;
  END AddUnitI;

PROCEDURE AddUnit (b: RT0.Binder) =
  VAR m: RT0.ModulePtr;
  BEGIN
    IF (b = NIL) THEN RETURN END;
    m := b(0);
    IF (m = NIL) THEN RETURN END;
    AddUnitI(m);
  END AddUnit;

PROCEDURE AddUnitImports (b: RT0.Binder) =
  VAR m: RT0.ModulePtr;
      imp: RT0.ImportPtr;
  BEGIN
    IF (b = NIL) THEN RETURN END;
    m := b(0);
    IF (m = NIL) THEN RETURN END;
    TraceModule("AddUnitImports: ", m);
    imp := m.imports;
    WHILE (imp # NIL) DO
      IF (imp.import = NIL) THEN
        imp.import := imp.binder (0);
      END;
      AddUnitI(imp.import);
      imp := imp.next;
    END;
  END AddUnitImports;

(****
PROCEDURE DumpModules () =
  VAR
    mp : UNTRACED REF RT0.ModulePtr;
    imp: RT0.ImportPtr;
  BEGIN
    FOR i := 0 TO n_modules - 1 DO
      mp := modules + i * ADRSIZE (RT0.ModulePtr);
      IF (mp^ # NIL) THEN
        RTIO.PutText (" ");
        RTIO.PutString (mp^.file);
        imp := mp^.imports;
        WHILE (imp # NIL) DO
          IF (imp.import = NIL) THEN
            RTIO.PutText (" <<< ");
            EXIT;
          END;
          imp := imp.next;
        END;
        RTIO.Flush ();
      END;
    END;
    RTIO.PutText ("\r\n");
    RTIO.Flush();
  END DumpModules;
***)

PROCEDURE FindModules (m: RT0.ModulePtr) =
  VAR
    n  : INTEGER := n_modules;
    mp : UNTRACED REF RT0.ModulePtr;
    imp: RT0.ImportPtr;
  BEGIN
    TraceModule("FindModules: ", m);
    LinkModule (m);
    WHILE (n < n_modules) DO
      mp := modules + n * ADRSIZE (RT0.ModulePtr);
      imp := mp^.imports;
      WHILE (imp # NIL) DO
        IF (imp.import = NIL) THEN  imp.import := imp.binder (0);  END;
        LinkModule (imp.import);
        imp := imp.next;
      END;
      INC (n);
    END;
  END FindModules;

PROCEDURE LinkModule (m: RT0.ModulePtr) =
  VAR mp: UNTRACED REF RT0.ModulePtr;
  BEGIN
    IF (m # NIL) AND (m.link_state = LS_Initial) THEN
      TraceModuleAndImports("LinkModule: ", m);
      (* add this module to the list of known modules *)
      IF n_modules >= max_modules THEN ExpandModuleTable (); END;
      mp := modules + n_modules * ADRSIZE (RT0.ModulePtr);
      mp^ := m; INC (n_modules);
      m.link_state := LS_Linked;
    END;
  END LinkModule;

PROCEDURE ExpandModuleTable () =
  CONST InitialTableSize = 500;
  VAR new_mods: ADDRESS;  n_bytes: INTEGER;
  BEGIN
    IF (modules = NIL) THEN
      (* first time... *)
      max_modules := InitialTableSize;
      modules := Cstdlib.malloc (InitialTableSize * BYTESIZE (RT0.ModulePtr));
      IF (modules = NIL) THEN Cstdlib.abort (); END;
    ELSE
      n_bytes := max_modules * BYTESIZE (RT0.ModulePtr);
      new_mods := Cstdlib.malloc (n_bytes + n_bytes);
      IF (new_mods = NIL) THEN Cstdlib.abort (); END;
      EVAL Cstring.memcpy (new_mods, modules, n_bytes);
      Cstdlib.free (modules);
      modules := new_mods;
      INC (max_modules, max_modules);
    END;
  END ExpandModuleTable;

PROCEDURE FixTypes () =
  VAR
    mp: UNTRACED REF RT0.ModulePtr;
    start := n_fixed;
    stop  := n_modules - 1;
  BEGIN
    (* declare the modules' typecells & opaque types *)
    mp := modules + start * ADRSIZE (RT0.ModulePtr);
    FOR i := start TO stop DO
      IF (mp^ # NIL) AND (mp^.link_state = LS_Linked) THEN
        TraceModule("FixTypes: module types: ", mp^);
        DeclareModuleTypes (mp^);
      END;
      INC (mp, ADRSIZE (mp^));
    END;

    (* fix the modules' type links *)
    mp := modules + start * ADRSIZE (RT0.ModulePtr);
    FOR i := start TO stop DO
      IF (mp^ # NIL) AND (mp^.link_state = LS_Linked) THEN
        TraceModule("FixTypes: type links: ", mp^);
        ResolveTypeLinks (mp^);
      END;
      INC (mp, ADRSIZE (mp^));
    END;

    RTTypeSRC.FinishObjectTypes ();

    (* verify the partial revelations *)
    mp := modules + start * ADRSIZE (RT0.ModulePtr);
    FOR i := start TO stop DO
      IF (mp^ # NIL) AND (mp^.link_state = LS_Linked) THEN
        mp^.link_state := LS_TypesOK;
        TraceModule("FixTypes: verify: ", mp^);
        VerifyModuleTypes (mp^);
      END;
      INC (mp, ADRSIZE (mp^));
    END;

    (* remember that we're done with the types in these modules *)
    n_fixed := MAX (n_fixed, stop+1);
  END FixTypes;

PROCEDURE DeclareModuleTypes (m: RT0.ModulePtr) =
  VAR
    type  : RT0.TypeDefn;
    brand : RT0.BrandPtr;
    rev   : RT0.RevPtr;
    next  : ADDRESS;
  BEGIN
    (* register the typecells *)
    TraceModule("DeclareModuleTypes: ", m);
    type := m.type_cells;  m.type_cells := NIL;
    WHILE (type # NIL) DO
      next := type.next;  type.next := NIL;
      IF traceInit THEN
        TraceMsgS("  type ", type.name);
        TraceMsgI("    typecode ", type.typecode);
        TraceMsgI("    typeid   ", type.selfID);
        brand := type.brand_ptr;
        IF brand # NIL THEN
          TraceMsgC("    brand    ", ADR(brand.chars[0]), brand.length);
        END;
      END;
      RTTypeSRC.AddTypecell (type, m);
      type := next;
    END;

    (* Register the full revelations *)
    rev := m.full_rev;  m.full_rev := NIL;
    WHILE (rev # NIL) AND (rev.lhs_id # 0) DO
      RTTypeSRC.NoteFullRevelation (rev, m);
      INC (rev, ADRSIZE (rev^));
    END;
  END DeclareModuleTypes;

PROCEDURE ResolveTypeLinks (m: RT0.ModulePtr) =
  VAR tlink: RT0.TypeLinkPtr;  next: ADDRESS;
  BEGIN
    (* resolve the module's typecell pointers *)
    tlink := m.type_cell_ptrs;   m.type_cell_ptrs := NIL;
    WHILE (tlink # NIL) DO
      next := tlink.defn;  tlink.defn := NIL;
      RTTypeSRC.ResolveTypeLink (tlink.typecode, tlink, m);
      tlink := next;
    END;
  END ResolveTypeLinks;

PROCEDURE VerifyModuleTypes (m: RT0.ModulePtr) =
  VAR rev: RT0.RevPtr;
  BEGIN
    (* Register the partial revelations *)
    rev := m.partial_rev;  m.partial_rev := NIL;
    WHILE (rev # NIL) AND (rev.lhs_id # 0) DO
      RTTypeSRC.VerifyPartialRevelation (rev, m);
      INC (rev, ADRSIZE (rev^));
    END;
  END VerifyModuleTypes;

(******
PROCEDURE RunMainBody (m: RT0.ModulePtr) =
  VAR imp: RT0.ImportPtr;
  BEGIN
    IF (m = NIL) OR (m.link_state # LS_TypesOK) THEN RETURN END;
    m.link_state := LS_Ready;

    (* first, initialize its imports *)
    imp := m.imports;
    WHILE (imp # NIL) DO
      RunMainBody (imp.import);
      imp := imp.next;
    END;

    (* finally, run its main body *)
    IF (m.binder # NIL) THEN EVAL m.binder (1); END;
  END RunMainBody;
*****)

VAR
  max_init_stack := 0;
  init_depth     := 0;
  init_stack     : ADDRESS;  (* ARRAY ... OF InitDesc *)
TYPE
  InitPtr = UNTRACED REF InitDesc;
  InitDesc = RECORD
    module   : RT0.ModulePtr;
    low_link : INTEGER;
  END;

PROCEDURE RunMainBody (m: RT0.ModulePtr) =
  (* This procedure is adapted from the algorithm, SEARHC, given in
     "The Design and Analysis of Computer Algorithms" by Aho, Hopcroft,
     and Ullman for finding strongly connected components. *)
  VAR desc, desc2: InitPtr;  imp: RT0.ImportPtr;  m2: RT0.ModulePtr;
    desc_offset: INTEGER;
  BEGIN
    IF (m = NIL) THEN RETURN; END;
    TraceModuleAndImports("RunMainBody: ", m);
    IF (m.link_state = LS_Ready)   THEN RETURN (* already done. *) END;
    IF (m.link_state < LS_TypesOK) THEN RETURN (* not even prepped! *) END;

    IF (max_init_stack <= init_depth) THEN ExpandInitStack (); END;
    desc_offset := init_depth * ADRSIZE (InitDesc);
    desc := init_stack + desc_offset;
    desc.module := m;
    desc.low_link := init_depth;
    m.link_state := LS_Stacked + init_depth;  INC (init_depth);

    (* visit my imports *)
    imp := m.imports;
    WHILE (imp # NIL) DO
      m2 := imp.import;
      IF (m2 = NIL) OR (m2.link_state < LS_TypesOK) THEN
        (* m2 is a bogus import pointer, ignore it. *)
      ELSIF (m2.link_state = LS_Ready) THEN
        (* m2's main body has already been run. *)
      ELSIF (m2.link_state >= LS_Stacked) THEN
        (* m2 is already on the init stack *)
        desc := init_stack + desc_offset;
        desc2 := init_stack + (m2.link_state - LS_Stacked) * ADRSIZE (InitDesc);
        desc.low_link := MIN (desc.low_link, desc2.low_link);
        desc2.low_link := desc.low_link;
      ELSE
        RunMainBody (m2);
        IF (m2.link_state >= LS_Stacked) THEN
          desc := init_stack + desc_offset;
          desc2 := init_stack + (m2.link_state - LS_Stacked) * ADRSIZE (InitDesc);
          desc.low_link := MIN (desc.low_link, desc2.low_link);
        END;
      END;
      imp := imp.next;
    END;

    desc := init_stack + desc_offset;
    IF (m.link_state = LS_Stacked + desc.low_link) THEN
      (* "m" is the root of a strongly connected component *)
      (* => "pop" the component off the stack *)
      FOR i := init_depth-1 TO desc.low_link BY -1 DO
        desc2 := init_stack + i * ADRSIZE (InitDesc);
        m2 := desc2.module;
        m2.link_state := LS_Ready;
        IF (m2.binder # NIL) THEN 
          TraceModule("RunMainBody: exec: ", m2);
          EVAL m2.binder (1);
        END;
      END;
      desc := init_stack + desc_offset;
      init_depth := desc.low_link;
    END;
  END RunMainBody;

PROCEDURE ExpandInitStack () =
  CONST InitialStackSize = 200;
  VAR new_inits: ADDRESS;  n_bytes: INTEGER;
  BEGIN
    TraceMsgI("ExpandInitStack: ", max_init_stack);
    IF max_init_stack = 0 THEN
      (* first time... *)
      max_init_stack := InitialStackSize;
      init_stack := Cstdlib.malloc (InitialStackSize * BYTESIZE (InitDesc));
      IF (init_stack = NIL) THEN Cstdlib.abort (); END;
    ELSE
      n_bytes := max_init_stack * BYTESIZE (InitDesc);
      new_inits := Cstdlib.malloc (n_bytes + n_bytes);
      IF (new_inits = NIL) THEN Cstdlib.abort (); END;
      EVAL Cstring.memcpy (new_inits, init_stack, n_bytes);
      Cstdlib.free (init_stack);
      init_stack := new_inits;
      INC (max_init_stack, max_init_stack);
    END;
  END ExpandInitStack;

(*----------------------------------------------------------- RTModule ---*)

PROCEDURE Count (): CARDINAL =
  BEGIN
    RETURN n_modules;
  END Count;

PROCEDURE Get (m: CARDINAL): RT0.ModulePtr =
  VAR p : UNTRACED REF RT0.ModulePtr;
  BEGIN
    IF (m >= n_modules) THEN
      <*NOWARN*> EVAL VAL (-1, CARDINAL);  (* force a range fault *)
    END;
    p := modules + m * ADRSIZE (RT0.ModulePtr);
    RETURN p^;
  END Get;

PROCEDURE FromDataAddress (x: ADDRESS): RT0.ModulePtr =
  VAR
    p          : UNTRACED REF RT0.ModulePtr := modules;
    best       : RT0.ModulePtr := NIL;
    best_delta : INTEGER := LAST (INTEGER);
    cur_delta  : INTEGER;
  BEGIN
    FOR i := 0 TO n_modules-1 DO
      cur_delta := (x - p^);
      IF (cur_delta >= 0) AND (cur_delta < best_delta) THEN
        best := p^;   best_delta := cur_delta;
      END;
    END;
    RETURN best;
  END FromDataAddress;

(*------------------------------------------------------- trace support ---*)

PROCEDURE OutModuleName (m: RT0.ModulePtr) =
  BEGIN
    IF NOT traceInit THEN RETURN END;
    IF m = NIL THEN
      (* RTIO.PutText("NIL"); *)
      RETURN;
    END;
    IF m.file = NIL THEN
      RTIO.PutText("NIL");
    ELSE
      RTIO.PutString(m.file);
    END;
    RTIO.PutText("(");
    RTIO.PutInt(m.link_state);
    RTIO.PutText(")");
    RTIO.Flush();
  END OutModuleName;

PROCEDURE OutModuleImports (m: RT0.ModulePtr) =
  VAR imp: RT0.ImportPtr;
  BEGIN
    IF NOT traceInit THEN RETURN END;
    imp := m.imports;
    WHILE (imp # NIL) DO
      IF imp.import # NIL THEN
        RTIO.PutText("  ");
        OutModuleName(imp.import);
        RTIO.PutText("\r\n");
      END;
      imp := imp.next;
    END;
    RTIO.Flush();
  END OutModuleImports;

PROCEDURE OutModuleAndImports (m: RT0.ModulePtr) =
  BEGIN
    IF NOT traceInit THEN RETURN END;
    OutModuleName(m);
    RTIO.PutText("\r\n");
    OutModuleImports(m);
  END OutModuleAndImports;

PROCEDURE TraceModule(s: TEXT; m: RT0.ModulePtr) =
  BEGIN
    IF NOT traceInit THEN RETURN END;
    RTIO.PutText(s);
    OutModuleName(m);
    RTIO.PutText("\r\n");
    RTIO.Flush();
  END TraceModule;

PROCEDURE TraceModuleAndImports(s: TEXT; m: RT0.ModulePtr) =
  BEGIN
    IF NOT traceInit THEN RETURN END;
    RTIO.PutText(s);
    OutModuleAndImports(m);
  END TraceModuleAndImports;

<*UNUSED*> PROCEDURE TraceMsg(s: TEXT) =
  BEGIN
    IF NOT traceInit THEN RETURN END;
    RTIO.PutText(s);
    RTIO.PutText("\r\n");
    RTIO.Flush();
  END TraceMsg;

PROCEDURE TraceMsgI(s: TEXT; i: INTEGER) =
  BEGIN
    IF NOT traceInit THEN RETURN END;
    RTIO.PutText(s);
    RTIO.PutInt(i);
    RTIO.PutText("\r\n");
    RTIO.Flush();
  END TraceMsgI;

PROCEDURE TraceMsgS(s: TEXT; s2: RT0.String) =
  BEGIN
    IF NOT traceInit THEN RETURN END;
    RTIO.PutText(s);
    RTIO.PutString(s2);
    RTIO.PutText("\r\n");
    RTIO.Flush();
  END TraceMsgS;

PROCEDURE TraceMsgC(s: TEXT; a: ADDRESS; n: INTEGER) =
  BEGIN
    IF NOT traceInit THEN RETURN END;
    RTIO.PutText(s);
    RTIO.PutChars(a, n);
    RTIO.PutText("\r\n");
    RTIO.Flush();
  END TraceMsgC;

BEGIN
END RTLinker.
