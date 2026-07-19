(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Variable.m3                                           *)
(* Last Modified On Tue Jun 20 09:58:08 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 12:45:02 PDT 1995 By ericv      *)
(*      Modified On Thu Dec  5 17:21:40 PST 1991 By muller     *)

MODULE Variable;
(* Including formal parameters. *)

IMPORT M3, M3ID, CG, Value, ValueRep, Error, RunTyme;
IMPORT Scope, AssignStmt, Formal, M3RT, M3String;
IMPORT Target, TInt, Token, Ident, Module, CallExpr;
IMPORT Decl, Null, Int, LInt, Fmt, Procedure, Tracer;
IMPORT Expr, IntegerExpr, ArrayExpr, TextExpr, NamedExpr, RecordExpr;
IMPORT Type, OpenArrayType, ErrType, TipeMap, RecordType;
IMPORT RTIO, RTParams, MSIR, MSIRBuilder, MSIRType, MSIREmit;
IMPORT Text;
FROM Scanner IMPORT GetToken, Match, cur;

VAR debug := FALSE;

CONST
  Big_Local = 8192; (* x Target.Char.size *)
  Big_Param = 8;    (* x Target.Integer.size *)
  Max_zero_global = 64; (* x Target.Integer.size *)

REVEAL
  T = Value.T BRANDED "Variable.T" OBJECT
        type        : Type.T    := NIL; (* only written once, not lowered, to retain NamedType *)
        repType     : Type.T    := NIL;
        initExpr    : Expr.T    := NIL;
        qualName    : TEXT      := NIL;
        sibling     : T         := NIL;
        formal      : Value.T   := NIL;
        (* ^This Variable.T represents a formal parameter, but that's a
            Formal.T, a different and hidden proper subtype of Value.T.
            Field formal is actually the Formal.T object.
            A great example of TMIH (Too Much Information Hiding). *)
        alias       : T         := NIL;
        trace       : Tracer.T  := NIL;
        bounds      : BoundPair := NIL;
        cg_var      : CG.Var    := NIL; (* Used if it's a local, formal, or external. *)
        bss_var     : CG.Var    := NIL; (* Used if it's a global. *)
        nextTWACGVar : T; (* Link field for list of Variable.Ts that have a
                             non-NIL bss_var or cg_var. *)
        initValOffset : INTEGER := 0;
        offset      : INTEGER   := 0;
        size        : INTEGER   := 0;
        align       : AlignVal  := 0;
        cg_align    : AlignVal  := 0;
        (* 4 bits suffices, 8 bits provides endian neutral output of C++ backend *)
        mem_type    : BITS (*4*)8 FOR CG.Type := FIRST (CG.Type);
        stk_type    : BITS (*4*)8 FOR CG.Type := FIRST (CG.Type);
        indirect    : M3.Flag   := FALSE;
        open_ok     : M3.Flag   := FALSE;
        need_addr   : M3.Flag   := FALSE;
        no_type     : M3.Flag   := FALSE; (* Type not explicitly coded. *)
        global      : M3.Flag   := FALSE; (* Declared in outermost scope. *)
        initDone    : M3.Flag   := FALSE;
        initZero    : M3.Flag   := FALSE; (* Initial value is all binary zeros. *)
        initPending : M3.Flag   := FALSE; (* Initialization is postponed. *)
        msirInitDone: M3.Flag   := FALSE; (* MSIR init has been emitted. *)
        initStatic  : M3.Flag   := FALSE; (* Needs RT initialization to a value
                                             from the static constant area. *)
        allocated     : M3.Flag := FALSE;
          (* ^Has allocated space in the global variable area. *)
        initAllocated : M3.Flag := FALSE;
          (* ^Static initial value has allocated space in the global constant area. *)
      OVERRIDES
        typeCheck   := Check;
        set_globals := AllocGlobalVarSpace;
        load        := Load;
        declare     := Declare;
        const_init  := ConstInit;
        need_init   := NeedInit;
        lang_init   := LangInit;
        user_init   := UserInit;
        toExpr      := ValueRep.NoExpr;
        toType      := ValueRep.NoType;
        typeOf      := TypeOf;
        repTypeOf   := RepTypeOf;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := TypeOf;
      END;

TYPE
  AlignVal = [0..255];

TYPE
  BoundPair = REF RECORD
    min : Target.Int;
    max : Target.Int;
  END;

VAR
  TsWCGVars: T := NIL;
  (* Linked list of Variable.Ts that have a non-NIL bss_var or cg_var. *)

(* EXPORTED *)
PROCEDURE Reset () =
(* Toss as garbage, any CG.Var nodes that we've created *)
  VAR t, u: T;
  BEGIN
    t := TsWCGVars;
    WHILE (t # NIL) DO
      u := t;  t := t.nextTWACGVar;
      u.cg_var      := NIL;
      u.bss_var     := NIL;
      u.nextTWACGVar := NIL;
    END;
    TsWCGVars := NIL;
  END Reset;

(* EXPORTED *)
PROCEDURE ParseDecl (READONLY att: Decl.Attributes) =
  TYPE TK = Token.T;
  VAR
    t     : T;
    type  : Type.T;
    expr  : Expr.T;
    j, n  : INTEGER;
    trace : Tracer.T;
    alias : M3ID.T;
  BEGIN
    Match (TK.tVAR);
    WHILE (cur.token = TK.tIDENT) DO
      n := Ident.ParseList ();
      type := NIL;
      expr := NIL;
      IF (cur.token = TK.tCOLON) THEN
        GetToken (); (* : *)
        type := Type.Parse ();
      END;
      IF (cur.token = TK.tEQUAL) THEN
        Error.Msg ("Variable initialization must begin with ':='.");
        cur.token := TK.tASSIGN;
      END;
      IF (cur.token = TK.tASSIGN) THEN
        GetToken (); (* := *)
        expr := Expr.Parse ();
      END;
      trace := ParseTrace ();
      IF (expr = NIL) AND (type = NIL) THEN
        Error.Msg("Variable declaration must include a type or initial value.");
      END;
      IF att.isExternal AND att.alias # M3ID.NoID AND n > 1 THEN
        Error.WarnID (2, att.alias,
                       "EXTERNAL alias applies only to the first variable.");
      END;
      alias := att.alias;
      j := Ident.top - n;
      FOR i := 0 TO n - 1 DO
        t := New (Ident.stack[j + i], FALSE);
        t.origin   := Ident.offset[j + i];
        t.external := att.isExternal;
        t.unused   := att.isUnused;
        t.obsolete := att.isObsolete;
        t.type     := type;
        t.repType  := NIL;
        t.initExpr := expr;
        t.no_type  := (type = NIL);
        IF (att.isExternal) THEN
          IF (alias # M3ID.NoID)
            THEN t.extName := alias;  alias := M3ID.NoID;
            ELSE t.extName := t.name;
          END;
        END;
        Scope.Insert (t);
        BindTrace (t, trace);
      END;
      DEC (Ident.top, n);
      Match (TK.tSEMI);
    END;
  END ParseDecl;

(* EXPORTED *)
PROCEDURE New (name: M3ID.T;  used: BOOLEAN): T =
  VAR t := NEW (T);
  BEGIN
    ValueRep.Init (t, name, Value.Class.Var);
    t.used        := used;
    t.readonly    := FALSE;
    t.extName     := M3ID.NoID;
    t.mem_type    := CG.Type.Void;
    t.stk_type    := CG.Type.Void;
    RETURN t;
  END New;

(* EXPORTED *)
PROCEDURE NewFormal (formal: Value.T;  name: M3ID.T): T =
  VAR t := New (name, FALSE); f_info: Formal.Info;
  BEGIN
    Formal.Split (formal, f_info);
    t.formal   := formal;
    t.type     := f_info.type;
    t.origin   := formal.origin;
    t.indirect := (f_info.mode # Formal.Mode.mVALUE);
    t.readonly := (f_info.mode = Formal.Mode.mREADONLY);
    t.unused   := f_info.unused;
    t.initDone := TRUE;
(* REVIEW^ can this be right? *) 
    t.imported := FALSE; (* in spite of Module.depth *)
    IF (NOT t.indirect) AND (OpenArrayType.Is (t.type)) THEN
      t.indirect := TRUE;
    END;

    IF debug THEN
      RTIO.PutText ("NewFormal type:");
      RTIO.PutRef (t.type);
      RTIO.PutText (" name:");
      IF name # 0 THEN
        RTIO.PutText (M3ID.ToText (name));
      END;
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;

    t.trace := NIL;  (* the caller must call BindTrace after the variable
                        is inserted into a scope *)
    RETURN t;
  END NewFormal;

(* EXPORTED *)
PROCEDURE Split (t: T;  VAR type: Type.T;
                 VAR global, indirect, traced: BOOLEAN) =
  BEGIN
    <* ASSERT t.checked *>
    type     := t.type;
    global   := t.global;
    indirect := t.indirect;
    traced   := t.traced;
  END Split;

(* EXPORTED *)
PROCEDURE BindType (t: T; type: Type.T; 
                    indirect, readonly, open_array_ok, needs_init: BOOLEAN) =
(* This gets called at parse time, so can't do any Check. *)
  BEGIN
    <* ASSERT t.type = NIL *>
    t.type     := type;
    t.repType  := NIL;
    t.readonly := readonly;
    t.indirect := indirect;
    t.open_ok  := open_array_ok;
    IF NOT needs_init THEN t.initDone := TRUE END;
  END BindType;

(* EXPORTED *)
PROCEDURE NeedsAddress (t: T) =
  BEGIN
    IF (t = NIL) THEN RETURN END;
    t.need_addr := TRUE;
  END NeedsAddress;

(* EXPORTED *)
PROCEDURE IsFormal (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.formal # NIL);
  END IsFormal;

PROCEDURE IsUpLevel (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND t.up_level;
  END IsUpLevel;

PROCEDURE InitExpr (t: T): M3.Expr =
  BEGIN
    IF t = NIL THEN RETURN NIL END;
    RETURN t.initExpr;
  END InitExpr;

PROCEDURE DeclareGlobalMSIR (t: T;  weak: BOOLEAN := FALSE) =
  VAR mt: MSIR.T;  isTraced: BOOLEAN;  eltType: MSIR.T;
      m : MSIR.Module;  g: MSIR.Global;
      byteSize, byteAlign, byteOff: INTEGER;
      infoName: TEXT;
  BEGIN
    IF NOT MSIREmit.IsEnabled () THEN RETURN END;
    IF NOT t.global THEN RETURN END;
    (* Never emit a definition for a global with no allocated storage (set by
       AllocGlobalVarSpace, which runs before DeclareGlobalsMSIR); such a
       variable is owned elsewhere and reached through the import chain. *)
    IF NOT t.allocated THEN RETURN END;
    mt := MSIRType.Translate (t.type);
    IF mt = NIL THEN RETURN END;
    m := MSIREmit.CurrentModule ();
    IF m = NIL THEN RETURN END;
    (* The owner of an interface variable is the interface unit that declares
       it: the variable appears in the interface's localScope (here) and in
       every importing/re-exporting module's importScope (where DeclareGlobalsMSIR
       routes it to the import chain instead).  So the interface unit DEFINES the
       storage in its @<Intf>_M3_info struct — at the same byte offset the front
       end assigned (t.offset), which the import chain uses — and modules reach it
       via the _I3 binder's II_import.  This mirrors the C backend's I_<intf> data
       segment.  Hence we no longer skip interface units here. *)
    infoName  := MSIR.ModuleInfoName(m);
    IF t.indirect THEN
      (* Large global (size > Max_zero_global): in the C backend the module
         struct holds a pointer to a separately-allocated BSS buffer.  In MSIR
         we skip the pointer indirection entirely: emit the backing storage as a
         standalone zero-initialized global and register it directly so that
         LookupVar/LookupVarAddr return its address without any runtime load.
         This avoids the runtime pointer init and matches the real ABI since
         MSIR only accesses the data via LookupVar, never via the module struct.
         DeclareGlobalMSIR is called twice (AllocGlobalVarSpace + DeclareGlobalsMSIR);
         skip the second call if the variable is already in the global map. *)
      IF MSIRBuilder.LookupVarAddr (t) # NIL THEN RETURN END;
      byteSize  := t.size DIV Target.Char.size;
      byteAlign := MAX(1, t.align DIV Target.Char.size);
      IF byteSize <= 0 THEN byteSize := Target.Address.bytes END;
      g := MSIR.NewGlobal(Value.GlobalName(t, dots:=FALSE, with_module:=TRUE),
                          mt, isTraced := FALSE);
      MSIR.GlobalSetBackingBytes(g, byteSize);
      IF weak THEN MSIR.GlobalSetWeak(g) END;
      (* Set refValue so LookupVarAddr returns the global's address typed as
         ptr(mt) so that SubscriptExpr can GEP into the backing storage.
         Use byteOffset=-1 to mark it as standalone (not struct-embedded). *)
      VAR addrVal := MSIR.GlobalAddrValue(g);
      BEGIN
        (* Retype the ptr(void) addr to ptr(mt) for correct subscript GEP. *)
        addrVal := MSIR.RetypeValue(addrVal, MSIR.TPtr(mt));
        MSIR.GlobalSetStructField(g, -1, addrVal);
      END;
      MSIR.ModuleAddGlobal(m, g);
      MSIRBuilder.GlobalMapAdd(t, g, m);
      (* The OWNER accesses this indirect global via the backing address directly
         (above), but an IMPORTER reads it through the RT0 import chain, which
         loads the pointer from this module's info struct at t.offset
         (RegisterExternMSIR's needsLoad path).  That slot is otherwise zero, so
         the importer would load NULL (e.g. RTCollector writing RTHeapRep.align,
         a 2KB > Max_zero_global array → NULL base → SIGSEGV).  Initialise the
         slot to the backing address via the early global constructor. *)
      VAR ptrByteOff := t.offset DIV Target.Char.size;
      BEGIN
        MSIR.ModuleAddGlobalInit (m, ptrByteOff, MSIR.GlobalAddrValue (g));
        (* Reserve the pointer slot in the info-struct blob so the emitter sizes
           @<Mod>_M3_info to contain it.  Without this, nextGlobalOff stays at the
           MI header size, no [N x i8] blob is emitted, and the ctor's store at
           ptrByteOff overflows past the info struct into the adjacent global
           (e.g. corrupting a typecell's selfID -> FinishObjectTypes MissingType). *)
        MSIR.ModuleNoteGlobal (m, ptrByteOff + Target.Address.bytes);
      END;
      RETURN;
    END;
    isTraced := (MSIR.Kind(mt) = MSIR.TypeKind.GcRef
                 OR MSIR.Kind(mt) = MSIR.TypeKind.GcSlot);
    eltType  := mt;
    IF isTraced THEN eltType := MSIR.EltType(mt) END;
    (* Allocate in the module struct; compute byte size and alignment. *)
    byteSize  := t.size DIV Target.Char.size;
    byteAlign := MAX(1, t.align DIV Target.Char.size);
    IF byteSize <= 0 THEN byteSize := Target.Address.bytes END;
    (* Place the global at the front-end's canonical byte offset (t.offset), the
       SAME offset importing modules use to read it (RegisterExternMSIR below).
       MSIR's own dense ModuleAllocGlobal packing disagreed with t.offset
       whenever the front-end reserved extra module-record space, so a global
       written here landed at a different offset than an importer read it from
       (p289: imported interface VAR read as 0).  ModuleNoteGlobal grows the
       struct to contain it; the emitter reserves [MI_SIZE..max) as an opaque
       byte blob accessed by offset. *)
    byteOff   := t.offset DIV Target.Char.size;
    MSIR.ModuleNoteGlobal(m, byteOff + byteSize);
    g := MSIR.NewGlobal(Value.GlobalName(t, dots:=FALSE, with_module:=TRUE),
                        eltType, isTraced);
    IF weak THEN MSIR.GlobalSetWeak(g) END;
    (* Attach struct field info and update refValue to a StructFieldRef. *)
    VAR fieldType: MSIR.T;
    BEGIN
      IF isTraced THEN fieldType := MSIR.TGcSlot(eltType)
                  ELSE fieldType := MSIR.TPtr(eltType) END;
      MSIRBuilder.GlobalMapAddStruct(t, g, m, infoName, byteOff, fieldType);
    END;
    (* Record a compile-time-constant record initializer so an early global
       constructor can apply it (the info-struct user region is a zero blob, and
       globals such as RTType's InfoMap tables — uids/types/brands — are read
       during RTLinker startup, before any module body runs).  Only whole-record
       const initializers are handled here; scalar/array cases are covered by the
       runtime init path / zeroinit and are added on demand. *)
    (* Compile-time-constant global initializer: lower it to a constant MSIR
       value (record via nested ConstStruct, int/enum/BOOLEAN, float, TEXT lit,
       proc ref, or fixed-array) and apply it through the early @MSIR_InitGlobals
       constructor, which runs BEFORE RTLinker — exactly like the C backend's
       static data segment.  This is the SOLE place module-global constant inits
       are emitted in MSIRObj mode; there is no late module-body store (which
       would re-run after RTLinker.FixTypes and clobber runtime-populated tables,
       e.g. RTType's InfoMap: cnt 131 -> 0 -> MissingType).  Mark initDone +
       msirInitDone so ConstInit/UserInit emit nothing further.
       Skip zero initializers: the info-struct blob is already zero (BSS).
       The ctor is recorded in BOTH backend modes (the C-authoritative
       @M3m3front-msir diagnostic path also lowers + runs the .ll).  Only in
       AUTHORITATIVE MSIRObj mode do we mark initDone/msirInitDone — there the
       ctor is the sole initializer, so ConstInit's CG GenLiteral and UserInit's
       module-body store must both be suppressed; in diagnostic mode CG still
       owns the real static init and must run. *)
    IF t.initExpr # NIL AND NOT t.initZero THEN
      VAR cv := RecordExpr.TryConstFieldMSIR (t.initExpr, mt);
      BEGIN
        IF cv # NIL THEN
          MSIR.ModuleAddGlobalInit (m, byteOff, cv);
          IF Target.BackendMode IN Target.BackendMSIRSet THEN
            t.initDone := TRUE;
            t.msirInitDone := TRUE;
          END;
        END;
      END;
    END;
  END DeclareGlobalMSIR;

PROCEDURE RegisterExternMSIR (t: T) =
  VAR mt: MSIR.T;  eltType: MSIR.T;
      m : MSIR.Module;  g: MSIR.Global;
      nm: TEXT;
  BEGIN
    IF NOT MSIREmit.IsEnabled () THEN RETURN END;
    mt := MSIRType.Translate (t.type);
    IF mt = NIL THEN RETURN END;
    m := MSIREmit.CurrentModule ();
    IF m = NIL THEN RETURN END;

    (* For variables imported from other M3 modules (not C-external), use the
       RT0 import-chain mechanism.  C-compiled libraries store exported vars as
       fields in a static interface struct (not as exported symbols), so we must
       load through the II_import pointer at runtime rather than referencing a
       standalone external symbol. *)
    IF t.imported AND NOT t.external THEN
      VAR unit      := Scope.ToUnit (t);
          ownerName : TEXT;
          binderName: TEXT;
          byteOff   : INTEGER;
      BEGIN
        IF unit = NIL THEN RETURN END;
        ownerName  := M3ID.ToText (Module.Name (NARROW (unit, Module.T)));
        binderName := ownerName & "_I3";
        byteOff    := t.offset DIV Target.Char.size;
        IF t.indirect THEN
          (* Large (indirect) imported global, e.g. RTHeapRep.align: the owner's
             interface struct holds a POINTER to separately-allocated storage at
             t.offset.  Register with needsLoad so LookupVar* loads through it.
             (Without this, indirect imported globals were dropped entirely,
             producing "unbound variable reference".) *)
          MSIRBuilder.GlobalMapAddImport(t, m, binderName, byteOff,
                                         MSIR.TPtr(MSIR.TVoid()),
                                         needsLoad := TRUE, dataType := mt);
        ELSE
          (* Pass the actual MSIR type (not ptr(void)) so LookupVar returns a
             correctly-typed value for traced references (gc_ref @T) rather than
             a raw ptr that causes a type mismatch at every RETURN site. *)
          MSIRBuilder.GlobalMapAddImport(t, m, binderName, byteOff, mt);
        END;
      END;
      RETURN;
    END;

    (* C-external / re-exported indirect globals are not yet modelled here. *)
    IF t.indirect THEN RETURN END;

    nm := Value.GlobalName(t, dots:=FALSE, with_module:=TRUE);
    (* Two different interfaces may declare the same <*EXTERNAL*> variable.
       Deduplicate by name to avoid emitting the same external global twice. *)
    FOR i := 0 TO MSIR.ModuleGlobalCount(m) - 1 DO
      g := MSIR.ModuleGlobal(m, i);
      IF MSIR.GlobalIsExternal(g) AND Text.Equal(MSIR.GlobalName(g), nm) THEN
        MSIRBuilder.GlobalMapAdd(t, g, m);
        RETURN;
      END;
    END;
    (* Keep the real element type (a GcRef renders as LLVM `ptr`, so there is no
       `void` global) so a load returns a correctly typed reference.  Flattening
       traced externals to ptr(void) — as the old code did — made an exported
       traced interface variable load as ptr(void), which mismatches the declared
       type when returned directly (Sx.FromBool: RETURN True where True: Atom.T).
       Pass isTraced := FALSE to NewGlobal regardless: the storage is owned and
       GC-traced by the DEFINING unit, so this (referencing) module must not add
       the external symbol to its own GC map. *)
    eltType  := mt;
    g := MSIR.NewGlobal(nm, eltType, isTraced := FALSE, isExternal := TRUE);
    MSIRBuilder.GlobalMapAdd(t, g, m);
  END RegisterExternMSIR;

PROCEDURE AddLocalMSIR (t: T;  b: MSIR.Block;  force: BOOLEAN := FALSE): BOOLEAN =
  VAR mt: MSIR.T;  slotAddr: MSIR.Value;  allocType: MSIR.T;
      typeInfoForZero : Type.Info;
  BEGIN
    IF b = NIL THEN RETURN FALSE END;
    IF MSIRBuilder.VarMapContains (t) THEN RETURN TRUE END;
    IF t.indirect THEN
      (* An indirect designator alias (WITH x = <bitfield>) cannot be held by
         reference — the source has no lvalue.  force=TRUE turns it into a plain
         by-value local: the caller stores the extracted rvalue into the slot,
         and reads go through LookupVar as an ordinary value. *)
      IF NOT force THEN RETURN FALSE END;
      t.indirect := FALSE;
    END;
    mt := MSIRType.Translate (t.type);
    IF mt = NIL THEN RETURN FALSE END;
    (* Use the wide ZType for the alloca: ordinal scalars narrower than word
       size use TI64 so that loop counters can exceed the type's range without
       wrapping (CHAR[0..255] must reach 256; i8 wraps to 0 → infinite loop).
       VarMapAdd receives the same wide type as storageType, so LookupVar
       loads TI64 directly with no extension.  Values are always in range for
       regular locals; counters briefly exceed range but the alloca holds them.
       With lambda-lifting, up-level variables are ordinary stack allocas in
       the outer proc. *)
    allocType := mt;
    IF Type.IsOrdinal (t.type) THEN
      allocType := MSIR.TI (Target.Integer.size);
      mt := allocType;  (* keep mt in sync — init code uses mt for ConstInt/ConstZero *)
    END;
    slotAddr := MSIR.BuildAlloca(b,
                  MSIRBuilder.UniqueLocalName(
                    Value.GlobalName(t, dots:=FALSE, with_module:=FALSE) & ".slot"), allocType);
    IF slotAddr = NIL THEN RETURN FALSE END;
    MSIRBuilder.VarMapAdd (t, slotAddr, allocType);
    (* Zero-initialize non-solid structured allocas so that padding bytes
       (e.g. after a CHAR field in a RECORD, or in ARRAY elements of records)
       don't contain garbage that breaks byte-wise equality comparisons.
       A type is "solid" if every bit in its storage is meaningful (no gaps).
       Non-solid types have holes that need zeroing; solid types don't.
       Scalar allocas are already init'd via Type.InitCost below. *)
    EVAL Type.CheckInfo (t.type, typeInfoForZero);
    IF (MSIR.Kind (allocType) = MSIR.TypeKind.Struct
        OR MSIR.Kind (allocType) = MSIR.TypeKind.FixedArray)
       AND NOT typeInfoForZero.isSolid THEN
      VAR sizeBytes := t.size DIV Target.Char.size;
          wordBytes := Target.Integer.size DIV Target.Byte;
          intT      := MSIR.TI (Target.Integer.size);
          zero      := MSIR.ConstInt (intT, 0);
          off       := 0;
      BEGIN
        WHILE off + wordBytes <= sizeBytes DO
          MSIR.BuildStore (b, zero,
            MSIRBuilder.BuildPtrByteOff (b, "", slotAddr, off));
          INC (off, wordBytes);
        END;
        (* Handle trailing partial word if struct size not a multiple of wordBytes *)
        IF off < sizeBytes THEN
          VAR byteT := MSIR.TW (Target.Byte);
              zero8 := MSIR.ConstInt (byteT, 0);
          BEGIN
            WHILE off < sizeBytes DO
              MSIR.BuildStore (b, zero8,
                MSIRBuilder.BuildPtrByteOff (b, "", slotAddr, off));
              INC (off, 1);
            END;
          END;
        END;
      END;
    END;
    (* Emit language-default init alongside CG's Type.InitValue in LangInit.
       For subranges with lo > 0 or hi < 0 (zero not in range), init to lo
       rather than zero — matches SubrangeType.GenInit (p143).
       For solid FixedArray/Struct allocas whose default value is all-zero
       (InitCost=0): emit a zeroinitializer store anyway, because LLVM `alloca`
       does NOT zero-initialize by default (unlike C's stack variables in
       optimized builds or heap allocations).  Without this, VAR x := ARRAY OF T
       { FALSE, .. } leaves x[3..N] as stack garbage, causing spurious TRUE when
       checked as BOOLEAN (p140/P26).  The non-solid path above already handles
       the padding-has-garbage case; this handles the content-is-garbage case. *)
    IF (MSIR.Kind (allocType) = MSIR.TypeKind.FixedArray
        OR MSIR.Kind (allocType) = MSIR.TypeKind.Struct)
       AND typeInfoForZero.isSolid
       AND Type.InitCost (t.type, FALSE) = 0 THEN
      VAR zeroVal := MSIR.ConstZero (allocType);
      BEGIN
        IF zeroVal # NIL THEN MSIR.BuildStore (b, zeroVal, slotAddr) END;
      END;
    END;
    IF Type.InitCost (t.type, FALSE) > 0 THEN
      VAR initVal: MSIR.Value := NIL;  lo, hi: Target.Int;
      BEGIN
        IF Type.GetBounds (t.type, lo, hi)
           AND (TInt.LT (TInt.Zero, lo) OR TInt.LT (hi, TInt.Zero)) THEN
          VAR loI: INTEGER;
          BEGIN
            IF TInt.ToInt (lo, loI) THEN
              initVal := MSIR.ConstInt (mt, loI);
            END;
          END;
        END;
        IF initVal = NIL THEN initVal := MSIR.ConstZero (mt) END;
        IF initVal # NIL THEN MSIR.BuildStore (b, initVal, slotAddr) END;
      END;
    END;
    RETURN TRUE;
  END AddLocalMSIR;

PROCEDURE BindFormalMSIR (t: T;  p: MSIR.Proc;  b: MSIR.Block) =
  VAR fName   := M3ID.ToText (t.name);  (* direct field access *)
      n       := MSIR.ProcParamCount (p);
      paramVal: MSIR.Value;
      mt      : MSIR.T;
  BEGIN
    IF p = NIL OR b = NIL THEN RETURN END;
    IF MSIRBuilder.VarMapContains (t) THEN RETURN END;
    FOR i := 0 TO n - 1 DO
      IF Text.Equal (MSIR.ProcParamName (p, i), "a." & fName) THEN
        paramVal := MSIR.ProcParam (p, i);
        mt       := MSIR.ValueType (paramVal);
        IF t.indirect AND MSIR.Kind(mt) = MSIR.TypeKind.Ptr
              AND MSIR.Kind(MSIR.EltType(mt)) # MSIR.TypeKind.Void THEN
          (* VAR or READONLY aggregate: passed as pointer, load through it.
             t.indirect guards VALUE formals of pointer type (e.g. p: IntPtr) from
             being misclassified here.  TPtr(TVoid()) falls to the else branch. *)
          MSIRBuilder.VarMapAdd (t, paramVal, MSIR.EltType(mt));
        ELSE
          (* VALUE, or READONLY scalar/proc-ref: spill to alloca so ADR works
             and the body can assign (VALUE) or take address (READONLY).
             Widen ordinal formals to Target.Integer.size so that sub-word
             scalars (CHAR, BOOLEAN, subranges) use the same wide alloca as
             AddLocalMSIR, keeping LookupVar loads consistent. *)
          VAR allocType := mt;
              storeVal  := paramVal;
          BEGIN
            IF Type.IsOrdinal (t.type) THEN
              allocType := MSIR.TI (Target.Integer.size);
              (* BOOLEAN has MSIR type TI1 (no W1 kind), so CoerceToMSIR would
                 use SExt and turn TRUE (i1=1) into -1.  Instead, derive the
                 extension direction from the M3 type bounds: non-negative lo
                 (BOOLEAN, CARDINAL, [0..N]) → ZExt; negative lo → SExt. *)
              VAR lo, hi: Target.Int;
                  hasBounds := Type.GetBounds (t.type, lo, hi);
              BEGIN
                IF hasBounds AND TInt.LT (lo, TInt.Zero) THEN
                  storeVal := MSIR.BuildSExt (b, "", paramVal, allocType);
                ELSE
                  storeVal := MSIR.BuildZExt (b, "", paramVal, allocType);
                END;
              END;
            END;
            VAR slot := MSIR.BuildAlloca (b,
                          Value.GlobalName(t, dots:=FALSE, with_module:=FALSE) & ".slot",
                          allocType);
            BEGIN
              MSIR.BuildStore (b, storeVal, slot);
              MSIRBuilder.VarMapAdd (t, slot, allocType);
            END;
          END;
        END;
        RETURN;
      END;
    END;
  END BindFormalMSIR;

(* EXPORTED *)
PROCEDURE HasClosure (t: T): BOOLEAN =
  BEGIN
    RETURN (t # NIL) AND (t.formal # NIL) AND Formal.HasClosure (t.formal);
  END HasClosure;

(* Externally dispatched-to *)
PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    IF (t.type = NIL) THEN
      IF t.initExpr # NIL THEN t.type := Expr.TypeOf (t.initExpr)
      ELSIF  t.formal # NIL THEN t.type := Value.TypeOf (t.formal)
      END;
      IF (t.type = NIL)
        THEN Error.ID (t.name, "Variable has no type.");  t.type := ErrType.T;
      END;
    END;
    RETURN t.type;
  END TypeOf;

(* Externally dispatched-to *)
PROCEDURE RepTypeOf (t: T): Type.T =
  VAR semType: Type.T;
  BEGIN
    IF t.repType = NIL THEN
      semType := TypeOf (t);
      t.repType := Type.StripPacked (semType);
    END;
    RETURN t.repType;
  END RepTypeOf;

(* Externally dispatched-to *)
PROCEDURE Check (t: T;  VAR cs: Value.CheckState) =
  VAR dfault: Expr.T;  min, max: Target.Int;  info: Type.Info;  refType: Type.T;
      type: Type.T := NIL;
  BEGIN
    type := Type.CheckInfo (TypeOf (t), info);
    t.repType  := Type.Check (Type.StripPacked (type));
    t.size     := info.size;
    t.align    := info.alignment;
    t.mem_type := info.mem_type;
    t.stk_type := info.stk_type;
    IF (info.class = Type.Class.OpenArray)
      AND (t.formal = NIL) AND (NOT t.open_ok) THEN
      Error.ID (t.name, "Variable cannot have an open array type.");
    END;
    IF (info.isEmpty) THEN
      Error.ID (t.name, "Variable cannot have empty type.");
(* CHECK: Is this always only secondary to some other error. *)
    END;
    IF type = Null.T THEN
      Error.WarnID (1, t.name, "Variable cannot have type NULL.");
    END;

    t.global := Scope.OuterMost (t.scope);
    t.checked := TRUE; (* Allow recursions through initExpr. *)

    IF (NOT t.indirect) AND (NOT t.global) THEN
      IF (t.formal # NIL) AND (info.size > Big_Param * Target.Integer.size) THEN
        Error.WarnID (1, t.name, "Large parameter passed by value ("
                       & Fmt.Int (info.size DIV Target.Char.size) & " bytes).");
      ELSIF (info.size > Big_Local * Target.Char.size) THEN
        Error.WarnID (1, t.name, "Large local variable ("
                       & Fmt.Int (info.size DIV Target.Char.size) & " bytes).");
      END;
    ELSIF (t.formal # NIL) AND (info.class = Type.Class.OpenArray)
      AND Formal.OpenArrayByVALUE (t.formal, (*VAR*) refType) THEN
      Error.WarnID (1, t.name, "Open array passed by value.");
    END;

    IF Type.IsStructured (type) THEN
      t.need_addr := TRUE; (* every load requires an address *)
    END;

    Value.TypeCheck (t.formal, cs);
    IF (t.external) THEN
      IF (t.initExpr # NIL) THEN
        Error.Msg ("<*EXTERNAL*> variables cannot be initialized.");
        Expr.TypeCheck (t.initExpr, cs);
        AssignStmt.Check (type, t.initExpr, cs);
      END;
    ELSIF (t.initExpr # NIL) THEN
      Expr.TypeCheck (t.initExpr, cs);
      AssignStmt.Check (type, t.initExpr, cs);
(* TODO: What if initExpr contains RT errors? *)
      dfault := Expr.ConstValue (t.initExpr);
      IF (dfault = NIL) THEN
        IF Module.IsInterface () THEN
          Error.ID (t.name, "Initial value in an interface must be constant.");
        END;
        IF (t.global) AND (info.size > Max_zero_global * Target.Integer.size) THEN
          <*ASSERT NOT t.indirect*>
          t.indirect := TRUE;
        END;
      ELSE (* initialize the variable to an explicit constant *)
        IF NOT t.indirect THEN
          t.initZero := Expr.IsZeroes (dfault);
          IF (t.global) THEN
            IF (t.initZero) THEN
              t.initDone := TRUE;
              IF (info.size > Max_zero_global * Target.Integer.size) THEN
                <*ASSERT NOT t.indirect*>
                t.indirect := TRUE;
              END;
            END;
          ELSIF (NOT t.initZero) AND Type.IsStructured (type) THEN
            t.initStatic := TRUE;
          END;
          t.initExpr := dfault;
        END;
      END;
    ELSIF (t.global) THEN
      (* no explict initialization is given, but the var is global *)
      IF Type.InitCost (type, TRUE) <= 0 THEN
        IF (info.size > Max_zero_global * Target.Integer.size) THEN
          <*ASSERT NOT t.indirect*>
          t.indirect := TRUE;
        END;
        t.initDone := TRUE;
      ELSIF Type.GetBounds (type, min, max) THEN
        (* synthesize an initialization expression *)
        IF Type.IsSubtype (type, LInt.T)
        THEN t.initExpr := IntegerExpr.New (LInt.T, min);
        ELSE t.initExpr := IntegerExpr.New (Int.T, min);
        END;
      END;
    END;

    t.qualName := Value.GlobalName(t);
    CheckTrace (t.trace, cs);
  END Check;

(* EXPORTED *)
(* Externally dispatched-to *)
PROCEDURE Load (t: T) =
  VAR type_info: Type.Info;
  BEGIN
    t.used := TRUE;
    Value.Declare (t);
    IF (t.initPending) THEN ForceInit (t); END;
    IF Type.IsStructured (t.type) THEN
      (* The runtime representation is an address *)
      IF (t.bss_var # NIL) THEN
        CG.Load_addr_of (t.bss_var, 0, t.cg_align);
      ELSIF (t.cg_var = NIL) THEN (* => global *)
        Module.LoadGlobalAddr (Scope.ToUnit (t), t.offset, is_const := FALSE);
        CG.Boost_addr_alignment (t.cg_align);
      ELSIF (t.indirect) THEN
        CG.Load_addr (t.cg_var, t.offset, t.cg_align);
        (* ^Misleading name.  Actually load value and label as an address. *)
      ELSE
        CG.Load_addr_of (t.cg_var, t.offset, CG.GCD(t.cg_align, t.offset));
      END;
    ELSE (* simple scalar *)
      EVAL Type.CheckInfo (t.type, type_info);
      IF (t.bss_var # NIL) THEN
        CG.Load
          (t.bss_var, 0, t.size, t.cg_align, type_info.alignment, t.stk_type);
      ELSIF (t.cg_var = NIL) THEN (* => global *)
        Module.LoadGlobalAddr (Scope.ToUnit (t), t.offset, is_const := FALSE);
        IF (t.indirect) THEN
          CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size);
        END;
        CG.Boost_addr_alignment (type_info.alignment);
        CG.Load_indirect (t.stk_type, 0, t.size, type_info.addr_align);
      ELSIF (t.indirect) THEN
        CG.Load_addr (t.cg_var, t.offset, type_info.alignment);
        (* ^Misleading name.  Actually load value and label as an address. *)
     (* CG.Load_indirect (t.stk_type, 0, t.size, type_info.addr_align); *)
        CG.Load_indirect
          (type_info.stk_type, 0, type_info.size, type_info.addr_align);
      ELSE
        CG.Load
          (t.cg_var, t.offset, t.size, CG.GCD (t.cg_align, t.offset),
           type_info.addr_align, t.stk_type);
      END;
    END;
  END Load;

(* EXPORTED *)
PROCEDURE LoadLValue (t: T) =
  VAR type_info: Type.Info;
  BEGIN
    t.used := TRUE;
    Value.Declare (t);
    IF (t.initPending) THEN ForceInit (t); END;
    EVAL Type.CheckInfo (t.type, type_info);
    IF (t.bss_var # NIL) THEN
      CG.Load_addr_of (t.bss_var, 0, type_info.alignment);
    ELSIF (t.cg_var = NIL) THEN (* => global variable *)
      Module.LoadGlobalAddr (Scope.ToUnit (t), t.offset, is_const := FALSE);
      IF (t.indirect) THEN
        CG.Load_indirect (CG.Type.Addr, 0, Target.Address.size);
      END;
    ELSIF (t.indirect) THEN
      CG.Load_addr (t.cg_var, t.offset, type_info.alignment);
      (* ^Misleading name.  Actually load value and label as an address. *)
    ELSE
      CG.Load_addr_of
        (t.cg_var, t.offset, CG.GCD (type_info.alignment, t.offset));
    END;
    CG.Boost_addr_alignment (t.cg_align);
  END LoadLValue;

(* EXPORTED *)
PROCEDURE SetLValue (t: T) =
  VAR v: CG.Var;  align: INTEGER;
  BEGIN
    t.used := TRUE;
    Value.Declare (t);
    IF (t.initPending) THEN t.initPending := FALSE; END;
    v := t.cg_var;
    align := t.cg_align;
    IF (v = NIL) THEN
      v := Module.GlobalData (is_const := FALSE);
      align := CG.Max_alignment;
    END;
    <*ASSERT t.indirect *>
    CG.Boost_addr_alignment (t.cg_align);
    CG.Store_addr (v, t.offset);
  END SetLValue;

(* EXPORTED *)
PROCEDURE LocalCGName (t: T;  VAR unit: CG.Var;  VAR offset: INTEGER) =
  BEGIN
    t.used := TRUE;
    Value.Declare (t);
    IF (t.initPending) THEN ForceInit (t); END;
    <*ASSERT NOT t.imported*>
    IF (t.cg_var = NIL)
      THEN unit := Module.GlobalData (FALSE);  offset := t.offset;
      ELSE unit := t.cg_var;                   offset := 0;
    END;
  END LocalCGName;

(* EXPORTED *)
PROCEDURE SetBounds (t: T;  READONLY min, max: Target.Int) =
  BEGIN
    IF (t.bounds = NIL) THEN t.bounds := NEW (BoundPair) END;
    t.bounds.min := min;
    t.bounds.max := max;
  END SetBounds;

(* EXPORTED *)
PROCEDURE GetBounds (t: T;  VAR min, max: Target.Int) =
  VAR xx := t.bounds;
  BEGIN
    EVAL Type.GetBounds (t.type, min, max);
    IF (xx = NIL) THEN RETURN; END;
    IF TInt.LT (min, xx.min) THEN min := xx.min; END;
    IF TInt.LT (xx.max, max) THEN max := xx.max; END;
  END GetBounds;

(* Externally dispatched-to *)
PROCEDURE AllocGlobalVarSpace (t: T) =
(* Allocate space for a non-external global. *)
  VAR size, align: INTEGER;
  VAR constInitExpr: Expr.T;
  VAR initRepType: Type.T := NIL;
  VAR varID: M3ID.T;
  BEGIN
    (* Type.SetGlobals (t.type); *)
    (* IF (t.initExpr # NIL) THEN Type.SetGlobals (Expr.TypeOf (t.initExpr)) END; *)
    IF t.allocated (* Already done.*) OR NOT t.global OR t.external THEN RETURN END;
    EVAL Type.Check (t.type);

    IF t.initExpr # NIL THEN
      constInitExpr := Expr.ConstValue (t.initExpr);
      ArrayExpr.NoteTargetType (constInitExpr, t.type);
      initRepType := Expr.RepTypeOf (t.initExpr)
    END;

    IF (t.indirect) THEN
      size  := Target.Address.size;
      align := Target.Address.align;
      varID := M3ID.Add (t.qualName & "_INDIRECT_");
    ELSIF OpenArrayType.Is (initRepType) THEN
      size  := Target.Address.size
               + OpenArrayType.OpenDepth(initRepType) * Target.Integer.size;
      align := MAX (Target.Address.align, Target.Integer.align);
      varID := M3ID.Add (t.qualName & "_DOPE_");
    ELSE
      size  := t.size;
      align := t.align;
      varID := M3ID.Add (t.qualName);
    END;

    (* declare the actual variable *)
    t.offset := Module.Allocate (size, align, FALSE, id := varID);
    t.allocated := TRUE;
  END AllocGlobalVarSpace;

(* Externally dispatched-to *)
PROCEDURE Declare (t: T): BOOLEAN =
  VAR
    size       := t.size;
    align      := t.align;
    typeUID    := Type.GlobalUID (t.type);
    mtype      := Type.CGType (t.type, in_memory := TRUE);
    is_struct  := Type.IsStructured (t.type);
    externName :TEXT := NIL;
    externM3ID := M3ID.NoID;
    typename   := M3ID.NoID;
    indirect_text := " ";
  BEGIN
    Type.Compile (t.type);

    t.cg_var  := NIL;
    t.bss_var := NIL;

    IF (is_struct) THEN mtype := CG.Type.Struct; END;

    IF (t.indirect) THEN
      Type.Typename (t.type, typename);
      typeUID := CG.Declare_indirect (typeUID, typename);
      typename := M3ID.NoID;
      size := Target.Address.size;
      align := Target.Address.align;
      mtype := CG.Type.Addr;
    END;

    (* declare the actual variable *)
    IF (t.external) THEN
      externName := Value.GlobalName (t, dots := FALSE, with_module := FALSE);
      externM3ID := M3ID.Add (externName);
      t.nextTWACGVar := TsWCGVars;  TsWCGVars := t;
      Type.Typename (t.type, typename);

      IF debug THEN
        RTIO.PutText ("Variable.Declare t:");
        RTIO.PutRef (t);
        RTIO.PutText (" t.type:");
        RTIO.PutRef (t.type);
        RTIO.PutText (" typename:");
        RTIO.PutInt (typename);
        RTIO.PutText ("\n");
        RTIO.Flush ();
      END;

      IF Target.BackendMode # Target.M3BackendMode_t.C THEN
        typeUID := 0(*no mangling*);
      END;
      t.cg_var := CG.Import_global (externM3ID, size, align, mtype, typeUID, typename);
      t.cg_align := align;

    ELSIF (t.imported) THEN
      <*ASSERT t.allocated*>

    ELSIF (t.global) THEN
      <*ASSERT t.allocated*>
      CG.Declare_global_field (t.name, t.offset, size, typeUID, FALSE);
      DeclareGlobalMSIR(t);
      IF (t.initZero) THEN t.initDone := TRUE END;
      t.cg_align := align;
      IF (t.indirect) THEN
        t.cg_align := t.align;
        t.nextTWACGVar := TsWCGVars;  TsWCGVars := t;
        t.bss_var := CG.Declare_global (t.name, t.size, t.cg_align,
                              CG.Type.Struct, Type.GlobalUID (t.type),
                              exported := FALSE, init := FALSE);
        CG.Init_var (t.offset, t.bss_var, 0, FALSE);
      END;

    ELSIF (t.formal = NIL) THEN
      (* simple local variable *)
      IF (size < 0) THEN
        (* it's an open array local introduced by a WITH statement *)
        align := MAX (Target.Address.align, Target.Integer.align);
        size  := Target.Address.pack
                  + OpenArrayType.OpenDepth(t.type) * Target.Integer.pack;
      END;
      (** align := FindAlignment (align, size); **)
      t.cg_align := align;
      t.nextTWACGVar := TsWCGVars;  TsWCGVars := t;
      t.cg_var := CG.Declare_local (t.name, size, align, mtype, typeUID,
                                    t.need_addr, t.up_level, CG.Maybe);
    ELSE
      (* parameter *)
      IF (t.indirect) THEN
        (* formal passed by reference => param is an address *)
        indirect_text := " indirect "
        (* typename is earlier in this function, for the target of the indirect *)
      ELSE
        (* simple parameter *)
        (** align := FindAlignment (align, size); **)
        Type.Typename (TypeOf (t), typename);
      END;

      IF debug THEN
        RTIO.PutText ("Variable.Declare" & indirect_text & "param type:");
        RTIO.PutRef (t.type);
        RTIO.PutText (" name:");
        IF t.name # 0 THEN
          RTIO.PutText (M3ID.ToText (t.name));
        END;
        RTIO.PutText (" typename:");
        RTIO.PutInt (typename);
        RTIO.PutText ("\n");
        RTIO.Flush ();
      END;

      t.cg_align := t.align;
      t.nextTWACGVar := TsWCGVars;  TsWCGVars := t;
      t.cg_var := CG.Declare_param (t.name, size, align, mtype, typeUID, t.need_addr, t.up_level, CG.Maybe, typename);
    END;

    RETURN TRUE;
  END Declare;

(** -- this doesn't work with the current gcc-based backend.  It
       chokes on  VAR v: BITS 32 FOR CHAR := 'X' -- 10/9/96 WKK
PROCEDURE FindAlignment (align: AlignVal;  size: INTEGER): AlignVal =
  (* Fix the alignment of small local variables and parameters
     with BITS FOR types *)
  BEGIN
    IF    size < 0                  THEN (*don't mess with open array alignments*)
    ELSIF size >= Target.Int_D.size THEN align := MAX (align, Target.Int_D.align);
    ELSIF size <= Target.Int_A.size THEN align := MAX (align, Target.Int_A.align);
    ELSIF size <= Target.Int_B.size THEN align := MAX (align, Target.Int_B.align);
    ELSIF size <= Target.Int_C.size THEN align := MAX (align, Target.Int_C.align);
    ELSE                                 align := MAX (align, Target.Int_D.align);
    END;
    RETURN align;
  END FindAlignment;
**)

(* Externally dispatched-to *)
PROCEDURE ConstInit (t: T) =
  VAR
    initSize : INTEGER;
    initAlign : AlignVal;
    initRepType : Type.T;
    initDepth : INTEGER;
    typeUID       : INTEGER;
    constInitExpr : Expr.T;
    initName      : TEXT;
    initM3ID      : M3ID.T;
    initInfo : Type.Info;
  BEGIN
    IF t.external OR t.imported THEN RETURN END;
    IF NOT t.initStatic AND NOT t.global THEN RETURN END;

    IF t.initStatic AND NOT t.initAllocated THEN
      (* Allocate space in the global constant area for the initial value. *)
      typeUID := Type.GlobalUID (t.type);
      constInitExpr := Expr.ConstValue (t.initExpr);
      <* ASSERT constInitExpr # NIL *>
      IF (t.indirect) THEN
        typeUID  := CG.Declare_indirect (typeUID);
        initSize  := Target.Address.size;
        initAlign := Target.Address.align;
        initName := t.qualName & "_INIT_INDIRECT_";
      ELSE
        initRepType := Expr.RepTypeOf (constInitExpr);
        EVAL Type.CheckInfo (initRepType, initInfo);
        initDepth := OpenArrayType.OpenDepth (initRepType);

        IF initDepth > 0 THEN (* initial value is an open array *)
          (* Allocate space for the dope only. *)
          (* See ArrayExpr.GenLiteral, where element space will
             be allocated. *)
          initSize := Target.Address.pack + initDepth * Target.Integer.pack;
          initAlign := MAX (Target.Address.align, Target.Integer.align);
          initName := t.qualName & "_INIT_DOPE_";
        ELSE
          initSize  := initInfo.size;
          initAlign := initInfo.alignment;
          initName := t.qualName & "_INIT_";
        END;
      END;
      initM3ID := M3ID.Add (initName);
(* TODO: Eliminate duplicate copies of same value, including reused,
         named constant. *)
      t.initValOffset
        := Module.Allocate
             (initSize, initAlign, TRUE, "init value for ", initM3ID);
      t.initAllocated := TRUE;
      CG.Declare_global_field
        (t.name, t.initValOffset, initSize, typeUID, TRUE);
      CG.Comment
        (t.initValOffset, TRUE, "init value for ", initName);
      Expr.PrepLiteral (constInitExpr, initRepType, TRUE);
      Expr.GenLiteral (constInitExpr, t.initValOffset, initRepType, TRUE);
    END;

    IF (t.global) THEN
      (* Try to statically initialize directly in the global variable area. *)
      <*ASSERT t.allocated*>
      constInitExpr := NIL;
      IF (t.initExpr # NIL) AND (NOT t.initDone) AND (NOT t.initStatic) THEN
        constInitExpr := Expr.ConstValue (t.initExpr);
      END;
      IF (constInitExpr # NIL) THEN
        IF NOT Expr.CheckUseFailure (t.initExpr) THEN
         (* NOTE: Modula3 defines this as a checked runtime error, but in a
            global variable, as in this case, execution of the assignment is
            inevitable, thus it can't fail to fail at runtime.  Also,
            portions of the runtime system are executed before their module's
            initialization (the only place the compiler could put a runtime
            abort) and depend instead on variables being statically initialized.
            So we make this a compile time error.
          *)
          Error.Msg
            ("Variable's initial value contains runtime assignability "
             & "failure(s).");
        END;
        Expr.PrepLiteral (constInitExpr, t.type, FALSE);
        Expr.GenLiteral (constInitExpr, t.offset, t.type, FALSE);
        t.initDone := TRUE;
      END;
    END;
  END ConstInit;

(* Externally dispatched-to *)
PROCEDURE NeedInit (t: T): BOOLEAN =
  VAR refType: Type.T;
  BEGIN
    IF (t.imported) OR (t.external) OR (t.initDone) THEN
      RETURN FALSE;
    ELSIF (t.formal # NIL) THEN
      RETURN (t.indirect)
             AND Formal.OpenArrayByVALUE (t.formal, (*VAR*) refType);
    ELSIF (t.indirect) AND (NOT t.global) THEN
      RETURN FALSE;
    ELSIF (t.global) AND (t.initExpr # NIL) AND (NOT t.initStatic)
      AND (Expr.ConstValue (t.initExpr) # NIL) THEN
      RETURN FALSE;
    ELSIF (t.initExpr # NIL) THEN
      RETURN TRUE;
    ELSE
      RETURN Type.InitCost (t.type, FALSE) > 0;
    END;
  END NeedInit;
  
(* Externally dispatched-to *)
PROCEDURE GenScalarInitMSIR (t: T) =
(* Emit MSIR initialization for scalar subrange variables where the language-
   defined init value is NOT zero (e.g. BITS 8 FOR [-30..-12] → init to -30).
   Mirrors SubrangeType.GenInit.  Only emits if InitCost > 0 and the type is a
   scalar subrange with lo > 0 or hi < 0 (zero not in range).  Called from
   LangInit alongside RecordType.GenInitMSIR so both record fields and scalars
   get their correct initial values (p143). *)
  VAR lo, hi: Target.Int;  loI: INTEGER;
      mt  := MSIRType.Translate (t.type);
      addr := MSIRBuilder.LookupVarAddr (t);
  BEGIN
    IF mt = NIL OR addr = NIL THEN RETURN END;
    IF Type.IsOrdinal (t.type) THEN
      mt := MSIR.TI (Target.Integer.size);
    END;
    IF NOT Type.GetBounds (t.type, lo, hi) THEN RETURN END;
    IF NOT (TInt.LT (TInt.Zero, lo) OR TInt.LT (hi, TInt.Zero)) THEN RETURN END;
    IF NOT TInt.ToInt (lo, loI) THEN RETURN END;
    VAR initVal := MSIR.ConstInt (mt, loI);
        b       := MSIRBuilder.CurrentBlock ();
    BEGIN
      IF initVal # NIL THEN MSIR.BuildStore (b, initVal, addr) END;
    END;
  END GenScalarInitMSIR;

PROCEDURE LangInit (t: T) =
  VAR refType: Type.T;
  BEGIN
    IF (t.imported) OR (t.external) THEN
      t.initDone := TRUE;
    ELSIF (t.formal # NIL) THEN
      IF t.indirect
         AND Formal.OpenArrayByVALUE (t.formal, (*VAR*) refType) THEN
        (* a by-value open array! *)
        CG.Gen_location (t.origin);
        Load(t);
        CopyOpenArray (t.type, refType);
        (* ^Change the formal parameter to refer to the new storage. *)
        CG.Store_addr (t.cg_var);
      END;
      (* formal parameters don't need any further initialization *)
      Tracer.Schedule (t.trace);
      t.initDone := TRUE;
    ELSIF (t.indirect) AND (NOT t.global) THEN
      (* is a WITH variable bound to a designator *)
      Tracer.Schedule (t.trace);
      t.initDone := TRUE;
    END;

    IF (t.initDone) THEN RETURN END;

    (* MSIR: register alloca for block-scope locals (proc-scope ones are
       already registered by BeginProc; AddLocalMSIR is idempotent). *)
    IF NOT (t.indirect OR t.global) AND MSIRBuilder.InProc () THEN
      EVAL AddLocalMSIR (t, MSIRBuilder.CurrentBlock ());
    END;

    (* initialize the value *)
    IF (t.initExpr # NIL) AND (NOT t.up_level) AND (NOT t.imported) THEN
      (* variable has a user specified initExpr value and isn't referenced
         by any nested procedures => try to avoid the language defined
         init and wait until we get to the user defined initialization. *)
      t.initPending := TRUE;
    ELSE
      IF Type.InitCost (t.type, FALSE) > 0 THEN
        CG.Gen_location (t.origin);
        LoadLValue (t);
        Type.InitValue (t.type, FALSE);
        (* MSIR: GenInit (Type.InitValue) is CG-only.  Emit equivalent MSIR
           for records (field defaults) and scalar subranges (p143, p288). *)
        IF MSIRBuilder.InProc () THEN
          GenScalarInitMSIR (t);
          RecordType.GenInitMSIR (t.type, MSIRBuilder.LookupVarAddr (t));
        END;
      END;
      IF (t.trace # NIL) AND (NOT t.imported) THEN
        IF (t.initExpr = NIL) OR (t.initDone) THEN
          (* there's no explicit user init => might as well trace it now *)
          CG.Gen_location (t.origin);
          Tracer.Schedule (t.trace);
        END;
      END;
    END;
  END LangInit;

PROCEDURE ForceInit (t: T) =
  BEGIN
    t.initPending := FALSE;
    CG.Gen_location (t.origin);
    LoadLValue (t);
    Type.InitValue (t.type, FALSE);
    IF MSIRBuilder.InProc () THEN
      RecordType.GenInitMSIR (t.type, MSIRBuilder.LookupVarAddr (t));
    END;
  END ForceInit;

(* EXPORTED *)
PROCEDURE BitSize (t: T): INTEGER =
  BEGIN RETURN t.size END BitSize;

PROCEDURE InitMSIR (tv: Value.T) =
(* MSIR-only LangInit + UserInit for local variables in a proc's syms scope.
   Called from Procedure.GenBodyMSIR to set up allocas and initializers for
   VAR declarations when the CG's Scope.InitValues runs too late (with
   InProc=FALSE because msirSkip=TRUE in GenBody).
   Skips non-Variable.T values (procs, formals with CG side-effects) and
   variables that were already initialized (VarMapContains, msirInitDone). *)
  BEGIN
    TYPECASE tv OF
    | T (t) =>
        IF NOT MSIRBuilder.InProc () THEN RETURN END;
        IF t.indirect OR t.global OR t.imported THEN RETURN END;
        IF t.formal # NIL THEN RETURN END;  (* formals already bound by BeginProc *)
        IF MSIRBuilder.VarMapContains (t) THEN
          (* Already registered (e.g., re-entry or CG GenBody ran first with InProc=TRUE).
             Still emit initializer if not yet done. *)
        ELSE
          (* Create the alloca and zero-init. *)
          IF NOT AddLocalMSIR (t, MSIRBuilder.CurrentBlock ()) THEN RETURN END;
        END;
        IF t.msirInitDone THEN RETURN END;
        (* Emit the user-specified initializer if any.
           When t.initZero=TRUE (e.g. VAR x := ARRAY OF T { zero_val, .. }),
           the initializer is all-zeros.  For scalar types, the AddLocalMSIR
           zero-init path already handles this.  For FixedArray/Struct, the
           alloca is NOT zero-initialized by default — we must emit a
           zeroinitializer store explicitly so x[3..N] aren't stack garbage.
           Example: VAR x := ARRAY [0..10] OF BOOLEAN { FALSE, .. }  leaves
           x[3..10] uninitialized without this fix (p140/P26). *)
        IF (t.initExpr # NIL) AND t.initZero AND NOT t.initAllocated THEN
          VAR addr := MSIRBuilder.LookupVarAddr (t);
              mt   : MSIR.T;
              info : Type.Info;
          BEGIN
            IF addr # NIL THEN
              mt := MSIR.EltType (MSIR.ValueType (addr));
              EVAL Type.CheckInfo (t.type, info);
              IF mt # NIL
                 AND (MSIR.Kind (mt) = MSIR.TypeKind.FixedArray
                      OR MSIR.Kind (mt) = MSIR.TypeKind.Struct)
                 AND info.isSolid THEN
                VAR zeroVal := MSIR.ConstZero (mt);
                    blk     := MSIRBuilder.CurrentBlock ();
                BEGIN
                  IF zeroVal # NIL THEN MSIR.BuildStore (blk, zeroVal, addr) END;
                END;
              END;
            END;
          END;
          t.msirInitDone := TRUE;
        END;
        IF (t.initExpr # NIL) AND NOT t.initZero AND NOT t.initAllocated THEN
          VAR initVal := Expr.CompileMSIR (t.initExpr);
              addr    := MSIRBuilder.LookupVarAddr (t);
          BEGIN
            IF initVal # NIL AND addr # NIL THEN
              IF MSIR.Kind (MSIR.ValueType (addr)) = MSIR.TypeKind.GcSlot THEN
                MSIR.BuildGcStore (MSIRBuilder.CurrentBlock (), addr, initVal);
              ELSIF NOT MSIRBuilder.OpenArrayToFixedStore (addr, initVal, t.type) THEN
                VAR slotT := MSIR.EltType (MSIR.ValueType (addr));
                    blk   := MSIRBuilder.CurrentBlock ();
                    srcW  := MSIR.BitWidth (MSIR.ValueType (initVal));
                    dstW  := MSIR.BitWidth (slotT);
                BEGIN
                  IF srcW > 0 AND dstW > 0 AND srcW # dstW THEN
                    IF srcW > dstW
                      THEN initVal := MSIR.BuildTrunc (blk, "", initVal, slotT);
                      ELSE initVal := MSIR.BuildZExt  (blk, "", initVal, slotT);
                    END;
                  END;
                  MSIR.BuildStore (blk, initVal, addr);
                END;
              END;
            END;
          END;
          t.msirInitDone := TRUE;
        END;
    ELSE (* Not a Variable.T; skip *)
    END;
  END InitMSIR;

PROCEDURE ForceInitMSIR (t: T) =
(* Emit only the MSIR part of the initialization for t, if it has not been
   emitted yet.  Uses a separate msirInitDone flag (NOT initPending) because
   the CG path's ForceInit clears initPending BEFORE the MSIR path runs, so
   a check on initPending would always see FALSE. *)
  BEGIN
    IF t.msirInitDone THEN RETURN END;
    (* Only force-init global variables (module-level VARs with initExpr that
       depend on other module VARs initialized later).  Local variables and
       uplevel-captured vars are initialized by their own VAR binding in the
       proc body — forcing them here would reset them to their initial value
       every time they are read (e.g., a nested-proc capture resets a loop
       accumulator on each call). *)
    IF NOT t.global THEN RETURN END;
    IF t.initExpr = NIL THEN RETURN END;
    IF NOT MSIRBuilder.InProc () THEN RETURN END;
    (* Prevent recursion: mark done before emitting (in case initExpr references t). *)
    t.msirInitDone := TRUE;
    (* Compile the init expression and store it.  For dependency-forced init
       (called from NamedExpr.CompileMSIR when a variable references another
       variable that hasn't been initialized yet), initDone might already be
       TRUE (e.g., set by AllocGlobalVarSpace for struct-const inits) while the
       MSIR store still needs to be emitted.  So we check msirInitDone (which
       we just set) rather than initDone. *)
    IF (t.initExpr # NIL) AND (NOT t.initDone) AND (NOT t.imported) THEN
      VAR initVal := Expr.CompileMSIR (t.initExpr);
          addr    := MSIRBuilder.LookupVarAddr (t);
      BEGIN
        IF initVal # NIL AND addr # NIL THEN
          IF MSIR.Kind (MSIR.ValueType (addr)) = MSIR.TypeKind.GcSlot THEN
            MSIR.BuildGcStore (MSIRBuilder.CurrentBlock (), addr, initVal);
          ELSIF NOT MSIRBuilder.OpenArrayToFixedStore (addr, initVal, t.type) THEN
            VAR slotT := MSIR.EltType (MSIR.ValueType (addr));
                blk   := MSIRBuilder.CurrentBlock ();
                srcW  := MSIR.BitWidth (MSIR.ValueType (initVal));
                dstW  := MSIR.BitWidth (slotT);
            BEGIN
              IF srcW > 0 AND dstW > 0 AND srcW # dstW THEN
                IF srcW > dstW
                  THEN initVal := MSIR.BuildTrunc (blk, "", initVal, slotT);
                  ELSE initVal := MSIR.BuildZExt  (blk, "", initVal, slotT);
                END;
              END;
              MSIR.BuildStore (blk, initVal, addr);
            END;
          END;
        END;
      END;
    END;
  END ForceInitMSIR;

(* EXPORTED *)
PROCEDURE CopyOpenArray (arrayType: Type.T;  refType: Type.T) =
(* PRE: Pointer to array dope is on TOS. *)
(* Generate code to heap-allocate the copy. *)
(* POST: TOS replaced by pointer to dope of copy. *) 
  VAR
    oldDopePtr, newDopePtr : CG.Val;
    depth := OpenArrayType.OpenDepth (arrayType);
    align := MAX (OpenArrayType.EltAlign (arrayType), Target.Word8.align);
    pack  := OpenArrayType.EltPack (arrayType);
    sizes := CG.Declare_temp (Target.Address.pack + Target.Integer.pack,
                              Target.Address.align, CG.Type.Struct,
                              in_memory := TRUE);
    proc  : Procedure.T;
  BEGIN
    oldDopePtr := CG.Pop (); 
    (* This is confusing.  Build a new 1-D dope vector that treats the shape
       portion of the to-be-copied dope vector as an open array. *) 
    CG.Push(oldDopePtr);
    CG.Add_offset (M3RT.OA_sizes);
    CG.Store_addr (sizes, M3RT.OA_elt_ptr);
    CG.Load_intt (depth);
    CG.Store_int (Target.Integer.cg_type, sizes, M3RT.OA_size_0);
        
    (* allocate the storage *)
    proc := RunTyme.LookUpProc (RunTyme.Hook.NewTracedArray);
    Procedure.StartCall (proc);
    IF Target.DefaultCall.args_left_to_right THEN
      Type.LoadInfo (refType, -1);
      CG.Pop_param (CG.Type.Addr);
      CG.Load_addr_of (sizes, 0, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
    ELSE
      CG.Load_addr_of (sizes, 0, Target.Address.align);
      CG.Pop_param (CG.Type.Addr);
      Type.LoadInfo (refType, -1);
      CG.Pop_param (CG.Type.Addr);
    END;
    newDopePtr := Procedure.EmitValueCall (proc);

    (* load the destination and source elements' addresses *)
    CG.Push (newDopePtr);
    CG.Boost_addr_alignment (Target.Address.align);
    CG.Open_elt_ptr (align); (* Addr of the new elements. *)
    CG.ForceStacked ();
    CG.Push(oldDopePtr);
    CG.Open_elt_ptr (align); (* Addr of the old elements. *)
    CG.ForceStacked ();

    (* compute the number of elements *)
    FOR i := 0 TO depth - 1 DO
      CG.Push(oldDopePtr); 
      CG.Open_size (i);
      IF (i # 0) THEN CG.Multiply (Target.Word.cg_type) END;
    END;

    (* copy the elements into the new storage *)
    CG.Copy_n (pack, overlap := FALSE);

    (* Push new dope pointer for the caller. *) 
    CG.Push (newDopePtr);
    CG.Boost_addr_alignment (Target.Address.align);

    (* free our temps *)
    CG.Free_temp (sizes);
    CG.Free (oldDopePtr);
    CG.Free (newDopePtr); (* It's now safely on the stack, so this is OK. *) 
  END CopyOpenArray;

(* Externally dispatched-to *)
PROCEDURE UserInit (t: T) =
  VAR constInitExpr: Expr.T;
  VAR initRepType: Type.T;
  VAR openEltAlign: INTEGER;
  BEGIN
    IF (t.initExpr # NIL) AND (NOT t.initDone) AND (NOT t.imported) THEN
      CG.Gen_location (t.origin);
      IF (t.initZero) THEN
        t.initPending := FALSE;
        LoadLValue (t);
        Type.Zero (t.type);
        (* MSIR: store zero constant using the actual alloca type (i64 for ordinals,
           not MSIRType.Translate which returns i1 for BOOLEAN). *)
        IF NOT t.global AND MSIRBuilder.InProc () THEN
          VAR addr  := MSIRBuilder.LookupVarAddr (t);
              slotT : MSIR.T;
              zero  : MSIR.Value;
          BEGIN
            IF addr # NIL THEN
              slotT := MSIR.EltType (MSIR.ValueType (addr));
              IF slotT # NIL THEN
                zero := MSIR.ConstZero (slotT);
                IF zero # NIL THEN
                  MSIR.BuildStore (MSIRBuilder.CurrentBlock (), zero, addr);
                END;
              END;
            END;
          END;
        END;
      ELSIF t.initAllocated THEN
        t.initPending := FALSE;
        IF Expr.CheckUseFailure (t.initExpr) THEN
          LoadLValue (t);
          Module.LoadGlobalAddr
            (Scope.ToUnit (t), t.initValOffset, is_const := TRUE);
          constInitExpr := Expr.ConstValue (t.initExpr);
          <* ASSERT constInitExpr # NIL *>
          initRepType := Expr.RepTypeOf (constInitExpr);
          IF OpenArrayType.Is (initRepType) THEN
            openEltAlign
              := MAX (OpenArrayType.EltAlign(initRepType), Target.Word8.align);
            CG.Open_elt_ptr (openEltAlign);
          END;
          CG.Copy (t.size, overlap := FALSE);
        ELSE
          (* Expr.CheckUseFailure will have generated an unconditional RT error. *)
        END;
        (* MSIR: memcpy from the initializer lvalue to the local alloca.  Some
           value-typed const initializers (notably multi-word SETs) have no
           lvalue — fall back to compiling the value and storing it.  This also
           routes a const initializer carrying an RTErrorCode (e.g. an
           out-of-range SET element folded to a constant) through CompileMSIR,
           which emits the unconditional runtime fault the CG path raises via
           CheckUseFailure. *)
        IF NOT t.global AND MSIRBuilder.InProc () THEN
          VAR lval      := Expr.LValueMSIR (t.initExpr);
              addr      := MSIRBuilder.LookupVarAddr (t);
              byteCount := t.size DIV Target.Char.size;
          BEGIN
            IF lval # NIL AND addr # NIL AND byteCount > 0 THEN
              MSIRBuilder.EmitMemcpy (addr, lval, byteCount);
            ELSIF lval = NIL AND addr # NIL THEN
              VAR initVal := Expr.CompileMSIR (t.initExpr);
              BEGIN
                IF initVal # NIL
                   AND MSIR.Kind (MSIR.ValueType (addr)) # MSIR.TypeKind.GcSlot
                   AND NOT MSIRBuilder.OpenArrayToFixedStore (addr, initVal, t.type)
                THEN
                  VAR slotT := MSIR.EltType (MSIR.ValueType (addr));
                      blk   := MSIRBuilder.CurrentBlock ();
                      srcW  := MSIR.BitWidth (MSIR.ValueType (initVal));
                      dstW  := MSIR.BitWidth (slotT);
                  BEGIN
                    IF srcW > 0 AND dstW > 0 AND srcW # dstW THEN
                      IF srcW > dstW
                        THEN initVal := MSIR.BuildTrunc (blk, "", initVal, slotT);
                        ELSE initVal := MSIR.BuildZExt  (blk, "", initVal, slotT);
                      END;
                    END;
                    MSIR.BuildStore (blk, initVal, addr);
                  END;
                END;
              END;
            END;
          END;
        END;
      ELSE
        t.initPending := FALSE;
        ArrayExpr.NoteUseTargetVar (t.initExpr);
        AssignStmt.PrepForEmit (t.type, t.initExpr, initializing := TRUE);
        LoadLValue (t);
        AssignStmt.DoEmit (t.type, t.initExpr, t.cg_align, initializing := TRUE);
        (* MSIR: compile and store the initializer expression *)
        IF MSIRBuilder.InProc () THEN
          VAR initVal := Expr.CompileMSIR (t.initExpr);
              addr    := MSIRBuilder.LookupVarAddr (t);
          BEGIN
            IF initVal # NIL AND addr # NIL THEN
              IF MSIR.Kind (MSIR.ValueType (addr)) = MSIR.TypeKind.GcSlot THEN
                MSIR.BuildGcStore (MSIRBuilder.CurrentBlock (), addr, initVal);
              ELSIF NOT MSIRBuilder.OpenArrayToFixedStore (addr, initVal, t.type) THEN
                VAR slotT := MSIR.EltType (MSIR.ValueType (addr));
                    blk   := MSIRBuilder.CurrentBlock ();
                    srcW  := MSIR.BitWidth (MSIR.ValueType (initVal));
                    dstW  := MSIR.BitWidth (slotT);
                BEGIN
                  IF srcW > 0 AND dstW > 0 AND srcW # dstW THEN
                    IF srcW > dstW
                      THEN initVal := MSIR.BuildTrunc (blk, "", initVal, slotT);
                      ELSE initVal := MSIR.BuildZExt  (blk, "", initVal, slotT);
                    END;
                  END;
                  MSIR.BuildStore (blk, initVal, addr);
                END;
              END;
            END;
          END;
          t.msirInitDone := TRUE;  (* MSIR init done; ForceInitMSIR must not re-emit *)
        END;
        (* If InProc() was FALSE (CG compiled this before MSIR proc context),
           leave msirInitDone FALSE so the fallback below can emit the MSIR init. *)
      END;
      t.initDone := TRUE;
      Tracer.Schedule (t.trace);
    END;
    (* MSIR: non-global locals whose ELSE-branch CG UserInit ran with InProc=FALSE
       (the CG's ProcBody.EmitAll fires before GenBodyMSIR sets up the MSIR proc
       context) leave msirInitDone=FALSE.  When GenBodyMSIR/BlockStmt.CompileMSIR
       calls UserInit again, initDone=TRUE skips the block above, but InProc is now
       TRUE.  This fallback emits the deferred MSIR init for those locals. *)
    IF FALSE AND NOT t.global AND NOT t.imported AND NOT t.indirect AND (t.formal = NIL)
       AND t.initDone AND NOT t.msirInitDone AND (t.initExpr # NIL)
       AND NOT t.initZero AND NOT t.initAllocated
       AND MSIRBuilder.InProc () THEN
      VAR initVal := Expr.CompileMSIR (t.initExpr);
          addr    := MSIRBuilder.LookupVarAddr (t);
      BEGIN
        IF initVal # NIL AND addr # NIL THEN
          IF MSIR.Kind (MSIR.ValueType (addr)) = MSIR.TypeKind.GcSlot THEN
            MSIR.BuildGcStore (MSIRBuilder.CurrentBlock (), addr, initVal);
          ELSIF NOT MSIRBuilder.OpenArrayToFixedStore (addr, initVal, t.type) THEN
            VAR slotT := MSIR.EltType (MSIR.ValueType (addr));
                blk   := MSIRBuilder.CurrentBlock ();
                srcW  := MSIR.BitWidth (MSIR.ValueType (initVal));
                dstW  := MSIR.BitWidth (slotT);
            BEGIN
              IF srcW > 0 AND dstW > 0 AND srcW # dstW THEN
                IF srcW > dstW
                  THEN initVal := MSIR.BuildTrunc (blk, "", initVal, slotT);
                  ELSE initVal := MSIR.BuildZExt  (blk, "", initVal, slotT);
                END;
              END;
              MSIR.BuildStore (blk, initVal, addr);
            END;
          END;
        END;
      END;
      t.msirInitDone := TRUE;
    END;
    (* NOTE: module-global CONSTANT initializers are NOT emitted here.  In MSIRObj
       mode DeclareGlobalMSIR records them in the early @MSIR_InitGlobals ctor
       (runs before RTLinker, like the C backend's static data); in the C-
       authoritative diagnostic path CG's ConstInit does the static init.  A late
       module-body store here would run AFTER RTLinker.FixTypes and clobber
       runtime-populated tables (RTType's InfoMap: cnt 131 -> 0 -> MissingType). *)
  END UserInit;

(* EXPORTED *)
PROCEDURE GenGlobalMap (s: Scope.T): INTEGER =
  (* generate the garbage collector's map-proc for the variables of s *)
  VAR started := FALSE;  info: Type.Info;  v := Scope.ToList (s);
  BEGIN
    WHILE (v # NIL) DO
      TYPECASE Value.Base (v) OF
      | NULL =>  (* do nothing *)
      | T(t) =>  IF (NOT t.imported)
                   AND (NOT t.external) THEN
                   EVAL Type.CheckInfo (t.type, info);
                   IF (info.isTraced) THEN
                     IF (NOT started) THEN
                       TipeMap.Start ();
                       started := TRUE;
                     END;
                     t.used := TRUE;
                     Value.Declare (t);
                     IF (t.indirect) THEN
                       TipeMap.Add (t.offset, TipeMap.Op.PushPtr, 0);
                       Type.GenMap (t.type, 0, -1, refs_only := TRUE);
                       TipeMap.Add (t.size, TipeMap.Op.Return, 0);
                       TipeMap.SetCursor (t.offset + Target.Address.size);
                     ELSE
                       Type.GenMap (t.type, t.offset, -1, refs_only := TRUE);
                     END;
                   END;
                 END;
      ELSE (* do nothing *)
      END;
      v := v.next;
    END;
    IF (started)
      THEN RETURN TipeMap.Finish ("global type map");
      ELSE RETURN -1;
    END;
  END GenGlobalMap;

(* EXPORTED *)
PROCEDURE NeedGlobalInit (t: T): BOOLEAN =
  BEGIN
    RETURN (NOT t.initDone) AND (NOT t.external);
  END NeedGlobalInit;

(* EXPORTED *)
PROCEDURE InitGlobal (t: T) =
  BEGIN
    IF (NOT t.initDone) AND (NOT t.external) THEN
      LoadLValue (t);
      Type.InitValue (t.type, TRUE);
    END;
  END InitGlobal;

(* Externally dispatched-to *)
PROCEDURE AddFPTag (t: T;  VAR x: M3.FPInfo): CARDINAL =
  BEGIN
    ValueRep.FPStart (t, x, "VAR ", t.offset, global := TRUE);
    RETURN 1;
  END AddFPTag;

(*--------------------------------------------------------- trace support ---*)

TYPE TraceNode = Tracer.T OBJECT
                   handler : Expr.T := NIL;
                   call    : Expr.T := NIL;
                 OVERRIDES
                   apply := DoTrace;
                 END;

(* EXPORTED *)
PROCEDURE ParseTrace (): Tracer.T =
  TYPE TK = Token.T;
  VAR e: Expr.T;
  BEGIN
    IF (cur.token # TK.tTRACE) THEN RETURN NIL END;
    Match (TK.tTRACE);
    e := Expr.Parse ();
    Match (TK.tENDPRAGMA);
    IF (e = NIL) THEN RETURN NIL END;
    RETURN NEW (TraceNode, handler := e);
  END ParseTrace;

(* EXPORTED *)
PROCEDURE BindTrace (t: T;  xx: Tracer.T) =
  VAR x: TraceNode := xx;  p: Scope.IDStack;  z: M3String.T;  args: Expr.List;
  BEGIN
    IF (xx = NIL) THEN RETURN END;

    IF (x.call # NIL) THEN
      x := NEW (TraceNode, handler := x.handler);
    END;

    (* get the variable's full name *)
    p.top := 0;
    Scope.NameToPrefix (t, p, dots := TRUE, with_module := TRUE);
    z := M3String.Add (Scope.StackToText (p));

    (* build the trace procedure call *)
    args := NEW (Expr.List, 2);
    args[0] := TextExpr.New8 (z);
    args[1] := NamedExpr.FromValue (t);
    x.call  := CallExpr.New (x.handler, args);

    <*ASSERT t.trace = NIL*>
    t.trace := x;
  END BindTrace;

PROCEDURE DoTrace (x: TraceNode) =
  BEGIN
    Expr.Prep (x.call);
    Expr.Compile (x.call);
  END DoTrace;

(* EXPORTED *)
PROCEDURE CheckTrace (tt: Tracer.T;  VAR cs: Value.CheckState) =
  VAR x: TraceNode := tt;
  BEGIN
    IF (x # NIL) THEN
      Expr.TypeCheck (x.handler, cs);
      Expr.TypeCheck (x.call, cs);
    END;
  END CheckTrace;

(* EXPORTED *)
PROCEDURE ScheduleTrace (t: T) =
  BEGIN
    Tracer.Schedule (t.trace);
  END ScheduleTrace;

BEGIN
  debug := RTParams.IsPresent ("m3front-debug-variable");
END Variable.
