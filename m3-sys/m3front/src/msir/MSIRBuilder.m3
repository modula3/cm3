MODULE MSIRBuilder;

IMPORT MSIR, MSIRType, MSIREmit, MSIRBitLoc;
IMPORT M3ID, Type, Value, Formal, Variable, Scope, ProcType, Fmt, Target, Text;
IMPORT Error;
IMPORT RunTyme, Procedure, M3FP, CaptureAnalysis, M3RT;
IMPORT PackedType, TInt;
IMPORT Scanner;
IMPORT RefType, Exceptionz, Host, M3CG;

CONST MaxVarMap    = 512;  (* EmitInsn alone has ~108 local vars; 512 gives headroom *)
CONST MaxExitStack = 16;
CONST MaxCleanup   = 64;   (* unified loop/finally cleanup-frame stack *)
(* Initial sizes of the module-wide maps, which grow on demand: a module has
   no procedure to abandon, so an entry must never be dropped. *)
CONST InitProcMap   = 256;
CONST InitShimMap   = 64;
CONST InitGlobalMap = 256;
(* TRY/FINALLY selector codes Sel_Normal/Sel_Exc/Sel_Exit are exported from the
   MSIRBuilder interface and visible here without redeclaration. *)
CONST MaxNestDepth  = 16;  (* maximum nesting depth for nested procs *)

(* Each formal maps to a Param SSA value (elemType = NIL).
   Each local maps to an alloca ptr (elemType = the allocated type). *)
TYPE VarEntry = RECORD
  key:         Variable.T;
  val:         MSIR.Value;
  storageType: MSIR.T;     (* MType: narrow memory type; NIL => by-value formal *)
  wideType:    MSIR.T;     (* ZType: computation type; = TI64 for narrow ordinals *)
  signedWiden: BOOLEAN;    (* TRUE → SExt on widen; FALSE → ZExt (lo >= 0 or BOOLEAN) *)
  (* Bit-field VIEW binding: a WITH alias over a designator with no byte
     address (`WITH s = rec.bitfield`, `WITH e = packed[i]`).  bfLoc holds
     its location, the base captured once at WITH entry; reads go through
     ExtractBitLoc and writes through InsertBitLoc, matching the C backend's
     by-reference alias. *)
  isBitField:  BOOLEAN := FALSE;
  bfLoc:       MSIRBitLoc.T;
  (* WITH designator alias over a HEAP lvalue: the containing heap object,
     the designator's container at bind time (Expr.HeapLValueMSIR).  Returned
     by VarContainer so a store THROUGH the alias carries the
     generational/incremental write barrier — Table.mg Put's
     `WITH first = tbl.buckets[h] DO ... first := NEW(...)` emitted gc.store
     with no container, the young EntryList was missed by the minor GC, and
     the stage-2 driver crashed on stale TextIntTbl keys. *)
  container:   MSIR.Value := NIL;
END;

(* One frame of the EXIT/RETURN routing stack: a loop, or the protected
   body of a TRY with an ELSE (whose ELSE catches EXIT and RETURN). *)
TYPE CleanupKind = {Loop, Else};
TYPE CleanupFrame = RECORD
  kind:       CleanupKind;
  exitBlock:  MSIR.Block := NIL;   (* Loop: branch target for EXIT;
                                      Else: the ELSE code *)
END;

(* Saved state for one level of proc context (for nested proc compilation). *)
TYPE ProcContext = RECORD
  proc:          MSIR.Proc;
  block:         MSIR.Block;
  abandoned:     BOOLEAN;
  blockSeq:      INTEGER;
  varMapN:       INTEGER;
  varMap:        ARRAY [0..MaxVarMap-1] OF VarEntry;
  exitDepth:     INTEGER;
  scope:         MSIR.Scope;   (* curScope *)
  cleanupDepth:  INTEGER;
END;

TYPE ProcEntry   = RECORD
  key:  Value.T;
  val:  MSIR.Proc;
END;
TYPE GlobalEntry = RECORD
  key:         Variable.T;
  val:         MSIR.Global;
  needsLoad:   BOOLEAN := FALSE;  (* TRUE for large indirect globals: struct holds ptr-to-data *)
  dataType:    MSIR.T  := NIL;   (* actual element type for indirect globals; used for typed ptr *)
  importBind:  TEXT    := NIL;   (* non-NIL for import chain entries: owning module binder name *)
  varByteOff:  INTEGER := 0;     (* byte offset of variable in imported module's interface struct *)
  varMSIRType: MSIR.T  := NIL;  (* stored element type for import chain entries *)
END;

VAR
  curProc:          MSIR.Proc  := NIL;
  curBlock:         MSIR.Block := NIL;
  abandoned:        BOOLEAN    := FALSE;
  blockSeq:         INTEGER    := 0;

  (* Saved contexts for nested proc compilation. *)
  procContextStack: ARRAY [0..MaxNestDepth-1] OF ProcContext;
  procContextDepth: INTEGER := 0;

  varMap:  ARRAY [0..MaxVarMap-1]  OF VarEntry;
  varMapN: INTEGER := 0;

  exitStack: ARRAY [0..MaxExitStack-1] OF MSIR.Block;
  exitDepth: INTEGER := 0;

  (* The EH scope new blocks go in (D26); NIL: the proc's root. *)
  curScope: MSIR.Scope := NIL;

  (* EXIT/RETURN routing frames in nesting order: loops (PushExitBlock) and
     the protected bodies of TRYs with an ELSE (PushElseFrame). *)
  cleanupStack: ARRAY [0..MaxCleanup-1] OF CleanupFrame;
  cleanupDepth: INTEGER := 0;

  procMap:  REF ARRAY OF ProcEntry := NEW (REF ARRAY OF ProcEntry, InitProcMap);
  procMapN: INTEGER := 0;


TYPE ShimEntry = RECORD key: Value.T; val: MSIR.Proc END;
VAR
  shimMap:  REF ARRAY OF ShimEntry := NEW (REF ARRAY OF ShimEntry, InitShimMap);
  shimMapN: INTEGER := 0;

VAR globalMap:  REF ARRAY OF GlobalEntry :=
                  NEW (REF ARRAY OF GlobalEntry, InitGlobalMap);
  globalMapN: INTEGER := 0;

VAR
  constArraySeq:  INTEGER := 0;

  memcpyProc: MSIR.Proc := NIL;  (* lazy C memcpy stub *)
  memcmpProc: MSIR.Proc := NIL;  (* lazy C memcmp stub *)
  memsetProc: MSIR.Proc := NIL;  (* lazy C memset stub *)

PROCEDURE IsScalarType(mt: MSIR.T): BOOLEAN =
  (* TRUE for types safe to pass by value as a capture param.
     Integer and float widths are pure values; Ptr covers UNTRACED REF, ADDRESS,
     and procedure values (addrspace 0).  GcRef (addrspace 1) is excluded: traced
     references must remain on the stack so the conservative GC scanner finds them. *)
  BEGIN
    CASE MSIR.Kind(mt) OF
    | MSIR.TypeKind.I1,  MSIR.TypeKind.I8,  MSIR.TypeKind.I16, MSIR.TypeKind.I32,
      MSIR.TypeKind.I64, MSIR.TypeKind.W8,  MSIR.TypeKind.W16, MSIR.TypeKind.W32,
      MSIR.TypeKind.W64, MSIR.TypeKind.F32, MSIR.TypeKind.F64, MSIR.TypeKind.F128,
      MSIR.TypeKind.Ptr, MSIR.TypeKind.Enum => RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END IsScalarType;

(* Called on failed BeginProc after we've already pushed the outer context.
   Restores the outer proc's state so the caller can continue normally. *)
PROCEDURE PopBeginContext() =
  BEGIN
    IF procContextDepth <= 0 THEN RETURN END;
    DEC(procContextDepth);
    WITH ctx = procContextStack[procContextDepth] DO
      curProc          := ctx.proc;
      curBlock         := ctx.block;
      abandoned        := ctx.abandoned;
      blockSeq         := ctx.blockSeq;
      varMapN          := ctx.varMapN;
      exitDepth        := ctx.exitDepth;
      curScope         := ctx.scope;
      cleanupDepth     := ctx.cleanupDepth;
      FOR i := 0 TO varMapN - 1 DO varMap[i] := ctx.varMap[i] END;
    END;
  END PopBeginContext;

PROCEDURE BeginProc(name: TEXT;
                    formals: Value.T;
                    syms: Scope.T;
                    result: Type.T;
                    isExternal: BOOLEAN;
                    captures: CaptureAnalysis.T := NIL): BOOLEAN =
  VAR
    info:          Formal.Info;
    nFormals:      INTEGER := 0;
    nCaptures:     INTEGER := 0;
    f:             Value.T;
    resultT:       MSIR.T;
    isNested:      BOOLEAN;
    pBase:         INTEGER;  (* param index offset for formals = nCaptures *)
    caps:          REF ARRAY OF CaptureAnalysis.Capture;
    reuseProc:     MSIR.Proc;
  BEGIN
    IF NOT MSIREmit.IsEnabled() THEN RETURN FALSE END;
    (* Push current state if we're already inside a proc (nested proc). *)
    IF curProc # NIL THEN
      IF procContextDepth >= MaxNestDepth THEN
        MSIREmit.NoteSkipped(name, "nesting too deep");
        RETURN FALSE;
      END;
      WITH ctx = procContextStack[procContextDepth] DO
        ctx.proc       := curProc;
        ctx.block      := curBlock;
        ctx.abandoned  := abandoned;
        ctx.blockSeq   := blockSeq;
        ctx.varMapN    := varMapN;
        ctx.exitDepth  := exitDepth;
        ctx.scope      := curScope;
        ctx.cleanupDepth := cleanupDepth;
        FOR i := 0 TO varMapN - 1 DO ctx.varMap[i] := varMap[i] END;
      END;
      INC(procContextDepth);
      (* The caller (Procedure.GenBodyMSIR) now passes the canonical
         Value.GlobalName — already fully qualified (module + enclosing procs +
         a scope number for shadowed siblings), matching the call site's
         LookupOrCreateProc name.  So no parent-prefix reconstruction here; doing
         it would double-qualify and (for shadowed procs) still diverge from the
         call site. *)
    END;
    abandoned      := FALSE;
    varMapN        := 0;
    exitDepth      := 0;
    curScope       := NIL;
    cleanupDepth   := 0;
    blockSeq       := 0;

    resultT := MSIRType.TranslateResult(result);
    IF resultT = NIL THEN
      MSIREmit.NoteSkipped(name, "unsupported result type");
      PopBeginContext();
      RETURN FALSE;
    END;
    (* Native large-result ABI: MSIR returns large results BY VALUE (`ret <agg>`;
       LLVM applies the target aggregate-return ABI — an x8 sret slot on AArch64
       — uniformly) instead of CG's hidden result pointer as an explicit first
       argument.  All M3 code is recompiled under this convention, and the
       C<->M3 boundary carries no large-by-value result (C returns scalars and
       struct POINTERS), so nothing external is broken and every call path treats
       a large result exactly like a small one.  resultT keeps the real
       aggregate type from TranslateResult above; there is no hidden result
       param, so a proc's params are just [captures…, formals…]. *)

    isNested  := procContextDepth > 0;
    IF captures = NIL THEN caps := NIL
    ELSE caps := CaptureAnalysis.GetCaptures(captures)
    END;
    IF caps = NIL THEN nCaptures := 0
    ELSE nCaptures := NUMBER(caps^)
    END;
    pBase := nCaptures;

    f := formals;
    WHILE f # NIL DO INC(nFormals); f := f.next END;

    VAR params := NEW(REF ARRAY OF MSIR.Param, nCaptures + nFormals);
    BEGIN
      (* Lambda-lifted capture params.
         Read-only scalar captures pass by value (Integer, Float, Ptr).
         Written or aggregate captures pass a typed ptr so callers can do
         typed GEP/subscript without needing a fallback retype heuristic. *)
      FOR i := 0 TO nCaptures - 1 DO
        VAR v:  Variable.T := caps[i].var;
            vt: Type.T;  vg, vi, vlhs: BOOLEAN;
            mt: MSIR.T;
        BEGIN
          Variable.Split(v, vt, vg, vi, vlhs);
          mt := MSIRType.Translate(vt);
          params[i].name := "__cap_" & Fmt.Int(i);
          params[i].mode := MSIR.ParamMode.ByValue;
          IF NOT caps[i].written AND mt # NIL AND IsScalarType(mt) THEN
            (* By-value scalar capture: use ZType (computation width), not MType.
               The value passed in is always ZType (LookupVar widens; the outer
               var's own slot is ZType), so the param, the closure-env spill
               slot, and the shim's reload must all be ZType to match (p048). *)
            params[i].type := MSIRType.ComputeType(vt);  (* by value *)
          ELSIF mt # NIL THEN
            params[i].type := MSIR.TPtr(mt); (* typed ptr to storage *)
          ELSE
            params[i].type := MSIR.TPtr(MSIR.TVoid()); (* unknown type *)
          END;
        END;
      END;
      (* Regular explicit formals, shifted past capture params. *)
      f := formals;
      FOR i := 0 TO nFormals - 1 DO
        Formal.Split(f, info);
        VAR pt := MSIRType.Translate(info.type);
        BEGIN
          IF pt = NIL THEN
            MSIREmit.NoteSkipped(name, "unsupported formal type");
            PopBeginContext();
            RETURN FALSE;
          END;
          params[i + pBase].name := "a." & M3ID.ToText(info.name);
          CASE info.mode OF
          | Formal.Mode.mVALUE =>
              params[i + pBase].mode := MSIR.ParamMode.ByValue;
              IF MSIR.Kind(pt) = MSIR.TypeKind.OpenArray THEN
                params[i + pBase].type := MSIR.TPtr(pt);
              ELSE
                params[i + pBase].type := pt;
              END;
          | Formal.Mode.mVAR      => params[i + pBase].mode := MSIR.ParamMode.Var;
                                     params[i + pBase].type := MSIR.TPtr(pt);
          | Formal.Mode.mREADONLY =>
              (* READONLY formals are passed BY REFERENCE (the formal's
                 indirect=TRUE; the C backend does the same).  Declare every
                 READONLY param as a pointer — not just aggregates — so a scalar
                 READONLY designator aliases its actual (p031).  The caller
                 (Formal.EmitArgMSIR/ReadonlyArgAddrMSIR) passes an address, and
                 BindFormalMSIR binds it by-ref (loads through the pointer). *)
              params[i + pBase].mode := MSIR.ParamMode.Readonly;
              params[i + pBase].type := MSIR.TPtr(pt);
          END;
        END;
        f := f.next;
      END;

      (* If LookupOrCreateProc already built a bodyless stub with this name
         (because the proc was called before its body was compiled), reuse
         that stub as curProc so the definition and the call-site reference
         the same MSIR.Proc object and end up at the same LLVM symbol.
         Only fall through to the uniqueness rename when no bodyless stub
         exists — that path handles genuinely duplicate proc names (e.g.
         sequential BEGIN...END blocks that happen to share a generated name). *)
      reuseProc := NIL;
      FOR i := 0 TO procMapN - 1 DO
        VAR pn := MSIR.ProcName(procMap[i].val); BEGIN
          IF pn # NIL AND Text.Equal(pn, name)
             AND MSIR.ProcBlockCount(procMap[i].val) = 0 THEN
            reuseProc := procMap[i].val;
            EXIT;
          END;
        END;
      END;
      IF reuseProc # NIL THEN
        curProc := reuseProc;
        (* The stub was built by LookupOrCreateProc from the source formals only
           (nFormals); the real definition also has lambda-lifted
           capture params.  Widen the reused stub's param list to the full set
           so the capture/formal bindings below (ProcParam by index) are in
           range and the definition's signature matches the call site (whose
           capture args came from the pre-registered capture list). *)
        MSIR.ProcSetParams(curProc, params^);
      ELSE
        (* Ensure unique LLVM function name; multiple Modula-3 procs of the
           same name at the same nesting level would produce an "invalid
           redefinition" llc error. *)
        VAR uniqueName := name;  nameCounter := 1;
        BEGIN
          LOOP
            VAR clash := FALSE;
            BEGIN
              FOR i := 0 TO procMapN - 1 DO
                VAR pn := MSIR.ProcName(procMap[i].val); BEGIN
                  IF pn # NIL AND Text.Equal(pn, uniqueName) THEN
                    clash := TRUE;  EXIT;
                  END;
                END;
              END;
              IF NOT clash THEN EXIT END;
            END;
            INC(nameCounter);
            uniqueName := name & "__" & Fmt.Int(nameCounter);
          END;
          name := uniqueName;
        END;
        curProc := MSIR.NewProc(name, params^, resultT);
      END;
      curBlock := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
      MSIR.ProcAddBlock(curProc, curBlock);
      IF NOT isExternal THEN
        MSIR.ProcSetLinkage(curProc, MSIR.Linkage.Internal);
      END;
      VAR srcFile: TEXT;  srcLine: INTEGER;
      BEGIN
        Scanner.Here(srcFile, srcLine);
        IF srcFile # NIL AND srcLine > 0 THEN
          MSIR.ProcSetSrcLoc(curProc, srcFile, srcLine);
          MSIR.SetCurrentSrcLine(srcLine);
        END;
      END;

      (* For nested procs: bind capture params in the inner proc's varMap.
         Read-only scalar captures: param holds the value directly (elemType=NIL).
         Written or aggregate captures: param holds a ptr; loads go through it.
         Use nCaptures > 0 (not isNested) because GenBody may call BeginProc
         at procContextDepth=0 — after the outer proc's EndProc — even for
         genuinely nested procs that have capture params. *)
      IF nCaptures > 0 THEN
        FOR i := 0 TO nCaptures - 1 DO
          VAR v:  Variable.T := caps[i].var;
              vt: Type.T;  vg, vi, vlhs: BOOLEAN;
              mt: MSIR.T;
          BEGIN
            Variable.Split(v, vt, vg, vi, vlhs);
            mt := MSIRType.Translate(vt);
            IF mt # NIL AND varMapN < MaxVarMap THEN
              varMap[varMapN].key := v;
              varMap[varMapN].isBitField := FALSE;  (* slot reused: clear stale flag *)
              varMap[varMapN].val := MSIR.ProcParam(curProc, i);
              IF NOT caps[i].written AND IsScalarType(mt) THEN
                varMap[varMapN].storageType := NIL;  (* by-value: return param directly *)
                varMap[varMapN].wideType    := NIL;
                varMap[varMapN].signedWiden := FALSE;
                varMap[varMapN].container   := NIL;
              ELSE
                varMap[varMapN].storageType := mt;   (* ptr: load through it *)
                varMap[varMapN].wideType    := mt;
                varMap[varMapN].signedWiden := FALSE;
                varMap[varMapN].container   := NIL;
              END;
              INC(varMapN);
            END;
          END;
        END;
      END;

      (* Bind formals and locals in one pass over the proc scope, mirroring the
         CG path's scope enumeration.  Do NOT resolve formals via Scope.LookUp:
         LookUp marks the symbol used and, while Scanner.in_main, spuriously
         fires the "<*UNUSED*> symbol used" warning for <*UNUSED*> formals (a
         diff the CG path never produces — p269/p270/p280).  BindFormalMSIR
         self-locates its parameter by name, so scope-list order is fine. *)
      VAR sv: Value.T := Scope.ToList(syms);
      BEGIN
        WHILE sv # NIL DO
          TYPECASE sv OF
          | Variable.T(svv) =>
              IF Variable.IsFormal(svv) THEN
                Variable.BindFormalMSIR(svv, curProc, curBlock);
              ELSE
                EVAL Variable.AddLocalMSIR(svv, curBlock);
              END;
          ELSE
          END;
          sv := sv.next;
        END;
      END;
    END;
    RETURN TRUE;
  END BeginProc;

PROCEDURE LookupVar(v: Variable.T): MSIR.Value =
  (* Centralized ordinal-read widening: every variable VALUE read — local,
     global, formal, WITH-alias, captured, via VarExpr OR NamedExpr OR a
     qualified reference — returns the ZType (machine width).  Local allocas
     already widen internally; globals/captured-by-value return narrow MType, so
     widen here.  This is the single choke point that lets binops assume uniform
     machine-width operands (no per-op coercion band-aids). *)
  VAR res := LookupVarRaw(v);
      t   := Value.TypeOf(v);
      zt  : MSIR.T;
  BEGIN
    IF res = NIL OR t = NIL THEN RETURN res END;
    zt := MSIRType.ComputeType(t);
    IF zt # NIL AND MSIR.BitWidth(zt) > 0
       AND MSIR.BitWidth(MSIR.ValueType(res)) > 0
       AND NOT MSIR.Equal(zt, MSIR.ValueType(res)) THEN
      res := CoerceToMSIR(CurrentBlock(), res, zt);
    END;
    RETURN res;
  END LookupVar;

PROCEDURE LookupVarRaw(v: Variable.T): MSIR.Value =
  VAR gv: MSIR.Value;  gt: MSIR.T;
  BEGIN
    FOR i := 0 TO varMapN - 1 DO
      IF varMap[i].key = v THEN
        IF varMap[i].isBitField THEN
          (* Bit-field WITH alias: extract from the captured base each read. *)
          RETURN ExtractBitLoc(varMap[i].bfLoc);
        ELSIF varMap[i].storageType = NIL THEN
          RETURN varMap[i].val;   (* by-value formal: return param directly *)
        ELSE
          (* Local alloca: load as storageType (MType), widen to wideType (ZType).
             WITH aliases bound to a heap GcSlot (e.g. REF ARRAY element) need
             gc.load (read barrier), not a plain load. *)
          IF MSIR.Kind(MSIR.ValueType(varMap[i].val)) = MSIR.TypeKind.GcSlot THEN
            RETURN MSIR.BuildGcLoad(curBlock, "", varMap[i].val);
          END;
          (* Traced ref read through NON-LOCAL storage: byref formals
             (VAR/READONLY parameters — pointers into caller storage that may
             be heap) and WITH designator aliases bound to global or heap
             lvalues (BindVarAddr: `WITH wr = Stdio.stdout` in MxGen.Msg was
             a plain gc_ref load — rb-audit).  Needs the incremental-GC read
             barrier, like CG's NamedExpr global/indirect case.  Only a
             function-local alloca slot is exempt — its refs were barriered
             when acquired and stacks are scanned conservatively. *)
          IF MSIR.Kind(varMap[i].storageType) = MSIR.TypeKind.GcRef THEN
            VAR isLocalSlot := FALSE;
            BEGIN
              IF MSIR.GetValueKind(varMap[i].val) = MSIR.ValueKind.InsnResult THEN
                VAR d := MSIR.ValueDefInsn(varMap[i].val);
                BEGIN
                  isLocalSlot := d # NIL AND MSIR.InsnOp(d) = MSIR.Op.Alloca;
                END;
              END;
              IF NOT isLocalSlot THEN
                RETURN MSIR.BuildGcLoad(curBlock, "",
                         MSIR.RetypeValue(varMap[i].val,
                           MSIR.TGcSlot(MSIR.EltType(varMap[i].storageType))));
              END;
            END;
          END;
          VAR loaded := MSIR.BuildLoad(curBlock, "", varMap[i].storageType, varMap[i].val);
          BEGIN
            IF NOT MSIR.Equal(varMap[i].storageType, varMap[i].wideType) THEN
              IF varMap[i].signedWiden
                THEN RETURN MSIR.BuildSExt(curBlock, "", loaded, varMap[i].wideType)
                ELSE RETURN MSIR.BuildZExt(curBlock, "", loaded, varMap[i].wideType)
              END;
            END;
            RETURN loaded;
          END;
        END;
      END;
    END;
    FOR i := 0 TO globalMapN - 1 DO
      IF globalMap[i].key = v THEN
        IF globalMap[i].importBind # NIL THEN
          (* Import chain: load the import ptr, GEP to the var, load the value. *)
          VAR addr := ImportChainAddr(globalMap[i].importBind, globalMap[i].varByteOff);
          VAR mt   := globalMap[i].varMSIRType;
          BEGIN
            IF addr = NIL THEN RETURN NIL END;
            IF globalMap[i].needsLoad THEN
              (* Large indirect imported global: the slot holds a ptr to the
                 storage; load the ptr, then load the value through it. *)
              VAR dataPtr := MSIR.BuildLoad(curBlock, "", MSIR.TPtr(MSIR.TVoid()), addr);
              BEGIN
                IF globalMap[i].dataType # NIL THEN
                  VAR typedPtr := MSIR.RetypeValue(dataPtr,
                                    MSIR.TPtr(globalMap[i].dataType));
                  BEGIN
                    RETURN MSIR.BuildLoad(curBlock, "", globalMap[i].dataType, typedPtr);
                  END;
                END;
                RETURN dataPtr;
              END;
            END;
            IF mt = NIL THEN RETURN NIL END;
            (* Traced ref imported global: read barrier, like CG's NamedExpr
               'global' case (the import-chain slot lives in another module's
               info struct — plain memory, but the VALUE needs the gray
               check). *)
            IF MSIR.Kind(mt) = MSIR.TypeKind.GcRef THEN
              RETURN MSIR.BuildGcLoad(curBlock, "",
                       MSIR.RetypeValue(addr, MSIR.TGcSlot(MSIR.EltType(mt))));
            END;
            RETURN MSIR.BuildLoad(curBlock, "", mt, addr);
          END;
        END;
        gv := MSIR.GlobalValue(globalMap[i].val);
        IF globalMap[i].needsLoad THEN
          (* Large indirect global: struct field holds ptr-to-data.
             Load the ptr, then load the value through it. *)
          VAR dataPtr := MSIR.BuildLoad(curBlock, "", MSIR.TPtr(MSIR.TVoid()), gv);
          BEGIN
            IF globalMap[i].dataType # NIL THEN
              VAR typedPtr := MSIR.RetypeValue(dataPtr,
                                MSIR.TPtr(globalMap[i].dataType));
              BEGIN
                RETURN MSIR.BuildLoad(curBlock, "", globalMap[i].dataType, typedPtr);
              END;
            END;
            gt := MSIR.GlobalType(globalMap[i].val);
            RETURN MSIR.BuildLoad(curBlock, "", gt, dataPtr);
          END;
        ELSIF MSIR.Kind(MSIR.ValueType(gv)) = MSIR.TypeKind.GcSlot THEN
          RETURN MSIR.BuildGcLoad(curBlock, "", gv);
        ELSE
          gt := MSIR.GlobalType(globalMap[i].val);
          (* Traced ref in the module's OWN globals: read barrier, like CG's
             NamedExpr 'global' case (rb-audit: Stdio.stdout et al. were
             plain loads — only the import-chain branch had the barrier). *)
          IF MSIR.Kind(gt) = MSIR.TypeKind.GcRef THEN
            RETURN MSIR.BuildGcLoad(curBlock, "",
                     MSIR.RetypeValue(gv, MSIR.TGcSlot(MSIR.EltType(gt))));
          END;
          RETURN MSIR.BuildLoad(curBlock, "", gt, gv);
        END;
      END;
    END;
    RETURN NIL;
  END LookupVarRaw;

PROCEDURE LookupVarAddr(v: Variable.T): MSIR.Value =
  BEGIN
    FOR i := 0 TO varMapN - 1 DO
      IF varMap[i].key = v THEN
        IF varMap[i].isBitField THEN
          (* Bit-field WITH alias has no plain lvalue; writes go through
             BitLocOfVar (Expr.BitLocMSIR) and InsertBitLoc. *)
          RETURN NIL;
        END;
        IF varMap[i].storageType = NIL THEN
          Abandon("cannot store to by-value formal in MSIR v0");
          RETURN NIL;
        END;
        RETURN varMap[i].val;   (* alloca ptr (typed as ptr(storageType)) *)
      END;
    END;
    FOR i := 0 TO globalMapN - 1 DO
      IF globalMap[i].key = v THEN
        IF globalMap[i].importBind # NIL THEN
          (* Import chain: load the import ptr, GEP to the var's address.
             Retype to Ptr(varMSIRType) so subscript/field accesses get a
             typed pointer instead of ptr void. *)
          VAR addr := ImportChainAddr(globalMap[i].importBind, globalMap[i].varByteOff);
              mt   := globalMap[i].varMSIRType;
          BEGIN
            IF addr = NIL THEN RETURN NIL END;
            IF globalMap[i].needsLoad THEN
              (* Large indirect imported global: the import-chain slot holds a
                 POINTER to the separately-allocated storage; load through it. *)
              VAR dataPtr := MSIR.BuildLoad(curBlock, "", MSIR.TPtr(MSIR.TVoid()), addr);
              BEGIN
                IF globalMap[i].dataType # NIL THEN
                  RETURN MSIR.RetypeValue(dataPtr, MSIR.TPtr(globalMap[i].dataType));
                END;
                RETURN dataPtr;
              END;
            END;
            IF mt # NIL THEN RETURN MSIR.RetypeValue(addr, MSIR.TPtr(mt)) END;
            RETURN addr;
          END;
        END;
        VAR ref := MSIR.GlobalValue(globalMap[i].val);
        BEGIN
          IF globalMap[i].needsLoad THEN
            (* Large indirect global: the struct field holds a ptr-to-data.
               Load it to get the address of the actual variable storage.
               Retype to a typed pointer when we know the actual element type. *)
            VAR dataPtr := MSIR.BuildLoad(curBlock, "", MSIR.TPtr(MSIR.TVoid()), ref);
            BEGIN
              IF globalMap[i].dataType # NIL THEN
                RETURN MSIR.RetypeValue(dataPtr, MSIR.TPtr(globalMap[i].dataType));
              END;
              RETURN dataPtr;
            END;
          END;
          RETURN ref;
        END;
      END;
    END;
    RETURN NIL;
  END LookupVarAddr;

PROCEDURE UniqueLocalName(rawName: TEXT): TEXT =
  (* Return rawName if no existing varMap entry resolves to the same LLVM SSA
     name in the current function.  If there is a clash, append ".<N>" for the
     smallest N >= 1 that is free.  Both alloca values (name = "%foo") and
     param values (name = "foo") are checked by stripping a leading '%'. *)
  VAR suffix := 0;
      name   := rawName;
      clash  : BOOLEAN;
      vn     : TEXT;
  BEGIN
    LOOP
      clash := FALSE;
      FOR i := 0 TO varMapN - 1 DO
        vn := MSIR.ValueName(varMap[i].val);
        IF Text.Length(vn) > 0 AND Text.GetChar(vn, 0) = '%' THEN
          vn := Text.Sub(vn, 1);
        END;
        IF Text.Equal(vn, name) THEN clash := TRUE;  EXIT END;
      END;
      IF NOT clash THEN RETURN name END;
      INC(suffix);
      name := rawName & "." & Fmt.Int(suffix);
    END;
  END UniqueLocalName;

PROCEDURE AddLocal(v: Variable.T): BOOLEAN =
  (* Delegate to Variable.AddLocalMSIR — the single canonical registration
     path for all scalar locals. *)
  BEGIN
    RETURN Variable.AddLocalMSIR(v, curBlock);
  END AddLocal;

PROCEDURE RegisterLoopVar(v: Variable.T;  paramVal: MSIR.Value) =
  (* Register a FOR loop variable as an SSA block-parameter value.
     No alloca needed: the loop variable is read-only (M3 spec forbids
     assignments to it in the body) and not addressable.
     VarMapAdd with storageType = NIL causes LookupVar to return paramVal
     directly, as for by-value formal parameters. *)
  BEGIN
    IF VarMapContains(v) THEN RETURN END;
    IF varMapN >= MaxVarMap THEN
      Abandon("RegisterLoopVar: overflow — increase MaxVarMap");
      RETURN;
    END;
    varMap[varMapN].key         := v;
    varMap[varMapN].isBitField  := FALSE;  (* slot reused: clear stale flag *)
    varMap[varMapN].val         := paramVal;
    varMap[varMapN].storageType := NIL;   (* return paramVal directly *)
    varMap[varMapN].wideType    := NIL;
    varMap[varMapN].signedWiden := FALSE;
    varMap[varMapN].container   := NIL;
    INC(varMapN);
  END RegisterLoopVar;

PROCEDURE BindVarAddr(v: Variable.T; addr: MSIR.Value; elemType: MSIR.T;
                      container: MSIR.Value := NIL) =
  (* WITH designator: addr is already a pointer to the target; no alloca widening. *)
  BEGIN
    IF varMapN >= MaxVarMap THEN
      Abandon("too many variables in proc");
      RETURN;
    END;
    varMap[varMapN].key         := v;
    varMap[varMapN].isBitField  := FALSE;  (* slot reused: clear stale flag *)
    varMap[varMapN].val         := addr;
    varMap[varMapN].storageType := elemType;
    varMap[varMapN].wideType    := elemType;  (* WITH aliases keep natural type *)
    varMap[varMapN].signedWiden := FALSE;
    varMap[varMapN].container   := container;
    INC(varMapN);
  END BindVarAddr;

PROCEDURE BindVarBitField(v: Variable.T;  READONLY loc: MSIRBitLoc.T) =
  BEGIN
    IF varMapN >= MaxVarMap THEN
      Abandon("too many variables in proc");
      RETURN;
    END;
    varMap[varMapN].key         := v;
    varMap[varMapN].val         := loc.base;
    varMap[varMapN].storageType := NIL;
    varMap[varMapN].wideType    := NIL;
    varMap[varMapN].signedWiden := FALSE;
    varMap[varMapN].container   := NIL;
    varMap[varMapN].isBitField  := TRUE;
    varMap[varMapN].bfLoc       := loc;
    INC(varMapN);
  END BindVarBitField;

PROCEDURE IsBitFieldVar(v: Variable.T): BOOLEAN =
  BEGIN
    FOR i := 0 TO varMapN - 1 DO
      IF varMap[i].key = v THEN RETURN varMap[i].isBitField END;
    END;
    RETURN FALSE;
  END IsBitFieldVar;

PROCEDURE BitLocOfVar(v: Variable.T;  VAR loc: MSIRBitLoc.T): BOOLEAN =
  BEGIN
    FOR i := 0 TO varMapN - 1 DO
      IF varMap[i].key = v THEN
        IF NOT varMap[i].isBitField THEN RETURN FALSE END;
        loc := varMap[i].bfLoc;
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END BitLocOfVar;

PROCEDURE EndProc() =
  VAR resultT: MSIR.T;
  BEGIN
    IF curProc = NIL THEN RETURN END;
    IF NOT abandoned
       AND curBlock # NIL
       AND NOT MSIR.BlockIsTerminated(curBlock) THEN
      (* Implicit fall-through at end of body: emit `ret` for void procs;
         emit `unreachable` for value-returning procs (the source omits a
         return path, which is a runtime error in M3 if reached). *)
      resultT := MSIR.ProcResultType(curProc);
      IF resultT # NIL AND MSIR.Kind(resultT) = MSIR.TypeKind.Void THEN
        MSIR.BuildRet(curBlock, NIL);
      ELSE
        MSIR.BuildUnreachable(curBlock);
      END;
    END;
    IF NOT abandoned THEN
      MSIREmit.AddProc(curProc);
    ELSE
      (* Abandoned: emit an unreachable stub so the linker still finds the
         symbol.  This prevents undefined-symbol link errors when the proc is
         called from other procs in the same module that compiled successfully.
         Add `unreachable` to the current (partial) block, then emit. *)
      IF curBlock # NIL AND NOT MSIR.BlockIsTerminated(curBlock) THEN
        MSIR.BuildUnreachable(curBlock);
      END;
      MSIREmit.AddProc(curProc);
      (* Keep in procMap so callers in this module find the stub definition
         rather than creating a fresh external reference. *)
    END;
    (* Pop saved outer proc context if we're returning from a nested proc. *)
    IF procContextDepth > 0 THEN
      DEC(procContextDepth);
      WITH ctx = procContextStack[procContextDepth] DO
        curProc          := ctx.proc;
        curBlock         := ctx.block;
        abandoned        := ctx.abandoned;
        blockSeq         := ctx.blockSeq;
        varMapN          := ctx.varMapN;
        exitDepth        := ctx.exitDepth;
        curScope         := ctx.scope;
        cleanupDepth     := ctx.cleanupDepth;
        FOR i := 0 TO varMapN - 1 DO varMap[i] := ctx.varMap[i] END;
      END;
    ELSE
      curProc          := NIL;
      curBlock         := NIL;
      abandoned        := FALSE;
      varMapN          := 0;
      exitDepth        := 0;
      curScope         := NIL;
      cleanupDepth     := 0;
      MSIR.SetCurrentSrcLine(0);
    END;
  END EndProc;

PROCEDURE Abandon(reason: TEXT) =
  VAR pname: TEXT;
  BEGIN
    IF NOT abandoned THEN
      IF curProc # NIL
        THEN pname := MSIR.ProcName(curProc)
        ELSE pname := "<no-proc>"
      END;
      MSIREmit.NoteSkipped(pname, "msir-abandon: " & reason);
      (* In MSIRObj/MSIRAsm mode the MSIR lowering IS the object code, so an
         abandon mid-procedure leaves truncated/malformed IR (its symptom is the
         verifier's "empty block").  There is no C fallback, so fail the build
         with the root reason rather than silently emitting a broken module.
         In parallel emission (@M3m3front-msir, backend = C) the C output is
         authoritative, so abandons stay informational. *)
      IF Target.BackendMode IN Target.BackendMSIRSet THEN
        Error.Msg ("MSIR cannot compile " & pname & ": " & reason);
      END;
      (* The module now holds an `unreachable` stub for this procedure (see
         EndProc), so it must not be lowered as the unit's code: @M3msircg
         would otherwise report it lowered and run the stub.  Marking the
         module incomplete makes MSIRToM3CG.Lowerable fall back to C. *)
      IF MSIREmit.CurrentModule () # NIL THEN
        MSIR.ModuleSetIncomplete (MSIREmit.CurrentModule (),
                                  "abandoned " & pname & ": " & reason);
      END;
      abandoned := TRUE;
    END;
  END Abandon;

PROCEDURE InProc(): BOOLEAN =
  BEGIN
    RETURN curProc # NIL AND NOT abandoned;
  END InProc;

PROCEDURE IsAbandoned(): BOOLEAN =
  BEGIN RETURN abandoned END IsAbandoned;


PROCEDURE VarContainer(v: Variable.T): MSIR.Value =
  BEGIN
    FOR i := 0 TO varMapN - 1 DO
      IF varMap[i].key = v THEN RETURN varMap[i].container END;
    END;
    RETURN NIL;
  END VarContainer;


PROCEDURE CurrentProc(): MSIR.Proc =
  BEGIN RETURN curProc END CurrentProc;

PROCEDURE CurrentBlock(): MSIR.Block =
  BEGIN
    IF curBlock # NIL AND MSIR.BlockIsTerminated(curBlock) THEN
      INC(blockSeq);
      VAR dead := MSIR.NewBlock("dead." & Fmt.Int(blockSeq),
                                ARRAY OF MSIR.BlockParam{});
      BEGIN
        MSIR.ProcAddBlock(curProc, dead);
        IF curScope # NIL THEN MSIR.BlockSetScope(dead, curScope) END;
        curBlock := dead;
      END;
    END;
    RETURN curBlock;
  END CurrentBlock;

PROCEDURE NewBlock(label: TEXT): MSIR.Block =
  VAR b: MSIR.Block;  uniq: TEXT;
  BEGIN
    INC(blockSeq);
    uniq := label & "." & Fmt.Int(blockSeq);
    b := MSIR.NewBlock(uniq, ARRAY OF MSIR.BlockParam{});
    MSIR.ProcAddBlock(curProc, b);
    IF curScope # NIL THEN MSIR.BlockSetScope(b, curScope) END;
    RETURN b;
  END NewBlock;

PROCEDURE NewBlockWithParam(label: TEXT;  paramName: TEXT;  paramType: MSIR.T): MSIR.Block =
  VAR b: MSIR.Block;  uniq: TEXT;
  BEGIN
    INC(blockSeq);
    uniq := label & "." & Fmt.Int(blockSeq);
    b := MSIR.NewBlock(uniq,
           ARRAY OF MSIR.BlockParam{MSIR.BlockParam{
                                       name := paramName & "." & Fmt.Int(blockSeq),
                                       type := paramType}});
    MSIR.ProcAddBlock(curProc, b);
    IF curScope # NIL THEN MSIR.BlockSetScope(b, curScope) END;
    RETURN b;
  END NewBlockWithParam;

PROCEDURE SetCurrentBlock(b: MSIR.Block) =
  BEGIN
    curBlock := b;
  END SetCurrentBlock;

PROCEDURE BeginHelperProc(p: MSIR.Proc;  entry: MSIR.Block) =
  (* Caller must save/restore the previous context manually using
     CurrentProc(), CurrentBlock(), and IsAbandoned() before calling this,
     or use the SavedContext type.  BeginHelperProc saves nothing itself. *)
  BEGIN
    curProc   := p;
    curBlock  := entry;
    abandoned := FALSE;
  END BeginHelperProc;

PROCEDURE EndHelperProc() =
  BEGIN
    curProc   := NIL;
    curBlock  := NIL;
    abandoned := FALSE;
  END EndHelperProc;

PROCEDURE CurrentBlockTerminated(): BOOLEAN =
  BEGIN
    RETURN MSIR.BlockIsTerminated(curBlock);
  END CurrentBlockTerminated;

PROCEDURE PushExitBlock(b: MSIR.Block) =
  BEGIN
    IF exitDepth < MaxExitStack THEN
      exitStack[exitDepth] := b;
      INC(exitDepth);
    ELSE
      Abandon("exit block stack overflow");
    END;
    (* Also record a Loop frame on the unified cleanup stack so a non-local
       EXIT runs intervening finallys before reaching this loop's exit. *)
    IF cleanupDepth < MaxCleanup THEN
      cleanupStack[cleanupDepth] :=
        CleanupFrame{kind := CleanupKind.Loop, exitBlock := b};
      INC(cleanupDepth);
    ELSE
      Abandon("cleanup stack overflow");
    END;
  END PushExitBlock;

PROCEDURE PopExitBlock() =
  BEGIN
    IF exitDepth > 0 THEN DEC(exitDepth) END;
    (* Pop the matching Loop frame (any finallys inside this loop were already
       popped, so the top of the cleanup stack is this loop's frame). *)
    IF cleanupDepth > 0 AND cleanupStack[cleanupDepth-1].kind = CleanupKind.Loop
      THEN DEC(cleanupDepth)
    END;
  END PopExitBlock;

PROCEDURE CurrentExitBlock(): MSIR.Block =
  BEGIN
    IF exitDepth = 0 THEN RETURN NIL END;
    RETURN exitStack[exitDepth - 1];
  END CurrentExitBlock;

(*----------------------------------------------------------- EH scopes *)

PROCEDURE CurrentScope(): MSIR.Scope =
  BEGIN
    IF curScope # NIL THEN RETURN curScope END;
    RETURN MSIR.ProcScope(curProc);
  END CurrentScope;

PROCEDURE SetScope(s: MSIR.Scope): MSIR.Scope =
  VAR prev := curScope;
  BEGIN
    curScope := s;
    RETURN prev;
  END SetScope;

PROCEDURE NewScope(kind: MSIR.ScopeKind;  parent: MSIR.Scope;
                   owner: MSIR.Scope := NIL): MSIR.Scope =
  BEGIN
    RETURN MSIR.NewScope(curProc, kind, parent, owner);
  END NewScope;

PROCEDURE EmitGoto(target: MSIR.Block;  READONLY args: ARRAY OF MSIR.Value) =
  BEGIN
    IF MSIR.BlockScope(target) = CurrentScope() THEN
      MSIR.BuildBr(CurrentBlock(), target, args);
    ELSE
      MSIR.BuildUnwindTo(CurrentBlock(), target, args);
    END;
  END EmitGoto;

PROCEDURE PushElseFrame(target: MSIR.Block) =
  BEGIN
    IF cleanupDepth < MaxCleanup THEN
      cleanupStack[cleanupDepth] :=
        CleanupFrame{kind := CleanupKind.Else, exitBlock := target};
      INC(cleanupDepth);
    ELSE
      Abandon("cleanup stack overflow");
    END;
  END PushElseFrame;

PROCEDURE PopElseFrame() =
  BEGIN
    IF cleanupDepth > 0 AND cleanupStack[cleanupDepth-1].kind = CleanupKind.Else
      THEN DEC(cleanupDepth)
    END;
  END PopElseFrame;

PROCEDURE EmitExitMSIR() =
  BEGIN
    IF cleanupDepth = 0 THEN
      Abandon("EXIT not inside a loop in MSIR");
      RETURN;
    END;
    EmitGoto(cleanupStack[cleanupDepth-1].exitBlock, ARRAY OF MSIR.Value{});
  END EmitExitMSIR;

PROCEDURE EmitReturnMSIR(v: MSIR.Value) =
  BEGIN
    FOR k := cleanupDepth - 1 TO 0 BY -1 DO
      IF cleanupStack[k].kind = CleanupKind.Else THEN
        (* the ELSE catches the RETURN; its value is dropped *)
        EmitGoto(cleanupStack[k].exitBlock, ARRAY OF MSIR.Value{});
        RETURN;
      END;
    END;
    IF MSIR.ScopeIndex(CurrentScope()) = 0 THEN
      MSIR.BuildRet(CurrentBlock(), v);
    ELSE
      MSIR.BuildRetThroughEnvelope(CurrentBlock(), v);
    END;
  END EmitReturnMSIR;

PROCEDURE EmitNestedCall(name: TEXT;  callee: MSIR.Proc;
                         caps: REF ARRAY OF CaptureAnalysis.Capture;
                         READONLY args: ARRAY OF MSIR.Value): MSIR.Value =
  (* Build capture args from the outer proc's varMap, then call.
     Read-only scalar captures are passed by value; others by pointer. *)
  VAR
    nCaps   : INTEGER;
    allArgs : REF ARRAY OF MSIR.Value;
    v       : Variable.T;
    vt      : Type.T;  vg, vi, vlhs: BOOLEAN;
    mt      : MSIR.T;
  BEGIN
    IF caps = NIL THEN nCaps := 0 ELSE nCaps := NUMBER(caps^) END;
    allArgs := NEW(REF ARRAY OF MSIR.Value, nCaps + NUMBER(args));
    FOR i := 0 TO nCaps - 1 DO
      v := caps[i].var;
      Variable.Split(v, vt, vg, vi, vlhs);
      mt := MSIRType.Translate(vt);
      IF NOT caps[i].written AND mt # NIL AND IsScalarType(mt) THEN
        allArgs[i] := LookupVar(v);   (* pass the current value *)
      ELSE
        allArgs[i] := LookupVarAddr(v);  (* pass the alloca address *)
      END;
      IF allArgs[i] = NIL THEN
        Abandon("capture var not found in outer proc varMap");
        RETURN NIL;
      END;
    END;
    FOR i := 0 TO NUMBER(args) - 1 DO allArgs[nCaps + i] := args[i] END;
    RETURN EmitCall(name, callee, allArgs^);
  END EmitNestedCall;

PROCEDURE GetOrCreateClosureShim(v: Value.T;  nested: MSIR.Proc;
                                   caps: REF ARRAY OF CaptureAnalysis.Capture;
                                   procType: Type.T): MSIR.Proc =
  (* Build a shim function  @F__shim(ptr %__env, explicit_params…) → result
     that unpacks env and calls the lambda-lifted nested proc. *)
  VAR
    ptrT    := MSIR.TPtr(MSIR.TVoid());
    AP      := Target.Address.bytes;
    nCaps   : INTEGER;
    nFormals: INTEGER;
    resultT : MSIR.T;
    params  : REF ARRAY OF MSIR.Param;
    shimProc: MSIR.Proc;
    shimBlk : MSIR.Block;
    shimName: TEXT;
    envP    : MSIR.Value;
    capArgs : REF ARRAY OF MSIR.Value;
    allArgs : REF ARRAY OF MSIR.Value;
    f       : Value.T;
    info    : Formal.Info;
  BEGIN
    FOR i := 0 TO shimMapN - 1 DO
      IF shimMap[i].key = v THEN RETURN shimMap[i].val END;
    END;

    IF caps = NIL THEN nCaps := 0 ELSE nCaps := NUMBER(caps^) END;
    (* Native large-result ABI: the shim returns the lifted proc's result by
       value (see BeginProc); no hidden result pointer. *)
    resultT := MSIRType.TranslateResult(ProcType.Result(procType));
    IF resultT = NIL THEN RETURN NIL END;

    (* Count explicit formals. *)
    f := ProcType.Formals(procType);
    nFormals := 0;
    WHILE f # NIL DO INC(nFormals); f := f.next END;

    (* params: [ptr %__env, explicit_formals…] *)
    params := NEW(REF ARRAY OF MSIR.Param, 1 + nFormals);
    params[0].name := "__env";
    params[0].type := ptrT;
    params[0].mode := MSIR.ParamMode.ByValue;
    f := ProcType.Formals(procType);
    FOR i := 0 TO nFormals - 1 DO
      Formal.Split(f, info);
      VAR pt := MSIRType.Translate(info.type);
      BEGIN
        IF pt = NIL THEN RETURN NIL END;
        params[1 + i].name := "a." & M3ID.ToText(info.name);
        CASE info.mode OF
        | Formal.Mode.mVALUE =>
            params[1 + i].mode := MSIR.ParamMode.ByValue;
            IF MSIR.Kind(pt) = MSIR.TypeKind.OpenArray THEN
              params[1 + i].type := MSIR.TPtr(pt);
            ELSE
              params[1 + i].type := pt;
            END;
        | Formal.Mode.mVAR =>
            params[1 + i].mode := MSIR.ParamMode.Var;
            params[1 + i].type := MSIR.TPtr(pt);
        | Formal.Mode.mREADONLY =>
            (* Every READONLY formal is a pointer, as in BeginProc and at
               the call (Formal.EmitArgMSIR); a scalar by value here made an
               imported procedure's signature disagree with its definition. *)
            params[1 + i].mode := MSIR.ParamMode.Readonly;
            params[1 + i].type := MSIR.TPtr(pt);
        END;
        f := f.next;
      END;
    END;

    shimName := MSIR.ProcName(nested) & "__shim";

    (* Save outer proc context. *)
    IF curProc # NIL THEN
      IF procContextDepth >= MaxNestDepth THEN
        Abandon("shim: nesting too deep");
        RETURN NIL;
      END;
      WITH ctx = procContextStack[procContextDepth] DO
        ctx.proc       := curProc;
        ctx.block      := curBlock;
        ctx.abandoned  := abandoned;
        ctx.blockSeq   := blockSeq;
        ctx.varMapN    := varMapN;
        ctx.exitDepth  := exitDepth;
        ctx.scope      := curScope;
        ctx.cleanupDepth := cleanupDepth;
        FOR i := 0 TO varMapN - 1 DO ctx.varMap[i] := varMap[i] END;
      END;
      INC(procContextDepth);
    END;

    abandoned      := FALSE;
    varMapN        := 0;
    exitDepth      := 0;
    curScope       := NIL;
    cleanupDepth   := 0;
    blockSeq       := 0;

    shimProc := MSIR.NewProc(shimName, params^, resultT);
    shimBlk  := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
    MSIR.ProcAddBlock(shimProc, shimBlk);
    MSIR.ProcSetLinkage(shimProc, MSIR.Linkage.Internal);
    curProc  := shimProc;
    curBlock := shimBlk;

    (* Unpack capture env: env is param[0]. *)
    envP := MSIR.ProcParam(shimProc, 0);
    capArgs := NEW(REF ARRAY OF MSIR.Value, nCaps);
    FOR k := 0 TO nCaps - 1 DO
      VAR v_k  := caps[k].var;
          vt   : Type.T;  vg, vi, vlhs: BOOLEAN;
          mt   : MSIR.T;
          slot : MSIR.Value;
          ptr  : MSIR.Value;
      BEGIN
        Variable.Split(v_k, vt, vg, vi, vlhs);
        mt   := MSIRType.Translate(vt);
        slot := MSIR.BuildPtrAdd(curBlock, "", envP, k * AP);
        ptr  := MSIR.BuildLoad(curBlock, "", ptrT, slot);
        IF NOT caps[k].written AND mt # NIL AND IsScalarType(mt) THEN
          (* Reload the by-value scalar capture at ZType to match the env slot
             and the nested proc's (now ZType) capture param (p048). *)
          capArgs[k] := MSIR.BuildLoad(curBlock, "", MSIRType.ComputeType(vt), ptr);
        ELSE
          capArgs[k] := ptr;
        END;
      END;
    END;

    (* Build allArgs: [caps…, explicit_formals…]. *)
    allArgs := NEW(REF ARRAY OF MSIR.Value, nCaps + nFormals);
    FOR k := 0 TO nCaps - 1 DO allArgs[k] := capArgs[k] END;
    FOR i := 0 TO nFormals - 1 DO
      allArgs[nCaps + i] := MSIR.ProcParam(shimProc, 1 + i);
    END;

    (* The lifted proc returns its result (large or small) by value. *)
    MSIR.BuildRet(curBlock, MSIR.BuildCall(curBlock, "", nested, allArgs^));

    (* Register shim with module. *)
    VAR m := MSIREmit.CurrentModule();
    BEGIN
      IF m # NIL THEN MSIR.ModuleAddProc(m, shimProc) END;
    END;

    (* Restore outer proc context. *)
    IF procContextDepth > 0 THEN
      DEC(procContextDepth);
      WITH ctx = procContextStack[procContextDepth] DO
        curProc          := ctx.proc;
        curBlock         := ctx.block;
        abandoned        := ctx.abandoned;
        blockSeq         := ctx.blockSeq;
        varMapN          := ctx.varMapN;
        exitDepth        := ctx.exitDepth;
        curScope         := ctx.scope;
        cleanupDepth     := ctx.cleanupDepth;
        FOR i := 0 TO varMapN - 1 DO varMap[i] := ctx.varMap[i] END;
      END;
    ELSE
      curProc  := NIL;
      curBlock := NIL;
    END;

    IF shimMapN >= NUMBER (shimMap^) THEN
      VAR n := NEW (REF ARRAY OF ShimEntry, 2 * shimMapN);
      BEGIN SUBARRAY (n^, 0, shimMapN) := shimMap^;  shimMap := n END;
    END;
    shimMap[shimMapN].key := v;
    shimMap[shimMapN].val := shimProc;
    INC(shimMapN);
    RETURN shimProc;
  END GetOrCreateClosureShim;

PROCEDURE BuildClosureValue(v: Value.T; procType: Type.T;
                            caps: REF ARRAY OF CaptureAnalysis.Capture): MSIR.Value =
  VAR
    ptrT     := MSIR.TPtr(MSIR.TVoid());
    intT     := MSIR.TI(Target.Integer.size);
    IP       := Target.Integer.bytes;
    AP       := Target.Address.bytes;
    clSize   := IP + AP + AP;
    b        : MSIR.Block;
    msirProc : MSIR.Proc;
    nCaps    : INTEGER;
    shim     : MSIR.Proc;
    envAlloca: MSIR.Value;
    clAlloca : MSIR.Value;
    markerV  : MSIR.Value;
  BEGIN
    IF NOT InProc() THEN RETURN NIL END;
    msirProc := LookupOrCreateProc(v, procType);
    IF msirProc = NIL THEN RETURN NIL END;
    IF caps = NIL THEN nCaps := 0 ELSE nCaps := NUMBER(caps^) END;

    shim := GetOrCreateClosureShim(v, msirProc, caps, procType);
    IF shim = NIL THEN
      Abandon("BuildClosureValue: shim creation failed");
      RETURN NIL;
    END;
    IF abandoned THEN RETURN NIL END;

    b := CurrentBlock();

    (* Allocate env: nCaps consecutive ptr slots (at least 1 byte). *)
    IF nCaps > 0 THEN
      envAlloca := MSIR.BuildAlloca(b, "", TFixedArrayI(nCaps, ptrT));
      FOR k := 0 TO nCaps - 1 DO
        VAR capVar  := caps[k].var;
            vt      : Type.T;  vg, vi, vlhs: BOOLEAN;
            mt      : MSIR.T;
            capAddr : MSIR.Value;
        BEGIN
          Variable.Split(capVar, vt, vg, vi, vlhs);
          mt := MSIRType.Translate(vt);
          IF NOT caps[k].written AND mt # NIL AND IsScalarType(mt) THEN
            (* Scalar read-only capture: no alloca exists for it (it was passed
               by value).  Spill the current value to a fresh stack slot so the
               closure env can hold a stable pointer.  The shim will reload the
               value through this pointer when invoking the nested proc. *)
            VAR tmp := MSIR.BuildAlloca(b, "", MSIRType.ComputeType(vt));
                val := LookupVar(capVar);   (* ZType; slot must match (p048) *)
            BEGIN
              IF val = NIL THEN
                Abandon("BuildClosureValue: scalar cap var not found");
                RETURN NIL;
              END;
              MSIR.BuildStore(b, val, tmp);
              capAddr := tmp;
            END;
          ELSE
            capAddr := LookupVarAddr(capVar);
            IF capAddr = NIL THEN
              Abandon("BuildClosureValue: cap var not in outer proc varMap");
              RETURN NIL;
            END;
          END;
          MSIR.BuildStore(b, capAddr, MSIR.BuildPtrAdd(b, "", envAlloca, k * AP));
        END;
      END;
    ELSE
      (* No captures (or captures not accessible in current scope): build a
         closure struct with a null (NIL ptr) env.  This ensures:
           - EmitClosureCall reads CL_marker=-1 → takes the closure path →
             calls shim(null_env, formals); the shim for non-capturing procs
             ignores env and calls lambda(formals). ✓
           - EqualExpr normalises both sides via the CL_marker check, extracting
             the shim pointer → icmp eq @shim, @shim → TRUE. ✓
         Using a plain alloca for the dummy env (as before) would cause each
         reference to create a new struct at a different address so icmp eq
         on the addresses would always be FALSE — hence we must normalise. *)
      envAlloca := MSIR.ConstNil(ptrT);
    END;

    (* Allocate closure struct as [clSize x i8]. *)
    clAlloca := MSIR.BuildAlloca(b, "", TFixedArrayI(clSize, MSIR.TI(8)));

    (* Store CL_marker = -1 at byte offset 0. Use BuildPtrAdd(…,0) to get a
       TPtr(TVoid()) destination so the verifier's type-match check is skipped. *)
    markerV := MSIR.ConstInt(intT, M3RT.CL_marker_value);
    MSIR.BuildStore(b, markerV, MSIR.BuildPtrAdd(b, "", clAlloca, 0));

    (* Store CL_proc = shim at byte offset IP. *)
    MSIR.BuildStore(b, MSIR.ConstProcRef(shim),
                    MSIR.BuildPtrAdd(b, "", clAlloca, IP));

    (* Store CL_frame = envAlloca at byte offset IP+AP. *)
    MSIR.BuildStore(b, envAlloca,
                    MSIR.BuildPtrAdd(b, "", clAlloca, IP + AP));

    RETURN clAlloca;
  END BuildClosureValue;

PROCEDURE EmitClosureCall(name: TEXT;  fn: MSIR.Value;  rtype: MSIR.T;
                           READONLY args: ARRAY OF MSIR.Value): MSIR.Value =
  (* Runtime CL_marker check: if *fn == -1 it's a closure, else a direct ptr.
     Uses an alloca slot to merge the result across the two paths, since
     MSIRToLLVM does not yet lower block-param values to phi nodes.  The result
     (large or small) comes back by value — the closure shim and the direct
     proc both return it. *)
  VAR
    ptrT       := MSIR.TPtr(MSIR.TVoid());
    intT       := MSIR.TI(Target.Integer.size);
    IP         := Target.Integer.bytes;
    AP         := Target.Address.bytes;
    b          := CurrentBlock();
    markerVal  : MSIR.Value;
    isClosure  : MSIR.Value;
    closureBlk : MSIR.Block;
    directBlk  : MSIR.Block;
    mergeBlk   : MSIR.Block;
    shimPtr    : MSIR.Value;
    envPtr     : MSIR.Value;
    shimArgs   : REF ARRAY OF MSIR.Value;
    nArgs      := NUMBER(args);
    isVoid     : BOOLEAN;
    resultSlot : MSIR.Value;
    closureRes : MSIR.Value;
    directRes  : MSIR.Value;
  BEGIN
    (* A NIL procedure is no closure; its call faults (CG.If_closure, then
       CG.Gen_Call_indirect's check).  Check before reading the marker. *)
    CheckCallee(fn);
    b := CurrentBlock();
    isVoid := (rtype = NIL) OR (MSIR.Kind(rtype) = MSIR.TypeKind.Void);
    IF NOT isVoid THEN
      resultSlot := MSIR.BuildAlloca(b, "", rtype);
    END;

    markerVal := MSIR.BuildLoad(b, "", intT, fn);
    isClosure := MSIR.BuildICmp(b, "", MSIR.CmpPred.Eq, markerVal,
                                MSIR.ConstInt(intT, M3RT.CL_marker_value));

    closureBlk := NewBlock("cl.closure");
    directBlk  := NewBlock("cl.direct");
    mergeBlk   := NewBlock("cl.merge");

    MSIR.BuildCondBr(b, isClosure,
      closureBlk, ARRAY OF MSIR.Value{},
      directBlk,  ARRAY OF MSIR.Value{});

    (* Closure path: unpack CL_proc (shim) and CL_frame (env), call shim. *)
    SetCurrentBlock(closureBlk);
    shimPtr := MSIR.BuildLoad(curBlock, "", ptrT,
                 MSIR.BuildPtrAdd(curBlock, "", fn, IP));
    envPtr  := MSIR.BuildLoad(curBlock, "", ptrT,
                 MSIR.BuildPtrAdd(curBlock, "", fn, IP + AP));
    (* shim(env, formals…) *)
    shimArgs := NEW(REF ARRAY OF MSIR.Value, 1 + nArgs);
    shimArgs[0] := envPtr;
    FOR i := 0 TO nArgs - 1 DO shimArgs[1 + i] := args[i] END;
    closureRes := EmitCallIndirect(name, shimPtr, rtype, shimArgs^);
    IF NOT isVoid THEN
      MSIR.BuildStore(curBlock, closureRes, resultSlot);
    END;
    MSIR.BuildBr(curBlock, mergeBlk, ARRAY OF MSIR.Value{});

    (* Direct path: call fn directly. *)
    SetCurrentBlock(directBlk);
    directRes := EmitCallIndirect(name, fn, rtype, args);
    IF NOT isVoid THEN
      MSIR.BuildStore(curBlock, directRes, resultSlot);
    END;
    MSIR.BuildBr(curBlock, mergeBlk, ARRAY OF MSIR.Value{});

    SetCurrentBlock(mergeBlk);
    IF isVoid THEN
      RETURN NIL;
    ELSE
      RETURN MSIR.BuildLoad(curBlock, name, rtype, resultSlot);
    END;
  END EmitClosureCall;

PROCEDURE EmitCall(name: TEXT;  callee: MSIR.Proc;
                   READONLY args: ARRAY OF MSIR.Value): MSIR.Value =
  VAR
    b:       MSIR.Block;
    unwind:  MSIR.Block;
    normalB: MSIR.Block;
    result:  MSIR.Value;
  BEGIN
    b      := CurrentBlock();   (* advance past any dead-terminator block *)
    result := MSIR.BuildCall(b, name, callee, args);
    RETURN result;
  END EmitCall;

PROCEDURE EmitCallIndirect(name: TEXT;  fn: MSIR.Value;  rtype: MSIR.T;
                            READONLY args: ARRAY OF MSIR.Value): MSIR.Value =
  VAR
    b:       MSIR.Block;
    unwind:  MSIR.Block;
    normalB: MSIR.Block;
    result:  MSIR.Value;
  BEGIN
    CheckCallee(fn);
    b      := CurrentBlock();
    result := MSIR.BuildCallIndirect(b, name, fn, rtype, args);
    RETURN result;
  END EmitCallIndirect;

PROCEDURE CheckCallee(fn: MSIR.Value) =
  (* As CG.Gen_Call_indirect: with Host.doProcChk, calling a NIL procedure
     faults with BadMemoryReference before the call. *)
  VAR k := MSIR.Kind(MSIR.ValueType(fn));
  BEGIN
    IF NOT Host.doProcChk THEN RETURN END;
    IF k # MSIR.TypeKind.Ptr AND k # MSIR.TypeKind.GcRef THEN RETURN END;
    EmitReportFault(
      MSIR.BuildICmp(CurrentBlock(), "", MSIR.CmpPred.Eq,
                     fn, MSIR.ConstNil(MSIR.ValueType(fn))),
      ORD(M3CG.RuntimeError.BadMemoryReference));
  END CheckCallee;

PROCEDURE CurrentModuleInfoRef(): MSIR.Value =
  VAR m := MSIREmit.CurrentModule();
  BEGIN
    IF m = NIL THEN RETURN NIL END;
    RETURN MSIR.StructFieldRef(MSIR.ModuleInfoName(m), 0,
                               MSIR.TPtr(MSIR.TVoid()));
  END CurrentModuleInfoRef;

PROCEDURE EmitReportFault(faultCond: MSIR.Value;  errCode: INTEGER) =
  VAR
    reportProc := HookProc(RunTyme.Hook.Abort);  (* RTHooks__ReportFault *)
    faultBlk, contBlk: MSIR.Block;
    f: TEXT;  line: INTEGER;
    ptrT  := MSIR.TPtr(MSIR.TVoid());
    args  : ARRAY [0..1] OF MSIR.Value;
  BEGIN
    IF NOT InProc() OR faultCond = NIL THEN RETURN END;
    IF reportProc = NIL THEN
      Abandon("check: RTHooks__ReportFault hook unavailable");  RETURN;
    END;
    Scanner.Here(f, line);
    faultBlk := NewBlock("check.fault");
    contBlk  := NewBlock("check.cont");
    MSIR.BuildCondBr(CurrentBlock(), faultCond,
                     faultBlk, ARRAY OF MSIR.Value{},
                     contBlk,  ARRAY OF MSIR.Value{});
    curBlock := faultBlk;
    (* info = line*32 + ORD(code); module = &@<curMod>_{M3,I3}_info (offset 0). *)
    args[0] := MSIR.StructFieldRef(
                 MSIR.ModuleInfoName(MSIREmit.CurrentModule()), 0, ptrT);
    args[1] := MSIR.ConstInt(MSIR.TI(Target.Integer.size), line * 32 + errCode);
    EVAL EmitCall("", reportProc, args);
    IF NOT CurrentBlockTerminated() THEN
      MSIR.BuildUnreachable(CurrentBlock());
    END;
    curBlock := contBlk;
  END EmitReportFault;

PROCEDURE WidenCallResult(v: MSIR.Value;  resultType: Type.T): MSIR.Value =
  BEGIN
    IF v = NIL OR resultType = NIL THEN RETURN v END;
    RETURN CoerceToMSIR(curBlock, v, MSIRType.ComputeType(resultType));
  END WidenCallResult;

PROCEDURE PackConstBits(byteVals: REF ARRAY OF INTEGER;
                        bitOff, bitWidth, value: INTEGER) =
  VAR twoW := 1;  v := value;  p2 := 1;
  BEGIN
    FOR k := 1 TO bitWidth DO twoW := twoW * 2 END;    (* 2^bitWidth *)
    v := v MOD twoW;
    IF v < 0 THEN v := v + twoW END;
    FOR bit := 0 TO bitWidth - 1 DO
      IF (v DIV p2) MOD 2 = 1 THEN
        VAR pos := bitOff + bit;  bitp2 := 1;
        BEGIN
          FOR z := 1 TO pos MOD Target.Byte DO bitp2 := bitp2 * 2 END;  (* 2^(pos MOD 8) *)
          byteVals[pos DIV Target.Byte] := byteVals[pos DIV Target.Byte] + bitp2;
        END;
      END;
      p2 := p2 * 2;
    END;
  END PackConstBits;

PROCEDURE EmitAssertFail(faultCond: MSIR.Value;  msgVal: MSIR.Value) =
  VAR
    assertProc := HookProc(RunTyme.Hook.AssertFailed);  (* RTHooks__AssertFailed *)
    faultBlk, contBlk: MSIR.Block;
    f: TEXT;  line: INTEGER;
    ptrT  := MSIR.TPtr(MSIR.TVoid());
    modRef: MSIR.Value;
    args  : ARRAY [0..2] OF MSIR.Value;
  BEGIN
    IF NOT InProc() OR faultCond = NIL OR msgVal = NIL THEN RETURN END;
    IF assertProc = NIL THEN
      Abandon("assert: RTHooks__AssertFailed hook unavailable");  RETURN;
    END;
    Scanner.Here(f, line);
    faultBlk := NewBlock("assert.fault");
    contBlk  := NewBlock("assert.cont");
    MSIR.BuildCondBr(CurrentBlock(), faultCond,
                     faultBlk, ARRAY OF MSIR.Value{},
                     contBlk,  ARRAY OF MSIR.Value{});
    curBlock := faultBlk;
    modRef := CurrentModuleInfoRef();
    IF modRef = NIL THEN modRef := MSIR.ConstNil(ptrT) END;
    args[0] := modRef;                                              (* module *)
    args[1] := MSIR.ConstInt(MSIR.TI(Target.Integer.size), line);  (* line *)
    args[2] := msgVal;                                             (* msg: TEXT *)
    EVAL EmitCall("", assertProc, args);
    IF NOT CurrentBlockTerminated() THEN
      MSIR.BuildUnreachable(CurrentBlock());
    END;
    curBlock := contBlk;
  END EmitAssertFail;

PROCEDURE EmitMethodCall(name: TEXT;  obj: MSIR.Value;  midx: INTEGER;
                          rtype: MSIR.T;
                          READONLY args: ARRAY OF MSIR.Value): MSIR.Value =
  VAR
    ptrT    := MSIR.TPtr(MSIR.TVoid());
    b       : MSIR.Block;
    suite   : MSIR.Value;
    slotPtr : MSIR.Value;
    fn      : MSIR.Value;
    allArgs : REF ARRAY OF MSIR.Value;
    nArgs   := NUMBER(args);
    unwind  : MSIR.Block;
    normalB : MSIR.Block;
    result  : MSIR.Value;
  BEGIN
    b := CurrentBlock();   (* advance past any dead-terminator block *)

    (* 1. Load vtable pointer (first word of object).
       Cast obj to ptr(ptr void) so the load element type matches ptrT. *)
    VAR objAsPtr := MSIR.BuildConvert(b, "", obj, MSIR.TPtr(ptrT));
    BEGIN
      suite := MSIR.BuildLoad(b, "", ptrT, objAsPtr);
    END;

    (* 2. Advance to the method slot (idx * sizeof(ptr) bytes). *)
    IF midx = 0 THEN
      slotPtr := suite;
    ELSE
      slotPtr := MSIR.BuildPtrAdd(b, "", suite,
                                  midx * Target.Address.bytes);
    END;

    (* 3. Load function pointer from the slot. *)
    fn := MSIR.BuildLoad(b, "", ptrT, slotPtr);

    (* 4. Build argument list: obj (self) first, then explicit args.  The result
       — large or small — is returned by value. *)
    allArgs := NEW(REF ARRAY OF MSIR.Value, 1 + nArgs);
    allArgs[0] := obj;
    FOR k := 0 TO nArgs - 1 DO allArgs[1 + k] := args[k] END;

    (* 5. Indirect call, after the check Gen_Call_indirect makes. *)
    CheckCallee(fn);
    result := MSIR.BuildCallIndirect(CurrentBlock(), name, fn, rtype, allArgs^);
    RETURN result;
  END EmitMethodCall;

PROCEDURE ExcDescValue (v: Value.T): MSIR.Value =
  VAR d := ExcDescFor(v);
  BEGIN
    IF d = NIL THEN RETURN NIL END;
    RETURN MSIR.ExcDescValue(d);
  END ExcDescValue;

PROCEDURE ExcDescFor (v: Value.T): MSIR.ExcDesc =
  VAR
    m    := MSIREmit.CurrentModule();
    name := Value.GlobalName(v, dots := FALSE, with_module := TRUE) & "_excptr";
    uid  := M3FP.ToInt(M3FP.FromText(Value.GlobalName(v)));
    desc : MSIR.ExcDesc;
    argT := Exceptionz.ArgType(v);
    mt   : MSIR.T := NIL;
    byRef := FALSE;
  BEGIN
    IF m = NIL THEN RETURN NIL END;
    IF argT # NIL THEN
      mt := MSIRType.Translate(argT);
      byRef := Exceptionz.ArgByReference(argT);
    END;
    (* Check if already registered for this module. *)
    FOR i := 0 TO MSIR.ModuleExcDescCount(m) - 1 DO
      desc := MSIR.ModuleExcDesc(m, i);
      IF Text.Equal(MSIR.ExcDescName(desc), name) THEN
        IF MSIR.ExcDescArgType(desc) = NIL AND mt # NIL THEN
          MSIR.SetExcDescArg(desc, mt, byRef);
        END;
        RETURN desc;
      END;
    END;
    (* Not found — create and register.  Carry the qualified display name
       ("Module.Exc") so the descriptor's RT0.ExceptionDesc.name field can point
       at it — RTException.Crash reads it via M3toC.StoT to print "Unhandled
       exception: Module.Exc"; a null name there prints blank and then SEGVs. *)
    desc := MSIR.NewExcDesc(name, uid,
              Value.GlobalName(v, dots := TRUE, with_module := TRUE),
              argType := mt, argByRef := byRef);
    MSIR.ModuleAddExcDesc(m, desc);
    RETURN desc;
  END ExcDescFor;

PROCEDURE SetProcRaises (READONLY excs: ARRAY OF Value.T) =
  VAR ds := NEW (REF ARRAY OF MSIR.ExcDesc, NUMBER (excs));
  BEGIN
    IF curProc = NIL THEN RETURN END;
    FOR i := 0 TO LAST (excs) DO
      ds[i] := ExcDescFor (excs[i]);
      IF ds[i] = NIL THEN RETURN END;
    END;
    MSIR.ProcSetRaises (curProc, ds^);
  END SetProcRaises;

PROCEDURE TypeLinkValueForRef(t: Type.T): MSIR.Value =
  VAR m    := MSIREmit.CurrentModule();
      (* Resolve opaque types to their revealed REF type so the TypeLink UID
         matches the TypeCell UID registered by RefType.InitTypecellMSIR. *)
      tRef := RefType.ReduceToRef(t);
      uid  : INTEGER;
      nm   : TEXT;
      tl   : MSIR.TypeLink;
      addr : MSIR.Value;
  BEGIN
    IF m = NIL THEN RETURN NIL END;
    IF tRef # NIL THEN t := tRef END;
    uid := Type.GlobalUID(t);
    nm  := "tl_ref_" & Fmt.Int(uid);
    FOR i := 0 TO MSIR.ModuleTypeLinkCount(m) - 1 DO
      tl := MSIR.ModuleTypeLink(m, i);
      IF Text.Equal(MSIR.TypeLinkName(tl), nm) THEN
        addr := MSIR.TypeCellRef(nm);
        RETURN MSIR.BuildLoad(CurrentBlock(), "", MSIR.TPtr(MSIR.TVoid()), addr);
      END;
    END;
    tl := MSIR.NewTypeLink(nm, uid);
    MSIR.ModuleAddTypeLink(m, tl);
    addr := MSIR.TypeCellRef(nm);
    RETURN MSIR.BuildLoad(CurrentBlock(), "", MSIR.TPtr(MSIR.TVoid()), addr);
  END TypeLinkValueForRef;

PROCEDURE TypeLinkValueForRefArray(t: Type.T): MSIR.Value =
  VAR m    := MSIREmit.CurrentModule();
      uid  := Type.GlobalUID(t);
      nm   := "tl_arr_" & Fmt.Int(uid);
      tl   : MSIR.TypeLink;
      addr : MSIR.Value;
  BEGIN
    IF m = NIL THEN RETURN NIL END;
    FOR i := 0 TO MSIR.ModuleTypeLinkCount(m) - 1 DO
      tl := MSIR.ModuleTypeLink(m, i);
      IF Text.Equal(MSIR.TypeLinkName(tl), nm) THEN
        addr := MSIR.TypeCellRef(nm);
        RETURN MSIR.BuildLoad(CurrentBlock(), "", MSIR.TPtr(MSIR.TVoid()), addr);
      END;
    END;
    tl := MSIR.NewTypeLink(nm, uid);
    MSIR.ModuleAddTypeLink(m, tl);
    addr := MSIR.TypeCellRef(nm);
    RETURN MSIR.BuildLoad(CurrentBlock(), "", MSIR.TPtr(MSIR.TVoid()), addr);
  END TypeLinkValueForRefArray;

PROCEDURE TypeLinkValueForObject(t: Type.T): MSIR.Value =
  VAR m    := MSIREmit.CurrentModule();
      uid  := Type.GlobalUID(t);
      nm   := "tl_obj_" & Fmt.Int(uid);
      tl   : MSIR.TypeLink;
      addr : MSIR.Value;
  BEGIN
    IF m = NIL THEN RETURN NIL END;
    FOR i := 0 TO MSIR.ModuleTypeLinkCount(m) - 1 DO
      tl := MSIR.ModuleTypeLink(m, i);
      IF Text.Equal(MSIR.TypeLinkName(tl), nm) THEN
        addr := MSIR.TypeCellRef(nm);
        RETURN MSIR.BuildLoad(CurrentBlock(), "", MSIR.TPtr(MSIR.TVoid()), addr);
      END;
    END;
    tl := MSIR.NewTypeLink(nm, uid);
    MSIR.ModuleAddTypeLink(m, tl);
    addr := MSIR.TypeCellRef(nm);
    RETURN MSIR.BuildLoad(CurrentBlock(), "", MSIR.TPtr(MSIR.TVoid()), addr);
  END TypeLinkValueForObject;

PROCEDURE AddRevelation (lhsUID, rhsUID: INTEGER) =
  VAR m := MSIREmit.CurrentModule();
  BEGIN
    IF m = NIL THEN RETURN END;
    MSIR.ModuleAddRevelation(m, MSIR.NewRevelation(lhsUID, rhsUID));
  END AddRevelation;

PROCEDURE HookProc (h: RunTyme.Hook): MSIR.Proc =
  VAR proc: Procedure.T;
  BEGIN
    IF NOT MSIREmit.IsEnabled() THEN RETURN NIL END;
    proc := RunTyme.LookUpProc(h);
    IF proc = NIL THEN RETURN NIL END;
    RETURN LookupOrCreateProc(proc, Value.TypeOf(proc));
  END HookProc;

PROCEDURE RegisterProc(v: Value.T;  p: MSIR.Proc) =
  BEGIN
    IF v = NIL OR p = NIL THEN RETURN END;
    FOR i := 0 TO procMapN - 1 DO
      IF procMap[i].key = v THEN
        procMap[i].val  := p;
        RETURN;
      END;
    END;
    IF procMapN >= NUMBER (procMap^) THEN
      VAR n := NEW (REF ARRAY OF ProcEntry, 2 * procMapN);
      BEGIN SUBARRAY (n^, 0, procMapN) := procMap^;  procMap := n END;
    END;
    procMap[procMapN].key  := v;
    procMap[procMapN].val  := p;
    INC(procMapN);
  END RegisterProc;

PROCEDURE ProcMapContains(v: Value.T): BOOLEAN =
  BEGIN
    FOR i := 0 TO procMapN - 1 DO
      IF procMap[i].key = v THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END ProcMapContains;

PROCEDURE ProcMapHasBody(v: Value.T): BOOLEAN =
(* TRUE only if v's MSIR proc already has a compiled body (>= 1 block).  A
   bodyless forward stub (created when v is CALLED before its body is compiled,
   e.g. a nested proc called by an earlier-compiled sibling) returns FALSE, so
   GenBodyMSIR proceeds to fill the stub in (BeginProc reuses it). *)
  BEGIN
    FOR i := 0 TO procMapN - 1 DO
      IF procMap[i].key = v THEN
        RETURN MSIR.ProcBlockCount(procMap[i].val) > 0;
      END;
    END;
    RETURN FALSE;
  END ProcMapHasBody;

PROCEDURE LookupOrCreateProc(v: Value.T;  procType: Type.T): MSIR.Proc =
  VAR
    f:            Value.T;
    info:         Formal.Info;
    nFormals:     INTEGER := 0;
    resultT:      MSIR.T;
  BEGIN
    FOR i := 0 TO procMapN - 1 DO
      IF procMap[i].key = v THEN RETURN procMap[i].val END;
    END;
    (* Not found — build an external stub. *)
    resultT := MSIRType.TranslateResult(ProcType.Result(procType));
    IF resultT = NIL THEN
      Abandon("unsupported result type in callee");
      RETURN NIL;
    END;
    (* Native large-result ABI: the callee stub returns by value, no hidden
       result pointer (see BeginProc); resultT stays the real result type, and
       params are just the explicit formals. *)
    f := ProcType.Formals(procType);
    WHILE f # NIL DO INC(nFormals);  f := f.next END;
    VAR params := NEW(REF ARRAY OF MSIR.Param, nFormals);
    BEGIN
      f := ProcType.Formals(procType);
      FOR i := 0 TO nFormals - 1 DO
        Formal.Split(f, info);
        VAR pt := MSIRType.Translate(info.type);
        BEGIN
          IF pt = NIL THEN
            Abandon("unsupported parameter type in callee");
            RETURN NIL;
          END;
          params[i].name := "a." & M3ID.ToText(info.name);
          CASE info.mode OF
          | Formal.Mode.mVALUE =>
              params[i].mode := MSIR.ParamMode.ByValue;
              IF MSIR.Kind(pt) = MSIR.TypeKind.OpenArray THEN
                params[i].type := MSIR.TPtr(pt);
              ELSE
                params[i].type := pt;
              END;
          | Formal.Mode.mVAR      => params[i].mode := MSIR.ParamMode.Var;
                                     params[i].type := MSIR.TPtr(pt);
          | Formal.Mode.mREADONLY =>
              (* Every READONLY formal is a pointer, as in BeginProc and at
                 the call (Formal.EmitArgMSIR); a scalar by value here made an
                 imported procedure's signature disagree with its definition. *)
              params[i].mode := MSIR.ParamMode.Readonly;
              params[i].type := MSIR.TPtr(pt);
          END;
        END;
        f := f.next;
      END;
      (* dots := FALSE → considerExternal = TRUE in NameToPrefix, so
         <* EXTERNAL ThreadPThread__foo *> pragmas are respected instead
         of generating "InterfaceName.proc" with the interface's name. *)
      VAR stub := MSIR.NewProc(Value.GlobalName(v, dots := FALSE), params^, resultT);
      BEGIN
        RegisterProc(v, stub);
        RETURN stub;
      END;
    END;
  END LookupOrCreateProc;

PROCEDURE GrowGlobalMap () =
  BEGIN
    IF globalMapN >= NUMBER (globalMap^) THEN
      VAR n := NEW (REF ARRAY OF GlobalEntry, 2 * globalMapN);
      BEGIN SUBARRAY (n^, 0, globalMapN) := globalMap^;  globalMap := n END;
    END;
  END GrowGlobalMap;

PROCEDURE BeginModule() =
  BEGIN
    globalMapN       := 0;
    procMapN         := 0;
    shimMapN         := 0;
    procContextDepth := 0;
    constArraySeq    := 0;
    memcpyProc       := NIL;
    memcmpProc       := NIL;
    memsetProc       := NIL;
    MSIRType.Reset();
  END BeginModule;

PROCEDURE GetMemcpyProc(): MSIR.Proc =
  BEGIN
    IF memcpyProc = NIL THEN
      memcpyProc := MSIR.NewProc("memcpy",
        ARRAY OF MSIR.Param{
          MSIR.Param{name := "dst",  type := MSIR.TPtr(MSIR.TVoid()),
                     mode := MSIR.ParamMode.ByValue},
          MSIR.Param{name := "src",  type := MSIR.TPtr(MSIR.TVoid()),
                     mode := MSIR.ParamMode.ByValue},
          MSIR.Param{name := "n",    type := MSIR.TI(Target.Integer.size),
                     mode := MSIR.ParamMode.ByValue}
        },
        MSIR.TPtr(MSIR.TVoid()));  (* memcpy returns ptr; result unused *)
    END;
    RETURN memcpyProc;
  END GetMemcpyProc;

PROCEDURE EmitMemcpy(dst, src: MSIR.Value; byteCount: INTEGER) =
  BEGIN
    IF curBlock = NIL OR abandoned THEN RETURN END;
    EVAL MSIR.BuildCall(curBlock, "", GetMemcpyProc(),
      ARRAY OF MSIR.Value{dst, src,
        MSIR.ConstInt(MSIR.TI(Target.Integer.size), byteCount)});
  END EmitMemcpy;

PROCEDURE EmitZeroBytes(dst: MSIR.Value;  byteCount: INTEGER) =
  BEGIN
    IF curBlock = NIL OR abandoned OR byteCount <= 0 THEN RETURN END;
    IF memsetProc = NIL THEN
      memsetProc := MSIR.NewProc("memset",
        ARRAY OF MSIR.Param{
          MSIR.Param{name := "dst", type := MSIR.TPtr(MSIR.TVoid()),
                     mode := MSIR.ParamMode.ByValue},
          MSIR.Param{name := "c",   type := MSIR.TI(32),
                     mode := MSIR.ParamMode.ByValue},
          MSIR.Param{name := "n",   type := MSIR.TI(Target.Integer.size),
                     mode := MSIR.ParamMode.ByValue}
        },
        MSIR.TPtr(MSIR.TVoid()));
    END;
    EVAL MSIR.BuildCall(curBlock, "", memsetProc,
      ARRAY OF MSIR.Value{dst, MSIR.ConstInt(MSIR.TI(32), 0),
        MSIR.ConstInt(MSIR.TI(Target.Integer.size), byteCount)});
  END EmitZeroBytes;

PROCEDURE BeginCountedLoop(count: MSIR.Value;  VAR l: CountedLoop): MSIR.Value =
  VAR intT := MSIR.TI(Target.Integer.size);  body: MSIR.Block;
  BEGIN
    l := CountedLoop{};
    IF curBlock = NIL OR abandoned THEN RETURN NIL END;
    l.hdr  := NewBlock("loop.hdr");
    body   := NewBlock("loop.body");
    l.exit := NewBlock("loop.exit");
    l.idxA := MSIR.BuildAlloca(curBlock, "", intT);
    MSIR.BuildStore(curBlock, MSIR.ConstInt(intT, 0), l.idxA);
    MSIR.BuildBr(curBlock, l.hdr, ARRAY OF MSIR.Value{});
    l.idx := MSIR.BuildLoad(l.hdr, "", intT, l.idxA);
    MSIR.BuildCondBr(l.hdr,
      MSIR.BuildICmp(l.hdr, "", MSIR.CmpPred.Slt, l.idx, count),
      body, ARRAY OF MSIR.Value{}, l.exit, ARRAY OF MSIR.Value{});
    curBlock := body;
    RETURN l.idx;
  END BeginCountedLoop;

PROCEDURE EndCountedLoop(READONLY l: CountedLoop) =
  VAR intT := MSIR.TI(Target.Integer.size);
  BEGIN
    IF l.hdr = NIL THEN RETURN END;
    IF curBlock # NIL THEN
      MSIR.BuildStore(curBlock,
        MSIR.BuildIAdd(curBlock, "", l.idx, MSIR.ConstInt(intT, 1)), l.idxA);
      MSIR.BuildBr(curBlock, l.hdr, ARRAY OF MSIR.Value{});
    END;
    curBlock := l.exit;
  END EndCountedLoop;

PROCEDURE EmitMemcpyDyn(dst, src, byteCount: MSIR.Value) =
  BEGIN
    IF curBlock = NIL OR abandoned THEN RETURN END;
    EVAL MSIR.BuildCall(curBlock, "", GetMemcpyProc(),
      ARRAY OF MSIR.Value{dst, src, byteCount});
  END EmitMemcpyDyn;

PROCEDURE GetMemcmpProc(): MSIR.Proc =
  BEGIN
    IF memcmpProc = NIL THEN
      memcmpProc := MSIR.NewProc("memcmp",
        ARRAY OF MSIR.Param{
          MSIR.Param{name := "a",  type := MSIR.TPtr(MSIR.TVoid()),
                     mode := MSIR.ParamMode.ByValue},
          MSIR.Param{name := "b",  type := MSIR.TPtr(MSIR.TVoid()),
                     mode := MSIR.ParamMode.ByValue},
          MSIR.Param{name := "n",  type := MSIR.TI(Target.Integer.size),
                     mode := MSIR.ParamMode.ByValue}
        },
        MSIR.TI(32));  (* memcmp returns int (0 iff equal) *)
    END;
    RETURN memcmpProc;
  END GetMemcmpProc;

PROCEDURE BuildMemcmp(b: MSIR.Block;  a, bb: MSIR.Value;
                       byteCount: INTEGER): MSIR.Value =
(* memcmp(a, bb, byteCount) -> i32 (0 iff the byteCount bytes are equal).  For
   comparing aggregate values (records / fixed arrays) byte-wise, where LLVM
   icmp does not apply (p280 open-array-of-aggregates equality). *)
  BEGIN
    RETURN MSIR.BuildCall(b, "", GetMemcmpProc(),
      ARRAY OF MSIR.Value{a, bb,
        MSIR.ConstInt(MSIR.TI(Target.Integer.size), byteCount)});
  END BuildMemcmp;

PROCEDURE OpenArrayToFixedStore (lhsPtr, rhsVal: MSIR.Value;
                                  lhsType: Type.T): BOOLEAN =
  VAR
    slotT  := MSIR.ValueType (lhsPtr);
    eltT   := MSIR.EltType (slotT);
    rhsT   := MSIR.ValueType (rhsVal);
    srcPtr : MSIR.Value;
    info   : Type.Info;
  BEGIN
    IF MSIR.Kind (eltT) # MSIR.TypeKind.FixedArray OR
       MSIR.Kind (rhsT) # MSIR.TypeKind.OpenArray THEN
      RETURN FALSE;
    END;
    IF curBlock = NIL OR abandoned THEN RETURN TRUE END;
    srcPtr := OpenArrayDataPtr (curBlock, rhsVal);
    IF MSIR.OpenArrayRank (rhsT) = 1 AND
       MSIR.Equal (MSIR.FixedArrayElt (eltT), MSIR.OpenArrayElt (rhsT)) THEN
      VAR tPtr := MSIR.RetypeValue (srcPtr, MSIR.TPtr (eltT));
          arr  := MSIR.BuildLoad (curBlock, "", eltT, tPtr);
      BEGIN
        MSIR.BuildStore (curBlock, arr, lhsPtr);
      END;
    ELSE
      EVAL Type.CheckInfo (lhsType, info);
      EmitMemcpy (lhsPtr, srcPtr, info.size DIV Target.Char.size);
    END;
    RETURN TRUE;
  END OpenArrayToFixedStore;

PROCEDURE IsWideSet(t: MSIR.T): BOOLEAN =
  BEGIN
    RETURN t # NIL AND MSIR.Kind(t) = MSIR.TypeKind.IWide
       AND MSIR.BitWidth(t) > Target.Word.size;
  END IsWideSet;

PROCEDURE SetIndex(blk: MSIR.Block;  v: MSIR.Value;  minOrd: INTEGER): MSIR.Value =
  VAR iT := MSIR.TI(Target.Integer.size);
  BEGIN
    v := CoerceToMSIR(blk, v, iT);
    IF minOrd # 0 THEN v := MSIR.BuildISub(blk, "", v, MSIR.ConstInt(iT, minOrd)) END;
    RETURN v;
  END SetIndex;

PROCEDURE CoerceToMSIR(blk: MSIR.Block;  v: MSIR.Value;  rt: MSIR.T): MSIR.Value =
  (* Sign-aware widening/truncation for arithmetic operators.
     SExt for signed I-kind (I8, I16, I32, I64 — signed subranges like [-20..20]).
     ZExt for W-kind (unsigned: W8, W16, W32, W64 — CHAR, [0..N], CARDINAL) AND
     for I1 (BOOLEAN is 0/1 unsigned — there is no signed 1-bit type, so SExt
     would turn TRUE into all-ones).
     MSIRType.Translate encodes signedness: non-negative subranges → TW,
     signed subranges → TI.  No coercion needed when bit-widths match. *)
  VAR vb := MSIR.BitWidth (MSIR.ValueType (v));
      rb := MSIR.BitWidth (rt);
  BEGIN
    IF vb <= 0 OR rb <= 0 OR vb = rb THEN RETURN v END;
    IF vb < rb THEN
      IF MSIR.Kind (MSIR.ValueType (v)) >= MSIR.TypeKind.I8 AND
         MSIR.Kind (MSIR.ValueType (v)) <= MSIR.TypeKind.I64
        THEN RETURN MSIR.BuildSExt (blk, "", v, rt);
        ELSE RETURN MSIR.BuildZExt (blk, "", v, rt);
      END;
    END;
    RETURN MSIR.BuildTrunc (blk, "", v, rt);
  END CoerceToMSIR;

PROCEDURE ConstInt(t: MSIR.T;  READONLY v: Target.Int): MSIR.Value =
  VAR x: INTEGER;
  BEGIN
    IF NOT TInt.ToInt(v, x) THEN
      Abandon("ConstInt: value out of range for host INTEGER");
      RETURN NIL;
    END;
    RETURN MSIR.ConstInt(t, x);
  END ConstInt;

PROCEDURE BuildPtrByteOff(b: MSIR.Block;  name: TEXT;  base: MSIR.Value;  off: INTEGER): MSIR.Value =
  BEGIN RETURN MSIR.BuildPtrAdd(b, name, base, off) END BuildPtrByteOff;

PROCEDURE TFixedArrayI(len: INTEGER;  elt: MSIR.T): MSIR.T =
  BEGIN RETURN MSIR.TFixedArray(len, elt) END TFixedArrayI;

PROCEDURE ConstArrayGlobal(eltT: MSIR.T;
                           READONLY elts: ARRAY OF MSIR.Value): MSIR.Value =
  VAR ca := MSIR.NewConstArray("constarray_" & Fmt.Int(constArraySeq),
                               eltT, elts);
  BEGIN
    INC(constArraySeq);
    MSIR.ModuleAddConstArray(MSIREmit.CurrentModule(), ca);
    RETURN MSIR.ConstArrayValue(ca);
  END ConstArrayGlobal;

PROCEDURE OpenArrayDataPtr(blk: MSIR.Block;  oa: MSIR.Value): MSIR.Value =
  VAR rank  := MSIR.OpenArrayRank(MSIR.ValueType(oa));
      zeros := NEW(REF ARRAY OF MSIR.Value, MAX(rank, 1));
  BEGIN
    FOR k := 0 TO NUMBER(zeros^) - 1 DO
      zeros[k] := MSIR.ConstInt(MSIR.TI(Target.Integer.size), 0);
    END;
    RETURN MSIR.BuildOpenArrayElemAddr(blk, "", oa, zeros^);
  END OpenArrayDataPtr;

(* ---- raw map-management helpers called from Variable.m3 ---- *)

PROCEDURE GlobalMapAdd(v: Variable.T;  g: MSIR.Global;  m: MSIR.Module) =
  BEGIN
    FOR i := 0 TO globalMapN-1 DO
      IF globalMap[i].key = v THEN RETURN END;
    END;
    GrowGlobalMap ();
    MSIR.ModuleAddGlobal(m, g);
    globalMap[globalMapN].key         := v;
    globalMap[globalMapN].val         := g;
    globalMap[globalMapN].needsLoad   := FALSE;
    globalMap[globalMapN].dataType    := NIL;
    globalMap[globalMapN].importBind  := NIL;
    globalMap[globalMapN].varByteOff  := 0;
    globalMap[globalMapN].varMSIRType := NIL;
    INC(globalMapN);
  END GlobalMapAdd;

PROCEDURE ImportChainAddr(binderName: TEXT;  varByteOff: INTEGER): MSIR.Value =
  (* Load the import pointer for the module whose binder is 'binderName',
     then GEP by varByteOff bytes to get the address of the imported variable.
     The RT0.ImportInfo chain for the current module has its II_import field
     (at byte offset 0) filled in by RTLinker at runtime with a pointer to the
     imported module's interface struct. *)
  VAR
    m       := MSIREmit.CurrentModule();
    modName : TEXT;
    k       : INTEGER := -1;
    nBind   : INTEGER;
    i       : INTEGER;
    impRef  : MSIR.Value;
    impPtr  : MSIR.Value;
  BEGIN
    IF m = NIL THEN RETURN NIL END;
    modName := MSIR.ModuleName(m);
    nBind   := MSIR.ModuleImportBinderCount(m);
    i       := 0;
    WHILE i < nBind AND k < 0 DO
      IF Text.Equal(MSIR.ModuleImportBinder(m, i), binderName) THEN
        k := i;
      END;
      INC(i);
    END;
    IF k < 0 THEN
      Abandon("import chain: binder not registered: " & binderName);
      RETURN NIL;
    END;
    (* @<modName>_M3_imp.k field 0 = II_import (ptr to imported interface struct). *)
    impRef := MSIR.StructFieldRef(modName & "_M3_imp." & Fmt.Int(k), 0,
                                  MSIR.TPtr(MSIR.TPtr(MSIR.TVoid())));
    impPtr := MSIR.BuildLoad(curBlock, "", MSIR.TPtr(MSIR.TVoid()), impRef);
    IF impPtr = NIL THEN RETURN NIL END;
    IF varByteOff = 0 THEN RETURN impPtr END;
    RETURN MSIR.BuildPtrAdd(curBlock, "", impPtr, varByteOff);
  END ImportChainAddr;

PROCEDURE GlobalMapAddImport(v: Variable.T;  m: MSIR.Module;
                              ownerBinder: TEXT;  varByteOff: INTEGER;
                              varMSIRType: MSIR.T;
                              needsLoad: BOOLEAN := FALSE;
                              dataType: MSIR.T := NIL) =
  (* Register an imported (non-external) M3 variable as an import-chain entry.
     At code-generation time, LookupVar/LookupVarAddr loads the import pointer
     from @<curMod>_M3_imp.k and GEPs to varByteOff to reach the variable.
     needsLoad = TRUE for a large (indirect) global, whose import-chain slot
     holds a POINTER to separately-allocated storage; LookupVar* loads through
     it, returning a Ptr(dataType). *)
  BEGIN
    FOR i := 0 TO globalMapN - 1 DO
      IF globalMap[i].key = v THEN RETURN END;
    END;
    GrowGlobalMap ();
    (* No MSIR.ModuleAddGlobal call — import chain entries have no standalone global. *)
    globalMap[globalMapN].key         := v;
    globalMap[globalMapN].val         := NIL;
    globalMap[globalMapN].needsLoad   := needsLoad;
    globalMap[globalMapN].dataType    := dataType;
    globalMap[globalMapN].importBind  := ownerBinder;
    globalMap[globalMapN].varByteOff  := varByteOff;
    globalMap[globalMapN].varMSIRType := varMSIRType;
    INC(globalMapN);
  END GlobalMapAddImport;

PROCEDURE GlobalMapAddStruct(v: Variable.T;  g: MSIR.Global;  m: MSIR.Module;
                              infoName: TEXT;  byteOff: INTEGER;
                              fieldType: MSIR.T;  needsLoad: BOOLEAN := FALSE;
                              dataType: MSIR.T := NIL) =
  BEGIN
    FOR i := 0 TO globalMapN - 1 DO
      IF globalMap[i].key = v THEN RETURN END;
    END;
    GrowGlobalMap ();
    (* Patch the global with struct field info and a StructFieldRef value. *)
    MSIR.GlobalSetStructField(g, byteOff,
                              MSIR.StructFieldRef(infoName, byteOff, fieldType));
    MSIR.ModuleAddGlobal(m, g);
    globalMap[globalMapN].key         := v;
    globalMap[globalMapN].val         := g;
    globalMap[globalMapN].needsLoad   := needsLoad;
    globalMap[globalMapN].dataType    := dataType;
    globalMap[globalMapN].importBind  := NIL;
    globalMap[globalMapN].varByteOff  := 0;
    globalMap[globalMapN].varMSIRType := NIL;
    INC(globalMapN);
  END GlobalMapAddStruct;

PROCEDURE VarMapAdd(v: Variable.T;  val: MSIR.Value;  storageType: MSIR.T) =
  (* Register a local variable in the varMap.  storageType is the narrow MType
     (actual memory layout).  wideType (ZType) and signedWiden are computed
     here from the M3 type: ordinals narrower than word size get TI64; sign
     is determined by whether the M3 type's lower bound is negative. *)
  VAR wideType    := storageType;
      signedWiden := FALSE;
  BEGIN
    IF varMapN >= MaxVarMap THEN
      Abandon("VarMapAdd: overflow — increase MaxVarMap");
      RETURN;
    END;
    IF storageType # NIL THEN
      VAR vType: Type.T;  vg, vi, vlhs: BOOLEAN;
          lo, hi: Target.Int;
      BEGIN
        Variable.Split(v, vType, vg, vi, vlhs);
        (* Widen all ordinal types to word size — including Enums.
           TryStmt's exception value binding uses CoerceToMSIR to ZExt
           the TEnum value to the wide alloca, so ORD(n) works correctly. *)
        IF Type.IsOrdinal(vType) THEN
          wideType := MSIR.TI(Target.Integer.size);
          (* SExt if lower bound is negative (signed subrange), ZExt otherwise.
             BOOLEAN: lo=0 → ZExt ✓.  Short=[-30..-12]: lo<0 → SExt ✓. *)
          signedWiden := Type.GetBounds(vType, lo, hi) AND TInt.LT(lo, TInt.Zero);
        END;
      END;
    END;
    varMap[varMapN].key         := v;
    varMap[varMapN].isBitField  := FALSE;  (* slot reused: clear stale flag *)
    varMap[varMapN].val         := val;
    varMap[varMapN].storageType := storageType;
    varMap[varMapN].wideType    := wideType;
    varMap[varMapN].signedWiden := signedWiden;
    varMap[varMapN].container   := NIL;
    INC(varMapN);
  END VarMapAdd;

PROCEDURE VarMapContains(v: Variable.T): BOOLEAN =
  BEGIN
    FOR i := 0 TO varMapN - 1 DO
      IF varMap[i].key = v THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END VarMapContains;

PROCEDURE NoteModuleSrcFile() =
  VAR mf: TEXT;  ml: INTEGER;
  BEGIN
    IF NOT MSIREmit.IsEnabled() THEN RETURN END;
    Scanner.Here(mf, ml);
    IF mf # NIL THEN MSIR.ModuleSetSrcFile(MSIREmit.CurrentModule(), mf) END;
  END NoteModuleSrcFile;

PROCEDURE BeginModuleInit(name: TEXT): BOOLEAN =
  VAR resultT: MSIR.T;
  BEGIN
    IF NOT MSIREmit.IsEnabled() THEN RETURN FALSE END;
    <* ASSERT curProc = NIL *>
    abandoned := FALSE;
    varMapN   := 0;
    exitDepth := 0;
    curScope  := NIL;
    blockSeq  := 0;
    resultT   := MSIR.TVoid();
    curProc  := MSIR.NewProc(name, ARRAY OF MSIR.Param{}, resultT);
    curBlock := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
    MSIR.ProcAddBlock(curProc, curBlock);
    RETURN TRUE;
  END BeginModuleInit;

PROCEDURE ExtractBitField (base: MSIR.Value;  bitOff, bitWidth: INTEGER;
                            rawFieldType: Type.T): MSIR.Value =
  VAR
    b         := curBlock;
    byteStart := bitOff DIV 8;
    bitInByte := bitOff MOD 8;
    p0        := MSIR.BuildPtrAdd (b, "", base, byteStart);
    b0        := MSIR.BuildLoad (b, "", MSIR.TI (8), p0);
    word      : MSIR.Value;
    wordBits  : INTEGER;
  BEGIN
    IF bitInByte + bitWidth <= 8 THEN
      word     := b0;
      wordBits := 8;
    ELSIF bitInByte + bitWidth <= 16 THEN
      (* Field spans exactly two bytes: stitch b0 | (b1 << 8) as i16 *)
      VAR p1  := MSIR.BuildPtrAdd (b, "", base, byteStart + 1);
          b1  := MSIR.BuildLoad (b, "", MSIR.TI (8), p1);
          b0w := MSIR.BuildZExt (b, "", b0, MSIR.TI (16));
          b1w := MSIR.BuildZExt (b, "", b1, MSIR.TI (16));
      BEGIN
        word := MSIR.BuildIOr (b, "", b0w,
                    MSIR.BuildIShl (b, "", b1w, MSIR.ConstInt (MSIR.TI (16), 8)));
      END;
      wordBits := 16;
    ELSIF bitInByte + bitWidth <= 24 THEN
      (* Field spans three bytes: stitch b0 | (b1<<8) | (b2<<16) as i32 *)
      VAR p1  := MSIR.BuildPtrAdd (b, "", base, byteStart + 1);
          p2  := MSIR.BuildPtrAdd (b, "", base, byteStart + 2);
          b1  := MSIR.BuildLoad (b, "", MSIR.TI (8), p1);
          b2  := MSIR.BuildLoad (b, "", MSIR.TI (8), p2);
          i32 := MSIR.TI (32);
          b0w := MSIR.BuildZExt (b, "", b0, i32);
          b1w := MSIR.BuildZExt (b, "", b1, i32);
          b2w := MSIR.BuildZExt (b, "", b2, i32);
      BEGIN
        word := MSIR.BuildIOr (b, "",
                  MSIR.BuildIOr (b, "", b0w,
                    MSIR.BuildIShl (b, "", b1w, MSIR.ConstInt (i32, 8))),
                  MSIR.BuildIShl (b, "", b2w, MSIR.ConstInt (i32, 16)));
      END;
      wordBits := 32;
    ELSIF bitInByte + bitWidth > 32 THEN
      (* Field spans five to nine bytes (e.g. RTHeapRep.Desc.link, 46 bits at
         bit 10): stitch the first min(8, n) bytes as i64.  A ninth byte
         (a 64-bit field not starting on a byte boundary) supplies the bits
         above 64 - bitInByte, ORed in after the shift below. *)
      VAR i64 := MSIR.TI (64);
          n   := (bitInByte + bitWidth + 7) DIV 8;
      BEGIN
        word := MSIR.BuildZExt (b, "", b0, i64);
        FOR k := 1 TO MIN (n, 8) - 1 DO
          word := MSIR.BuildIOr (b, "", word,
                    MSIR.BuildIShl (b, "",
                      MSIR.BuildZExt (b, "",
                        MSIR.BuildLoad (b, "", MSIR.TI (8),
                          MSIR.BuildPtrAdd (b, "", base, byteStart + k)),
                        i64),
                      MSIR.ConstInt (i64, 8 * k)));
        END;
        IF n > 8 THEN
          word := MSIR.BuildIOr (b, "",
                    MSIR.BuildILShr (b, "", word,
                      MSIR.ConstInt (i64, bitInByte)),
                    MSIR.BuildIShl (b, "",
                      MSIR.BuildZExt (b, "",
                        MSIR.BuildLoad (b, "", MSIR.TI (8),
                          MSIR.BuildPtrAdd (b, "", base, byteStart + 8)),
                        i64),
                      MSIR.ConstInt (i64, 64 - bitInByte)));
          bitInByte := 0;  (* already shifted into place *)
        END;
      END;
      wordBits := 64;
    ELSE
      (* Field spans four bytes: stitch four bytes as i32 *)
      VAR p1  := MSIR.BuildPtrAdd (b, "", base, byteStart + 1);
          p2  := MSIR.BuildPtrAdd (b, "", base, byteStart + 2);
          p3  := MSIR.BuildPtrAdd (b, "", base, byteStart + 3);
          b1  := MSIR.BuildLoad (b, "", MSIR.TI (8), p1);
          b2  := MSIR.BuildLoad (b, "", MSIR.TI (8), p2);
          b3  := MSIR.BuildLoad (b, "", MSIR.TI (8), p3);
          i32 := MSIR.TI (32);
          b0w := MSIR.BuildZExt (b, "", b0, i32);
          b1w := MSIR.BuildZExt (b, "", b1, i32);
          b2w := MSIR.BuildZExt (b, "", b2, i32);
          b3w := MSIR.BuildZExt (b, "", b3, i32);
      BEGIN
        word := MSIR.BuildIOr (b, "",
                  MSIR.BuildIOr (b, "", b0w,
                    MSIR.BuildIShl (b, "", b1w, MSIR.ConstInt (i32, 8))),
                  MSIR.BuildIOr (b, "",
                    MSIR.BuildIShl (b, "", b2w, MSIR.ConstInt (i32, 16)),
                    MSIR.BuildIShl (b, "", b3w, MSIR.ConstInt (i32, 24))));
      END;
      wordBits := 32;
    END;
    VAR
      wordT    := MSIR.TI (wordBits);
      shifted  := MSIR.BuildILShr (b, "", word,
                      MSIR.ConstInt (wordT, bitInByte));
      maskVal  : INTEGER := 1;
      extracted: MSIR.Value;
    BEGIN
      IF bitWidth >= BITSIZE (INTEGER) THEN
        maskVal := -1;
      ELSE
        FOR i := 1 TO bitWidth DO maskVal := maskVal * 2 END;
        maskVal := maskVal - 1;
      END;
      extracted := MSIR.BuildIAnd (b, "", shifted, MSIR.ConstInt (wordT, maskVal));
      (* Aggregate field (a misaligned {i8,i8}/[3 x i8] record/array field read
         via Misaligned.TheField): the extracted bits are not a scalar ordinal —
         write them out as the aggregate's byte image and return the aggregate
         value, not a widened integer (p280 TestRecord/TestSmallArray). *)
      VAR fieldMsir := MSIRType.Translate (rawFieldType);
      BEGIN
        IF fieldMsir # NIL
           AND (MSIR.Kind (fieldMsir) = MSIR.TypeKind.Struct
                OR MSIR.Kind (fieldMsir) = MSIR.TypeKind.FixedArray) THEN
          (* Clamp the byte count to the aggregate's NATURAL size: a BITS n
             FOR <agg> field may declare MORE bits than the aggregate holds
             (BITS 17 FOR a 16-bit record), and ceil(n/8) would then write
             one byte past the temp alloca (stack clobber, p280 short
             aggregate reads). *)
          VAR natInfo : Type.Info;
              nB      := (bitWidth + 7) DIV 8;
              tmp     := MSIR.BuildAlloca (b, "", fieldMsir);
          BEGIN
            EVAL Type.CheckInfo (Type.StripPacked (rawFieldType), natInfo);
            IF natInfo.size > 0 THEN
              nB := MIN (nB, (natInfo.size + 7) DIV 8);
            END;
            FOR j := 0 TO nB - 1 DO
              MSIR.BuildStore (b,
                MSIR.BuildTrunc (b, "",
                  MSIR.BuildILShr (b, "", extracted,
                    MSIR.ConstInt (wordT, j * 8)),
                  MSIR.TI (8)),
                MSIR.BuildPtrAdd (b, "", tmp, j));
            END;
            RETURN MSIR.BuildLoad (b, "", fieldMsir, tmp);
          END;
        END;
      END;
      VAR
        packedBase : Type.T;
        packedSize : INTEGER;
        naturalT   : MSIR.T;
        lo, hi     : Target.Int;
        doSExt     : BOOLEAN;
      BEGIN
        PackedType.Split (rawFieldType, packedSize, packedBase);
        (* Widen the extracted bitfield to the ZType (machine width) so a
           bitfield read is a uniform-width value like every other ordinal read
           (e.g. RealFloat.Logb's `LOOPHOLE(x,Rep.T).exponent - 127`). *)
        naturalT := MSIRType.ComputeType (packedBase);
        IF naturalT = NIL THEN naturalT := MSIR.TI (Target.Integer.size) END;
        doSExt := Type.GetBounds (packedBase, lo, hi) AND TInt.LT (lo, TInt.Zero);
        VAR srcBits := wordBits;
            dstBits := MSIR.BitWidth (naturalT);
            inNat   : MSIR.Value;
        BEGIN
          (* First widen or narrow extracted to naturalT width. *)
          IF srcBits > dstBits
            THEN inNat := MSIR.BuildTrunc (b, "", extracted, naturalT)
          ELSIF srcBits < dstBits
            THEN inNat := MSIR.BuildZExt  (b, "", extracted, naturalT)
          ELSE       inNat := extracted
          END;
          IF doSExt AND dstBits > bitWidth THEN
            (* Sign-extend from bitWidth bits to dstBits via shift trick. *)
            VAR shift := MSIR.ConstInt (naturalT, dstBits - bitWidth);
            BEGIN
              RETURN MSIR.BuildIAShr (b, "",
                       MSIR.BuildIShl (b, "", inNat, shift), shift)
            END
          ELSE
            RETURN inNat
          END
        END;
      END;
    END;
  END ExtractBitField;

PROCEDURE AggByteSize (t: MSIR.T): INTEGER =
  (* Byte size of an MSIR value type; -1 when it cannot be determined. *)
  VAR w: INTEGER;
  BEGIN
    IF t = NIL THEN RETURN -1 END;
    CASE MSIR.Kind (t) OF
    | MSIR.TypeKind.FixedArray =>
        VAR eb := AggByteSize (MSIR.FixedArrayElt (t));
        BEGIN
          IF eb <= 0 THEN RETURN -1 END;
          RETURN MSIR.FixedArrayLen (t) * eb;
        END;
    | MSIR.TypeKind.Struct =>
        VAR n := MSIR.StructFieldCount (t);
        BEGIN
          IF n = 0 THEN RETURN 0 END;
          VAR f  := MSIR.StructField (t, n - 1);
              eb := AggByteSize (f.type);
          BEGIN
            IF eb <= 0 THEN RETURN -1 END;
            RETURN f.offset + eb;
          END;
        END;
    | MSIR.TypeKind.Ptr, MSIR.TypeKind.GcRef, MSIR.TypeKind.GcSlot =>
        RETURN Target.Address.bytes;
    ELSE
        w := MSIR.BitWidth (t);
        IF w <= 0 THEN RETURN -1 END;
        RETURN (w + 7) DIV 8;
    END;
  END AggByteSize;

PROCEDURE InsertBitField (base: MSIR.Value;  bitOff, bitWidth: INTEGER;
                           rhs: MSIR.Value) =
  VAR
    b         := curBlock;
    byteStart := bitOff DIV 8;
    bitInByte := bitOff MOD 8;
    p0        : MSIR.Value;
    b0        : MSIR.Value;
    maskVal   : INTEGER := 1;
  BEGIN
    (* An aggregate rhs — a small record/array VALUE stored into a misaligned
       field, e.g. `Misaligned.TheField := Val1` where TheField is {i8,i8} or
       [3 x i8] — has no scalar bit width and cannot be zext/trunc'd by the bit
       math below.  Spill it and reassemble its bytes (little-endian) into a
       machine-width integer first (p280 TestRecord/TestSmallArray). *)
    VAR rhsK := MSIR.Kind (MSIR.ValueType (rhs));
    BEGIN
      IF rhsK = MSIR.TypeKind.Struct OR rhsK = MSIR.TypeKind.FixedArray THEN
        VAR aggT := MSIR.ValueType (rhs);
            nB   := (bitWidth + 7) DIV 8;
            intT := MSIR.TI (Target.Integer.size);
            tmp  := MSIR.BuildAlloca (b, "", aggT);
            acc  : MSIR.Value := MSIR.ConstInt (intT, 0);
            aggB : INTEGER;
        BEGIN
          (* Clamp to the aggregate's own byte size: the declared field width
             (BITS n FOR <agg>) may exceed the aggregate's bits, and reading
             ceil(n/8) bytes would run past the spill temp. *)
          aggB := AggByteSize (aggT);
          IF aggB > 0 THEN nB := MIN (nB, aggB) END;
          MSIR.BuildStore (b, rhs, tmp);
          FOR j := 0 TO nB - 1 DO
            VAR bj := MSIR.BuildLoad (b, "", MSIR.TI (8),
                        MSIR.BuildPtrAdd (b, "", tmp, j));
            BEGIN
              acc := MSIR.BuildIOr (b, "", acc,
                       MSIR.BuildIShl (b, "", MSIR.BuildZExt (b, "", bj, intT),
                         MSIR.ConstInt (intT, j * 8)));
            END;
          END;
          rhs := acc;
        END;
      END;
    END;
    (* Wide field: > 16 bits (> 2 bytes).  Decompose into 8-bit chunks, each
       injected by a recursive single/two-byte InsertBitField call. *)
    IF bitInByte + bitWidth > 16 THEN
      VAR intT     := MSIR.TI (Target.Integer.size);
          rhsBits  := MSIR.BitWidth (MSIR.ValueType (rhs));
          rhsNat   : MSIR.Value;
          bitsLeft := bitWidth;
          boff     := bitOff;
      BEGIN
        IF rhsBits < Target.Integer.size
          THEN rhsNat := MSIR.BuildZExt  (b, "", rhs, intT)
        ELSIF rhsBits > Target.Integer.size
          THEN rhsNat := MSIR.BuildTrunc (b, "", rhs, intT)
        ELSE       rhsNat := rhs
        END;
        WHILE bitsLeft > 0 DO
          b := curBlock;
          VAR bitsNow := MIN (8, bitsLeft);
              jByte   := (boff - bitOff) DIV 8;
              shifted := MSIR.BuildILShr (b, "", rhsNat,
                             MSIR.ConstInt (intT, jByte * 8));
              byteJ   := MSIR.BuildIAnd (b, "", shifted,
                             MSIR.ConstInt (intT, 16_FF));
          BEGIN
            InsertBitField (base, boff, bitsNow, byteJ);
            INC (boff, bitsNow);
            DEC (bitsLeft, bitsNow);
          END;
        END;
      END;
      RETURN;
    END;
    p0 := MSIR.BuildPtrAdd (b, "", base, byteStart);
    b0 := MSIR.BuildLoad (b, "", MSIR.TI (8), p0);
    FOR i := 1 TO bitWidth DO maskVal := maskVal * 2 END;
    maskVal := maskVal - 1;   (* (1 << bitWidth) - 1 *)
    IF bitInByte + bitWidth <= 8 THEN
      VAR mk : INTEGER := maskVal;
      BEGIN
        FOR i := 1 TO bitInByte DO mk := mk * 2 END;
        mk := mk MOD 256;
        VAR i8T     := MSIR.TI (8);
            rhsBits := MSIR.BitWidth (MSIR.ValueType (rhs));
            notMask := MSIR.ConstInt (i8T, (256 - mk - 1) MOD 256);
            val8    : MSIR.Value;
            shifted : MSIR.Value;
          cleared : MSIR.Value;
          merged  : MSIR.Value;
        BEGIN
          IF rhsBits > 8
            THEN val8 := MSIR.BuildTrunc (b, "", rhs, i8T)
          ELSIF rhsBits < 8
            THEN val8 := MSIR.BuildZExt  (b, "", rhs, i8T)
          ELSIF MSIR.Equal (MSIR.ValueType (rhs), i8T)
            THEN val8 := rhs
          ELSE       val8 := MSIR.RetypeValue (rhs, i8T)
          END;
          shifted := MSIR.BuildIShl (b, "", val8,
                       MSIR.ConstInt (i8T, bitInByte));
          cleared := MSIR.BuildIAnd (b, "", b0, notMask);
          merged  := MSIR.BuildIOr  (b, "", cleared, shifted);
          MSIR.BuildStore (b, merged, p0);
        END;
      END;
    ELSE
      VAR
        p1   := MSIR.BuildPtrAdd (b, "", base, byteStart + 1);
        b1   := MSIR.BuildLoad   (b, "", MSIR.TI (8), p1);
        b0w  := MSIR.BuildZExt   (b, "", b0, MSIR.TI (16));
        b1w  := MSIR.BuildZExt   (b, "", b1, MSIR.TI (16));
        word := MSIR.BuildIOr    (b, "", b0w,
                    MSIR.BuildIShl (b, "", b1w, MSIR.ConstInt (MSIR.TI (16), 8)));
        mk16 : INTEGER := maskVal;
      BEGIN
        FOR i := 1 TO bitInByte DO mk16 := mk16 * 2 END;
        VAR
          notMask16 := MSIR.ConstInt (MSIR.TI (16), (16_10000 - mk16 - 1) MOD 16_10000);
          rhsBitsX  := MSIR.BitWidth (MSIR.ValueType (rhs));
          bwT       := MSIR.TI (bitWidth);
          valTrunc  : MSIR.Value;
          val16     : MSIR.Value;
          shiftedV  : MSIR.Value;
          merged    : MSIR.Value;
        BEGIN
          IF rhsBitsX > bitWidth
            THEN valTrunc := MSIR.BuildTrunc (b, "", rhs, bwT)
          ELSIF rhsBitsX < bitWidth
            THEN valTrunc := MSIR.BuildZExt  (b, "", rhs, bwT)
          ELSE       valTrunc := rhs
          END;
          val16    := MSIR.BuildZExt  (b, "", valTrunc, MSIR.TI (16));
          shiftedV := MSIR.BuildIShl  (b, "", val16,
                          MSIR.ConstInt (MSIR.TI (16), bitInByte));
          merged   := MSIR.BuildIOr (b, "",
                          MSIR.BuildIAnd (b, "", word, notMask16), shiftedV);
          MSIR.BuildStore (b, MSIR.BuildTrunc (b, "", merged, MSIR.TI (8)), p0);
          MSIR.BuildStore (b, MSIR.BuildTrunc (b, "",
                                MSIR.BuildILShr (b, "", merged,
                                    MSIR.ConstInt (MSIR.TI (16), 8)),
                                MSIR.TI (8)), p1);
        END;
      END;
    END;
  END InsertBitField;

PROCEDURE ExtractBitFieldDyn (base: MSIR.Value;  eltPack: INTEGER;
                               containerBits: INTEGER;
                               idx: MSIR.Value;  rawEltType: Type.T): MSIR.Value =
  VAR b    := curBlock;
      intT := MSIR.TI (Target.Integer.size);
      i8T  := MSIR.TI (8);
  BEGIN
    IF 8 MOD eltPack # 0 THEN
      (* Non-power-of-2 eltPack (e.g. 6-bit SixBit in 16-bit TwelveBit array).
         Load the entire container as a wide integer, shift, and mask.
         BitOffset(i) = i * eltPack.  containerBits must be a multiple of 8
         and ≤ 64.  Returns NIL if containerBits is unknown or too large. *)
      IF containerBits <= 0 OR containerBits MOD 8 # 0 OR containerBits > 64 THEN
        RETURN NIL;
      END;
      VAR wideT    := MSIR.TI (containerBits);
          voidBase := MSIR.BuildGepByte (b, "", base, MSIR.ConstInt (intT, 0));
          loaded   := MSIR.BuildLoad (b, "", wideT, voidBase);
          idxW     := idx;
          maskV    : INTEGER := 1;
          idxBitsW := MSIR.BitWidth (MSIR.ValueType (idx));
      BEGIN
        IF idxBitsW # containerBits THEN
          IF idxBitsW > containerBits
            THEN idxW := MSIR.BuildTrunc (b, "", idx, wideT)  (* e.g. i64→i32 *)
            ELSE idxW := MSIR.BuildZExt  (b, "", idx, wideT)  (* e.g. i8→i32  *)
          END;
        END;
        (* shift = idx * eltPack *)
        VAR shift := MSIR.BuildIMul (b, "", idxW,
                       MSIR.ConstInt (wideT, eltPack));
            shiftedW := MSIR.BuildILShr (b, "", loaded, shift);
        BEGIN
          FOR i := 1 TO eltPack DO maskV := maskV * 2 END;
          maskV := maskV - 1;
          VAR extracted := MSIR.BuildIAnd (b, "", shiftedW,
                             MSIR.ConstInt (wideT, maskV));
          BEGIN
            (* Widen/narrow to natural element type *)
            VAR packedBase: Type.T;  packedSize: INTEGER;
                naturalT: MSIR.T;  lo, hi: Target.Int;  doSExt: BOOLEAN;
                dstBits: INTEGER;
            BEGIN
              PackedType.Split (rawEltType, packedSize, packedBase);
              naturalT := MSIRType.ComputeType (packedBase);  (* ZType: widen packed elt read to machine width *)
              IF naturalT = NIL THEN naturalT := intT END;
              doSExt := Type.GetBounds (packedBase, lo, hi) AND TInt.LT (lo, TInt.Zero);
              dstBits := MSIR.BitWidth (naturalT);
              VAR inNat: MSIR.Value;
              BEGIN
                IF containerBits > dstBits
                  THEN inNat := MSIR.BuildTrunc (b, "", extracted, naturalT)
                ELSIF containerBits < dstBits
                  THEN inNat := MSIR.BuildZExt  (b, "", extracted, naturalT)
                ELSE  inNat := extracted
                END;
                IF doSExt AND dstBits > eltPack THEN
                  VAR shift2 := MSIR.ConstInt (naturalT, dstBits - eltPack);
                  BEGIN
                    RETURN MSIR.BuildIAShr (b, "",
                             MSIR.BuildIShl (b, "", inNat, shift2), shift2);
                  END;
                END;
                RETURN inNat;
              END;
            END;
          END;
        END;
      END;
    END;
    VAR idxW := idx;
    BEGIN
      IF MSIR.BitWidth (MSIR.ValueType (idx)) # Target.Integer.size THEN
        idxW := MSIR.BuildZExt (b, "", idx, intT);
      END;
      VAR elemsPerByte := 8 DIV eltPack;
          logEPB       := 0;
          tmp          := elemsPerByte;
      BEGIN
        WHILE tmp > 1 DO logEPB := logEPB + 1; tmp := tmp DIV 2 END;
        (* byteOff = idx >> logEPB  (byte containing element idx) *)
        VAR byteOff    := MSIR.BuildILShr (b, "", idxW,
                              MSIR.ConstInt (intT, logEPB));
            bytePtr    := MSIR.BuildGepByte (b, "", base, byteOff);
            loaded     := MSIR.BuildLoad (b, "", i8T, bytePtr);
            (* bitInByte = (idx AND (elemsPerByte-1)) * eltPack *)
            modMask    := MSIR.ConstInt (intT, elemsPerByte - 1);
            bitInByteW := MSIR.BuildIMul (b, "",
                              MSIR.BuildIAnd (b, "", idxW, modMask),
                              MSIR.ConstInt (intT, eltPack));
            bitInByte8 := MSIR.BuildTrunc (b, "", bitInByteW, i8T);
            shifted    := MSIR.BuildILShr (b, "", loaded, bitInByte8);
            maskVal    : INTEGER := 1;
            extracted  : MSIR.Value;
        BEGIN
          FOR i := 1 TO eltPack DO maskVal := maskVal * 2 END;
          maskVal := maskVal - 1;   (* (1 << eltPack) - 1 *)
          extracted := MSIR.BuildIAnd (b, "", shifted,
                           MSIR.ConstInt (i8T, maskVal));
          VAR
            packedBase : Type.T;
            packedSize : INTEGER;
            naturalT   : MSIR.T;
            lo, hi     : Target.Int;
            doSExt     : BOOLEAN;
          BEGIN
            PackedType.Split (rawEltType, packedSize, packedBase);
            naturalT := MSIRType.ComputeType (packedBase);  (* ZType: widen packed elt read to machine width *)
            IF naturalT = NIL THEN naturalT := MSIR.TI (Target.Integer.size) END;
            doSExt := Type.GetBounds (packedBase, lo, hi) AND TInt.LT (lo, TInt.Zero);
            VAR srcBits8 := 8;  (* extracted is always i8 here *)
                dstBits  := MSIR.BitWidth (naturalT);
                inNat    : MSIR.Value;
            BEGIN
              IF srcBits8 > dstBits
                THEN inNat := MSIR.BuildTrunc (b, "", extracted, naturalT)
              ELSIF srcBits8 < dstBits
                THEN inNat := MSIR.BuildZExt  (b, "", extracted, naturalT)
              ELSE       inNat := extracted
              END;
              IF doSExt AND dstBits > eltPack THEN
                VAR shift := MSIR.ConstInt (naturalT, dstBits - eltPack);
                BEGIN
                  RETURN MSIR.BuildIAShr (b, "",
                           MSIR.BuildIShl (b, "", inNat, shift), shift)
                END
              ELSE
                RETURN inNat
              END
            END;
          END;
        END;
      END;
    END;
  END ExtractBitFieldDyn;

PROCEDURE InsertBitFieldDyn (base: MSIR.Value;  eltPack: INTEGER;
                              containerBits: INTEGER := 0;
                              idx: MSIR.Value;  rhs: MSIR.Value) =
  VAR b    := curBlock;
      intT := MSIR.TI (Target.Integer.size);
      i8T  := MSIR.TI (8);
  BEGIN
    IF 8 MOD eltPack # 0 THEN
      (* Non-power-of-2 eltPack: byte-level read-modify-write matching C backend.
         The C backend writes exactly one byte per element store, even when the
         element spans a byte boundary; this replicates that behaviour so that
         MSIR results match the C reference on split-byte elements. *)
      VAR idxW     := idx;
          maskVal  : INTEGER := 1;
      BEGIN
        IF MSIR.BitWidth (MSIR.ValueType (idx)) # Target.Integer.size THEN
          idxW := MSIR.BuildZExt (b, "", idx, intT);
        END;
        FOR i := 1 TO eltPack DO maskVal := maskVal * 2 END;
        maskVal := maskVal - 1;   (* (1 << eltPack) - 1 *)
        VAR bitOff     := MSIR.BuildIMul (b, "", idxW,
                              MSIR.ConstInt (intT, eltPack));
            byteOff    := MSIR.BuildILShr (b, "", bitOff,
                              MSIR.ConstInt (intT, 3));
            bytePtr    := MSIR.BuildGepByte (b, "", base, byteOff);
            b0         := MSIR.BuildLoad (b, "", i8T, bytePtr);
            bitInByteW := MSIR.BuildIAnd (b, "", bitOff,
                              MSIR.ConstInt (intT, 7));
            bitInByte8 := MSIR.BuildTrunc (b, "", bitInByteW, i8T);
            mask8      := MSIR.ConstInt (i8T, maskVal);
            shiftedMask:= MSIR.BuildIShl  (b, "", mask8, bitInByte8);
            notMask8   := MSIR.BuildIXor  (b, "", shiftedMask,
                               MSIR.ConstInt (i8T, 255));
            val8       := MSIR.BuildTrunc (b, "", rhs, i8T);
            positioned := MSIR.BuildIShl  (b, "", val8, bitInByte8);
            cleared    := MSIR.BuildIAnd  (b, "", b0, notMask8);
            merged     := MSIR.BuildIOr   (b, "", cleared, positioned);
        BEGIN
          MSIR.BuildStore (b, merged, bytePtr);
        END;
      END;
      RETURN;
    END;
    VAR idxW := idx;
    BEGIN
      IF MSIR.BitWidth (MSIR.ValueType (idx)) # Target.Integer.size THEN
        idxW := MSIR.BuildZExt (b, "", idx, intT);
      END;
      VAR elemsPerByte := 8 DIV eltPack;
          logEPB       := 0;
          tmp          := elemsPerByte;
      BEGIN
        WHILE tmp > 1 DO logEPB := logEPB + 1; tmp := tmp DIV 2 END;
        VAR byteOff    := MSIR.BuildILShr (b, "", idxW,
                              MSIR.ConstInt (intT, logEPB));
            bytePtr    := MSIR.BuildGepByte (b, "", base, byteOff);
            b0         := MSIR.BuildLoad (b, "", i8T, bytePtr);
            modMask    := MSIR.ConstInt (intT, elemsPerByte - 1);
            bitInByteW := MSIR.BuildIMul (b, "",
                              MSIR.BuildIAnd (b, "", idxW, modMask),
                              MSIR.ConstInt (intT, eltPack));
            bitInByte8 := MSIR.BuildTrunc (b, "", bitInByteW, i8T);
            maskVal    : INTEGER := 1;
        BEGIN
          FOR i := 1 TO eltPack DO maskVal := maskVal * 2 END;
          maskVal := maskVal - 1;   (* (1 << eltPack) - 1 *)
          VAR
            mask8       := MSIR.ConstInt (i8T, maskVal);
            shiftedMask := MSIR.BuildIShl  (b, "", mask8, bitInByte8);
            notMask8    := MSIR.BuildIXor  (b, "", shiftedMask,
                               MSIR.ConstInt (i8T, 255));
            val8        := MSIR.BuildTrunc (b, "", rhs, i8T);
            positioned  := MSIR.BuildIShl  (b, "", val8, bitInByte8);
            cleared     := MSIR.BuildIAnd  (b, "", b0, notMask8);
            merged      := MSIR.BuildIOr   (b, "", cleared, positioned);
          BEGIN
            MSIR.BuildStore (b, merged, bytePtr);
          END;
        END;
      END;
    END;
  END InsertBitFieldDyn;

PROCEDURE ExtractBitLoc (READONLY loc: MSIRBitLoc.T): MSIR.Value =
  BEGIN
    IF loc.idx = NIL THEN
      RETURN ExtractBitField (loc.base, loc.bitOff, loc.width, loc.type);
    ELSE
      RETURN ExtractBitFieldDyn (loc.base, loc.width, loc.containerBits,
                                 loc.idx, loc.type);
    END;
  END ExtractBitLoc;

PROCEDURE InsertBitLoc (READONLY loc: MSIRBitLoc.T;  rhs: MSIR.Value) =
  BEGIN
    IF loc.readOnly THEN
      Abandon ("store into a bit field of a value with no address");
    ELSIF loc.idx = NIL THEN
      InsertBitField (loc.base, loc.bitOff, loc.width, rhs);
    ELSE
      InsertBitFieldDyn (loc.base, loc.width, loc.containerBits, loc.idx, rhs);
    END;
  END InsertBitLoc;

PROCEDURE GenLocation() =
  VAR f: TEXT;  l: INTEGER;
  BEGIN
    IF NOT InProc() THEN RETURN END;
    Scanner.Here(f, l);
    IF l > 0 THEN MSIR.SetCurrentSrcLine(l) END;
  END GenLocation;

BEGIN
END MSIRBuilder.
