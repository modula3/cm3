MODULE MSIRBuilder;

IMPORT MSIR, MSIRType, MSIREmit;
IMPORT M3ID, Type, Value, Formal, Variable, Scope, ProcType, Fmt, Target, Text;
IMPORT Error;
IMPORT RunTyme, Procedure, M3FP, CaptureAnalysis, M3RT, TypeFP;
IMPORT Expr, ArrayExpr, ArrayType, RecordExpr;
IMPORT PackedType, TInt;
IMPORT Scanner;
IMPORT RefType;

CONST MaxVarMap    = 512;  (* EmitInsn alone has ~108 local vars; 512 gives headroom *)
CONST MaxExitStack = 16;
CONST MaxTryDepth  = 16;
CONST MaxCatchDepth = 16;
CONST MaxCleanup   = 64;   (* unified loop/finally cleanup-frame stack *)
CONST MaxProcMap   = 2048;
CONST MaxShimMap   = 256;
CONST MaxGlobalMap  = 256;
(* TRY/FINALLY selector codes Sel_Normal/Sel_Exc/Sel_Exit are exported from the
   MSIRBuilder interface and visible here without redeclaration. *)
CONST MaxNestDepth  = 16;  (* maximum nesting depth for nested procs *)

(* Each formal maps to a Param SSA value (elemType = NIL).
   Each local maps to an alloca ptr (elemType = the allocated type). *)
TYPE VarEntry = RECORD
  key:      Variable.T;
  val:      MSIR.Value;
  elemType: MSIR.T;       (* NIL => formal; non-NIL => local alloca ptr *)
END;

(* One frame of the unified loop/finally cleanup stack. *)
TYPE CleanupKind = {Loop, Finally};
TYPE CleanupFrame = RECORD
  kind:      CleanupKind;
  exitBlock: MSIR.Block := NIL;   (* Loop: branch target for EXIT *)
  finBody:   MSIR.Block := NIL;   (* Finally: shared finally-body entry *)
  selector:  MSIR.Value := NIL;   (* Finally: i32 alloca holding a Sel_* code *)
  exitSeen:  BOOLEAN := FALSE;     (* Finally: an EXIT routed through it (so its
                                      epilogue must emit the Sel_Exit arm) *)
END;

(* Saved state for one level of proc context (for nested proc compilation). *)
TYPE ProcContext = RECORD
  proc:          MSIR.Proc;
  block:         MSIR.Block;
  abandoned:     BOOLEAN;
  blockSeq:      INTEGER;
  pending:       MSIR.Value;   (* pendingContainer *)
  resultPtr:     MSIR.Value;   (* curResultPtr *)
  resultType:    MSIR.T;       (* curResultType *)
  varMapN:       INTEGER;
  varMap:        ARRAY [0..MaxVarMap-1] OF VarEntry;
  exitDepth:     INTEGER;
  tryDepth:      INTEGER;
  catchDepth:    INTEGER;
  cleanupDepth:  INTEGER;
END;

TYPE ProcEntry   = RECORD
  key:  Value.T;
  val:  MSIR.Proc;
  caps: REF ARRAY OF CaptureAnalysis.Capture;  (* NIL for non-nested procs *)
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
  pendingContainer: MSIR.Value := NIL;
  curResultPtr:     MSIR.Value := NIL;
  curResultType:    MSIR.T     := NIL;  (* non-void MSIR type for large-result procs *)

  (* Saved contexts for nested proc compilation. *)
  procContextStack: ARRAY [0..MaxNestDepth-1] OF ProcContext;
  procContextDepth: INTEGER := 0;

  varMap:  ARRAY [0..MaxVarMap-1]  OF VarEntry;
  varMapN: INTEGER := 0;

  exitStack: ARRAY [0..MaxExitStack-1] OF MSIR.Block;
  exitDepth: INTEGER := 0;

  tryStack:  ARRAY [0..MaxTryDepth-1] OF MSIR.Block;
  tryDepth:  INTEGER := 0;

  catchStack: ARRAY [0..MaxCatchDepth-1] OF MSIR.Proc;  (* endCatch procs *)
  catchDepth: INTEGER := 0;

  (* Unified cleanup-frame stack in nesting order — loops and finallys
     interleaved — so a non-local EXIT can run every intervening finally before
     branching to its target loop's exit block.  Loop frames are pushed by
     PushExitBlock; Finally frames by PushFinallyCleanup. *)
  cleanupStack: ARRAY [0..MaxCleanup-1] OF CleanupFrame;
  cleanupDepth: INTEGER := 0;

  procMap:  ARRAY [0..MaxProcMap-1] OF ProcEntry;
  procMapN: INTEGER := 0;

TYPE ShimEntry = RECORD key: Value.T; val: MSIR.Proc END;
VAR
  shimMap:  ARRAY [0..MaxShimMap-1] OF ShimEntry;
  shimMapN: INTEGER := 0;

VAR globalMap:  ARRAY [0..MaxGlobalMap-1] OF GlobalEntry;
  globalMapN: INTEGER := 0;

TYPE ConstArrayEntry = RECORD key: Value.T; val: MSIR.Value END;
CONST MaxConstArrayMap = 64;
VAR
  constArrayMap:  ARRAY [0..MaxConstArrayMap-1] OF ConstArrayEntry;
  constArrayMapN: INTEGER := 0;
  constArraySeq:  INTEGER := 0;

  memcpyProc: MSIR.Proc := NIL;  (* lazy C memcpy stub *)

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
      pendingContainer := ctx.pending;
      curResultPtr     := ctx.resultPtr;
      curResultType    := ctx.resultType;
      varMapN          := ctx.varMapN;
      exitDepth        := ctx.exitDepth;
      tryDepth         := ctx.tryDepth;
      catchDepth       := ctx.catchDepth;
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
    isLargeResult: BOOLEAN := FALSE;
    nHidden:       INTEGER := 0;
    pBase:         INTEGER;  (* param index offset = nHidden + nCaptures *)
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
        ctx.pending    := pendingContainer;
        ctx.resultPtr  := curResultPtr;
        ctx.resultType := curResultType;
        ctx.varMapN    := varMapN;
        ctx.exitDepth  := exitDepth;
        ctx.tryDepth   := tryDepth;
        ctx.catchDepth := catchDepth;
        ctx.cleanupDepth := cleanupDepth;
        FOR i := 0 TO varMapN - 1 DO ctx.varMap[i] := varMap[i] END;
      END;
      INC(procContextDepth);
      (* Prefix nested proc name with parent's fully-qualified name using "__"
         (the M3 ABI separator) so the LLVM symbol matches the C backend. *)
      name := MSIR.ProcName(procContextStack[procContextDepth - 1].proc)
              & "__" & name;
    END;
    abandoned      := FALSE;
    varMapN        := 0;
    exitDepth      := 0;
    tryDepth       := 0;
    catchDepth     := 0;
    cleanupDepth   := 0;
    blockSeq       := 0;
    curResultPtr   := NIL;
    curResultType  := NIL;

    resultT := MSIRType.TranslateResult(result);
    IF resultT = NIL THEN
      MSIREmit.NoteSkipped(name, "unsupported result type");
      PopBeginContext();
      RETURN FALSE;
    END;
    isLargeResult := ProcType.LargeResult(result);
    IF isLargeResult THEN
      curResultType := resultT;  (* save translated type before overriding *)
      resultT       := MSIR.TVoid();
      nHidden       := 1;
    ELSE
      nHidden := 0;
    END;

    isNested  := procContextDepth > 0;
    IF captures = NIL THEN caps := NIL
    ELSE caps := CaptureAnalysis.GetCaptures(captures)
    END;
    IF caps = NIL THEN nCaptures := 0
    ELSE nCaptures := NUMBER(caps^)
    END;
    pBase := nHidden + nCaptures;

    f := formals;
    WHILE f # NIL DO INC(nFormals); f := f.next END;

    VAR params := NEW(REF ARRAY OF MSIR.Param, nHidden + nCaptures + nFormals);
    BEGIN
      (* Hidden result pointer: first param for large-result procs. *)
      IF isLargeResult THEN
        params[0].name := "_result_ptr";
        params[0].type := MSIR.TPtr(MSIR.TVoid());
        params[0].mode := MSIR.ParamMode.ByValue;
      END;
      (* Lambda-lifted capture params.
         Read-only scalar captures pass by value (Integer, Float, Ptr).
         Written or aggregate captures pass by opaque ptr. *)
      FOR i := 0 TO nCaptures - 1 DO
        VAR v:  Variable.T := caps[i].var;
            vt: Type.T;  vg, vi, vlhs: BOOLEAN;
            mt: MSIR.T;
        BEGIN
          Variable.Split(v, vt, vg, vi, vlhs);
          mt := MSIRType.Translate(vt);
          params[nHidden + i].name := "__cap_" & Fmt.Int(i);
          params[nHidden + i].mode := MSIR.ParamMode.ByValue;
          IF NOT caps[i].written AND mt # NIL AND IsScalarType(mt) THEN
            params[nHidden + i].type := mt;          (* pass the value directly *)
          ELSE
            params[nHidden + i].type := MSIR.TPtr(MSIR.TVoid());  (* pass ptr *)
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
              params[i + pBase].mode := MSIR.ParamMode.Readonly;
              CASE MSIR.Kind(pt) OF
              | MSIR.TypeKind.Struct,    MSIR.TypeKind.FixedArray,
                MSIR.TypeKind.OpenArray, MSIR.TypeKind.HeapArray,
                MSIR.TypeKind.Object,    MSIR.TypeKind.Set =>
                  params[i + pBase].type := MSIR.TPtr(pt);
              ELSE
                  params[i + pBase].type := pt;
              END;
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

      (* Hidden result ptr: first param for large-result procs. *)
      IF isLargeResult THEN
        curResultPtr := MSIR.ProcParam(curProc, 0);
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
              varMap[varMapN].val := MSIR.ProcParam(curProc, nHidden + i);
              IF NOT caps[i].written AND IsScalarType(mt) THEN
                varMap[varMapN].elemType := NIL;  (* value: return param directly *)
              ELSE
                varMap[varMapN].elemType := mt;   (* ptr: load through it *)
              END;
              INC(varMapN);
            END;
          END;
        END;
      END;

      (* Bind explicit formals. *)
      VAR fDecl := formals;  fInfo: Formal.Info;
      BEGIN
        WHILE fDecl # NIL DO
          Formal.Split(fDecl, fInfo);
          VAR sv := Scope.LookUp(syms, fInfo.name, strict := TRUE);
          BEGIN
            TYPECASE sv OF
            | Variable.T(svv) => Variable.BindFormalMSIR(svv, curProc, curBlock);
            ELSE
            END;
          END;
          fDecl := fDecl.next;
        END;
      END;
      (* Bind non-formal locals. *)
      VAR sv: Value.T := Scope.ToList(syms);
      BEGIN
        WHILE sv # NIL DO
          TYPECASE sv OF
          | Variable.T(svv) =>
              IF NOT Variable.IsFormal(svv) THEN
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
  VAR gv: MSIR.Value;  gt: MSIR.T;
  BEGIN
    FOR i := 0 TO varMapN - 1 DO
      IF varMap[i].key = v THEN
        IF varMap[i].elemType = NIL THEN
          RETURN varMap[i].val;   (* formal: return param value directly *)
        ELSE
          (* local: emit a load from the alloca ptr *)
          RETURN MSIR.BuildLoad(curBlock, "", varMap[i].elemType, varMap[i].val);
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
            IF addr = NIL OR mt = NIL THEN RETURN NIL END;
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
          RETURN MSIR.BuildLoad(curBlock, "", gt, gv);
        END;
      END;
    END;
    RETURN NIL;
  END LookupVar;

PROCEDURE LookupVarAddr(v: Variable.T): MSIR.Value =
  BEGIN
    FOR i := 0 TO varMapN - 1 DO
      IF varMap[i].key = v THEN
        IF varMap[i].elemType = NIL THEN
          Abandon("cannot store to by-value formal in MSIR v0");
          RETURN NIL;
        END;
        RETURN varMap[i].val;   (* alloca ptr *)
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
  VAR
    type:                  Type.T;
    global, indirect, lhs: BOOLEAN;
    mt:                    MSIR.T;
    allocaVal:             MSIR.Value;
  BEGIN
    (* Idempotent: skip if already registered (e.g. by BeginProc for p.syms). *)
    FOR i := 0 TO varMapN - 1 DO
      IF varMap[i].key = v THEN RETURN TRUE END;
    END;
    Variable.Split(v, type, global, indirect, lhs);
    IF indirect THEN
      Abandon("VAR-mode variable not supported in MSIR v0");
      RETURN FALSE;
    END;
    mt := MSIRType.Translate(type);
    IF mt = NIL THEN
      Abandon("unsupported local variable type");
      RETURN FALSE;
    END;
    allocaVal := MSIR.BuildAlloca(
                   curBlock,
                   UniqueLocalName(Value.GlobalName(v, dots := FALSE, with_module := FALSE) & ".slot"),
                   mt);
    IF varMapN >= MaxVarMap THEN
      Abandon("too many variables in proc");
      RETURN FALSE;
    END;
    varMap[varMapN].key      := v;
    varMap[varMapN].val      := allocaVal;
    varMap[varMapN].elemType := mt;
    INC(varMapN);
    RETURN TRUE;
  END AddLocal;

PROCEDURE BindVarAddr(v: Variable.T; addr: MSIR.Value; elemType: MSIR.T) =
  BEGIN
    IF varMapN >= MaxVarMap THEN
      Abandon("too many variables in proc");
      RETURN;
    END;
    varMap[varMapN].key      := v;
    varMap[varMapN].val      := addr;
    varMap[varMapN].elemType := elemType;
    INC(varMapN);
  END BindVarAddr;

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
        pendingContainer := ctx.pending;
        curResultPtr     := ctx.resultPtr;
        curResultType    := ctx.resultType;
        varMapN          := ctx.varMapN;
        exitDepth        := ctx.exitDepth;
        tryDepth         := ctx.tryDepth;
        catchDepth       := ctx.catchDepth;
      cleanupDepth     := ctx.cleanupDepth;
        FOR i := 0 TO varMapN - 1 DO varMap[i] := ctx.varMap[i] END;
      END;
    ELSE
      curProc          := NIL;
      curBlock         := NIL;
      abandoned        := FALSE;
      varMapN          := 0;
      exitDepth        := 0;
      tryDepth         := 0;
      catchDepth       := 0;
      pendingContainer := NIL;
      curResultPtr     := NIL;
      curResultType    := NIL;
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
      abandoned := TRUE;
    END;
  END Abandon;

PROCEDURE InProc(): BOOLEAN =
  BEGIN
    RETURN curProc # NIL AND NOT abandoned;
  END InProc;

PROCEDURE IsAbandoned(): BOOLEAN =
  BEGIN RETURN abandoned END IsAbandoned;

PROCEDURE ClearAbandoned() =
  BEGIN abandoned := FALSE END ClearAbandoned;

PROCEDURE SetPendingContainer(v: MSIR.Value) =
  BEGIN pendingContainer := v END SetPendingContainer;

PROCEDURE TakePendingContainer(): MSIR.Value =
  VAR v := pendingContainer;
  BEGIN pendingContainer := NIL; RETURN v END TakePendingContainer;


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
        curBlock := dead;
      END;
    END;
    RETURN curBlock;
  END CurrentBlock;

PROCEDURE CurrentResultPtr(): MSIR.Value =
  BEGIN RETURN curResultPtr END CurrentResultPtr;

PROCEDURE CurrentResultType(): MSIR.T =
  BEGIN RETURN curResultType END CurrentResultType;

PROCEDURE NewBlock(label: TEXT): MSIR.Block =
  VAR b: MSIR.Block;  uniq: TEXT;
  BEGIN
    INC(blockSeq);
    uniq := label & "." & Fmt.Int(blockSeq);
    b := MSIR.NewBlock(uniq, ARRAY OF MSIR.BlockParam{});
    MSIR.ProcAddBlock(curProc, b);
    RETURN b;
  END NewBlock;

PROCEDURE SetCurrentBlock(b: MSIR.Block) =
  BEGIN
    curBlock := b;
  END SetCurrentBlock;

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

PROCEDURE PushFinallyCleanup(finBody: MSIR.Block;  selector: MSIR.Value) =
  BEGIN
    IF cleanupDepth < MaxCleanup THEN
      cleanupStack[cleanupDepth] := CleanupFrame{kind := CleanupKind.Finally,
                                                 finBody := finBody,
                                                 selector := selector};
      INC(cleanupDepth);
    ELSE
      Abandon("cleanup stack overflow");
    END;
  END PushFinallyCleanup;

PROCEDURE PopFinallyCleanup() =
  BEGIN
    IF cleanupDepth > 0
       AND cleanupStack[cleanupDepth-1].kind = CleanupKind.Finally
      THEN DEC(cleanupDepth)
    END;
  END PopFinallyCleanup;

(* Emit the control transfer for an EXIT: branch to the innermost cleanup frame.
   A Loop frame → branch straight to its exit block (identical to a plain EXIT).
   A Finally frame → store Sel_Exit into its selector and branch to its body;
   the finally's epilogue, after running the finally, calls EmitExitMSIR again
   (its own frame now popped) to continue out to the next finally or the loop. *)
PROCEDURE EmitExitMSIR() =
  BEGIN
    IF cleanupDepth = 0 THEN
      Abandon("EXIT not inside a loop in MSIR");
      RETURN;
    END;
    IF cleanupStack[cleanupDepth-1].kind = CleanupKind.Finally THEN
      (* Route the EXIT through this finally: record that its epilogue must emit
         the Sel_Exit arm, store the selector, and branch to the finally body. *)
      cleanupStack[cleanupDepth-1].exitSeen := TRUE;
      MSIR.BuildStore(CurrentBlock(),
                      MSIR.ConstInt(MSIR.TI(32), Sel_Exit),
                      cleanupStack[cleanupDepth-1].selector);
      MSIR.BuildBr(CurrentBlock(), cleanupStack[cleanupDepth-1].finBody,
                   ARRAY OF MSIR.Value{});
    ELSE
      MSIR.BuildBr(CurrentBlock(), cleanupStack[cleanupDepth-1].exitBlock,
                   ARRAY OF MSIR.Value{});
    END;
  END EmitExitMSIR;

(* Whether an EXIT routed through the innermost (current) finally cleanup frame.
   Called by TryFinStmt — while its Finally frame is still on top — to decide
   whether the finally epilogue needs the Sel_Exit dispatch arm. *)
PROCEDURE CurrentFinallyExitSeen(): BOOLEAN =
  BEGIN
    IF cleanupDepth > 0
       AND cleanupStack[cleanupDepth-1].kind = CleanupKind.Finally
      THEN RETURN cleanupStack[cleanupDepth-1].exitSeen
      ELSE RETURN FALSE
    END;
  END CurrentFinallyExitSeen;

PROCEDURE CurrentExitBlock(): MSIR.Block =
  BEGIN
    IF exitDepth = 0 THEN RETURN NIL END;
    RETURN exitStack[exitDepth - 1];
  END CurrentExitBlock;

PROCEDURE PushTryContext(lpadBlock: MSIR.Block) =
  BEGIN
    IF tryDepth < MaxTryDepth THEN
      tryStack[tryDepth] := lpadBlock;
      INC(tryDepth);
    ELSE
      Abandon("try context stack overflow");
    END;
  END PushTryContext;

PROCEDURE PopTryContext() =
  BEGIN
    IF tryDepth > 0 THEN DEC(tryDepth) END;
  END PopTryContext;

PROCEDURE CurrentUnwindBlock(): MSIR.Block =
  BEGIN
    IF tryDepth = 0 THEN RETURN NIL END;
    RETURN tryStack[tryDepth - 1];
  END CurrentUnwindBlock;

PROCEDURE EmitNestedCall(name: TEXT;  callee: MSIR.Proc;  calleeVal: Value.T;
                          resultPtr: MSIR.Value;
                          READONLY args: ARRAY OF MSIR.Value): MSIR.Value =
  (* Build capture args from the outer proc's varMap, then call.
     Read-only scalar captures are passed by value; others by pointer.
     resultPtr (non-NIL for large-result procs) is placed at arg index 0
     before captures, matching the BeginProc hidden-result-ptr convention. *)
  VAR
    caps    : REF ARRAY OF CaptureAnalysis.Capture;
    nCaps   : INTEGER;
    nHidden : INTEGER;
    allArgs : REF ARRAY OF MSIR.Value;
    v       : Variable.T;
    vt      : Type.T;  vg, vi, vlhs: BOOLEAN;
    mt      : MSIR.T;
  BEGIN
    caps := GetProcCaptures(calleeVal);
    IF caps = NIL THEN nCaps := 0 ELSE nCaps := NUMBER(caps^) END;
    IF resultPtr = NIL THEN nHidden := 0 ELSE nHidden := 1 END;
    allArgs := NEW(REF ARRAY OF MSIR.Value, nHidden + nCaps + NUMBER(args));
    IF resultPtr # NIL THEN allArgs[0] := resultPtr END;
    FOR i := 0 TO nCaps - 1 DO
      v := caps[i].var;
      Variable.Split(v, vt, vg, vi, vlhs);
      mt := MSIRType.Translate(vt);
      IF NOT caps[i].written AND mt # NIL AND IsScalarType(mt) THEN
        allArgs[nHidden + i] := LookupVar(v);   (* pass the current value *)
      ELSE
        allArgs[nHidden + i] := LookupVarAddr(v);  (* pass the alloca address *)
      END;
      IF allArgs[nHidden + i] = NIL THEN
        Abandon("capture var not found in outer proc varMap");
        RETURN NIL;
      END;
    END;
    FOR i := 0 TO NUMBER(args) - 1 DO allArgs[nHidden + nCaps + i] := args[i] END;
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
    nHidden : INTEGER;
    resultT : MSIR.T;
    isLR    : BOOLEAN;
    params  : REF ARRAY OF MSIR.Param;
    shimProc: MSIR.Proc;
    shimBlk : MSIR.Block;
    shimName: TEXT;
    envP    : MSIR.Value;
    capArgs : REF ARRAY OF MSIR.Value;
    allArgs : REF ARRAY OF MSIR.Value;
    f       : Value.T;
    info    : Formal.Info;
    result  : MSIR.Value;
  BEGIN
    FOR i := 0 TO shimMapN - 1 DO
      IF shimMap[i].key = v THEN RETURN shimMap[i].val END;
    END;

    IF caps = NIL THEN nCaps := 0 ELSE nCaps := NUMBER(caps^) END;
    resultT := MSIRType.TranslateResult(ProcType.Result(procType));
    IF resultT = NIL THEN RETURN NIL END;
    isLR := ProcType.LargeResult(ProcType.Result(procType));
    IF isLR THEN resultT := MSIR.TVoid(); nHidden := 1
    ELSE nHidden := 0
    END;

    (* Count explicit formals. *)
    f := ProcType.Formals(procType);
    nFormals := 0;
    WHILE f # NIL DO INC(nFormals); f := f.next END;

    (* params: [hidden_result_ptr?, ptr %__env, explicit_formals…] *)
    params := NEW(REF ARRAY OF MSIR.Param, nHidden + 1 + nFormals);
    IF isLR THEN
      params[0].name := "_result_ptr";
      params[0].type := ptrT;
      params[0].mode := MSIR.ParamMode.ByValue;
    END;
    params[nHidden].name := "__env";
    params[nHidden].type := ptrT;
    params[nHidden].mode := MSIR.ParamMode.ByValue;
    f := ProcType.Formals(procType);
    FOR i := 0 TO nFormals - 1 DO
      Formal.Split(f, info);
      VAR pt := MSIRType.Translate(info.type);
      BEGIN
        IF pt = NIL THEN RETURN NIL END;
        params[nHidden + 1 + i].name := "a." & M3ID.ToText(info.name);
        CASE info.mode OF
        | Formal.Mode.mVALUE =>
            params[nHidden + 1 + i].mode := MSIR.ParamMode.ByValue;
            IF MSIR.Kind(pt) = MSIR.TypeKind.OpenArray THEN
              params[nHidden + 1 + i].type := MSIR.TPtr(pt);
            ELSE
              params[nHidden + 1 + i].type := pt;
            END;
        | Formal.Mode.mVAR =>
            params[nHidden + 1 + i].mode := MSIR.ParamMode.Var;
            params[nHidden + 1 + i].type := MSIR.TPtr(pt);
        | Formal.Mode.mREADONLY =>
            params[nHidden + 1 + i].mode := MSIR.ParamMode.Readonly;
            CASE MSIR.Kind(pt) OF
            | MSIR.TypeKind.Struct,    MSIR.TypeKind.FixedArray,
              MSIR.TypeKind.OpenArray, MSIR.TypeKind.HeapArray,
              MSIR.TypeKind.Object,    MSIR.TypeKind.Set =>
                params[nHidden + 1 + i].type := MSIR.TPtr(pt);
            ELSE
                params[nHidden + 1 + i].type := pt;
            END;
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
        ctx.pending    := pendingContainer;
        ctx.resultPtr  := curResultPtr;
        ctx.resultType := curResultType;
        ctx.varMapN    := varMapN;
        ctx.exitDepth  := exitDepth;
        ctx.tryDepth   := tryDepth;
        ctx.catchDepth := catchDepth;
        ctx.cleanupDepth := cleanupDepth;
        FOR i := 0 TO varMapN - 1 DO ctx.varMap[i] := varMap[i] END;
      END;
      INC(procContextDepth);
    END;

    abandoned      := FALSE;
    varMapN        := 0;
    exitDepth      := 0;
    tryDepth       := 0;
    catchDepth     := 0;
    cleanupDepth   := 0;
    blockSeq       := 0;
    curResultPtr   := NIL;
    curResultType  := NIL;

    shimProc := MSIR.NewProc(shimName, params^, resultT);
    shimBlk  := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
    MSIR.ProcAddBlock(shimProc, shimBlk);
    MSIR.ProcSetLinkage(shimProc, MSIR.Linkage.Internal);
    curProc  := shimProc;
    curBlock := shimBlk;

    IF isLR THEN
      curResultPtr  := MSIR.ProcParam(shimProc, 0);
      curResultType := MSIRType.Translate(ProcType.Result(procType));
    END;

    (* Unpack capture env: env is param[nHidden]. *)
    envP := MSIR.ProcParam(shimProc, nHidden);
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
          capArgs[k] := MSIR.BuildLoad(curBlock, "", mt, ptr);
        ELSE
          capArgs[k] := ptr;
        END;
      END;
    END;

    (* Build allArgs: [hidden_result_ptr?, caps…, explicit_formals…] *)
    allArgs := NEW(REF ARRAY OF MSIR.Value, nHidden + nCaps + nFormals);
    IF isLR THEN allArgs[0] := curResultPtr END;
    FOR k := 0 TO nCaps - 1 DO allArgs[nHidden + k] := capArgs[k] END;
    FOR i := 0 TO nFormals - 1 DO
      allArgs[nHidden + nCaps + i] := MSIR.ProcParam(shimProc, nHidden + 1 + i);
    END;

    result := MSIR.BuildCall(curBlock, "", nested, allArgs^);
    IF isLR THEN
      MSIR.BuildRet(curBlock, NIL);
    ELSE
      MSIR.BuildRet(curBlock, result);
    END;

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
        pendingContainer := ctx.pending;
        curResultPtr     := ctx.resultPtr;
        curResultType    := ctx.resultType;
        varMapN          := ctx.varMapN;
        exitDepth        := ctx.exitDepth;
        tryDepth         := ctx.tryDepth;
        catchDepth       := ctx.catchDepth;
      cleanupDepth     := ctx.cleanupDepth;
        FOR i := 0 TO varMapN - 1 DO varMap[i] := ctx.varMap[i] END;
      END;
    ELSE
      curProc  := NIL;
      curBlock := NIL;
    END;

    IF shimMapN < MaxShimMap THEN
      shimMap[shimMapN].key := v;
      shimMap[shimMapN].val := shimProc;
      INC(shimMapN);
    END;
    RETURN shimProc;
  END GetOrCreateClosureShim;

PROCEDURE BuildClosureValue(v: Value.T; procType: Type.T): MSIR.Value =
  VAR
    ptrT     := MSIR.TPtr(MSIR.TVoid());
    intT     := MSIR.TI(Target.Integer.size);
    IP       := Target.Integer.bytes;
    AP       := Target.Address.bytes;
    clSize   := IP + AP + AP;
    b        : MSIR.Block;
    msirProc : MSIR.Proc;
    caps     : REF ARRAY OF CaptureAnalysis.Capture;
    nCaps    : INTEGER;
    shim     : MSIR.Proc;
    envAlloca: MSIR.Value;
    clAlloca : MSIR.Value;
    markerV  : MSIR.Value;
  BEGIN
    IF NOT InProc() THEN RETURN NIL END;
    msirProc := LookupOrCreateProc(v, procType);
    IF msirProc = NIL THEN RETURN NIL END;
    caps := GetProcCaptures(v);
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
            VAR tmp := MSIR.BuildAlloca(b, "", mt);
                val := LookupVar(capVar);
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
      (* No captures: allocate a 1-byte dummy so envAlloca is a valid ptr. *)
      envAlloca := MSIR.BuildAlloca(b, "", MSIR.TI(8));
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
     MSIRToLLVM does not yet lower block-param values to phi nodes. *)
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
    unwind := CurrentUnwindBlock();
    IF unwind # NIL THEN
      normalB := NewBlock("invoke.cont");
      result  := MSIR.BuildInvoke(b, name, callee, args, normalB, unwind);
      curBlock := normalB;
    ELSE
      result := MSIR.BuildCall(b, name, callee, args);
    END;
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
    b      := CurrentBlock();
    unwind := CurrentUnwindBlock();
    IF unwind # NIL THEN
      normalB := NewBlock("invoke.ind.cont");
      result  := MSIR.BuildInvokeIndirect(b, name, fn, rtype, args, normalB, unwind);
      curBlock := normalB;
    ELSE
      result := MSIR.BuildCallIndirect(b, name, fn, rtype, args);
    END;
    RETURN result;
  END EmitCallIndirect;

PROCEDURE EmitMethodCall(name: TEXT;  obj: MSIR.Value;  midx: INTEGER;
                          rtype: MSIR.T;  resultSlot: MSIR.Value;
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
    largeRes := resultSlot # NIL;
    nExtra  : INTEGER;
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

    (* 4. Build argument list.
       CM3 large-result convention: resultSlot (hidden ptr) is prepended
       before obj (self), matching the CG path: GenResultArg before PassObject.
       Small-result: obj first, then explicit args. *)
    nExtra := 1 + ORD(largeRes);  (* 1 for obj; +1 for resultSlot if large *)
    allArgs := NEW(REF ARRAY OF MSIR.Value, nExtra + nArgs);
    IF largeRes THEN
      allArgs[0] := resultSlot;
      allArgs[1] := obj;
    ELSE
      allArgs[0] := obj;
    END;
    FOR k := 0 TO nArgs - 1 DO allArgs[nExtra + k] := args[k] END;

    (* 5. Indirect call or invoke depending on TRY context.
       Large-result: call with void return (result written through resultSlot). *)
    IF largeRes THEN rtype := NIL END;
    unwind := CurrentUnwindBlock();
    IF unwind # NIL THEN
      normalB := NewBlock("dispatch.cont");
      result  := MSIR.BuildInvokeIndirect(b, name, fn, rtype, allArgs^,
                                            normalB, unwind);
      curBlock := normalB;
    ELSE
      result := MSIR.BuildCallIndirect(b, name, fn, rtype, allArgs^);
    END;
    RETURN result;
  END EmitMethodCall;

PROCEDURE ExcDescValue (v: Value.T): MSIR.Value =
  VAR
    m    := MSIREmit.CurrentModule();
    name := Value.GlobalName(v, dots := FALSE, with_module := TRUE) & "_excptr";
    uid  := M3FP.ToInt(M3FP.FromText(Value.GlobalName(v)));
    desc : MSIR.ExcDesc;
  BEGIN
    IF m = NIL THEN RETURN NIL END;
    (* Check if already registered for this module. *)
    FOR i := 0 TO MSIR.ModuleExcDescCount(m) - 1 DO
      desc := MSIR.ModuleExcDesc(m, i);
      IF Text.Equal(MSIR.ExcDescName(desc), name) THEN
        RETURN MSIR.ExcDescValue(desc);
      END;
    END;
    (* Not found — create and register. *)
    desc := MSIR.NewExcDesc(name, uid);
    MSIR.ModuleAddExcDesc(m, desc);
    RETURN MSIR.ExcDescValue(desc);
  END ExcDescValue;

PROCEDURE CxaStub(name: TEXT;  READONLY params: ARRAY OF MSIR.Param;
                   rtype: MSIR.T): MSIR.Proc =
  (* Return a cached MSIR extern stub for a C++ ABI function. *)
  BEGIN
    FOR i := 0 TO procMapN - 1 DO
      VAR pn := MSIR.ProcName(procMap[i].val); BEGIN
        IF pn # NIL AND Text.Equal(pn, name) THEN
          RETURN procMap[i].val;
        END;
      END;
    END;
    VAR p := MSIR.NewProc(name, params, rtype);
    BEGIN
      IF procMapN < MaxProcMap THEN
        procMap[procMapN].key := NIL;
        procMap[procMapN].val := p;
        INC(procMapN);
      END;
      RETURN p;
    END;
  END CxaStub;

PROCEDURE CxaBeginCatch(): MSIR.Proc =
  VAR params := ARRAY [0..0] OF MSIR.Param{
    MSIR.Param{name := "exc_header", type := MSIR.TPtr(MSIR.TVoid()),
               mode := MSIR.ParamMode.ByValue}};
  BEGIN
    RETURN CxaStub("__cxa_begin_catch", params, MSIR.TPtr(MSIR.TVoid()));
  END CxaBeginCatch;

PROCEDURE CxaEndCatch(): MSIR.Proc =
  BEGIN
    RETURN CxaStub("__cxa_end_catch", ARRAY OF MSIR.Param{}, MSIR.TVoid());
  END CxaEndCatch;

PROCEDURE CxaRethrow(): MSIR.Proc =
  BEGIN
    RETURN CxaStub("__cxa_rethrow", ARRAY OF MSIR.Param{}, MSIR.TVoid());
  END CxaRethrow;

PROCEDURE CxaGetExceptionPtr(): MSIR.Proc =
  VAR params := ARRAY [0..0] OF MSIR.Param{
    MSIR.Param{name := "exc_header", type := MSIR.TPtr(MSIR.TVoid()),
               mode := MSIR.ParamMode.ByValue}};
  BEGIN
    RETURN CxaStub("__cxa_get_exception_ptr", params, MSIR.TPtr(MSIR.TVoid()));
  END CxaGetExceptionPtr;

PROCEDURE PushCatchContext(endCatch: MSIR.Proc) =
  BEGIN
    IF catchDepth < MaxCatchDepth THEN
      catchStack[catchDepth] := endCatch;
      INC(catchDepth);
    ELSE
      Abandon("catch context stack overflow");
    END;
  END PushCatchContext;

PROCEDURE PopCatchContext() =
  BEGIN
    IF catchDepth > 0 THEN DEC(catchDepth) END;
  END PopCatchContext;

PROCEDURE CurrentCatchEndProc(): MSIR.Proc =
  BEGIN
    IF catchDepth = 0 THEN RETURN NIL END;
    RETURN catchStack[catchDepth - 1];
  END CurrentCatchEndProc;

PROCEDURE TypeDescValueForRef(t: Type.T;  dataSize: INTEGER;
                               dataAlignment: INTEGER;
                               isTraced: BOOLEAN): MSIR.Value =
  VAR
    m   := MSIREmit.CurrentModule();
    uid := Type.GlobalUID(t);
    nm  := "tc_ref_" & Fmt.Int(uid);
    desc: MSIR.TypeDesc;
  BEGIN
    IF m = NIL THEN RETURN NIL END;
    FOR i := 0 TO MSIR.ModuleTypeDescCount(m) - 1 DO
      desc := MSIR.ModuleTypeDesc(m, i);
      IF MSIR.TypeDescUID(desc) = uid
         AND MSIR.TypeDescKind(desc) = ORD(M3RT.TypeKind.Ref) THEN
        RETURN MSIR.TypeDescValue(desc);
      END;
    END;
    desc := MSIR.NewTypeDesc(nm, uid, isTraced, ORD(M3RT.TypeKind.Ref),
                              dataSize, dataAlignment);
    VAR tfp := TypeFP.FromType(t);  fpa: ARRAY [0..7] OF [0..255];
    BEGIN
      FOR i := 0 TO 7 DO fpa[i] := tfp.byte[i] END;
      MSIR.SetTypeDescFP(desc, fpa);
    END;
    MSIR.ModuleAddTypeDesc(m, desc);
    RETURN MSIR.TypeDescValue(desc);
  END TypeDescValueForRef;

PROCEDURE TypeDescValueForRefArray(t: Type.T;  dopeSize: INTEGER;
                                    dataAlignment: INTEGER;
                                    nDimensions: INTEGER;
                                    elementSize: INTEGER;
                                    isTraced: BOOLEAN): MSIR.Value =
  VAR
    m    := MSIREmit.CurrentModule();
    uid  := Type.GlobalUID(t);
    nm   := "tc_arr_" & Fmt.Int(uid);
    desc : MSIR.TypeDesc;
  BEGIN
    IF m = NIL THEN RETURN NIL END;
    FOR i := 0 TO MSIR.ModuleTypeDescCount(m) - 1 DO
      desc := MSIR.ModuleTypeDesc(m, i);
      IF MSIR.TypeDescUID(desc) = uid
         AND MSIR.TypeDescKind(desc) = ORD(M3RT.TypeKind.Array) THEN
        RETURN MSIR.TypeDescValue(desc);
      END;
    END;
    desc := MSIR.NewTypeDesc(nm, uid, isTraced, ORD(M3RT.TypeKind.Array),
                              dopeSize, dataAlignment);
    VAR tfp := TypeFP.FromType(t);  fpa: ARRAY [0..7] OF [0..255];
    BEGIN
      FOR i := 0 TO 7 DO fpa[i] := tfp.byte[i] END;
      MSIR.SetTypeDescFP(desc, fpa);
    END;
    MSIR.TypeDescSetArrayInfo(desc, nDimensions, elementSize);
    MSIR.ModuleAddTypeDesc(m, desc);
    RETURN MSIR.TypeDescValue(desc);
  END TypeDescValueForRefArray;

PROCEDURE ObjectTypeCellRef(t: Type.T): MSIR.Value =
  VAR uid := Type.GlobalUID(t);
  BEGIN
    RETURN MSIR.TypeCellRef("tc_obj_" & Fmt.Int(uid));
  END ObjectTypeCellRef;

PROCEDURE ArrayTypeCellRef(t: Type.T): MSIR.Value =
  VAR uid := Type.GlobalUID(t);
  BEGIN
    RETURN MSIR.TypeCellRef("tc_arr_" & Fmt.Int(uid));
  END ArrayTypeCellRef;

PROCEDURE TypeLinkValueForRef(t: Type.T): MSIR.Value =
  VAR m    := MSIREmit.CurrentModule();
      (* Resolve opaque types to their revealed REF type so the TypeLink UID
         matches the TypeCell UID registered by InitTypecellMSIR. *)
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

PROCEDURE RegisterProc(v: Value.T;  p: MSIR.Proc;
                       caps: REF ARRAY OF CaptureAnalysis.Capture := NIL) =
  BEGIN
    IF v = NIL OR p = NIL THEN RETURN END;
    FOR i := 0 TO procMapN - 1 DO
      IF procMap[i].key = v THEN
        procMap[i].val  := p;
        procMap[i].caps := caps;
        RETURN;
      END;
    END;
    IF procMapN >= MaxProcMap THEN RETURN END;
    procMap[procMapN].key  := v;
    procMap[procMapN].val  := p;
    procMap[procMapN].caps := caps;
    INC(procMapN);
  END RegisterProc;

PROCEDURE GetProcCaptures(v: Value.T): REF ARRAY OF CaptureAnalysis.Capture =
  BEGIN
    FOR i := 0 TO procMapN - 1 DO
      IF procMap[i].key = v THEN RETURN procMap[i].caps END;
    END;
    RETURN NIL;
  END GetProcCaptures;

PROCEDURE ProcMapContains(v: Value.T): BOOLEAN =
  BEGIN
    FOR i := 0 TO procMapN - 1 DO
      IF procMap[i].key = v THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END ProcMapContains;

PROCEDURE LookupOrCreateProc(v: Value.T;  procType: Type.T): MSIR.Proc =
  VAR
    f:            Value.T;
    info:         Formal.Info;
    nFormals:     INTEGER := 0;
    resultT:      MSIR.T;
    largeResult:  BOOLEAN;
    nHidden:      INTEGER;
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
    largeResult := ProcType.LargeResult(ProcType.Result(procType));
    IF largeResult THEN resultT := MSIR.TVoid(); nHidden := 1
    ELSE nHidden := 0
    END;
    f := ProcType.Formals(procType);
    WHILE f # NIL DO INC(nFormals);  f := f.next END;
    VAR params := NEW(REF ARRAY OF MSIR.Param, nHidden + nFormals);
    BEGIN
      IF largeResult THEN
        params[0].name := "_result_ptr";
        params[0].type := MSIR.TPtr(MSIR.TVoid());
        params[0].mode := MSIR.ParamMode.ByValue;
      END;
      f := ProcType.Formals(procType);
      FOR i := 0 TO nFormals - 1 DO
        Formal.Split(f, info);
        VAR pt := MSIRType.Translate(info.type);
        BEGIN
          IF pt = NIL THEN
            Abandon("unsupported parameter type in callee");
            RETURN NIL;
          END;
          params[i + nHidden].name := "a." & M3ID.ToText(info.name);
          CASE info.mode OF
          | Formal.Mode.mVALUE =>
              params[i + nHidden].mode := MSIR.ParamMode.ByValue;
              IF MSIR.Kind(pt) = MSIR.TypeKind.OpenArray THEN
                params[i + nHidden].type := MSIR.TPtr(pt);
              ELSE
                params[i + nHidden].type := pt;
              END;
          | Formal.Mode.mVAR      => params[i + nHidden].mode := MSIR.ParamMode.Var;
                                     params[i + nHidden].type := MSIR.TPtr(pt);
          | Formal.Mode.mREADONLY =>
              params[i + nHidden].mode := MSIR.ParamMode.Readonly;
              CASE MSIR.Kind(pt) OF
              | MSIR.TypeKind.Struct,    MSIR.TypeKind.FixedArray,
                MSIR.TypeKind.OpenArray, MSIR.TypeKind.HeapArray,
                MSIR.TypeKind.Object,    MSIR.TypeKind.Set =>
                  params[i + nHidden].type := MSIR.TPtr(pt);
              ELSE
                  params[i + nHidden].type := pt;
              END;
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

PROCEDURE BeginModule() =
  BEGIN
    globalMapN       := 0;
    procMapN         := 0;
    shimMapN         := 0;
    procContextDepth := 0;
    constArrayMapN   := 0;
    constArraySeq    := 0;
    memcpyProc       := NIL;
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

PROCEDURE EmitMemcpyDyn(dst, src, byteCount: MSIR.Value) =
  BEGIN
    IF curBlock = NIL OR abandoned THEN RETURN END;
    EVAL MSIR.BuildCall(curBlock, "", GetMemcpyProc(),
      ARRAY OF MSIR.Value{dst, src, byteCount});
  END EmitMemcpyDyn;

PROCEDURE OpenArrayToFixedStore (lhsPtr, rhsVal: MSIR.Value;
                                  lhsType: Type.T): BOOLEAN =
  VAR
    slotT  := MSIR.ValueType (lhsPtr);
    eltT   := MSIR.EltType (slotT);
    rhsT   := MSIR.ValueType (rhsVal);
    zero   : MSIR.Value;
    srcPtr : MSIR.Value;
    info   : Type.Info;
  BEGIN
    IF MSIR.Kind (eltT) # MSIR.TypeKind.FixedArray OR
       MSIR.Kind (rhsT) # MSIR.TypeKind.OpenArray THEN
      RETURN FALSE;
    END;
    IF curBlock = NIL OR abandoned THEN RETURN TRUE END;
    zero   := MSIR.ConstInt (MSIR.TI (Target.Integer.size), 0);
    srcPtr := MSIR.BuildOpenArrayElemAddr (curBlock, "", rhsVal,
                ARRAY OF MSIR.Value {zero});
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

PROCEDURE MaterializeConstArray(m3Val: Value.T; constExpr: Expr.T): MSIR.Value =
  VAR
    ae:       ArrayExpr.T;
    n:        INTEGER;
    indexT, eltT: Type.T;
    eltMsir:  MSIR.T;
    elts:     REF ARRAY OF MSIR.Value;
    ca:       MSIR.ConstArray;
    name:     TEXT;
    v:        MSIR.Value;
    m:        MSIR.Module;
  BEGIN
    IF NOT MSIREmit.IsEnabled() THEN RETURN NIL END;
    (* De-dup: return cached value for this CONST if already materialized. *)
    FOR i := 0 TO constArrayMapN - 1 DO
      IF constArrayMap[i].key = m3Val THEN RETURN constArrayMap[i].val END;
    END;
    ae := ArrayExpr.ArrayConstrExpr(constExpr);
    IF ae = NIL THEN
      Abandon("ConstArray: not an array constructor");  RETURN NIL;
    END;
    (* EltCount returns only the number of explicit elements in the constructor
       (e.g. 2 for Int{10, 0, ..}).  The total array size comes from the type.
       Use the type-derived count to handle '..' (repeat-last) constructors. *)
    VAR nExplicit := ArrayExpr.EltCount(ae);  nTotal: INTEGER;
    BEGIN
      IF NOT ArrayType.Split(Expr.TypeOf(constExpr), indexT, eltT) THEN
        Abandon("ConstArray: not an array type");  RETURN NIL;
      END;
      IF NOT TInt.ToInt(Type.Number(indexT), nTotal) OR nTotal <= 0 THEN
        nTotal := nExplicit;
      END;
      n := nTotal;
    END;
    eltMsir := MSIRType.Translate(eltT);
    IF eltMsir = NIL THEN
      Abandon("ConstArray: unsupported element type");  RETURN NIL;
    END;
    elts := NEW(REF ARRAY OF MSIR.Value, n);
    FOR i := 0 TO n - 1 DO
      (* For elements beyond the explicit args, repeat the last explicit value
         ('..' constructor).  MIN ensures we never go past the args array. *)
      VAR eltIdx := MIN(i, ArrayExpr.EltCount(ae) - 1);
          elt := ArrayExpr.Elt(ae, eltIdx);  cv: MSIR.Value;
      BEGIN
        (* Try to build a compile-time constant struct (avoids emitting function-local
           alloca/store/load sequences that are invalid in global constant initializers). *)
        IF RecordExpr.TryCompileConstMSIR(elt, cv) THEN
          elts[i] := cv;
        ELSE
          elts[i] := Expr.CompileMSIR(elt);
          IF elts[i] = NIL THEN RETURN NIL END;  (* e.g. sub-byte packed element *)
          (* Non-constant values (function-local alloca/load results) cannot
             appear in a global constant array initializer — abandon and let
             the caller fall back to CG. *)
          CASE MSIR.GetValueKind(elts[i]) OF
          | MSIR.ValueKind.ConstInt, MSIR.ValueKind.ConstFloat,
            MSIR.ValueKind.ConstNil, MSIR.ValueKind.ConstProc,
            MSIR.ValueKind.ConstTextLit, MSIR.ValueKind.ConstStruct,
            MSIR.ValueKind.GlobalRef, MSIR.ValueKind.StructFieldRef => (* OK *)
          ELSE
            Abandon("ConstArray: element has non-constant value — cannot emit global");
            RETURN NIL;
          END;
        END;
        (* Coerce integer constants to the declared element type.
           E.g. IntegerExpr produces TI(64) for literal 10 even when the array
           element type is IByte = TI(8).  Re-create as a same-value ConstInt
           with the target element type so the LLVM array initializer is typed
           consistently.  Only safe for ConstInt values (other constant kinds
           already carry the correct type). *)
        IF MSIR.GetValueKind(elts[i]) = MSIR.ValueKind.ConstInt AND
           NOT MSIR.Equal(MSIR.ValueType(elts[i]), eltMsir) THEN
          elts[i] := MSIR.ConstInt(eltMsir, MSIR.GetIntVal(elts[i]));
        END;
      END;
    END;
    m    := MSIREmit.CurrentModule();
    name := "constarray_" & Fmt.Int(constArraySeq);  INC(constArraySeq);
    ca   := MSIR.NewConstArray(name, eltMsir, elts^);
    MSIR.ModuleAddConstArray(m, ca);
    v    := MSIR.ConstArrayValue(ca);
    IF constArrayMapN < MaxConstArrayMap THEN
      constArrayMap[constArrayMapN].key := m3Val;
      constArrayMap[constArrayMapN].val := v;
      INC(constArrayMapN);
    END;
    RETURN v;
  END MaterializeConstArray;

(* ---- raw map-management helpers called from Variable.m3 ---- *)

PROCEDURE GlobalMapAdd(v: Variable.T;  g: MSIR.Global;  m: MSIR.Module) =
  BEGIN
    FOR i := 0 TO globalMapN-1 DO
      IF globalMap[i].key = v THEN RETURN END;
    END;
    IF globalMapN >= MaxGlobalMap THEN RETURN END;
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
                              varMSIRType: MSIR.T) =
  (* Register an imported (non-external) M3 variable as an import-chain entry.
     At code-generation time, LookupVar/LookupVarAddr loads the import pointer
     from @<curMod>_M3_imp.k and GEPs to varByteOff to reach the variable. *)
  BEGIN
    FOR i := 0 TO globalMapN - 1 DO
      IF globalMap[i].key = v THEN RETURN END;
    END;
    IF globalMapN >= MaxGlobalMap THEN RETURN END;
    (* No MSIR.ModuleAddGlobal call — import chain entries have no standalone global. *)
    globalMap[globalMapN].key         := v;
    globalMap[globalMapN].val         := NIL;
    globalMap[globalMapN].needsLoad   := FALSE;
    globalMap[globalMapN].dataType    := NIL;
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
    IF globalMapN >= MaxGlobalMap THEN RETURN END;
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

PROCEDURE VarMapAdd(v: Variable.T;  val: MSIR.Value;  elt: MSIR.T) =
  BEGIN
    IF varMapN >= MaxVarMap THEN
      Abandon("VarMapAdd: overflow — increase MaxVarMap");
      RETURN;
    END;
    varMap[varMapN].key      := v;
    varMap[varMapN].val      := val;
    varMap[varMapN].elemType := elt;
    INC(varMapN);
  END VarMapAdd;

PROCEDURE VarMapContains(v: Variable.T): BOOLEAN =
  BEGIN
    FOR i := 0 TO varMapN - 1 DO
      IF varMap[i].key = v THEN RETURN TRUE END;
    END;
    RETURN FALSE;
  END VarMapContains;

PROCEDURE BeginModuleInit(name: TEXT): BOOLEAN =
  VAR resultT: MSIR.T;
  BEGIN
    IF NOT MSIREmit.IsEnabled() THEN RETURN FALSE END;
    <* ASSERT curProc = NIL *>
    abandoned := FALSE;
    varMapN   := 0;
    exitDepth := 0;
    tryDepth  := 0;
    catchDepth := 0;
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
    ELSE
      (* Field spans two bytes: stitch b0 | (b1 << 8) as i16 *)
      VAR p1  := MSIR.BuildPtrAdd (b, "", base, byteStart + 1);
          b1  := MSIR.BuildLoad (b, "", MSIR.TI (8), p1);
          b0w := MSIR.BuildZExt (b, "", b0, MSIR.TI (16));
          b1w := MSIR.BuildZExt (b, "", b1, MSIR.TI (16));
      BEGIN
        word := MSIR.BuildIOr (b, "", b0w,
                    MSIR.BuildIShl (b, "", b1w, MSIR.ConstInt (MSIR.TI (16), 8)));
      END;
      wordBits := 16;
    END;
    VAR
      wordT    := MSIR.TI (wordBits);
      shifted  := MSIR.BuildILShr (b, "", word,
                      MSIR.ConstInt (wordT, bitInByte));
      maskVal  : INTEGER := 1;
      extracted: MSIR.Value;
    BEGIN
      FOR i := 1 TO bitWidth DO maskVal := maskVal * 2 END;
      maskVal := maskVal - 1;
      extracted := MSIR.BuildIAnd (b, "", shifted, MSIR.ConstInt (wordT, maskVal));
      VAR
        packedBase : Type.T;
        packedSize : INTEGER;
        naturalT   : MSIR.T;
        lo, hi     : Target.Int;
        doSExt     : BOOLEAN;
      BEGIN
        PackedType.Split (rawFieldType, packedSize, packedBase);
        naturalT := MSIRType.Translate (packedBase);
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
                               idx: MSIR.Value;  rawEltType: Type.T): MSIR.Value =
  VAR b    := curBlock;
      intT := MSIR.TI (Target.Integer.size);
      i8T  := MSIR.TI (8);
  BEGIN
    IF 8 MOD eltPack # 0 THEN RETURN NIL END;  (* non-power-of-2 eltPack: not yet supported *)
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
            naturalT := MSIRType.Translate (packedBase);
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
                              idx: MSIR.Value;  rhs: MSIR.Value) =
  VAR b    := curBlock;
      intT := MSIR.TI (Target.Integer.size);
      i8T  := MSIR.TI (8);
  BEGIN
    IF 8 MOD eltPack # 0 THEN RETURN END;  (* non-power-of-2 eltPack: not yet supported *)
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

PROCEDURE GenLocation() =
  VAR f: TEXT;  l: INTEGER;
  BEGIN
    IF NOT InProc() THEN RETURN END;
    Scanner.Here(f, l);
    IF l > 0 THEN MSIR.SetCurrentSrcLine(l) END;
  END GenLocation;

BEGIN
END MSIRBuilder.
