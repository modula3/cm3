MODULE MSIRBuilder;

IMPORT MSIR, MSIRType, MSIREmit;
IMPORT M3ID, Type, Value, Formal, Variable, Scope, ProcType, Fmt, Target, Text;
IMPORT RunTyme, Procedure, M3FP;

CONST MaxVarMap    = 64;
CONST MaxExitStack = 16;
CONST MaxTryDepth  = 16;
CONST MaxCatchDepth = 16;
CONST MaxProcMap   = 128;
CONST MaxGlobalMap = 256;

(* Each formal maps to a Param SSA value (elemType = NIL).
   Each local maps to an alloca ptr (elemType = the allocated type). *)
TYPE VarEntry = RECORD
  key:      Variable.T;
  val:      MSIR.Value;
  elemType: MSIR.T;       (* NIL => formal; non-NIL => local alloca ptr *)
END;

TYPE ProcEntry   = RECORD key: Value.T;    val: MSIR.Proc   END;
TYPE GlobalEntry = RECORD key: Variable.T; val: MSIR.Global END;

VAR
  curProc:   MSIR.Proc  := NIL;
  curBlock:  MSIR.Block := NIL;
  abandoned: BOOLEAN    := FALSE;
  blockSeq:  INTEGER    := 0;   (* per-proc block label counter *)

  varMap:  ARRAY [0..MaxVarMap-1]  OF VarEntry;
  varMapN: INTEGER := 0;

  exitStack: ARRAY [0..MaxExitStack-1] OF MSIR.Block;
  exitDepth: INTEGER := 0;

  tryStack:  ARRAY [0..MaxTryDepth-1] OF MSIR.Block;
  tryDepth:  INTEGER := 0;

  catchStack: ARRAY [0..MaxCatchDepth-1] OF MSIR.Proc;  (* endCatch procs *)
  catchDepth: INTEGER := 0;

  procMap:  ARRAY [0..MaxProcMap-1] OF ProcEntry;
  procMapN: INTEGER := 0;

  globalMap:  ARRAY [0..MaxGlobalMap-1] OF GlobalEntry;
  globalMapN: INTEGER := 0;

PROCEDURE BeginProc(name: M3ID.T;
                    formals: Value.T;
                    <*UNUSED*> syms: Scope.T;
                    result: Type.T;
                    isExternal: BOOLEAN): BOOLEAN =
  VAR
    info:     Formal.Info;
    nFormals: INTEGER := 0;
    f:        Value.T;
    resultT:  MSIR.T;
  BEGIN
    IF NOT MSIREmit.IsEnabled() THEN RETURN FALSE END;
    <* ASSERT curProc = NIL *>
    abandoned := FALSE;
    varMapN   := 0;
    exitDepth := 0;
    tryDepth  := 0;
    catchDepth := 0;
    blockSeq  := 0;

    resultT := MSIRType.TranslateResult(result);
    IF resultT = NIL THEN
      MSIREmit.NoteSkipped(M3ID.ToText(name), "unsupported result type");
      RETURN FALSE;
    END;

    f := formals;
    WHILE f # NIL DO INC(nFormals); f := f.next END;

    VAR params := NEW(REF ARRAY OF MSIR.Param, nFormals);
    BEGIN
      f := formals;
      FOR i := 0 TO nFormals - 1 DO
        Formal.Split(f, info);
        VAR pt := MSIRType.Translate(info.type);
        BEGIN
          IF pt = NIL THEN
            MSIREmit.NoteSkipped(M3ID.ToText(name), "unsupported formal type");
            RETURN FALSE;
          END;
          params[i].name := M3ID.ToText(info.name);
          CASE info.mode OF
          | Formal.Mode.mVALUE =>
              params[i].mode := MSIR.ParamMode.ByValue;
              IF MSIR.Kind(pt) = MSIR.TypeKind.OpenArray THEN
                (* Open arrays are always passed indirectly even for VALUE mode:
                   the caller copies the data; the formal is a fat-pointer to
                   the copy.  Use TPtr so the binding path treats it as
                   indirect (vIndirect = TRUE for open array locals). *)
                params[i].type := MSIR.TPtr(pt);
              ELSE
                params[i].type := pt;
              END;
          | Formal.Mode.mVAR      => params[i].mode := MSIR.ParamMode.Var;
                                     params[i].type := MSIR.TPtr(pt);
          | Formal.Mode.mREADONLY =>
              params[i].mode := MSIR.ParamMode.Readonly;
              CASE MSIR.Kind(pt) OF
              | MSIR.TypeKind.Struct,    MSIR.TypeKind.FixedArray,
                MSIR.TypeKind.OpenArray, MSIR.TypeKind.HeapArray,
                MSIR.TypeKind.Object,    MSIR.TypeKind.Set =>
                  params[i].type := MSIR.TPtr(pt);
              ELSE
                  params[i].type := pt;
              END;
          END;
        END;
        f := f.next;
      END;

      curProc  := MSIR.NewProc(M3ID.ToText(name), params^, resultT);
      curBlock := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
      MSIR.ProcAddBlock(curProc, curBlock);
      IF NOT isExternal THEN
        MSIR.ProcSetLinkage(curProc, MSIR.Linkage.Internal);
      END;

      (* Bind formals and locals upfront — independent of CG declare timing. *)
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
        gv := MSIR.GlobalValue(globalMap[i].val);
        IF MSIR.Kind(MSIR.ValueType(gv)) = MSIR.TypeKind.GcSlot THEN
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
        RETURN MSIR.GlobalValue(globalMap[i].val);
      END;
    END;
    RETURN NIL;
  END LookupVarAddr;

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
                   Value.GlobalName(v, dots := FALSE, with_module := FALSE),
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
    END;
    curProc   := NIL;
    curBlock  := NIL;
    abandoned := FALSE;
    varMapN   := 0;
    exitDepth := 0;
    tryDepth  := 0;
    catchDepth := 0;
  END EndProc;

PROCEDURE Abandon(reason: TEXT) =
  BEGIN
    IF NOT abandoned AND curProc # NIL THEN
      MSIREmit.NoteSkipped(MSIR.ProcName(curProc), "abandon: " & reason);
    END;
    abandoned := TRUE;
  END Abandon;

PROCEDURE InProc(): BOOLEAN =
  BEGIN
    RETURN curProc # NIL AND NOT abandoned;
  END InProc;

PROCEDURE CurrentProc(): MSIR.Proc =
  BEGIN RETURN curProc END CurrentProc;

PROCEDURE CurrentBlock(): MSIR.Block =
  BEGIN RETURN curBlock END CurrentBlock;

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
  END PushExitBlock;

PROCEDURE PopExitBlock() =
  BEGIN
    IF exitDepth > 0 THEN DEC(exitDepth) END;
  END PopExitBlock;

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

PROCEDURE EmitCall(name: TEXT;  callee: MSIR.Proc;
                   READONLY args: ARRAY OF MSIR.Value): MSIR.Value =
  VAR
    unwind:  MSIR.Block;
    normalB: MSIR.Block;
    result:  MSIR.Value;
  BEGIN
    unwind := CurrentUnwindBlock();
    IF unwind # NIL THEN
      normalB := NewBlock("invoke.cont");
      result  := MSIR.BuildInvoke(curBlock, name, callee, args, normalB, unwind);
      curBlock := normalB;
    ELSE
      result := MSIR.BuildCall(curBlock, name, callee, args);
    END;
    RETURN result;
  END EmitCall;

PROCEDURE EmitMethodCall(name: TEXT;  obj: MSIR.Value;  midx: LONGINT;
                          rtype: MSIR.T;
                          READONLY args: ARRAY OF MSIR.Value): MSIR.Value =
  VAR
    ptrT    := MSIR.TPtr(MSIR.TVoid());
    suite   : MSIR.Value;
    slotPtr : MSIR.Value;
    fn      : MSIR.Value;
    allArgs : REF ARRAY OF MSIR.Value;
    nArgs   := NUMBER(args);
    unwind  : MSIR.Block;
    normalB : MSIR.Block;
    result  : MSIR.Value;
  BEGIN
    (* 1. Load vtable pointer (first word of object). *)
    suite := MSIR.BuildLoad(curBlock, "", ptrT, obj);

    (* 2. Advance to the method slot (idx * sizeof(ptr) bytes). *)
    (* Vtable slot N is at byte offset N * Target.Address.bytes. *)
    IF midx = 0L THEN
      slotPtr := suite;
    ELSE
      slotPtr := MSIR.BuildPtrAdd(curBlock, "",
                                  suite,
                                  midx * VAL(Target.Address.bytes, LONGINT));
    END;

    (* 3. Load function pointer from the slot. *)
    fn := MSIR.BuildLoad(curBlock, "", ptrT, slotPtr);

    (* 4. Build argument list: obj (implicit self) first, then explicit args. *)
    allArgs := NEW(REF ARRAY OF MSIR.Value, 1 + nArgs);
    allArgs[0] := obj;
    FOR k := 0 TO nArgs - 1 DO allArgs[1 + k] := args[k] END;

    (* 5. Indirect call or invoke depending on TRY context. *)
    unwind := CurrentUnwindBlock();
    IF unwind # NIL THEN
      normalB := NewBlock("dispatch.cont");
      result  := MSIR.BuildInvokeIndirect(curBlock, name, fn, rtype, allArgs^,
                                            normalB, unwind);
      curBlock := normalB;
    ELSE
      result := MSIR.BuildCallIndirect(curBlock, name, fn, rtype, allArgs^);
    END;
    RETURN result;
  END EmitMethodCall;

PROCEDURE ExcDescValue (v: Value.T): MSIR.Value =
  VAR
    m    := MSIREmit.CurrentModule();
    name := Value.GlobalName(v, dots := FALSE, with_module := TRUE) & "_excptr";
    uid  := VAL(M3FP.ToInt(M3FP.FromText(Value.GlobalName(v))), LONGINT);
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
      IF Text.Equal(MSIR.ProcName(procMap[i].val), name) THEN
        RETURN procMap[i].val;
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
    uid := VAL(Type.GlobalUID(t), LONGINT);
    nm  := "tc_ref_" & Fmt.LongInt(uid);
    desc: MSIR.TypeDesc;
  BEGIN
    IF m = NIL THEN RETURN NIL END;
    FOR i := 0 TO MSIR.ModuleTypeDescCount(m) - 1 DO
      desc := MSIR.ModuleTypeDesc(m, i);
      IF MSIR.TypeDescUID(desc) = uid AND MSIR.TypeDescKind(desc) = 6 THEN
        RETURN MSIR.TypeDescValue(desc);
      END;
    END;
    desc := MSIR.NewTypeDesc(nm, uid, isTraced, 6 (* Ref *),
                              dataSize, dataAlignment);
    MSIR.ModuleAddTypeDesc(m, desc);
    RETURN MSIR.TypeDescValue(desc);
  END TypeDescValueForRef;

PROCEDURE ObjectTypeCellRef(t: Type.T): MSIR.Value =
  VAR uid := VAL(Type.GlobalUID(t), LONGINT);
  BEGIN
    RETURN MSIR.TypeCellRef("tc_obj_" & Fmt.LongInt(uid));
  END ObjectTypeCellRef;

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
    IF procMapN >= MaxProcMap THEN RETURN END;
    procMap[procMapN].key := v;
    procMap[procMapN].val := p;
    INC(procMapN);
  END RegisterProc;

PROCEDURE LookupOrCreateProc(v: Value.T;  procType: Type.T): MSIR.Proc =
  VAR
    f:        Value.T;
    info:     Formal.Info;
    nFormals: INTEGER := 0;
    resultT:  MSIR.T;
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
          params[i].name := M3ID.ToText(info.name);
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
              params[i].mode := MSIR.ParamMode.Readonly;
              CASE MSIR.Kind(pt) OF
              | MSIR.TypeKind.Struct,    MSIR.TypeKind.FixedArray,
                MSIR.TypeKind.OpenArray, MSIR.TypeKind.HeapArray,
                MSIR.TypeKind.Object,    MSIR.TypeKind.Set =>
                  params[i].type := MSIR.TPtr(pt);
              ELSE
                  params[i].type := pt;
              END;
          END;
        END;
        f := f.next;
      END;
      VAR stub := MSIR.NewProc(Value.GlobalName(v), params^, resultT);
      BEGIN
        RegisterProc(v, stub);
        RETURN stub;
      END;
    END;
  END LookupOrCreateProc;

PROCEDURE BeginModule() =
  BEGIN
    globalMapN := 0;
    procMapN   := 0;
  END BeginModule;

(* ---- raw map-management helpers called from Variable.m3 ---- *)

PROCEDURE GlobalMapAdd(v: Variable.T;  g: MSIR.Global;  m: MSIR.Module) =
  BEGIN
    IF globalMapN >= MaxGlobalMap THEN RETURN END;
    MSIR.ModuleAddGlobal(m, g);
    globalMap[globalMapN].key := v;
    globalMap[globalMapN].val := g;
    INC(globalMapN);
  END GlobalMapAdd;

PROCEDURE VarMapAdd(v: Variable.T;  val: MSIR.Value;  elt: MSIR.T) =
  BEGIN
    IF varMapN >= MaxVarMap THEN RETURN END;
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

BEGIN
END MSIRBuilder.
