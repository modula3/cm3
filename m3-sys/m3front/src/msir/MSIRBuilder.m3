MODULE MSIRBuilder;

IMPORT MSIR, MSIRType, MSIREmit;
IMPORT M3ID, Type, Value, Formal, Variable, Scope, ProcType, Fmt, Target, Text;
IMPORT RunTyme, Procedure, M3FP, CaptureAnalysis, M3RT;
IMPORT Expr, ArrayExpr, ArrayType;

CONST MaxVarMap    = 64;
CONST MaxExitStack = 16;
CONST MaxTryDepth  = 16;
CONST MaxCatchDepth = 16;
CONST MaxProcMap   = 128;
CONST MaxGlobalMap  = 256;
CONST MaxNestDepth  = 16;  (* maximum nesting depth for nested procs *)

(* Each formal maps to a Param SSA value (elemType = NIL).
   Each local maps to an alloca ptr (elemType = the allocated type). *)
TYPE VarEntry = RECORD
  key:      Variable.T;
  val:      MSIR.Value;
  elemType: MSIR.T;       (* NIL => formal; non-NIL => local alloca ptr *)
END;

(* Saved state for one level of proc context (for nested proc compilation). *)
TYPE ProcContext = RECORD
  proc:          MSIR.Proc;
  block:         MSIR.Block;
  abandoned:     BOOLEAN;
  blockSeq:      INTEGER;
  pending:       MSIR.Value;   (* pendingContainer *)
  varMapN:       INTEGER;
  varMap:        ARRAY [0..MaxVarMap-1] OF VarEntry;
  exitDepth:     INTEGER;
  tryDepth:      INTEGER;
  catchDepth:    INTEGER;
END;

TYPE ProcEntry   = RECORD
  key:  Value.T;
  val:  MSIR.Proc;
  caps: REF ARRAY OF CaptureAnalysis.Capture;  (* NIL for non-nested procs *)
END;
TYPE GlobalEntry = RECORD key: Variable.T; val: MSIR.Global END;

VAR
  curProc:          MSIR.Proc  := NIL;
  curBlock:         MSIR.Block := NIL;
  abandoned:        BOOLEAN    := FALSE;
  blockSeq:         INTEGER    := 0;
  pendingContainer: MSIR.Value := NIL;

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

  procMap:  ARRAY [0..MaxProcMap-1] OF ProcEntry;
  procMapN: INTEGER := 0;

  globalMap:  ARRAY [0..MaxGlobalMap-1] OF GlobalEntry;
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
      MSIR.TypeKind.Ptr  => RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END IsScalarType;

PROCEDURE BeginProc(name: TEXT;
                    formals: Value.T;
                    syms: Scope.T;
                    result: Type.T;
                    isExternal: BOOLEAN;
                    captures: CaptureAnalysis.T := NIL): BOOLEAN =
  VAR
    info:      Formal.Info;
    nFormals:  INTEGER := 0;
    nCaptures: INTEGER := 0;
    f:         Value.T;
    resultT:   MSIR.T;
    isNested:  BOOLEAN;
    pBase:     INTEGER;  (* param index offset = nCaptures *)
    caps:      REF ARRAY OF CaptureAnalysis.Capture;
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
        ctx.varMapN    := varMapN;
        ctx.exitDepth  := exitDepth;
        ctx.tryDepth   := tryDepth;
        ctx.catchDepth := catchDepth;
        FOR i := 0 TO varMapN - 1 DO ctx.varMap[i] := varMap[i] END;
      END;
      INC(procContextDepth);
    END;
    abandoned  := FALSE;
    varMapN    := 0;
    exitDepth  := 0;
    tryDepth   := 0;
    catchDepth := 0;
    blockSeq   := 0;

    resultT := MSIRType.TranslateResult(result);
    IF resultT = NIL THEN
      MSIREmit.NoteSkipped(name, "unsupported result type");
      RETURN FALSE;
    END;

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
         Written or aggregate captures pass by opaque ptr. *)
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
            params[i].type := mt;          (* pass the value directly *)
          ELSE
            params[i].type := MSIR.TPtr(MSIR.TVoid());  (* pass ptr *)
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
            RETURN FALSE;
          END;
          params[i + pBase].name := M3ID.ToText(info.name);
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

      curProc  := MSIR.NewProc(name, params^, resultT);
      curBlock := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
      MSIR.ProcAddBlock(curProc, curBlock);
      IF NOT isExternal THEN
        MSIR.ProcSetLinkage(curProc, MSIR.Linkage.Internal);
      END;

      (* For nested procs: bind capture params in the inner proc's varMap.
         Read-only scalar captures: param holds the value directly (elemType=NIL).
         Written or aggregate captures: param holds a ptr; loads go through it. *)
      IF isNested AND nCaptures > 0 THEN
        FOR i := 0 TO nCaptures - 1 DO
          VAR v:  Variable.T := caps[i].var;
              vt: Type.T;  vg, vi, vlhs: BOOLEAN;
              mt: MSIR.T;
          BEGIN
            Variable.Split(v, vt, vg, vi, vlhs);
            mt := MSIRType.Translate(vt);
            IF mt # NIL AND varMapN < MaxVarMap THEN
              varMap[varMapN].key := v;
              varMap[varMapN].val := MSIR.ProcParam(curProc, i);
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
    (* Pop saved outer proc context if we're returning from a nested proc. *)
    IF procContextDepth > 0 THEN
      DEC(procContextDepth);
      WITH ctx = procContextStack[procContextDepth] DO
        curProc          := ctx.proc;
        curBlock         := ctx.block;
        abandoned        := ctx.abandoned;
        blockSeq         := ctx.blockSeq;
        pendingContainer := ctx.pending;
        varMapN          := ctx.varMapN;
        exitDepth        := ctx.exitDepth;
        tryDepth         := ctx.tryDepth;
        catchDepth       := ctx.catchDepth;
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
      VAR dead := MSIR.NewBlock("dead", ARRAY OF MSIR.BlockParam{});
      BEGIN
        MSIR.ProcAddBlock(curProc, dead);
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

PROCEDURE EmitNestedCall(name: TEXT;  callee: MSIR.Proc;  calleeVal: Value.T;
                          READONLY args: ARRAY OF MSIR.Value): MSIR.Value =
  (* Build capture args from the outer proc's varMap, then call.
     Read-only scalar captures are passed by value; others by pointer. *)
  VAR
    caps    : REF ARRAY OF CaptureAnalysis.Capture;
    nCaps   : INTEGER;
    allArgs : REF ARRAY OF MSIR.Value;
    v       : Variable.T;
    vt      : Type.T;  vg, vi, vlhs: BOOLEAN;
    mt      : MSIR.T;
  BEGIN
    caps := GetProcCaptures(calleeVal);
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

PROCEDURE EmitMethodCall(name: TEXT;  obj: MSIR.Value;  midx: LONGINT;
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

    (* 1. Load vtable pointer (first word of object). *)
    suite := MSIR.BuildLoad(b, "", ptrT, obj);

    (* 2. Advance to the method slot (idx * sizeof(ptr) bytes). *)
    (* Vtable slot N is at byte offset N * Target.Address.bytes. *)
    IF midx = 0L THEN
      slotPtr := suite;
    ELSE
      slotPtr := MSIR.BuildPtrAdd(b, "",
                                  suite,
                                  midx * VAL(Target.Address.bytes, LONGINT));
    END;

    (* 3. Load function pointer from the slot. *)
    fn := MSIR.BuildLoad(b, "", ptrT, slotPtr);

    (* 4. Build argument list: obj (implicit self) first, then explicit args. *)
    allArgs := NEW(REF ARRAY OF MSIR.Value, 1 + nArgs);
    allArgs[0] := obj;
    FOR k := 0 TO nArgs - 1 DO allArgs[1 + k] := args[k] END;

    (* 5. Indirect call or invoke depending on TRY context. *)
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
      IF MSIR.TypeDescUID(desc) = uid
         AND MSIR.TypeDescKind(desc) = ORD(M3RT.TypeKind.Ref) THEN
        RETURN MSIR.TypeDescValue(desc);
      END;
    END;
    desc := MSIR.NewTypeDesc(nm, uid, isTraced, ORD(M3RT.TypeKind.Ref),
                              dataSize, dataAlignment);
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
    uid  := VAL(Type.GlobalUID(t), LONGINT);
    nm   := "tc_arr_" & Fmt.LongInt(uid);
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
    MSIR.TypeDescSetArrayInfo(desc, nDimensions, elementSize);
    MSIR.ModuleAddTypeDesc(m, desc);
    RETURN MSIR.TypeDescValue(desc);
  END TypeDescValueForRefArray;

PROCEDURE ObjectTypeCellRef(t: Type.T): MSIR.Value =
  VAR uid := VAL(Type.GlobalUID(t), LONGINT);
  BEGIN
    RETURN MSIR.TypeCellRef("tc_obj_" & Fmt.LongInt(uid));
  END ObjectTypeCellRef;

PROCEDURE ArrayTypeCellRef(t: Type.T): MSIR.Value =
  VAR uid := VAL(Type.GlobalUID(t), LONGINT);
  BEGIN
    RETURN MSIR.TypeCellRef("tc_arr_" & Fmt.LongInt(uid));
  END ArrayTypeCellRef;

PROCEDURE TypeLinkValueForRef(t: Type.T): MSIR.Value =
  VAR m    := MSIREmit.CurrentModule();
      uid  := VAL(Type.GlobalUID(t), LONGINT);
      nm   := "tl_ref_" & Fmt.LongInt(uid);
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
  END TypeLinkValueForRef;

PROCEDURE TypeLinkValueForRefArray(t: Type.T): MSIR.Value =
  VAR m    := MSIREmit.CurrentModule();
      uid  := VAL(Type.GlobalUID(t), LONGINT);
      nm   := "tl_arr_" & Fmt.LongInt(uid);
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
      uid  := VAL(Type.GlobalUID(t), LONGINT);
      nm   := "tl_obj_" & Fmt.LongInt(uid);
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
    globalMapN       := 0;
    procMapN         := 0;
    procContextDepth := 0;
    constArrayMapN   := 0;
    constArraySeq    := 0;
    memcpyProc       := NIL;
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
        MSIR.ConstInt(MSIR.TI(Target.Integer.size), VAL(byteCount, LONGINT))});
  END EmitMemcpy;

PROCEDURE EmitMemcpyDyn(dst, src, byteCount: MSIR.Value) =
  BEGIN
    IF curBlock = NIL OR abandoned THEN RETURN END;
    EVAL MSIR.BuildCall(curBlock, "", GetMemcpyProc(),
      ARRAY OF MSIR.Value{dst, src, byteCount});
  END EmitMemcpyDyn;

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
    n := ArrayExpr.EltCount(ae);
    IF NOT ArrayType.Split(Expr.TypeOf(constExpr), indexT, eltT) THEN
      Abandon("ConstArray: not an array type");  RETURN NIL;
    END;
    eltMsir := MSIRType.Translate(eltT);
    IF eltMsir = NIL THEN
      Abandon("ConstArray: unsupported element type");  RETURN NIL;
    END;
    elts := NEW(REF ARRAY OF MSIR.Value, n);
    FOR i := 0 TO n - 1 DO
      elts[i] := Expr.CompileMSIR(ArrayExpr.Elt(ae, i));
      IF elts[i] = NIL THEN
        Abandon("ConstArray: element " & Fmt.Int(i) & " failed");  RETURN NIL;
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
    IF globalMapN >= MaxGlobalMap THEN RETURN END;
    MSIR.ModuleAddGlobal(m, g);
    globalMap[globalMapN].key := v;
    globalMap[globalMapN].val := g;
    INC(globalMapN);
  END GlobalMapAdd;

PROCEDURE GlobalMapAddStruct(v: Variable.T;  g: MSIR.Global;  m: MSIR.Module;
                              infoName: TEXT;  byteOff: INTEGER;
                              fieldType: MSIR.T) =
  BEGIN
    IF globalMapN >= MaxGlobalMap THEN RETURN END;
    (* Patch the global with struct field info and a StructFieldRef value. *)
    MSIR.GlobalSetStructField(g, byteOff,
                              MSIR.StructFieldRef(infoName, byteOff, fieldType));
    MSIR.ModuleAddGlobal(m, g);
    globalMap[globalMapN].key := v;
    globalMap[globalMapN].val := g;
    INC(globalMapN);
  END GlobalMapAddStruct;

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
