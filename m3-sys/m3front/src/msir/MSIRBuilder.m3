MODULE MSIRBuilder;

IMPORT MSIR, MSIRType, MSIREmit;
IMPORT M3ID, Type, Value, Formal, Variable, Scope, ProcType, Fmt, Target;
IMPORT RunTyme, Procedure;

CONST MaxVarMap    = 64;
CONST MaxExitStack = 16;
CONST MaxTryDepth  = 16;
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

  procMap:  ARRAY [0..MaxProcMap-1] OF ProcEntry;
  procMapN: INTEGER := 0;

  globalMap:  ARRAY [0..MaxGlobalMap-1] OF GlobalEntry;
  globalMapN: INTEGER := 0;

PROCEDURE BeginProc(name: M3ID.T;
                    formals: Value.T;
                    syms: Scope.T;
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

      (* Bind declared formals in declaration order by looking up their Variable.T
         in the scope by name.  This avoids confusing hidden _result/_return
         variables (also IsFormal=TRUE but NOT in ProcType.Formals) with the
         declared formals. *)
      VAR
        fDecl: Value.T := formals;
        vIdx:  INTEGER := 0;
        fInfo: Formal.Info;
      BEGIN
        WHILE fDecl # NIL DO
          Formal.Split(fDecl, fInfo);
          VAR sv := Scope.LookUp(syms, fInfo.name, strict := TRUE);
          BEGIN
            TYPECASE sv OF
            | Variable.T(svv) =>
                VAR
                  paramVal := MSIR.ProcParam(curProc, vIdx);
                  mt:        MSIR.T;
                  vType:     Type.T;
                  vGlobal, vIndirect, vLhs: BOOLEAN;
                BEGIN
                  Variable.Split(svv, vType, vGlobal, vIndirect, vLhs);
                  mt := MSIR.ValueType(paramVal);
                  IF vIndirect THEN
                    (* VAR/READONLY-indirect formal: param value has type ptr T.
                       elemType is T (the pointee). Loads/stores route through
                       the param directly — no alloca needed. *)
                    IF varMapN < MaxVarMap THEN
                      varMap[varMapN].key      := svv;
                      varMap[varMapN].val      := paramVal;
                      varMap[varMapN].elemType := MSIR.EltType(mt);
                      INC(varMapN);
                    END;
                  ELSIF MSIR.Kind(mt) = MSIR.TypeKind.Struct THEN
                    (* struct by-value formal: alloca+store for field access.
                       Use ".slot" suffix so the alloca name differs from the param name. *)
                    VAR allocaVal := MSIR.BuildAlloca(curBlock,
                          Value.GlobalName(sv, dots := FALSE, with_module := FALSE) & ".slot",
                          mt);
                    BEGIN
                      MSIR.BuildStore(curBlock, paramVal, allocaVal);
                      IF varMapN < MaxVarMap THEN
                        varMap[varMapN].key      := svv;
                        varMap[varMapN].val      := allocaVal;
                        varMap[varMapN].elemType := mt;
                        INC(varMapN);
                      END;
                    END;
                  ELSE
                    IF varMapN < MaxVarMap THEN
                      varMap[varMapN].key      := svv;
                      varMap[varMapN].val      := paramVal;
                      varMap[varMapN].elemType := NIL;
                      INC(varMapN);
                    END;
                  END;
                END;
            ELSE (* no Variable.T found for this formal name — skip *)
            END;
          END;
          INC(vIdx);
          fDecl := fDecl.next;
        END;
      END;

      (* Walk scope for locals (skip all formals, including hidden _result/_return). *)
      VAR sv: Value.T := Scope.ToList(syms);
      BEGIN
        WHILE sv # NIL DO
          TYPECASE sv OF
          | Variable.T(svv) =>
              IF NOT Variable.IsFormal(svv) THEN
                EVAL AddLocal(svv);
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

PROCEDURE DeclareGlobal(v: Variable.T;  name: TEXT;  mt: MSIR.T;
                         isTraced: BOOLEAN): BOOLEAN =
  VAR
    m:  MSIR.Module;
    g:  MSIR.Global;
  BEGIN
    IF NOT MSIREmit.IsEnabled() THEN RETURN FALSE END;
    m := MSIREmit.CurrentModule();
    IF m = NIL THEN RETURN FALSE END;
    IF globalMapN >= MaxGlobalMap THEN RETURN FALSE END;
    g := MSIR.NewGlobal(name, mt, isTraced);
    MSIR.ModuleAddGlobal(m, g);
    globalMap[globalMapN].key := v;
    globalMap[globalMapN].val := g;
    INC(globalMapN);
    RETURN TRUE;
  END DeclareGlobal;

PROCEDURE BeginModuleInit(name: TEXT): BOOLEAN =
  VAR resultT: MSIR.T;
  BEGIN
    IF NOT MSIREmit.IsEnabled() THEN RETURN FALSE END;
    <* ASSERT curProc = NIL *>
    abandoned := FALSE;
    varMapN   := 0;
    exitDepth := 0;
    tryDepth  := 0;
    blockSeq  := 0;
    resultT   := MSIR.TVoid();
    curProc  := MSIR.NewProc(name, ARRAY OF MSIR.Param{}, resultT);
    curBlock := MSIR.NewBlock("entry", ARRAY OF MSIR.BlockParam{});
    MSIR.ProcAddBlock(curProc, curBlock);
    RETURN TRUE;
  END BeginModuleInit;

BEGIN
END MSIRBuilder.
