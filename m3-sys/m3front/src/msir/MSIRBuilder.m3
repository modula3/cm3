MODULE MSIRBuilder;

IMPORT MSIR, MSIRType, MSIREmit;
IMPORT M3ID, Type, Value, Formal, Variable, Scope, ProcType;

CONST MaxVarMap   = 64;
CONST MaxExitStack = 16;
CONST MaxProcMap  = 128;

(* Each formal maps to a Param SSA value (elemType = NIL).
   Each local maps to an alloca ptr (elemType = the allocated type). *)
TYPE VarEntry = RECORD
  key:      Variable.T;
  val:      MSIR.Value;
  elemType: MSIR.T;       (* NIL => formal; non-NIL => local alloca ptr *)
END;

TYPE ProcEntry = RECORD key: Value.T;  val: MSIR.Proc END;

VAR
  curProc:   MSIR.Proc  := NIL;
  curBlock:  MSIR.Block := NIL;
  abandoned: BOOLEAN    := FALSE;

  varMap:  ARRAY [0..MaxVarMap-1]  OF VarEntry;
  varMapN: INTEGER := 0;

  exitStack: ARRAY [0..MaxExitStack-1] OF MSIR.Block;
  exitDepth: INTEGER := 0;

  procMap:  ARRAY [0..MaxProcMap-1] OF ProcEntry;
  procMapN: INTEGER := 0;

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

    resultT := MSIRType.TranslateResult(result);
    IF resultT = NIL THEN RETURN FALSE END;

    f := formals;
    WHILE f # NIL DO INC(nFormals); f := f.next END;

    VAR params := NEW(REF ARRAY OF MSIR.Param, nFormals);
    BEGIN
      f := formals;
      FOR i := 0 TO nFormals - 1 DO
        Formal.Split(f, info);
        VAR pt := MSIRType.Translate(info.type);
        BEGIN
          IF pt = NIL THEN RETURN FALSE END;
          params[i].name := M3ID.ToText(info.name);
          params[i].type := pt;
          CASE info.mode OF
          | Formal.Mode.mVALUE    => params[i].mode := MSIR.ParamMode.ByValue;
          | Formal.Mode.mVAR      => params[i].mode := MSIR.ParamMode.Var;
          | Formal.Mode.mREADONLY => params[i].mode := MSIR.ParamMode.Readonly;
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

      (* Walk the proc scope: bind formals to Param values and locals to
         alloca ptrs.  Formals and locals share the same scope list. *)
      VAR
        v:    Value.T := Scope.ToList(syms);
        vIdx: INTEGER := 0;
      BEGIN
        WHILE v # NIL DO
          TYPECASE v OF
          | Variable.T(vv) =>
              IF Variable.IsFormal(vv) THEN
                IF varMapN < MaxVarMap THEN
                  varMap[varMapN].key      := vv;
                  varMap[varMapN].val      := MSIR.ProcParam(curProc, vIdx);
                  varMap[varMapN].elemType := NIL;
                  INC(varMapN);
                END;
                INC(vIdx);
              ELSE
                (* local variable: allocate a stack slot *)
                EVAL AddLocal(vv);
              END;
          ELSE
          END;
          v := v.next;
        END;
      END;
    END;
    RETURN TRUE;
  END BeginProc;

PROCEDURE LookupVar(v: Variable.T): MSIR.Value =
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

PROCEDURE EndProc() =
  BEGIN
    IF curProc = NIL THEN RETURN END;
    IF NOT abandoned THEN
      MSIREmit.AddProc(curProc);
    END;
    curProc   := NIL;
    curBlock  := NIL;
    abandoned := FALSE;
    varMapN   := 0;
    exitDepth := 0;
  END EndProc;

PROCEDURE Abandon(<*UNUSED*> reason: TEXT) =
  BEGIN
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
  VAR b: MSIR.Block;
  BEGIN
    b := MSIR.NewBlock(label, ARRAY OF MSIR.BlockParam{});
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
          params[i].type := pt;
          CASE info.mode OF
          | Formal.Mode.mVALUE    => params[i].mode := MSIR.ParamMode.ByValue;
          | Formal.Mode.mVAR      => params[i].mode := MSIR.ParamMode.Var;
          | Formal.Mode.mREADONLY => params[i].mode := MSIR.ParamMode.Readonly;
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

BEGIN
END MSIRBuilder.
