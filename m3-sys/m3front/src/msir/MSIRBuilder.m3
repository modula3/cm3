MODULE MSIRBuilder;

IMPORT MSIR, MSIRType, MSIREmit;
IMPORT M3ID, Type, Value, Formal, Variable, Scope;

CONST MaxVarMap = 64;

(* Each formal maps to a Param SSA value (elemType = NIL).
   Each local maps to an alloca ptr (elemType = the allocated type). *)
TYPE VarEntry = RECORD
  key:      Variable.T;
  val:      MSIR.Value;
  elemType: MSIR.T;       (* NIL => formal; non-NIL => local alloca ptr *)
END;

VAR
  curProc:   MSIR.Proc  := NIL;
  curBlock:  MSIR.Block := NIL;
  abandoned: BOOLEAN    := FALSE;

  varMap: ARRAY [0..MaxVarMap-1] OF VarEntry;
  varMapN: INTEGER := 0;

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

BEGIN
END MSIRBuilder.
