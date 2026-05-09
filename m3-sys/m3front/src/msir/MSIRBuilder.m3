MODULE MSIRBuilder;

IMPORT MSIR, MSIRType, MSIREmit;
IMPORT M3ID, Type, Value, Formal, Variable, Scope;

CONST MaxVarMap = 64;

VAR
  curProc:    MSIR.Proc  := NIL;
  curBlock:   MSIR.Block := NIL;
  abandoned:  BOOLEAN    := FALSE;

  (* Variable.T -> MSIR.Value lookup, populated at BeginProc.
     Linear scan; v0 procs have few formals/locals. *)
  varMapKeys: ARRAY [0..MaxVarMap-1] OF Variable.T;
  varMapVals: ARRAY [0..MaxVarMap-1] OF MSIR.Value;
  varMapN:    INTEGER := 0;

PROCEDURE BeginProc(name: M3ID.T;
                    formals: Value.T;
                    syms: Scope.T;
                    result: Type.T;
                    isExternal: BOOLEAN): BOOLEAN =
  VAR
    info:      Formal.Info;
    nFormals:  INTEGER := 0;
    f:         Value.T;
    resultT:   MSIR.T;
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

      (* Bind each formal's wrapping Variable.T to its MSIR Param value
         so VarExpr references can resolve. The Variables are inserted
         into syms in the same order as ProcType.Formals(p.signature). *)
      VAR
        v:    Value.T := Scope.ToList(syms);
        vIdx: INTEGER := 0;
      BEGIN
        WHILE (v # NIL) AND (vIdx < nFormals) DO
          TYPECASE v OF
          | Variable.T(vv) =>
              IF Variable.IsFormal(vv) THEN
                IF varMapN < MaxVarMap THEN
                  varMapKeys[varMapN] := vv;
                  varMapVals[varMapN] := MSIR.ProcParam(curProc, vIdx);
                  INC(varMapN);
                END;
                INC(vIdx);
              END;
          ELSE
            (* skip non-Variable scope entries *)
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
      IF varMapKeys[i] = v THEN RETURN varMapVals[i] END;
    END;
    RETURN NIL;
  END LookupVar;

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
