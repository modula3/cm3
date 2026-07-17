(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: RaiseStmt.m3                                          *)
(* Last modified on Fri Jun 24 15:50:11 PDT 1994 by kalsow     *)
(*      modified on Sat Jan  5 04:47:38 1991 by muller         *)

MODULE RaiseStmt;

IMPORT M3ID, Expr, Token, Scanner, Stmt, StmtRep, Error, ESet;
IMPORT Value, Type, Scope, Exceptionz, AssignStmt;
IMPORT MSIR, MSIRBuilder, RunTyme, CaptureAnalysis;
IMPORT Target, RefType;
FROM M3 IMPORT QID;

TYPE
  P = Stmt.T OBJECT
        scope   : Scope.T;
        qid     : QID;
        except  : Value.T;
        arg     : Expr.T;
      OVERRIDES
        check       := Check;
        compile     := Compile;
        outcomes    := GetOutcome;
        compileMSIR := CompileMSIR;
        capture  := Capture;
      END;

PROCEDURE Parse (): Stmt.T =
  TYPE TK = Token.T;
  VAR p: P;
  BEGIN
    p := NEW (P);
    StmtRep.Init (p);
    Scanner.Match (TK.tRAISE);
    p.scope      := Scope.Top ();
    p.except     := NIL;
    p.arg        := NIL;
    p.qid.module := M3ID.NoID;
    p.qid.item   := Scanner.MatchID ();
    IF (Scanner.cur.token = TK.tDOT) THEN
      Scanner.GetToken (); (* . *)
      p.qid.module := p.qid.item;
      p.qid.item := Scanner.MatchID ();
    END;
    IF (Scanner.cur.token = TK.tLPAREN) THEN
      Scanner.GetToken ();  (* ( *)
      p.arg := Expr.Parse ();
      Scanner.Match (TK.tRPAREN);
    END;
    RETURN p;
  END Parse;

PROCEDURE Check (p: P;  VAR cs: Stmt.CheckState) =
  VAR t, u: Type.T;  v: Value.T;
  BEGIN
    Expr.TypeCheck (p.arg, cs);
    t := Expr.TypeOf (p.arg);

    v := Scope.LookUpQID (p.scope, p.qid);
    Value.TypeCheck (v, cs);
    IF (v = NIL) THEN
      Error.QID (p.qid, "undefined");
    ELSIF (Value.ClassOf (v) # Value.Class.Exception) THEN
      Error.QID (p.qid, "not an exception");
    ELSE
      p.except := v;
      ESet.NoteException (cs, v);
    END;

    u := Exceptionz.ArgType (p.except);
    IF (p.except = NIL) THEN
      (* we've already generated an error... *)
      Expr.TypeCheck (p.arg, cs);
    ELSIF (u = NIL) THEN
      (* takes no argument *)
      IF (p.arg # NIL) THEN
        Error.QID (p.qid, "exception takes no argument");
      END;
    ELSIF (p.arg = NIL) THEN
      Error.QID (p.qid, "exception requires an argument");
    ELSIF NOT Type.IsAssignable (u, t) THEN
      Error.QID (p.qid, "argument has wrong type");
    ELSE
      (* We want p.arg compiled into a temp -> No NoteUseTargetVar *)
      AssignStmt.Check (u, p.arg, cs);
    END;
  END Check;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  BEGIN
    Exceptionz.EmitRaise (p.except, p.arg);
    RETURN Stmt.Outcomes {(* Raises *)};
  END Compile;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {(* Raises *)};
  END GetOutcome;

PROCEDURE CompileMSIR (p: P) =
  (* RAISE E  or  RAISE E(arg)
     RTHooks__Raise(ex, arg, module, line):
       ex     = direct pointer to ExceptionDesc { uid, name_ptr, implicit }
       arg    = argument value cast to ptr (NIL for no-arg exceptions)
       module = NIL (diagnostics only; skip for now)
       line   = 0  (diagnostics only; skip for now)
     RTHooks__Raise never returns — emit unreachable after the call. *)
  VAR
    raiseProc : MSIR.Proc;
    descVal   : MSIR.Value;
    argVal    : MSIR.Value;
    ptrT      := MSIR.TPtr(MSIR.TVoid());
    args      : REF ARRAY OF MSIR.Value;
  BEGIN
    IF NOT MSIRBuilder.InProc() THEN RETURN END;
    IF p.except = NIL THEN
      MSIRBuilder.Abandon("raise: exception not resolved");
      RETURN;
    END;

    raiseProc := MSIRBuilder.HookProc(RunTyme.Hook.RaiseEx);
    IF raiseProc = NIL THEN
      MSIRBuilder.Abandon("raise: RTHooks__Raise not available");
      RETURN;
    END;

    (* Exception descriptor pointer. *)
    descVal := MSIRBuilder.ExcDescValue(p.except);
    IF descVal = NIL THEN
      MSIRBuilder.Abandon("raise: cannot build exception descriptor");
      RETURN;
    END;

    (* Argument: compile if present, else NIL. *)
    IF p.arg # NIL THEN
      VAR argType  := Exceptionz.ArgType(p.except);
          argInfo  : Type.Info;
      BEGIN
        EVAL Type.CheckInfo(argType, argInfo);
        IF Exceptionz.ArgByReference(argType) THEN
          (* Aggregate argument (array/struct/large value): must be heap-allocated
             so the pointer remains valid after the stack frame is unwound by C++.
             RTExStack.Raise copies the activation record by value but NOT the
             argument data; a stack alloca becomes a dangling pointer after RAISE.
             Use NEW(REF argType) → copy → pass heap pointer. *)
          VAR refT     := RefType.New(Type.StripPacked(argType), TRUE, NIL);
              b        := MSIRBuilder.CurrentBlock();
              allocHk  := MSIRBuilder.HookProc(RunTyme.Hook.NewTracedRef);
              nBytes   := argInfo.size DIV Target.Byte;
              argAddr  : MSIR.Value;
              typeCell : MSIR.Value;
              heapPtr  : MSIR.Value;
          BEGIN
            refT     := Type.Check(refT);
            typeCell := MSIRBuilder.TypeDescValueForRef(refT, nBytes,
                          argInfo.alignment DIV Target.Byte,
                          TRUE (*isTraced*));
            IF typeCell = NIL OR allocHk = NIL THEN
              MSIRBuilder.Abandon("raise: cannot heap-alloc aggregate arg");
              RETURN;
            END;
            (* Use LValue of arg to get its address; fall back to value+spill. *)
            argAddr := Expr.LValueMSIR(p.arg);
            IF argAddr = NIL THEN
              argVal := Expr.CompileMSIR(p.arg);
              IF argVal = NIL THEN RETURN END;
              b := MSIRBuilder.CurrentBlock();
              VAR slot := MSIR.BuildAlloca(b, "", MSIR.ValueType(argVal)); BEGIN
                MSIR.BuildStore(b, argVal, slot);
                argAddr := slot;
              END;
            END;
            b := MSIRBuilder.CurrentBlock();
            heapPtr := MSIR.BuildCall(b, "", allocHk,
                         ARRAY OF MSIR.Value{typeCell});
            b := MSIRBuilder.CurrentBlock();
            MSIRBuilder.EmitMemcpy(MSIR.BuildConvert(b, "", heapPtr, ptrT),
                                   MSIR.BuildConvert(b, "", argAddr, ptrT),
                                   nBytes);
            b := MSIRBuilder.CurrentBlock();
            argVal := MSIR.BuildConvert(b, "", heapPtr, ptrT);
          END;
        ELSE
          argVal := Expr.CompileMSIR(p.arg);
          IF argVal = NIL THEN RETURN END;
          VAR argT := MSIR.ValueType(argVal); b := MSIRBuilder.CurrentBlock();
          BEGIN
            CASE MSIR.Kind(argT) OF
            | MSIR.TypeKind.IWide =>
                (* Wide integer: spill to stack, pass address. *)
                VAR slot := MSIR.BuildAlloca(b, "", argT);
                BEGIN
                  MSIR.BuildStore(b, argVal, slot);
                  argVal := MSIR.BuildConvert(b, "", slot, ptrT);
                END;
            ELSE
              argVal := MSIR.BuildConvert(b, "", argVal, ptrT);
            END;
          END;
        END;
      END;
    ELSE
      argVal := MSIR.ConstNil(ptrT);
    END;

    args := NEW(REF ARRAY OF MSIR.Value, 4);
    args[0] := descVal;
    args[1] := argVal;
    (* module = &@<curMod>_M3_info (offset 0), line = current source line, so the
       unhandled-exception backstop can print the "file ... line" diagnostic.
       Get the module ref via MSIRBuilder (NOT a direct IMPORT MSIREmit here — a
       direct RaiseStmt->MSIREmit import edge perturbs m3front's module-init order
       and crashes MSIR emission of the first unit; see CurrentModuleInfoRef). *)
    args[2] := MSIRBuilder.CurrentModuleInfoRef();
    IF args[2] = NIL THEN args[2] := MSIR.ConstNil(ptrT) END;
    VAR srcFile: TEXT;  srcLine: INTEGER;
    BEGIN
      Scanner.Here(srcFile, srcLine);
      args[3] := MSIR.ConstInt(MSIR.TI(64), srcLine);
    END;

    EVAL MSIRBuilder.EmitCall("", raiseProc, args^);
    (* RTHooks__Raise never returns normally. *)
    IF NOT MSIRBuilder.CurrentBlockTerminated() THEN
      MSIR.BuildUnreachable(MSIRBuilder.CurrentBlock());
    END;
  END CompileMSIR;

PROCEDURE Capture (p: P;  ca: CaptureAnalysis.T) =
  BEGIN
    Expr.Capture (p.arg, ca);  (* NIL-safe: Expr.Capture checks for NIL *)
  END Capture;

BEGIN
END RaiseStmt.
