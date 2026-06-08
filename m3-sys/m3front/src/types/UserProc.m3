(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: UserProc.m3                                           *)
(* Last Modified On Tue Jun 20 15:59:11 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 12:16:47 PDT 1995 By ericv      *)

MODULE UserProc;

IMPORT M3ID, CG, Type, Expr, ExprRep, ProcType, Formal;
IMPORT Procedure, NamedExpr, Variable, QualifyExpr, Value;
IMPORT CallExpr, ProcExpr, Marker, ErrType;
IMPORT MSIR, MSIRBuilder, MSIRType, Method, Target, CaptureAnalysis;
IMPORT M3RT, ObjectType;

(* Externally dispatched-to, using a field of Methods: *)
PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;  proc := ce.proc;
  BEGIN
    t := Expr.TypeOf (proc);
    IF (t = ErrType.T) THEN RETURN t; END;
    IF (t = NIL) THEN t := QualifyExpr.MethodType (proc) END;
    RETURN ProcType.Result (Type.Base (t));
  END TypeOf;

(* Externally dispatched-to, using a field of Methods: *)
PROCEDURE NeedsAddress (<*UNUSED*> ce: CallExpr.T) =
  BEGIN
    (* ya, my result is a structure that needs an address... *)
  END NeedsAddress;

(* Externally dispatched-to, using a field of Methods: *)
PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR t: Type.T;  formals: Value.T;
  BEGIN
    t := Expr.TypeOf (ce.proc);
    IF (t = NIL) THEN t := QualifyExpr.MethodType(ce.proc) END;
    t := Type.Base (t);
    formals := ProcType.Formals (t);
    IF Formal.CheckArgs (cs, ce.args, formals, ce.proc) THEN END;
    ce.type := ProcType.Result (t);
  END Check;

PROCEDURE InnerPrep (ce: CallExpr.T) =
  VAR
    proc         : Expr.T    := ce.proc;
    args         : Expr.List := ce.args;
    p_value      : Value.T;
    p_type       : Type.T;
    p_temp       : CG.Val;
    t_result     : Type.T;
    cg_result    : CG.Type;
    tmp_result   : CG.Var;
    lhs_result   : CG.Val := NIL;
    align_result : INTEGER;
    large_result : BOOLEAN := FALSE;
    formals, v   : Value.T;
    n            : INTEGER;
    result_info  : Type.Info;
    callConv     : CG.CallingConvention;
    handler      : CG.Label;
    handler_body : CG.Label;
    info         : CG.Var;
  BEGIN
    (* If this is a direct structure return, the LHS has already
     * been prepped and compiled -- save it.  *)
    IF ce.doDirectAssign THEN lhs_result := CG.Pop (); END;

    p_type := Expr.TypeOf (proc);
    IF (p_type = NIL) THEN p_type := QualifyExpr.MethodType (proc); END;
    p_type := Type.Base (p_type);

    (* grab the formals list *)
    formals := ProcType.Formals (p_type);
    callConv := ProcType.CallConv (p_type);

    (* prep the target *)
    IF NOT IsProcedureLiteral (proc, p_value) THEN
      Expr.Prep (proc);
      p_value := NIL;
    END;

    (* prep the actuals *)
    IF callConv.args_left_to_right THEN
      v := formals;  n := 0;
      WHILE (v # NIL) DO
        Formal.PrepArg (v, args[n]);
        v := v.next;  INC (n);
      END;
    ELSE
      (* use the runtime stack to reverse the list... *)
      PrepRightToLeft (formals, args, 0);
    END;

    (* get the result type and allocate the temp for return results *)
    t_result     := Type.CheckInfo (ProcType.Result (p_type), result_info);
    cg_result    := ProcType.CGResult (p_type);
    align_result := result_info.alignment;
    large_result := ProcType.LargeResult (t_result);
    IF large_result AND NOT ce.doDirectAssign THEN
      tmp_result := CG.Declare_temp (result_info.size, align_result,
                                     CG.Type.Struct, in_memory := TRUE);
    END;

    (* start the call *)
    IF (p_value # NIL) THEN
      Procedure.StartCall (p_value);
    ELSIF CouldBeClosure (proc) THEN
      Expr.Compile (proc);
      p_temp := CG.Pop_temp ();
      CG.Start_call_indirect (cg_result, callConv);
    ELSE
      Expr.Compile (proc);
      p_temp := CG.Pop ();
      CG.Start_call_indirect (cg_result, callConv);
    END;

    (* pass the arguments *)
    IF large_result AND
      (callConv.args_left_to_right = callConv.results_on_left) THEN
      GenResultArg (lhs_result, tmp_result, align_result);
    END;
    IF callConv.args_left_to_right THEN
      EVAL QualifyExpr.PassObject (proc);
      v := formals;  n := 0;
      WHILE (v # NIL) DO
        Formal.EmitArg (proc, v, args[n]);
        v := v.next;  INC (n);
      END;
    ELSE (* evaluate args right to left *)
      (* use the runtime stack to reverse the list... *)
      EmitRightToLeft (proc, formals, args, 0);
      EVAL QualifyExpr.PassObject (proc);
    END;
    IF large_result AND
      (callConv.args_left_to_right # callConv.results_on_left) THEN
      GenResultArg (lhs_result, tmp_result, align_result);
    END;

    (* generate the call *)
    IF (p_value # NIL) THEN
      ce.tmp := Procedure.EmitValueCall (p_value);
    ELSIF CouldBeClosure (proc) THEN
      ce.tmp := GenClosureCall (p_temp, cg_result, p_type, callConv);
      CG.Free (p_temp);
    ELSE
      CG.Push (p_temp);
      IF Marker.NextHandler(handler, handler_body, info) THEN
        CG.Invoke_indirect(cg_result, callConv, handler);
        Marker.Invoked();
      ELSE
        CG.Gen_Call_indirect (cg_result, callConv);
      END;
      ce.tmp := Marker.EmitExceptionTest (p_type, need_value := TRUE);
      CG.Free (p_temp);
    END;

    ce.align := align_result;
    IF (large_result) THEN
      <*ASSERT ce.tmp = NIL *>
      IF ce.doDirectAssign THEN
        ce.tmp := lhs_result;
      ELSE
        CG.Load_addr_of_temp (tmp_result, 0, align_result);
        ce.tmp := CG.Pop ();
      END;
    END;
  END InnerPrep;

(* Externally dispatched-to, using a field of Methods: *)
PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    IF NOT ce.doDirectAssign
    THEN InnerPrep (ce)
 (* ELSE postpone InnerPrep until Compile, when LHS will have been pushed. *)
    END;
  END Prep;

(* Externally dispatched-to: *)
PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    IF ce.doDirectAssign
    THEN (* InnerPrep was postponed until now. *)
      InnerPrep (ce)
    END;
    IF (ce.tmp # NIL) THEN
      CG.Push (ce.tmp);
      CG.Boost_addr_alignment (ce.align);
      CG.Free (ce.tmp);
      ce.tmp := NIL;
    END;
  END Compile;

PROCEDURE PrepRightToLeft (formal: Value.T;  args: Expr.List;  cnt: INTEGER) =
  BEGIN
    IF (formal # NIL) THEN
      PrepRightToLeft (formal.next, args, cnt+1);
      Formal.PrepArg (formal, args[cnt]);
    END;
  END PrepRightToLeft;

PROCEDURE EmitRightToLeft (proc: Expr.T;  formal: Value.T;
                           args: Expr.List;  cnt: INTEGER) =
  BEGIN
    IF (formal # NIL) THEN
      EmitRightToLeft (proc, formal.next, args, cnt+1);
      Formal.EmitArg (proc, formal, args[cnt]);
    END;
  END EmitRightToLeft;

PROCEDURE GenResultArg (lhs: CG.Val;  tmp: CG.Var;  align: CG.Alignment) =
  BEGIN
    IF (lhs # NIL)
      THEN CG.Push (lhs);
      ELSE CG.Load_addr_of (tmp, 0, align);
    END;
    CG.Pop_param (CG.Type.Addr);
  END GenResultArg;

PROCEDURE GenClosureCall (p_temp: CG.Val;  result: CG.Type;
                          sig: Type.T;  cc: CG.CallingConvention): CG.Val =
  VAR skip := CG.Next_label ();
      handler,handler_body : CG.Label;
      info : CG.Var;
  BEGIN
    CG.If_closure (p_temp, CG.No_label, skip, CG.Maybe);
    CG.Push (p_temp);
    CG.Closure_frame ();
    CG.Pop_static_link ();
    CG.Push (p_temp);
    CG.Closure_proc ();
    CG.Store_temp (p_temp);
    CG.Set_label (skip);
    CG.Push (p_temp);
    IF Marker.NextHandler(handler, handler_body, info) THEN
      CG.Invoke_indirect(result, cc, handler);
      Marker.Invoked();
    ELSE
      CG.Gen_Call_indirect (result, cc);
    END;
    RETURN Marker.EmitExceptionTest (sig, need_value := TRUE);
  END GenClosureCall;

PROCEDURE CouldBeClosure (proc: Expr.T): BOOLEAN =
  VAR name: M3ID.T;  value: Value.T;
  BEGIN
    RETURN (NamedExpr.Split (proc, name, value))
        AND (Value.ClassOf (value) = Value.Class.Var)
        AND (Variable.HasClosure (value));
  END CouldBeClosure;

(* EXPORTED: *)
PROCEDURE IsProcedureLiteral (e: Expr.T;  VAR proc: Value.T): BOOLEAN =
  VAR name: M3ID.T;  v: Value.T;  vc: Value.Class;
  BEGIN
    e := Expr.ConstValue (e);
    IF (e = NIL) THEN RETURN FALSE END;
    IF NOT (NamedExpr.Split (e, name, v)
            OR QualifyExpr.Split (e, v)
            OR ProcExpr.Split (e, v)) THEN RETURN FALSE END;
    vc := Value.ClassOf (v);
    IF (vc = Value.Class.Procedure)
      THEN  proc := Value.Base (v);  RETURN TRUE;
      ELSE  RETURN FALSE;
    END;

    (*******
    RETURN (NamedExpr.Split (e, name, proc)
              OR QualifyExpr.Split (e, proc)
              OR ProcExpr.Split (e, proc))
          AND (Value.ClassOf (proc) = Value.Class.Procedure);
    *******)
  END IsProcedureLiteral;

(* EXPORTED: *)
PROCEDURE CompileMSIR (p: CallExpr.T): MSIR.Value =
  (* MSIR handler for user procedure calls — direct, virtual, and nested.
     Dispatched through methods.compileMSIR by CallExpr.CompileMSIR. *)
  VAR
    v:          Value.T;
    procType    : Type.T;
    msirCallee: MSIR.Proc;
    argVals:    REF ARRAY OF MSIR.Value;
    n:          INTEGER;
    argVal:     MSIR.Value;
    isNested:   BOOLEAN;
  BEGIN
    (* Expr.TypeOf returns NIL for method expressions (obj.method cannot be
       used as a first-class value).  Fall back to QualifyExpr.MethodType. *)
    VAR t := Expr.TypeOf (p.proc); BEGIN
      IF t = NIL THEN t := QualifyExpr.MethodType (p.proc) END;
      procType := Type.Base (t);
    END;
    IF NOT IsProcedureLiteral(p.proc, v) THEN
      (* Virtual method dispatch: obj.method(args) *)
      VAR
        methodVal : Value.T;
        methodInfo: Method.Info;
        objExpr   : Expr.T;
        objVal    : MSIR.Value;
        rtype     : MSIR.T;
        dispArgs  : REF ARRAY OF MSIR.Value;
        midx      : INTEGER;
      BEGIN
        IF QualifyExpr.Split(p.proc, methodVal) AND
           Value.ClassOf(methodVal) = Value.Class.Method THEN
          objExpr := QualifyExpr.LhsExpr(p.proc);
          EVAL Method.Split(methodVal, methodInfo);
          IF objExpr = NIL THEN
            (* T.m(self, ...): a static supercall naming the method through its
               type, not an instance.  T's binding of m lives in T's typecell
               OTC_defaultMethods table; load it and call indirectly, with the
               receiver passed as the first explicit actual.  Mirrors the C path
               QualifyExpr.Compile (Class.objTypeMethod). *)
            VAR otObjType, otHolder: Type.T; BEGIN
              IF NOT QualifyExpr.ObjTypeMethod(p.proc, otObjType, otHolder) THEN
                MSIRBuilder.Abandon("method call: cannot get receiver");
                RETURN NIL;
              END;
              VAR objOff := ObjectType.MethodOffset(otHolder); BEGIN
                IF objOff < 0 THEN
                  MSIRBuilder.Abandon(
                    "static method call: runtime method offset (opaque holder)");
                  RETURN NIL;
                END;
                VAR
                  b     := MSIRBuilder.CurrentBlock();
                  ptrT  := MSIR.TPtr(MSIR.TVoid());
                  tc    := MSIRBuilder.TypeLinkValueForObject(otObjType);
                  dmOff := M3RT.OTC_defaultMethods DIV Target.Char.size;
                  pOff  := (methodInfo.offset + objOff) DIV Target.Char.size;
                  rtype := MSIRType.TranslateResult(ProcType.Result(procType));
                  dmTbl, fn: MSIR.Value;
                  callArgs : REF ARRAY OF MSIR.Value;
                BEGIN
                  IF tc = NIL THEN RETURN NIL END;
                  (* table = *(typecell + OTC_defaultMethods) *)
                  dmTbl := MSIR.BuildLoad(b, "", ptrT,
                             MSIR.BuildPtrAdd(b, "", tc, dmOff));
                  (* proc = *(table + method byte offset) *)
                  fn := MSIR.BuildLoad(b, "", ptrT,
                          MSIR.BuildPtrAdd(b, "", dmTbl, pOff));
                  n := NUMBER(p.args^);
                  callArgs := NEW(REF ARRAY OF MSIR.Value, n);
                  VAR fv := ProcType.Formals(procType); BEGIN
                    FOR i := 0 TO n - 1 DO
                      <* ASSERT fv # NIL *>
                      callArgs[i] := Formal.EmitArgMSIR(fv, p.args[i]);
                      fv := fv.next;
                      IF callArgs[i] = NIL THEN RETURN NIL END;
                    END;
                  END;
                  RETURN MSIRBuilder.EmitClosureCall("", fn, rtype, callArgs^);
                END;
              END;
            END;
          END;
          VAR mBase := QualifyExpr.MethodSlotBase(p.proc); BEGIN
            IF mBase < 0 THEN
              (* Opaque supertype: static method offset not known.  Mirrors the
                 CG backend: Type.LoadInfo(holder, OTC_methodOffset) + Index_bytes.
                 Load the methodOffset field from the holder's TypeCell at runtime,
                 add methodInfo.offset (byte offset within this type's own methods),
                 then do a dynamic vtable dispatch using the computed byte offset.
                 OTC_methodOffset = TC_SIZE + OTC_parentID(IP) + OTC_linkProc(AP)
                                  + OTC_dataOffset(IP) = TC_SIZE + 2*IP + AP. *)
              VAR
                ptrT   := MSIR.TPtr(MSIR.TVoid());
                intT   := MSIR.TI(Target.Integer.size);
                b2     := MSIRBuilder.CurrentBlock();
                (* holder TypeCell via the TypeLink for the holder type *)
                holderType   : Type.T;
                tc          : MSIR.Value;
                otcMethOff  : INTEGER;
                dynBaseVal  : MSIR.Value;
                totalByteOff: MSIR.Value;
                suite, slotPtr, fn: MSIR.Value;
                nExtra      : INTEGER;
                largeResT   : MSIR.T;
                largeResSlot: MSIR.Value := NIL;
                isLargeR    := ProcType.LargeResult(ProcType.Result(procType));
                rtype2      : MSIR.T;
                allArgs2    : REF ARRAY OF MSIR.Value;
                procResult2 := ProcType.Result(procType);
              BEGIN
                holderType := QualifyExpr.MethodHolder(p.proc);
                IF holderType = NIL THEN
                  MSIRBuilder.Abandon("method call: cannot get holder type for opaque dispatch");
                  RETURN NIL;
                END;
                objVal := Expr.CompileMSIR(objExpr);
                IF objVal = NIL THEN RETURN NIL END;
                n := NUMBER(p.args^);
                dispArgs := NEW(REF ARRAY OF MSIR.Value, n);
                VAR fv := ProcType.Formals(procType); BEGIN
                  FOR i := 0 TO n - 1 DO
                    <* ASSERT fv # NIL *>
                    dispArgs[i] := Formal.EmitArgMSIR(fv, p.args[i]);
                    fv := fv.next;
                    IF dispArgs[i] = NIL THEN RETURN NIL END;
                  END;
                END;
                b2 := MSIRBuilder.CurrentBlock();
                tc := MSIRBuilder.TypeLinkValueForObject(holderType);
                IF tc = NIL THEN
                  MSIRBuilder.Abandon("method call: no TypeLink for opaque holder");
                  RETURN NIL;
                END;
                (* OTC_methodOffset byte position within the ObjectTypeCell.
                   OTC = TC_SIZE + OTC_parentID(IP) + OTC_linkProc(AP)
                        + OTC_dataOffset(IP) *)
                otcMethOff := M3RT.OTC_methodOffset DIV Target.Char.size;
                dynBaseVal  := MSIR.BuildLoad(b2, "", intT,
                                 MSIRBuilder.BuildPtrByteOff(b2, "", tc, otcMethOff));
                b2 := MSIRBuilder.CurrentBlock();
                totalByteOff := MSIR.BuildIAdd(b2, "", dynBaseVal,
                                  MSIR.ConstInt(intT, methodInfo.offset));
                b2 := MSIRBuilder.CurrentBlock();
                (* Load vtable pointer from object (first word). *)
                suite := MSIR.BuildLoad(b2, "", ptrT,
                           MSIR.BuildConvert(b2, "", objVal, MSIR.TPtr(ptrT)));
                b2 := MSIRBuilder.CurrentBlock();
                (* GEP into vtable by dynamic byte offset. *)
                slotPtr := MSIR.BuildGepByte(b2, "", suite, totalByteOff);
                b2 := MSIRBuilder.CurrentBlock();
                fn := MSIR.BuildLoad(b2, "", ptrT, slotPtr);
                b2 := MSIRBuilder.CurrentBlock();
                (* Build call args: [resultSlot?, obj, explicit args] *)
                IF isLargeR THEN
                  largeResT    := MSIRType.Translate(procResult2);
                  largeResSlot := MSIR.BuildAlloca(b2, "", largeResT);
                END;
                rtype2 := MSIRType.TranslateResult(procResult2);
                nExtra := 1 + ORD(isLargeR);
                allArgs2 := NEW(REF ARRAY OF MSIR.Value, nExtra + n);
                IF isLargeR THEN
                  allArgs2[0] := largeResSlot;
                  allArgs2[1] := objVal;
                ELSE
                  allArgs2[0] := objVal;
                END;
                FOR k := 0 TO n - 1 DO allArgs2[nExtra + k] := dispArgs[k] END;
                (* Direct indirect call through the vtable function pointer —
                   NOT closure dispatch (vtable holds direct proc ptrs). *)
                VAR callRes := MSIRBuilder.EmitCallIndirect("", fn, rtype2, allArgs2^); BEGIN
                  IF isLargeR THEN
                    RETURN MSIR.BuildLoad(MSIRBuilder.CurrentBlock(), "", largeResT, largeResSlot);
                  END;
                  RETURN callRes;
                END;
              END;
            END;
            midx := (mBase + methodInfo.offset) DIV Target.Address.size;
          END;
          objVal := Expr.CompileMSIR(objExpr);
          IF objVal = NIL THEN RETURN NIL END;
          n       := NUMBER(p.args^);
          dispArgs := NEW(REF ARRAY OF MSIR.Value, n);
          VAR fv := ProcType.Formals(procType); BEGIN
            FOR i := 0 TO n - 1 DO
              <* ASSERT fv # NIL *>
              dispArgs[i] := Formal.EmitArgMSIR(fv, p.args[i]);
              fv := fv.next;
              IF dispArgs[i] = NIL THEN RETURN NIL END;
            END;
          END;
          VAR procResult    := ProcType.Result(procType);
              isLargeResult := ProcType.LargeResult(procResult);
              resultMsirT   : MSIR.T;
              resultSlot    : MSIR.Value := NIL;
          BEGIN
            IF isLargeResult THEN
              resultMsirT := MSIRType.Translate(procResult);
              IF resultMsirT = NIL THEN
                MSIRBuilder.Abandon("method call: large-result type not translatable");
                RETURN NIL;
              END;
              resultSlot := MSIR.BuildAlloca(MSIRBuilder.CurrentBlock(), "", resultMsirT);
              rtype := NIL;
            ELSE
              rtype := MSIRType.TranslateResult(procResult);
            END;
            IF isLargeResult THEN
              EVAL MSIRBuilder.EmitMethodCall("", objVal, midx, rtype, resultSlot, dispArgs^);
              RETURN MSIR.BuildLoad(MSIRBuilder.CurrentBlock(), "", resultMsirT, resultSlot);
            ELSE
              RETURN MSIRBuilder.EmitMethodCall("", objVal, midx, rtype, NIL, dispArgs^);
            END;
          END;
        END;
      END;
      (* Indirect call through a procedure-typed variable or expression. *)
      VAR
        fnVal   : MSIR.Value;
        rtype   : MSIR.T;
        iArgVals: REF ARRAY OF MSIR.Value;
      BEGIN
        fnVal := Expr.CompileMSIR(p.proc);
        IF fnVal = NIL THEN RETURN NIL END;
        rtype := MSIRType.TranslateResult(ProcType.Result(procType));
        n := NUMBER(p.args^);
        iArgVals := NEW(REF ARRAY OF MSIR.Value, n);
        VAR fv := ProcType.Formals(procType); BEGIN
          FOR i := 0 TO n - 1 DO
            <* ASSERT fv # NIL *>
            argVal := Formal.EmitArgMSIR(fv, p.args[i]);
            fv := fv.next;
            IF argVal = NIL THEN RETURN NIL END;
            iArgVals[i] := argVal;
          END;
        END;
        RETURN MSIRBuilder.EmitClosureCall("", fnVal, rtype, iArgVals^);
      END;
    END;
    msirCallee := MSIRBuilder.LookupOrCreateProc(v, procType);
    IF msirCallee = NIL THEN RETURN NIL END;
    isNested := Procedure.IsNested(v);
    n       := NUMBER(p.args^);
    argVals := NEW(REF ARRAY OF MSIR.Value, n);
    VAR fv := ProcType.Formals(procType);
    BEGIN
      FOR i := 0 TO n - 1 DO
        <* ASSERT fv # NIL *>
        argVal := Formal.EmitArgMSIR(fv, p.args[i]);
        fv := fv.next;
        IF argVal = NIL THEN RETURN NIL END;
        argVals[i] := argVal;
      END;
    END;
    (* Large-result: alloca slot, prepend hidden result ptr, load after call. *)
    VAR procResult    := ProcType.Result(procType);
        isLargeResult := ProcType.LargeResult(procResult);
        resultSlot    : MSIR.Value := NIL;
        resultMsirT   : MSIR.T    := NIL;
        actualArgs    : REF ARRAY OF MSIR.Value;
        callResult    : MSIR.Value;
    BEGIN
      IF isLargeResult THEN
        resultMsirT := MSIRType.Translate(procResult);
        IF resultMsirT = NIL THEN
          MSIRBuilder.Abandon("large-result type not translatable");
          RETURN NIL;
        END;
        resultSlot := MSIR.BuildAlloca(MSIRBuilder.CurrentBlock(), "", resultMsirT);
        IF isNested THEN
          callResult := MSIRBuilder.EmitNestedCall("", msirCallee, v, resultSlot, argVals^);
        ELSE
          actualArgs := NEW(REF ARRAY OF MSIR.Value, 1 + n);
          actualArgs[0] := resultSlot;
          FOR i := 0 TO n - 1 DO actualArgs[1 + i] := argVals[i] END;
          callResult := MSIRBuilder.EmitCall("", msirCallee, actualArgs^);
        END;
        RETURN MSIR.BuildLoad(MSIRBuilder.CurrentBlock(), "", resultMsirT, resultSlot);
      ELSE
        IF isNested THEN
          callResult := MSIRBuilder.EmitNestedCall("", msirCallee, v, NIL, argVals^);
        ELSE
          callResult := MSIRBuilder.EmitCall("", msirCallee, argVals^);
        END;
        RETURN callResult;
      END;
    END;
  END CompileMSIR;

PROCEDURE LValueMSIR (p: CallExpr.T): MSIR.Value =
  (* Return the address of the call result so it can be passed READONLY by-ref.
     Only direct (non-nested, non-virtual) calls are supported. *)
  VAR
    v         : Value.T;
    procType  : Type.T;
    msirCallee: MSIR.Proc;
    argVals   : REF ARRAY OF MSIR.Value;
    n         : INTEGER;
    argVal    : MSIR.Value;
  BEGIN
    VAR t := Expr.TypeOf(p.proc); BEGIN
      IF t = NIL THEN t := QualifyExpr.MethodType(p.proc) END;
      procType := Type.Base(t);
    END;
    IF NOT IsProcedureLiteral(p.proc, v) THEN
      (* Virtual method dispatch or indirect proc call — handle the lvalue case.
         We need an addressable result: allocate a slot, emit the method call
         using the CM3 large-result hidden-pointer convention, return the slot. *)
      VAR
        methodVal : Value.T;
        methodInfo: Method.Info;
        objExpr   : Expr.T;
        objVal    : MSIR.Value;
        dispArgs  : REF ARRAY OF MSIR.Value;
        midx      : INTEGER;
        procResult    : Type.T;
        isLargeResult : BOOLEAN;
        resultMsirT   : MSIR.T;
        resultSlot    : MSIR.Value;
        rtype         : MSIR.T;
      BEGIN
        IF QualifyExpr.Split(p.proc, methodVal) AND
           Value.ClassOf(methodVal) = Value.Class.Method THEN
          objExpr := QualifyExpr.LhsExpr(p.proc);
          IF objExpr = NIL THEN
            MSIRBuilder.Abandon("method lvalue: cannot get receiver");
            RETURN NIL;
          END;
          EVAL Method.Split(methodVal, methodInfo);
          VAR mBase2 := QualifyExpr.MethodSlotBase(p.proc); BEGIN
            IF mBase2 < 0 THEN
              MSIRBuilder.Abandon("method lvalue: vtable base offset unknown (opaque type)");
              RETURN NIL;
            END;
            midx := (mBase2 + methodInfo.offset) DIV Target.Address.size;
          END;
          objVal     := Expr.CompileMSIR(objExpr);
          IF objVal = NIL THEN RETURN NIL END;
          n          := NUMBER(p.args^);
          procResult := ProcType.Result(procType);
          isLargeResult := ProcType.LargeResult(procResult);
          resultMsirT   := MSIRType.Translate(procResult);
          IF resultMsirT = NIL THEN
            MSIRBuilder.Abandon("method lvalue: result type not translatable");
            RETURN NIL;
          END;
          dispArgs   := NEW(REF ARRAY OF MSIR.Value, n);
          FOR i := 0 TO n - 1 DO
            dispArgs[i] := Expr.CompileMSIR(p.args[i]);
            IF dispArgs[i] = NIL THEN RETURN NIL END;
          END;
          IF isLargeResult THEN
            (* Hidden-ptr convention: alloca receives result written by callee. *)
            resultSlot := MSIR.BuildAlloca(MSIRBuilder.CurrentBlock(), "", resultMsirT);
            EVAL MSIRBuilder.EmitMethodCall("", objVal, midx, NIL, resultSlot, dispArgs^);
            RETURN resultSlot;
          ELSE
            (* Small result: make call, spill return value to alloca. *)
            rtype      := MSIRType.TranslateResult(procResult);
            resultSlot := MSIR.BuildAlloca(MSIRBuilder.CurrentBlock(), "", resultMsirT);
            VAR callVal := MSIRBuilder.EmitMethodCall("", objVal, midx, rtype, NIL, dispArgs^);
            BEGIN
              IF callVal = NIL THEN RETURN NIL END;
              MSIR.BuildStore(MSIRBuilder.CurrentBlock(), callVal, resultSlot);
            END;
            RETURN resultSlot;
          END;
        END;
      END;
      MSIRBuilder.Abandon("lvalue of indirect call result not yet supported in MSIR");
      RETURN NIL;
    END;
    IF Procedure.IsNested(v) THEN
      MSIRBuilder.Abandon("lvalue of nested proc result not yet supported in MSIR");
      RETURN NIL;
    END;
    msirCallee := MSIRBuilder.LookupOrCreateProc(v, procType);
    IF msirCallee = NIL THEN RETURN NIL END;
    n := NUMBER(p.args^);
    argVals := NEW(REF ARRAY OF MSIR.Value, n);
    VAR fv := ProcType.Formals(procType);
    BEGIN
      FOR i := 0 TO n - 1 DO
        <* ASSERT fv # NIL *>
        argVal := Formal.EmitArgMSIR(fv, p.args[i]);
        fv := fv.next;
        IF argVal = NIL THEN RETURN NIL END;
        argVals[i] := argVal;
      END;
    END;
    VAR procResult    := ProcType.Result(procType);
        isLargeResult := ProcType.LargeResult(procResult);
        resultMsirT   : MSIR.T;
    BEGIN
      resultMsirT := MSIRType.Translate(procResult);
      IF resultMsirT = NIL THEN
        MSIRBuilder.Abandon("proc lvalue: result type not translatable");
        RETURN NIL;
      END;
      IF isLargeResult THEN
        (* Hidden-ptr convention: alloca slot, prepend as arg, call, return slot. *)
        VAR actualArgs := NEW(REF ARRAY OF MSIR.Value, 1 + n);
            resultSlot := MSIR.BuildAlloca(MSIRBuilder.CurrentBlock(), "", resultMsirT);
        BEGIN
          actualArgs[0] := resultSlot;
          FOR i := 0 TO n - 1 DO actualArgs[1 + i] := argVals[i] END;
          EVAL MSIRBuilder.EmitCall("", msirCallee, actualArgs^);
          RETURN resultSlot;
        END;
      ELSE
        (* Small result: call, alloca temp, store, return temp address. *)
        VAR callResult := MSIRBuilder.EmitCall("", msirCallee, argVals^);
            tmpSlot    : MSIR.Value;
        BEGIN
          IF callResult = NIL THEN RETURN NIL END;
          tmpSlot := MSIR.BuildAlloca(MSIRBuilder.CurrentBlock(), "", resultMsirT);
          MSIR.BuildStore(MSIRBuilder.CurrentBlock(), callResult, tmpSlot);
          RETURN tmpSlot;
        END;
      END;
    END;
  END LValueMSIR;

PROCEDURE Capture (ce: CallExpr.T;  ca: CaptureAnalysis.T) =
  (* Capture proc and call arguments using formal parameter modes. *)
  VAR
    procType := Type.Base (Expr.TypeOf (ce.proc));
    formal   : Value.T;
    finfo    : Formal.Info;
  BEGIN
    Expr.Capture(ce.proc, ca);
    formal := ProcType.Formals(procType);
    FOR i := 0 TO LAST(ce.args^) DO
      IF formal # NIL THEN
        Formal.Split(formal, finfo);
        formal := formal.next;
      ELSE
        finfo.mode := Formal.Mode.mVALUE;  (* extra args beyond formal list *)
      END;
      IF finfo.mode = Formal.Mode.mVAR THEN
        Expr.CaptureLV(ce.args[i], ca);
      ELSE
        Expr.Capture(ce.args[i], ca);
      END;
    END;
    (* Transitively add sibling nested-proc captures so the caller can forward
       outer-scope vars the callee needs.  Example: P0_0 calls Dump0 which needs
       P0.LVisitNo; adding Dump0's captures to P0_0's set ensures P0 passes
       LVisitNo to P0_0 (via an extra capture param) and P0_0 forwards it to Dump0.
       Uses captureMap data built by the first PreRegisterNestedCaptures round;
       fires on the second round where that data is already available.
       IMPORTANT: skip module-scope globals (v.global = TRUE).  Those are always
       accessible via LookupVar/LookupVarAddr through globalMap without lambda-lifting.
       Adding them as value captures would shadow the globalMap lookup and break
       LookupVarAddr for callers that need the write address (e.g. VAR params). *)
    VAR calleeV: Value.T;
    BEGIN
      IF IsProcedureLiteral(ce.proc, calleeV) AND Procedure.IsNested(calleeV) THEN
        VAR siblCaps := MSIRBuilder.GetProcCaptures(calleeV);
        BEGIN
          IF siblCaps # NIL THEN
            FOR i := 0 TO NUMBER(siblCaps^) - 1 DO
              VAR sv := siblCaps[i].var;
                  svType: Type.T;  svGlobal, svIndirect, svTraced: BOOLEAN;
              BEGIN
                Variable.Split(sv, svType, svGlobal, svIndirect, svTraced);
                IF NOT svGlobal THEN
                  CaptureAnalysis.Note(ca, sv, siblCaps[i].written);
                END;
              END;
            END;
          END;
        END;
      END;
    END;
  END Capture;

PROCEDURE Initialize () =
  BEGIN
    Methods := CallExpr.NewMethodList (0, 99999, FALSE, TRUE, TRUE, NIL,
                                 TypeOf,
                                 TypeOf,
                                 NeedsAddress,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.PrepNoBranch,
                                 CallExpr.NoBranch,
                                 CallExpr.NoValue,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    CallExpr.SetMethodMSIR      (Methods, CompileMSIR);
    CallExpr.SetMethodLValueMSIR(Methods, LValueMSIR);
    CallExpr.SetMethodCapture   (Methods, Capture);
  END Initialize;

BEGIN
END UserProc.
