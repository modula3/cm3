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

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;  proc := ce.proc;
  BEGIN
    t := Expr.TypeOf (proc);
    IF (t = ErrType.T) THEN RETURN t; END;
    IF (t = NIL) THEN t := QualifyExpr.MethodType (proc) END;
    RETURN ProcType.Result (Type.Base (t));
  END TypeOf;

PROCEDURE NeedsAddress (<*UNUSED*> ce: CallExpr.T) =
  BEGIN
    (* ya, my result is a structure that needs an address... *)
  END NeedsAddress;

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

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    (* all the real work is done by Prep *)
    IF (ce.tmp # NIL) THEN
      CG.Push (ce.tmp);
      CG.Boost_addr_alignment (ce.align);
      CG.Free (ce.tmp);
      ce.tmp := NIL;
    END;
  END Compile;

PROCEDURE Prep (ce: CallExpr.T) =
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
  BEGIN
    (* If this is a direct structure return, the LHS has already
     * been prepped and compiled -- save it.  *)
    IF ce.do_direct THEN lhs_result := CG.Pop (); END;

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
    IF large_result AND NOT ce.do_direct THEN
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
      CG.Gen_Call_indirect (cg_result, callConv);
      ce.tmp := Marker.EmitExceptionTest (p_type, need_value := TRUE);
      CG.Free (p_temp);
    END;

    ce.align := align_result;
    IF (large_result) THEN
      <*ASSERT ce.tmp = NIL *>
      IF ce.do_direct THEN
        ce.tmp := lhs_result;
      ELSE
        CG.Load_addr_of_temp (tmp_result, 0, align_result);
        ce.tmp := CG.Pop ();
      END;
    END;
  END Prep;

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
    CG.Gen_Call_indirect (result, cc);
    RETURN Marker.EmitExceptionTest (sig, need_value := TRUE);
  END GenClosureCall;

PROCEDURE CouldBeClosure (proc: Expr.T): BOOLEAN =
  VAR name: M3ID.T;  value: Value.T;
  BEGIN
    RETURN (NamedExpr.Split (proc, name, value))
        AND (Value.ClassOf (value) = Value.Class.Var)
        AND (Variable.HasClosure (value));
  END CouldBeClosure;

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

PROCEDURE Initialize () =
  BEGIN
    Methods := CallExpr.NewMethodList (0, 99999, FALSE, TRUE, TRUE, NIL,
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
  END Initialize;

BEGIN
END UserProc.
