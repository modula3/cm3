(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: IsLockFree.mg                                         *)

GENERIC MODULE IsLockFree (Rep);

IMPORT CallExpr, Expr, ExprRep, Procedure, Target;
IMPORT Value, Formal, Type, ProcType, Bool;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := Bool.T;
  END Check;

PROCEDURE Compile (<*UNUSED*> ce: CallExpr.T) =
  BEGIN
    IF Target.Atomic_lock_free[Type.CGType(Rep.T, in_memory := TRUE)]
      THEN Value.Load (Bool.True);
      ELSE Value.Load (Bool.False);
    END;
  END Compile;

PROCEDURE Fold (<*UNUSED*> ce: CallExpr.T): Expr.T =
  VAR bool := Target.Atomic_lock_free[Type.CGType(Rep.T, in_memory := TRUE)];
  BEGIN
    RETURN Bool.Map[bool];
  END Fold;

PROCEDURE Initialize () =
  VAR t0 := ProcType.New (Bool.T);
  BEGIN
    Z := CallExpr.NewMethodList (0, 0, TRUE, FALSE, FALSE, Bool.T,
                                 NIL, NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 CallExpr.PrepArgs,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.PrepNoBranch,
                                 CallExpr.NoBranch,
                                 Fold,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.DefinePredefined ("IsLockFree", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END IsLockFree.
