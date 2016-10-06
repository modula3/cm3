(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Fence.mg                                              *)

GENERIC MODULE Fence (Atomic);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TInt, M3ID;
IMPORT Value, Formal, Type, ProcType, Error, EnumExpr;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR order: Target.Int;  t: Type.T;  z: INTEGER;
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    IF EnumExpr.Split (ce.args[0], order, t) AND TInt.ToInt (order, z) THEN
      CASE z OF
      | ORD(CG.MemoryOrder.Sequential) =>
        (* ok *)
      ELSE
        Error.Warn (0, "Fence currently only supports Order.Sequential");
      END;
    ELSE
      Error.Msg ("order must be an enumeration constant");
    END;
    ce.type := NIL;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR order: Target.Int;  t: Type.T;  z: INTEGER;
  BEGIN
    EVAL EnumExpr.Split (ce.args[0], order, t);
    EVAL TInt.ToInt (order, z);
    CG.Fence (order := VAL(z, CG.MemoryOrder));
  END Compile;

PROCEDURE Initialize () =
  VAR
    order := Formal.Info { name := M3ID.Add ("order"),
                           mode := Formal.Mode.mVALUE,
                           offset := 0,
                           type := Atomic.Order,
                           dfault := Atomic.Sequential,
                           unused := FALSE,
                           trace := NIL };
    t0 := ProcType.New (NIL, Formal.New (order));
  BEGIN
    Z := CallExpr.NewMethodList (0, 1, FALSE, TRUE, TRUE, NIL,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 CallExpr.PrepArgs,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 CallExpr.NoValue,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.DefinePredefined ("Fence", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END Fence.
