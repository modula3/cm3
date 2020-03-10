(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Load.mg                                               *)

GENERIC MODULE Load (Rep, Atomic);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TInt, M3ID;
IMPORT Value, Formal, Type, ProcType, Error, EnumExpr;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR order: Target.Int;  t: Type.T;  z: INTEGER;
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    IF EnumExpr.Split (ce.args[1], order, t) AND TInt.ToInt (order, z) THEN
      CASE z OF
      | ORD(CG.MemoryOrder.Release) =>
        Error.Msg ("Order.Release not allowed on Load");
      | ORD(CG.MemoryOrder.AcquireRelease) =>
        Error.Msg ("Order.AcquireRelease not allowed on Load");
      | ORD(CG.MemoryOrder.Relaxed) =>
        (* ok *)
      ELSE
        Error.Warn (0, "Load currently only supports Order.Relaxed");
      END;
    ELSE
      Error.Msg ("order must be an enumeration constant");
    END;
    ce.type := Rep.T;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.PrepLValue (ce.args[0], traced := TRUE);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR order: Target.Int;  z: INTEGER;  t: Type.T;
  BEGIN
    Expr.CompileAddress (ce.args[0], traced := TRUE);
    EVAL EnumExpr.Split (ce.args[1], order, t);
    EVAL TInt.ToInt (order, z);
    CG.Load_ordered (Type.CGType(Rep.T, in_memory := TRUE),
                     order := VAL(z, CG.MemoryOrder));
  END Compile;

PROCEDURE Initialize () =
  VAR
    var := Formal.Info { name := M3ID.Add ("var"),
                         mode := Formal.Mode.mCONST,
                         offset := 0,
                         type := Atomic.T,
                         dfault := NIL,
                         unused := FALSE,
                         trace := NIL };
    order := Formal.Info { name := M3ID.Add ("order"),
                           mode := Formal.Mode.mVALUE,
                           offset := 1,
                           type := Atomic.Order,
                           dfault := Atomic.Sequential,
                           unused := FALSE,
                           trace := NIL };
    t0 := ProcType.New (Rep.T, Formal.New (var), Formal.New (order));
  BEGIN
    Z := CallExpr.NewMethodList (1, 2, FALSE, TRUE, TRUE, Rep.T,
                                 NIL, NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
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
    Procedure.DefinePredefined ("Load", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END Load.
