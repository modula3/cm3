(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Store.mg                                              *)

GENERIC MODULE Store (Rep, Atomic);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TInt, M3ID;
IMPORT Value, Formal, Type, ProcType, Error, EnumExpr;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR order: Target.Int;  t: Type.T;  z: INTEGER;
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    IF EnumExpr.Split (ce.args[2], order, t) AND TInt.ToInt (order, z) THEN
      CASE z OF
      | ORD(CG.MemoryOrder.Acquire) =>
        Error.Msg ("Order.Acquire not allowed on Store");
      | ORD(CG.MemoryOrder.AcquireRelease) =>
        Error.Msg ("Order.AcquireRelease not allowed on Store");
      | ORD(CG.MemoryOrder.Relaxed) =>
        (* ok *)
      ELSE
        Error.Warn (0, "Store currently only supports Order.Relaxed");
      END;
    ELSE
      Error.Msg ("order must be an enumeration constant");
    END;
    ce.type := NIL;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.PrepLValue (ce.args[0], traced := TRUE);
    Expr.Prep (ce.args[1]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR order: Target.Int;  z: INTEGER;  t: Type.T;
  BEGIN
    Expr.CompileAddress (ce.args[0], traced := TRUE);
    Expr.Compile (ce.args[1]);
    EVAL EnumExpr.Split (ce.args[2], order, t);
    EVAL TInt.ToInt (order, z);
    CG.Store_ordered (Type.CGType(Rep.T, in_memory := TRUE),
                      order := VAL(z, CG.MemoryOrder));
    Expr.NoteWrite (ce.args[0]);
  END Compile;

PROCEDURE Initialize () =
  VAR
    var := Formal.Info { name := M3ID.Add ("var"),
                         mode := Formal.Mode.mVAR,
                         offset := 0,
                         type := Atomic.T,
                         dfault := NIL,
                         unused := FALSE,
                         trace := NIL };
    val := Formal.NewBuiltin ("val", 1, Rep.T);
    order := Formal.Info { name := M3ID.Add ("order"),
                           mode := Formal.Mode.mVALUE,
                           offset := 2,
                           type := Atomic.Order,
                           dfault := Atomic.Sequential,
                           unused := FALSE,
                           trace := NIL };
    t0 := ProcType.New (NIL, Formal.New (var), val, Formal.New (order));
  BEGIN
    Z := CallExpr.NewMethodList (2, 3, FALSE, TRUE, TRUE, NIL,
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
    Procedure.DefinePredefined ("Store", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END Store.
