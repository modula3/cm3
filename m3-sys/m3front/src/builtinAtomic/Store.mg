(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Store.mg                                              *)

GENERIC MODULE Store (Rep, Atomic);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TInt, M3ID;
IMPORT Value, Formal, Type, ProcType, Error, EnumExpr;
IMPORT MSIR, MSIRBuilder;

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

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  VAR
    ptr       := Expr.LValueMSIR (ce.args[0]);
    container := MSIRBuilder.TakePendingContainer ();
    val       := Expr.CompileMSIR (ce.args[1]);
    b         := MSIRBuilder.CurrentBlock ();
    elemT     : MSIR.T;  atomT : MSIR.T;
    order     : Target.Int;  t: Type.T;  z: INTEGER;  ord: MSIR.MemOrder;
  BEGIN
    IF ptr = NIL OR val = NIL THEN RETURN NIL END;
    elemT := MSIR.ValueType(val);
    atomT := elemT;
    IF MSIR.BitWidth(elemT) > 0 AND MSIR.BitWidth(elemT) < 8 THEN
      atomT := MSIR.TI(8);
      val := MSIR.BuildZExt(b, "", val, atomT);
    END;
    IF EnumExpr.Split(ce.args[2], order, t) AND TInt.ToInt(order, z)
      THEN ord := VAL(z, MSIR.MemOrder);
      ELSE ord := MSIR.MemOrder.SeqCst;
    END;
    MSIR.BuildAtomicStore(b, val, ptr, ord, container);
    RETURN NIL;
  END CompileMSIR;

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
    CallExpr.SetMethodMSIR (Z, CompileMSIR);
    Procedure.DefinePredefined ("Store", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END Store.
