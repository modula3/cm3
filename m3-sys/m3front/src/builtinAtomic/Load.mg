(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Load.mg                                               *)

GENERIC MODULE Load (Rep, Atomic);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TInt, M3ID;
IMPORT Value, Formal, Type, ProcType, Error, EnumExpr;
IMPORT MSIR, MSIRBuilder, MSIRType;

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

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  VAR
    ptr    := Expr.LValueMSIR (ce.args[0]);
    b      := MSIRBuilder.CurrentBlock ();
    elemT  := MSIRType.Translate (Rep.T);
    atomT  := elemT;
    order  : Target.Int;  t: Type.T;  z: INTEGER;  ord: MSIR.MemOrder;
    loaded : MSIR.Value;
  BEGIN
    EVAL MSIRBuilder.TakePendingContainer (); (* clear pending; read-only op *)
    IF ptr = NIL THEN RETURN NIL END;
    IF MSIR.BitWidth(elemT) > 0 AND MSIR.BitWidth(elemT) < 8 THEN atomT := MSIR.TI(8) END;
    IF EnumExpr.Split(ce.args[1], order, t) AND TInt.ToInt(order, z)
      THEN ord := VAL(z, MSIR.MemOrder);
      ELSE ord := MSIR.MemOrder.SeqCst;
    END;
    loaded := MSIR.BuildAtomicLoad(b, "", atomT, ptr, ord);
    IF atomT # elemT THEN
      RETURN MSIR.BuildTrunc(b, "", loaded, elemT);
    END;
    RETURN loaded;
  END CompileMSIR;

PROCEDURE Initialize () =
  VAR
    var := Formal.Info { name := M3ID.Add ("var"),
                         mode := Formal.Mode.mREADONLY,
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
    CallExpr.SetMethodMSIR (Z, CompileMSIR);
    Procedure.DefinePredefined ("Load", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END Load.
