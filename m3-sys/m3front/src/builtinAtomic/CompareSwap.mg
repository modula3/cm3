(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CompareSwap.mg                                        *)

GENERIC MODULE CompareSwap (Rep, Atomic);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TInt, M3ID;
IMPORT Value, Formal, Type, ProcType, Error, EnumExpr, Bool;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR order: Target.Int;  t: Type.T;  success, failure: INTEGER;
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    IF EnumExpr.Split (ce.args[3], order, t) AND TInt.ToInt (order, success) THEN
      CASE success OF
      | ORD(CG.MemoryOrder.Sequential) =>
        (* ok *)
      ELSE
        Error.Warn (0, "CompareSwap currently only supports Order.Sequential");
      END;
    ELSE
      success := ORD(CG.MemoryOrder.Sequential);
      Error.Msg ("success must be an enumeration constant");
    END;
    IF EnumExpr.Split (ce.args[4], order, t) AND TInt.ToInt (order, failure) THEN
      CASE failure OF
      | ORD(CG.MemoryOrder.Release) =>
        Error.Msg ("Order.Release not allowed for failure");
      | ORD(CG.MemoryOrder.AcquireRelease) =>
        Error.Msg ("Order.AcquireRelease not allowed for failure");
      | ORD(CG.MemoryOrder.Sequential) =>
        (* ok *)
      ELSE
        Error.Warn (0, "CompareSwap currently only supports Order.Sequential");
      END;
    ELSE
      failure := ORD(CG.MemoryOrder.Sequential);
      Error.Msg ("failure must be an enumeration constant");
    END;
    IF failure > success THEN
      Error.Msg ("failure is stronger than success");
    END;
    ce.type := Bool.T;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.PrepLValue (ce.args[0], traced := TRUE);
    Expr.PrepLValue (ce.args[1], traced := TRUE);
    Expr.Prep (ce.args[2]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR order: Target.Int;  t: Type.T;  success, failure: INTEGER;
  BEGIN
    Expr.CompileAddress (ce.args[0], traced := TRUE);
    CG.Force ();
    Expr.CompileLValue (ce.args[1], traced := TRUE);
    Expr.Compile (ce.args[2]);
    EVAL EnumExpr.Split (ce.args[3], order, t);
    EVAL TInt.ToInt (order, success);
    EVAL EnumExpr.Split (ce.args[4], order, t);
    EVAL TInt.ToInt (order, failure);
    CG.Compare_exchange (Type.CGType(Rep.T, in_memory := TRUE),
                         success := VAL(success, CG.MemoryOrder),
                         failure := VAL(failure, CG.MemoryOrder));
    Expr.NoteWrite (ce.args[0]);
    Expr.NoteWrite (ce.args[1]);
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
    expected := Formal.Info { name := M3ID.Add ("expected"),
                              mode := Formal.Mode.mVAR,
                              offset := 1,
                              type := Rep.T,
                              dfault := NIL,
                              unused := FALSE,
                              trace := NIL };
    desired := Formal.NewBuiltin ("desired", 2, Rep.T);
    success := Formal.Info { name := M3ID.Add ("success"),
                             mode := Formal.Mode.mVALUE,
                             offset := 3,
                             type := Atomic.Order,
                             dfault := Atomic.Sequential,
                             unused := FALSE,
                             trace := NIL };
    failure := Formal.Info { name := M3ID.Add ("failure"),
                             mode := Formal.Mode.mVALUE,
                             offset := 4,
                             type := Atomic.Order,
                             dfault := Atomic.Sequential,
                             unused := FALSE,
                             trace := NIL };
    t0 := ProcType.New (Bool.T, Formal.New (var), Formal.New (expected),
                        desired, Formal.New (success), Formal.New (failure));
  BEGIN
    Z := CallExpr.NewMethodList (3, 5, FALSE, TRUE, TRUE, Bool.T,
                                 NIL,
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
    Procedure.DefinePredefined ("CompareSwap", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END CompareSwap.
