(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: FetchInc.mg                                           *)

GENERIC MODULE FetchInc (Rep, Atomic);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TInt, M3ID;
IMPORT Value, Formal, Type, ProcType, Error, EnumExpr, Addr, Module;
IMPORT IntegerExpr, Int, LInt;
IMPORT MSIR, MSIRBuilder, MSIRType;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR order: Target.Int;  t: Type.T;  z: INTEGER;
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    t := Type.Base (Rep.T);
    IF NOT Type.IsOrdinal (t) THEN
      IF Type.IsSubtype (t, Addr.T) THEN
        IF Module.Name (Module.Current ()) = M3ID.Add ("AtomicAddress") THEN
          (* allow it *)
        ELSIF Module.IsSafe () THEN Error.Msg ("unsafe operation") END;
      ELSIF Module.Name (Module.Current ()) = M3ID.Add ("AtomicRefany") THEN
        (* generate a run-time error *)
      ELSE
        Error.Msg ("first argument must be of an ordinal type");
      END;
    ELSIF EnumExpr.Split (ce.args[2], order, t) AND TInt.ToInt (order, z) THEN
      CASE z OF
      | ORD(CG.MemoryOrder.Sequential) =>
        (* ok *)
      ELSE
        Error.Warn (0, "FetchInc currently only supports Order.Sequential");
      END;
    ELSE
      Error.Msg ("order must be an enumeration constant");
    END;
    ce.type := Rep.T;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.PrepLValue (ce.args[0], traced := TRUE);
    Expr.Prep (ce.args[1]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR order: Target.Int;  t: Type.T;  z: INTEGER;
  BEGIN
    IF Module.Name (Module.Current ()) = M3ID.Add ("AtomicRefany") THEN
      (* generate a run-time error *)
      CG.Abort (CG.RuntimeError.ValueOutOfRange);
    END;
    Expr.CompileAddress (ce.args[0], traced := TRUE);
    Expr.Compile (ce.args[1]);
    t := Type.Base (Rep.T);
    IF Type.IsSubtype (t, LInt.T) OR NOT Type.IsOrdinal (t) THEN 
      CG.Loophole (Target.Integer.cg_type, Type.CGType(Rep.T));
    END;
    EVAL EnumExpr.Split (ce.args[2], order, t);
    EVAL TInt.ToInt (order, z);
    CG.Fetch_and_op (CG.AtomicOp.Add,
                     Type.CGType(Rep.T, in_memory := TRUE),
                     order := VAL(z, CG.MemoryOrder));
    Expr.NoteWrite (ce.args[0]);
  END Compile;

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  VAR
    ptr   := Expr.LValueMSIR (ce.args[0]);
    incr  := Expr.CompileMSIR (ce.args[1]);
    b     := MSIRBuilder.CurrentBlock ();
    elemT := MSIRType.Translate (Rep.T);
    atomT := elemT;
    incrT : MSIR.T;
    order : Target.Int;  t: Type.T;  z: INTEGER;  ord: MSIR.MemOrder;
    old   : MSIR.Value;
  BEGIN
    EVAL MSIRBuilder.TakePendingContainer ();
    IF ptr = NIL OR incr = NIL THEN RETURN NIL END;
    IF MSIR.BitWidth(elemT) > 0 AND MSIR.BitWidth(elemT) < 8 THEN atomT := MSIR.TI(8) END;
    (* Convert incr (Integer.T = i64) to atomT *)
    incrT := MSIR.ValueType(incr);
    IF NOT MSIR.Equal(incrT, atomT) THEN
      IF MSIR.BitWidth(incrT) > MSIR.BitWidth(atomT) THEN
        incr := MSIR.BuildTrunc(b, "", incr, atomT);
      ELSE
        incr := MSIR.BuildZExt(b, "", incr, atomT);
      END;
    END;
    IF EnumExpr.Split(ce.args[2], order, t) AND TInt.ToInt(order, z)
      THEN ord := VAL(z, MSIR.MemOrder);
      ELSE ord := MSIR.MemOrder.SeqCst;
    END;
    old := MSIR.BuildAtomicRMW(b, "", MSIR.AtomicRMWOp.Add, ptr, incr, ord);
    IF atomT # elemT THEN
      RETURN MSIR.BuildTrunc(b, "", old, elemT);
    END;
    RETURN old;
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
    incr := Formal.Info { name := M3ID.Add ("incr"),
                          mode := Formal.Mode.mVALUE,
                          offset := 1,
                          type := Int.T,
                          dfault := IntegerExpr.New (Int.T, TInt.One),
                          unused := FALSE,
                          trace := NIL };
    order := Formal.Info { name := M3ID.Add ("order"),
                           mode := Formal.Mode.mVALUE,
                           offset := 2,
                           type := Atomic.Order,
                           dfault := Atomic.Sequential,
                           unused := FALSE,
                           trace := NIL };
    t0 := ProcType.New (Rep.T, Formal.New (var), Formal.New (incr),
                        Formal.New (order));
  BEGIN
    Z := CallExpr.NewMethodList (1, 3, FALSE, TRUE, TRUE, Rep.T,
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
    Procedure.DefinePredefined ("FetchInc", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END FetchInc.
