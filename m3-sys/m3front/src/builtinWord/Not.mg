(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC MODULE Not (Rep);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TWord;
IMPORT IntegerExpr, Value, Formal, Type, ProcType, TInt;
IMPORT MSIR, MSIRBuilder;
FROM Rep IMPORT T;
FROM TargetMap IMPORT Integer_types;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;
VAR rep: [FIRST (Integer_types) .. LAST (Integer_types)];

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := T;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    CG.Not (Integer_types[rep].cg_type);
  END Compile;

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  VAR
    x := Expr.CompileMSIR (ce.args[0]);
    b := MSIRBuilder.CurrentBlock ();
    t : MSIR.T;
  BEGIN
    IF x = NIL THEN RETURN NIL END;
    t := MSIR.ValueType (x);
    RETURN MSIR.BuildIXor (b, "", x, MSIR.ConstInt (t, -1));
  END CompileMSIR;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e: Expr.T;  w, result: Target.Int;  t: Type.T;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF (e # NIL) AND IntegerExpr.Split (e, w, t) THEN
      TWord.Not (w, result);
      EVAL TInt.Extend (result, Integer_types[rep].bytes, result);
      RETURN IntegerExpr.New (T, result);
    END;
    RETURN NIL;
  END Fold;

PROCEDURE Initialize (r: INTEGER) =
  VAR
    f0 := Formal.NewBuiltin ("x", 0, T);
    t  := ProcType.New (T, f0);
  BEGIN
    rep := r;
    Z := CallExpr.NewMethodList (1, 1, TRUE, TRUE, TRUE, T,
                                 NIL, NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 CallExpr.PrepArgs,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 Fold,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    CallExpr.SetMethodMSIR (Z, CompileMSIR);
    Procedure.DefinePredefined ("Not", Z, FALSE, t, assignable:=TRUE);
    formals := ProcType.Formals (t);
  END Initialize;

BEGIN
END Not.
