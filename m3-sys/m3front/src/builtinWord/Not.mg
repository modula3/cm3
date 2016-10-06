(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

GENERIC MODULE Not (Rep);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TWord;
IMPORT IntegerExpr, Value, Formal, Type, ProcType, TInt;
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
                                 NIL,
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
    Procedure.DefinePredefined ("Not", Z, FALSE, t, assignable:=TRUE);
    formals := ProcType.Formals (t);
  END Initialize;

BEGIN
END Not.
