(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: GenAnd.mg                                             *)
(* Last Modified On Mon Dec  5 15:19:35 PST 1994 By kalsow     *)
(*      Modified On Tue Apr 10 11:06:36 1990 By muller         *)

GENERIC MODULE GenAnd (Rep);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Type, ProcType;
IMPORT IntegerExpr, Formal, Value, Target, TWord, TInt;
FROM Rep IMPORT T;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := T;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    CG.And (Rep.Signed);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR w0, w1, result: Target.Int;
  BEGIN
    IF GetArgs (ce.args, w0, w1) THEN
      TWord.And (w0, w1, result);
      RETURN IntegerExpr.New (T, result);
    END;
    RETURN NIL;
  END Fold;

PROCEDURE GetArgs (args: Expr.List;  VAR w0, w1: Target.Int): BOOLEAN =
  VAR e0, e1: Expr.T;  t: Type.T;
  BEGIN
    e0 := Expr.ConstValue (args[0]);
    e1 := Expr.ConstValue (args[1]);
    RETURN (e0 # NIL) AND IntegerExpr.Split (e0, w0, t) AND 
           (e1 # NIL) AND IntegerExpr.Split (e1, w1, t);
  END GetArgs;

PROCEDURE GetBounds (ce: CallExpr.T;  VAR min, max: Target.Int) =
  VAR min_a, max_a, min_b, max_b : Target.Int;
  BEGIN
    Expr.GetBounds (ce.args[0], min_a, max_a);
    Expr.GetBounds (ce.args[1], min_b, max_b);
    IF TInt.LT (min_a, TInt.Zero) OR TInt.LT (max_a, TInt.Zero) THEN
      (* "a" could be 16_ffff...  => any bits from "b" can survive *)
      IF TInt.LT (min_b, TInt.Zero) OR TInt.LT (max_b, TInt.Zero) THEN
        (* too complicated *)
        EVAL Type.GetBounds (T, min, max);
      ELSE
        (* "b" is non-negative, but "a" could be 16_ffff... *)
        min := TInt.Zero;  (* no bits in common *)
        max := max_b;
      END;
    ELSIF TInt.LT (min_b, TInt.Zero) OR TInt.LT (max_b, TInt.Zero) THEN
      (* "a" is non-negative, but "b" could be 16_ffff... *)
      min := TInt.Zero;  (* no bits in common *)
      max := max_a;
    ELSE
      (* both a and b are non-negative *)
      min := TInt.Zero;  (* no bits in common *)
      TWord.And (max_a, max_b, max);
    END;
  END GetBounds;

PROCEDURE Initialize () =
  VAR
    x0 := Formal.NewBuiltin ("x", 0, T);
    y0 := Formal.NewBuiltin ("y", 1, T);
    t0 := ProcType.New (T, x0, y0);
  BEGIN
    Z := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, T,
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
                                 GetBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("And", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END GenAnd.
