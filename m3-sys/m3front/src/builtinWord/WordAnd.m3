(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordAnd.m3                                            *)
(* Last Modified On Mon Dec  5 15:19:35 PST 1994 By kalsow     *)
(*      Modified On Tue Apr 10 11:06:36 1990 By muller         *)

MODULE WordAnd;

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, ProcType;
IMPORT Int, IntegerExpr, Formal, Value, WordPlus, Target, TWord, TInt;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := Int.T;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
    Expr.Compile (ce.args[1]);
    CG.And ();
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR w0, w1, result: Target.Int;
  BEGIN
    IF WordPlus.GetArgs (ce.args, w0, w1)
      THEN w0 := TWord.Trim (w0); w1 := TWord.Trim (w1);
           TWord.And (w0, w1, result); RETURN IntegerExpr.New (result);
      ELSE RETURN NIL;
    END;
  END Fold;

PROCEDURE GetBounds (ce: CallExpr.T;  VAR min, max: Target.Int) =
  VAR min_a, max_a, min_b, max_b : Target.Int;
  BEGIN
    Expr.GetBounds (ce.args[0], min_a, max_a);
    Expr.GetBounds (ce.args[1], min_b, max_b);
    (* 
    min_a := TInt.Trim (min_a);
    max_a := TInt.Trim (max_a);
    min_b := TInt.Trim (min_b);
    max_b := TInt.Trim (max_b);
    *)
    IF TInt.LT (min_a, TInt.Zero) OR TInt.LT (max_a, TInt.Zero) THEN
      (* "a" could be 16_ffff...  => any bits from "b" can survive *)
      IF TInt.LT (min_b, TInt.Zero) OR TInt.LT (max_b, TInt.Zero) THEN
        (* too complicated *)
        min := Target.Integer.min;
        max := Target.Integer.max;
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
    min := TInt.Trim (min);
    max := TInt.Trim (max);
  END GetBounds;

PROCEDURE Initialize () =
  VAR
    x0 := Formal.NewBuiltin ("x", 0, Int.T);
    y0 := Formal.NewBuiltin ("y", 1, Int.T);
    t0 := ProcType.New (Int.T, x0, y0);
  BEGIN
    Z := CallExpr.NewMethodList (2, 2, TRUE, TRUE, TRUE, Int.T,
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
END WordAnd.
