(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordDivide.m3                                         *)
(* Last Modified On Mon Dec  5 15:30:53 PST 1994 By kalsow     *)
(*      Modified On Tue Apr 10 11:07:52 1990 By muller         *)

MODULE WordDivide;

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, ProcType;
IMPORT Int, IntegerExpr, WordPlus, Value, Formal, Target, TWord;

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
    CG.Div (Target.Word.cg_type, Expr.GetSign (ce.args[0]),
                                 Expr.GetSign (ce.args[1]));
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR w0, w1, result: Target.Int;
  BEGIN
    IF WordPlus.GetArgs (ce.args, w0, w1) AND TWord.Div (w0, w1, result)
      THEN RETURN IntegerExpr.New (result);
      ELSE RETURN NIL;
    END;
  END Fold;

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
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("Divide", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END WordDivide.
