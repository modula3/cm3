(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordXor.m3                                            *)
(* Last Modified On Mon Dec  5 15:30:41 PST 1994 By kalsow     *)
(*      Modified On Tue Apr 10 11:19:38 1990 By muller         *)

MODULE WordXor;

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
    CG.Xor ();
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR w0, w1, result: Target.Int;
  BEGIN
    IF WordPlus.GetArgs (ce.args, w0, w1)
      THEN w0 := TWord.Trim (w0); w1 := TWord.Trim (w1);
           TWord.Xor (w0, w1, result);
           RETURN IntegerExpr.New (TWord.Trim(result));
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
    Procedure.Define ("Xor", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END WordXor.
