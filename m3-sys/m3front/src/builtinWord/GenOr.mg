(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: GenOr.mg                                              *)
(* Last Modified On Mon Dec  5 15:30:44 PST 1994 By kalsow     *)
(*      Modified On Tue Apr 10 11:16:07 1990 By muller         *)

GENERIC MODULE GenOr (Rep, Plus);

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, ProcType;
IMPORT IntegerExpr, Value, Formal, Target, TWord;
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
    CG.Or (Rep.signed);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR w0, w1, result: Target.Int;
  BEGIN
    IF Plus.GetArgs (ce.args, w0, w1) THEN
      TWord.Or (w0, w1, result);
      RETURN IntegerExpr.New (T, result);
    END;
    RETURN NIL;
  END Fold;

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
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("Or", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END GenOr.
