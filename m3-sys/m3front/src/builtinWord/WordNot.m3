(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: WordNot.m3                                            *)
(* Last Modified On Mon Dec  5 15:30:45 PST 1994 By kalsow     *)
(*      Modified On Tue Apr 10 11:15:33 1990 By muller         *)

MODULE WordNot;

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, Target, TWord;
IMPORT Int, IntegerExpr, Value, Formal, ProcType;

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
    CG.Not ();
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e: Expr.T;  w, result: Target.Int;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF (e # NIL) AND IntegerExpr.Split (e, w)
      THEN TWord.Not (TWord.Trim(w), result);
           RETURN IntegerExpr.New (TWord.Trim(result));
      ELSE RETURN NIL;
    END;
  END Fold;

PROCEDURE Initialize () =
  VAR
    f0 := Formal.NewBuiltin ("x", 0, Int.T);
    t  := ProcType.New (Int.T, f0);
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, TRUE, TRUE, Int.T,
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
    Procedure.Define ("Not", Z, FALSE, t);
    formals := ProcType.Formals (t);
  END Initialize;

BEGIN
END WordNot.
