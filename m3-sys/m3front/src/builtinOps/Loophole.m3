(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Loophole.m3                                           *)
(* Last Modified On Tue May  3 16:36:31 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:57:40 1990 By muller         *)

MODULE Loophole;

IMPORT CallExpr, Expr, ExprRep, Type, Procedure, CastExpr;
IMPORT Int, Module, TypeExpr, Error;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T := Int.T;
  BEGIN
    IF TypeExpr.Split (ce.args[1], t) THEN (*ok*) END;
    RETURN t;
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR t: Type.T;
  BEGIN
    IF NOT TypeExpr.Split (ce.args[1], t) THEN
      Error.Msg ("LOOPHOLE: second argument must be a type");
      t := Int.T;
    END;
    ce.args[0] := CastExpr.New (ce.args[0], t);
    IF Module.IsSafe () THEN Error.Msg ("LOOPHOLE: unsafe operation") END;
    Expr.TypeCheck (ce.args[0], cs);
    ce.type := t;
  END Check;

PROCEDURE NeedsAddress (ce: CallExpr.T) =
  BEGIN
    Expr.NeedsAddress (ce.args[0]);
  END NeedsAddress;

PROCEDURE LoopholeExprAlign (ce: CallExpr.T): Type.BitAlignT =
  BEGIN
    RETURN Expr.Alignment(ce.args[0]);
  END LoopholeExprAlign;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.Prep (ce.args[0]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.Compile (ce.args[0]);
  END Compile;

PROCEDURE PrepLV (ce: CallExpr.T; traced: BOOLEAN) =
  BEGIN
    Expr.PrepLValue (ce.args[0], traced);
  END PrepLV;

PROCEDURE CompileLV (ce: CallExpr.T; traced: BOOLEAN) =
  BEGIN
    Expr.CompileLValue (ce.args[0], traced);
  END CompileLV;

PROCEDURE IsWritable (ce: CallExpr.T;  lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN Expr.IsWritable (ce.args[0], lhs);
  END IsWritable;

PROCEDURE IsDesignator (ce: CallExpr.T;  <*UNUSED*> lhs: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN Expr.IsDesignator (ce.args[0]);
  END IsDesignator;

PROCEDURE NoteWrites (ce: CallExpr.T) =
  BEGIN
    Expr.NoteWrite (ce.args[0]);
  END NoteWrites;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (2, 2, TRUE, FALSE, TRUE, NIL,
                                 TypeOf,
                                 TypeOf,
                                 NeedsAddress,
                                 Check,
                                 Prep,
                                 Compile,
                                 PrepLV,
                                 CompileLV,
                                 CallExpr.PrepNoBranch,
                                 CallExpr.NoBranch,
                                 CallExpr.NoValue, (*fold*)
                                 CallExpr.NoBounds,
                                 IsWritable,
                                 IsDesignator,
                                 NoteWrites,
                                 LoopholeExprAlign);
    Procedure.DefinePredefined ("LOOPHOLE", Z, TRUE);
  END Initialize;

BEGIN
END Loophole.
