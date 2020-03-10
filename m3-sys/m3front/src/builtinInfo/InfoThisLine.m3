(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE InfoThisLine;

IMPORT CG, CallExpr, Expr, ExprRep, Procedure, ProcType;
IMPORT Int, Card, IntegerExpr, Formal, Value, Target, TInt, Scanner;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := Card.T;
  END Check;

PROCEDURE Compile (<*UNUSED*> ce: CallExpr.T) =
  VAR file: TEXT;  line: INTEGER;
  BEGIN
    Scanner.Here (file, line);
    line := MAX (0,  line);
    CG.Load_intt (line);
  END Compile;

PROCEDURE Fold (<*UNUSED*> ce: CallExpr.T): Expr.T =
  VAR file: TEXT;  line: INTEGER;  b: BOOLEAN;  val: Target.Int;
  BEGIN
    Scanner.Here (file, line);
    line := MAX (0,  line);
    b := TInt.FromInt (line, val);  <*ASSERT b*>
    RETURN IntegerExpr.New (Int.T, val);
  END Fold;

PROCEDURE Initialize () =
  VAR t0 := ProcType.New (Card.T);
  BEGIN
    Z := CallExpr.NewMethodList (0, 0, TRUE, TRUE, TRUE, Card.T,
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
    Procedure.DefinePredefined ("ThisLine", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END InfoThisLine.
