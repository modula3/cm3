(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE InfoThisException;

IMPORT CallExpr, Expr, ExprRep, Procedure, ProcType, Addr;
IMPORT Formal, Value, TryStmt, Error;
IMPORT MSIR, MSIRBuilder;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    IF NOT TryStmt.InHandler() THEN
      Error.Msg ("Compiler.ThisException: must be called from a TRY handler");
    END;
    ce.type := Addr.T;
  END Check;

PROCEDURE Compile (<*UNUSED*> ce: CallExpr.T) =
  BEGIN
    TryStmt.LoadInfoPtr ();
  END Compile;

PROCEDURE CompileMSIR (<*UNUSED*> ce: CallExpr.T): MSIR.Value =
  (* Return the activation pointer (RT0.ActivationPtr as ADDRESS) of the
     exception being handled in the enclosing TRY handler.  TryStmt pushes it
     onto the catch-context stack when it enters each handler body. *)
  VAR act := MSIRBuilder.CurrentCatchActPtr ();
  BEGIN
    IF act = NIL THEN
      MSIRBuilder.Abandon ("Compiler.ThisException: no handler activation");
      RETURN NIL;
    END;
    RETURN act;
  END CompileMSIR;

PROCEDURE Initialize () =
  VAR t0 := ProcType.New (Addr.T);
  BEGIN
    Z := CallExpr.NewMethodList (0, 0, TRUE, TRUE, TRUE, Addr.T,
                                 NIL, NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 CallExpr.PrepArgs,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 CallExpr.NoValue,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    CallExpr.SetMethodMSIR (Z, CompileMSIR);
    Procedure.DefinePredefined ("ThisException", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

BEGIN
END InfoThisException.
