(* Copyright 1996-2000, Critical Mass, Inc.  All rights reserved. *)
(* See file COPYRIGHT-CMASS for details. *)

MODULE InfoThisFile;

IMPORT CallExpr, Expr, ExprRep, Procedure, ProcType, Textt;
IMPORT Formal, Value, Scanner, TextExpr, M3String;
IMPORT Target;

VAR Z: CallExpr.MethodList;
VAR formals: Value.T;
VAR value: Expr.T := NIL;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    EVAL Formal.CheckArgs (cs, ce.args, formals, ce.proc);
    ce.type := Textt.T;
  END Check;

PROCEDURE Compile (<*UNUSED*> ce: CallExpr.T) =
  BEGIN
    GetValue ();
    Expr.Prep (value);
    Expr.Compile (value);
  END Compile;

PROCEDURE Fold (<*UNUSED*> ce: CallExpr.T): Expr.T =
  BEGIN
    GetValue ();
    RETURN value;
  END Fold;

PROCEDURE Initialize () =
  VAR t0 := ProcType.New (Textt.T);
  BEGIN
    Z := CallExpr.NewMethodList (0, 0, TRUE, TRUE, TRUE, Textt.T,
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
    Procedure.DefinePredefined ("ThisFile", Z, FALSE, t0);
    formals := ProcType.Formals (t0);
  END Initialize;

PROCEDURE Reset () =
  BEGIN
    value := NIL;
  END Reset;

PROCEDURE GetValue () =
  VAR file: TEXT;  line: INTEGER;
  BEGIN
    IF (value = NIL) THEN
      Scanner.LocalHere (file, line);
      file := Target.CleanupSourcePath (file);
      value := TextExpr.New8 (M3String.Add (file));
    END;
  END GetValue;

BEGIN
END InfoThisFile.
