(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Cas.m3                                                *)
(* Last Modified On Wed Jun 29 17:01:08 PDT 1994 By kalsow     *)
(*      Modified On Tue Feb 12 11:52:16 1991 By muller         *)

MODULE Cas;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure;
IMPORT Error, Target, ErrType, Int, LInt;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;
  BEGIN
    t := Expr.TypeOf (ce.args[0]);
    t := Type.Check (t);
    RETURN Type.Base (t);
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T; <*UNUSED*> VAR cs: Expr.CheckState) =
  BEGIN
    DoCheck ("CAS", ce);
  END Check;

PROCEDURE DoCheck (name: TEXT; ce: CallExpr.T) =
  VAR t: Type.T; e: Expr.T;
  BEGIN
    e := ce.args[0];
    t := Expr.TypeOf (e);
    IF NOT (Type.IsEqual (t, Int.T, NIL) OR Type.IsEqual (t, LInt.T, NIL)) THEN
      Error.Txt (name, "first argument must have integer type");
    ELSIF (NOT Expr.IsDesignator (e)) THEN
      Error.Txt (name, "first argument must be a variable");
    ELSIF (NOT Expr.IsWritable (e, lhs := TRUE)) THEN
      Error.Txt (name, "first argument must be writable");
    ELSE
      Expr.NeedsAddress (e);
    END;
    ce.type := t;
    t := Expr.TypeOf (ce.args[1]);
    IF NOT Type.IsAssignable (t, ce.type) THEN
      IF t # ErrType.T AND ce.type # ErrType.T THEN
        Error.Txt (name, "second argument is not assignable to first");
      END;
    END;
    t := Expr.TypeOf (ce.args[2]);
    IF NOT Type.IsAssignable (t, ce.type) THEN
      IF t # ErrType.T AND ce.type # ErrType.T THEN
        Error.Txt (name, "third argument is not assignable to first");
      END;
    END;
  END DoCheck;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.PrepLValue (ce.args[0], lhs := TRUE);
    Expr.Prep (ce.args[1]);
    Expr.Prep (ce.args[2]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR lhs := ce.args[0];
  BEGIN
    Expr.CompileAddress (lhs); CG.Force ();
    Expr.Compile (ce.args[1]); CG.Force ();
    Expr.Compile (ce.args[2]); CG.Force ();
    CG.Cas (Type.CGType (ce.type, in_memory := TRUE));
    Expr.NoteWrite (lhs);
  END Compile;

PROCEDURE GetBounds (ce: CallExpr.T;  VAR min, max: Target.Int) =
  BEGIN
    Expr.GetBounds (ce.args[0], min, max);
  END GetBounds;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (3, 3, FALSE, FALSE, TRUE, NIL,
                                 TypeOf,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 CallExpr.NoValue,
                                 GetBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("CAS", Z, TRUE);
  END Initialize;

BEGIN
END Cas.
