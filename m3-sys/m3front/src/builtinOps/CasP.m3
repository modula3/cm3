(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: CasP.m3                                               *)
(* Last Modified On Wed Jun 29 17:01:08 PDT 1994 By kalsow     *)
(*      Modified On Tue Feb 12 11:52:16 1991 By muller         *)

MODULE CasP;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure;
IMPORT Error, ErrType, Bool, Addr, Module;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T; <*UNUSED*> VAR cs: Expr.CheckState) =
  BEGIN
    DoCheck ("CASP", ce);
  END Check;

PROCEDURE DoCheck (name: TEXT; ce: CallExpr.T) =
  VAR t: Type.T; e: Expr.T;
  BEGIN
    e := ce.args[0];
    t := Expr.TypeOf (e);
    IF NOT Type.IsOrdinal (t) THEN
      IF Type.IsSubtype (t, Addr.T) THEN
        IF Module.IsSafe () THEN Error.Txt (name, "unsafe operation") END;
      ELSE
        Error.Txt (name, "first argument must be of an ordinal type");
      END;
    END;
    IF (NOT Expr.IsDesignator (e)) THEN
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
    ce.type := Bool.T;
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
    Expr.CompileAddress (lhs, lhs := TRUE); CG.Force ();
    Expr.Compile (ce.args[1]); CG.Force ();
    Expr.Compile (ce.args[2]); CG.Force ();
    CG.CasP (Type.CGType (Expr.TypeOf (lhs), in_memory := TRUE),
             Type.CGType (Bool.T, in_memory := FALSE));
    Expr.NoteWrite (lhs);
  END Compile;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (3, 3, FALSE, FALSE, TRUE, Bool.T,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.PrepNoBranch,
                                 CallExpr.NoBranch,
                                 CallExpr.NoValue,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("CASP", Z, TRUE);
  END Initialize;

BEGIN
END CasP.
