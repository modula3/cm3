(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Adr.m3                                                *)
(* Last Modified On Wed Jun 29 16:59:33 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:28 1990 By muller         *)

MODULE Adr;

IMPORT CallExpr, Expr, ExprRep, Procedure, Error, Module, Addr, CG;
IMPORT RefType, Type, Host;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t := Addr.T;
  BEGIN
    IF Host.new_adr THEN
      t := RefType.New (Expr.TypeOf (ce.args[0]), FALSE, NIL);
    END;
    RETURN t;
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR e := ce.args[0];
  BEGIN
    IF ce.type = NIL THEN ce.type := TypeOf (ce); END;
    ce.type := Type.Check (ce.type);
    IF Module.IsSafe () THEN Error.Msg ("unsafe operation"); END;
    IF Expr.IsDesignator (e)
      THEN Expr.NeedsAddress (e);
      ELSE Error.Msg ("ADR: argument must be a designator");
    END;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.PrepLValue (ce.args[0]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    Expr.CompileLValue (ce.args[0]);
    CG.Check_byte_aligned ();
  END Compile;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, TRUE, NIL,
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
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("ADR", Z, TRUE);
  END Initialize;

BEGIN
END Adr.
