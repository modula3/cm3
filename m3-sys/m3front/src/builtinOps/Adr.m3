(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Adr.m3                                                *)
(* Last Modified On Wed Jun 29 16:59:33 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:28 1990 By muller         *)

MODULE Adr;

IMPORT CallExpr, Expr, ExprRep, Procedure, Error, Module, Addr, CG;
IMPORT RefType, Type, Host, Target;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  BEGIN
    IF Host.new_adr THEN
      RETURN RefType.New (Expr.TypeOf (ce.args[0]), FALSE, NIL);
    ELSE
      RETURN Addr.T;
    END;
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR e := ce.args[0];
  BEGIN
    IF ce.type = NIL THEN ce.type := TypeOf (ce); END;
    ce.type := Type.Check (ce.type);
    IF Module.IsSafe () THEN Error.Msg ("unsafe operation"); END;
    IF NOT Expr.IsDesignator (e)
    THEN
      Error.Msg ("Argument of ADR must be a designator (2.7).");
      ce.hasError := TRUE;
    ELSE
      Expr.NeedsAddress (e);
      IF Expr.Alignment (e) MOD Target.Byte # 0 THEN
        Error.Msg
          ("CM3 restriction: non-byte-aligned value cannot be passed to ADR"
           & " (2.3.2)");
        ce.hasError := TRUE;
      END;
    END;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    IF ce.hasError THEN RETURN END;
    Expr.PrepLValue (ce.args[0], traced := FALSE);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    IF ce.hasError THEN RETURN END;
    Expr.CompileLValue (ce.args[0], traced := FALSE);
    CG.Check_byte_aligned ();
  END Compile;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, TRUE, NIL,
                                 TypeOf,
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
    Procedure.DefinePredefined ("ADR", Z, TRUE);
  END Initialize;

BEGIN
END Adr.
