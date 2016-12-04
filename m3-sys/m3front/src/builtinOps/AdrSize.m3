(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AdrSize.m3                                            *)
(* Last Modified On Tue May  3 16:29:11 PDT 1994 By kalsow     *)
(*      Modified On Tue Apr 10 10:57:13 1990 By muller         *)

MODULE AdrSize;

IMPORT CallExpr, Expr, Procedure, Target, BitSize, Card;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    BitSize.DoCheck ("ADRSIZE", ce, cs);
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    BitSize.DoPrep (ce.args[0]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    BitSize.DoCompile (ce.args[0], Target.Byte);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  BEGIN
    RETURN BitSize.DoFold (ce.args[0], Target.Byte);
  END Fold;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, FALSE, Card.T,
                                 NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
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
    Procedure.DefinePredefined ("ADRSIZE", Z, TRUE);
  END Initialize;

BEGIN
END AdrSize.
