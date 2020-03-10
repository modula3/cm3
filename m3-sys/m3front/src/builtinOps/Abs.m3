(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Abs.m3                                                *)
(* Last Modified On Tue May  3 16:28:46 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:46:53 1990 By muller         *)

MODULE Abs;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Error;
IMPORT Target, TInt, Int, LInt, Reel, LReel, EReel, IntegerExpr, ReelExpr;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  BEGIN
    RETURN Type.Base (Expr.TypeOf (ce.args[0]));
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR t := TypeOf (ce);
  BEGIN
    ce.type := t;
    IF (t = Int.T) OR (t = LInt.T)
      OR (t = Reel.T) OR (t = LReel.T) OR (t = EReel.T) THEN
      (* ok *)
    ELSE
      Error.Msg ("ABS: wrong argument type");
    END;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR e := ce.args[0];
  BEGIN
    Expr.Compile (e);
    CG.Abs (Type.CGType (Expr.TypeOf (e)));
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e, x: Expr.T;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF (e = NIL) THEN
      RETURN NIL;
    ELSIF IntegerExpr.Abs (e, x) THEN
      RETURN x;
    ELSIF ReelExpr.Abs (e, x) THEN
      RETURN x;
    ELSE
      RETURN NIL;
    END;
  END Fold;

PROCEDURE GetBounds (ce: CallExpr.T;  VAR min, max: Target.Int) =
  BEGIN
    Expr.GetBounds (ce.args[0], min, max);
    IF TInt.LT (min, TInt.Zero) THEN min := TInt.Zero; END;
  END GetBounds;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, TRUE, NIL,
                                 TypeOf,
                                 TypeOf,
                                 CallExpr.NotAddressable,
                                 Check,
                                 CallExpr.PrepArgs,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 Fold,
                                 GetBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.DefinePredefined ("ABS", Z, TRUE);
  END Initialize;

BEGIN
END Abs.
