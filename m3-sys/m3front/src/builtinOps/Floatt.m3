(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Floatt.m3                                             *)
(* Last Modified On Tue May  3 16:30:33 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:23 1990 By muller         *)

MODULE Floatt;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Reel, LReel, EReel;
IMPORT Int, LInt, Error, ReelExpr, TypeExpr;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR u: Type.T;
  BEGIN
    u := Reel.T;
    IF (NUMBER (ce.args^) > 1) THEN
      EVAL TypeExpr.Split (ce.args[1], u);
      u := Type.Base (u);
    END;
    RETURN u;
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;
                 <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR t, u: Type.T;  e: Expr.T;
  BEGIN
    e := ce.args[0];

    u := Reel.T;
    IF (NUMBER (ce.args^) > 1) THEN
      IF NOT TypeExpr.Split (ce.args[1], u) THEN
        Error.Msg ("FLOAT: second argument must be a floating point type");
      END;
      u := Type.Base (u);
    END;

    t := Type.Base (Expr.TypeOf (ce.args[0]));
    IF (t # Int.T) AND (t # LInt.T)
      AND (t # Reel.T) AND (t # LReel.T) AND (t # EReel.T) THEN
      Error.Msg ("FLOAT: wrong first argument type");
    END;

    IF (u # Reel.T) AND (u # LReel.T) AND (u # EReel.T) THEN
      Error.Msg ("FLOAT: wrong second argument type");
    END;

    ce.type := u;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.Prep (ce.args[0]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR
    e := ce.args[0];
    t := Expr.TypeOf (e);
    u := TypeOf (ce);
  BEGIN
    Expr.Compile (e);
    CG.Cvt_float (Type.CGType (t), Type.CGType (u));
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e, x: Expr.T;  t: Type.T;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF (e = NIL) THEN RETURN NIL END;

    t := Reel.T;
    IF (NUMBER (ce.args^) > 1) THEN
      IF NOT TypeExpr.Split (ce.args[1], t) THEN RETURN NIL END;
    END;

    IF ReelExpr.Float (e, t, x)
      THEN  RETURN x;
      ELSE  RETURN NIL;
    END;
  END Fold;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 2, TRUE, FALSE, TRUE, NIL,
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
                                 Fold,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.DefinePredefined ("FLOAT", Z, TRUE);
  END Initialize;

BEGIN
END Floatt.
