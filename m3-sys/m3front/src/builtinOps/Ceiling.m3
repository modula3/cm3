(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Ceiling.m3                                            *)
(* Last Modified On Fri Jun 24 09:37:32 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:26 1990 By muller         *)

MODULE Ceiling;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Int, LInt;
IMPORT Reel, LReel, EReel, Error, ReelExpr, TypeExpr;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;
  BEGIN
    t := Int.T;
    IF (NUMBER (ce.args^) > 1) THEN
      EVAL TypeExpr.Split (ce.args[1], t);
      t := Type.Base (t);
    END;
    RETURN t;
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    DoCheck ("CEILING", ce, cs);
  END Check;

PROCEDURE DoCheck (name: TEXT;  ce: CallExpr.T;
                   <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR t: Type.T;
  BEGIN
    t := Type.Base (Expr.TypeOf (ce.args[0]));
    IF (t # Reel.T) AND (t # LReel.T) AND (t # EReel.T) THEN
      Error.Txt (name, "argument not REAL, LONGREAL or EXTENDED");
    END;
    t := Int.T;
    IF (NUMBER (ce.args^) > 1) THEN
      IF NOT TypeExpr.Split (ce.args[1], t) THEN
        Error.Txt (name, "second argument must be a type");
      END;
      t := Type.Base (t);
    END;
    IF (t # Int.T) AND (t # LInt.T) THEN
      Error.Txt (name, "second argument must be an integer type");
    END;

    ce.type := t;
  END DoCheck;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Expr.Prep (ce.args[0]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR e := ce.args[0];  t := TypeOf (ce);
  BEGIN
    Expr.Compile (e);
    CG.Cvt_int (Type.CGType (Expr.TypeOf (e)), Type.CGType (t), CG.Cvt.Ceiling);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e, x: Expr.T;  t := TypeOf (ce);
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF    (e = NIL)                  THEN RETURN NIL
    ELSIF ReelExpr.Ceiling (e, t, x) THEN RETURN x;
    ELSE  (* bogus *)                     RETURN NIL;
    END;
  END Fold;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 2, TRUE, FALSE, TRUE, NIL,
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
    Procedure.DefinePredefined ("CEILING", Z, TRUE);
  END Initialize;

BEGIN
END Ceiling.
