(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Ceiling.m3                                            *)
(* Last Modified On Fri Jun 24 09:37:32 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:26 1990 By muller         *)

MODULE Ceiling;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Int;
IMPORT Reel, LReel, EReel, Error, ReelExpr;

VAR Z: CallExpr.MethodList;

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
    ce.type := Int.T;
  END DoCheck;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR e := ce.args[0];
  BEGIN
    Expr.Compile (e);
    CG.Cvt_int (Type.CGType (Expr.TypeOf (e)), CG.Cvt.Ceiling);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e, x: Expr.T;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF    (e = NIL)                THEN RETURN NIL;
    ELSIF ReelExpr.Ceiling (e, x)  THEN RETURN x;
    ELSE  (* bogus *)                   RETURN NIL;
    END;
  END Fold;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, TRUE, Int.T,
                                 NIL,
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
    Procedure.Define ("CEILING", Z, TRUE);
  END Initialize;

BEGIN
END Ceiling.
