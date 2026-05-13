(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Floatt.m3                                             *)
(* Last Modified On Tue May  3 16:30:33 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:23 1990 By muller         *)

MODULE Floatt;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Reel, LReel, EReel;
IMPORT Int, LInt, Error, ReelExpr, TypeExpr;
IMPORT MSIR, MSIRBuilder, MSIRType;

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

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  VAR
    e    := ce.args[0];
    srcT := Type.Base (Expr.TypeOf (e));
    dstT := TypeOf (ce);
    blk  : MSIR.Block;
    src  : MSIR.Value;
    dst  : MSIR.T;
    srcIsFloat := (srcT = Reel.T) OR (srcT = LReel.T) OR (srcT = EReel.T);
    dstIsFloat := (dstT = Reel.T) OR (dstT = LReel.T) OR (dstT = EReel.T);
  BEGIN
    IF NOT dstIsFloat THEN
      MSIRBuilder.Abandon ("FLOAT: unexpected dest type");
      RETURN NIL;
    END;
    src := Expr.CompileMSIR (e);  IF src = NIL THEN RETURN NIL END;
    (* Capture currentBlock AFTER compiling e — an invoke inside a TRY may switch it. *)
    blk := MSIRBuilder.CurrentBlock ();
    dst := MSIRType.Translate (dstT);
    IF dst = NIL THEN
      MSIRBuilder.Abandon ("FLOAT: unsupported dest type in MSIR");
      RETURN NIL;
    END;
    IF NOT srcIsFloat THEN
      (* Integer → float: signed integer to floating-point *)
      RETURN MSIR.BuildSIToFP (blk, "", src, dst);
    ELSE
      VAR srcBits := MSIR.BitWidth (MSIR.ValueType (src));
          dstBits := MSIR.BitWidth (dst);
      BEGIN
        IF dstBits > srcBits THEN
          RETURN MSIR.BuildFPExt (blk, "", src, dst);
        ELSIF dstBits < srcBits THEN
          RETURN MSIR.BuildFPTrunc (blk, "", src, dst);
        ELSE
          RETURN src;   (* same width, identity *)
        END;
      END;
    END;
  END CompileMSIR;

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
    CallExpr.SetMethodMSIR (Z, CompileMSIR);
    Procedure.DefinePredefined ("FLOAT", Z, TRUE);
  END Initialize;

BEGIN
END Floatt.
