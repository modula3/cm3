(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: BitSize.m3                                            *)
(* Last Modified On Wed Jun 29 17:00:26 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:27 1990 By muller         *)

MODULE BitSize;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, IntegerExpr;
IMPORT TypeExpr, OpenArrayType, Error, Card, Target, TInt, Int;
IMPORT MSIR, MSIRBuilder;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  BEGIN
    DoCheck ("BITSIZE", ce, cs);
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    DoPrep (ce.args[0]);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    DoCompile (ce.args[0], 1);
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  BEGIN
    RETURN DoFold (ce.args[0], 1);
  END Fold;

PROCEDURE DoCheck (name: TEXT;  ce: CallExpr.T;
                   <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR t: Type.T;   e := ce.args[0];  info: Type.Info;
  BEGIN
    IF Expr.IsDesignator (e) THEN
      (* ok *)
      t := Type.CheckInfo (Expr.TypeOf (e), info);
    ELSIF TypeExpr.Split (e, t) THEN
      IF OpenArrayType.Is (t) THEN
        Error.Txt (name, "argument cannot be an open array type");
      END;
    ELSE
      Error.Txt (name, "argument must be a designator or type");
    END;
    ce.type := Card.T;
  END DoCheck;

PROCEDURE DoPrep (e: Expr.T) =
  VAR t: Type.T;  info: Type.Info;
  BEGIN
    IF TypeExpr.Split (e, t) THEN RETURN; END;

    t := Type.CheckInfo (Expr.TypeOf (e), info);
    IF (info.class # Type.Class.OpenArray) THEN RETURN; END;

    (* ELSE, open array *)
    Expr.Prep (e);
  END DoPrep;

PROCEDURE DoCompile (e: Expr.T;  unit: INTEGER) =
  VAR t: Type.T;  sz, align: INTEGER;  t_array: CG.Val;  info: Type.Info;
  BEGIN
    IF TypeExpr.Split (e, t) THEN
      Type.Compile (t);
      EVAL Type.CheckInfo (t, info);
      <* ASSERT info.size > 0 *>
      CG.Load_intt ((info.size + unit - 1) DIV unit);
      RETURN;
    END;

    t := Type.CheckInfo (Expr.TypeOf (e), info);
    IF (info.class # Type.Class.OpenArray) THEN
      CG.Load_intt ((info.size + unit - 1) DIV unit);
      RETURN;
    END;

    (* ELSE, open array *)
    align := info.alignment;
    Expr.Compile (e);
    t_array := CG.Pop ();
    FOR i := 0 TO OpenArrayType.OpenDepth (t) - 1 DO
      CG.Push (t_array);
      CG.Open_size (i);
      IF (i # 0) THEN CG.Multiply (Target.Integer.cg_type) END;
    END;
    CG.Free (t_array);

    sz := OpenArrayType.EltPack (t);
    IF (sz MOD unit) = 0 THEN
      CG.Load_intt (sz DIV unit);
      CG.Multiply (Target.Integer.cg_type);
    ELSE (* array elements aren't "unit"-aligned *)
      CG.Load_intt (sz);
      CG.Multiply (Target.Integer.cg_type);
      CG.Load_intt (unit - 1);
      CG.Add (Target.Integer.cg_type);
      CG.Load_intt (unit);
      CG.Div (Target.Integer.cg_type, CG.Sign.Positive, CG.Sign.Positive);
    END;
  END DoCompile;

PROCEDURE DoCompileMSIR (e: Expr.T;  unit: INTEGER): MSIR.Value =
  VAR
    t:       Type.T;
    info:    Type.Info;
    iT:      MSIR.T    := MSIR.TI (Target.Integer.size);
    oa:      MSIR.Value;
    total:   MSIR.Value;
    dim:     MSIR.Value;
    eltPack: INTEGER;
    depth:   INTEGER;
    blk:     MSIR.Block;
  BEGIN
    IF TypeExpr.Split (e, t) THEN
      t := Type.CheckInfo (t, info);
      RETURN MSIR.ConstInt (iT, (info.size + unit - 1) DIV unit);
    END;
    t := Type.CheckInfo (Expr.TypeOf (e), info);
    IF info.class # Type.Class.OpenArray THEN
      RETURN MSIR.ConstInt (iT, (info.size + unit - 1) DIV unit);
    END;
    (* Open array: compute total element count at runtime. *)
    oa := Expr.CompileMSIR (e);
    IF oa = NIL THEN RETURN NIL END;
    blk   := MSIRBuilder.CurrentBlock ();
    depth := OpenArrayType.OpenDepth (t);
    total := MSIR.BuildOpenArraySize (blk, "", oa, 0);
    FOR i := 1 TO depth - 1 DO
      dim   := MSIR.BuildOpenArraySize (blk, "", oa, i);
      total := MSIR.BuildIMul (blk, "", total, dim);
    END;
    eltPack := OpenArrayType.EltPack (t);
    IF (eltPack MOD unit) = 0 THEN
      RETURN MSIR.BuildIMul (blk, "", total,
               MSIR.ConstInt (iT, eltPack DIV unit));
    ELSE
      (* (total * eltPack + unit - 1) DIV unit *)
      total := MSIR.BuildIMul (blk, "", total,
                 MSIR.ConstInt (iT, eltPack));
      total := MSIR.BuildIAdd (blk, "", total,
                 MSIR.ConstInt (iT, unit - 1));
      RETURN MSIR.BuildIDiv (blk, "", total,
               MSIR.ConstInt (iT, unit));
    END;
  END DoCompileMSIR;

PROCEDURE DoFold (e: Expr.T;  unit: INTEGER): Expr.T =
  VAR t: Type.T;  size, a, b, c, d: Target.Int;  info: Type.Info;
  BEGIN
    IF NOT TypeExpr.Split (e, t) THEN
      t := Type.CheckInfo (Expr.TypeOf (e), info);
      IF (info.class = Type.Class.OpenArray) THEN RETURN NIL END;
    END;
    t := Type.CheckInfo (t, info);
    IF    TInt.FromInt (info.size, size)
      AND TInt.FromInt (unit, a)
      AND TInt.FromInt (unit - 1, b)
      AND TInt.Add (size, b, c)
      AND TInt.Div (c, a, d)
      AND NOT TInt.LT (d, Target.Integer.min)
      AND NOT TInt.LT (Target.Integer.max, d)
      THEN RETURN IntegerExpr.New (Int.T, d);
      ELSE RETURN NIL;
    END;
  END DoFold;

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  BEGIN  RETURN DoCompileMSIR (ce.args[0], 1)  END CompileMSIR;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, FALSE, Card.T,
                                 NIL, NIL,
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
    Procedure.DefinePredefined ("BITSIZE", Z, TRUE);
  END Initialize;

BEGIN
END BitSize.
