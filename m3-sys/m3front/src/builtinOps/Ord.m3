(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Ord.m3                                                *)
(* Last Modified On Tue May  3 16:32:32 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:19 1990 By muller         *)

MODULE Ord;

IMPORT CallExpr, Expr, ExprRep, Type, Procedure, Int, LInt, Error;
IMPORT IntegerExpr, EnumExpr, CheckExpr, Target, TInt, CG;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR e := ce.args[0];  t := Expr.TypeOf (e);
      min, max, emin, emax: Target.Int;
  BEGIN
    IF NOT Type.IsOrdinal (t) THEN
      Error.Msg ("ORD: argument must be an ordinal");
    END;
    ce.type := Int.T;
    IF Type.IsSubtype (t, LInt.T) THEN
      (* must bound check the result *)
      Expr.GetBounds (e, emin, emax);
      <*ASSERT TInt.Prec (emin) = Target.Pre.Longint*>
      <*ASSERT TInt.Prec (emax) = Target.Pre.Longint*>
      min := Target.Int{Target.Integer.min, Target.Pre.Longint};
      max := Target.Int{Target.Integer.max, Target.Pre.Longint};
      IF TInt.LT (emin, min) THEN
        (* we need a lower bound check *)
        IF TInt.LT (max, emax) THEN
          (* we also need an upper bound check *)
          e := CheckExpr.New (e, min, max, CG.RuntimeError.ValueOutOfRange);
          Expr.TypeCheck (e, cs);
          ce.args[0] := e;
        ELSE
          e := CheckExpr.NewLower (e, min, CG.RuntimeError.ValueOutOfRange);
          Expr.TypeCheck (e, cs);
          ce.args[0] := e;
        END;
      ELSIF TInt.LT (max, emax) THEN
        (* we need an upper bound check *)
        e := CheckExpr.NewUpper (e, max, CG.RuntimeError.ValueOutOfRange);
        Expr.TypeCheck (e, cs);
        ce.args[0] := e;
      END;
    END;
  END Check;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR e := ce.args[0];  t := Expr.TypeOf (e);
  BEGIN
    Expr.Compile (e);
    IF Type.IsSubtype (t, LInt.T) THEN
      CG.Loophole (Target.Longint.cg_type, Target.Integer.cg_type);
    END;
  END Compile;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  VAR e: Expr.T;  x, i: Target.Int;  t: Type.T;
  BEGIN
    e := Expr.ConstValue (ce.args[0]);
    IF (e = NIL) THEN
      RETURN NIL;
    ELSIF EnumExpr.Split (e, i, t) THEN
      RETURN IntegerExpr.New (i);
    ELSIF IntegerExpr.Split (e, x) AND TInt.Ord (x, i) THEN
      RETURN IntegerExpr.New (i);
    ELSE
      RETURN NIL;
    END;
  END Fold;

PROCEDURE GetBounds (ce: CallExpr.T;  VAR min, max: Target.Int) =
  VAR e := ce.args[0];
  BEGIN
    Expr.GetBounds (e, min, max);
    WITH z = TInt.Ord (min, min) DO <*ASSERT z*> END;
    WITH z = TInt.Ord (max, max) DO <*ASSERT z*> END;
  END GetBounds;

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
                                 GetBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.Define ("ORD", Z, TRUE);
  END Initialize;

BEGIN
END Ord.
