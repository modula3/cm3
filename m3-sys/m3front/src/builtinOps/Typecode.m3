(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Typecode.m3                                           *)
(* Last Modified On Tue May  3 16:33:20 PDT 1994 By kalsow     *)
(*      Modified On Fri Mar 15 03:50:01 1991 By muller         *)

MODULE Typecode;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Card, Error;
IMPORT Reff, TypeExpr, ObjectType, M3RT, Target, TInt;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR t: Type.T;
  BEGIN
    IF TypeExpr.Split (ce.args[0], t) THEN
      IF (ObjectType.Is (t)) THEN
        (* ok *)
      ELSIF (Type.IsEqual (t, Reff.T, NIL)) THEN
        Error.Msg ("TYPECODE: T must be a fixed reference type");
      ELSIF (NOT Type.IsSubtype (t, Reff.T)) THEN
        Error.Msg ("TYPECODE: T must be a traced reference type");
      END;
    ELSE
      t := Expr.TypeOf (ce.args[0]);
      IF NOT Type.IsSubtype (t, Reff.T) AND NOT ObjectType.Is (t) THEN
        Error.Msg ("TYPECODE: r must be a traced reference or object");
      END;
    END;
    ce.type := Card.T;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  VAR e := ce.args[0];  t: Type.T;  nil, tagged: CG.Label;
  BEGIN
    IF TypeExpr.Split (e, t) THEN
      (* get the typecode from the typecell *)
    ELSE
      (* get the typecode from the REF's header *)
      Expr.Prep (e);
      Expr.Compile (e);
      ce.tmp := CG.Pop_temp ();
      tagged := CG.Next_label ();
      nil := CG.Next_label ();

      CG.Push (ce.tmp);
      CG.Load_nil ();
      CG.If_compare (CG.Type.Addr, CG.Cmp.EQ, nil, CG.Never);

      CG.Push (ce.tmp);
      CG.Loophole (CG.Type.Addr, Target.Word.cg_type);
      CG.Load_integer (Target.Word.cg_type, TInt.One);
      CG.And (Target.Word.cg_type);
      CG.If_true (tagged, CG.Maybe);

      CG.Push (ce.tmp);
      CG.Ref_to_info (M3RT.RH_typecode_offset, M3RT.RH_typecode_size);
      CG.Loophole (Target.Integer.cg_type, CG.Type.Addr);
      CG.Store_temp (ce.tmp);
      CG.Jump (nil);

      CG.Set_label (tagged);
      CG.Load_intt (M3RT.REFANY_typecode);
      CG.Loophole (Target.Integer.cg_type, CG.Type.Addr);
      CG.Store_temp (ce.tmp);

      CG.Set_label (nil);
    END;
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  VAR e := ce.args[0];  t: Type.T;
  BEGIN
    IF TypeExpr.Split (e, t) THEN
      (* get the typecode from the typecell *)
      Type.Compile (t);
      Type.LoadInfo (t, M3RT.TC_typecode);
    ELSE
      (* get the typecode from the REF's header *)
      CG.Push (ce.tmp);
      CG.Loophole (CG.Type.Addr, Target.Integer.cg_type);
      CG.Free (ce.tmp);
      ce.tmp := NIL;
    END;
  END Compile;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, TRUE, FALSE, TRUE, Card.T,
                                 NIL, NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 CallExpr.NoValue, (* fold *)
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.DefinePredefined ("TYPECODE", Z, TRUE);
  END Initialize;

BEGIN
END Typecode.
