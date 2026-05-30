(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Typecode.m3                                           *)
(* Last Modified On Tue May  3 16:33:20 PDT 1994 By kalsow     *)
(*      Modified On Fri Mar 15 03:50:01 1991 By muller         *)

MODULE Typecode;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Card, Error;
IMPORT Reff, TypeExpr, ObjectType, M3RT, Target, TInt;
IMPORT MSIR, MSIRBuilder, Word;

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

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  VAR
    e    := ce.args[0];
    t    : Type.T;
    intT := MSIR.TI (Target.Integer.size);
  BEGIN
    IF TypeExpr.Split (e, t) THEN
      (* TYPECODE(T) — byte 0 of the TypeCell holds TC_typecode. *)
      Type.Compile (t);
      t := Type.Base (t);
      VAR tc: MSIR.Value;
      BEGIN
        IF ObjectType.Is (t) THEN
          tc := MSIRBuilder.TypeLinkValueForObject (t);
        ELSE
          tc := MSIRBuilder.TypeLinkValueForRef (t);
        END;
        IF tc = NIL THEN
          MSIRBuilder.Abandon ("TYPECODE: cannot get typecell");  RETURN NIL;
        END;
        RETURN MSIR.BuildLoad (MSIRBuilder.CurrentBlock (), "", intT, tc);
      END;
    ELSE
      (* TYPECODE(r) — read typecode from ref header at runtime.
         TYPECODE(NIL) = 0 (NULL_typecode), matching CM3's CG behaviour. *)
      VAR
        refVal   : MSIR.Value;
        resAlloca: MSIR.Value;
        nilBlk, normalBlk, mergeBlk : MSIR.Block;
        nilCond  : MSIR.Value;
        hdrPtr, hdrWord, shifted, masked : MSIR.Value;
        blk      : MSIR.Block;
      BEGIN
        refVal := Expr.CompileMSIR (e);
        IF refVal = NIL THEN RETURN NIL END;
        blk := MSIRBuilder.CurrentBlock ();

        (* Allocate result slot before branching. *)
        resAlloca := MSIR.BuildAlloca (blk, "", intT);

        nilBlk    := MSIRBuilder.NewBlock ("");
        normalBlk := MSIRBuilder.NewBlock ("");
        mergeBlk  := MSIRBuilder.NewBlock ("");

        (* NIL → NULL_typecode = 0 *)
        nilCond := MSIR.BuildICmp (blk, "", MSIR.CmpPred.Eq,
                                   refVal, MSIR.ConstNil (MSIR.ValueType (refVal)));
        MSIR.BuildCondBr (blk, nilCond,
                          nilBlk,    ARRAY OF MSIR.Value {},
                          normalBlk, ARRAY OF MSIR.Value {});

        MSIR.BuildStore (nilBlk,
                         MSIR.ConstInt (intT, M3RT.NULL_typecode),
                         resAlloca);
        MSIR.BuildBr (nilBlk, mergeBlk, ARRAY OF MSIR.Value {});

        (* Non-NIL: ref header is one word before the data pointer.
           RH layout: bits [RH_typecode_offset .. +RH_typecode_size) = bits [1..20]. *)
        hdrPtr  := MSIRBuilder.BuildPtrByteOff (normalBlk, "", refVal,
                                                -(Target.Address.size DIV 8));
        hdrWord := MSIR.BuildLoad (normalBlk, "", intT, hdrPtr);
        shifted := MSIR.BuildILShr (normalBlk, "", hdrWord,
                                    MSIR.ConstInt (intT, M3RT.RH_typecode_offset));
        masked  := MSIR.BuildIAnd (normalBlk, "", shifted,
                                   MSIR.ConstInt (intT,
                                     Word.LeftShift (1, M3RT.RH_typecode_size) - 1));
        MSIR.BuildStore (normalBlk, masked, resAlloca);
        MSIR.BuildBr (normalBlk, mergeBlk, ARRAY OF MSIR.Value {});

        MSIRBuilder.SetCurrentBlock (mergeBlk);
        RETURN MSIR.BuildLoad (mergeBlk, "", intT, resAlloca);
      END;
    END;
  END CompileMSIR;

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
    CallExpr.SetMethodMSIR (Z, CompileMSIR);
    Procedure.DefinePredefined ("TYPECODE", Z, TRUE);
  END Initialize;

BEGIN
END Typecode.
