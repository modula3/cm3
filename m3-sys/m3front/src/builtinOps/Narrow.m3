(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Narrow.m3                                             *)
(* Last Modified On Wed Jun 29 17:29:43 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:20 1990 By muller         *)

MODULE Narrow;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Error, TypeExpr;
IMPORT Procedure, ObjectType, Reff, Null, M3RT, RefType;
IMPORT Target, RunTyme, TInt, Word;
IMPORT MSIR, MSIRBuilder, MSIRType;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;
  BEGIN
    IF TypeExpr.Split (ce.args[1], t)
      THEN RETURN t;
      ELSE RETURN Expr.TypeOf (ce.args[0]);
    END;
  END TypeOf;

PROCEDURE RepTypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;
  BEGIN
    IF TypeExpr.Split (ce.args[1], t)
      THEN RETURN t;
      ELSE RETURN Expr.RepTypeOf (ce.args[0]);
    END;
  END RepTypeOf;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR dest: Type.T;  src := Expr.TypeOf (ce.args[0]);
  BEGIN
    IF NOT TypeExpr.Split (ce.args[1], dest) THEN
      Error.Msg ("NARROW: second argument must be a type");
      dest := src;
    END;

    IF NOT Type.IsAssignable (dest, src) THEN
      Error.Msg ("NARROW: types must be assignable");
    ELSIF ObjectType.Is (dest) OR Type.IsSubtype (dest, Reff.T) THEN
      (* ok *)
    ELSE (* untraced ref type *)
      Error.Msg ("NARROW: must be a traced reference or object type");
    END;

    ce.type := dest;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  VAR t: Type.T;
  BEGIN
    EVAL TypeExpr.Split (ce.args[1], t);
    Type.Compile (t);
    Expr.Prep (ce.args[0]);
    Expr.Compile (ce.args[0]);
    ce.tmp := EmitCore (t, Expr.TypeOf (ce.args[0]));
    IF (ce.tmp = NIL) THEN
      (* capture the ref value *)
      ce.tmp := CG.Pop_temp ();
    END;
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    (* all the work was done by Prep *)
    CG.Push (ce.tmp);
    CG.Free (ce.tmp);
    ce.tmp := NIL;
  END Compile;

PROCEDURE Emit (tlhs, trhs: Type.T) =
  VAR tmp := EmitCore (tlhs, trhs);
  BEGIN
    IF (tmp # NIL) THEN
      (* reload the ref value on the stack *)
      CG.Push (tmp);
      CG.Free (tmp);
    END;
  END Emit;

PROCEDURE EmitCore (tlhs, trhs: Type.T): CG.Val =
  VAR
    ok, tagged: CG.Label;
    ref: CG.Val;
    is_object := ObjectType.Is (tlhs);
    target: Type.T;
    align: INTEGER;
    lhs_info, info: Type.Info;
    proc: Procedure.T;
  BEGIN
    tlhs := Type.CheckInfo (tlhs, lhs_info);
    IF is_object THEN
      align := ObjectType.FieldAlignment (tlhs);
      CG.Boost_addr_alignment (align);
    ELSIF RefType.Split (tlhs, target) THEN
      target := Type.CheckInfo (target, info);
      align := info.alignment;
      CG.Boost_addr_alignment (align);
    END;

    (* CT test for the no-check cases... *)
    IF Type.IsSubtype (trhs, tlhs) THEN RETURN NIL; END;
    IF (NOT is_object) AND (NOT lhs_info.isTraced) THEN RETURN NIL; END;

    (* capture the right-hand side *)
    ref := CG.Pop ();
    ok := CG.Next_label ();

    (* RT check for ref = NIL *)
    CG.Push (ref);
    CG.Load_nil ();
    CG.If_compare (CG.Type.Addr, CG.Cmp.EQ, ok, CG.Maybe);

    IF NOT Type.IsEqual (tlhs, Null.T, NIL) THEN
      tagged := CG.Next_label ();

      (* RT check for ref is tagged. *) 
      CG.Push (ref);
      CG.Loophole (CG.Type.Addr, Target.Word.cg_type);
      CG.Load_integer (Target.Word.cg_type, TInt.One);
      CG.And (Target.Word.cg_type);
      CG.If_true (tagged, CG.Maybe);

      (* RT check for TYPECODE(ref) = TYPECODE(type) *)
      CG.Push (ref);
      CG.Ref_to_info (M3RT.RH_typecode_offset, M3RT.RH_typecode_size);
      Type.LoadInfo (tlhs, M3RT.TC_typecode);
      CG.If_compare (Target.Integer.cg_type, CG.Cmp.EQ, ok, CG.Always);

      IF NOT RefType.Is (tlhs) THEN
        (*Note: When T is unrevealed <: REFANY, both ObjectType(T) and 
                RefType.Is(T) return FALSE.  We need to call the runtime in
                in this case. *) 
        (* finally, call the runtime to figure it out... *)
        proc := RunTyme.LookUpProc (RunTyme.Hook.CheckIsType);
        Procedure.StartCall (proc);
        IF Target.DefaultCall.args_left_to_right THEN
          CG.Push (ref);
          CG.Pop_param (CG.Type.Addr);
          Type.LoadInfo (tlhs, -1);
          CG.Pop_param (CG.Type.Addr);
        ELSE
          Type.LoadInfo (tlhs, -1);
          CG.Pop_param (CG.Type.Addr);
          CG.Push (ref);
          CG.Pop_param (CG.Type.Addr);
        END;
        Procedure.EmitCall (proc);
        CG.If_true (ok, CG.Always);
      END;
      CG.Abort (CG.RuntimeError.NarrowFailed);

      CG.Set_label (tagged);
      CG.Load_intt (M3RT.REFANY_typecode);
      Type.LoadInfo (tlhs, M3RT.TC_typecode);
      CG.If_compare (Target.Integer.cg_type, CG.Cmp.EQ, ok, CG.Always);
    END;

    CG.Abort (CG.RuntimeError.NarrowFailed);
    CG.Set_label (ok);

    RETURN ref;
  END EmitCore;

PROCEDURE Fold (ce: CallExpr.T): Expr.T =
  BEGIN
    RETURN Expr.ConstValue (ce.args[0]);
  END Fold;

PROCEDURE NoteWrites (ce: CallExpr.T) =
  BEGIN
    Expr.NoteWrite (ce.args[0]);
  END NoteWrites;

PROCEDURE TypeTestMSIR (refVal: MSIR.Value;  t: Type.T): MSIR.Value =
  VAR
    intT  := MSIR.TI (Target.Integer.size);
    i1    := MSIR.TI1 ();
    blk   := MSIRBuilder.CurrentBlock ();
    res   := MSIR.BuildAlloca (blk, "", i1);
    yes   := MSIRBuilder.NewBlock ("");
    no    := MSIRBuilder.NewBlock ("");
    done  := MSIRBuilder.NewBlock ("");
    notNil, untagged, tagged, other: MSIR.Block;
    cell, tc, word, hdr, rtc, raw: MSIR.Value;
    hook: MSIR.Proc;
  BEGIN
    IF ObjectType.Is (t) THEN
      cell := MSIRBuilder.TypeLinkValueForObject (t);
    ELSE
      cell := MSIRBuilder.TypeLinkValueForRef (t);
    END;
    IF cell = NIL THEN
      MSIRBuilder.Abandon ("NARROW: cannot get typecell");  RETURN NIL;
    END;
    hook := NIL;
    IF NOT RefType.Is (t) THEN
      (* an object, or an opaque type: the runtime decides *)
      hook := MSIRBuilder.HookProc (RunTyme.Hook.CheckIsType);
      IF hook = NIL THEN
        MSIRBuilder.Abandon ("NARROW: CheckIsType hook missing");  RETURN NIL;
      END;
    END;
    blk := MSIRBuilder.CurrentBlock ();
    tc  := MSIR.BuildLoad (blk, "", intT, cell);  (* byte 0: TC_typecode *)

    (* NIL is of every reference type *)
    notNil := MSIRBuilder.NewBlock ("");
    MSIR.BuildCondBr (blk, MSIR.BuildICmp (blk, "", MSIR.CmpPred.Eq, refVal,
                                           MSIR.ConstNil (MSIR.ValueType (refVal))),
                      yes, ARRAY OF MSIR.Value {}, notNil, ARRAY OF MSIR.Value {});

    (* a tagged reference (low bit set) has typecode REFANY_typecode *)
    tagged   := MSIRBuilder.NewBlock ("");
    untagged := MSIRBuilder.NewBlock ("");
    word := MSIR.BuildConvert (notNil, "", refVal, intT);
    MSIR.BuildCondBr (notNil,
      MSIR.BuildICmp (notNil, "", MSIR.CmpPred.Ne,
                      MSIR.BuildIAnd (notNil, "", word, MSIR.ConstInt (intT, 1)),
                      MSIR.ConstInt (intT, 0)),
      tagged, ARRAY OF MSIR.Value {}, untagged, ARRAY OF MSIR.Value {});
    MSIR.BuildCondBr (tagged,
      MSIR.BuildICmp (tagged, "", MSIR.CmpPred.Eq,
                      MSIR.ConstInt (intT, M3RT.REFANY_typecode), tc),
      yes, ARRAY OF MSIR.Value {}, no, ARRAY OF MSIR.Value {});

    (* the typecode in the header, one word before the referent *)
    hdr := MSIR.BuildLoad (untagged, "", intT,
             MSIRBuilder.BuildPtrByteOff (untagged, "", refVal,
                                          -(Target.Address.size DIV Target.Byte)));
    rtc := MSIR.BuildIAnd (untagged, "",
             MSIR.BuildILShr (untagged, "", hdr,
                              MSIR.ConstInt (intT, M3RT.RH_typecode_offset)),
             MSIR.ConstInt (intT, Word.LeftShift (1, M3RT.RH_typecode_size) - 1));
    IF hook = NIL THEN
      other := no;
    ELSE
      other := MSIRBuilder.NewBlock ("");
    END;
    MSIR.BuildCondBr (untagged,
      MSIR.BuildICmp (untagged, "", MSIR.CmpPred.Eq, rtc, tc),
      yes, ARRAY OF MSIR.Value {}, other, ARRAY OF MSIR.Value {});

    IF hook # NIL THEN
      MSIRBuilder.SetCurrentBlock (other);
      raw := MSIRBuilder.EmitCall ("", hook, ARRAY OF MSIR.Value {refVal, cell});
      IF raw = NIL THEN RETURN NIL END;
      blk := MSIRBuilder.CurrentBlock ();
      MSIR.BuildCondBr (blk,
        MSIR.BuildICmp (blk, "", MSIR.CmpPred.Ne, raw,
                        MSIR.ConstInt (MSIR.ValueType (raw), 0)),
        yes, ARRAY OF MSIR.Value {}, no, ARRAY OF MSIR.Value {});
    END;

    MSIR.BuildStore (yes, MSIR.ConstInt (i1, 1), res);
    MSIR.BuildBr (yes, done, ARRAY OF MSIR.Value {});
    MSIR.BuildStore (no, MSIR.ConstInt (i1, 0), res);
    MSIR.BuildBr (no, done, ARRAY OF MSIR.Value {});
    MSIRBuilder.SetCurrentBlock (done);
    RETURN MSIR.BuildLoad (done, "", i1, res);
  END TypeTestMSIR;

PROCEDURE EmitMSIR (refVal: MSIR.Value;  tlhs, trhs: Type.T): MSIR.Value =
  (* As Emit (EmitCore). *)
  VAR lhs_info: Type.Info;  blk: MSIR.Block;  cond: MSIR.Value;
  BEGIN
    EVAL Type.CheckInfo (tlhs, lhs_info);
    IF Type.IsSubtype (trhs, tlhs) THEN RETURN refVal END;
    IF (NOT ObjectType.Is (tlhs)) AND (NOT lhs_info.isTraced) THEN
      RETURN refVal;
    END;
    blk := MSIRBuilder.CurrentBlock ();
    IF Type.IsEqual (tlhs, Null.T, NIL) THEN
      (* only NIL narrows to NULL *)
      cond := MSIR.BuildICmp (blk, "", MSIR.CmpPred.Ne, refVal,
                              MSIR.ConstNil (MSIR.ValueType (refVal)));
    ELSE
      cond := TypeTestMSIR (refVal, Type.Base (tlhs));
      IF cond = NIL THEN RETURN NIL END;
      blk := MSIRBuilder.CurrentBlock ();
      cond := MSIR.BuildICmp (blk, "", MSIR.CmpPred.Eq, cond,
                              MSIR.ConstInt (MSIR.TI1 (), 0));
    END;
    MSIRBuilder.EmitReportFault (cond, ORD (CG.RuntimeError.NarrowFailed));
    (* Retype refVal to the destination type so assignment type-checks pass. *)
    VAR lhsT := MSIRType.Translate (tlhs);
    BEGIN
      IF lhsT # NIL THEN RETURN MSIR.RetypeValue (refVal, lhsT) END;
    END;
    RETURN refVal;
  END EmitMSIR;

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  VAR
    t    : Type.T;
    trhs := Expr.TypeOf (ce.args[0]);
    refVal : MSIR.Value;
  BEGIN
    EVAL TypeExpr.Split (ce.args[1], t);
    Type.Compile (t);
    t := Type.Base (t);
    refVal := Expr.CompileMSIR (ce.args[0]);
    IF refVal = NIL THEN RETURN NIL END;
    RETURN EmitMSIR (refVal, t, trhs);
  END CompileMSIR;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (2, 2, TRUE, FALSE, TRUE, NIL,
                                 TypeOf,
                                 RepTypeOf,
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
                                 NoteWrites);
    CallExpr.SetMethodMSIR (Z, CompileMSIR);
    Procedure.DefinePredefined ("NARROW", Z, TRUE);
  END Initialize;

BEGIN
END Narrow.

