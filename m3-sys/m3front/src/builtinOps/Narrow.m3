(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Narrow.m3                                             *)
(* Last Modified On Wed Jun 29 17:29:43 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:20 1990 By muller         *)

MODULE Narrow;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Error, TypeExpr;
IMPORT Procedure, ObjectType, Reff, Null, M3RT, RefType;
IMPORT Target, RunTyme, TInt;
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

PROCEDURE EmitMSIR (refVal: MSIR.Value;  tlhs, trhs: Type.T): MSIR.Value =
  VAR
    lhs_info              : Type.Info;
    tc, checkVal          : MSIR.Value;
    hook                  : MSIR.Proc;
    okBlk, failBlk        : MSIR.Block;
  BEGIN
    EVAL Type.CheckInfo (tlhs, lhs_info);

    (* Static: trhs <: tlhs → no check needed *)
    IF Type.IsSubtype (trhs, tlhs) THEN RETURN refVal END;

    (* Untraced, non-object → cast only, no runtime check *)
    IF (NOT ObjectType.Is (tlhs)) AND (NOT lhs_info.isTraced) THEN
      RETURN refVal;
    END;

    (* General: call RTHooks__CheckIsType(ref, typecell) *)
    IF ObjectType.Is (tlhs) THEN
      tc := MSIRBuilder.TypeLinkValueForObject (tlhs);
    ELSE
      tc := MSIRBuilder.TypeLinkValueForRef (tlhs);
    END;
    IF tc = NIL THEN
      MSIRBuilder.Abandon ("NARROW: cannot get typecell");  RETURN NIL;
    END;
    hook := MSIRBuilder.HookProc (RunTyme.Hook.CheckIsType);
    IF hook = NIL THEN
      MSIRBuilder.Abandon ("NARROW: CheckIsType hook missing");  RETURN NIL;
    END;
    checkVal := MSIRBuilder.EmitCall ("narrow.chk", hook,
                                      ARRAY OF MSIR.Value {refVal, tc});
    IF checkVal = NIL THEN RETURN NIL END;

    (* CheckIsType returns INTEGER; convert to i1 for conditional branch. *)
    VAR cond := MSIR.BuildICmp (MSIRBuilder.CurrentBlock (), "narrow.cond",
                                MSIR.CmpPred.Ne,
                                checkVal,
                                MSIR.ConstInt (MSIR.ValueType (checkVal), 0));
    BEGIN
      okBlk   := MSIRBuilder.NewBlock ("narrow.ok");
      failBlk := MSIRBuilder.NewBlock ("narrow.fail");
      MSIR.BuildCondBr (MSIRBuilder.CurrentBlock (), cond,
                        okBlk,   ARRAY OF MSIR.Value {},
                        failBlk, ARRAY OF MSIR.Value {});
    END;

    (* Abort branch: RTHooks__ReportFault(NIL, NarrowFailed=5) + unreachable *)
    MSIRBuilder.SetCurrentBlock (failBlk);
    VAR faultHook := MSIRBuilder.HookProc (RunTyme.Hook.Abort);
    BEGIN
      IF faultHook # NIL THEN
        EVAL MSIRBuilder.EmitCall ("", faultHook,
               ARRAY OF MSIR.Value {
                 MSIR.ConstNil (MSIR.TPtr (MSIR.TVoid ())),
                 MSIR.ConstInt (MSIR.TI (Target.Integer.size), 5)});
      END;
    END;
    MSIR.BuildUnreachable (MSIRBuilder.CurrentBlock ());

    MSIRBuilder.SetCurrentBlock (okBlk);
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

