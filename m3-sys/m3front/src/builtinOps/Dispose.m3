(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Dispose.m3                                            *)
(* Last Modified On Wed Jun 29 17:01:08 PDT 1994 By kalsow     *)
(*      Modified On Tue Feb 12 11:52:16 1991 By muller         *)

MODULE Dispose;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, RunTyme;
IMPORT Addr, Reff, Module, Error, ObjectRef, ObjectAdr, Target;
IMPORT MSIR, MSIRBuilder;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR t: Type.T;  info: Type.Info;
  BEGIN
    t := Expr.TypeOf (ce.args[0]);
    t := Type.CheckInfo (t, info);
    IF Module.IsSafe () THEN Error.Msg ("DISPOSE: unsafe operation") END;
    IF (NOT Type.IsSubtype (t, Reff.T))
      AND (NOT Type.IsSubtype (t, Addr.T)) THEN
      Error.Msg ("DISPOSE: must be applied to a reference type");
    ELSIF Type.IsEqual (t, Reff.T, NIL)
       OR Type.IsEqual (t, Addr.T, NIL)
       OR Type.IsEqual (t, ObjectRef.T, NIL)
       OR Type.IsEqual (t, ObjectAdr.T, NIL) THEN
      Error.Msg ("DISPOSE: must be applied to a fixed reference type");
    ELSIF NOT Expr.IsWritable (ce.args[0], traced := FALSE) THEN
      Error.Msg ("DISPOSE: must be applied to a writable designator");
    ELSIF NOT info.isTraced THEN
      Expr.NeedsAddress (ce.args[0]);
    END;
    ce.type := NIL;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.DisposeRef,
                                                RunTyme.Hook.DisposeObj };
  VAR
    e := ce.args[0];
    t := Type.Base (Expr.TypeOf (e));
    proc: Procedure.T;
    info: Type.Info;
  BEGIN
    Expr.PrepLValue (e, traced := FALSE);
    t := Type.CheckInfo (t, info);
    IF info.isTraced THEN
      Expr.CompileLValue (e, traced := FALSE);
      CG.Load_nil ();
      CG.Store_indirect (CG.Type.Addr, 0, Target.Address.size);
    ELSE
      proc := RunTyme.LookUpProc (PHook [Type.IsSubtype (t, ObjectAdr.T)]);
      Procedure.StartCall (proc);
      Expr.CompileAddress (e, traced := FALSE);
      CG.Pop_param (CG.Type.Addr);
      Procedure.EmitCall (proc);
    END;
    Expr.NoteWrite (e);
  END Prep;

PROCEDURE Compile (<*UNUSED*> ce: CallExpr.T) =
  BEGIN
    (* all the work was done by Prep *)
  END Compile;

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  (* Mirrors Prep: for a traced ref, store NIL into the reference; for an
     untraced ref/object, call the DisposeRef/DisposeObj runtime hook with the
     ADDRESS of the reference variable (the hook frees the block and nils it). *)
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.DisposeRef,
                                                RunTyme.Hook.DisposeObj };
  VAR
    e    := ce.args[0];
    t    := Type.Base (Expr.TypeOf (e));
    info : Type.Info;
    addr : MSIR.Value;
    proc : MSIR.Proc;
    b    : MSIR.Block;
  BEGIN
    IF NOT MSIRBuilder.InProc () THEN RETURN NIL END;
    t := Type.CheckInfo (t, info);
    (* Address of the reference variable (pointer to the ref/adr slot). *)
    addr := Expr.LValueMSIR (e);
    IF addr = NIL THEN RETURN NIL END;
    b := MSIRBuilder.CurrentBlock ();
    IF info.isTraced THEN
      (* Set the reference to NIL.  Storing NIL never needs a write barrier. *)
      IF MSIR.Kind (MSIR.ValueType (addr)) = MSIR.TypeKind.GcSlot THEN
        MSIR.BuildGcStore (b, addr,
                           MSIR.ConstNil (MSIR.TGcRef (MSIR.TVoid ())), NIL);
      ELSE
        MSIR.BuildStore (b, MSIR.ConstNil (MSIR.TPtr (MSIR.TVoid ())), addr);
      END;
    ELSE
      proc := MSIRBuilder.HookProc (PHook [Type.IsSubtype (t, ObjectAdr.T)]);
      IF proc = NIL THEN
        MSIRBuilder.Abandon ("DISPOSE: hook not available");  RETURN NIL;
      END;
      EVAL MSIRBuilder.EmitCall ("", proc, ARRAY OF MSIR.Value{addr});
    END;
    Expr.NoteWrite (e);
    RETURN NIL;
  END CompileMSIR;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, 1, FALSE, FALSE, TRUE, NIL,
                                 NIL, NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 CallExpr.NotBoolean,
                                 CallExpr.NotBoolean,
                                 CallExpr.NoValue,
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    CallExpr.SetMethodMSIR (Z, CompileMSIR);
    Procedure.DefinePredefined ("DISPOSE", Z, TRUE);
  END Initialize;

BEGIN
END Dispose.
