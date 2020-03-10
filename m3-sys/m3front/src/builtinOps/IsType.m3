(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: IsType.m3                                             *)
(* Last Modified On Tue May  3 16:31:06 PDT 1994 By kalsow     *)
(*      Modified On Sat Dec  8 00:54:22 1990 By muller         *)

MODULE IsType;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Error, TypeExpr, Reff, RefType;
IMPORT Procedure, Bool, ObjectType, Null, Value, M3RT, Target, RunTyme;
IMPORT TInt;

VAR Z: CallExpr.MethodList;

PROCEDURE Check (ce: CallExpr.T;  <*UNUSED*> VAR cs: Expr.CheckState) =
  VAR t, u: Type.T;
  BEGIN
    IF  NOT TypeExpr.Split (ce.args[1], t) THEN
      Error.Msg ("ISTYPE: second argument must be a type");
      t := Expr.TypeOf (ce.args[0]);
    END;
    t := Type.Base (t);
    u := Expr.TypeOf (ce.args[0]);

    IF NOT Type.IsAssignable (t, u) THEN
      Error.Msg ("ISTYPE: types must be assignable");
    ELSIF ObjectType.Is (t) OR Type.IsSubtype (t, Reff.T) THEN
      (* ok *)
    ELSE (* untraced ref type *)
      Error.Msg ("ISTYPE: must be a traced reference or object type");
    END;

    ce.type := Bool.T;
  END Check;

PROCEDURE Prep (ce: CallExpr.T) =
  VAR
    e := ce.args[0];
    t, u: Type.T;
    ptr: CG.Val;
    true, false, tagged: CG.Label;
    proc: Procedure.T;
  BEGIN
    IF NOT TypeExpr.Split (ce.args[1], t) THEN
      t := Expr.TypeOf (e);
    END;
    Type.Compile (t);
    t := Type.Base (t);
    u := Expr.TypeOf (e);

    Expr.Prep (ce.args[0]);
    IF Type.IsSubtype (u, t) THEN
      (* the test succeeds statically *)
      Expr.Compile (ce.args[0]);
      CG.Discard (CG.Type.Addr);
      Value.Load (Bool.True);
      ce.tmp := CG.Pop ();

    ELSIF Type.IsEqual (t, Null.T, NIL) THEN
      Expr.Compile (ce.args[0]);
      CG.Load_nil ();
      CG.Compare (CG.Type.Addr, CG.Cmp.EQ);
      ce.tmp := CG.Pop ();

    ELSIF RefType.Is (t) THEN
      Expr.Compile (ce.args[0]);
      tagged := CG.Next_label ();
      false := CG.Next_label ();
      true := CG.Next_label ();
      ptr := CG.Pop ();
      Value.Load (Bool.True);
      CG.ForceStacked (); (* we need a temp *)
      ce.tmp := CG.Pop_temp ();
      CG.Push (ptr);
      CG.Load_nil ();
      CG.If_compare (CG.Type.Addr, CG.Cmp.EQ, true, CG.Maybe);

      CG.Push (ptr);
      CG.Loophole (CG.Type.Addr, Target.Word.cg_type);
      CG.Load_integer (Target.Word.cg_type, TInt.One);
      CG.And (Target.Word.cg_type);
      CG.If_true (tagged, CG.Maybe);

      CG.Push (ptr);
      CG.Ref_to_info (M3RT.RH_typecode_offset, M3RT.RH_typecode_size);
      Type.LoadInfo (t, M3RT.TC_typecode);
      CG.If_compare (Target.Integer.cg_type, CG.Cmp.EQ, true, CG.Always);
      CG.Jump (false);
      
      CG.Set_label (tagged);
      CG.Load_intt (M3RT.REFANY_typecode);
      Type.LoadInfo (t, M3RT.TC_typecode);
      CG.If_compare (Target.Integer.cg_type, CG.Cmp.EQ, true, CG.Always);

      CG.Set_label (false);
      Value.Load (Bool.False);
      CG.Store_temp (ce.tmp);

      CG.Set_label (true);
      CG.Free (ptr);

    ELSE (* general object type *)
      proc := RunTyme.LookUpProc (RunTyme.Hook.CheckIsType);
      Procedure.StartCall (proc);
      IF Target.DefaultCall.args_left_to_right THEN
        Expr.Compile (ce.args[0]);
        CG.Pop_param (CG.Type.Addr);
        Type.LoadInfo (t, -1);
        CG.Pop_param (CG.Type.Addr);
      ELSE
        Type.LoadInfo (t, -1);
        CG.Pop_param (CG.Type.Addr);
        Expr.Compile (ce.args[0]);
        CG.Pop_param (CG.Type.Addr);
      END;
      ce.tmp := Procedure.EmitValueCall (proc);
    END;
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    (* all the work was done by Prep *)
    CG.Push (ce.tmp);
    CG.Free (ce.tmp);
    ce.tmp := NIL;
  END Compile;

PROCEDURE PrepBR (ce: CallExpr.T;  true, false: CG.Label;  freq: CG.Frequency)=
  VAR
    e := ce.args[0];
    t, u: Type.T;
    ptr: CG.Val;
    skip, tagged: CG.Label;
    proc: Procedure.T;
  BEGIN
    IF NOT TypeExpr.Split (ce.args[1], t) THEN
      t := Expr.TypeOf (e);
    END;
    Type.Compile (t);
    t := Type.Base (t);
    u := Expr.TypeOf (e);

    Expr.Prep (ce.args[0]);
    IF Type.IsSubtype (u, t) THEN
      (* the test succeeds statically *)
      Expr.Compile (ce.args[0]);
      CG.Discard (CG.Type.Addr);
      IF (true # CG.No_label)
        THEN CG.Jump (true);
      (*ELSE fall through*)
      END;

    ELSIF Type.IsEqual (t, Null.T, NIL) THEN
      Expr.Compile (ce.args[0]);
      CG.Load_nil ();
      CG.If_then (CG.Type.Addr, CG.Cmp.EQ, true, false, freq);

    ELSIF RefType.Is (t) THEN
      Expr.Compile (ce.args[0]);
      tagged := CG.Next_label ();
      skip := CG.Next_label ();
      ptr := CG.Pop ();
      CG.Push (ptr);
      CG.Load_nil ();
      IF (true # CG.No_label)
        THEN CG.If_compare (CG.Type.Addr, CG.Cmp.EQ, true, CG.Maybe);
        ELSE CG.If_compare (CG.Type.Addr, CG.Cmp.EQ, skip, CG.Maybe);
      END;

      CG.Push (ptr);
      CG.Loophole (CG.Type.Addr, Target.Word.cg_type);
      CG.Load_integer (Target.Word.cg_type, TInt.One);
      CG.And (Target.Word.cg_type);
      CG.If_true (tagged, CG.Maybe);

      CG.Push (ptr);
      CG.Ref_to_info (M3RT.RH_typecode_offset, M3RT.RH_typecode_size);
      Type.LoadInfo (t, M3RT.TC_typecode);
      CG.If_then (Target.Integer.cg_type, CG.Cmp.EQ, true, false, freq);
      CG.Jump (skip);

      CG.Set_label (tagged);
      CG.Load_intt (M3RT.REFANY_typecode);
      Type.LoadInfo (t, M3RT.TC_typecode);
      CG.If_then (Target.Integer.cg_type, CG.Cmp.EQ, true, false, freq);
      CG.Set_label (skip);
      CG.Free (ptr);

    ELSE (* general object type *)
      proc := RunTyme.LookUpProc (RunTyme.Hook.CheckIsType);
      Procedure.StartCall (proc);
      IF Target.DefaultCall.args_left_to_right THEN
        Expr.Compile (ce.args[0]);
        CG.Pop_param (CG.Type.Addr);
        Type.LoadInfo (t, -1);
        CG.Pop_param (CG.Type.Addr);
      ELSE
        Type.LoadInfo (t, -1);
        CG.Pop_param (CG.Type.Addr);
        Expr.Compile (ce.args[0]);
        CG.Pop_param (CG.Type.Addr);
      END;
      Procedure.EmitCall (proc);
      IF (true # CG.No_label)
        THEN CG.If_true (true, CG.Always);
        ELSE CG.If_false (false, CG.Never);
      END;
    END;
  END PrepBR;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (2, 2, TRUE, FALSE, TRUE, Bool.T,
                                 NIL, NIL,
                                 CallExpr.NotAddressable,
                                 Check,
                                 Prep,
                                 Compile,
                                 CallExpr.NoLValue,
                                 CallExpr.NoLValue,
                                 PrepBR,
                                 CallExpr.NoBranch,
                                 CallExpr.NoValue, (* fold *)
                                 CallExpr.NoBounds,
                                 CallExpr.IsNever, (* writable *)
                                 CallExpr.IsNever, (* designator *)
                                 CallExpr.NotWritable (* noteWriter *));
    Procedure.DefinePredefined ("ISTYPE", Z, TRUE);
  END Initialize;

BEGIN
END IsType.
