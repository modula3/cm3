(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AssignStmt.m3                                         *)
(* Last modified on Fri Jul  7 15:10:54 PDT 1995 by kalsow     *)
(*      modified on Thu Jun 15 15:12:38 PDT 1995 by ericv      *)
(*      modified on Fri Dec 21 01:24:28 1990 by muller         *)

MODULE AssignStmt;

IMPORT CG, Stmt, StmtRep, Expr, Type, Error, Module, Target, TInt;
IMPORT Token, Scanner, CallStmt, Addr, CheckExpr, ErrType;
IMPORT M3ID, Value, NamedExpr, ArrayType, ConsExpr;
IMPORT QualifyExpr, Variable, Procedure, OpenArrayType;
IMPORT ProcExpr, ProcType, ObjectType, CallExpr, Host, Narrow;

TYPE
  P = Stmt.T OBJECT
        lhs     : Expr.T;
        rhs     : Expr.T;
      OVERRIDES
        check       := CheckMethod;
        compile     := Compile;
        outcomes    := GetOutcome;
      END;

PROCEDURE Parse (): Stmt.T =
  VAR e: Expr.T;  p: P;  s: Stmt.T;  here := Scanner.offset;
  BEGIN
    e := Expr.Parse ();
    IF (Scanner.cur.token # Token.T.tASSIGN) THEN
      IF NOT CallExpr.Is (e) THEN
        Error.Msg ("Expression is not a statement");
      END;
      s := CallStmt.New (e);
      s.origin := here;
      RETURN s;
    END;

    p := NEW (P);
    StmtRep.Init (p);
    p.origin := here;
    Scanner.GetToken (); (* := *)
    p.lhs := e;
    p.rhs := Expr.Parse ();
    RETURN p;
  END Parse;

PROCEDURE CheckMethod (p: P;  VAR cs: Stmt.CheckState) =
  VAR tlhs: Type.T;  rhs_info: Type.Info;
  BEGIN
    Expr.TypeCheck (p.lhs, cs);
    Expr.TypeCheck (p.rhs, cs);

    tlhs := Expr.TypeOf (p.lhs);
    IF  NOT Expr.IsDesignator (p.lhs) THEN
      Error.Msg ("left-hand side is not a designator");
    ELSE
      EVAL Type.CheckInfo (Expr.TypeOf (p.rhs), rhs_info);
      IF NOT Expr.IsWritable (p.lhs, rhs_info.isTraced) THEN
        Error.Msg ("left-hand side is read-only");
      END;
    END;

    Check (tlhs, p.rhs, cs);
  END CheckMethod;

PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR
    tlhs := Expr.TypeOf (p.lhs);
    rhs_info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (Expr.TypeOf (p.rhs), rhs_info);
    Expr.PrepLValue (p.lhs, traced := rhs_info.isTraced);
    PrepForEmit (tlhs, p.rhs, initializing := FALSE);
    Expr.CompileLValue (p.lhs, traced := rhs_info.isTraced);
    DoEmit (tlhs, p.rhs, Expr.Alignment (p.lhs));
    Expr.NoteWrite (p.lhs);
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END Compile;

PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END GetOutcome;


(*--------------------------------------------------------- type checking ---*)

PROCEDURE Check (tlhs: Type.T;  rhs: Expr.T;  VAR cs: Stmt.CheckState) =
  VAR
    base_tlhs := Type.Base (tlhs); (* strip renaming and packing *)
    trhs := Expr.TypeOf (rhs);
    lhs_info, base_lhs_info: Type.Info;
    c: Type.Class;
  BEGIN
    tlhs := Type.CheckInfo (tlhs, lhs_info);
    base_tlhs := Type.CheckInfo (base_tlhs, base_lhs_info);
    c := base_lhs_info.class;
    Expr.TypeCheck (rhs, cs);

    IF NOT Type.IsAssignable (tlhs, trhs) THEN
      IF (tlhs # ErrType.T) AND (trhs # ErrType.T) THEN
        Error.Msg ("types are not assignable in assignment statement");
      END;

    ELSIF (Type.IsOrdinal (base_tlhs)) THEN
      CheckOrdinal (tlhs, rhs);

    ELSIF (c = Type.Class.Ref) OR (c = Type.Class.Object)
       OR (c = Type.Class.Opaque) THEN
      CheckReference (tlhs, trhs, lhs_info);

    ELSIF (c = Type.Class.Procedure) THEN
      CheckProcedure (rhs);

    ELSE
      (* ok *)

    END;
  END Check;

PROCEDURE CheckOrdinal (tlhs: Type.T;  rhs: Expr.T) =
  VAR lmin, lmax, rmin, rmax: Target.Int;  constant: Expr.T;
  BEGIN
    (* ok, but must generate a check *)
    constant := Expr.ConstValue (rhs);
    IF (constant # NIL) THEN rhs := constant END;
    Expr.GetBounds (rhs, rmin, rmax);
    EVAL Type.GetBounds (tlhs, lmin, lmax);
    IF TInt.LE (lmin, lmax) AND TInt.LE (rmin, rmax)
      AND (TInt.LT (lmax, rmin) OR TInt.LT (rmax, lmin)) THEN
      (* non-overlapping, non-empty ranges *)
      Error.Warn (2, "value not assignable (range fault)");
    END;
  END CheckOrdinal;

PROCEDURE CheckReference (tlhs, trhs: Type.T;  READONLY lhs_info: Type.Info) =
  BEGIN
    IF Type.IsSubtype (trhs, tlhs) THEN
      (*ok*)
    ELSIF NOT Type.IsSubtype (tlhs, trhs) THEN
      Error.Msg ("types are not assignable");
    ELSIF Type.IsEqual (trhs, Addr.T, NIL) THEN 
      (* that is legal only in UNSAFE modules *)
      IF Module.IsSafe() THEN Error.Msg ("unsafe implicit NARROW"); END;
    ELSIF ObjectType.Is (trhs) THEN
      (*ok*)
    ELSIF lhs_info.isTraced THEN
      (*ok*)
    ELSE
      Error.Msg ("types are not assignable");
    END;
  END CheckReference;

PROCEDURE CheckProcedure (rhs: Expr.T) =
  BEGIN
    IF NeedsClosureCheck (rhs, TRUE) THEN
      (* may generate a more detailed message *)
    END;
  END CheckProcedure;

PROCEDURE NeedsClosureCheck (proc: Expr.T;  errors: BOOLEAN): BOOLEAN =
  VAR name: M3ID.T;  obj: Value.T;  class: Value.Class;  nested: BOOLEAN;
  BEGIN
    IF NOT (NamedExpr.Split (proc, name, obj)
            OR QualifyExpr.Split (proc, obj)
            OR ProcExpr.Split (proc, obj)) THEN
      (* non-constant, non-variable => OK *)
      RETURN FALSE;
    END;
    obj := Value.Base (obj);
    class := Value.ClassOf (obj);
    IF (class = Value.Class.Procedure) THEN
      nested := Procedure.IsNested (obj);
      IF (nested) AND (errors) THEN
        Error.ID (Value.CName (obj), "cannot assign nested procedures");
      END;
      RETURN FALSE;
    ELSIF (class = Value.Class.Var) AND Variable.HasClosure (obj) THEN
      RETURN TRUE;
    ELSE (* non-formal, non-const => no check *)
      RETURN FALSE;
    END;
  END NeedsClosureCheck;

(*------------------------------------------------------- code generation ---*)

PROCEDURE PrepForEmit (tlhs: Type.T;  rhs: Expr.T;  initializing: BOOLEAN) =
  (* When the rhs has the potential to assign its result directly into
     a given destination, try to avoid explicit copying.  Currently this
     means large-result procedure calls and array and record constructors. *)
  BEGIN
    IF Host.direct_struct_assign
      AND Expr.SupportsDirectAssignment (rhs)
      AND CanAvoidCopy (tlhs, rhs, initializing)
    THEN
      Expr.MarkForDirectAssignment (rhs);
    ELSE
      Expr.Prep (rhs);
    END;
  END PrepForEmit;

PROCEDURE CanAvoidCopy (tlhs: Type.T;  rhs: Expr.T;  initializing: BOOLEAN): BOOLEAN =
  VAR
    t      : Type.T;
    t_info : Type.Info;
    r_info : Type.Info;
    base   : Expr.T;
  BEGIN
    t := Type.Base (tlhs); (* strip renaming and packing *)
    t := Type.CheckInfo (t, t_info);
    IF NOT ProcType.LargeResult (t) THEN
      RETURN FALSE;
    END;

    (* Only attempt to avoid copying records and fixed-length arrays *)
    CASE t_info.class OF
    | Type.Class.Array =>
        (* i.e. lhs is not Type.Class.OpenArray -- check rhs *)
        IF OpenArrayType.Is (Expr.TypeOf (rhs)) THEN  RETURN FALSE;  END;
        (* drop out of CASE *)
    | Type.Class.Record =>
        (* drop out of CASE *)
    ELSE
        RETURN FALSE;
    END;

    (* make sure the source and destination are both aligned properly *)
    EVAL Type.CheckInfo (tlhs, t_info);
    EVAL Type.CheckInfo (Expr.TypeOf (rhs), r_info);
    IF (t_info.alignment # r_info.alignment) THEN RETURN FALSE; END;
    IF (t_info.class # r_info.class)         THEN RETURN FALSE; END;
    IF (t_info.class = Type.Class.Packed)    THEN RETURN FALSE; END;
    IF (r_info.class = Type.Class.Packed)    THEN RETURN FALSE; END;

    IF CallExpr.Is (rhs) THEN
      (* For user procedures, we can always pass in the true destination.
         It is the callee's responsibility not to assign to this location
         until the final procedure outcome is known, and not to overwrite
         the contents until the entire result value has been computed. *)
      RETURN CallExpr.IsUserProc (rhs);
    END;

    (* If the lhs contents are uninitialized (i.e. the Modula-3 spec
       only guarantees that the contents will be a member of its type),
       then we can write to the final destination incrementally without
       worrying about exceptions or references to the original contents. *)

    IF NOT initializing THEN  RETURN FALSE;  END;

    IF ConsExpr.Is (rhs) AND Expr.ConstValue (rhs) = NIL THEN
      base := ConsExpr.Base (rhs);
      IF Expr.SupportsDirectAssignment (base) THEN
        Expr.MarkForDirectAssignment (base);
        RETURN TRUE;
      END;
    END;

    RETURN FALSE;
  END CanAvoidCopy;

PROCEDURE DoEmit (tlhs: Type.T;  rhs: Expr.T; lhs_align := Target.Byte) =
  (* on entry the lhs is compiled and the rhs is prepped,
     preferrably using PrepForEmit() above. *)
  VAR
    t_lhs_base := Type.Base (tlhs); (* strip renaming, packing, and subrange *)
    lhs_info, t_lhs_base_info: Type.Info;
  BEGIN
    t_lhs_base := Type.CheckInfo (t_lhs_base, t_lhs_base_info);
    tlhs := Type.CheckInfo (tlhs, lhs_info);

    IF Expr.IsMarkedForDirectAssignment (rhs) THEN
      (* Do the prep now that we have the LHS compiled *)
      Expr.Prep (rhs);
      Expr.Compile (rhs);
      CG.Discard (Type.CGType (Expr.TypeOf (rhs)));
      RETURN;
    END;

    CASE t_lhs_base_info.class OF
    | Type.Class.Integer, Type.Class.Longint, Type.Class.Subrange,
      Type.Class.Enum =>
        AssignOrdinal (tlhs, rhs, lhs_info);
    | Type.Class.Real, Type.Class.Longreal, Type.Class.Extended =>
        AssignFloat (rhs, lhs_info);
    | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
        AssignReference (tlhs, rhs, lhs_info);
    | Type.Class.Array, Type.Class.OpenArray =>
        AssignArray (tlhs, rhs, lhs_info, lhs_align);
    | Type.Class.Procedure =>
        AssignProcedure (rhs, lhs_info);
    | Type.Class.Record =>
        AssignRecord (tlhs, rhs, lhs_info, lhs_align);
    | Type.Class.Set =>
        AssignSet (tlhs, rhs, lhs_info);
    | Type.Class.Error =>
    ELSE <*ASSERT FALSE*>
    END;
  END DoEmit;

PROCEDURE AssignOrdinal (tlhs: Type.T;  rhs: Expr.T;
                         READONLY lhs_info: Type.Info) =
  VAR min, max : Target.Int;
  BEGIN
    EVAL Type.GetBounds (tlhs, min, max);
    CheckExpr.EmitChecks (rhs, min, max, CG.RuntimeError.ValueOutOfRange);
    CG.Store_indirect (lhs_info.stk_type, 0, lhs_info.size);
  END AssignOrdinal;

PROCEDURE AssignFloat (rhs: Expr.T;  READONLY lhs_info: Type.Info) =
  BEGIN
    Expr.Compile (rhs);
    CG.Store_indirect (lhs_info.stk_type, 0, lhs_info.size);
  END AssignFloat;

PROCEDURE AssignReference (tlhs: Type.T;  rhs: Expr.T;
                           READONLY lhs_info: Type.Info) =
  VAR lhs: CG.Val;
  BEGIN
    lhs := CG.Pop ();
    Expr.Compile (rhs);
    IF Host.doNarrowChk THEN Narrow.Emit (tlhs, Expr.TypeOf (rhs)) END;
    CG.Push (lhs);
    CG.Swap ();
    CG.Store_indirect (lhs_info.stk_type, 0, lhs_info.size);
    CG.Free (lhs);
  END AssignReference;

PROCEDURE AssignProcedure (rhs: Expr.T;  READONLY lhs_info: Type.Info) =
  VAR ok: CG.Label;  lhs, t1: CG.Val;
  BEGIN
    IF NOT Host.doNarrowChk THEN
      Expr.Compile (rhs);
    ELSIF NOT NeedsClosureCheck (rhs, FALSE) THEN
      Expr.Compile (rhs);
    ELSE
      lhs := CG.Pop ();
      Expr.Compile (rhs);
      t1 := CG.Pop ();
      ok := CG.Next_label ();
      CG.If_closure (t1, CG.No_label, ok, CG.Always);
      CG.Abort (CG.RuntimeError.NarrowFailed);
      CG.Set_label (ok);
      CG.Push (t1);  CG.Free (t1);
      CG.Push (lhs);
      CG.Swap ();
      CG.Free (lhs);
    END;
    CG.Store_indirect (lhs_info.stk_type, 0, lhs_info.size);
  END AssignProcedure;

PROCEDURE AssignRecord
  (tlhs: Type.T; rhs: Expr.T; READONLY lhs_info: Type.Info; lhs_align: INTEGER) =
  BEGIN
    AssertSameSize (tlhs, Expr.TypeOf (rhs));
    IF Expr.IsDesignator (rhs)
      THEN Expr.CompileLValue (rhs, traced := FALSE);
      ELSE Expr.Compile (rhs);
    END;
    IF lhs_align < Target.Byte OR lhs_info.size MOD Target.Byte # 0 THEN 
      CG.Load_indirect (Target.Word.cg_type, 0 , lhs_info.size); 
      CG.Store_indirect (Target.Word.cg_type, 0 , lhs_info.size); 
    ELSE
      CG.Copy (lhs_info.size, overlap := FALSE);
    END; 
  END AssignRecord;

PROCEDURE AssignSet (tlhs: Type.T;  rhs: Expr.T;
                     READONLY lhs_info: Type.Info) =
  BEGIN
    AssertSameSize (tlhs, Expr.TypeOf (rhs));
    IF Type.IsStructured (tlhs) THEN
      IF Expr.IsDesignator (rhs)
        THEN Expr.CompileLValue (rhs, traced := FALSE);
        ELSE Expr.Compile (rhs);
      END;
      CG.Copy (lhs_info.size, overlap := FALSE);
    ELSE (* small set *)
      Expr.Compile (rhs);
      CG.Store_indirect (lhs_info.stk_type, 0, lhs_info.size);
    END;
  END AssignSet;

PROCEDURE AssertSameSize (a, b: Type.T) = 
  VAR a_info, b_info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (a, a_info);
    EVAL Type.CheckInfo (b, b_info);
    IF (a_info.size # b_info.size) THEN
      Error.Msg ("INTERNAL ERROR: trying to assign values of differing sizes");
      <* ASSERT FALSE *>
    END;
  END AssertSameSize;

PROCEDURE AssignArray (tlhs: Type.T;  e_rhs: Expr.T;
                       READONLY lhs_info: Type.Info;
                       lhs_align: INTEGER) =
  VAR
    trhs    := Expr.TypeOf (e_rhs);
    openRHS := OpenArrayType.Is (trhs);
    openLHS := OpenArrayType.Is (tlhs);
    lhs, rhs: CG.Val;
    eltCt : INTEGER;
  BEGIN
    (* Capture the lhs & rhs pointers in temps. *)
    IF (openRHS) OR (openLHS) THEN lhs := CG.Pop (); END;
    IF Expr.IsDesignator (e_rhs)
      THEN Expr.CompileLValue (e_rhs, traced := FALSE);
      ELSE Expr.Compile (e_rhs);
    END;
    IF (openRHS) OR (openLHS) THEN rhs := CG.Pop (); END;

    IF openRHS AND openLHS THEN
      GenOpenArraySizeChecks (lhs, rhs, tlhs, trhs, (*OUT*) eltCt);
      CG.Push (lhs);
      CG.Open_elt_ptr (lhs_align);
      CG.ForceStacked ();
      CG.Push (rhs);
      CG.Open_elt_ptr (Expr.Alignment(e_rhs));
      CG.ForceStacked ();
      GenOpenArrayCopy (rhs, tlhs, trhs);

    ELSIF openRHS THEN
      GenOpenArraySizeChecks (lhs, rhs, tlhs, trhs, (*OUT*) eltCt);
      CG.Push (lhs);
      CG.Push (rhs);
      CG.Open_elt_ptr (Expr.Alignment(e_rhs));
      CG.Copy (eltCt * ArrayType.EltPack(trhs), overlap := TRUE);

    ELSIF openLHS THEN
      GenOpenArraySizeChecks (lhs, rhs, tlhs, trhs, (*OUT*) eltCt);
      CG.Push (lhs);
      CG.Open_elt_ptr (lhs_align);
      CG.Push (rhs);
      CG.Copy (eltCt *ArrayType.EltPack(trhs), overlap := TRUE);

    ELSE (* both sides are fixed length arrays *)
      CG.Copy (lhs_info.size, overlap := TRUE);
      (* Note: overlap = TRUE because aliased VAR parameters can hide
         the open arrays produced by SUBARRAY behind fixed array formal
         parameters. *)
    END;

    IF (openRHS) OR (openLHS) THEN
      CG.Free (lhs);
      CG.Free (rhs);
    END;
  END AssignArray;

PROCEDURE GenOpenArraySizeChecks (READONLY lhs, rhs: CG.Val;
                                  tlhs, trhs: Type.T;
                                  VAR eltCt: INTEGER) =
  VAR ilhs, irhs, elhs, erhs: Type.T;  n := 0;
  VAR eltCtTInt: TInt.Int;
  VAR b: BOOLEAN; 
  BEGIN
    eltCt := 0; 
    WHILE ArrayType.Split (tlhs, ilhs, elhs)
      AND ArrayType.Split (trhs, irhs, erhs) DO

      IF (ilhs # NIL) AND (irhs # NIL) THEN (* Neither is open in this dimension. *) 
        eltCtTInt := Type.Number (ilhs);
        b := TInt.ToInt (eltCtTInt, eltCt); <*ASSERT b*> 
        RETURN;
      ELSIF (ilhs # NIL) THEN (* Only lhs is open in this dimension. *) 
        CG.Push (rhs);
        CG.Open_size (n);
        eltCtTInt := Type.Number (ilhs);
        b := TInt.ToInt (eltCtTInt, eltCt); <*ASSERT b*> 
        (* ^In case this turns out to be the innermost dimension. *)
        IF Host.doNarrowChk THEN
          CG.Load_integer (Target.Integer.cg_type, eltCtTInt);
          CG.Check_eq (Target.Integer.cg_type, CG.RuntimeError.IncompatibleArrayShape);
        END
      ELSIF (irhs # NIL) THEN (* Only rhs is open in this dimension. *)
        CG.Push (lhs);
        CG.Open_size (n);
        eltCtTInt := Type.Number (irhs);
        b := TInt.ToInt (eltCtTInt, eltCt); <*ASSERT b*>
        (* ^In case this turns out to be the innermost dimension. *) 
        IF Host.doNarrowChk THEN
          CG.Load_integer (Target.Integer.cg_type, eltCtTInt);
          CG.Check_eq (Target.Integer.cg_type, CG.RuntimeError.IncompatibleArrayShape);
        END
      ELSE (* both arrays are open *)
        IF Host.doNarrowChk THEN
          CG.Push (lhs);
          CG.Open_size (n);
          CG.Push (rhs);
          CG.Open_size (n);
          CG.Check_eq (Target.Integer.cg_type, CG.RuntimeError.IncompatibleArrayShape);
        END
      END;
      INC (n);
      tlhs := elhs;
      trhs := erhs;
    END;
  END GenOpenArraySizeChecks;

PROCEDURE GenOpenArrayCopy (READONLY rhs: CG.Val;  tlhs, trhs: Type.T) =
  VAR
    lhs_depth := OpenArrayType.OpenDepth (tlhs);
    rhs_depth := OpenArrayType.OpenDepth (trhs);
  BEGIN
    <*ASSERT (lhs_depth > 0) AND (rhs_depth > 0) *>

    FOR i := 0 TO MIN (lhs_depth, rhs_depth) - 1 DO
      CG.Push (rhs);
      CG.Open_size (i);
      IF (i # 0) THEN CG.Multiply (Target.Word.cg_type) END;
    END;

    IF (lhs_depth < rhs_depth)
      THEN CG.Copy_n (OpenArrayType.EltPack (tlhs), overlap := TRUE);
      ELSE CG.Copy_n (OpenArrayType.EltPack (trhs), overlap := TRUE);
    END;
  END GenOpenArrayCopy;

(*---------------------------------------- code generation: checking only ---*)

PROCEDURE DoEmitCheck (tlhs: Type.T;  rhs: Expr.T) =
  (* on entry the lhs is compiled and the rhs is prepped. *)
  VAR
    t_lhs_base := Type.Base (tlhs); (* strip renaming and packing *)
    lhs_info, t_lhs_base_info: Type.Info;
  BEGIN
    t_lhs_base := Type.CheckInfo (t_lhs_base, t_lhs_base_info);
    tlhs := Type.CheckInfo (tlhs, lhs_info);

    CASE t_lhs_base_info.class OF
    | Type.Class.Integer, Type.Class.Longint, Type.Class.Subrange,
      Type.Class.Enum =>
        DoCheckOrdinal (tlhs, rhs);
    | Type.Class.Real, Type.Class.Longreal, Type.Class.Extended =>
        DoCheckFloat (rhs);
    | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
        DoCheckReference (tlhs, rhs);
    | Type.Class.Array, Type.Class.OpenArray =>
        DoCheckArray (tlhs, rhs);
    | Type.Class.Procedure =>
        DoCheckProcedure (rhs);
    | Type.Class.Record =>
        DoCheckRecord (tlhs, rhs);
    | Type.Class.Set =>
        DoCheckSet (tlhs, rhs);
    ELSE <* ASSERT FALSE *>
    END;
  END DoEmitCheck;

PROCEDURE DoCheckOrdinal (tlhs: Type.T;  rhs: Expr.T) =
  VAR min, max : Target.Int;
  BEGIN
    EVAL Type.GetBounds (tlhs, min, max);
    CheckExpr.EmitChecks (rhs, min, max, CG.RuntimeError.ValueOutOfRange);
  END DoCheckOrdinal;

PROCEDURE DoCheckFloat (rhs: Expr.T) =
  BEGIN
    Expr.Compile (rhs);
  END DoCheckFloat;

PROCEDURE DoCheckReference (tlhs: Type.T;  rhs: Expr.T) =
  BEGIN
    Expr.Compile (rhs);
    IF Host.doNarrowChk THEN Narrow.Emit (tlhs, Expr.TypeOf (rhs)) END;
  END DoCheckReference;

PROCEDURE DoCheckProcedure (rhs: Expr.T) =
  VAR ok: CG.Label;  t1: CG.Val;
  BEGIN
    IF NOT Host.doNarrowChk THEN
      Expr.Compile (rhs);
    ELSIF NOT NeedsClosureCheck (rhs, FALSE) THEN
      Expr.Compile (rhs);
    ELSE
      Expr.Compile (rhs);
      t1 := CG.Pop ();
      ok := CG.Next_label ();
      CG.If_closure (t1, CG.No_label, ok, CG.Always);
      CG.Abort (CG.RuntimeError.NarrowFailed);
      CG.Set_label (ok);
      CG.Push (t1);  CG.Free (t1);
    END;
  END DoCheckProcedure;

PROCEDURE DoCheckRecord (tlhs: Type.T;  rhs: Expr.T) =
  BEGIN
    AssertSameSize (tlhs, Expr.TypeOf (rhs));
    IF Expr.IsDesignator (rhs)
      THEN Expr.CompileLValue (rhs, traced := FALSE);
      ELSE Expr.Compile (rhs);
    END;
  END DoCheckRecord;

PROCEDURE DoCheckSet (tlhs: Type.T;  rhs: Expr.T) =
  BEGIN
    AssertSameSize (tlhs, Expr.TypeOf (rhs));
    IF Type.IsStructured (tlhs) THEN
      IF Expr.IsDesignator (rhs)
        THEN Expr.CompileLValue (rhs, traced := FALSE);
        ELSE Expr.Compile (rhs);
      END;
    ELSE (* small set *)
      Expr.Compile (rhs);
    END;
  END DoCheckSet;

PROCEDURE DoCheckArray (tlhs: Type.T;  e_rhs: Expr.T) =
  VAR
    trhs    := Expr.TypeOf (e_rhs);
    openRHS := OpenArrayType.Is (trhs);
    openLHS := OpenArrayType.Is (tlhs);
    rhs     : CG.Val;
  BEGIN
    (* evaluate the right-hand side *)
    IF Expr.IsDesignator (e_rhs)
      THEN Expr.CompileLValue (e_rhs, traced := FALSE);
      ELSE Expr.Compile (e_rhs);
    END;

    IF openLHS THEN
      Error.Msg ("INTERNAL ERROR: AssignStmt.EmitCheck (OPEN ARRAY)");

    ELSIF openRHS THEN
      rhs := CG.Pop ();
      GenOpenArraySizeChk (rhs, tlhs, trhs);
      CG.Push (rhs);
      CG.Open_elt_ptr (ArrayType.EltAlign (trhs));
      CG.Free (rhs);

    ELSE (* both sides are fixed length arrays *)
      (* no more code to generate *)

    END;

  END DoCheckArray;

PROCEDURE GenOpenArraySizeChk (READONLY rhs: CG.Val;  tlhs, trhs: Type.T) =
  VAR ilhs, irhs, elhs, erhs: Type.T;  n := 0;
  BEGIN
    IF NOT Host.doNarrowChk THEN RETURN END;
    WHILE ArrayType.Split (tlhs, ilhs, elhs)
      AND ArrayType.Split (trhs, irhs, erhs)
      AND (irhs = NIL) DO

      CG.Push (rhs);
      CG.Open_size (n);
      CG.Load_integer (Target.Integer.cg_type, Type.Number (ilhs));
      CG.Check_eq (Target.Integer.cg_type, CG.RuntimeError.IncompatibleArrayShape);

      INC (n);
      tlhs := elhs;
      trhs := erhs;
    END;
  END GenOpenArraySizeChk;

BEGIN
END AssignStmt.
