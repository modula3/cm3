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
IMPORT QualifyExpr, ArrayExpr;
IMPORT Variable, Procedure, OpenArrayType;
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

(* EXPORTED: *) 
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

(*--------------------------------------------------------- type checking ---*)

(* Externally dispatched-to: *)
PROCEDURE CheckMethod (p: P;  VAR cs: Stmt.CheckState) =
  VAR tlhs: Type.T;  rhs_info: Type.Info;
  BEGIN
    Expr.TypeCheck (p.lhs, cs);
    Expr.TypeCheck (p.rhs, cs);

    tlhs := Expr.SemTypeOf (p.lhs);
    IF  NOT Expr.IsDesignator (p.lhs) THEN
      Error.Msg ("left-hand side is not a designator");
    ELSE
      EVAL Type.CheckInfo (Expr.SemTypeOf (p.rhs), rhs_info);
      IF NOT Expr.IsWritable (p.lhs, rhs_info.isTraced) THEN
        Error.Msg ("left-hand side is read-only");
      END;
    END;
    EVAL Type.Check(tlhs);

    Check (tlhs, p.rhs, cs);
  END CheckMethod;

(* EXPORTED: *) 
PROCEDURE Check (tlhs: Type.T;  rhsExpr: Expr.T;  VAR cs: Stmt.CheckState) =
  VAR
    base_tlhs := Type.Base (tlhs); (* strip renaming, packing, and subranges. *)
    trhs := Expr.SemTypeOf (rhsExpr);
    lhs_type_info, base_lhs_type_info: Type.Info;
    lhsClass: Type.Class;
  BEGIN
    tlhs := Type.CheckInfo (tlhs, lhs_type_info);
    base_tlhs := Type.CheckInfo (base_tlhs, base_lhs_type_info);
    lhsClass := base_lhs_type_info.class;
    Expr.TypeCheck (rhsExpr, cs);
    ArrayExpr.NoteUseTargetVar (rhsExpr);

    IF NOT Type.IsAssignable (tlhs, trhs) THEN
      IF (tlhs # ErrType.T) AND (trhs # ErrType.T) THEN
        Error.Msg ("types are not assignable in assignment statement");
      END;
    ELSE
      CASE lhsClass OF
      | Type.Class.Enum, Type.Class.Subrange, Type.Class.Integer,
        Type.Class.Longint =>
        CheckOrdinal (tlhs, rhsExpr);
      | Type.Class.Ref, Type.Class.Object, Type.Class.Opaque =>
        CheckReference (tlhs, trhs, lhs_type_info);
      | Type.Class.Procedure =>
        CheckProcedure (rhsExpr);
      ELSE
      END (*CASE*)
    END
  END Check;

PROCEDURE CheckOrdinal (tlhs: Type.T;  rhsExpr: Expr.T) =
  VAR lmin, lmax, rmin, rmax: Target.Int;  constant: Expr.T;
  BEGIN
    (* Range check if rhsExpr is constant. *)
    constant := Expr.ConstValue (rhsExpr);
    IF (constant # NIL) THEN rhsExpr := constant END;
    Expr.GetBounds (rhsExpr, rmin, rmax);
    EVAL Type.GetBounds (tlhs, lmin, lmax);
    IF TInt.LE (lmin, lmax) AND TInt.LE (rmin, rmax)
      AND (TInt.LT (lmax, rmin) OR TInt.LT (rmax, lmin)) THEN
      (* non-overlapping, non-empty ranges *)
(* CHECK^ Could/should tis only warn at CT and except at RT? *)
      Error.Warn (2, "value not assignable (disjoint ranges)");
    END;
  END CheckOrdinal;

PROCEDURE CheckReference (tlhs, trhs: Type.T;  READONLY lhs_type_info: Type.Info) =
  BEGIN
(* Check: Doesn't this just duplicate checks already done by Type.IsAssibable? *)
    IF Type.IsSubtype (trhs, tlhs) THEN
      (*ok*)
    ELSIF NOT Type.IsSubtype (tlhs, trhs) THEN
      Error.Msg ("types are not assignable");
    ELSIF Type.IsEqual (trhs, Addr.T, NIL) THEN 
      (* that is legal only in UNSAFE modules *)
      IF Module.IsSafe() THEN Error.Msg ("unsafe implicit NARROW"); END;
    ELSIF ObjectType.Is (trhs) THEN
      (*ok*)
    ELSIF lhs_type_info.isTraced THEN
      (*ok*)
    ELSE
      Error.Msg ("expression not assignable to reference type");
    END;
  END CheckReference;

PROCEDURE CheckProcedure (rhsExpr: Expr.T) =
  BEGIN
    IF NeedsClosureCheck (rhsExpr, TRUE) THEN
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

(* EXPORTED: *) 
PROCEDURE PrepForEmit
  (lhsRepType: Type.T; rhsExpr: Expr.T; initializing: BOOLEAN) =
(* Before Prepping the rhsExpr, give it some info it could need to decide where
   to build its result and whether it needs a LHS expr on the CG stack. *)
  VAR baseExpr: Expr.T;
  BEGIN
    baseExpr := ConsExpr.Base (rhsExpr);
    IF baseExpr # NIL THEN rhsExpr := baseExpr END;
    ArrayExpr.NoteTargetType (rhsExpr, lhsRepType);
    IF CanAvoidCopy (lhsRepType, rhsExpr, initializing)
    THEN Expr.MarkForDirectAssignment (rhsExpr)
    END;
    Expr.Prep (rhsExpr);
  END PrepForEmit;

PROCEDURE CanAvoidCopy
  (lhsRepType: Type.T;  rhsExpr: Expr.T;  initializing: BOOLEAN): BOOLEAN =
(* Is it possible to assign directly into a LHS variable? *)
  VAR
    lhsBaseType, rhsRepType: Type.T;
    lhsRepTypeInfo, rhsRepTypeInfo: Type.Info;
    rhsBaseExpr: Expr.T;
  BEGIN
    IF NOT Host.direct_struct_assign THEN RETURN FALSE END;
    IF NOT initializing THEN RETURN FALSE END;
    (* If this is not the first assignment to the LHS, evaluation of the RHS
       could access the LHS, whose contents must remain unchanged until all
       RHS evaluation is done.  Or, the RHS could raise an exception, and the
       the LHS must not be changed at all. *)
    IF NOT Expr.SupportsDirectAssignment (rhsExpr) THEN
      (* Currently array and record.  Maybe someday multi-word sets. *)
      RETURN FALSE
    END;
    lhsBaseType := Type.Base (lhsRepType);
    IF CallExpr.Is (rhsExpr) THEN
      IF NOT ProcType.LargeResult (lhsBaseType) THEN RETURN FALSE END;
(* TODO: Allow direct assignment through SUBARRAY and LOOPHOLE. *)
      IF NOT CallExpr.IsUserProc (rhsExpr) THEN RETURN FALSE END;
      (* For user procedures, we can always pass in the true destination.
         It is the callee's responsibility not to assign to this location
         until the final procedure outcome is known, and not to overwrite
         the contents until the entire result value has been computed. *)
    END;

    (* Make sure the source and destination are both aligned properly. *)
    EVAL Type.CheckInfo (lhsBaseType, lhsRepTypeInfo);
    IF ConsExpr.Is (rhsExpr)
    THEN rhsBaseExpr := ConsExpr.Base (rhsExpr);
    ELSE rhsBaseExpr := rhsExpr;
    END;
    rhsRepType := Type.StripPacked (Expr.RepTypeOf (rhsBaseExpr));
    EVAL Type.CheckInfo (rhsRepType, rhsRepTypeInfo);
    IF lhsRepTypeInfo.alignment # rhsRepTypeInfo.alignment THEN RETURN FALSE END;
    IF lhsRepTypeInfo.class # rhsRepTypeInfo.class THEN RETURN FALSE; END;
    IF lhsRepTypeInfo.class = Type.Class.Packed THEN RETURN FALSE; END;
    RETURN TRUE;
  END CanAvoidCopy;

(* EXPORTED: *) 
PROCEDURE DoEmit
  (lhsRepType: Type.T;  rhsExpr: Expr.T; lhsAlign := Target.Byte) =
  (* PRE: LHS is Compiled and on TOS. It is dope if an open array. *)
  (* PRE: RHS is Prepped. *)

  VAR lhsRepBaseType: Type.T;
  VAR lhsRepTypeInfo, lhsRepBaseTypeInfo: Type.Info;
  VAR baseExpr: Expr.T;
  BEGIN
    baseExpr := ConsExpr.Base (rhsExpr);
    IF baseExpr # NIL THEN rhsExpr := baseExpr END;
    lhsRepType := Type.CheckInfo (lhsRepType, lhsRepTypeInfo);
    lhsRepBaseType := Type.Base (lhsRepType);
    (* ^Strip renaming, packing, and subranges. *)
    lhsRepBaseType := Type.CheckInfo (lhsRepBaseType, lhsRepBaseTypeInfo);

    CASE lhsRepBaseTypeInfo.class OF
    | Type.Class.Integer, Type.Class.Longint, Type.Class.Subrange,
      Type.Class.Enum =>
        AssignOrdinal (lhsRepType, rhsExpr, lhsRepTypeInfo);
    | Type.Class.Real, Type.Class.Longreal, Type.Class.Extended =>
        AssignFloat (rhsExpr, lhsRepTypeInfo);
    | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
        AssignReference (lhsRepType, rhsExpr, lhsRepTypeInfo);
    | Type.Class.Procedure =>
        AssignProcedure (rhsExpr, lhsRepTypeInfo);
    | Type.Class.Record =>
        AssignRecord (lhsRepType, rhsExpr, lhsRepTypeInfo, lhsAlign);
    | Type.Class.Set =>
        AssignSet (lhsRepType, rhsExpr, lhsRepTypeInfo);
    | Type.Class.Array, Type.Class.OpenArray =>
        AssignArray (lhsRepType, lhsRepBaseTypeInfo, rhsExpr, lhsAlign);
    | Type.Class.Error =>
    ELSE <*ASSERT FALSE*>
    END;
  END DoEmit;

PROCEDURE AssignOrdinal (tlhs: Type.T;  rhsExpr: Expr.T;
                         READONLY lhsTypeInfo: Type.Info) =
  (* PRE: LHS is compiled and on TOS. *)
  (* PRE: RHS is prepped. *)
  VAR min, max : Target.Int;
  BEGIN
    EVAL Type.GetBounds (tlhs, min, max);
    CheckExpr.EmitChecks (rhsExpr, min, max, CG.RuntimeError.ValueOutOfRange);
    (* ^Which does Compile(rhsExpr), thus pushing its CG.Val. *)
    CG.Store_indirect (lhsTypeInfo.stk_type, 0, lhsTypeInfo.size);
  END AssignOrdinal;

PROCEDURE AssignFloat (rhsExpr: Expr.T;  READONLY lhsTypeInfo: Type.Info) =
  (* PRE: LHS is compiled and on TOS. *)
  (* PRE: RHS is prepped. *)
  BEGIN
    Expr.Compile (rhsExpr);
    CG.Store_indirect (lhsTypeInfo.stk_type, 0, lhsTypeInfo.size);
  END AssignFloat;

PROCEDURE AssignReference (tlhs: Type.T;  rhsExpr: Expr.T;
                           READONLY lhsTypeInfo: Type.Info) =
  (* PRE: LHS is compiled and on TOS. *)
  (* PRE: RHS is prepped. *)
  VAR lhsVal: CG.Val;
  BEGIN
    lhsVal := CG.Pop ();
    Expr.Compile (rhsExpr);
    IF Host.doNarrowChk THEN Narrow.Emit (tlhs, Expr.TypeOf (rhsExpr)) END;
    CG.Push (lhsVal);
    CG.Swap ();
    CG.Store_indirect (lhsTypeInfo.stk_type, 0, lhsTypeInfo.size);
    CG.Free (lhsVal);
  END AssignReference;

PROCEDURE AssignProcedure (rhsExpr: Expr.T;  READONLY lhsTypeInfo: Type.Info) =
  (* PRE: LHS is compiled and on TOS. *)
  (* PRE: RHS is prepped. *)
  VAR ok: CG.Label;  lhsVal, rhsVal: CG.Val;
  BEGIN
    IF NOT Host.doNarrowChk THEN
      Expr.Compile (rhsExpr);
    ELSIF NOT NeedsClosureCheck (rhsExpr, FALSE) THEN
      Expr.Compile (rhsExpr);
    ELSE
      lhsVal := CG.Pop ();
      Expr.Compile (rhsExpr);
      rhsVal := CG.Pop ();
      ok := CG.Next_label ();
      CG.If_closure (rhsVal, CG.No_label, ok, CG.Always);
      CG.Abort (CG.RuntimeError.NarrowFailed);
      CG.Set_label (ok);
      CG.Push (lhsVal);
      CG.Push (rhsVal);
      CG.Free (rhsVal);
      CG.Free (lhsVal);
    END;
    CG.Store_indirect (lhsTypeInfo.stk_type, 0, lhsTypeInfo.size);
  END AssignProcedure;

PROCEDURE AssignSet (tlhs: Type.T;  rhsExpr: Expr.T;
                     READONLY lhsTypeInfo: Type.Info) =
  (* PRE: LHS is compiled and on TOS. *)
  (* PRE: RHS is prepped. *)
  BEGIN
(* TODO: Merge AssignSet and AssignRecord. *)
    AssertSameSize (tlhs, Expr.TypeOf (rhsExpr));
    (* RHS is Prepped. *)
    IF Type.IsStructured (tlhs) THEN
      IF Expr.IsDesignator (rhsExpr)
        THEN Expr.CompileLValue (rhsExpr, traced := FALSE);
        ELSE Expr.Compile (rhsExpr);
      END;
      IF Expr.IsMarkedForDirectAssignment (rhsExpr) THEN
        CG.Discard (CG.Type.Addr);
      ELSE
        CG.Copy (lhsTypeInfo.size, overlap := FALSE);
      END
    ELSE (* small set *)
      Expr.Compile (rhsExpr);
      CG.Store_indirect (lhsTypeInfo.stk_type, 0, lhsTypeInfo.size);
    END;
  END AssignSet;

PROCEDURE CompileStruct (expr: Expr.T) =
(* This works for a record or array that is a packed component of something. *)
  BEGIN
    IF Expr.IsDesignator (expr)
    THEN Expr.CompileLValue (expr, traced := FALSE);
(* CHECK ----------------------------------- ^ *)
    ELSE Expr.Compile (expr);
    END;
  END CompileStruct;

PROCEDURE CopyStruct (lhsAlign: Type.BitAlignT; bitSize: INTEGER) =
(* PRE: CGstack: RHS addr on top, LHS addr below. *)
(* PRE: Using exprssion protocol. *)
  BEGIN
    IF lhsAlign < Target.Byte OR bitSize MOD Target.Byte # 0
    THEN 
      CG.Load_indirect (Target.Word.cg_type, 0 , bitSize); 
      CG.Store_indirect (Target.Word.cg_type, 0 , bitSize); 
    ELSE
      CG.Copy (bitSize, overlap := FALSE);
      (* Expression protocol means ^ thevalue is already in a temporary. *)
    END
  END CopyStruct;

PROCEDURE AssignRecord
  (tlhs: Type.T; rhsExpr: Expr.T; READONLY lhsTypeInfo: Type.Info;
   lhsAlign: Type.BitAlignT) =
  (* PRE: LHS is compiled and on TOS. *)
  (* PRE: RHS is prepped. *)
  BEGIN
    AssertSameSize (tlhs, Expr.TypeOf (rhsExpr));
    (* Leave the LHS address on the CG stack, regardless of protocol. *)
    CompileStruct (rhsExpr);
    IF Expr.UsesAssignProtocol (rhsExpr)
    THEN (* Compile will have copied RHS into LHS,, leaving only the LHS
            address on CG stack, which is the expression's result. *) 
      CG.Discard (CG.Type.Addr);
    ELSE (* Using expression protocol. *)
      (* RHS is on top of CG stack.  LHS is below.
         Compile will have left LHS alone. *)
      CopyStruct (lhsAlign, lhsTypeInfo.size );
    END;
  END AssignRecord;

PROCEDURE AssignArray
  (lhsRepType: Type.T; READONLY lhsRepTypeInfo: Type.Info; rhsExpr: Expr.T; 
   lhsAlign: Type.BitAlignT) =
  (* PRE: LHS is compiled and on TOS. *)
  (* PRE: RHS is prepped. *)
  VAR
    lhsVal, rhsVal : CG.Val;
    rhsRepType: Type.T;
    rhsRepTypeInfo : Type.Info;
    LHSIsOpen, RHSIsOpen: BOOLEAN;
  BEGIN
    IF Expr.UsesAssignProtocol (rhsExpr)
    THEN
      CompileStruct (rhsExpr);
      (* CompileStruct will have done any needed shape check and copied rhsExpr's
         value into the LHS, leaving only the LHS address on CG stack, which
         is the expression's result. *)
      CG.Discard (CG.Type.Addr);
    ELSE (* Using expression protocol. *)
      LHSIsOpen := OpenArrayType.Is (lhsRepType);
      rhsRepType := Expr.RepTypeOf (rhsExpr);
      rhsRepType := Type.StripPacked (rhsRepType);
      RHSIsOpen := OpenArrayType.Is (rhsRepType);
      IF NOT RHSIsOpen AND NOT LHSIsOpen
      THEN(* Both sides are fixed length arrays *)
        CompileStruct (rhsExpr);
        (* RHS is on top of CG stack.  LHS is below, unmolested by Compile. *)
        CopyStruct (lhsAlign, lhsRepTypeInfo.size);
      ELSE (* Something is open. *)
        lhsVal := CG.Pop ();
        CompileStruct (rhsExpr);
        (* RHS on top of CG stack. *)
        rhsVal := CG.Pop ();

        IF ArrayExpr.ShapeCheckNeeded (rhsExpr) THEN
          GenOpenArrayShapeChecks (lhsRepType, lhsVal, rhsRepType, rhsVal);
          (* Assignability check, done in Check will have taken care of
             static shape checking. *)
        END;

        IF LHSIsOpen THEN
          IF RHSIsOpen THEN (* Both sides are open. *) 
            CG.Push (lhsVal);
            CG.Open_elt_ptr (lhsAlign);
            CG.ForceStacked ();
            CG.Push (rhsVal);
            CG.Open_elt_ptr (Expr.Alignment(rhsExpr));
            CG.ForceStacked ();
            GenOpenArrayCopy (lhsVal, lhsRepType, rhsRepType);
          ELSE (* LHS is open and RHS is fixed. *) 
            CG.Push (lhsVal);
            CG.Open_elt_ptr (lhsAlign);
            CG.Push (rhsVal);
            EVAL Type.CheckInfo (rhsRepType, rhsRepTypeInfo);
            CopyStruct (lhsAlign, rhsRepTypeInfo.size);
          END;
        ELSE (* LHS is fixed and RHS is open. *)
          CG.Push (lhsVal);
          CG.Push (rhsVal);
          CG.Open_elt_ptr (Expr.Alignment(rhsExpr));
          CopyStruct (lhsAlign, lhsRepTypeInfo.size);
        END;
        CG.Free (lhsVal);
        CG.Free (rhsVal);
      END
    END
  END AssignArray;

PROCEDURE GenOpenArrayShapeChecks       
  (lhsRepType: Type.T; lhsDopeVal: CG.Val; rhsRepType: Type.T; rhsVal: CG.Val) =
(* Leaves the CG stack alone. *)
  VAR lhsIndexType, rhsIndexType, lhsEltType, rhsEltType: Type.T;
  VAR depth:= 0;
  BEGIN
    IF NOT Host.doNarrowChk THEN RETURN; END;
    lhsRepType := Type.StripPacked (lhsRepType);
    rhsRepType := Type.StripPacked (rhsRepType);
    WHILE ArrayType.Split (lhsRepType, lhsIndexType, lhsEltType)
          AND ArrayType.Split (rhsRepType, rhsIndexType, rhsEltType) DO
      IF lhsIndexType = NIL THEN
        IF rhsIndexType = NIL THEN (* Both LHS and RHS are open. *)
          CG.Push (lhsDopeVal);
          CG.Open_size (depth);
          CG.Push (rhsVal);
          CG.Open_size (depth);
          CG.Check_eq
            (Target.Integer.cg_type, CG.RuntimeError.IncompatibleArrayShape);
        ELSE (* LHS is open and RHS is fixed. *)
          CG.Push (lhsDopeVal);
          CG.Open_size (depth);
          CG.Load_integer (Target.Integer.cg_type, Type.Number (rhsIndexType));
          CG.Check_eq
            (Target.Integer.cg_type, CG.RuntimeError.IncompatibleArrayShape);
        END
      ELSE
        IF rhsIndexType = NIL THEN (* LHS is fixed and RHS is open. *)
          CG.Push (rhsVal);
          CG.Open_size (depth);
          CG.Load_integer (Target.Integer.cg_type, Type.Number (lhsIndexType));
          CG.Check_eq
            (Target.Integer.cg_type, CG.RuntimeError.IncompatibleArrayShape);
        ELSE (* Both LHS and RHS are fixed.  Static assignability check will
                have ensured equality here and in any inner dimensions. *)
          RETURN;
        END
      END;      

      INC (depth);
      lhsRepType := lhsEltType;
      rhsRepType := rhsEltType;
    END;
  END GenOpenArrayShapeChecks;

PROCEDURE GenOpenArrayCopy (lhsDopeVal: CG.Val;  lhsRepType, rhsRepType: Type.T) =
(* PRE: formal lhsDopeVal is the LHS dope.
   PRE: both lhsRepType and rhsRepType are open array types.
   PRE: RHS element ptr on TOS and has been ForceStacked.
   PRE: LHS element ptr below and has been ForceStacked.
   PRE: Using the assign protocol.
*)
  VAR
    lhsDepth, rhsDepth: INTEGER;
  BEGIN
    lhsDepth := OpenArrayType.OpenDepth (lhsRepType);
    <*ASSERT lhsDepth > 0 *>
    rhsDepth := OpenArrayType.OpenDepth (rhsRepType);
    <*ASSERT rhsDepth > 0 *>
    FOR i := 0 TO MIN (lhsDepth, rhsDepth) - 1 DO
      CG.Push (lhsDopeVal);
      CG.Open_size (i);
      IF (i # 0) THEN CG.Multiply (Target.Word.cg_type) END;
    END;
    IF lhsDepth < rhsDepth
    THEN CG.Copy_n (OpenArrayType.EltPack (lhsRepType), overlap := FALSE);
    ELSE CG.Copy_n (OpenArrayType.EltPack (rhsRepType), overlap := FALSE);
    END;
  END GenOpenArrayCopy;

PROCEDURE AssertSameSize (a, b: Type.T) = 
  VAR a_info, b_info: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (a, a_info);
    EVAL Type.CheckInfo (b, b_info);
    IF (a_info.size # b_info.size) THEN
      Error.Msg ("INTERNAL ERROR: trying to assign values of differing sizes");
    END;
  END AssertSameSize;

(*---------------------------------------- code generation: checking only ---*)

(* EXPORTED: *) 
PROCEDURE DoEmitCheck (tlhs: Type.T;  rhsExpr: Expr.T) =
  (* PRE: The lhs is compiled (thus on TOS).
     PRE: The rhsExpr is prepped.
     POST: RHS is TOS. *)
  VAR
    t_lhs_base := Type.Base (tlhs); (* strip renaming and packing *)
    t_lhs_base_info: Type.Info;
  BEGIN
    t_lhs_base := Type.CheckInfo (t_lhs_base, t_lhs_base_info);

    CASE t_lhs_base_info.class OF
    | Type.Class.Integer, Type.Class.Longint, Type.Class.Subrange,
      Type.Class.Enum =>
        DoCheckOrdinal (tlhs, rhsExpr);
    | Type.Class.Real, Type.Class.Longreal, Type.Class.Extended =>
        DoCheckFloat (rhsExpr);
    | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
        DoCheckReference (tlhs, rhsExpr);
    | Type.Class.Array, Type.Class.OpenArray =>
        DoCheckArray (tlhs, rhsExpr);
    | Type.Class.Procedure =>
        DoCheckProcedure (rhsExpr);
    | Type.Class.Record =>
        DoCheckRecord (tlhs, rhsExpr);
    | Type.Class.Set =>
        DoCheckSet (tlhs, rhsExpr);
    ELSE <* ASSERT FALSE *>
    END;
  END DoEmitCheck;

PROCEDURE DoCheckOrdinal (tlhs: Type.T;  rhsExpr: Expr.T) =
  VAR min, max : Target.Int;
  BEGIN
    EVAL Type.GetBounds (tlhs, min, max);
    CheckExpr.EmitChecks (rhsExpr, min, max, CG.RuntimeError.ValueOutOfRange);
  END DoCheckOrdinal;

PROCEDURE DoCheckFloat (rhsExpr: Expr.T) =
  BEGIN
    Expr.Compile (rhsExpr);
  END DoCheckFloat;

PROCEDURE DoCheckReference (tlhs: Type.T;  rhsExpr: Expr.T) =
  BEGIN
    Expr.Compile (rhsExpr);
    IF Host.doNarrowChk THEN Narrow.Emit (tlhs, Expr.TypeOf (rhsExpr)) END;
  END DoCheckReference;

PROCEDURE DoCheckProcedure (rhsExpr: Expr.T) =
  VAR ok: CG.Label;  rhsVal: CG.Val;
  BEGIN
    IF NOT Host.doNarrowChk THEN
      Expr.Compile (rhsExpr);
    ELSIF NOT NeedsClosureCheck (rhsExpr, FALSE) THEN
      Expr.Compile (rhsExpr);
    ELSE
      Expr.Compile (rhsExpr);
      rhsVal := CG.Pop ();
      ok := CG.Next_label ();
      CG.If_closure (rhsVal, CG.No_label, ok, CG.Always);
      CG.Abort (CG.RuntimeError.NarrowFailed);
      CG.Set_label (ok);
      CG.Push (rhsVal);
      CG.Free (rhsVal);
    END;
  END DoCheckProcedure;

PROCEDURE DoCheckRecord (tlhs: Type.T;  rhsExpr: Expr.T) =
  BEGIN
    AssertSameSize (tlhs, Expr.TypeOf (rhsExpr));
    IF Expr.IsDesignator (rhsExpr)
      THEN Expr.CompileLValue (rhsExpr, traced := FALSE);
      ELSE Expr.Compile (rhsExpr);
    END;
  END DoCheckRecord;

PROCEDURE DoCheckSet (tlhs: Type.T;  rhsExpr: Expr.T) =
  BEGIN
    AssertSameSize (tlhs, Expr.TypeOf (rhsExpr));
    IF Type.IsStructured (tlhs) THEN
      IF Expr.IsDesignator (rhsExpr)
        THEN Expr.CompileLValue (rhsExpr, traced := FALSE);
        ELSE Expr.Compile (rhsExpr);
      END;
    ELSE (* small set *)
      Expr.Compile (rhsExpr);
    END;
  END DoCheckSet;

PROCEDURE DoCheckArray (tlhs: Type.T;  rhsExpr: Expr.T) =
  VAR
    trhs    := Expr.TypeOf (rhsExpr);
    LHSIsOpen := OpenArrayType.Is (tlhs);
    RHSIsOpen := OpenArrayType.Is (trhs);
    rhs     : CG.Val;
  BEGIN
    (* evaluate the right-hand side *)
    IF Expr.IsDesignator (rhsExpr)
      THEN Expr.CompileLValue (rhsExpr, traced := FALSE);
      ELSE Expr.Compile (rhsExpr);
    END;

    IF LHSIsOpen THEN
      Error.Msg ("INTERNAL ERROR: AssignStmt.EmitCheck (OPEN ARRAY)");

    ELSIF RHSIsOpen THEN
      rhs := CG.Pop ();
      GenOpenArraySizeChk (rhs, tlhs, trhs);
      CG.Push (rhs);
      CG.Open_elt_ptr (ArrayType.EltAlign (trhs));
      CG.Free (rhs);

    ELSE (* both sides are fixed length arrays *)
      (* no more code to generate *)

    END;
  END DoCheckArray;

PROCEDURE GenOpenArraySizeChk (rhsVal: CG.Val;  tlhs, trhs: Type.T) =
  VAR lhsIndexType, rhsIndexType, lhsEltType, rhsEltType: Type.T;
  VAR depth := 0;
  BEGIN
    IF NOT Host.doNarrowChk THEN RETURN END;
    WHILE ArrayType.Split (tlhs, lhsIndexType, lhsEltType)
      AND ArrayType.Split (trhs, rhsIndexType, rhsEltType)
      AND (rhsIndexType = NIL) DO

      CG.Push (rhsVal);
      CG.Open_size (depth);
      CG.Load_integer (Target.Integer.cg_type, Type.Number (lhsIndexType));
      CG.Check_eq
        (Target.Integer.cg_type, CG.RuntimeError.IncompatibleArrayShape);

      INC (depth);
      tlhs := lhsEltType;
      trhs := rhsEltType;
    END;
  END GenOpenArraySizeChk;

(* --------------------------- Compile ------------------------------ *)

(* Externally dispatched-to: *)
PROCEDURE Compile (p: P): Stmt.Outcomes =
  VAR
    tlhs := Expr.RepTypeOf (p.lhs);
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

(* Externally dispatched-to: *)
PROCEDURE GetOutcome (<*UNUSED*> p: P): Stmt.Outcomes =
  BEGIN
    RETURN Stmt.Outcomes {Stmt.Outcome.FallThrough};
  END GetOutcome;

BEGIN
END AssignStmt.
