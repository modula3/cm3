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
IMPORT QualifyExpr, SetExpr, RecordExpr, ArrayExpr;
IMPORT Variable, Procedure, OpenArrayType;
IMPORT ProcExpr, ProcType, ObjectType, CallExpr, Host, Narrow;

TYPE P = Stmt.T OBJECT
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
      Error.Msg ("Assignment left-hand side is not a designator (2.3.1).");
    ELSE
      EVAL Type.CheckInfo (Expr.SemTypeOf (p.rhs), rhs_info);
      IF NOT Expr.IsWritable (p.lhs, rhs_info.isTraced) THEN
        Error.Msg ("Assignment left-hand side is read-only (2.3.1).");
      END;
    END;
    EVAL Type.Check(tlhs);

    Check (tlhs, p.rhs, cs, IsError := FALSE);
  END CheckMethod;

(* EXPORTED: *) 
PROCEDURE Check
  (tlhs: Type.T;  rhsExpr: Expr.T;  VAR cs: Stmt.CheckState; IsError := FALSE )=
  VAR Code: CG.RuntimeError;
  VAR Msg: TEXT;
  BEGIN
    CheckStaticRTErrExec ( tlhs, rhsExpr, cs, (*OUT*)Code, (*OUT*)Msg, IsError);
  END Check;

(* EXPORTED: *) 
PROCEDURE CheckStaticRTErrExec
  (tlhs: Type.T;  rhsExpr: Expr.T;  VAR cs: Stmt.CheckState;
   VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT; IsError := FALSE
  ) =
(* Like Check, but if a warning is produced for a runtime error that is
   statically inevitable whenever its code is executed, return the RT error
   Code # CG.RuntimeError.Unknown and a message text. in Msg.*)

  VAR
    base_tlhs := Type.Base (tlhs); (* strip renaming, packing, and subranges. *)
    trhs: Type.T;
    lhs_type_info, base_lhs_type_info: Type.Info;
    lhsTypeClass: Type.Class;
  BEGIN
    Msg := NIL;
    Code := CG.RuntimeError.Unknown;
    tlhs := Type.CheckInfo (tlhs, lhs_type_info);
    base_tlhs := Type.CheckInfo (base_tlhs, base_lhs_type_info);
    lhsTypeClass := base_lhs_type_info.class;
    Expr.TypeCheck (rhsExpr, cs);
    trhs := Expr.SemTypeOf (rhsExpr);

    IF NOT Type.IsAssignable (tlhs, trhs) THEN
      IF (tlhs # ErrType.T) AND (trhs # ErrType.T) THEN
        Error.Msg ("Types are not assignable in assignment (2.3.1).");
      END;
    ELSE
      CASE lhsTypeClass OF
      | Type.Class.Enum, Type.Class.Subrange, Type.Class.Integer,
        Type.Class.Longint =>
        CheckOrdinal (tlhs, rhsExpr, (*OUT*)Code, (*OUT*)Msg, IsError);
      | Type.Class.Ref, Type.Class.Object, Type.Class.Opaque =>
        CheckReference (tlhs, trhs, lhs_type_info);
      | Type.Class.Procedure =>
        CheckProcedure (rhsExpr, (*OUT*)Code, (*OUT*)Msg, IsError);
      | Type.Class.Set =>
        SetExpr.CheckStaticRTErrEval (rhsExpr, (*OUT*)Code, (*OUT*)Msg);
      | Type.Class.Record =>
        RecordExpr.CheckStaticRTErrEval (rhsExpr, (*OUT*)Code, (*OUT*)Msg);
      | Type.Class.Array, Type.Class.OpenArray =>
        ArrayExpr.CheckStaticRTErrEval (rhsExpr, (*OUT*)Code, (*OUT*)Msg);
        ArrayExpr.CheckStaticRTErrAssign
          (tlhs, rhsExpr, (*OUT*)Code, (*OUT*)Msg);
      ELSE
      END (*CASE*)
    END
  END CheckStaticRTErrExec;

PROCEDURE CheckOrdinal
  (tlhs: Type.T;  rhsExpr: Expr.T;
   VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT; IsError: BOOLEAN
  ) =
   
  VAR lmin, lmax, rmin, rmax: Target.Int;
  VAR constant: Expr.T;
  VAR reason: TEXT;
  BEGIN
    (* Range check if rhsExpr is constant. *)
    constant := Expr.ConstValue (rhsExpr);
    IF constant # NIL THEN rhsExpr := constant END;
    Expr.GetBounds (rhsExpr, rmin, rmax);
    EVAL Type.GetBounds (tlhs, lmin, lmax);
    IF TInt.LE (lmin, lmax) AND TInt.LE (rmin, rmax)
       AND (TInt.LT (lmax, rmin) OR TInt.LT (rmax, lmin)) THEN
      IF constant = NIL THEN
        (* Non-constant RHS, disjoint ranges.  Type assignability check in
           Check will have given a CT error and precluded this. *)
        reason := "(disjoint ranges)";
        <* ASSERT FALSE *>
      ELSE reason := "(out of range)"
      END; 
(* CHECK^ Can this duplicate a warning from IsAssignable on types, if
          RHS has a constant value outside LHS bounds? *)
      IF IsError THEN
        Error.Msg ("Constant value not assignable " & reason & " (2.3.1).");
      ELSE
        Error.Warn
          (2, "Ordinal value not assignable at runtime " & reason & " (2.3.1).");
        Code := CG.RuntimeError.ValueOutOfRange;
        Msg := "value out of range."
      END;
    ELSE 
    END;
  END CheckOrdinal;

PROCEDURE CheckReference
  (tlhs, trhs: Type.T;  READONLY lhs_type_info: Type.Info) =
  BEGIN
(* CHECK: Doesn't this just duplicate checks already done by Type.IsAssignable? *)
    (* Other than NIL, which is a member of every reference type, there are
       no constant reference values to do CT assignability warnings for. *) 
    IF Type.IsSubtype (trhs, tlhs) THEN
      (*ok*)
    ELSIF NOT Type.IsSubtype (tlhs, trhs) THEN
      Error.Msg ("Reference types are not assignable (2.3.1).");
    ELSIF Type.IsEqual (trhs, Addr.T, NIL) THEN 
      (* this is legal only in UNSAFE modules *)
      IF Module.IsSafe() THEN
        Error.Msg ("Unsafe implicit NARROW to ADDRESS (2.3.1).");
      END;
    ELSIF ObjectType.Is (trhs) THEN
      (*ok*)
    ELSIF lhs_type_info.isTraced THEN
      (*ok*)
    ELSE
      Error.Msg ("Expression not assignable to reference type (2.3.1).");
    END;
    (* A non-constant reference value will never be an element/field of a
       constant constructor, so not in static constant area.  The only
       constant value of any reference is NIL, and it will never fail a
       CT narrow check. *)
  END CheckReference;

PROCEDURE CheckProcedure
  (proc: Expr.T; VAR(*OUT*) Code: CG.RuntimeError; VAR(*OUT*) Msg: TEXT; IsError: BOOLEAN) =
  VAR name: M3ID.T;
  VAR obj: Value.T;
  VAR valueClass: Value.Class;
  VAR nested: BOOLEAN;
  BEGIN
    IF NOT (NamedExpr.Split (proc, name, obj)
            OR QualifyExpr.Split (proc, obj)
            OR ProcExpr.Split (proc, obj)) THEN
      (* NIL, or anything else? *)
      RETURN 
    END;
    obj := Value.Base (obj);
    valueClass := Value.ClassOf (obj);
    IF valueClass = Value.Class.Procedure THEN (* Procedure constant. *)
      nested := Procedure.IsNested (obj);
      IF nested THEN
        (* Although we statically know this is an error, the rule (2.3.1) that
           it violates is one that cannot in general be checked statically.  I
           believe Modula3 is saying such cases should produce errors only at
           runtime, if/when the subject code is actually executed.
           rodney.m.bates@acm.org. *)
        IF IsError THEN (* But the mechanism exists to make it a CT error. *)
          Error.ID
            ( Value.CName (obj),
             "Nested procedure not assignable at runtime (2.3.1).");
        ELSE 
          Error.WarnID
            (2, Value.CName (obj),
             "Nested procedure not assignable at runtime (2.3.1).");
          Code := CG.RuntimeError.NarrowFailed;
          Msg := "Nested procedure assigned.";
        END;
      END;
    END;
  END CheckProcedure;

TYPE RTCheckKind = {None, Conditional, Fail}; 

PROCEDURE ProcRTCheckKind (proc: Expr.T): RTCheckKind  =
  VAR name: M3ID.T;
  VAR obj: Value.T;
  VAR valueClass: Value.Class;
  VAR nested: BOOLEAN;
  BEGIN
    IF NOT Host.doNarrowChk THEN RETURN RTCheckKind.None END;
    IF NOT (NamedExpr.Split (proc, name, obj)
            OR QualifyExpr.Split (proc, obj)
            OR ProcExpr.Split (proc, obj)) THEN
      (* NIL, or anything else? *)
      RETURN RTCheckKind.None
    END;
    obj := Value.Base (obj);
    valueClass := Value.ClassOf (obj);
    IF valueClass = Value.Class.Procedure THEN (* Procedure constant. *)
      nested := Procedure.IsNested (obj);
      IF nested THEN RETURN RTCheckKind.Fail;
      ELSE RETURN RTCheckKind.None
      END;
    ELSIF valueClass = Value.Class.Var AND Variable.HasClosure (obj) THEN
      (* Don't know statically.  RT check will be needed. *)
      RETURN RTCheckKind.Conditional;
    ELSE (* non-formal, non-const => no check *)
      RETURN RTCheckKind.None;
    END;
  END ProcRTCheckKind;

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
(* REVIEW: Is this overconservative when assigning an array constructor? *)
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
  (lhsRepType: Type.T;  rhsExpr: Expr.T; lhsAlign := Target.Byte; initializing: BOOLEAN) =
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

    EVAL Expr.CheckUseFailure (rhsExpr);
    CASE lhsRepBaseTypeInfo.class OF
    | Type.Class.Integer, Type.Class.Longint, Type.Class.Subrange,
      Type.Class.Enum =>
        AssignOrdinal (lhsRepType, rhsExpr, lhsRepTypeInfo);
    | Type.Class.Real, Type.Class.Longreal, Type.Class.Extended =>
        AssignFloat (rhsExpr, lhsRepTypeInfo);
    | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
        AssignReference (lhsRepType, rhsExpr, lhsRepTypeInfo);
    | Type.Class.Procedure =>
        AssignOrRTCheckProcedure (rhsExpr, lhsIsPushed := TRUE);
        CG.Store_indirect (lhsRepTypeInfo.stk_type, 0, lhsRepTypeInfo.size);
    | Type.Class.Record =>
        AssignRecord
          (rhsExpr, lhsRepTypeInfo, lhsAlign, initializing);
    | Type.Class.Set =>
        AssignSet (lhsRepType, rhsExpr, lhsRepTypeInfo, initializing);
    | Type.Class.Array, Type.Class.OpenArray =>
        AssignArray
          (lhsRepType, lhsRepBaseTypeInfo, rhsExpr, lhsAlign, initializing);
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

PROCEDURE AssignSet
  (tlhs: Type.T;  rhsExpr: Expr.T; READONLY lhsTypeInfo: Type.Info;
   initializing: BOOLEAN) =
  (* PRE: LHS is compiled and on TOS. *)
  (* PRE: RHS is prepped. *)
  BEGIN
(* TODO: Merge AssignSet and AssignRecord.  Or maybe not. *)
    (* Leave the LHS address on the CG stack, regardless of protocol. *)
    IF Type.IsStructured (tlhs) THEN
      AssertSameSize (tlhs, Expr.TypeOf (rhsExpr));
      CompileStruct (rhsExpr);
      IF Expr.IsMarkedForDirectAssignment (rhsExpr) THEN
        CG.Discard (CG.Type.Addr);
      ELSE
        CG.Copy (lhsTypeInfo.size, overlap := NOT initializing);
      END
    ELSE (* Small set. *)
(* REVIEW: Do we really need this case, or will CompileStruct handle it? *)
      Expr.Compile (rhsExpr);
      CG.Store_indirect (lhsTypeInfo.stk_type, 0, lhsTypeInfo.size);
    END;
  END AssignSet;

PROCEDURE CompileStruct (expr: Expr.T) =
(* This works for a record, set, or array that is a packed component
   of something. *)
  BEGIN
    IF Expr.IsDesignator (expr)
    THEN Expr.CompileLValue (expr, traced := FALSE);
(* CHECK ----------------------------------- ^ *)
    ELSE Expr.Compile (expr);
    END;
  END CompileStruct;

PROCEDURE CopyStruct
  (lhsAlign, rhsAlign: Type.BitAlignT; bitSize: INTEGER; overlap: BOOLEAN) =
(* PRE: CGstack: RHS addr on top, LHS addr below. *)
(* PRE: Using expression protocol. *)
  BEGIN
    IF lhsAlign < Target.Byte
       OR rhsAlign < Target.Byte
       OR bitSize MOD Target.Byte # 0
    THEN 
      CG.Load_indirect (Target.Word.cg_type, 0 , bitSize); 
      CG.Store_indirect (Target.Word.cg_type, 0 , bitSize); 
    ELSE
      CG.Copy (bitSize, overlap);
      (* Expression protocol means ^ the value is already in a temporary. *)
    END
  END CopyStruct;

PROCEDURE AssignRecord
  (rhsExpr: Expr.T; READONLY lhsTypeInfo: Type.Info; lhsAlign: Type.BitAlignT;
   initializing: BOOLEAN) =
  (* PRE: LHS is compiled and on TOS. *)
  (* PRE: RHS is prepped. *)
  VAR rhsAlign: Type.BitAlignT;
  BEGIN
    (* Leave the LHS address on the CG stack, regardless of protocol. *)
    CompileStruct (rhsExpr);
    rhsAlign := Expr.Alignment (rhsExpr);
    IF Expr.UsesAssignProtocol (rhsExpr)
    THEN (* Compile will have copied RHS into LHS, leaving only the LHS
            address on CG stack, which is the expression's result. *) 
      CG.Discard (CG.Type.Addr);
    ELSE (* Using expression protocol. *)
      (* RHS is on top of CG stack.  LHS is below.
         Compile will have left LHS alone. *)
      CopyStruct
        (lhsAlign, rhsAlign, lhsTypeInfo.size, overlap := NOT initializing);
    END;
  END AssignRecord;

PROCEDURE AssignArray
  (lhsRepType: Type.T; READONLY lhsRepTypeInfo: Type.Info; rhsExpr: Expr.T; 
   lhsAlign: Type.BitAlignT; initializing: BOOLEAN) =
  (* PRE: LHS is compiled and on TOS. *)
  (* PRE: RHS is prepped. *)
  VAR
    lhsVal, rhsVal : CG.Val;
    rhsAlign: Type.BitAlignT;
    rhsRepType: Type.T;
    rhsRepTypeInfo : Type.Info;
    LHSIsOpen, RHSIsOpen: BOOLEAN;
  BEGIN
    IF Expr.UsesAssignProtocol (rhsExpr)
    THEN
      CompileStruct (rhsExpr);
      (* CompileStruct will have done any needed shape check and copied
         rhsExpr's value into the LHS, leaving only the LHS address on CG
         stack, which is the expression's result. *)
      CG.Discard (CG.Type.Addr);
    ELSE (* Using expression protocol. *)
      LHSIsOpen := OpenArrayType.Is (lhsRepType);
      rhsRepType := Expr.RepTypeOf (rhsExpr);
      rhsRepType := Type.StripPacked (rhsRepType);
      RHSIsOpen := OpenArrayType.Is (rhsRepType);
      IF NOT RHSIsOpen AND NOT LHSIsOpen
      THEN(* Both sides are fixed length arrays *)
        CompileStruct (rhsExpr);
        rhsAlign := Expr.Alignment (rhsExpr);
        (* RHS is on top of CG stack.  LHS is below, unmolested by Compile. *)
        CopyStruct
          (lhsAlign, rhsAlign, lhsRepTypeInfo.size, overlap := NOT initializing);
      ELSE (* Something is open. *)
        lhsVal := CG.Pop ();
        CompileStruct (rhsExpr);
        rhsAlign := Expr.Alignment (rhsExpr);
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
            CopyStruct
              (lhsAlign, rhsAlign, rhsRepTypeInfo.size, overlap := NOT initializing);
          END;
        ELSE (* LHS is fixed and RHS is open. *)
          CG.Push (lhsVal);
          CG.Push (rhsVal);
          CG.Open_elt_ptr (Expr.Alignment(rhsExpr));
          CopyStruct
            (lhsAlign, rhsAlign, lhsRepTypeInfo.size, overlap := NOT initializing);
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
    THEN CG.Copy_n (OpenArrayType.EltPack (lhsRepType), overlap := TRUE);
    ELSE CG.Copy_n (OpenArrayType.EltPack (rhsRepType), overlap := TRUE);
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
PROCEDURE EmitRTCheck (tlhs: Type.T;  rhsExpr: Expr.T) =
  (* PRE: The CG stack is empty.
     PRE: The rhsExpr is prepped.
     POST: RHS is TOS. *)
  VAR
    t_lhs_base := Type.Base (tlhs); (* strip renaming and packing *)
    t_lhs_base_info: Type.Info;
  BEGIN
    EVAL Expr.CheckUseFailure (rhsExpr);

    t_lhs_base := Type.CheckInfo (t_lhs_base, t_lhs_base_info);
    CASE t_lhs_base_info.class OF
    | Type.Class.Integer, Type.Class.Longint, Type.Class.Subrange,
      Type.Class.Enum =>
        RTCheckOrdinal (tlhs, rhsExpr);
    | Type.Class.Real, Type.Class.Longreal, Type.Class.Extended =>
        RTCheckFloat (rhsExpr);
    | Type.Class.Object, Type.Class.Opaque, Type.Class.Ref =>
        RTCheckReference (tlhs, rhsExpr);
    | Type.Class.Array, Type.Class.OpenArray =>
        RTCheckArray (tlhs, rhsExpr);
    | Type.Class.Procedure =>
        AssignOrRTCheckProcedure (rhsExpr, lhsIsPushed := FALSE);
    | Type.Class.Record =>
        RTCheckRecord (rhsExpr);
    | Type.Class.Set =>
        RTCheckSet (tlhs, rhsExpr);
    ELSE <* ASSERT FALSE *>
    END;
  END EmitRTCheck;

PROCEDURE RTCheckOrdinal (tlhs: Type.T;  rhsExpr: Expr.T) =
  VAR min, max : Target.Int;
  BEGIN
    EVAL Type.GetBounds (tlhs, min, max);
    CheckExpr.EmitChecks (rhsExpr, min, max, CG.RuntimeError.ValueOutOfRange);
  END RTCheckOrdinal;

PROCEDURE RTCheckFloat (rhsExpr: Expr.T) =
  BEGIN
    Expr.Compile (rhsExpr);
  END RTCheckFloat;

PROCEDURE RTCheckReference (tlhs: Type.T;  rhsExpr: Expr.T) =
  BEGIN
    Expr.Compile (rhsExpr);
    IF Host.doNarrowChk THEN Narrow.Emit (tlhs, Expr.TypeOf (rhsExpr)) END;
  END RTCheckReference;

PROCEDURE AssignOrRTCheckProcedure (rhsExpr: Expr.T; lhsIsPushed: BOOLEAN) =
  (* PRE: The CG stack is empty.
     PRE: The rhsExpr is prepped.
     POST: RHS is TOS. *)
  VAR lhsVal, rhsVal: CG.Val;
  VAR ok: CG.Label;
  BEGIN
    CASE <*NOWARN*> ProcRTCheckKind (rhsExpr) OF
    | RTCheckKind.Fail =>
      CG.Abort (CG.RuntimeError.NarrowFailed);
      Expr.Compile (rhsExpr);
    | RTCheckKind.Conditional =>
      (* <rant>
         Could there ever be a better example than this of what
         an utterly dreadful idea an operand stack machine is?
         
         We have to call If_Closure with empty stack.  We have to know how
         many items are on it when we are called.  Not only does this involve
         backing up through the reverse call graph in many places, checking
         undocumented preconditions, but in the end, it is conditional.
         If things are there, we have to spill them.

         Then, after pushing the RHS, we have to spill it too, pass the
         spilled rhs value to If_closure in a parameter, then re-push it
         afterwards for further use by our callers.  Finally, conditionally
         re-push the previously spilled items.

         Or, we could have writen two nearly-same copies of this procedure.

         All of this is entirely gratuitous, as it has no actual effect
         on the If_closure operation.
         </rant> *)
      IF lhsIsPushed THEN
        lhsVal := CG.Pop ();
      END;
      Expr.Compile (rhsExpr);
      rhsVal := CG.Pop ();
      ok := CG.Next_label ();
      CG.If_closure (rhsVal, CG.No_label, ok, CG.Always);
      CG.Abort (CG.RuntimeError.NarrowFailed);
(* TODO^ I think we need another runtime error code for assigning a nested
         procedure. *)
      CG.Set_label (ok);
      IF lhsIsPushed THEN
        CG.Push (lhsVal);
        CG.Free (lhsVal);
      END;
      CG.Push (rhsVal);
      CG.Free (rhsVal);
    | RTCheckKind.None =>
      Expr.Compile (rhsExpr);
    END; 
  END AssignOrRTCheckProcedure;

PROCEDURE RTCheckRecord (rhsExpr: Expr.T) =
  BEGIN
    IF Expr.IsDesignator (rhsExpr)
      THEN Expr.CompileLValue (rhsExpr, traced := FALSE);
      ELSE Expr.Compile (rhsExpr);
    END;
  END RTCheckRecord;

PROCEDURE RTCheckSet (tlhs: Type.T;  rhsExpr: Expr.T) =
  BEGIN
    IF Type.IsStructured (tlhs) THEN
      IF Expr.IsDesignator (rhsExpr)
        THEN Expr.CompileLValue (rhsExpr, traced := FALSE);
        ELSE Expr.Compile (rhsExpr);
      END;
    ELSE (* small set *)
      Expr.Compile (rhsExpr);
    END;
  END RTCheckSet;

PROCEDURE RTCheckArray (tlhs: Type.T;  rhsExpr: Expr.T) =
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
  END RTCheckArray;

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

(* EXPORTED: *) 
PROCEDURE DoGenRTAbort ( Code: CG.RuntimeError ): BOOLEAN =
  BEGIN
    CASE Code OF
    | CG.RuntimeError.ValueOutOfRange
    => RETURN Host.doRangeChk;
    | CG.RuntimeError.NarrowFailed
    => RETURN Host.doNarrowChk;
    | CG.RuntimeError.IncompatibleArrayShape
    => RETURN Host.doNarrowChk;
(* TODO: We should have a separate compiler switch for this one. *)
    ELSE RETURN FALSE;
    END;
  END DoGenRTAbort;

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
    DoEmit (tlhs, p.rhs, Expr.Alignment (p.lhs), initializing := FALSE);
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
