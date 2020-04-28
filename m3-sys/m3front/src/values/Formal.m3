(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Formal.m3                                             *)
(* Last modified on Tue May 23 15:42:16 PDT 1995 by kalsow     *)
(*      modified on Fri Nov  9 20:39:07 1990 by muller         *)

MODULE Formal;

IMPORT M3, M3ID, CG, Value, ValueRep, Type, Error, Expr, ProcType;
IMPORT KeywordExpr, OpenArrayType, RefType, ErrType, CheckExpr;
IMPORT ArrayType, ArrayExpr, SetType, Host, NarrowExpr, M3Buf, Tracer;
IMPORT Procedure, UserProc, Target, M3RT;

TYPE
  T = Value.T BRANDED OBJECT 
        offset   : INTEGER;
        tipe     : Type.T;
        repType  : Type.T;
        dfault   : Expr.T;
        refType  : Type.T; (* Needed to copy an open array. *)
        cg_type  : CG.TypeUID;
        mode     : Mode;
        kind     : Type.Class;
        trace    : Tracer.T;
        openArray: BOOLEAN;
      OVERRIDES
        typeCheck   := Check;
        set_globals := SetGlobals;
        load        := Load;
        declare     := ValueRep.Never;
        const_init  := ValueRep.NoInit;
        need_init   := ValueRep.Never;
        lang_init   := Compile;
        user_init   := ValueRep.NoInit;
        toExpr      := ValueRep.NoExpr;
        toType      := ValueRep.NoType;
        typeOf      := TypeOf;
        repTypeOf   := RepTypeOf;
        base        := ValueRep.Self;
        add_fp_tag  := AddFPTag;
        fp_type     := TypeOf;
      END;

TYPE
  ArgSlot = RECORD
    formal  : T;
    actual  : Expr.T;
    name    : M3ID.T;
    matched : BOOLEAN;
    errored : BOOLEAN;
  END;

(*EXPORTED*)
PROCEDURE NewBuiltin (name: TEXT;  offset: INTEGER;  type: Type.T): Value.T =
  VAR t := NEW (T);
  BEGIN
    ValueRep.Init (t, M3ID.Add (name), Value.Class.Formal);
    t.readonly := FALSE;
    t.offset   := offset;
    t.mode     := Mode.mVALUE;
    t.tipe     := type;
    t.repType  := NIL;
    t.dfault   := NIL;
    t.unused   := FALSE;
    t.kind     := Type.Class.Error;
    t.refType  := NIL;
    t.cg_type  := 0;
    t.trace    := NIL;
    t.openArray := FALSE;
    RETURN t;
  END NewBuiltin;

(*EXPORTED*)
PROCEDURE New (READONLY info: Info): Value.T =
  VAR t := NEW (T);
  BEGIN
    ValueRep.Init (t, info.name, Value.Class.Formal);
    t.readonly := (info.mode = Mode.mREADONLY);
    t.offset   := info.offset;
    t.mode     := info.mode;
    t.tipe     := info.type;
    t.repType  := NIL;
    t.dfault   := info.dfault;
    t.unused   := info.unused;
    t.kind     := Type.Class.Error;
    t.refType  := NIL;
    t.cg_type  := 0;
    t.trace    := info.trace;
    t.openArray := FALSE;
    RETURN t;
  END New;

(*EXPORTED*)
PROCEDURE Split (formal: Value.T;  VAR info: Info) =
  VAR t: T := formal;
  BEGIN
    info.name   := t.name;
    info.offset := t.offset;
    info.mode   := t.mode;
    info.type   := TypeOf (t);
    info.dfault := t.dfault;
    info.unused := t.unused;
    info.trace  := t.trace;
  END Split;

(*EXPORTED*)
PROCEDURE EmitDeclaration (formal: Value.T;  types_only, param: BOOLEAN) =
(* Only for a formal of a procedure type or an imported procedure. *)
  VAR
    t        : T := formal;
    type     : Type.T;
    repType  : Type.T;
    mtype    : CG.Type;
    size     : CG.Size;
    align    : CG.Alignment;
    info     : Type.Info;
  BEGIN
    IF (types_only) THEN
      type := TypeOf (t);
      Type.Compile (type);
      repType := RepTypeOf (t);
      IF repType # type THEN Type.Compile (repType) END;
      Type.Compile (t.refType);
      IF (t.dfault # NIL) THEN Type.Compile (Expr.TypeOf (t.dfault)) END;
      t.cg_type := Type.GlobalUID (type);
      IF t.mode # Mode.mVALUE OR t.openArray
      THEN (* lo-level pass by reference. *)
        t.cg_type := CG.Declare_indirect (t.cg_type);
      END;
    ELSIF (param) THEN
      type  := TypeOf (t);
      IF t.mode # Mode.mVALUE OR t.openArray
      THEN (* lo-level pass by reference. *)
        size  := Target.Address.size;
        align := Target.Address.align;
        mtype := CG.Type.Addr;
      ELSE (* lo-level pass by value. *)
        EVAL Type.CheckInfo (type, info);
        size  := info.size;
        align := info.alignment;
        mtype := info.mem_type;
      END;
      EVAL CG.Declare_param (t.name, size, align, mtype,
                             t.cg_type, in_memory := FALSE, up_level := FALSE,
                             f := CG.Maybe);
    ELSE (* This is part of debug info for a signature. *)
      CG.Declare_formal (t.name, t.cg_type);
    END;
  END EmitDeclaration;

(*EXPORTED*)
PROCEDURE HasClosure (formal: Value.T): BOOLEAN =
  BEGIN
    TYPECASE formal OF
    | NULL => RETURN FALSE;
    | T(t) => RETURN (t.mode # Mode.mVAR)
                 AND ((t.kind = Type.Class.Procedure)
                      OR ProcType.Is (Type.Base (TypeOf (t))));
    ELSE      RETURN FALSE;
    END;
  END HasClosure;

(*EXPORTED*)
PROCEDURE OpenArrayByVALUE (formal: Value.T;  VAR refType: Type.T): BOOLEAN =
(* If 'formal' is a "VALUE ARRAY OF X" formal, sets 'refType' to "REF ARRAY OF X"
   and returns TRUE, otherwise returns FALSE. *)
  BEGIN
    TYPECASE formal OF
    | NULL =>
    | T(t)
    => IF t.mode = Mode.mVALUE AND t.openArray THEN
         refType := t.refType;
         <* ASSERT refType # NIL *>
         RETURN TRUE;
       END;
    ELSE
    END (*TYPECASE*);
    RETURN FALSE;
  END OpenArrayByVALUE;

(* Externally dispatched-to: *)
PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    IF (t.tipe = NIL) THEN t.tipe := Expr.TypeOf (t.dfault) END;
    RETURN t.tipe;
  END TypeOf;

(* Externally dispatched-to: *)
PROCEDURE RepTypeOf (t: T): Type.T =
  BEGIN
    IF t.repType = NIL THEN t.repType := Expr.RepTypeOf (t.dfault) END;
    RETURN t.repType;
  END RepTypeOf;

(* Externally dispatched-to: *)
PROCEDURE Check (t: T;  VAR cs: Value.CheckState) =
(* Only checks on the formal itself. *)
  VAR info: Type.Info;
  BEGIN
    t.tipe := Type.CheckInfo (TypeOf (t), info);
    t.repType := Type.StripPacked (t.tipe);
    EVAL Type.Check (t.repType);
    t.kind := info.class;
    IF (info.class = Type.Class.Packed) THEN (* Ignore BITS in setting class. *)
      EVAL Type.CheckInfo (Type.Base (t.tipe), info);
      t.kind := info.class;
    END;

    IF (t.dfault # NIL) THEN
      Expr.TypeCheck (t.dfault, cs);
      IF (t.mode = Mode.mVAR) THEN
        Error.ID (t.name, "VAR parameters cannot have defaults (2.2.8).");
        t.kind := Type.Class.Error;
      END;
      IF  NOT Type.IsAssignable (t.tipe, Expr.TypeOf (t.dfault)) THEN
        Error.ID (t.name, "default must be assignable to formal (2.2.8).");
(* FIXME ^ 2.2.8 says default must be a *member* of t.tipe, which is stronger
   than assignable. *)
        t.kind := Type.Class.Error;
      END;
      IF (Expr.ConstValue (t.dfault) = NIL) THEN
        Error.ID (t.name, "parameter default must be constant (2.2.8).");
        t.kind := Type.Class.Error;
      END;
      (* NOTE: we don't save the constant-folded version of the default,
         otherwise we'd loose references to large named constants. *)
    END;

    t.openArray := OpenArrayType.Is (Type.Base (t.tipe));
    IF t.openArray AND t.mode = Mode.mVALUE
       (* ^Open array VALUE formal is lo-level passed by reference.
          Prolog will make a copy. *)
       OR t.mode = Mode.mREADONLY (* lo-level passed by reference. *)
          (* ^Also, some actuals (non-designator, multi-use, and open array)
             passed READONLY will do call-site copying. *)
    THEN (* We need a reference type to the formal type to do open array
            copying. *)
      t.refType := RefType.New (t.tipe, traced := TRUE, brand := NIL);
      EVAL Type.Check (t.refType);
    END;

  END Check;

(* Externally dispatched-to: *)
PROCEDURE Load (t: T) =
  VAR useOK: BOOLEAN;
  BEGIN
    IF (t.dfault = NIL) THEN
      Error.ID (t.name, "formal has no default value");
    END;
    useOK := Expr.CheckUseFailure (t.dfault);
    IF useOK THEN
      Expr.Prep (t.dfault);
      Expr.Compile (t.dfault);
    END;
  END Load;

(* Externally dispatched-to: *)
PROCEDURE Compile (t: T) =
  BEGIN
    Type.Compile (t.tipe);
    IF t.repType # t.tipe THEN Type.Compile (t.repType) END;
    Type.Compile (t.refType);
    IF (t.dfault # NIL) THEN Type.Compile (Expr.TypeOf (t.dfault)) END;
  END Compile;

(* Externally dispatched-to: *)
PROCEDURE SetGlobals (<*UNUSED*> t: T) =
  BEGIN
    (* Type.SetGlobals (t.tipe); *)
    (* Type.SetGlobals (t.refType); *)
    (* IF (t.dfault # NIL) THEN Type.SetGlobals (Expr.TypeOf (t.dfault)) END; *)
  END SetGlobals;

(* Externally dispatched-to: *)
PROCEDURE AddFPTag  (t: T;  VAR x: M3.FPInfo): CARDINAL =
  CONST Tags = ARRAY Mode OF TEXT { "VALUE ", "VAR ", "READONLY "};
  BEGIN
    ValueRep.FPStart (t, x, Tags[t.mode], 0, global := FALSE);
    IF (t.dfault # NIL) THEN
      M3Buf.PutText (x.buf, " := ");
      Expr.GenFPLiteral (t.dfault, x.buf);
    END;
    RETURN 1;
  END AddFPTag;

(*--------------------------------------------------- actual typechecking ---*)

(*EXPORTED*)
PROCEDURE CheckArgs (VAR cs       : Value.CheckState;
                     VAR actuals  : Expr.List;
                         formals  : Value.T;
                         proc     : Expr.T): BOOLEAN =
(* Do checks on actual/formal relationship, for one call, all parameters. *)
  VAR slots: ARRAY [0..19] OF ArgSlot;  v: Value.T;  n: INTEGER;
  BEGIN
    v := formals;  n := 0;
    WHILE (v # NIL) DO  INC (n);  v := v.next;  END;
    IF (n <= NUMBER (slots))
      THEN RETURN DoCheckArgs (cs, actuals, formals, n, slots, proc);
      ELSE RETURN DoCheckArgs (cs, actuals, formals, n,
                                NEW (REF ARRAY OF ArgSlot, n)^, proc);
    END;
  END CheckArgs;

PROCEDURE DoCheckArgs (VAR cs       : Value.CheckState;
                       VAR actuals  : Expr.List;
                           formals  : Value.T;
                           nFormals : INTEGER;
                       VAR slots    : ARRAY OF ArgSlot;
                           proc     : Expr.T): BOOLEAN =
  (* Formals are linked.  Actuals are in an array.*)
  VAR
    j                 : INTEGER;
    actualExpr, value : Expr.T;
    index, elt, t, te : Type.T; 
    name              : M3ID.T;
    formal            : T;
    posOK, ok         : BOOLEAN;
  BEGIN
    ok := TRUE;

    IF (nFormals < NUMBER (actuals^)) THEN
      Error.Msg ("too many actual parameters (2.3.2): " & ProcName (proc));
      ok := FALSE;
    END;

    (* Initialize the local/formal argument array. *)
    formal := formals;
    WHILE (formal # NIL) DO
      WITH z = slots[formal.offset] DO
        z.formal  := formal;
        z.actual  := formal.dfault;
        z.matched := FALSE;
        z.errored := FALSE;
        z.name    := formal.name;
      END;
      formal := formal.next;
    END;

    (* bind the parameters *)
    posOK := TRUE;
    FOR i := 0 TO MIN (LAST (actuals^) , nFormals -1) DO
      actualExpr := actuals[i];
      IF KeywordExpr.Split (actualExpr, name, value) THEN
        posOK := FALSE;
        actualExpr := value;
        j := 0;
        LOOP
          IF (j >= nFormals) THEN
            Error.ID (name, "unknown parameter (2.3.2): ");
            ok := FALSE;
            j := i;
            EXIT;
          END;
          IF (slots[j].name = name) THEN EXIT END;
          INC (j);
        END;
      ELSE
        IF (NOT posOK) THEN
          Error.Msg
            ("positional parameters must precede keyword parameters (2.3.2): "
                       & ProcName (proc));
          ok := FALSE;
        END;
        j := i;
      END;
      WITH z = slots[j] DO
        IF (z.matched) THEN
          Err (z, "This parameter was previously specified (2.3.2): ");
          ok := FALSE;
        END;
        z.matched := TRUE;
        z.actual := actualExpr;
      END;
    END;

    (* check for any unspecified parameters *)
    FOR i := 0 TO nFormals - 1 DO
      IF (slots[i].actual # NIL) THEN slots[i].matched := TRUE END;
      IF NOT slots[i].matched THEN
        Err (slots[i], "parameter not specified (2.3.2): ");
        ok := FALSE;
      END;
    END;

    (* typecheck each binding *)
    FOR i := 0 TO nFormals - 1 DO
      actualExpr  := slots[i].actual;
      formal := slots[i].formal;
      IF (actualExpr # NIL) AND (formal # NIL) THEN
        (* we've got both a formal and an actual *)
        Expr.TypeCheck (actualExpr, cs);

        (* try to fold scalar constant values *)
        (* NOTE: if we fold named structured constants, we lose the
             names and hence generate code to build the value... *)
        IF NOT Type.IsStructured (formal.tipe) THEN
          value := Expr.ConstValue (actualExpr);
          IF value # NIL THEN
            actualExpr := value;
            slots[i].actual := actualExpr
          END;
        END;

        te := Expr.TypeOf (actualExpr);
        t  := formal.tipe;
        CASE formal.mode OF
        | Mode.mVALUE =>
            IF NOT Type.IsAssignable (t, te) THEN
              Err (slots[i], "actual not assignable to VALUE formal (2.3.2) ");
              ok := FALSE;
            END;
        | Mode.mVAR =>
            IF NOT Expr.IsDesignator (actualExpr) THEN
              Err (slots[i], "VAR actual must be a designator (2.3.2) ");
              ok := FALSE;
            END;
            IF NOT Expr.IsWritable (actualExpr, traced := TRUE) THEN
              Err (slots[i], "VAR actual must be writable (2.3.2): ");
              ok := FALSE;
            END;
            IF Type.IsEqual (t, te, NIL) 
                  OR (ArrayType.Split (t, index, elt)
                      AND ArrayType.Split (te, index, elt)
                      AND Type.IsAssignable (t, te) ) THEN
              Expr.NeedsAddress (actualExpr);
            ELSE
              Err (slots[i], "Actual's type must equal VAR formal's (2.3.2): ");
              ok := FALSE;
            END;
        | Mode.mREADONLY =>
            IF NOT Type.IsAssignable (t, te) THEN
              Err (slots[i],
                   "Actual must be assignable to READONLY formal (2.3.2): ");
              ok := FALSE;
            ELSIF Expr.IsDesignator (actualExpr)
                  AND Type.IsEqual (t, te, NIL) 
                      OR (ArrayType.Split (t, index, elt))
            THEN (* Pass by reference. *) 
              Expr.NeedsAddress (actualExpr);
            ELSE (* Type.IsAssignable (t, te), pass by value. *)
              (* we'll make a copy when it's generated *)
            END;
        END; (*case*)

        (* check to see if it's a reference and needs an implicit NARROW,
           which may generate a nested procedure call... *)
        IF (ok) AND Host.doNarrowChk
                AND ((formal.kind = Type.Class.Ref)
                  OR (formal.kind = Type.Class.Object)
                  OR (formal.kind = Type.Class.Opaque)) THEN
          IF NOT Type.IsSubtype (te, t) THEN
            (* This reference value needs an implicit NARROW *)
            actualExpr := NarrowExpr.New (actualExpr, t);
            slots[i].actual := actualExpr;
            Expr.TypeCheck (actualExpr, cs);
          END;
        END;

      END; (* if got actual & formal *)
    END (*FOR*);

    IF (NOT ok) THEN RETURN FALSE END;

    (* no more possible errors => build the new argument list *)
    IF (NUMBER (actuals^) # nFormals) THEN 
      actuals := NEW (Expr.List, nFormals) 
    END;
    FOR i := 0 TO nFormals - 1 DO  actuals[i] := slots[i].actual  END;
    RETURN TRUE;
  END DoCheckArgs;

PROCEDURE Err (VAR slot: ArgSlot;  msg: TEXT) =
  BEGIN
    IF (NOT slot.errored) THEN
      Error.ID (slot.name, msg);
      slot.errored := TRUE;
    END;
  END Err;

PROCEDURE ProcName (proc: Expr.T): TEXT =
  VAR v: Value.T;
  BEGIN
    IF (proc # NIL) AND UserProc.IsProcedureLiteral (proc, v) THEN
      RETURN ": " & Value.GlobalName (v, dots := TRUE, with_module := TRUE);
    ELSE
      RETURN "";
    END;
  END ProcName;

(*----------------------------------------------------------- caller code ---*)

(*EXPORTED*)
PROCEDURE PrepArg (formal: Value.T; actExpr: Expr.T) =
  VAR formVal: T := formal;
  BEGIN
    ArrayExpr.NoteTargetType (actExpr, formVal.tipe);
    Type.Compile (Expr.RepTypeOf (actExpr));

    CASE formVal.mode OF
    | Mode.mVALUE => (* Will pass by value. *)
        Expr.Prep (actExpr);
    | Mode.mVAR => (* Will pass by reference. *)
        IF Expr.Alignment (actExpr) MOD Target.Byte = 0 THEN
          Expr.PrepLValue (actExpr, traced := TRUE);
        ELSE
          Error.ID (formVal.name,
            "CM3 restriction: non-byte-aligned value cannot be passed VAR (2.3.2)"); 
          formVal.tipe := ErrType.T;
          formVal.kind := Type.Class.Error
        END;
    | Mode.mREADONLY =>
        IF Expr.IsDesignator (actExpr)
           AND ( Type.IsEqual (formVal.tipe, Expr.TypeOf (actExpr), NIL)
                 OR ArrayType.Is (formVal.tipe) (* => assignable. *)
               )
(* CHECK^ This condition against that found in GenArray. *)
        THEN (* Will pass by ref. *)
          IF Expr.Alignment (actExpr) MOD Target.Byte = 0 THEN
            Expr.PrepLValue (actExpr, traced := FALSE);
          ELSE
            Error.ID (formVal.name,
              "CM3 restriction: non-byte-aligned value cannot be passed by reference (2.3.2)");
            formVal.tipe := ErrType.T;
            formVal.kind := Type.Class.Error
          END; 
        ELSE (* Treat as VALUE: will make a copy and pass it by reference. *)
          Expr.Prep (actExpr);
        END (*IF*) 
    END;
  END PrepArg;

(*EXPORTED*)
PROCEDURE EmitArg (proc: Expr.T;  formalVal: Value.T; actExpr: Expr.T) =
  VAR form: T := formalVal;
  BEGIN
    CASE form.kind OF
    | Type.Class.Error
        =>  <*ASSERT FALSE*>
    | Type.Class.Named, Type.Class.Packed
        =>  <*ASSERT FALSE*>
    | Type.Class.Integer, Type.Class.Enum, Type.Class.Subrange,
      Type.Class.Longint
        =>  GenOrdinal (form, actExpr);
    | Type.Class.Real, Type.Class.Longreal, Type.Class.Extended
        =>  GenFloat (form, actExpr);
    | Type.Class.Ref, Type.Class.Object, Type.Class.Opaque
        =>  GenReference (form, actExpr);
    | Type.Class.Procedure
        =>  GenProcedure (form, actExpr, proc);
    | Type.Class.Set, Type.Class.Record
        =>  GenStruct
              (form, actExpr, formIsArray := FALSE, formIsOpen := FALSE);
    | Type.Class.Array
        =>  GenStruct
              (form, actExpr, formIsArray := TRUE, formIsOpen := FALSE);
    | Type.Class.OpenArray
        =>  GenStruct
              (form, actExpr, formIsArray := TRUE, formIsOpen := TRUE);
    END;
  END EmitArg;

PROCEDURE GenScalarCopy (type: Type.T) =
(* PRE: A scalar of type type is on top of CG stack. *)
(* POST: TOS replaced by the address of a temp containing a copy. *)
  VAR tempVar: CG.Var;
  VAR typeInfo: Type.Info;
  BEGIN
    EVAL Type.CheckInfo (type, typeInfo);
    tempVar := CG.Declare_temp
      (typeInfo.size, typeInfo.alignment, typeInfo.mem_type, in_memory := TRUE);
    CG.Store (tempVar, 0, typeInfo.size, typeInfo.alignment, typeInfo.mem_type);
    CG.Load_addr_of (tempVar, 0, typeInfo.alignment);
  END GenScalarCopy;

PROCEDURE GenOrdinal (t: T;  actExpr: Expr.T) =
  VAR min, max: Target.Int;  (** constant := Expr.ConstValue (actExpr); **)
  BEGIN
    (***
      -- we can't fold constant actExprs since they may have been precompiled
         and have allocated temporaries that still need to be freed ....
    IF (constant # NIL) THEN actExpr := constant END;
    ***)
    CASE t.mode OF
    | Mode.mVALUE =>
        EVAL Type.GetBounds (t.tipe, min, max); (* Of formal. *) 
        CheckExpr.EmitChecks (actExpr, min, max, CG.RuntimeError.ValueOutOfRange);
        CG.Pop_param (Type.CGType (t.tipe, in_memory := TRUE));
    | Mode.mVAR =>
        IF Expr.Alignment (actExpr) MOD Target.Byte = 0 THEN
          Expr.CompileAddress (actExpr, traced := TRUE);
          CG.Pop_param (CG.Type.Addr);
          Expr.NoteWrite (actExpr);
        ELSE (* Error recovery. *) 
          CG.Load_nil ();
          CG.Pop_param (CG.Type.Addr);
        END
    | Mode.mREADONLY =>
        IF NOT Type.IsEqual (t.tipe, Expr.TypeOf (actExpr), NIL) THEN
          EVAL Type.GetBounds (t.tipe, min, max); (* Of formal. *) 
          CheckExpr.EmitChecks (actExpr, min, max, CG.RuntimeError.ValueOutOfRange);
          GenScalarCopy (t.tipe);
          CG.Pop_param (CG.Type.Addr);
        ELSIF Expr.IsDesignator (actExpr) THEN
          IF Expr.Alignment (actExpr) MOD Target.Byte = 0 THEN
            Expr.CompileAddress (actExpr, traced := FALSE);
            CG.Pop_param (CG.Type.Addr);
          ELSE (* Error recovery. *) 
            CG.Load_nil ();
            CG.Pop_param (CG.Type.Addr);
          END
        ELSE (* non-designator, same type *)
          Expr.Compile (actExpr);
          GenScalarCopy (t.tipe);
          CG.Pop_param (CG.Type.Addr);
        END;
    END;
  END GenOrdinal;

PROCEDURE GenFloat (t: T;  actExpr: Expr.T) =
  BEGIN
    <* ASSERT Expr.Alignment (actExpr) MOD Target.Byte = 0 *>
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Compile (actExpr);
        CG.Pop_param (Type.CGType (t.tipe, in_memory := TRUE));
    | Mode.mVAR =>
        Expr.CompileAddress (actExpr, traced := TRUE);
        CG.Pop_param (CG.Type.Addr);
        Expr.NoteWrite (actExpr);
    | Mode.mREADONLY =>
        IF Expr.IsDesignator (actExpr)
           AND Type.IsEqual (t.tipe, Expr.TypeOf (actExpr), NIL) THEN
          Expr.CompileAddress (actExpr, traced := FALSE);
          CG.Pop_param (CG.Type.Addr);
        ELSE
          Expr.Compile (actExpr);
          GenScalarCopy (t.tipe);
          CG.Pop_param (CG.Type.Addr);
        END;
    END;
  END GenFloat;

PROCEDURE GenReference (t: T;  actExpr: Expr.T) =
  VAR t_actExpr := Expr.TypeOf (actExpr);
  BEGIN
    <* ASSERT Expr.Alignment (actExpr) MOD Target.Byte = 0 *>
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Compile (actExpr);
        CG.Pop_param (Type.CGType (t.tipe, in_memory := TRUE));
    | Mode.mVAR =>
        Expr.CompileAddress (actExpr, traced := TRUE);
        CG.Pop_param (CG.Type.Addr);
        Expr.NoteWrite (actExpr);
    | Mode.mREADONLY =>
        IF NOT Type.IsEqual (t.tipe, t_actExpr, NIL) THEN
          Expr.Compile (actExpr);
          GenScalarCopy (t.tipe);
          CG.Pop_param (CG.Type.Addr);
        ELSIF Expr.IsDesignator (actExpr) THEN
          Expr.CompileAddress (actExpr, traced := FALSE);
          CG.Pop_param (CG.Type.Addr);
        ELSE (* Same type, non-designator. *) 
          Expr.Compile (actExpr);
          (* A reference actual could be a NARROW, which, by language
             definition, produces a non-designator, but Expr.Compile
             does not generate a copy, so we do it here.  Are there
             other non-designator reference cases that would not be
             aliasable, and thus not need a copy? *) 
          GenScalarCopy (t.tipe);
          CG.Pop_param (CG.Type.Addr);
        END;
    END;
  END GenReference;

PROCEDURE GenProcedure (t: T;  actExpr: Expr.T;  proc: Expr.T) =
  BEGIN
    <* ASSERT Expr.Alignment (actExpr) MOD Target.Byte = 0 *>
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Compile (actExpr);
        GenClosure (actExpr, proc);
        CG.Pop_param (Type.CGType (t.tipe, in_memory := TRUE));
    | Mode.mVAR =>
        Expr.CompileAddress (actExpr, traced := TRUE);
        CG.Pop_param (CG.Type.Addr);
        Expr.NoteWrite (actExpr);
    | Mode.mREADONLY =>
        IF Expr.IsDesignator (actExpr)
           AND Type.IsEqual (t.tipe, Expr.TypeOf (actExpr), NIL) THEN
          Expr.CompileAddress (actExpr, traced := FALSE);
          CG.Pop_param (CG.Type.Addr);
        ELSE
          Expr.Compile (actExpr);
          GenClosure (actExpr, proc);
          GenScalarCopy (t.tipe);
          CG.Pop_param (CG.Type.Addr);
        END;
    END;
  END GenProcedure;

PROCEDURE GenClosure (actExpr: Expr.T;  proc: Expr.T) =
  VAR tmp: CG.Var; proc_v: Value.T; n_elts: INTEGER;
    ASIZE := Target.Address.size;
  BEGIN
    IF RequiresClosure (actExpr, proc_v) THEN
      (* actExpr is a nested procedure literal passed by value *)
      IF IsExternalProcedure (proc) THEN
        Error.Warn (1, "passing nested procedure to external procedure");
      END;

      (* allocate space for the closure *)
      n_elts := (M3RT.CL_SIZE + ASIZE - 1) DIV ASIZE;
      tmp := CG.Declare_temp (M3RT.CL_SIZE, Target.Address.align,
                              CG.Type.Struct, in_memory := TRUE);

      (* and fill it in *)
      CG.Store_addr (tmp, M3RT.CL_proc);
      CG.Load_intt  (M3RT.CL_marker_value);
      CG.Store_int (Target.Integer.cg_type, tmp, M3RT.CL_marker);
      Procedure.LoadStaticLink (proc_v);
      CG.Store_addr (tmp, M3RT.CL_frame);
      CG.Load_addr_of (tmp,  0, Target.Address.align);
    END;
  END GenClosure;

PROCEDURE RequiresClosure (e: Expr.T;  VAR proc: Value.T): BOOLEAN =
  BEGIN
    RETURN UserProc.IsProcedureLiteral (e, proc) AND Procedure.IsNested (proc);
  END RequiresClosure;

PROCEDURE IsExternalProcedure (e: Expr.T): BOOLEAN =
  VAR proc: Value.T;
  BEGIN
    RETURN UserProc.IsProcedureLiteral (e, proc) AND Value.IsExternal (proc);
  END IsExternalProcedure;

PROCEDURE CompileStruct (expr: Expr.T; traced: BOOLEAN) =
(* Push the address of a struct.  It need not be byte- sized or aligned. *)
  BEGIN
    IF Expr.IsDesignator (expr)
    THEN Expr.CompileLValue (expr, traced);
    ELSE Expr.Compile (expr) (* Constant constructor, function result, or subarray thereof. *);
         (* ^Which, for a struct will still push an address. *)
    END;
  END CompileStruct;

PROCEDURE IsBitPacked
  (formSize, formAlign, actSize, actAlign: INTEGER): BOOLEAN =
(* Not merely typed BITS-FOR and a field or element, but byte instructions
   cannot pass or copy it. *)
  BEGIN
    IF actSize < 0 (* Actual is open. *) THEN RETURN FALSE END;
    IF actSize MOD Target.Byte # 0 THEN RETURN TRUE END;
    IF actAlign MOD Target.Byte # 0 THEN RETURN TRUE END;
    IF formSize > 0 (* Formal is not open array. *) THEN
      IF actSize # formSize
         OR actAlign # formAlign THEN
        RETURN TRUE END;
      END;
    RETURN FALSE
  END IsBitPacked;

PROCEDURE CompileNCopyStructWInWord
  (actExpr:Expr.T; actSize: INTEGER; traced: BOOLEAN) =
  (* Compile and copy a struct that lies within a word, leaving the address
     of the copy on top of the CG stack.  The original struct may be bitpacked.
     The copy will be byte-aligned and right-justified within a word. *)
  VAR actTempVar: CG.Var;
  BEGIN
    <* ASSERT actSize <= Target.Word.size *>
    actTempVar := CG.Declare_temp
       (Target.Word.size, Target.Word.align, Target.Word.cg_type,
        in_memory := TRUE);
    CompileStruct (actExpr, traced);
    CG.Load_indirect (Target.Word.cg_type, 0, actSize);
    CG.Store_int (Target.Word.cg_type, actTempVar);
    IF Target.Little_endian THEN
    ELSE CG.Shift_left (Target.Word.cg_type);
    END;
    CG.Load_addr_of (actTempVar, 0, Target.Word.align);
  END CompileNCopyStructWInWord;

PROCEDURE CompileNPassStructByRef
  (formRepType: Type.T; actExpr: Expr.T; actRepType: Type.T; traced: BOOLEAN) =
  BEGIN
    CompileStruct (actExpr, traced);
    RedepthArray (formRepType, actRepType);
    CG.Pop_param (CG.Type.Addr);
  END CompileNPassStructByRef;

PROCEDURE GenStruct
  (formVal: T; actExpr: Expr.T; formIsArray, formIsOpen: BOOLEAN) =
  VAR actRepType: Type.T;
  VAR actTempVar: CG.Var;
  VAR formRepType: Type.T;
  VAR actRepTypeInfo: Type.Info;
  VAR formRepTypeInfo: Type.Info;
  VAR actAlign: INTEGER;
  VAR actStaticSize: INTEGER;
  VAR useOK: BOOLEAN := FALSE;
  BEGIN
    actRepType := Expr.RepTypeOf (actExpr);
    (* ^May be a BITS-FOR, type, but only if actExpr is a field or element. *)
    EVAL Type.CheckInfo (actRepType, actRepTypeInfo);
    formRepType := formVal.repType;
    EVAL Type.CheckInfo (formRepType, formRepTypeInfo);
    useOK := Expr.CheckUseFailure (actExpr);
    IF NOT useOK THEN RETURN END;

    CASE formVal.mode OF
    | Mode.mVALUE =>
        IF SetType.IsSmallSet (formRepType) THEN
          Expr.Compile (actExpr);
          (* ^Will push value and right-justify it within a word, even if
              non-byte-aligned and/or non-byte-sized. *)
          CG.Pop_param (Type.CGType (formRepType, in_memory := FALSE));
        ELSE
          actAlign := Expr.Alignment (actExpr);
          IF IsBitPacked
               (formRepTypeInfo.size, formRepTypeInfo.alignment,
                actRepTypeInfo.size, actRepTypeInfo.alignment)
          THEN (* Actual is bitpacked, which implies not open array.
                  Formal is never bitpacked, but could be open. *)
            (* Right-justify a copy of bitpacked struct and pass it by value. *)
            CompileNCopyStructWInWord
              (actExpr, actRepTypeInfo.size, traced := TRUE);
          ELSE (* No bit packing. *)
            CompileStruct (actExpr, traced := TRUE);
          END;
          RedepthArray (formRepType, actRepType);
          IF formIsOpen
          THEN (* Pass address.  Copy will be made by callee prolog. *)
            CG.Pop_param (CG.Type.Addr);
          ELSE
            CG.Pop_struct
              (Type.GlobalUID (formRepType), formRepTypeInfo.size,
               formRepTypeInfo.alignment);
          END;
        END;

    | Mode.mVAR =>
        CompileNPassStructByRef
          (formRepType, actExpr, actRepType, traced := TRUE);
        Expr.NoteWrite (actExpr);

    | Mode.mREADONLY =>
        IF (Expr.IsDesignator (actExpr)
            OR Expr.IsAnonConstructor (actExpr))
           (* ^We don't need a constructor to be a designator to avoid copying.
              This includes function results; constructors; and fields,
              elements, and SUBARRAYs thereof.  Any of these need not be copied
              because this its only use.  The formal will become a designator
              (per 2.6.3), but this is OK because it will be stored in memory.
              This makes it possible to test where a constant open array
              constructor has placed its dope. *)
            AND (formIsArray
                 OR (Type.IsEqual (formVal.tipe, Expr.SemTypeOf (actExpr), NIL)))
        THEN (* Pass truly by reference (2.3.2). *)
          CompileNPassStructByRef
            (formRepType, actExpr, actRepType, traced := FALSE);

        ELSE (* Pass by value (2.3.2).  Lo-level pass a copy by reference. *)
          actAlign := Expr.Alignment (actExpr);
          IF SetType.IsSmallSet (formRepType)
             OR IsBitPacked
                  (formRepTypeInfo.size, formRepTypeInfo.alignment,
                   actRepTypeInfo.size, actRepTypeInfo.alignment)
             (* ^Actual is a small set or bitpacked, which implies it is not
                 an open array.  Formals are never bitpacked. *)
          THEN
            (* Copy within-word value and pass it by reference. *)
            CompileNCopyStructWInWord
              (actExpr, actRepTypeInfo.size, traced := FALSE);
            RedepthArray (formRepType, actRepType);
            CG.Pop_param (CG.Type.Addr);
          ELSE (* Actual is byte-sized and aligned.
                  Pass by value.  Lo-level pass a copy by reference. *)

            IF NOT OpenArrayType.Is (formRepType)
            THEN (* Formal gives static size for the copy. *)
              actTempVar := CG.Declare_temp
                (formRepTypeInfo.size, formRepTypeInfo.alignment,
                 CG.Type.Struct, in_memory := TRUE);
              CG.Load_addr_of (actTempVar, 0, formRepTypeInfo.alignment);
              CompileStruct (actExpr, traced := FALSE);
              RedepthArray (formRepType, actRepType);
              CG.Copy (formRepTypeInfo.size, overlap := FALSE);
              CG.Load_addr_of (actTempVar, 0, formRepTypeInfo.alignment);
              CG.Pop_param (CG.Type.Addr);

            ELSE
              actStaticSize := Expr.StaticSize (actExpr);
              IF actStaticSize > 0 
              THEN (* Actual gives static size for the copy. *)
                actTempVar := CG.Declare_temp
                  (actStaticSize, actRepTypeInfo.alignment, CG.Type.Struct,
                   in_memory := TRUE);
                CG.Load_addr_of (actTempVar, 0, actRepTypeInfo.alignment);
                CompileStruct (actExpr, traced := FALSE);
                CG.Copy (actStaticSize, overlap := FALSE);
                CG.Load_addr_of (actTempVar, 0, actRepTypeInfo.alignment);
                RedepthArray (formRepType, actRepType);
                CG.Pop_param (CG.Type.Addr);
              ELSE (* No static size available. *)
  (* FIXME: -------V *)
                (* We are already inside a call, so can't do CopyOpenArray
                   here.  Must do it in Prep, under same conditions.
                   For now, punt and pass by reference, without copying. *)
                CompileNPassStructByRef
                  (formRepType, actExpr, actRepType, traced := FALSE);
             (* CompileStruct (actExpr, traced := FALSE);
                Variable.CopyOpenArray (actRepType, formVal.refType);
                RedepthArray (formRepType, actRepType);
                CG.Pop_param (CG.Type.Addr);
             *)

              END;
            END;
          END;
        END;
    END (*CASE*);
  END GenStruct;

PROCEDURE RedepthArray (formType, actType: Type.T) =
(* PRE: Address of actual is pushed on CG stack. *)
(* Change TOS array from actType's open depth to formType's open depth,
   while preserving shape.  Generate any necessary RT length checks. *)
  VAR
    formDepth, actDepth: INTEGER;
    indexType, eltType: Type.T;
    tempVar: CG.Var;
    actVal: CG.Val;
    b: BOOLEAN;
  BEGIN
    IF NOT ArrayType.Is (formType) THEN RETURN END;
    IF Type.IsEqual (formType, actType, NIL) THEN RETURN END;

    formDepth := OpenArrayType.OpenDepth (formType);
    actDepth := OpenArrayType.OpenDepth (actType);

    IF (formDepth = actDepth) THEN RETURN END;

    (* Capture the actual's address. *)
    actVal := CG.Pop ();

    IF (formDepth > actDepth) THEN
      (* Must build a bigger dope vector. *)
      tempVar := OpenArrayType.DeclareDopeTemp (formType);

      (* Copy the elements pointer. *)
      CG.Push (actVal);
      IF (actDepth > 0) THEN CG.Open_elt_ptr (Target.Byte) END;
      CG.Store_addr (tempVar, M3RT.OA_elt_ptr);

      (* Fill in the lengths. *)
      FOR i := 0 TO formDepth-1 DO
        b := ArrayType.Split (actType, indexType, eltType);  <*ASSERT b*>
        IF (indexType = NIL) THEN (* Actual is open at depth 'i'. *)
          CG.Push (actVal);
          CG.Open_size (i);
        ELSE
          CG.Load_integer (Target.Integer.cg_type, Type.Number (indexType));
        END;
        CG.Store_int (Target.Integer.cg_type,
                      tempVar, M3RT.OA_sizes + i * Target.Integer.pack);
        actType := eltType;
      END;

      (* Leave the address of the new dope. *)
      CG.Load_addr_of (tempVar, 0, Target.Address.align);
    ELSE (* formDepth < actDepth *)
      (* Check formal fixed array bounds;  Don't build a smaller dope vector.
         Just reuse the existing one! *)

      formType := OpenArrayType.NonopenEltType (formType);
      FOR i := formDepth TO actDepth - 1 DO
        b := ArrayType.Split (formType, indexType, eltType); <*ASSERT b*>
        <*ASSERT indexType # NIL*>
        CG.Push (actVal);
        CG.Open_size (i);
        CG.Load_integer (Target.Integer.cg_type, Type.Number (indexType));
        CG.Check_eq (Target.Integer.cg_type,
                     CG.RuntimeError.IncompatibleArrayShape);
        formType := eltType;
      END;

      (* Leave the old dope vector or elements pointer as the result. *)
      CG.Push (actVal);
      IF (formDepth <= 0) THEN CG.Open_elt_ptr (Target.Byte); END;
      CG.ForceStacked ();
    END;

    CG.Free (actVal);
  END RedepthArray;

(*EXPORTED*)
PROCEDURE GenCopy (type: Type.T) =
  (* PRE: Tos is addr of a variable of type 'type'. *)
  (* POST: TOS replaced by addr of a local copy thereof. *) 
  VAR info: Type.Info;  copyVar: CG.Var;  id: CG.TypeUID;
  BEGIN
    EVAL Type.CheckInfo (type, info);
    id := Type.GlobalUID (type);
    IF Type.IsStructured (type) THEN
      copyVar := CG.Declare_local (M3ID.NoID, info.size, info.alignment,
                               CG.Type.Struct, id, in_memory := TRUE,
                               up_level := FALSE, f := CG.Never);
      CG.Load_addr_of (copyVar, 0, info.alignment);
      CG.Swap ();
      CG.Copy (info.size, overlap := FALSE);
    ELSE
      copyVar := CG.Declare_local (M3ID.NoID, info.size, info.alignment,
                               info.mem_type, id, in_memory := TRUE,
                               up_level := FALSE, f := CG.Never);
      CG.Store (copyVar, 0, info.size, info.alignment, info.stk_type);
    END;
    CG.Load_addr_of (copyVar, 0, info.alignment);
  END GenCopy;

BEGIN
END Formal.
