(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Formal.m3                                             *)
(* Last modified on Tue May 23 15:42:16 PDT 1995 by kalsow     *)
(*      modified on Fri Nov  9 20:39:07 1990 by muller         *)

MODULE Formal;

IMPORT M3, M3ID, CG, Value, ValueRep, Type, Error, Expr, ProcType;
IMPORT KeywordExpr, OpenArrayType, RefType, ErrType, CheckExpr;
IMPORT ArrayType, ArrayExpr, Host, NarrowExpr, M3Buf, Tracer;
IMPORT Procedure, UserProc, Target, M3RT;

TYPE
  T = Value.T BRANDED OBJECT 
        offset   : INTEGER;
        tipe     : Type.T;
        dfault   : Expr.T;
        refType  : Type.T; (* Non-NIL IFF it's an open array passed by VALUE. *)
        cg_type  : CG.TypeUID;
        mode     : Mode;
        kind     : Type.Class;
        trace    : Tracer.T;
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
        repTypeOf   := TypeOf (* Never differs from semType. *);
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
    t.dfault   := NIL;
    t.unused   := FALSE;
    t.kind     := Type.Class.Error;
    t.refType  := NIL;
    t.cg_type  := 0;
    t.trace    := NIL;
    RETURN t;
  END NewBuiltin;

(*EXPORTED*)
PROCEDURE New (READONLY info: Info): Value.T =
  VAR t := NEW (T);
  BEGIN
    ValueRep.Init (t, info.name, Value.Class.Formal);
    t.readonly := (info.mode = Mode.mCONST);
    t.offset   := info.offset;
    t.mode     := info.mode;
    t.tipe     := info.type;
    t.dfault   := info.dfault;
    t.unused   := info.unused;
    t.kind     := Type.Class.Error;
    t.refType  := NIL;
    t.cg_type  := 0;
    t.trace    := info.trace;
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
  VAR
    t     : T := formal;
    type  : Type.T;
    mtype : CG.Type;
    size  : CG.Size;
    align : CG.Alignment;
    info  : Type.Info;
  BEGIN
    IF (types_only) THEN
      type := TypeOf (t);
      Type.Compile (type);
      t.cg_type := Type.GlobalUID (type);
      IF (t.mode # Mode.mVALUE) OR (t.refType # NIL) THEN
        t.cg_type := CG.Declare_indirect (t.cg_type);
        Type.Compile (t.refType);
      END;
    ELSIF (param) THEN
      type  := TypeOf (t);
      IF (t.mode = Mode.mVALUE) AND (t.refType = NIL) THEN
        EVAL Type.CheckInfo (type, info);
        size  := info.size;
        align := info.alignment;
        mtype := info.mem_type;
      ELSE
        size  := Target.Address.size;
        align := Target.Address.align;
        mtype := CG.Type.Addr;
      END;
      EVAL CG.Declare_param (t.name, size, align, mtype,
                             t.cg_type, in_memory := FALSE, up_level := FALSE,
                             f := CG.Maybe);
    ELSE
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
    | NULL => RETURN FALSE;
    | T(t) => refType := t.refType;  RETURN (refType # NIL);
    ELSE      RETURN FALSE;
    END;
  END OpenArrayByVALUE;

PROCEDURE TypeOf (t: T): Type.T =
  BEGIN
    IF (t.tipe = NIL) THEN t.tipe := Expr.TypeOf (t.dfault) END;
    RETURN t.tipe;
  END TypeOf;

PROCEDURE Check (t: T;  VAR cs: Value.CheckState) =
  VAR info: Type.Info;
  BEGIN
    t.tipe := Type.CheckInfo (TypeOf (t), info);
    t.kind := info.class;
    IF (info.class = Type.Class.Packed) THEN
      EVAL Type.CheckInfo (Type.Base (t.tipe), info);
      t.kind := info.class;
    END;

    IF (t.dfault # NIL) THEN
      Expr.TypeCheck (t.dfault, cs);
      IF (t.mode = Mode.mVAR) THEN
        Error.ID (t.name, "VAR parameters cannot have defaults");
      END;
      IF  NOT Type.IsAssignable (t.tipe, Expr.TypeOf (t.dfault)) THEN
        Error.ID (t.name, "default is not assignable to formal");
      END;
      IF (Expr.ConstValue (t.dfault) = NIL) THEN
        Error.ID (t.name, "default is not constant");
      END;
      (* NOTE: we don't save the constant-folded version of the default,
         otherwise we'd loose references to large named constants. *)
    END;

    IF (t.mode = Mode.mVALUE) AND OpenArrayType.Is (Type.Base (t.tipe)) THEN
      t.refType := RefType.New (t.tipe, traced := TRUE, brand := NIL);
      t.refType := Type.Check (t.refType);
    END;

  END Check;

PROCEDURE Load (t: T) =
  BEGIN
    IF (t.dfault = NIL) THEN
      Error.ID (t.name, "formal has no default value");
    END;
    EVAL Expr.Use (t.dfault);
(* TODO^ Suppress generating code if Expr.Use returns TRUE? *)
    Expr.Prep (t.dfault);
    Expr.Compile (t.dfault);
  END Load;

PROCEDURE Compile (t: T) =
  BEGIN
    Type.Compile (t.tipe);
    Type.Compile (t.refType);
    IF (t.dfault # NIL) THEN
      Type.Compile (Expr.TypeOf (t.dfault));
    END;
  END Compile;

PROCEDURE SetGlobals (<*UNUSED*> t: T) =
  BEGIN
    (* Type.SetGlobals (t.tipe); *)
    (* Type.SetGlobals (t.refType); *)
    (* IF (t.dfault # NIL) THEN Type.SetGlobals (Expr.TypeOf (t.dfault)) END; *)
  END SetGlobals;

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
      Error.Msg ("too many actual parameters" & ProcName (proc));
      ok := FALSE;
    END;

    (* initialize the argument list *)
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
            Error.ID (name, "unknown parameter");
            ok := FALSE;
            j := i;
            EXIT;
          END;
          IF (slots[j].name = name) THEN EXIT END;
          INC (j);
        END;
      ELSE
        IF (NOT posOK) THEN
          Error.Msg ("positional parameters must precede keyword parameters"
                       & ProcName (proc));
          ok := FALSE;
        END;
        j := i;
      END;
      WITH z = slots[j] DO
        IF (z.matched) THEN
          Err (z, "parameter already specified");
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
        Err (slots[i], "parameter not specified");
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
              Err (slots[i], "actual not assignable to VALUE formal");
              ok := FALSE;
            END;
        | Mode.mVAR =>
            IF NOT Expr.IsDesignator (actualExpr) THEN
              Err (slots[i], "VAR actual must be a designator");
              ok := FALSE;
            ELSIF NOT Expr.IsWritable (actualExpr, traced := TRUE) THEN
              Err (slots[i], "VAR actual must be writable");
              ok := FALSE;
            ELSIF Type.IsEqual (t, te, NIL) 
                  OR (ArrayType.Split (t, index, elt)
                      AND ArrayType.Split (te, index, elt)
                      AND Type.IsAssignable (t, te) ) THEN
              Expr.NeedsAddress (actualExpr);
            ELSE
              Err (slots[i], "actual not compatible with VAR formal");
              ok := FALSE;
            END;
        | Mode.mCONST =>
            IF NOT Type.IsAssignable (t, te) THEN
              Err (slots[i], "actual not assignable to READONLY formal");
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
PROCEDURE PrepArg (formalValue: Value.T; actual: Expr.T) =
  VAR formal: T := formalValue;
  BEGIN
    ArrayExpr.NoteTargetType (actual, Value.TypeOf (formal));

    CASE formal.mode OF
    | Mode.mVALUE => (* Will pass by value. *)
        Expr.Prep (actual);
    | Mode.mVAR => (* Will pass by reference. *)
        IF Expr.Alignment (actual) MOD Target.Byte = 0 THEN
          Expr.PrepLValue (actual, traced := TRUE);
        ELSE
          Error.ID (formal.name,
            "CM3 restriction: non-byte-aligned value cannot be passed VAR (2.3.2)"); 
        END;
    | Mode.mCONST =>
        IF Expr.IsDesignator (actual)
           AND ( Type.IsEqual (formal.tipe, Expr.TypeOf (actual), NIL)
                 OR ArrayType.Is (formal.tipe) (* => assignable. *)
               )
(* CHECK^ This condition against that found in GenArray. *)
        THEN (* Will pass by ref. *)
          IF Expr.Alignment (actual) MOD Target.Byte = 0 THEN
            Expr.PrepLValue (actual, traced := FALSE);
          ELSE
            Error.ID (formal.name,
              "CM3 restriction: non-byte-aligned value cannot be passed by reference (2.3.2)");
            formal.tipe := ErrType.T;
            formal.kind := Type.Class.Error
          END; 
        ELSE (* Treat as VALUE: will make a copy and pass it by reference. *)
          Expr.Prep (actual);
        END (*IF*) 
    END;
  END PrepArg;

(*EXPORTED*)
PROCEDURE EmitArg (proc: Expr.T;  formalValue: Value.T; actual: Expr.T) =
  VAR formal: T := formalValue;
  BEGIN
    CASE formal.kind OF
    | Type.Class.Error, Type.Class.Named, Type.Class.Packed
        =>  <*ASSERT FALSE*>
    | Type.Class.Integer, Type.Class.Enum, Type.Class.Subrange,
      Type.Class.Longint
        =>  GenOrdinal (formal, actual);
    | Type.Class.Real, Type.Class.Longreal, Type.Class.Extended
        =>  GenFloat (formal, actual);
    | Type.Class.Ref, Type.Class.Object, Type.Class.Opaque
        =>  GenReference (formal, actual);
    | Type.Class.Procedure
        =>  GenProcedure (formal, actual, proc);
    | Type.Class.Record
        =>  GenRecord (formal, actual);
    | Type.Class.Set
        =>  GenSet (formal, actual);
    | Type.Class.Array
        =>  GenArray (formal, actual, formal_is_open := FALSE);
    | Type.Class.OpenArray
        =>  GenArray (formal, actual, formal_is_open := TRUE);
    END;
  END EmitArg;

PROCEDURE GenOrdinal (t: T;  actual: Expr.T) =
  VAR min, max: Target.Int;  (** constant := Expr.ConstValue (actual); **)
  BEGIN
    (***
      -- we can't fold constant actuals since they may have been precompiled
         and have allocated temporaries that still need to be freed ....
    IF (constant # NIL) THEN actual := constant END;
    ***)
    CASE t.mode OF
    | Mode.mVALUE =>
        EVAL Type.GetBounds (t.tipe, min, max); (* Of formal. *) 
        CheckExpr.EmitChecks (actual, min, max, CG.RuntimeError.ValueOutOfRange);
        CG.Pop_param (Type.CGType (t.tipe, in_memory := TRUE));
    | Mode.mVAR =>
        IF Expr.Alignment (actual) MOD Target.Byte = 0 THEN
          Expr.CompileAddress (actual, traced := TRUE);
          CG.Pop_param (CG.Type.Addr);
          Expr.NoteWrite (actual);
        ELSE (* Error recovery. *) 
          CG.Load_nil ();
          CG.Pop_param (CG.Type.Addr);
        END
    | Mode.mCONST =>
        IF NOT Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) THEN
          EVAL Type.GetBounds (t.tipe, min, max); (* Of formal. *) 
          CheckExpr.EmitChecks (actual, min, max, CG.RuntimeError.ValueOutOfRange);
          GenCopy (t.tipe);
        ELSIF Expr.IsDesignator (actual) THEN
          IF Expr.Alignment (actual) MOD Target.Byte = 0 THEN
            Expr.CompileAddress (actual, traced := FALSE);
          ELSE (* Error recovery. *) 
            CG.Load_nil ();
          END
        ELSE (* non-designator, same type *)
          Expr.Compile (actual);
          GenCopy (t.tipe); 
        END;
        CG.Pop_param (CG.Type.Addr);
    END;
  END GenOrdinal;

PROCEDURE GenFloat (t: T;  actual: Expr.T) =
  BEGIN
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Compile (actual);
        CG.Pop_param (Type.CGType (t.tipe, in_memory := TRUE));
    | Mode.mVAR =>
        Expr.CompileAddress (actual, traced := TRUE);
        CG.Pop_param (CG.Type.Addr);
        Expr.NoteWrite (actual);
    | Mode.mCONST =>
        IF Expr.IsDesignator (actual)
           AND Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) THEN
          Expr.CompileAddress (actual, traced := FALSE);
        ELSE
          Expr.Compile (actual);
          GenCopy (t.tipe);
        END;
        CG.Pop_param (CG.Type.Addr);
    END;
  END GenFloat;

PROCEDURE GenReference (t: T;  actual: Expr.T) =
  VAR t_actual := Expr.TypeOf (actual);
  BEGIN
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Compile (actual);
        CG.Pop_param (Type.CGType (t.tipe, in_memory := TRUE));
    | Mode.mVAR =>
        Expr.CompileAddress (actual, traced := TRUE);
        CG.Pop_param (CG.Type.Addr);
        Expr.NoteWrite (actual);
    | Mode.mCONST =>
        IF NOT Type.IsEqual (t.tipe, t_actual, NIL) THEN
          Expr.Compile (actual);
          GenCopy (t.tipe);
        ELSIF Expr.IsDesignator (actual) THEN
          Expr.CompileAddress (actual, traced := FALSE);
        ELSE (* Same type, non-designator. *) 
          Expr.Compile (actual);
          (* A reference actual could be a NARROW, which, by language
             definition, produces a non-designator, but Expr.Compile
             does not generate a copy, so we do it here.  Are there
             other non-designator reference cases that would not be
             aliasable, and thus not need a copy? *) 
          GenCopy (t.tipe);
        END;
        CG.Pop_param (CG.Type.Addr);
    END;
  END GenReference;

PROCEDURE GenProcedure (t: T;  actual: Expr.T;  proc: Expr.T) =
  BEGIN
    CASE t.mode OF
    | Mode.mVALUE =>
        Expr.Compile (actual);
        GenClosure (actual, proc);
        CG.Pop_param (Type.CGType (t.tipe, in_memory := TRUE));
    | Mode.mVAR =>
        Expr.CompileAddress (actual, traced := TRUE);
        CG.Pop_param (CG.Type.Addr);
        Expr.NoteWrite (actual);
    | Mode.mCONST =>
        IF Expr.IsDesignator (actual)
           AND Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) THEN
          Expr.CompileAddress (actual, traced := FALSE);
        ELSE
          Expr.Compile (actual);
          GenClosure (actual, proc);
          GenCopy (t.tipe);
        END;
        CG.Pop_param (CG.Type.Addr);
    END;
  END GenProcedure;

PROCEDURE GenClosure (actual: Expr.T;  proc: Expr.T) =
  VAR tmp: CG.Var;  proc_v: Value.T;  n_elts: INTEGER;
    ASIZE := Target.Address.size;
  BEGIN
    IF RequiresClosure (actual, proc_v) THEN
      (* actual is a nested procedure literal passed by value *)
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
      CG.Load_addr_of_temp (tmp,  0, Target.Address.align);
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

PROCEDURE GenRecord (t: T;  actual: Expr.T) =
  VAR info: Type.Info;
  BEGIN
    (* <* ASSERT Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) *> *)
    CASE t.mode OF
    | Mode.mVALUE =>
        EVAL Expr.Use (actual);
(* TODO^ Suppress generating code if Expr.Use returns TRUE? *)
        Expr.Compile (actual);
        EVAL Type.CheckInfo (t.tipe, info);
        Type.Compile (t.tipe);
        CG.Pop_struct (Type.GlobalUID (t.tipe), info.size, info.alignment);
    | Mode.mVAR =>
        IF Expr.Alignment (actual) MOD Target.Byte = 0 THEN
          Expr.CompileAddress (actual, traced := TRUE);
          CG.Pop_param (CG.Type.Addr);
          Expr.NoteWrite (actual);
        ELSE (* Error recovery. *) 
          CG.Load_nil ();
          CG.Pop_param (CG.Type.Addr);
        END
    | Mode.mCONST =>
        EVAL Expr.Use (actual);
(* TODO^ Suppress generating code if Expr.Use returns TRUE? *)
        IF Expr.IsDesignator (actual)
           AND Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) THEN
          IF Expr.Alignment (actual) MOD Target.Byte = 0 THEN
            Expr.CompileAddress (actual, traced := FALSE);
          ELSE (* Error recovery. *) 
            CG.Load_nil ();
          END
        ELSE
          Expr.Compile (actual);
          GenCopy (t.tipe); 
        END;
        CG.Pop_param (CG.Type.Addr);
    END;
  END GenRecord;

PROCEDURE GenSet (t: T;  actual: Expr.T) =
  VAR info: Type.Info;
  BEGIN
    CASE t.mode OF
    | Mode.mVALUE =>
        EVAL Expr.Use (actual);
        Expr.Compile (actual);
        IF Type.IsStructured (t.tipe) THEN
          EVAL Type.CheckInfo (t.tipe, info);
          Type.Compile (t.tipe);
          CG.Pop_struct (Type.GlobalUID (t.tipe), info.size, info.alignment);
        ELSE
          CG.Pop_param (Type.CGType (t.tipe, in_memory := TRUE));
        END;
    | Mode.mVAR =>
        <* ASSERT Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL) *>
        IF Expr.Alignment (actual) MOD Target.Byte = 0 THEN
          Expr.CompileAddress (actual, traced := TRUE);
          CG.Pop_param (CG.Type.Addr);
          Expr.NoteWrite (actual);
        ELSE (* Error recovery. *) 
          CG.Load_nil ();
          CG.Pop_param (CG.Type.Addr);
        END
    | Mode.mCONST =>
        EVAL Expr.Use (actual);
        IF Expr.IsDesignator (actual)
           AND Type.IsEqual (t.tipe, Expr.TypeOf (actual), NIL)
        THEN (* Pass truly by reference. *) 
          IF Expr.Alignment (actual) MOD Target.Byte = 0 THEN
            Expr.CompileAddress (actual, traced := FALSE);
          ELSE (* Error recovery. *) 
            CG.Load_nil ();
          END
        ELSE (* Pass by value (actually, pass a copy by reference). *) 
          Expr.Compile (actual);
          GenCopy (t.tipe);
        END;
        CG.Pop_param (CG.Type.Addr);
    END;
  END GenSet;

PROCEDURE PassArrayVAR (formal: T; actual: Expr.T; actualRepType: Type.T) =
  BEGIN
    IF Expr.Alignment (actual) MOD Target.Byte = 0 THEN
      Expr.CompileAddress (actual, traced := TRUE);
      RedepthArray (formal.tipe, actualRepType);
    ELSE (* Error recovery. *)
      CG.Load_nil ();
    END (*IF*);
    CG.Pop_param (CG.Type.Addr);
  END PassArrayVAR;

PROCEDURE GenArray (formal: T; actual: Expr.T; formal_is_open: BOOLEAN) =
  VAR actualRepType: Type.T; 
  VAR formalTypeInfo: Type.Info;
  BEGIN
    actualRepType := Expr.RepTypeOf (actual);
    Type.Compile (formal.tipe);
    CASE formal.mode OF

    | Mode.mVALUE =>
        EVAL Expr.Use (actual);
(* TODO^ Suppress generating code if Expr.Use returns TRUE? *)
        Expr.Compile (actual); (* This being an array, will compile an address. *)
        RedepthArray (formal.tipe, actualRepType);
(* TODO: Avoid unnecessary copies when actual ia an array constructor or function
             result, in a newly-filled temporary. *)
        IF formal_is_open THEN
          (* Pass address, callee prolog will do copy, if needed. *)
          CG.Pop_param (CG.Type.Addr);
        ELSE (* Pass the (static-sized) value. *)
          EVAL Type.CheckInfo (formal.tipe, formalTypeInfo);
          Type.Compile (formal.tipe);
          CG.Pop_struct
            (Type.GlobalUID (formal.tipe), formalTypeInfo.size, formalTypeInfo.alignment);
          (* ^Which copies the whole array to the callee. *)
        END;

    | Mode.mVAR =>
        PassArrayVAR (formal, actual, actualRepType);
        Expr.NoteWrite (actual);

    | Mode.mCONST =>
        EVAL Expr.Use (actual);
(* TODO^ Suppress generating code if Expr.Use returns TRUE? *)
        IF Expr.IsDesignator (actual) THEN (* Pass as VAR. *)
          PassArrayVAR (formal, actual, actualRepType);
        ELSE
          Expr.Compile (actual);
          RedepthArray (formal.tipe, actualRepType);
          CG.Pop_param (CG.Type.Addr);
        END;
    END (*CASE*);
  END GenArray;

PROCEDURE RedepthArray (formalType, actualType: Type.T) =
(* PRE: Actual is pushed on CG stack. *)
(* Change TOS array from actualType's open depth to formalType's open depth,
   while preserving shape.  Generate any necessary RT length checks. *)
  VAR
    formalDepth, actualDepth: INTEGER;
    indexType, eltType: Type.T;
    tempVar: CG.Var;
    actualVal: CG.Val;
    b: BOOLEAN;
  BEGIN
    IF Type.IsEqual (formalType, actualType, NIL) THEN RETURN END;

    formalDepth := OpenArrayType.OpenDepth (formalType);
    actualDepth := OpenArrayType.OpenDepth (actualType);

    IF (formalDepth = actualDepth) THEN RETURN END;

    (* capture the actual *)
    actualVal := CG.Pop ();

    IF (formalDepth > actualDepth) THEN
      (* build a bigger dope vector *)
      tempVar := OpenArrayType.DeclareDopeTemp (formalType);

      (* copy the data pointer *)
      CG.Push (actualVal);
      IF (actualDepth > 0) THEN CG.Open_elt_ptr (Target.Byte) END;
      CG.Store_addr (tempVar, M3RT.OA_elt_ptr);

      (* fill in the sizes *)
      FOR i := 0 TO formalDepth-1 DO
        b := ArrayType.Split (actualType, indexType, eltType);  <*ASSERT b*>
        IF (indexType = NIL) THEN
          CG.Push (actualVal);
          CG.Open_size (i);
        ELSE
          CG.Load_integer (Target.Integer.cg_type, Type.Number (indexType));
        END;
        CG.Store_int (Target.Integer.cg_type,
                      tempVar, M3RT.OA_sizes + i * Target.Integer.pack);
        actualType := eltType;
      END;

      (* leave the result *)
      CG.Load_addr_of_temp (tempVar, 0, Target.Address.align);
    ELSE (* formalDepth < actualDepth *)
      (* check some array bounds;  don't build a smaller dope vector
         just reuse the existing one! *)

      formalType := OpenArrayType.NonopenEltType (formalType);
      FOR i := formalDepth TO actualDepth - 1 DO
        b := ArrayType.Split (formalType, indexType, eltType); <*ASSERT b*>
        <*ASSERT indexType # NIL*>
        CG.Push (actualVal);
        CG.Open_size (i);
        CG.Load_integer (Target.Integer.cg_type, Type.Number (indexType));
        CG.Check_eq (Target.Integer.cg_type,
                     CG.RuntimeError.IncompatibleArrayShape);
        formalType := eltType;
      END;

      (* leave the old dope vector as the result *)
      CG.Push (actualVal);
      IF (formalDepth <= 0) THEN CG.Open_elt_ptr (Target.Byte); END;
      CG.ForceStacked ();
    END;

    CG.Free (actualVal);
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
