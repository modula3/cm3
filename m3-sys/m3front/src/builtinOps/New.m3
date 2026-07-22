(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: New.m3                                                *)
(* Last Modified On Tue Jun 20 08:30:57 PDT 1995 By kalsow     *)
(*      Modified On Thu Jun 15 12:45:06 PDT 1995 By ericv      *)
(*      Modified On Fri Jan 25 08:10:52 1991 By muller         *)

MODULE New;

IMPORT CG, CallExpr, Expr, ExprRep, Type, Procedure, Error;
IMPORT RefType, ObjectType, OpaqueType, KeywordExpr, Value;
IMPORT Field, Method, Int, ProcType, AssignStmt, OpenArrayType;
IMPORT Scope, RecordType, TypeExpr, Null, Revelation, Target;
IMPORT ArrayExpr, M3ID, M3RT, RunTyme, ErrType;
IMPORT MSIR, MSIRType, MSIRBuilder, PackedType;

VAR Z: CallExpr.MethodList;

PROCEDURE TypeOf (ce: CallExpr.T): Type.T =
  VAR t: Type.T;
  BEGIN
    IF NOT TypeExpr.Split (ce.args[0], t) THEN RETURN Null.T;
    ELSIF RefType.Is (t)    THEN (* ok *)
    ELSIF ObjectType.Is (t) THEN (* sleazy bug!!  ignore method overrides *)
    ELSIF OpaqueType.Is (t) THEN (* sleazy bug!!  ignore method overrides *)
    ELSE  RETURN Null.T;
    END;
    RETURN Type.StripPacked (t);
  END TypeOf;

PROCEDURE Check (ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR t, r: Type.T;
  BEGIN
    IF KeywordExpr.Is (ce.args[0]) THEN
      Error.Msg ("NEW: keyword bindings not allowed for type");
    END;
    IF NOT TypeExpr.Split (ce.args[0], t) THEN
      Error.Msg ("NEW must be applied to a reference type");
      t := Null.T;
    ELSIF (RefType.Split (t, r)) THEN
      CheckRef (r, ce, cs);
    ELSIF (ObjectType.Is (t)) THEN
      r := CheckObject (t, ce, cs);
      IF (r # t) THEN
        ce.args[0] := TypeExpr.New (r);
        Expr.TypeCheck (ce.args[0], cs);
        t := r;
      END;
    ELSIF (OpaqueType.Is (t)) THEN
      r := CheckOpaque (t, ce, cs);
      IF (r # t) THEN
        ce.args[0] := TypeExpr.New (r);
        Expr.TypeCheck (ce.args[0], cs);
        t := r;
      END;
    ELSIF (t # ErrType.T) THEN
      Error.Msg ("NEW must be applied to a reference type");
    END;
    ce.type := t;
  END Check;

PROCEDURE CheckRef (r: Type.T;  ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR
    base: Type.T;
    fields: Value.T;
    info : Type.Info;
  BEGIN
    IF (r = NIL) THEN
     Error.Msg("cannot NEW a variable of type REFANY, ADDRESS, or NULL");
     RETURN;
    END;
    r := Type.Check (r);
    base := Type.Base (r);
    base := Type.CheckInfo (base, info);
    IF (info.isEmpty) THEN
      Error.Msg ("cannot allocate variables of empty types");
    ELSIF (info.class = Type.Class.OpenArray) THEN
      CheckOpenArray (base, ce);
    ELSIF (info.class = Type.Class.Record) THEN
      CheckRecord (base, ce, cs);
    ELSIF RecordType.Split (base, fields) THEN
      CheckRecord (base, ce, cs);
    ELSIF (NUMBER (ce.args^) > 1) THEN
      Error.Msg ("too many arguments to NEW");
    END;
  END CheckRef;

PROCEDURE CheckOpenArray (r: Type.T;  ce: CallExpr.T) =
  VAR x, elt: Type.T;
  BEGIN
    FOR i := 1 TO LAST (ce.args^) DO
      x := Type.Base (Expr.TypeOf (ce.args[i]));
      IF KeywordExpr.Is (ce.args[i]) THEN
        Error.Msg ("NEW: not a procedure; keyword bindings not allowed for array dimensions");
      END;
      IF  NOT Type.IsEqual (x, Int.T, NIL) THEN
        Error.Int (i, "argument must be an integer");
      ELSIF (NOT OpenArrayType.Split (r, elt)) THEN
        Error.Int (i, "too many dimensions specified");
      ELSE (* ok *)
        r := elt;
      END;
    END;
    IF OpenArrayType.Is (r) THEN
      Error.Msg ("not enough dimensions specified");
    END;
  END CheckOpenArray;

PROCEDURE CheckRecord (t: Type.T;  ce: CallExpr.T;  VAR cs: Expr.CheckState) =
  VAR
    x: Type.T;
    key: M3ID.T;
    value: Expr.T;
    field: Value.T;
  BEGIN
    FOR i := 1 TO LAST (ce.args^) DO
      x := Expr.TypeOf (ce.args[i]);
      IF  NOT KeywordExpr.Split (ce.args[i], key, value) THEN
        Error.Msg ("extra arguments to NEW must include keywords (2.6.9)");
      ELSIF NOT RecordType.LookUp (t, key, field) THEN
        Error.ID (key, "unknown record field");
      ELSIF NOT Field.Is (field) THEN
        Error.ID (key, "undefined field?");
      ELSIF NOT Type.IsAssignable (Value.TypeOf (field), x) THEN
        Error.ID (key, "value is not assignable to field");
      ELSE
        ArrayExpr.NoteUseTargetVar (value);
        AssignStmt.Check (Value.TypeOf (field), value, cs);
      END;
    END;
  END CheckRecord;

PROCEDURE CheckObject (t: Type.T;  ce: CallExpr.T;  VAR cs: Expr.CheckState): Type.T =
  VAR
    x: Type.T;
    key: M3ID.T;
    value: Expr.T;
    v: Value.T;
    visible: Type.T;
    newType: Type.T := NIL;
    fields: Scope.T;
    overrides: Scope.T;
    n_overrides: INTEGER := 0;
    zz: Scope.T;
    method: Method.Info;
    field: Field.Info;
    info: Type.Info;
  BEGIN
    t := Type.CheckInfo (t, info);

    (* first pass, remove the method overrides & build a new object type *)
    FOR i := 1 TO LAST (ce.args^) DO
      x := Expr.TypeOf (ce.args[i]);
      IF KeywordExpr.Split (ce.args[i], key, value)
        AND ObjectType.LookUp (t, key, v, visible)
        AND Method.Split (v, method) THEN
        IF (newType = NIL) THEN
          fields := Scope.PushNew (FALSE, M3ID.NoID); Scope.PopNew ();
          overrides := Scope.PushNew (FALSE, M3ID.NoID); Scope.PopNew ();
          newType := ObjectType.New (t, info.isTraced, NIL, fields, overrides);
        END;
        zz := Scope.Push (overrides);
          method.name      := key;
          method.offset    := n_overrides * Target.Address.size;
          method.parent    := newType;
          method.signature := NIL;
          method.dfault    := value;
          method.override  := TRUE;
          Method.NoteOverride (Method.New (method), v);
          INC (n_overrides);
        Scope.Pop (zz);
      END;
    END;

    IF (newType # NIL) THEN
      newType := Type.Check (newType);
      t := newType;
    END;

    (* second pass, do the checking *)
    FOR i := 1 TO LAST (ce.args^) DO
      x := Expr.TypeOf (ce.args[i]);
      IF NOT KeywordExpr.Split (ce.args[i], key, value) THEN
        Error.Msg ("extra arguments must include keywords");
      ELSIF NOT ObjectType.LookUp (t, key, v, visible) THEN
        Error.ID (key, "unknown object field or method");
      ELSIF Method.Split (v, method) THEN
        IF NOT ProcType.IsCompatible (x, t, method.signature) THEN
          Error.ID (key, "procedure is not compatible with method");
        END;
        ce.args[i] := NIL;
      ELSIF Field.Is (v) THEN
        Field.Split (v, field);
        IF NOT Type.IsAssignable (field.type, x)
        THEN Error.ID (key, "value is not assignable to field");
        ELSE
          ArrayExpr.NoteUseTargetVar (value);
          AssignStmt.Check (field.type, value, cs);
        END;
      ELSE
        Error.ID (key, "undefined?");
      END;
    END;

    RETURN t;
  END CheckObject;

PROCEDURE CheckOpaque (t: Type.T; ce: CallExpr.T;  VAR cs: Expr.CheckState): Type.T =
  (* we already know that t is not an object, so we only need to
     check for a full revelation that says it's a REF *)
  VAR x := Revelation.LookUp (t);  r: Type.T;
  BEGIN
    IF (x = NIL) THEN
      Error.Msg ("cannot apply NEW to non-object, opaque types");
    ELSIF RefType.Split (x, r) THEN
      (* full revelation => t is a REF *)
      CheckRef (r, ce, cs);
    ELSE
      Error.Msg ("cannot apply NEW to this type");
    END;
    RETURN t;
  END CheckOpaque;

PROCEDURE Prep (ce: CallExpr.T) =
  BEGIN
    Gen (ce);
  END Prep;

PROCEDURE Compile (ce: CallExpr.T) =
  BEGIN
    (* all the work was done by Prep *)
    CG.Push (ce.tmp);
    CG.Boost_addr_alignment (ce.align);
    CG.Free (ce.tmp);
    ce.tmp := NIL;
  END Compile;

PROCEDURE Gen (ce: CallExpr.T) =
  VAR t, r: Type.T;
  BEGIN
    VAR b := TypeExpr.Split (ce.args[0], t); BEGIN <* ASSERT b *> END;
    Type.Compile (t);
    t := Type.StripPacked (t); 
    IF (RefType.Split (t, r)) THEN GenRef (t, Type.StripPacked (r), ce);
    ELSIF (ObjectType.Is (t)) THEN GenObject (t, ce);
    ELSIF (OpaqueType.Is (t)) THEN GenOpaque (t, ce);
    ELSE Error.Msg ("NEW must name a reference type");
    END;
  END Gen;

PROCEDURE GenRef (t, r: Type.T;  ce: CallExpr.T) =
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.NewUntracedRef,
                                                RunTyme.Hook.NewTracedRef };
  VAR
    base   := Type.Base (r);
    fields : Value.T;
    proc   : Procedure.T;
    t_info : Type.Info;
    r_info : Type.Info;
  BEGIN
    t := Type.CheckInfo (t, t_info);
    r := Type.Check (r);
    r := Type.StripPacked (r);
    r := Type.CheckInfo (r, r_info);

    IF (r_info.class = Type.Class.OpenArray) THEN
      GenOpenArray (t, t_info.isTraced, r_info, ce);

    ELSIF RecordType.Split (base, fields) THEN
      GenRecord (t, base, t_info.isTraced, r_info, ce);

    ELSE
      proc := RunTyme.LookUpProc (PHook [t_info.isTraced]);
      Procedure.StartCall (proc);
      Type.LoadInfo (t, -1);
      CG.Pop_param (CG.Type.Addr);
      ce.tmp := Procedure.EmitValueCall (proc);
      ce.align := r_info.alignment;
    END;
  END GenRef;

PROCEDURE GenOpenArray (t: Type.T;  traced: BOOLEAN;
                        READONLY r_info: Type.Info;  ce: CallExpr.T) =
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.NewUntracedArray,
                                                RunTyme.Hook.NewTracedArray };
  VAR
    n := LAST (ce.args^); (* number of open dimensions *)
    sizesAlign := MAX(Target.Address.align, Target.Integer.align);
    sizes := CG.Declare_temp (Target.Address.pack + (n+1)*Target.Integer.pack,
(*TODO:                       ^RefType.InitTypecell and possibly other places
                               handle possible alignment padding between the
                               elements-address and the shape, in case Integer
                               has higher alignment than Address. *)
                              sizesAlign, CG.Type.Struct, in_memory := TRUE);
    offset: INTEGER;
    proc := RunTyme.LookUpProc (PHook [traced]);
  BEGIN
    (* initialize the pointer to the array sizes *)
    CG.Load_addr_of (sizes, M3RT.OA_size_1, Target.Integer.align);
    CG.Store_addr (sizes, M3RT.OA_elt_ptr);

    (* initialize the count of array sizes *)
    CG.Load_intt (n);
    CG.Store_int (Target.Integer.cg_type, sizes, M3RT.OA_size_0);

    (* initialize each array size *)
    offset := M3RT.OA_size_1;
    FOR i := 1 TO n DO
      Expr.Prep (ce.args[i]);
      Expr.Compile (ce.args[i]);
      CG.Store_int (Target.Integer.cg_type, sizes, offset);
      INC (offset, Target.Integer.pack);
    END;

    (* allocate the storage *)
    Procedure.StartCall (proc);
    IF Target.DefaultCall.args_left_to_right THEN
      Type.LoadInfo (t, -1);
      CG.Pop_param (CG.Type.Addr);
      CG.Load_addr_of (sizes, 0, sizesAlign);
      CG.Pop_param (CG.Type.Addr);
    ELSE
      CG.Load_addr_of (sizes, 0, sizesAlign);
      CG.Pop_param (CG.Type.Addr);
      Type.LoadInfo (t, -1);
      CG.Pop_param (CG.Type.Addr);
    END;
    ce.tmp := Procedure.EmitValueCall (proc);
    ce.align := r_info.alignment;

    CG.Free_temp (sizes);
  END GenOpenArray;

PROCEDURE GenRecord (t, r: Type.T;  traced: BOOLEAN;
                     READONLY r_info: Type.Info;  ce: CallExpr.T) =
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.NewUntracedRef,
                                                RunTyme.Hook.NewTracedRef };
  VAR
    key: M3ID.T;
    value: Expr.T;
    field: Field.Info;
    v: Value.T;
    align := r_info.alignment;
    proc := RunTyme.LookUpProc (PHook [traced]);
    b: BOOLEAN;
  BEGIN
    (* allocate the record's storage *)
    Procedure.StartCall (proc);
    Type.LoadInfo (t, -1);
    CG.Pop_param (CG.Type.Addr);
    ce.tmp := Procedure.EmitValueCall (proc);
    ce.align := align;

    (* do the user specified initialization *)
    FOR i := 1 TO LAST (ce.args^) DO
      b := KeywordExpr.Split (ce.args[i], key, value); <*ASSERT b*>
      AssignStmt.PrepForEmit (field.type, value, initializing := TRUE);
      EVAL RecordType.LookUp (r, key, v);
      Field.Split (v, field);
      CG.Push (ce.tmp);
      CG.Boost_addr_alignment (align);
      CG.Add_offset (field.offset);
      AssignStmt.DoEmit (field.type, value, initializing := TRUE);
    END;
  END GenRecord;

PROCEDURE GenObject (t: Type.T;  ce: CallExpr.T) =
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.NewUntracedObj,
                                                RunTyme.Hook.NewTracedObj };
  VAR
    key: M3ID.T;
    value: Expr.T;
    field: Field.Info;
    v: Value.T;
    visible: Type.T;
    obj_offset: INTEGER;
    obj_align: INTEGER;
    proc: Procedure.T;
    info: Type.Info;
    b: BOOLEAN;
  BEGIN
    t := Type.CheckInfo (t, info);
    proc := RunTyme.LookUpProc (PHook [info.isTraced]);

    (* allocate the object's storage *)
    Procedure.StartCall (proc);
    Type.LoadInfo (t, -1);
    CG.Pop_param (CG.Type.Addr);
    ce.tmp := Procedure.EmitValueCall (proc);
    ce.align := info.alignment;

    (* do the user specified initialization *)
    FOR i := 1 TO LAST (ce.args^) DO
      IF (ce.args[i] # NIL) THEN
        b := KeywordExpr.Split (ce.args[i], key, value); <*ASSERT b*>
        b := ObjectType.LookUp (t, key, v, visible); <*ASSERT b*>
        Field.Split (v, field);
        AssignStmt.PrepForEmit (field.type, value, initializing := TRUE);
        CG.Push (ce.tmp);
        ObjectType.GetFieldsOffsetAndAlign (visible, obj_offset, obj_align);
        IF (obj_offset >= 0) THEN
          INC (field.offset, obj_offset);
        ELSE
          Type.LoadInfo (visible, M3RT.OTC_dataOffset);
          CG.Index_bytes (Target.Byte);
        END;
        CG.Add_offset (field.offset);
        CG.Boost_addr_alignment (obj_align);
        AssignStmt.DoEmit (field.type, value, initializing := TRUE);
      END;
    END;
  END GenObject;

PROCEDURE GenOpaque (t: Type.T;  ce: CallExpr.T) =
  VAR x := Revelation.LookUp (t);  r: Type.T;
  BEGIN
    IF (x = NIL) THEN
      <* ASSERT FALSE *>
    ELSIF RefType.Split (x, r) THEN
      (* full revelation => t is a REF *)
      GenRef (x, Type.StripPacked (r), ce);
    ELSE
      <* ASSERT FALSE *>
    END;
  END GenOpaque;

PROCEDURE CompileMSIR (ce: CallExpr.T): MSIR.Value =
  VAR t, r: Type.T;
  BEGIN
    IF NOT MSIRBuilder.InProc () THEN RETURN NIL END;
    IF NOT TypeExpr.Split (ce.args[0], t) THEN
      MSIRBuilder.Abandon ("NEW: cannot determine type");  RETURN NIL;
    END;
    t := Type.StripPacked (t);
    IF    RefType.Split    (t, r) THEN
      (* RefType.Split succeeds for a locally-revealed opaque REF, reducing
         through the revelation to find the referent, but it leaves `t` as the
         opaque type.  Passing the opaque type to GenRefMSIR reaches
         TypeDescValueForRef with the opaque UID, which deliberately returns NIL
         for any UID that is a revelation LHS in this module (RTLinker maps the
         opaque uid to the revealed TypeCell).  That would silently drop the
         allocation (empty NewSet => `unreachable` => SIGTRAP at startup).
         Use the concrete (reduced) REF type so the allocator hook gets a real
         TypeDesc keyed by the concrete RHS uid.  For a non-opaque ref this is
         `t` itself, so there is no change for the common case. *)
      RETURN GenRefMSIR (RefType.ReduceToRef (t), Type.StripPacked (r), ce);
    ELSIF ObjectType.Is    (t)    THEN RETURN GenObjectMSIR (t, ce);
    ELSIF OpaqueType.Is    (t)    THEN RETURN GenOpaqueMSIR (t, ce);
    ELSE  MSIRBuilder.Abandon ("NEW: not a reference type");  RETURN NIL;
    END;
  END CompileMSIR;

(* Common tail: call the allocator hook with descV, convert result to type t. *)
PROCEDURE CallAllocHook (t: Type.T;  hook: RunTyme.Hook;
                          descV: MSIR.Value): MSIR.Value =
  VAR proc: MSIR.Proc;  res: MSIR.Value;  mt: MSIR.T;
  BEGIN
    IF descV = NIL THEN RETURN NIL END;
    proc := MSIRBuilder.HookProc (hook);
    IF proc = NIL THEN
      MSIRBuilder.Abandon ("NEW: allocator hook not available");  RETURN NIL;
    END;
    res := MSIRBuilder.EmitCall ("", proc, ARRAY OF MSIR.Value{descV});
    IF res = NIL THEN RETURN NIL END;
    mt := MSIRType.Translate (t);
    IF mt = NIL THEN mt := MSIR.TGcRef (MSIR.TVoid ()) END;
    RETURN MSIR.BuildConvert (MSIRBuilder.CurrentBlock (), "", res, mt);
  END CallAllocHook;

PROCEDURE NarrowFieldStoreMSIR (b: MSIR.Block;  valV: MSIR.Value;
                                fieldAddr: MSIR.Value;  fieldType: Type.T) =
(* Store a keyword-initializer value into a NEW'd record/object field.
   valV carries the ZType computation width (i64 for BOOLEAN/enum/subrange),
   but fieldAddr is an untyped byte pointer, so a plain store writes valV's
   FULL width — an 8-byte store into a 1-byte BOOLEAN field clobbers the
   following 7 bytes (QCode.ProcInfo: `builtin := TRUE` zeroed the adjacent
   isFunc byte, breaking every quake function call in a self-hosted cm3).
   Truncate integer values to the field's declared storage width first. *)
  VAR fti: Type.Info;  vw: INTEGER;
  BEGIN
    EVAL Type.CheckInfo (fieldType, fti);
    vw := MSIR.BitWidth (MSIR.ValueType (valV));
    IF fti.size > 0 AND vw > 0 AND fti.size < vw THEN
      valV := MSIR.BuildTrunc (b, "", valV, MSIR.TI (fti.size));
    END;
    MSIR.BuildStore (b, valV, fieldAddr);
  END NarrowFieldStoreMSIR;

PROCEDURE GenRefMSIR (t, r: Type.T;  ce: CallExpr.T): MSIR.Value =
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.NewUntracedRef,
                                                RunTyme.Hook.NewTracedRef };
  VAR t_info, r_info: Type.Info;
  BEGIN
    t := Type.CheckInfo (t, t_info);
    r := Type.CheckInfo (r, r_info);
    CASE r_info.class OF
    | Type.Class.OpenArray =>
        RETURN GenOpenArrayMSIR (t, t_info, r, ce);
    | Type.Class.Record =>
        VAR
          refVal   : MSIR.Value;
          key      : M3ID.T;
          value    : Expr.T;
          v        : Value.T;
          fieldInfo: Field.Info;
          fieldMsirT: MSIR.T;
          valV     : MSIR.Value;
          fieldAddr: MSIR.Value;
          byteOff  : INTEGER;
          b        : MSIR.Block;
        BEGIN
          refVal := CallAllocHook (t, PHook [t_info.isTraced],
                                   MSIRBuilder.TypeDescValueForRef (t,
                                     r_info.size DIV Target.Byte,
                                     r_info.alignment DIV Target.Byte,
                                     t_info.isTraced));
          IF refVal = NIL THEN RETURN NIL END;
          (* Apply the record type's field defaults inline (the runtime's initProc
             mechanism; in MSIR we emit it at the call site instead of via TC_initProc
             because generating the initProc suffers from a pre-existing crash in
             EndProc during interface compilation).  GenInitMSIR only stores fields
             that have non-zero defaults; the heap allocation is already zero-init.
             This runs BEFORE keyword args so that explicit keyword overrides win. *)
          RecordType.GenInitMSIR (r, refVal);
          (* Keyword-arg field initialization (initializing := TRUE, no GC barrier). *)
          FOR i := 1 TO LAST (ce.args^) DO
            EVAL KeywordExpr.Split (ce.args[i], key, value);
            EVAL RecordType.LookUp (r, key, v);
            Field.Split (v, fieldInfo);
            byteOff   := fieldInfo.offset DIV 8;
            fieldMsirT := MSIRType.Translate (fieldInfo.type);
            IF fieldMsirT = NIL THEN
              MSIRBuilder.Abandon ("NEW(REF record): unsupported field type");
              RETURN NIL;
            END;
            valV := Expr.CompileMSIR (value);
            IF valV = NIL THEN RETURN NIL END;
            b := MSIRBuilder.CurrentBlock ();
            VAR packedBase : Type.T;  packedSize : INTEGER;
                isPacked := PackedType.Is (fieldInfo.type);
            BEGIN
              IF isPacked THEN
                (* BITS-N field: must use InsertBitField to place the value at
                   the correct bit position within the record storage (both for
                   sub-byte-aligned fields and for narrower-than-byte fields).
                   A direct store would ignore bitInByte and overwrite adjacent
                   bits with the high bytes of the integer value (p178). *)
                PackedType.Split (fieldInfo.type, packedSize, packedBase);
                fieldAddr := MSIRBuilder.BuildPtrByteOff (b, "", refVal, 0);
                MSIRBuilder.InsertBitField (fieldAddr, fieldInfo.offset,
                                            packedSize, valV);
              ELSE
                fieldAddr := MSIRBuilder.BuildPtrByteOff (b, "", refVal, byteOff);
                NarrowFieldStoreMSIR (b, valV, fieldAddr, fieldInfo.type);
              END;
            END;
          END;
          RETURN refVal;
        END;
    ELSE
        RETURN CallAllocHook (t, PHook [t_info.isTraced],
                              MSIRBuilder.TypeDescValueForRef (t,
                                r_info.size DIV Target.Byte,
                                r_info.alignment DIV Target.Byte,
                                t_info.isTraced));
    END;
  END GenRefMSIR;

PROCEDURE GenOpenArrayMSIR (t: Type.T;  READONLY t_info: Type.Info;
                             r: Type.T;  ce: CallExpr.T): MSIR.Value =
  (* NEW(REF ARRAY OF T, dim0 [, dim1...]) — calls AllocateOpenArray.
     Use TypeLinkValueForRefArray so the TypeCell is resolved by RTLinker
     rather than defined inline at the call site. *)
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.NewUntracedArray,
                                                RunTyme.Hook.NewTracedArray };
  VAR
    ta       := Type.Base (r);
    ndims    : INTEGER;
    b        : MSIR.Block;
    ptrT     := MSIR.TPtr (MSIR.TVoid ());
    intT     := MSIR.TI (Target.Integer.size);
    flds     : REF ARRAY OF MSIR.Field;
    sizesT   : MSIR.T;
    sizesA   : MSIR.Value;
    proc     : MSIR.Proc;
    res      : MSIR.Value;
    mt       : MSIR.T;
    descV    : MSIR.Value;
    apBytes  : INTEGER;
    ipBytes  : INTEGER;
  BEGIN
    ndims := OpenArrayType.OpenDepth (ta);
    IF ndims < 1 THEN
      MSIRBuilder.Abandon ("NEW(REF open-array): zero-depth open array");
      RETURN NIL;
    END;
    descV := MSIRBuilder.TypeLinkValueForRefArray (t);
    IF descV = NIL THEN RETURN NIL END;
    proc := MSIRBuilder.HookProc (PHook [t_info.isTraced]);
    IF proc = NIL THEN
      MSIRBuilder.Abandon ("NEW(REF open-array): hook not available");  RETURN NIL;
    END;

    (* Build the sizes struct: { ptr elt_ptr, i64 count, i64 dim0, ... } *)
    flds := NEW (REF ARRAY OF MSIR.Field, ndims + 2);
    flds[0] := MSIR.Field{name := "", type := ptrT};
    flds[1] := MSIR.Field{name := "", type := intT};
    FOR k := 0 TO ndims - 1 DO
      flds[2 + k] := MSIR.Field{name := "", type := intT};
    END;
    sizesT := MSIR.TStruct ("__oa_shape", flds^);
    b := MSIRBuilder.CurrentBlock ();
    sizesA := MSIR.BuildAlloca (b, "", sizesT);

    apBytes := Target.Address.size DIV Target.Byte;
    ipBytes := Target.Integer.size DIV Target.Byte;

    (* OA_elt_ptr (byte 0) = &sizes.dim0 = sizesA + (AP + IP) bytes *)
    VAR dim0Addr := MSIRBuilder.BuildPtrByteOff (b, "", sizesA, apBytes + ipBytes);
    BEGIN
      MSIR.BuildStore (b, dim0Addr, MSIRBuilder.BuildPtrByteOff (b, "", sizesA, 0));
    END;

    (* OA_size_0 (byte AP) = number of open dimensions *)
    VAR cntAddr := MSIRBuilder.BuildPtrByteOff (b, "", sizesA, apBytes);
    BEGIN
      MSIR.BuildStore (b, MSIR.ConstInt (intT, ndims), cntAddr);
    END;

    (* OA_size_i (byte AP + IP*i, i in 1..ndims) = ce.args[i] dimension expression *)
    FOR i := 1 TO ndims DO
      VAR
        dimAddr := MSIRBuilder.BuildPtrByteOff (b, "", sizesA, apBytes + ipBytes * i);
        dimVal  : MSIR.Value;
      BEGIN
        dimVal := Expr.CompileMSIR (ce.args[i]);
        b := MSIRBuilder.CurrentBlock ();  (* re-fetch after potential invoke *)
        IF dimVal = NIL THEN RETURN NIL END;
        dimVal := MSIR.BuildConvert (b, "", dimVal, intT);
        MSIR.BuildStore (b, dimVal, dimAddr);
      END;
    END;

    res := MSIRBuilder.EmitCall ("", proc, ARRAY OF MSIR.Value{descV, sizesA});
    IF res = NIL THEN RETURN NIL END;
    mt := MSIRType.Translate (t);
    IF mt = NIL THEN mt := MSIR.TGcRef (MSIR.TVoid ()) END;
    RETURN MSIR.BuildConvert (MSIRBuilder.CurrentBlock (), "", res, mt);
  END GenOpenArrayMSIR;

PROCEDURE GenObjectMSIR (t: Type.T;  ce: CallExpr.T): MSIR.Value =
  CONST PHook = ARRAY BOOLEAN OF RunTyme.Hook { RunTyme.Hook.NewUntracedObj,
                                                RunTyme.Hook.NewTracedObj };
  VAR t_info    : Type.Info;
      key       : M3ID.T;
      value     : Expr.T;
      v         : Value.T;
      visible   : Type.T;
      fieldInfo : Field.Info;
      obj_offset, obj_align: INTEGER;
      objVal    : MSIR.Value;
      valV      : MSIR.Value;
      b         : MSIR.Block;
      fieldAddr : MSIR.Value;
      fieldBitOff : INTEGER;
      packedBase : Type.T;  packedSize : INTEGER;
  BEGIN
    t := Type.CheckInfo (t, t_info);
    objVal := CallAllocHook (t, PHook [t_info.isTraced],
                             MSIRBuilder.TypeLinkValueForObject (t));
    IF objVal = NIL THEN RETURN NIL END;
    FOR i := 1 TO LAST (ce.args^) DO
      IF ce.args[i] # NIL THEN
        IF KeywordExpr.Split (ce.args[i], key, value)
           AND ObjectType.LookUp (t, key, v, visible) THEN
          Field.Split (v, fieldInfo);
          ObjectType.GetFieldsOffsetAndAlign (visible, obj_offset, obj_align);
          IF obj_offset >= 0 THEN
            fieldBitOff := fieldInfo.offset + obj_offset;
          ELSE
            (* Runtime object offset — abandon and fall back to zero offset
               (conservative; will be wrong for dynamic inheritance). *)
            fieldBitOff := fieldInfo.offset;
          END;
          valV := Expr.CompileMSIR (value);
          IF valV = NIL THEN RETURN NIL END;
          b := MSIRBuilder.CurrentBlock ();
          IF PackedType.Is (fieldInfo.type) THEN
            PackedType.Split (fieldInfo.type, packedSize, packedBase);
            fieldAddr := MSIRBuilder.BuildPtrByteOff (b, "", objVal, 0);
            MSIRBuilder.InsertBitField (fieldAddr, fieldBitOff, packedSize, valV);
          ELSE
            fieldAddr := MSIRBuilder.BuildPtrByteOff (b, "", objVal, fieldBitOff DIV 8);
            NarrowFieldStoreMSIR (b, valV, fieldAddr, fieldInfo.type);
          END;
        END;
      END;
    END;
    RETURN objVal;
  END GenObjectMSIR;

PROCEDURE GenOpaqueMSIR (t: Type.T;  ce: CallExpr.T): MSIR.Value =
  VAR x := Revelation.LookUp (t);  r: Type.T;
  BEGIN
    IF x = NIL THEN
      MSIRBuilder.Abandon ("NEW(OPAQUE): no full revelation visible");
      RETURN NIL;
    END;
    IF RefType.Split (x, r) THEN
      RETURN GenRefMSIR (x, Type.StripPacked (r), ce);
    END;
    IF ObjectType.Is (x) THEN
      RETURN GenObjectMSIR (x, ce);
    END;
    MSIRBuilder.Abandon ("NEW(OPAQUE): unsupported revelation type in MSIR");
    RETURN NIL;
  END GenOpaqueMSIR;

PROCEDURE Initialize () =
  BEGIN
    Z := CallExpr.NewMethodList (1, LAST (INTEGER), TRUE, TRUE, TRUE, NIL,
                                 TypeOf,
                                 TypeOf,
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
    Procedure.DefinePredefined ("NEW", Z, TRUE);
    CallExpr.SetMethodMSIR (Z, CompileMSIR);
  END Initialize;

BEGIN
END New.
