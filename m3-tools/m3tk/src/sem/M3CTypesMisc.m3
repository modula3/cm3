MODULE M3CTypesMisc;

(***************************************************************************)
(*                      Copyright (C) Olivetti 1989                        *)
(*                          All Rights reserved                            *)
(*                                                                         *)
(* Use and copy of this software and preparation of derivative works based *)
(* upon this software are permitted to any person, provided this same      *)
(* copyright notice and the following Olivetti warranty disclaimer are     *) 
(* included in any copy of the software or any modification thereof or     *)
(* derivative work therefrom made by any person.                           *)
(*                                                                         *)
(* This software is made available AS IS and Olivetti disclaims all        *)
(* warranties with respect to this software, whether expressed or implied  *)
(* under any law, including all implied warranties of merchantibility and  *)
(* fitness for any purpose. In no event shall Olivetti be liable for any   *)
(* damages whatsoever resulting from loss of use, data or profits or       *)
(* otherwise arising out of or in connection with the use or performance   *)
(* of this software.                                                       *)
(***************************************************************************)


IMPORT M3AST_LX, M3AST_AS, M3AST_SM, M3ASTNext;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_M3TYPE, SeqM3AST_AS_Fields, SeqM3AST_AS_Enum_id, 
    SeqM3AST_AS_Override;

IMPORT M3CBackEnd, M3COrdinal, M3CConcTypeSpec;

PROCEDURE GetTYPE_SPECFromM3TYPE(
    t: M3AST_AS.M3TYPE;
    VAR (*out*) ts: M3AST_SM.TYPE_SPEC_UNSET) 
    RAISES {}=
  BEGIN
    TYPECASE t OF <*NOWARN*>
    | M3AST_AS.Named_type(namedType) =>
        ts := namedType.sm_type_spec;
    | M3AST_AS.TYPE_SPEC(typeSpec) =>
        ts := typeSpec;
    | M3AST_AS.Bad_M3TYPE =>
        ts := NIL;
    END;
  END GetTYPE_SPECFromM3TYPE;


PROCEDURE Unpack(
    p: M3AST_AS.Packed_type)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  VAR
    unpacked: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    LOOP
      GetTYPE_SPECFromM3TYPE(p.as_type, unpacked);
      TYPECASE unpacked OF
      | NULL =>
          RETURN NIL;
      | M3AST_AS.Packed_type(packed) =>
          p := packed; (* and loop *)
      ELSE
        RETURN unpacked;
      END;
    END;
  END Unpack;


PROCEDURE CheckedUnpack(
    t: M3AST_SM.TYPE_SPEC_UNSET)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  BEGIN
    TYPECASE t OF
    | NULL => RETURN NIL;
    | M3AST_AS.Packed_type(p) => RETURN Unpack(p);
    ELSE
      RETURN t;
    END;
  END CheckedUnpack;


PROCEDURE Reveal(
    ts: M3AST_SM.TYPE_SPEC_UNSET)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  BEGIN
    LOOP
      IF ts = NIL THEN RETURN ts
      ELSIF ISTYPE(ts, M3AST_AS.Opaque_type) THEN
        ts := M3CConcTypeSpec.CurrentReveal(ts);
      ELSE
        RETURN ts;
      END;
    END;
  END Reveal;


PROCEDURE Concrete(
    ts: M3AST_SM.TYPE_SPEC_UNSET)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  VAR
    reveal := M3CConcTypeSpec.CurrentReveal(ts);
  BEGIN
    IF (reveal # ts) AND
        (NARROW(ts, M3AST_AS.Opaque_type).sm_concrete_type_spec = reveal) THEN
      RETURN reveal;
    ELSE
      RETURN ts;
    END;
  END Concrete;


PROCEDURE IsConcrete(
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    fully: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  VAR
    typeSpec: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    TYPECASE ts OF
    | NULL =>
        RETURN TRUE;
    | M3AST_AS.Object_type(object) =>
        IF fully AND M3ASTNext.SimpleSuperType(object, typeSpec) THEN
          RETURN IsConcrete(typeSpec, TRUE);
        ELSE
          RETURN TRUE;
        END;
    | M3AST_AS.Opaque_type(opaque) =>
        typeSpec := M3CConcTypeSpec.CurrentReveal(ts);
        IF (typeSpec # ts) AND
            (opaque.sm_concrete_type_spec = typeSpec) THEN
          RETURN (NOT fully) OR IsConcrete(typeSpec, TRUE);
        ELSE
          RETURN FALSE;
        END;
    ELSE
      RETURN TRUE;
    END;
  END IsConcrete;


PROCEDURE Index(
    a: M3AST_AS.Array_type;
    VAR index: M3AST_SM.TYPE_SPEC_UNSET)
    : Ix
    RAISES {}=
  VAR
    i := SeqM3AST_AS_M3TYPE.NewIter(a.as_indextype_s);
    m3Type: M3AST_AS.M3TYPE;
  BEGIN
    IF SeqM3AST_AS_M3TYPE.Next(i, m3Type) THEN
      GetTYPE_SPECFromM3TYPE(m3Type, index);
      LOOP
        TYPECASE index OF
        | NULL =>
            RETURN Ix.Unknown;
        | M3AST_AS.Integer_type,
          M3AST_AS.Enumeration_type,
          M3AST_AS.Subrange_type =>
            RETURN Ix.Ordinal;
        | M3AST_AS.Packed_type(packed) =>
            index := Unpack(packed);
            (* loop and try again *)
        ELSE
          RETURN Ix.Bad;
        END; (* case *)
      END;
    ELSE
      RETURN Ix.Open;
    END; (* if *)
  END Index;


PROCEDURE Indexable(
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    VAR a: M3AST_AS.Array_type)
    : BOOLEAN
    RAISES {}=
  BEGIN
    TYPECASE Concrete(CheckedUnpack(ts)) OF
    | M3AST_AS.Array_type(arrayType) =>
        a := arrayType;
        RETURN TRUE; (* NIL case ok *)
    | M3AST_AS.Ref_type(refType) =>
        VAR
          referent: M3AST_SM.TYPE_SPEC_UNSET;
        BEGIN
          GetTYPE_SPECFromM3TYPE(refType.as_type, referent);
          TYPECASE CheckedUnpack(referent) OF
          | M3AST_AS.Array_type(arrayType) =>
              a := arrayType; (* NIL case ok *)
              RETURN TRUE;
          ELSE
          END;
        END;
    ELSE
    END;
    RETURN FALSE;
  END Indexable;


PROCEDURE IsRef(ts: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  BEGIN
    IF ts = NIL THEN RETURN TRUE END;
    TYPECASE ts OF
    | M3AST_AS.RefAny_type, M3AST_AS.Root_type,
      M3AST_AS.Address_type,
      M3AST_AS.Ref_type, M3AST_AS.Object_type,
      M3AST_AS.Opaque_type, M3AST_AS.Null_type =>
        RETURN TRUE;
    ELSE
      RETURN FALSE;
    END; (* case *)
  END IsRef;


CONST
  BadSuperType = RefSet{Ref.Null, Ref.Not};


PROCEDURE IsTracedObject(o: M3AST_AS.Object_type): Ref RAISES {}=
  VAR
    super: M3AST_SM.TYPE_SPEC_UNSET;
    traced: Ref;
  BEGIN
    IF M3ASTNext.SimpleSuperType(o, super) THEN
      traced := IsTracedRef(super);
      IF traced IN BadSuperType THEN RETURN Ref.Unknown ELSE RETURN traced END;
    ELSE
      RETURN Ref.Traced;
    END;
  END IsTracedObject;


PROCEDURE IsTracedRef(type: M3AST_SM.TYPE_SPEC_UNSET): Ref RAISES {}=
  BEGIN
    IF type = NIL THEN RETURN Ref.Unknown END;
    TYPECASE type OF
    | M3AST_AS.RefAny_type =>
        RETURN Ref.Traced;
    | M3AST_AS.Address_type =>
        RETURN Ref.Untraced;
    | M3AST_AS.Root_type(root_type) =>
        IF root_type.as_trace_mode = NIL THEN
          RETURN Ref.Traced;
        ELSE
          RETURN Ref.Untraced;
        END; (* if *)
    | M3AST_AS.Ref_type(ref_type) =>
        IF ref_type.as_trace_mode = NIL THEN
          RETURN Ref.Traced;
        ELSE
          RETURN Ref.Untraced;
        END; (* if *)
    | M3AST_AS.Object_type(object_type) =>
        RETURN IsTracedObject(object_type);
    | M3AST_AS.Opaque_type =>
        WITH ref = IsTracedRef(Reveal(type)) DO
          IF ref IN BadSuperType THEN RETURN Ref.Unknown ELSE RETURN ref END;
        END;
    | M3AST_AS.Null_type =>
        RETURN Ref.Null;
    ELSE
      RETURN Ref.Not;
    END; (* case *)
  END IsTracedRef;


PROCEDURE ContainsTracedFields(fields: SeqM3AST_AS_Fields.T): BOOLEAN RAISES {}=
  VAR
    iter := M3ASTNext.NewIterField(fields);
    fieldId: M3AST_AS.Field_id;
  BEGIN
    WHILE M3ASTNext.Field(iter, fieldId) DO
      IF IsTraced(fieldId.sm_type_spec) THEN RETURN TRUE END;
    END; (* while *)
    RETURN FALSE;
  END ContainsTracedFields;


PROCEDURE IsTraced(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  BEGIN
    TYPECASE type OF
    | NULL =>
        RETURN FALSE;
    | M3AST_AS.RefAny_type =>
        RETURN TRUE;
    | M3AST_AS.Root_type(root) =>
        RETURN root.as_trace_mode = NIL;
    | M3AST_AS.Ref_type(ref) =>
        RETURN ref.as_trace_mode = NIL;
    | M3AST_AS.Object_type(object) =>
        RETURN IsTracedObject(object) IN ProbablyTraced;
    | M3AST_AS.Record_type(record) =>
        RETURN ContainsTracedFields(record.as_fields_s);
    | M3AST_AS.Array_type(array) =>
        VAR
          elementType: M3AST_SM.TYPE_SPEC_UNSET;
        BEGIN
          GetTYPE_SPECFromM3TYPE(array.as_elementtype, elementType);
          RETURN IsTraced(elementType);
        END;
    | M3AST_AS.Packed_type(packed) =>
        VAR
          component: M3AST_SM.TYPE_SPEC_UNSET;
        BEGIN
          GetTYPE_SPECFromM3TYPE(packed.as_type, component);
          RETURN IsTraced(component);
        END;
    ELSE
      RETURN FALSE;
    END; (* case *)
  END IsTraced;


PROCEDURE IsOpenArray(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  BEGIN
    TYPECASE type OF
    | NULL => RETURN FALSE;
    | M3AST_AS.Array_type(array) => RETURN SeqM3AST_AS_M3TYPE.Empty(array.as_indextype_s);
    ELSE
      RETURN FALSE;
    END;
  END IsOpenArray;


PROCEDURE IsEmpty(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  BEGIN
    TYPECASE type OF
    | NULL =>
        RETURN FALSE;
    | M3AST_AS.Enumeration_type(enum) =>
        RETURN SeqM3AST_AS_Enum_id.Empty(enum.as_id_s);
    | M3AST_AS.Subrange_type =>
        VAR
          first, last: M3AST_SM.Exp_value;
        BEGIN
          RETURN M3COrdinal.ValidBounds(type, first, last) AND
              M3CBackEnd.Compare(first, last) > 0;
        END;
    | M3AST_AS.Array_type(array) =>
        (* empty if element type is empty and primary index type is not empty;
         If both element type and primary index type are empty the null
         constructor is a valid value of the type *)
        VAR
          index, element: M3AST_SM.TYPE_SPEC_UNSET;
        BEGIN
          array := array.sm_norm_type;
          GetTYPE_SPECFromM3TYPE(array.as_elementtype, element);
          RETURN IsEmpty(element) AND Index(array, index) = Ix.Ordinal AND
              NOT IsEmpty(index);
        END;
    | M3AST_AS.Packed_type(packed) =>
        RETURN IsEmpty(Unpack(packed));
    ELSE
      RETURN FALSE;
    END;
  END IsEmpty;


PROCEDURE NoDefaultForMethod(
    o: M3AST_AS.Object_type;
    methodId: M3AST_AS.Method_id)
    : BOOLEAN
    RAISES {}=
  VAR
    id: M3AST_LX.Symbol_rep;
    iter: SeqM3AST_AS_Override.Iter;
    override: M3AST_AS.Override;
    ts, concrete: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF methodId.vINIT_ID.sm_init_exp # NIL THEN RETURN FALSE END;
    (* Assert: 'methodId' must be the defining occurence of the method. Only
     the defining occurence has the option of having no default. *)
    id := methodId.lx_symrep;
    IF id = NIL THEN RETURN FALSE END;
    LOOP
      (* Search overrides at current level of object type *)
      iter := SeqM3AST_AS_Override.NewIter(o.as_override_s);
      WHILE SeqM3AST_AS_Override.Next(iter, override) DO
        WITH mId = override.as_id DO
          IF mId.lx_symrep = id AND mId.vREDEF_ID.sm_int_def = methodId THEN
              RETURN FALSE; (* default found *)
          END;
        END;
      END;
      (* If no luck at the current level we look at the supertype *)
      IF NOT M3ASTNext.SimpleSuperType(o, ts) THEN RETURN TRUE END;
      LOOP
        TYPECASE ts OF
        | NULL =>
            RETURN FALSE;
        | M3AST_AS.Object_type(objectType) =>
            o := objectType;
            EXIT; (* to outer loop; search for default in supertype *)
        | M3AST_AS.Opaque_type =>
            concrete := Concrete(ts);
            IF concrete = ts THEN
              (* We don't know the concrete representation; so the default may
               be hidden in the opaque supertype - we return FALSE *)
              RETURN FALSE;
            ELSE
              ts := concrete;
              (* Loop and check out the concrete supertype *)
            END;
        ELSE
          RETURN FALSE; (* A cockup elsewhere; we pretend there's no problem *)
        END;
      END;
    END;
  END NoDefaultForMethod;


PROCEDURE HiddenObjectParameter(
    p: M3AST_AS.Procedure_type;
    VAR ts: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    TYPECASE p.sm_def_id OF
    | NULL => RETURN FALSE;
    | M3AST_AS.METHOD_OVERRIDE_ID(methodId) =>
        ts := methodId.vRECOBJ_ID.sm_enc_type_spec;
        RETURN TRUE;
    | M3AST_AS.Type_id(typeId) =>
        ts := typeId.sm_type_spec;
        RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END HiddenObjectParameter;


BEGIN
END M3CTypesMisc.
