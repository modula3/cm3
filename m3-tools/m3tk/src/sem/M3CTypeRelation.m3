MODULE M3CTypeRelation;

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


IMPORT M3AST_AS,  M3AST_SM, M3ASTNext;

IMPORT M3AST_AS_F, M3AST_SM_F;


IMPORT M3CTypesMisc, M3CConcTypeSpec, M3CRaisesSet, M3CTypeCompare, M3COrdinal;
IMPORT M3CExpValue, M3CBackEnd;


<*INLINE*> PROCEDURE Identical(
    type1, type2: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN M3CTypeCompare.Identical(type1, type2);
  END Identical;


PROCEDURE IdenticalM3TYPEs(m1, m2: M3AST_AS.M3TYPE): BOOLEAN RAISES {}=
  VAR
    t1, t2: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m1, t1);
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m2, t2);
    RETURN Identical(t1, t2);
  END IdenticalM3TYPEs;


(* Procedures for checking that one type is a subtype of another. Here are the
subtyping rules, extracted from the M3 report:

   We write T <: U to indicate that T is a subtype of U and U is a
   supertype of T.

   If T <: U, then every value of type T is also a value of type U.  The
   converse does not hold:  for example, a record or array type with packed
   fields contains the same values as the corresponding type with unpacked
   fields, but there is no subtype relation between them.  The subtype
   relation is defined by the following rules:

   [n..m] <: INTEGER   if n and m are integers
   [n..m] <: E         if n and m are from the enumeration type E
   [n..m] <: [n'..m']  if [n..m] is a (possibly empty) subset of [n'..m']

   ARRAY U OF T <: ARRAY V OF T  if NUMBER(U) = NUMBER(V)

       That is, one fixed array type is a subtype of another if the element
       types are identical and the index types have the same number of
       elements.

   ARRAY I1 OF ... ARRAY In OF T <: (ARRAY OF)n T

       if the I's are ordinal types or omitted.
       That is, any n-dimensional array type with element type T is a
       subtype of (ARRAY OF)n T.  (Omitted I's create open array types.
       They must precede the fixed array types.)

   SET OF T <: SET OF T' if T <: T'

   NULL <: REF T <: REFANY
   NULL <: UNTRACED REF T <: ADDRESS

       That is, fixed references are subtypes of open references, as long
       as they are of the same reference class.  NIL is a member of every
       reference type.


   NULL <: PROCEDURE(A): R RAISES S  for any A, R, and S.

       That is, NIL is a member of every procedure type.

   PROCEDURE(A): Q RAISES E <: PROCEDURE(B): R RAISES F

       if (A): Q RAISES E is covered by (B): R RAISES F.
       That is, for procedure types, T <: T' if the raises set for T is
       contained in the raises set for T' and the signatures are otherwise
       identical except possibly for parameter names and defaults.

   OBJECT ... END <: REFANY
   UNTRACED OBJECT ... END <: ADDRESS

       That is, every object is a reference.

   NULL <: T OBJECT ... END <: T

       That is, NIL is a member of every object subtype (and therefore
       every object type).  Every subtype is included in its supertype.


   BITS n FOR T <: T  and  T <: BITS n FOR T

       That is, BITS FOR T has the same values as T.

   T <: T for all T
   T <: U and U <: V implies T <: V  for all T, U, V.

       That is, <: is reflexive and transitive.

   Note that T <: U and U <: T does not imply that T and U are identical,
   since the subtype relation is unaffected by parameter names, default
   values, and packing.
*)

PROCEDURE SameNumber(
    ordinal1, ordinal2: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  VAR
    ns1, ns2: M3CBackEnd.NumStatus;
    number1, number2: M3AST_SM.Exp_value;
  BEGIN
    (* arguments must be ordinals *)
    IF M3COrdinal.Identical(ordinal1, ordinal2) THEN
      RETURN TRUE;
    ELSE
      ns1 := M3CExpValue.Number(ordinal1, number1);
      ns2 := M3CExpValue.Number(ordinal2, number2);
      IF ns1 = ns2 THEN
        RETURN ns1 # M3CBackEnd.NumStatus.Valid OR
            M3CBackEnd.Compare(number1, number2) = 0;
      ELSE
        (* We return TRUE if one result was unknown and the other was valid *)
        RETURN ns1 # M3CBackEnd.NumStatus.Overflow AND
            ns1 # M3CBackEnd.NumStatus.Overflow;
      END; (* if *)
    END; (* if *)
  END SameNumber;


PROCEDURE IdenticalElements(
    a1, a2: M3AST_AS.Array_type;
    VAR elemType1, elemType2: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(a1.as_elementtype, elemType1);
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(a2.as_elementtype, elemType2);
    RETURN Identical(elemType1, elemType2);
  END IdenticalElements;


PROCEDURE SubArray(array1, array2: M3AST_AS.Array_type): BOOLEAN RAISES {}=
  VAR
    a1, a2: M3AST_AS.Array_type;
    index1, index2: M3CTypesMisc.Ix;
    indexType1, indexType2, elemType1, elemType2: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    a1 := array1.sm_norm_type;
    a2 := array2.sm_norm_type;
    index2 := M3CTypesMisc.Index(a2, indexType2);
    IF index2 = M3CTypesMisc.Ix.Open THEN
      LOOP
        IF IdenticalElements(a1, a2, elemType1, elemType2) THEN
          RETURN TRUE;
        ELSE
          IF M3CTypesMisc.IsOpenArray(elemType2) AND
              (ISTYPE(elemType1, M3AST_AS.Array_type)) THEN
            a1 := NARROW(elemType1, M3AST_AS.Array_type).sm_norm_type;
            a2 := NARROW(elemType2, M3AST_AS.Array_type).sm_norm_type;
            (* loop *)
          ELSE
            RETURN FALSE;
          END; (* if *)
        END; (* if *)
      END; (* loop *)
    ELSE
      IF IdenticalElements(a1, a2, elemType1, elemType2) THEN
        index1 := M3CTypesMisc.Index(a1, indexType1);
        IF (index1 = M3CTypesMisc.Ix.Ordinal) AND
            (index2 = M3CTypesMisc.Ix.Ordinal) THEN
          RETURN SameNumber(indexType1, indexType2);
        ELSE
          RETURN (index1 # M3CTypesMisc.Ix.Open) AND
              Identical(indexType1, indexType2);
        END;
      ELSE
        RETURN FALSE;
      END; (* if *)
    END; (* if *)
  END SubArray;


PROCEDURE IdenticalResultTypes(
    r1, r2: M3AST_AS.M3TYPE_NULL)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF r1 = r2 THEN
      RETURN TRUE;
    ELSIF r1 = NIL OR r2 = NIL THEN
      RETURN FALSE;
    ELSE
      RETURN IdenticalM3TYPEs(r1, r2);
    END; (* if *)
  END IdenticalResultTypes;


PROCEDURE FirstFormal(
    id: M3AST_AS.FORMAL_ID;
    VAR ts: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF ISTYPE(id, M3AST_AS.F_Value_id) THEN
      ts := M3CTypesMisc.Concrete(id.sm_type_spec);
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END FirstFormal;


PROCEDURE InternalCovered(
    p1, p2: M3AST_AS.Procedure_type;
    o2: M3AST_AS.Object_type)
    : BOOLEAN
    RAISES {}=
  VAR
    i1: M3ASTNext.IterFormal;
    i2: M3ASTNext.IterFormal;
    formal1, formal2: M3AST_AS.Formal_param;
    b1, b2: BOOLEAN;
    id1, id2: M3AST_AS.FORMAL_ID;
    t1, t2: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    (* Check result types identical, 'p1' raises set subset of 'p2's *)
    IF NOT
        (IdenticalResultTypes(p1.as_result_type, p2.as_result_type) AND
        (M3CRaisesSet.Compare(p1.as_raises, p2.as_raises) IN
            M3CRaisesSet.SubSet)) THEN
      RETURN FALSE;
    END;

    (* Initialize parameter iterations *)
    i1 := M3ASTNext.NewIterFormal(p1.as_formal_param_s);
    i2 := M3ASTNext.NewIterFormal(p2.as_formal_param_s);

    (* Check first parameter if either 'p1' or 'p2' is a method procedure type
     with an implied first parameter *)
    b1 := M3CTypesMisc.HiddenObjectParameter(p1, t1);
    IF o2 # NIL THEN
      b2 := TRUE;
      t2 := o2;
    ELSE
      b2 := M3CTypesMisc.HiddenObjectParameter(p2, t2);
    END;
    IF b1 OR b2 THEN
      IF NOT (b1 AND b2) THEN
        IF b1 THEN
          b2 := M3ASTNext.Formal(i2, formal2, id2);
          IF NOT (b2 AND FirstFormal(id2, t2)) THEN
            RETURN FALSE;
          END;
        ELSE
          b1 := M3ASTNext.Formal(i1, formal1, id1);
          IF NOT (b1 AND FirstFormal(id1, t1)) THEN
            RETURN FALSE;
          END;
        END;
      END;
      IF o2 # NIL THEN
        (* checking for method satisfaction, not normal procedure covering *)
        IF NOT SubType(t2, t1) THEN RETURN FALSE END;
      ELSE
        IF NOT Identical(t1, t2) THEN RETURN FALSE END;
      END;
    END;

    (* Check remaining parameters *)
    LOOP
      b1 := M3ASTNext.Formal(i1, formal1, id1);
      IF b1 # M3ASTNext.Formal(i2, formal2, id2) THEN RETURN FALSE END;
      IF NOT b1 THEN RETURN TRUE END; (* all parameters checked *)
      IF (TYPECODE(id1) # TYPECODE(id2)) OR
          NOT Identical(id1.sm_type_spec, id2.sm_type_spec) THEN
        RETURN FALSE;
      END;
    END; (* loop *)
  END InternalCovered;


<*INLINE*> PROCEDURE Covered(p1, p2: M3AST_AS.Procedure_type): BOOLEAN RAISES {}=
  BEGIN
    RETURN InternalCovered(p1, p2, NIL);
  END Covered;


PROCEDURE IsAncestor(
    ancestor: M3AST_SM.TYPE_SPEC_UNSET; (* must be object or opaque type *)
    child: M3AST_AS.Object_type)
    : BOOLEAN
    RAISES {}=
  VAR
    super: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    (* note: we already know 'child' and 'ancestor' are not identical *)
    super := child;
    WHILE M3ASTNext.SimpleSuperType(super, super) DO
      LOOP
        IF Identical(super, ancestor) THEN RETURN TRUE END;
        IF ISTYPE(super, M3AST_AS.Opaque_type) THEN
          super := M3CConcTypeSpec.CurrentReveal(super);
        ELSE
          EXIT;
        END;
      END;
      IF NOT ISTYPE(super, M3AST_AS.Object_type) THEN RETURN FALSE END;
    END; (* while *)
    RETURN FALSE;
  END IsAncestor;


PROCEDURE ObjectSubType(
    o: M3AST_AS.Object_type;
    type: M3AST_AS.TYPE_SPEC)
    : BOOLEAN
    RAISES {}=
  VAR
    traced: BOOLEAN;
  BEGIN
    IF ISTYPE(type, M3AST_AS.Object_type) OR ISTYPE(type, M3AST_AS.Opaque_type) THEN
      RETURN IsAncestor(type, o);
    ELSE
      traced := M3CTypesMisc.IsTracedObject(o) IN M3CTypesMisc.ProbablyTraced;
      TYPECASE type OF
      | M3AST_AS.RefAny_type =>
          RETURN traced;
      | M3AST_AS.Address_type =>
          RETURN NOT traced;
      | M3AST_AS.Root_type(root) =>
          RETURN (root.as_trace_mode = NIL) = traced;
      ELSE
        RETURN FALSE;
      END;
    END;
  END ObjectSubType;


PROCEDURE SubType(type1, type2: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  BEGIN
    IF type1 = type2 OR type1 = NIL OR type2 = NIL THEN RETURN TRUE END;

    (* assert: neither type is NIL *)
    type1 := M3CTypesMisc.CheckedUnpack(type1);
    type2 := M3CTypesMisc.CheckedUnpack(type2);
    IF type1 = type2 OR type1 = NIL OR type2 = NIL THEN RETURN TRUE END;

    (* assert: neither type is packed or NIL *)
    TYPECASE type1 OF
    | M3AST_AS.Integer_type =>
        IF ISTYPE(type2, M3AST_AS.Integer_type) THEN RETURN TRUE END;
        RETURN M3COrdinal.SubType(type1, type2);
    | M3AST_AS.Enumeration_type, M3AST_AS.Subrange_type,
      M3AST_AS.Set_type =>
        RETURN M3COrdinal.SubType(type1, type2);
    ELSE
      (* drop through *)
    END; (* case *)

    (* assert: neither type is packed or unset and 'type1' is not an ordinal
     or a set *)
    IF TYPECODE(type1) = TYPECODE(type2) THEN
      (* assert: types are not unset or packed, are of the same basic form and
       are not ordinal types, sets or opened opaque types*)
      TYPECASE type1 OF
      | M3AST_AS.FLOAT_TYPE,
        M3AST_AS.RefAny_type, M3AST_AS.Address_type,
        M3AST_AS.Null_type =>
          RETURN TRUE;
      | M3AST_AS.Root_type(root_type) =>
          RETURN root_type.as_trace_mode # NIL =
              (NARROW(type2, M3AST_AS.Root_type).as_trace_mode # NIL);
      | M3AST_AS.Array_type =>
          RETURN SubArray(type1, type2);
      | M3AST_AS.Procedure_type =>
          RETURN Covered(type1, type2);
      | M3AST_AS.Object_type =>
          RETURN Identical(type1, type2) OR IsAncestor(type2, type1);
      | M3AST_AS.Opaque_type =>
          RETURN SubType(M3CConcTypeSpec.CurrentReveal(type1), type2);
      ELSE
        RETURN Identical(type1, type2);
      END; (* case *)
    ELSE
      (* assert: types are not unset or packed, they do not have the same basic
       form, are not opened opaque types and 'type1' is not ordinal or a set *)
      TYPECASE type1 OF
      | M3AST_AS.Null_type =>
          TYPECASE type2 OF
          | M3AST_AS.Ref_type, M3AST_AS.Procedure_type,
            M3AST_AS.Object_type, M3AST_AS.Opaque_type,
            M3AST_AS.Address_type, M3AST_AS.RefAny_type,
            M3AST_AS.Root_type =>
              RETURN TRUE;
          ELSE
            RETURN FALSE;
          END; (* case *)
      | M3AST_AS.Ref_type, M3AST_AS.Root_type =>
          IF M3CTypesMisc.IsTracedRef(type1) IN
              M3CTypesMisc.ProbablyTraced THEN
            RETURN ISTYPE(type2, M3AST_AS.RefAny_type);
          ELSE
            RETURN ISTYPE(type2, M3AST_AS.Address_type);
          END;
      | M3AST_AS.Object_type =>
          RETURN ObjectSubType(type1, type2);
      | M3AST_AS.Opaque_type =>
          RETURN SubType(M3CConcTypeSpec.CurrentReveal(type1), type2);
      ELSE
        RETURN FALSE;
      END; (* case *)
    END; (* if *)
  END SubType;


(*  A type T is assignable to a type U if:

      - T <: U, or

      - T is a reference type other than ADDRESS(This restriction is
        lifted in unsafe modules.) and U <: T, or

      - T and U are ordinal types with at least one member in common.
*)

PROCEDURE Assignable(
    lhs, rhs: M3AST_SM.TYPE_SPEC_UNSET;
    safe: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF SubType(rhs, lhs) THEN
      (* returns TRUE if either 'rhs' or 'lhs' is unset *)
      RETURN TRUE;
    ELSE
      IF (M3CTypesMisc.IsRef(rhs) AND
              (NOT safe OR NOT ISTYPE(rhs, M3AST_AS.Address_type))) OR
          ISTYPE(rhs, M3AST_AS.Array_type) THEN
        RETURN SubType(lhs, rhs);
      ELSE
        RETURN M3COrdinal.Overlap(rhs, lhs);
      END; (* if *)
    END; (* if *)
  END Assignable;


PROCEDURE VARPassable(
    formal, actual: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF Identical(formal, actual) OR
        (M3CTypesMisc.IsOpenArray(formal) AND
            Assignable(formal, actual, TRUE)) OR
        (M3CTypesMisc.IsOpenArray(actual) AND
            Assignable(actual, formal, TRUE)) THEN
      (* type checking is ok - any restrictions imposed by the back end? *)
      RETURN formal = NIL OR M3CBackEnd.VarParamOK(formal);
    ELSE
      RETURN FALSE;
    END; (* if *)
  END VARPassable;


PROCEDURE Satisfies(
    procedure: M3AST_AS.Procedure_type;
    object: M3AST_AS.Object_type;
    method: M3AST_AS.Procedure_type)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN InternalCovered(procedure, method, object);
  END Satisfies;


PROCEDURE SameReferenceClass(
    t1, t2: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  CONST
    TracedAndUntraced = M3CTypesMisc.RefSet{
        M3CTypesMisc.Ref.Traced, M3CTypesMisc.Ref.Untraced};
  VAR
    te1, te2: M3CTypesMisc.Ref;
  BEGIN
    te1 := M3CTypesMisc.IsTracedRef(t1);
    te2 := M3CTypesMisc.IsTracedRef(t2);
    IF (te1 IN TracedAndUntraced) AND (te2 IN TracedAndUntraced) THEN
      RETURN te1 = te2;
    ELSE
      RETURN (te1 # M3CTypesMisc.Ref.Not) AND (te2 # M3CTypesMisc.Ref.Not);
    END; (* if *)
  END SameReferenceClass;


<*INLINE*> PROCEDURE SameOrdinalSupertype(
    type1, type2: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN M3COrdinal.SameSupertype(type1, type2);
  END SameOrdinalSupertype;


BEGIN
END M3CTypeRelation.
