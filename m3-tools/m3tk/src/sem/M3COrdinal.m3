MODULE M3COrdinal;

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

IMPORT M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_Enum_id;

IMPORT M3CBackEnd, M3CTypesMisc, M3Assert;


PROCEDURE Is(
    t: M3AST_SM.TYPE_SPEC_UNSET;
    VAR baseType: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    TYPECASE t OF
    | NULL =>
        baseType := t;
        RETURN TRUE;
    | M3AST_AS.Integer_type, M3AST_AS.Enumeration_type =>
        baseType := t;
        RETURN TRUE;
    | M3AST_AS.Subrange_type(subrange) =>
        baseType := subrange.sm_base_type_spec;
        RETURN TRUE;
    | M3AST_AS.Packed_type(packed) =>
        RETURN Is(M3CTypesMisc.Unpack(packed), baseType);
    ELSE
      RETURN FALSE;
    END; (* case *)
  END Is;


PROCEDURE IdenticalEnumerations(
    enum1, enum2: M3AST_AS.Enumeration_type)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF enum1 = enum2 THEN RETURN TRUE END;
    VAR
      i1 := SeqM3AST_AS_Enum_id.NewIter(enum1.as_id_s);
      i2 := SeqM3AST_AS_Enum_id.NewIter(enum2.as_id_s);
    BEGIN
      LOOP
        VAR
          id1, id2: M3AST_AS.Enum_id;
          b1 := SeqM3AST_AS_Enum_id.Next(i1, id1);
        BEGIN
          IF b1 # SeqM3AST_AS_Enum_id.Next(i2, id2) THEN RETURN FALSE END;
          IF NOT b1 THEN RETURN TRUE END;
          IF id1.lx_symrep # id2.lx_symrep THEN RETURN FALSE END;
        END;
      END; (* loop *)
    END;
  END IdenticalEnumerations;


PROCEDURE SameSupertype(
    type1, type2: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  VAR
    base1, base2: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF Is(type1, base1) AND Is(type2, base2) THEN
      IF base1 = NIL OR base2 = NIL THEN RETURN TRUE END;
      TYPECASE base1 OF
      | M3AST_AS.Enumeration_type(enum1) =>
          TYPECASE base2 OF
          | M3AST_AS.Enumeration_type(enum2) =>
              RETURN IdenticalEnumerations(enum1, enum2);
          ELSE
            RETURN FALSE;
          END;
      | M3AST_AS.Integer_type =>
          RETURN ISTYPE(base2, M3AST_AS.Integer_type);
      ELSE
        RETURN FALSE;
      END;
    ELSE
      RETURN FALSE;
    END; (* if *)
  END SameSupertype;


PROCEDURE ValidBounds(
    subrange: M3AST_AS.Subrange_type;
    VAR first, last: M3AST_SM.Exp_value)
    : BOOLEAN
    RAISES {}=
  VAR
    range := subrange.as_range;
    exp1 := range.as_exp1;
    exp2 := range.as_exp2;
    type1 := exp1.sm_exp_type_spec;
    type2 := exp2.sm_exp_type_spec;
    base1, base2: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF subrange.sm_base_type_spec # NIL AND
        type1 # NIL AND type2 # NIL AND
        Is(type1, base1) AND Is(type2, base2) AND
        base1 # NIL AND base2 # NIL AND
        Identical(base1, base2) AND
        exp1.sm_exp_value # NIL AND exp2.sm_exp_value # NIL THEN
      first := exp1.sm_exp_value;
      last := exp2.sm_exp_value;
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END; (* if *)
  END ValidBounds;


TYPE
  CompareProc = PROCEDURE(t1, t2: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};


PROCEDURE CompareM3TYPEs(
    m1, m2: M3AST_AS.M3TYPE;
    p: CompareProc)
    : BOOLEAN
    RAISES {}=
  VAR
    t1, t2: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m1, t1);
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m2, t2);
    IF t1 = NIL THEN RETURN TRUE END;
    TYPECASE t1 OF
    | M3AST_AS.Integer_type,
      M3AST_AS.Enumeration_type,
      M3AST_AS.Subrange_type,
      M3AST_AS.Set_type,
      M3AST_AS.Packed_type =>
        RETURN p(t1, t2);
    ELSE
      RETURN FALSE;
    END; (* case *)
  END CompareM3TYPEs;


PROCEDURE Identical(t1, t2: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  VAR
    base1: M3AST_SM.TYPE_SPEC_UNSET;
    first1, last1, first2, last2: M3AST_SM.Exp_value;
  BEGIN
    IF t1 = NIL THEN M3Assert.Fail() END;
    IF t2 = NIL THEN RETURN TRUE END;

    IF TYPECODE(t1) # TYPECODE(t2) THEN RETURN FALSE END;

    (* assert: 't1' and 't2' are of the same form and neither is NIL *)
    TYPECASE t1 OF
    | M3AST_AS.Integer_type =>
        RETURN TRUE;
    | M3AST_AS.Enumeration_type =>
        RETURN IdenticalEnumerations(t1, t2);
    | M3AST_AS.Subrange_type(subrange_type) =>
        base1 := subrange_type.sm_base_type_spec;
        IF base1 = NIL OR
            Identical(base1,
                NARROW(t2, M3AST_AS.Subrange_type).sm_base_type_spec) THEN
          IF ValidBounds(t1, first1, last1) AND
              ValidBounds(t2, first2, last2) THEN
            RETURN (M3CBackEnd.Compare(first1, first2) = 0) AND
                (M3CBackEnd.Compare(last1, last2) = 0);
          ELSE
            RETURN TRUE; (* one base type is unset; we'll be optimistic *)
          END; (* if *)
        ELSE
          RETURN FALSE;
        END;
    | M3AST_AS.Set_type(set_type) =>
        RETURN CompareM3TYPEs(set_type.as_type,
            NARROW(t2, M3AST_AS.Set_type).as_type, Identical);
    | M3AST_AS.Packed_type(packed_type) =>
        RETURN CompareM3TYPEs(packed_type.as_type,
            NARROW(t2, M3AST_AS.Packed_type).as_type, Identical);
    ELSE
      M3Assert.Fail();
      <*ASSERT FALSE*>
    END; (* case *)
  END Identical;


PROCEDURE SubType(t1, t2: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  VAR
    base1: M3AST_SM.TYPE_SPEC_UNSET;
    unset: BOOLEAN;
    first1, last1, first2, last2: M3AST_SM.Exp_value;
  BEGIN
    IF t1 = NIL THEN M3Assert.Fail() END;
    IF t2 = NIL THEN RETURN TRUE END;
    IF ISTYPE(t2, M3AST_AS.Packed_type) THEN
      RETURN SubType(t1, M3CTypesMisc.Unpack(t2));
    ELSE
      TYPECASE t1 OF
      | M3AST_AS.Integer_type =>
          RETURN ISTYPE(t2, M3AST_AS.Integer_type);
      | M3AST_AS.Enumeration_type =>
          RETURN ISTYPE(t2, M3AST_AS.Enumeration_type) AND
              IdenticalEnumerations(t1, t2);
      | M3AST_AS.Subrange_type(subrange_type) =>
          base1 := subrange_type.sm_base_type_spec;
          unset := base1 = NIL;
          TYPECASE t2 OF
          | M3AST_AS.Subrange_type(subrange_type) =>
              IF unset OR Identical(base1,
                  subrange_type.sm_base_type_spec) THEN
                IF ValidBounds(t1, first1, last1) AND
                    ValidBounds(t2, first2, last2) THEN
                  RETURN (M3CBackEnd.Compare(first1, first2) >= 0) AND
                      (M3CBackEnd.Compare(last1, last2) <= 0);
                ELSE
                  RETURN TRUE;
                END; (* if *)
              ELSE
                RETURN FALSE;
              END;
          | M3AST_AS.Enumeration_type =>
              RETURN unset OR
                  (ISTYPE(base1, M3AST_AS.Enumeration_type) AND
                      IdenticalEnumerations(base1, t2));
          | M3AST_AS.Integer_type =>
              RETURN unset OR ISTYPE(base1, M3AST_AS.Integer_type);
          ELSE
            RETURN FALSE;
          END; (* case *)
      | M3AST_AS.Set_type =>
          RETURN Identical(t1, t2);
      | M3AST_AS.Packed_type =>
          RETURN SubType(M3CTypesMisc.Unpack(t1), t2);
      ELSE
        M3Assert.Fail();
        <*ASSERT FALSE *>
      END; (* case *)
    END; (* if *)
  END SubType;


PROCEDURE Overlap(t1, t2: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  VAR
    base1, base2, o1, o2: M3AST_SM.TYPE_SPEC_UNSET;
    first1, last1, first2, last2: M3AST_SM.Exp_value;
  BEGIN
    IF Is(t1, base1) AND Is(t2, base2) AND
        (base1 = NIL OR Identical(base1, base2)) THEN
      o1 := M3CTypesMisc.CheckedUnpack(t1);
      o2 := M3CTypesMisc.CheckedUnpack(t2);
      IF o1 # NIL AND o2 # NIL AND
          ISTYPE(o1, M3AST_AS.Subrange_type) AND
          ISTYPE(o2, M3AST_AS.Subrange_type) AND
          ValidBounds(o1, first1, last1) AND
          ValidBounds(o2, first2, last2) THEN
        (* check FIRST(t1) <= LAST(t2) AND LAST(t1) >= FIRST(t2). Also have
         to check that both types are non empty *)
        RETURN (M3CBackEnd.Compare(first1, last2) <= 0) AND
            (M3CBackEnd.Compare(last1, first2) >= 0) AND
            (M3CBackEnd.Compare(first1, last1) <= 0) AND
            (M3CBackEnd.Compare(first2, last2) <= 0);
      ELSE
        (* either one type is not a subrange or there has been an error. In
         either case we test to see if either type is empty (an empty type
         cannot overlap with anything). Note that 'IsEmpty' returns FALSE
         if a type is unset *)
        RETURN NOT (M3CTypesMisc.IsEmpty(t1) OR M3CTypesMisc.IsEmpty(t2));
      END; (* if *)
    ELSE
      RETURN FALSE;
    END; (* if *)
  END Overlap;


PROCEDURE IsMemberOf(
    sub: M3AST_AS.Subrange_type;
    exp: M3AST_AS.EXP)
    : BOOLEAN
    RAISES {}=
  VAR
    base: M3AST_SM.TYPE_SPEC_UNSET;
    first, last: M3AST_SM.Exp_value;
  BEGIN
    IF Is(exp.sm_exp_type_spec, base) THEN
      IF base = NIL THEN
        RETURN TRUE;
      ELSE
        IF Identical(base, sub.sm_base_type_spec) THEN
          IF exp.sm_exp_value # NIL AND ValidBounds(sub, first, last) THEN
            RETURN (M3CBackEnd.Compare(first, exp.sm_exp_value) <= 0) AND
                (M3CBackEnd.Compare(exp.sm_exp_value, last) <= 0);
          ELSE
            (* something is unset; optimism is called for *)
            RETURN TRUE;
          END; (* if *)
        ELSE
          RETURN FALSE;
        END; (* if *)
      END; (* if *)
    ELSE
      RETURN FALSE;
    END; (* if *)
  END IsMemberOf;


BEGIN
END M3COrdinal.
