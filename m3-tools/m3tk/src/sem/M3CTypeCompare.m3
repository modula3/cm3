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
(**)
(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)


MODULE M3CTypeCompare;

IMPORT Word;

IMPORT M3AST_AS, M3AST_SM, M3ASTNext;

IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_Fields, SeqM3AST_AS_Method, SeqM3AST_AS_Override,
    SeqM3AST_AS_Formal_param, SeqM3AST_AS_M3TYPE;

IMPORT M3CSrcPos, M3CTypesMisc, M3CRaisesSet, M3COrdinal;
IMPORT M3CExpValue;


TYPE
  Map = REF RECORD
    next: Map := NIL;
    entryCount: CARDINAL := 0;
    entries: ARRAY [0..15] OF RECORD
      typeSpec1, typeSpec2: M3AST_AS.TYPE_SPEC;
    END;
  END; (* record *)


PROCEDURE NewMap(t1, t2: M3AST_AS.TYPE_SPEC): Map RAISES {}=
  VAR
    new := NEW(Map);
  BEGIN
    new.entries[0].typeSpec1 := t1;
    new.entries[0].typeSpec2 := t2;
    RETURN new;
  END NewMap;


PROCEDURE AlreadyBeenHere(
    t1, t2: M3AST_AS.TYPE_SPEC;
    map: Map)
    : BOOLEAN
    RAISES {}=
  VAR
    m: Map;
  BEGIN
    IF map # NIL THEN
      m := map;
      LOOP
        FOR e := 0 TO m.entryCount DO
          WITH entry = m.entries[e] DO
            IF ((t1 = entry.typeSpec1) AND (t2 = entry.typeSpec2)) OR
                ((t2 = entry.typeSpec1) AND (t1 = entry.typeSpec2)) THEN
              RETURN TRUE;
            END; (* if *)
          END; (* with *)
        END; (* for *)
        IF m.next = NIL THEN
          IF m.entryCount = LAST(m.entries) THEN
            m.next := NewMap(t1, t2);
          ELSE
            INC(m.entryCount);
            WITH entry = m.entries[m.entryCount] DO
              entry.typeSpec1 := t1; entry.typeSpec2 := t2;
            END; (* with *)
          END; (* if *)
          RETURN FALSE;
        ELSE
          m := m.next;
          (* and loop *)
        END; (* if *)
      END; (* loop *)
    ELSE
      RETURN FALSE;
    END; (* if *)
  END AlreadyBeenHere;


PROCEDURE TYPE_SPEC_UNSETs(
    ts1, ts2: M3AST_SM.TYPE_SPEC_UNSET;
    map: Map;
    search: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF ts1 = ts2 THEN RETURN TRUE END;
    IF ts1 = NIL OR ts2 = NIL THEN RETURN FALSE END;
    IF map # NIL THEN
      IF search AND AlreadyBeenHere(ts1, ts2, map) THEN
        RETURN TRUE;
      ELSE
        RETURN Compare(ts1, ts2, map);
      END; (* if *)
    ELSE
      RETURN ts1.tmp_type_code = ts2.tmp_type_code;
    END; (* if *)
  END TYPE_SPEC_UNSETs;


PROCEDURE M3TYPEs(m1, m2: M3AST_AS.M3TYPE; map: Map): BOOLEAN RAISES {}=
  VAR
    search: BOOLEAN;
    typeSpec1, typeSpec2: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    search := (map # NIL) AND
        ((ISTYPE(m1, M3AST_AS.Named_type)) OR
            (ISTYPE(m1, M3AST_AS.Named_type)));
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m1, typeSpec1);
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m2, typeSpec2);
    RETURN TYPE_SPEC_UNSETs(typeSpec1, typeSpec2, map, search);
  END M3TYPEs;


PROCEDURE TYPED_IDs(
    id1, id2: M3AST_AS.TYPED_ID;
    map: Map)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN (id1.lx_symrep = id2.lx_symrep) AND
        TYPE_SPEC_UNSETs(id1.sm_type_spec, id2.sm_type_spec, map, TRUE);
  END TYPED_IDs;


PROCEDURE EXPs(e1, e2: M3AST_AS.EXP): BOOLEAN RAISES {}=
  BEGIN
    IF e1.sm_exp_value # NIL AND e2.sm_exp_value # NIL THEN
      RETURN M3CExpValue.Equal(e1, e2);
    ELSE
      RETURN TRUE;
    END; (* if *)
  END EXPs;


PROCEDURE Defaults(d1, d2: M3AST_SM.EXP_NULL_UNSET): BOOLEAN RAISES {}=
  BEGIN
    IF d1 = NIL OR d2 = NIL THEN
      RETURN d1 = d2;
    ELSE
      WITH unset = M3AST_SM.UNSET_EXP() DO
        RETURN d1 = unset OR d2 = unset OR EXPs(d1, d2);
      END;
    END; (* if *)
  END Defaults;


PROCEDURE Arrays(
    array1, array2: M3AST_AS.Array_type;
    map: Map)
    : BOOLEAN
    RAISES {}=
  VAR
    index1, index2: M3CTypesMisc.Ix;
    index_type1, index_type2: M3AST_SM.TYPE_SPEC_UNSET;
    a1 := array1.sm_norm_type;
    a2 := array2.sm_norm_type;
  BEGIN
    IF M3TYPEs(a1.as_elementtype, a2.as_elementtype, map) THEN
      index1 := M3CTypesMisc.Index(a1, index_type1);
      index2 := M3CTypesMisc.Index(a2, index_type2);
      RETURN (index1 = index2) AND
          ((index1 = M3CTypesMisc.Ix.Open) OR
              TYPE_SPEC_UNSETs(index_type1, index_type2, map, TRUE));
    ELSE
      RETURN FALSE;
    END; (* if *)
  END Arrays;


PROCEDURE Fields(
    fields1, fields2: SeqM3AST_AS_Fields.T;
    map: Map)
    : BOOLEAN
    RAISES {}=
  VAR
    i1 := M3ASTNext.NewIterField(fields1);
    i2 := M3ASTNext.NewIterField(fields2);
    b1: BOOLEAN;
    id1, id2: M3AST_AS.Field_id;
  BEGIN
    LOOP
      b1 := M3ASTNext.Field(i1, id1);
      IF b1 # M3ASTNext.Field(i2, id2) THEN RETURN FALSE END;
      IF NOT b1 THEN RETURN TRUE END;
      IF TYPED_IDs(id1, id2, map) AND
          Defaults(id1.vINIT_ID.sm_init_exp, id2.vINIT_ID.sm_init_exp) THEN
        (* loop *)
      ELSE
        RETURN FALSE;
      END; (* if *)
    END; (* loop *)
  END Fields;


PROCEDURE Methods(
    seq1, seq2: SeqM3AST_AS_Method.T;
    map: Map)
    : BOOLEAN
    RAISES {}=
  VAR
    i1 := SeqM3AST_AS_Method.NewIter(seq1);
    i2 := SeqM3AST_AS_Method.NewIter(seq2);
    b1: BOOLEAN;
    m1, m2: M3AST_AS.Method;
    id1, id2: M3AST_AS.Method_id;
  BEGIN
    LOOP
      b1 := SeqM3AST_AS_Method.Next(i1, m1);
      IF b1 # SeqM3AST_AS_Method.Next(i2, m2) THEN RETURN FALSE END;
      IF NOT b1 THEN RETURN TRUE END;
      id1 := m1.as_id;
      id2 := m2.as_id;
      IF TYPED_IDs(id1, id2, map) AND
          Defaults(id1.vINIT_ID.sm_init_exp, id2.vINIT_ID.sm_init_exp) THEN
        (* continue *)
      ELSE
        RETURN FALSE;
      END; (* if *)
    END; (* loop *)
  END Methods;


PROCEDURE Overrides(
    seq1, seq2: SeqM3AST_AS_Override.T;
    map: Map)
    : BOOLEAN
    RAISES {}=
  VAR
    i1 := SeqM3AST_AS_Override.NewIter(seq1);
    i2 := SeqM3AST_AS_Override.NewIter(seq2);
    b1: BOOLEAN;
    m1, m2: M3AST_AS.Override;
    id1, id2: M3AST_AS.Override_id;
  BEGIN
    LOOP
      b1 := SeqM3AST_AS_Override.Next(i1, m1);
      IF b1 # SeqM3AST_AS_Override.Next(i2, m2) THEN RETURN FALSE END;
      IF NOT b1 THEN RETURN TRUE END;
      id1 := m1.as_id;
      id2 := m2.as_id;
      IF TYPED_IDs(id1, id2, map) AND
          Defaults(id1.vINIT_ID.sm_init_exp, id2.vINIT_ID.sm_init_exp) THEN
        (* continue *)
      ELSE
        RETURN FALSE;
      END; (* if *)
    END; (* loop *)
  END Overrides;


PROCEDURE Ancestors(
    t1, t2: M3AST_AS.M3TYPE_NULL;
    map: Map)
    : BOOLEAN
    RAISES {}=
  VAR
    m3type: M3AST_AS.M3TYPE;
    ts: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF t1 # NIL AND t2 # NIL THEN
      RETURN M3TYPEs(t1, t2, map);
    ELSIF t1 # NIL OR t2 # NIL THEN
      (* Only one type has an ancestor; they may still be identical if the
       single ancestor is ROOT *)
      IF t1 # NIL THEN
        m3type := t1;
      ELSE
        m3type := t2;
      END;
      M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m3type, ts);
      TYPECASE ts OF
      | NULL =>
          RETURN FALSE;
      | M3AST_AS.Root_type(root) =>
          RETURN root.as_trace_mode = NIL;
      ELSE
        RETURN FALSE;
      END;
    ELSE (* neither has an ancestor - all is well *)
      RETURN TRUE;
    END;
  END Ancestors;


PROCEDURE Objects(o1, o2: M3AST_AS.Object_type; map: Map): BOOLEAN RAISES {}=
  BEGIN
    IF o1.as_brand # NIL OR o2.as_brand # NIL THEN
      RETURN FALSE;
    ELSE
      RETURN Ancestors(o1.as_ancestor, o2.as_ancestor, map) AND
          Fields(o1.as_fields_s, o2.as_fields_s, map) AND
          Methods(o1.as_method_s, o2.as_method_s, map) AND
          Overrides(o1.as_override_s, o2.as_override_s, map);
    END;
  END Objects;


PROCEDURE Sets(s1, s2: M3AST_AS.Set_type; map: Map): BOOLEAN RAISES {}=
  BEGIN
    RETURN M3TYPEs(s1.as_type, s2.as_type, map);
  END Sets;


PROCEDURE Formals(
    f1, f2: SeqM3AST_AS_Formal_param.T;
    map: Map)
    : BOOLEAN
    RAISES {}=
  VAR
    i1 := M3ASTNext.NewIterFormal(f1);
    i2 := M3ASTNext.NewIterFormal(f2);
    b1: BOOLEAN;
    formal1, formal2: M3AST_AS.Formal_param;
    id1, id2: M3AST_AS.FORMAL_ID;
  BEGIN
    LOOP
      b1 := M3ASTNext.Formal(i1, formal1, id1);
      IF b1 # M3ASTNext.Formal(i2, formal2, id2) THEN RETURN FALSE END;
      IF NOT b1 THEN RETURN TRUE END;
      IF TYPECODE(id1) # TYPECODE(id2) OR NOT TYPED_IDs(id1, id2, map) THEN
        RETURN FALSE
      END;
      TYPECASE id1 OF
      | M3AST_AS.F_Value_id(valueId) =>
          IF NOT Defaults(valueId.vINIT_ID.sm_init_exp,
              NARROW(id2, M3AST_AS.F_Value_id).vINIT_ID.sm_init_exp) THEN
            RETURN FALSE;
          END; (* if *)
      | M3AST_AS.F_Readonly_id(readonlyId) =>
          IF NOT Defaults(readonlyId.vINIT_ID.sm_init_exp,
              NARROW(id2, M3AST_AS.F_Readonly_id).vINIT_ID.sm_init_exp) THEN
            RETURN FALSE;
          END; (* if *)
      ELSE
        (* no default - var parameter *)
      END; (* typecase *)
      (* loop *)
    END; (* loop *)
  END Formals;


PROCEDURE HiddenFirstParams(
    p1, p2: M3AST_AS.Procedure_type;
    map: Map)
    : BOOLEAN
    RAISES {}=
  VAR
    t1, t2: M3AST_SM.TYPE_SPEC_UNSET;
    h1 := M3CTypesMisc.HiddenObjectParameter(p1, t1);
    h2 := M3CTypesMisc.HiddenObjectParameter(p2, t2);
  BEGIN
    IF h1 OR h2 THEN
      (* If only one of the types has a hidden first parameter then they
       cannot be identical; the name of the hidden parameter will not match
       the name of any normal parameter *)
      IF h1 AND h2 THEN
        RETURN TYPE_SPEC_UNSETs(t1, t2, map, TRUE);
      ELSE
        RETURN FALSE;
      END;
    ELSE
      RETURN TRUE;
    END;
  END HiddenFirstParams;


PROCEDURE Procedures(
    p1, p2: M3AST_AS.Procedure_type;
    map: Map)
    : BOOLEAN
    RAISES {}=
  VAR
    res1 := p1.as_result_type;
    res2 := p2.as_result_type;
  BEGIN
    RETURN (res1 # NIL) = (res2 # NIL) AND
        (res1 = NIL OR M3TYPEs(res1, res2, map)) AND
        HiddenFirstParams(p1, p2, map) AND
        Formals(p1.as_formal_param_s, p2.as_formal_param_s, map) AND
        M3CRaisesSet.Compare(p1.as_raises, p2.as_raises) =
            M3CRaisesSet.Comparison.Equal;
  END Procedures;


PROCEDURE Refs(r1, r2: M3AST_AS.Ref_type; map: Map): BOOLEAN RAISES {}=
  BEGIN
    RETURN r1.as_brand = NIL AND r2.as_brand = NIL AND
        M3TYPEs(r1.as_type, r2.as_type, map);
  END Refs;


PROCEDURE Packed(
    b1, b2: M3AST_AS.Packed_type;
    map: Map)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN EXPs(b1.as_exp, b2.as_exp) AND
        M3TYPEs(b1.as_type, b2.as_type, map)
   END Packed;


PROCEDURE Compare(t1, t2: M3AST_AS.TYPE_SPEC; map: Map): BOOLEAN RAISES {}=
  BEGIN
    IF TYPECODE(t1) = TYPECODE(t2) THEN
      TYPECASE t1 OF
      | M3AST_AS.Enumeration_type, M3AST_AS.Subrange_type =>
          RETURN M3COrdinal.Identical(t1, t2);
      | M3AST_AS.Array_type =>
          RETURN Arrays(t1, t2, map);
      | M3AST_AS.Record_type(tt1) =>
          RETURN Fields(
              tt1.as_fields_s,
              NARROW(t2, M3AST_AS.Record_type).as_fields_s, map);
      | M3AST_AS.Object_type =>
          RETURN Objects(t1, t2, map);
      | M3AST_AS.Set_type =>
          RETURN Sets(t1, t2, map);
      | M3AST_AS.Procedure_type =>
          RETURN Procedures(t1, t2, map);
      | M3AST_AS.Ref_type =>
          RETURN Refs(t1, t2, map);
      | M3AST_AS.Packed_type =>
          RETURN Packed(t1, t2, map);
      | M3AST_AS.Opaque_type =>
          RETURN FALSE;
      ELSE
        (* integers, reals, long reals, null, poly types *)
          RETURN TRUE;
      END; (* case *)
    ELSE
      (* No hope *)
      RETURN FALSE;
    END; (* if *)
  END Compare;


PROCEDURE Similar(t1, t2: M3AST_AS.TYPE_SPEC): BOOLEAN RAISES {}=
  BEGIN
    RETURN Compare(t1, t2, NIL);
  END Similar;


(* The 'Identical' function is widely used so it must be efficient. An attempt
is made to catch the common cases quickly. In the worst case 'Identical' has to
do a full structural equivalence check which can be expensive.

  Here are the key observations we use to try and speed up 'Identical'
1) Most programs are substantially correct; we don't care if it takes a long
time to detect an error.
2) In correct programs 'Identical' is only called when two types have some
relationship i.e. one is a subtype of the other.
3) Users tend to use only simple forms of structural equivalence; e.g given a
RECORD type it is usually much simpler to give the type a name and use that
everywhere rather than to write out the full RECORD type wherever it is used.
On the other hand simple types like REF INTEGER may not get a name.

  So in 'Identical', which compares two type specs 't1' and 't2', we expect
these cases:
a) Often 't1 = t2'. This happens when a type has a name and people use the name
to declare an item of that type, rather than using a structurally equivalent
type expression. So this happens a lot for complex types and also for the basic
types like INTEGER, REAL etc.
b) Often 't1' and 't2' are not identical but are related by the subtype
relation.
c) Less commonly we get two type specs which are simple and structurally
equivalent.
d) Rarely we get two type specs which are complex and structurally equivalent.
e) Rarely we get two type specs which are completely unrelated (due to an error
in the users program) or 't1 = NIL' or 't2 = NIL' (also due to error).

Case a is easy to handle, we just check for the equality of 't1' and 't2'.

Case b is trickier. We do an early check to see if the typecode of 't1' is the
same as that of 't2'. This gets rid of cases where two differing classes of
type e.g. a subrange and an enumeration are related by the subtype relation.
The trickier case is where there is a subtype relation between the same class
of type. This occurs for arrays, objects, procedures and packed types. We
currently don't worry about procedures and packed types on the grounds that
they are fairly infrequent. For arrays we do a quick check that both arrays
are open/not open. Object types are the biggest worry; they are frequently used
and there is a subtype relation between objects. The solution we use is to
keep a hash value in the object type code field; we only do a heavyweight check
on two object types if their hash values match.

Case c we deal with by handling some of the most commonly used forms of
structural equivalence straight away. e.g. If 't1' and 't2' are both refs we
just check if the referents are identical.

We don't wory about d the slow, general purpose mechanism handles it.

Case e is so easy to check for that we do it early on *)


PROCEDURE HashObject(
    original: M3AST_AS.Object_type)
    : INTEGER
    RAISES {}=
(* Two objects can only be identical if they have the same depth i.e. the
same number of supertypes. So depth would make an ideal hash value; if two
objects have the same depth they can only be subtypes if they are identical
and we assume that 'Identical' is usually called when 't1' and 't2' are
related by the subtype relation.
  Unfortunately we can't always see the full depth, due to opaque supertypes.
So we settle for the depth to nearest opaque supertype plus the source position
of that supertype. *)
  VAR
    o := original;
    superType: M3AST_SM.TYPE_SPEC_UNSET;
    hash := 1;
  BEGIN
    WHILE M3ASTNext.SimpleSuperType(o, superType) DO
      TYPECASE superType OF
      | M3AST_AS.Root_type =>
          EXIT; (* also NIL case *)
      | M3AST_AS.Opaque_type(opaqueType) =>
          VAR
            pos: CARDINAL;
            line := M3CSrcPos.Unpack(opaqueType.lx_srcpos, pos);
          BEGIN
            hash := Word.Plus(Word.Times(Word.Plus(Word.Times(
                line, 256), pos), 256), hash);
          END;
          EXIT;
      | M3AST_AS.Object_type(objectType) =>
          o := objectType;
          INC(hash);
      ELSE
        EXIT;
      END;
    END;
    IF hash = 0 THEN
      (* Very unlikely, as 'hash' is initially 1 *)
      INC(hash);
    END;
    original.tmp_type_code := hash;
    RETURN hash;
  END HashObject;


PROCEDURE Identical(t1, t2: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {}=
  VAR
    m3type1, m3type2: M3AST_AS.M3TYPE;
    count := 0;
  BEGIN
    (* We only go round the following loop looking for simple cases a limited
     number of times to avoid infinite looping (e.g. T = REF T and U = REF U
     could cause us grief *)
    LOOP

      (* Dispose of easy cases first; if 't1' and 't2' are the same type
       constructor or if either is NIL we return TRUE *)
      IF t1 = t2 OR t1 = NIL OR t2 = NIL THEN RETURN TRUE END;

      (* If 't1' and 't2' do not have the same typecode there is no hope
       of them being identical *)
      IF TYPECODE(t1) # TYPECODE(t2) THEN RETURN FALSE END;

      (* fall out to long-winded comparison *)
      IF count > 3 THEN EXIT END; 

      (* assert: neither type is NIL *)
      TYPECASE t1 OF
      | M3AST_AS.Integer_type,
        M3AST_AS.FLOAT_TYPE,
        M3AST_AS.RefAny_type, M3AST_AS.Address_type,
        M3AST_AS.Null_type =>
          RETURN TRUE;
      | M3AST_AS.Root_type(root_type) =>
          RETURN (root_type.as_trace_mode # NIL) =
              (NARROW(t2, M3AST_AS.Root_type).as_trace_mode # NIL);
      | M3AST_AS.Enumeration_type,
        M3AST_AS.Subrange_type =>
          (* remember opaque types cannot be ordinals *)
          RETURN M3COrdinal.Identical(t1, t2);
      | M3AST_AS.Ref_type(refType1) =>
          VAR
            refType2 := NARROW(t2, M3AST_AS.Ref_type);
          BEGIN
            IF refType1.as_brand # NIL OR refType2.as_brand # NIL OR
                (refType1.as_trace_mode # NIL) #
                    (refType2.as_trace_mode # NIL) THEN
              RETURN FALSE;
            END;
            m3type1 := refType1.as_type;
            m3type2 := refType2.as_type;
            (* Drop through to bottom of loop *)
          END;
      | M3AST_AS.Object_type(objectType1) =>
          VAR
            objectType2 := NARROW(t2, M3AST_AS.Object_type);
          BEGIN
            IF objectType1.as_brand # NIL OR objectType2.as_brand # NIL THEN
              RETURN FALSE;
            END;
            WITH
              typeCode1 = objectType1.tmp_type_code,
              typeCode2 = objectType2.tmp_type_code
            DO
              IF typeCode1 = 0 THEN typeCode1 := HashObject(objectType1) END;
              IF typeCode2 = 0 THEN typeCode2 := HashObject(objectType2) END;
              IF typeCode1 # typeCode2 THEN
                RETURN FALSE;
              ELSE
                EXIT; (* We need to do a thorough check *)
              END;
            END;
          END;
      | M3AST_AS.Array_type(array_type) =>
          VAR
	    arrayType1 := array_type.sm_norm_type;
            arrayType2 := NARROW(t2, M3AST_AS.Array_type).sm_norm_type;
            open1 := SeqM3AST_AS_M3TYPE.Empty(arrayType1.as_indextype_s);
            open2 := SeqM3AST_AS_M3TYPE.Empty(arrayType2.as_indextype_s);
          BEGIN
            IF open1 # open2 THEN RETURN FALSE END;
            IF open1 THEN
              m3type1 := arrayType1.as_elementtype;
              m3type2 := arrayType2.as_elementtype;
              (* Drop through to bottom of loop *)
            ELSE
              EXIT; (* Arrays are not open, go for the full check *)
            END;
          END;
      | M3AST_AS.Set_type(set_type) =>
          m3type1 := set_type.as_type;
          m3type2 := NARROW(t2, M3AST_AS.Set_type).as_type;
          (* Drop through to bottom of loop *)
      | M3AST_AS.Opaque_type =>
          RETURN FALSE; (* All opaque types are different *)
      ELSE
        EXIT; (* For other types we always go straight to the full check *)
      END; (* case *)

      (* If we get here we are dealing with the case where 't1' is identical
       to 't2' iff 'm3type1' is identical to 'm3type2'. We get the
       corresponding type specs for the m3types and loop *)
      M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m3type1, t1);
      M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m3type2, t2);
      INC(count);
    END;

    (* assert: neither type is unset and 't1' is not an ordinal *)
    RETURN Compare(t1, t2, NewMap(t1, t2));
  END Identical;


BEGIN
END M3CTypeCompare.
