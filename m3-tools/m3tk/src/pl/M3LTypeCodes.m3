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

MODULE M3LTypeCodes;

IMPORT Text, Word, PropertyV, TextWr, Wr, IntIntTbl;

IMPORT AST, M3AST_AS, M3AST_SM, M3CUnit, ASTWalk;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_FE_F, M3AST_TM_F, M3AST_TL_F;

IMPORT SeqM3AST_AS_TYPE_SPEC;
 
IMPORT M3Context;
IMPORT M3Assert;

IMPORT M3LTypeEquiv, M3CTypesMisc;
IMPORT M3LTypeSpecToText, M3LFingerPrint;

(* We keep a list of opaque types so that we can quickly set their
   typecode to that of the concrete counterpart.
*)

IMPORT Thread;
<*FATAL Wr.Failure, Thread.Alerted*>

TYPE 
  CClosure = M3Context.Closure OBJECT
    componentTypes: IntIntTbl.T;
    allTypes := FALSE;
  OVERRIDES
    callback := SetUnit;
  END;

  WClosure = ASTWalk.Closure OBJECT
    ccl: CClosure;
  OVERRIDES
    callback := AddTypeSpec;
  END;

CONST
  NoGenericDefs = M3CUnit.TypeSet{
      M3CUnit.Type.Interface, M3CUnit.Type.Interface_gen_ins,
      M3CUnit.Type.Module, M3CUnit.Type.Module_gen_ins};

PROCEDURE AddArrayType(array: M3AST_AS.Array_type) RAISES {}=
  VAR
    a: M3AST_AS.Array_type;
    elem: M3AST_SM.TYPE_SPEC_UNSET := NIL;
  BEGIN
    a := array.sm_norm_type;
    IF array # a THEN
      M3LTypeEquiv.Add(array);
    END;
    LOOP
      (* on multiple dimension arrays, a is elem.sm_norm_type *)
      IF a # elem THEN
        M3LTypeEquiv.Add(a);
      END;
      M3CTypesMisc.GetTYPE_SPECFromM3TYPE(a.as_elementtype, elem);
      M3LTypeEquiv.Add(elem);
      IF ISTYPE(elem, M3AST_AS.Array_type) THEN
        a := NARROW(elem, M3AST_AS.Array_type).sm_norm_type;
      ELSE
        EXIT;
      END;
    END;
  END AddArrayType;


PROCEDURE AddComponentTypeSpec(
    wcl: WClosure; t: M3AST_SM.TYPE_SPEC_UNSET) RAISES {}=
  VAR
    void: INTEGER;
    unique_id := GetHackUniqueId(t);
  BEGIN
    IF M3CTypesMisc.IsRef(t) OR
        wcl.ccl.componentTypes.get(unique_id, void) THEN
      (* If is a reference type it will be handled when 'SetUnit' is
       called on the interface in which it is declared. If it is noted
       in the 'componentTypes' table it has already been dealt with *)
    ELSE
      EVAL wcl.ccl.componentTypes.put(unique_id, 0);
      ASTWalk.VisitNodes(t, wcl); <*NOWARN*>
    END;
  END AddComponentTypeSpec;


PROCEDURE AddTypeSpec(
    wcl: WClosure; any: AST.NODE; <*UNUSED*> vm: ASTWalk.VisitMode) RAISES {}=
  BEGIN
    TYPECASE any OF
    | M3AST_AS.Fields(fields) =>
        IF fields.as_type = NIL THEN
          AddComponentTypeSpec(wcl, fields.as_default.sm_exp_type_spec);
        END;
    | M3AST_AS.Formal_param(formal) =>
        IF formal.as_formal_type = NIL THEN
          AddComponentTypeSpec(wcl, formal.as_default.sm_exp_type_spec);
        END;
    | M3AST_AS.Array_type(array_type) =>
        AddArrayType(array_type);
    | M3AST_AS.Subrange_type(set_type) =>
        M3LTypeEquiv.Add(set_type);
        M3LTypeEquiv.Add(set_type.sm_base_type_spec);
    | M3AST_SM.Any_type, M3AST_SM.Type_type =>
        (* do nothing with these *)
    | M3AST_AS.TYPE_SPEC =>
        M3LTypeEquiv.Add(any);
    | M3AST_AS.Named_type(named_type) =>
        AddComponentTypeSpec(wcl, named_type.sm_type_spec);
    | M3AST_AS.EXP(e) =>
        IF NOT ISTYPE(e.sm_exp_type_spec, M3AST_SM.Void_type) THEN
          M3LTypeEquiv.Add(e.sm_exp_type_spec)
        END (* IF *)
    ELSE
      (* nothing *)
    END; (* case *)
  END AddTypeSpec;


PROCEDURE Set(
    c: M3Context.T;
    allTypes := FALSE;
    genTexts := TRUE;
    genFingerPrints := TRUE;
    ): T RAISES {}=
  VAR 
    ccl := NEW(CClosure, componentTypes := NEW(IntIntTbl.Default).init(),
               allTypes := allTypes);
    res: T;
  BEGIN
    HackUniqueIds(c);
    M3LTypeEquiv.Initialize();
    M3Context.ApplyToSet(c, ccl, NoGenericDefs, TRUE); <*NOWARN*>
    res.types := M3LTypeEquiv.Partition();
    IF genTexts THEN
      (* must be after partition for sm_type_codes *)
      M3LTypeSpecToText.Initialize(); 
      VAR
        textStream: TextWr.T;
      BEGIN
        res.texts := NEW(REF ARRAY OF TEXT, NUMBER(res.types^));
        FOR i := 0 TO LAST(res.texts^) DO
          textStream := TextWr.New();
          M3LTypeSpecToText.TypeSpec(textStream, res.types[i]);
          res.texts[i] := TextWr.ToText(textStream);
          Wr.Close(textStream);
        END; (* for *)
      END;
    END;
    IF genTexts AND genFingerPrints THEN
      res.fingerprints := M3LFingerPrint.Generate(res.texts);
    END; (* if *)
    RETURN res;
  END Set;


PROCEDURE SetUnit(ccl: CClosure; ut: M3CUnit.Type; <*UNUSED*> name: Text.T; 
    cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR 
    wcl := NEW(WClosure, ccl := ccl);
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    IF cu = NIL OR NOT(M3CUnit.State.SemChecked IN cu.fe_status) OR
                       M3CUnit.State.SErrors IN cu.fe_status THEN
      (* bullet proofing *)
      RETURN
    END;
 
    IF ccl.allTypes THEN
      ASTWalk.VisitNodes(cu, wcl) <*NOWARN*>
    ELSE
      VAR
        iter := SeqM3AST_AS_TYPE_SPEC.NewIter(
                    NARROW(cu.as_root, M3AST_AS.UNIT_NORMAL).sm_type_spec_s);
        type_spec: M3AST_AS.TYPE_SPEC;
      BEGIN
        (* visit all the TYPE_SPECs hung off the sm_type_spec_s list *)
        WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter, type_spec) DO
          ASTWalk.VisitNodes(type_spec, wcl); <*NOWARN*>
        END; (* while *)
      END;
    END;
  END SetUnit;

(* Since we cant use HashRef, we need a way to uniqely identify (with
an integer) a TYPE_SPEC node. This is a temporary lash up. *)

TYPE
  HackClosure = M3Context.Closure OBJECT
    id := 0;
  OVERRIDES
    callback := HackUniqueIdForUnit;
  END;

PROCEDURE HackUniqueIds(c: M3Context.T) RAISES {}=
  BEGIN
    M3Context.ApplyToSet(c, NEW(HackClosure), NoGenericDefs, TRUE); <*NOWARN*>
  END HackUniqueIds;

PROCEDURE HackUniqueIdForUnit(cl: HackClosure; ut: M3CUnit.Type;
                              <*UNUSED*> name: Text.T; 
                              cu: M3AST_AS.Compilation_Unit) RAISES {}=
  VAR
    ri := NEW(REF INTEGER);
  BEGIN
    cu := M3CUnit.ToGenIns(cu, ut);
    IF cu = NIL THEN RETURN END;
    ri^ := cl.id; INC(cl.id);
    PropertyV.Put(cu.tl_pset, ri);
  END HackUniqueIdForUnit;

PROCEDURE GetHackUniqueId(t: M3AST_AS.TYPE_SPEC): INTEGER RAISES {}=
  VAR
    x: INTEGER;
  BEGIN
    IF t.tmp_unit_id = NIL THEN x := 0; (* built-ins *)
    ELSE
      VAR
        cu := t.tmp_unit_id.sm_spec.sm_comp_unit;
        ri: REF INTEGER := PropertyV.Get(cu.tl_pset, TYPECODE(REF INTEGER));
      BEGIN
        M3Assert.Check(t.lx_srcpos # 0);
        x := ri^;
      END;
    END;
    RETURN Word.Or(Word.Shift(x, 16), t.lx_srcpos);
  END GetHackUniqueId;

BEGIN
END M3LTypeCodes.
