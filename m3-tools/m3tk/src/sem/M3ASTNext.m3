MODULE M3ASTNext;

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

(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* ToDo: OVERRIDES *)

IMPORT M3AST_LX, M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT M3CTypesMisc, M3CConcTypeSpec;
IMPORT SeqM3AST_AS_Var_decl, SeqM3AST_AS_Fields, SeqM3AST_AS_Formal_param,
    SeqM3AST_AS_Handler, SeqM3AST_AS_Tcase, SeqM3AST_AS_IMPORTED,
    SeqM3AST_AS_Case,  SeqM3AST_AS_RANGE_EXP, SeqM3AST_AS_Qual_used_id,
    SeqM3AST_AS_M3TYPE,
    SeqM3AST_AS_Var_id, SeqM3AST_AS_Field_id, SeqM3AST_AS_FORMAL_ID,
    SeqM3AST_AS_Method, SeqM3AST_AS_Override, SeqM3AST_AS_Import_item; 


(*-------------------------------*) 
(* FLATTENING COMPLEX ITERATIONS *)
(*-------------------------------*)

REVEAL
  IterVar = BRANDED OBJECT
    iterVar_decl: SeqM3AST_AS_Var_decl.Iter;
    iterVar_id: SeqM3AST_AS_Var_id.Iter;
    var_decl: M3AST_AS.Var_decl;
  END;


PROCEDURE NewIterVar(seqVar_decl: SeqM3AST_AS_Var_decl.T): IterVar RAISES {}=
  BEGIN
    RETURN NEW(IterVar,
        iterVar_decl := SeqM3AST_AS_Var_decl.NewIter(seqVar_decl),
        var_decl := NIL);
  END NewIterVar;


PROCEDURE Var(
    VAR (* INOUT *) iter: IterVar;
    VAR (* OUT *) var_id: M3AST_AS.Var_id)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    LOOP
      IF iter.var_decl # NIL AND
          SeqM3AST_AS_Var_id.Next(iter.iterVar_id, var_id) THEN
        RETURN TRUE;
      ELSE
        IF SeqM3AST_AS_Var_decl.Next(iter.iterVar_decl, iter.var_decl) THEN
          iter.iterVar_id :=
              SeqM3AST_AS_Var_id.NewIter(iter.var_decl.as_id_s);
          (* loop *)
        ELSE
          iter := NIL;
          RETURN FALSE;
        END;
      END;
    END; (* loop *)
  END Var;


REVEAL
  IterField = BRANDED OBJECT
    iterFields: SeqM3AST_AS_Fields.Iter := NIL;
    iterField_id: SeqM3AST_AS_Field_id.Iter := NIL;
    fields: M3AST_AS.Fields;
  END; (* record *)


PROCEDURE NewIterField(seqFields: SeqM3AST_AS_Fields.T): IterField RAISES {}=
  BEGIN
    RETURN NEW(IterField,
        iterFields := SeqM3AST_AS_Fields.NewIter(seqFields),
        fields := NIL);
  END NewIterField;


PROCEDURE Field(
    VAR (* INOUT *) iter: IterField;
    VAR (* OUT *) field_id: M3AST_AS.Field_id)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    LOOP
      IF iter.fields # NIL AND
          SeqM3AST_AS_Field_id.Next(iter.iterField_id, field_id) THEN
        RETURN TRUE;
      ELSE
        IF SeqM3AST_AS_Fields.Next(iter.iterFields, iter.fields) THEN
          iter.iterField_id :=
              SeqM3AST_AS_Field_id.NewIter(iter.fields.as_id_s);
          (* loop *)
        ELSE
          iter := NIL;
          RETURN FALSE;
        END; (* if *)
      END; (* if *)
    END; (* loop *)
  END Field;


REVEAL
  IterFormal = BRANDED OBJECT
    iterFormal_param: SeqM3AST_AS_Formal_param.Iter := NIL;
    iterFORMAL_ID: SeqM3AST_AS_FORMAL_ID.Iter := NIL;
    formal_param: M3AST_AS.Formal_param;
  END;


PROCEDURE NewIterFormal(
    seqFormal_param: SeqM3AST_AS_Formal_param.T)
    : IterFormal
    RAISES {}=
  BEGIN
    RETURN NEW(IterFormal,
        iterFormal_param := SeqM3AST_AS_Formal_param.NewIter(seqFormal_param),
        formal_param := NIL);
  END NewIterFormal;


PROCEDURE Formal(
    VAR (* INOUT *) iter: IterFormal;
    VAR (* OUT *) formal_param: M3AST_AS.Formal_param;
    VAR (* OUT *) formal_id: M3AST_AS.FORMAL_ID)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    LOOP
      IF iter.formal_param # NIL AND
          SeqM3AST_AS_FORMAL_ID.Next(iter.iterFORMAL_ID, formal_id) THEN
        formal_param := iter.formal_param;
        RETURN TRUE;
      ELSE
        IF SeqM3AST_AS_Formal_param.Next(
            iter.iterFormal_param, iter.formal_param) THEN
          iter.iterFORMAL_ID :=
              SeqM3AST_AS_FORMAL_ID.NewIter(iter.formal_param.as_id_s);
          (* loop *)
        ELSE
          iter := NIL;
          RETURN FALSE;
        END; (* if *)
      END; (* if *)
    END; (* loop *)
  END Formal;


REVEAL
  IterCaseLabel = BRANDED OBJECT
    iterCase: SeqM3AST_AS_Case.Iter := NIL;
    iterRANGE_EXP: SeqM3AST_AS_RANGE_EXP.Iter := NIL;
    caseArm: M3AST_AS.Case;
  END; (* record *)


PROCEDURE NewIterCaseLabel(
    seqCase: SeqM3AST_AS_Case.T)
    : IterCaseLabel
    RAISES {}=
  BEGIN
    RETURN NEW(IterCaseLabel, iterCase := SeqM3AST_AS_Case.NewIter(seqCase), 
        caseArm := NIL);
  END NewIterCaseLabel;


PROCEDURE CaseLabel(
    VAR (* INOUT *) iter: IterCaseLabel;
    VAR (* OUT *) case: M3AST_AS.Case;
    VAR (* OUT *) label: M3AST_AS.RANGE_EXP)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    LOOP
      IF iter.caseArm # NIL AND
          SeqM3AST_AS_RANGE_EXP.Next(iter.iterRANGE_EXP, label) THEN
        case := iter.caseArm;
        RETURN TRUE;
      ELSE
        IF SeqM3AST_AS_Case.Next(iter.iterCase, iter.caseArm) THEN
          iter.iterRANGE_EXP :=
              SeqM3AST_AS_RANGE_EXP.NewIter(iter.caseArm.as_case_label_s);
          (* loop *)
        ELSE
          iter := NIL;
          RETURN FALSE;
        END; (* if *)
      END; (* if *)
    END; (* loop *)
  END CaseLabel;


REVEAL
  IterHandlerLabel = BRANDED OBJECT
    iterHandler: SeqM3AST_AS_Handler.Iter := NIL;
    iterQual_used_id: SeqM3AST_AS_Qual_used_id.Iter := NIL;
    handler: M3AST_AS.Handler;
  END;


PROCEDURE NewIterHandlerLabel(
    seqHandler: SeqM3AST_AS_Handler.T)
    : IterHandlerLabel
    RAISES {}=
  BEGIN
    RETURN NEW(IterHandlerLabel,
        iterHandler := SeqM3AST_AS_Handler.NewIter(seqHandler),
        handler := NIL);
  END NewIterHandlerLabel;


PROCEDURE HandlerLabel(
    VAR (* INOUT *) iter: IterHandlerLabel;
    VAR (* OUT *) handler: M3AST_AS.Handler;
    VAR (* OUT *) label: M3AST_AS.Qual_used_id)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    LOOP
      IF iter.handler # NIL AND
          SeqM3AST_AS_Qual_used_id.Next(iter.iterQual_used_id, label) THEN
        handler := iter.handler;
        RETURN TRUE;
      ELSE
        IF SeqM3AST_AS_Handler.Next(iter.iterHandler, iter.handler) THEN
          iter.iterQual_used_id :=
              SeqM3AST_AS_Qual_used_id.NewIter(iter.handler.as_qual_id_s);
          (* loop *)
        ELSE
          iter := NIL;
          RETURN FALSE;
        END; (* if *)
      END; (* if *)
    END; (* loop *)
  END HandlerLabel;


REVEAL
  IterTypeCaseLabel = BRANDED OBJECT
    iterTcase: SeqM3AST_AS_Tcase.Iter := NIL;
    iterM3TYPE: SeqM3AST_AS_M3TYPE.Iter := NIL;
    tcase: M3AST_AS.Tcase;
  END;


PROCEDURE NewIterTypeCaseLabel(
    seqTcase: SeqM3AST_AS_Tcase.T)
    : IterTypeCaseLabel
    RAISES {}=
  BEGIN
    RETURN NEW(IterTypeCaseLabel, 
        iterTcase := SeqM3AST_AS_Tcase.NewIter(seqTcase), tcase := NIL);
  END NewIterTypeCaseLabel;


PROCEDURE TypeCaseLabel(
    VAR (* INOUT *) iter: IterTypeCaseLabel;
    VAR (* OUT *) tcase: M3AST_AS.Tcase;
    VAR (* OUT *) label: M3AST_AS.M3TYPE)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    LOOP
      IF iter.tcase # NIL AND
          SeqM3AST_AS_M3TYPE.Next(iter.iterM3TYPE, label) THEN
        tcase := iter.tcase;
        RETURN TRUE;
      ELSE
        IF SeqM3AST_AS_Tcase.Next(iter.iterTcase, iter.tcase) THEN
          iter.iterM3TYPE := SeqM3AST_AS_M3TYPE.NewIter(iter.tcase.as_type_s);
          (* loop *)
        ELSE
          iter := NIL;
          RETURN FALSE;
        END; (* if *)
      END; (* if *)
    END; (* loop *)
  END TypeCaseLabel;


REVEAL
  IterImportedId = BRANDED OBJECT
    iterIMPORTED: SeqM3AST_AS_IMPORTED.Iter := NIL;
    iterImport_item: SeqM3AST_AS_Import_item.Iter := NIL;
    simpleImport: BOOLEAN;
  END;


PROCEDURE NewIterImportedId(i: SeqM3AST_AS_IMPORTED.T): IterImportedId RAISES {}=
  BEGIN
    RETURN NEW(IterImportedId,
      iterIMPORTED := SeqM3AST_AS_IMPORTED.NewIter(i),
      simpleImport := FALSE);
  END NewIterImportedId;


PROCEDURE ImportedId(
    VAR (*inout*) iter: IterImportedId;
    VAR (*out*) used_intf_id: M3AST_AS.Used_interface_id)
    : BOOLEAN
    RAISES {}=
  VAR
    import_item: M3AST_AS.Import_item;
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    LOOP
      IF iter.iterImport_item # NIL AND
          SeqM3AST_AS_Import_item.Next(
              iter.iterImport_item, import_item) THEN
        used_intf_id := import_item.as_intf_id;
        RETURN TRUE;
      ELSE
        VAR
          imported: M3AST_AS.IMPORTED;
        BEGIN
          IF SeqM3AST_AS_IMPORTED.Next(iter.iterIMPORTED, imported) THEN
            TYPECASE imported OF <*NOWARN*>
            | M3AST_AS.From_import(from) =>
                used_intf_id := from.as_intf_id;
                iter.iterImport_item := NIL;
                RETURN TRUE;
            | M3AST_AS.Simple_import(simple) =>
                iter.iterImport_item :=
                    SeqM3AST_AS_Import_item.NewIter(simple.as_import_item_s);
                (* loop *)
            END; (* typecase *)
          ELSE
            iter := NIL;
            RETURN FALSE;
          END; (* if *)
        END; (* block *)
      END; (* if *)
    END; (* loop *)
  END ImportedId;


(*--------------*)
(* OBJECT TYPES *)
(*--------------*)

PROCEDURE SimpleSuperType(
    type: M3AST_AS.Object_type;
    VAR (* OUT *) superType: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF type.as_ancestor = NIL THEN RETURN FALSE END;
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(type.as_ancestor, superType);
    RETURN TRUE;
  END SimpleSuperType;


PROCEDURE SuperType(
    object: M3AST_AS.Object_type;
    VAR (* OUT *) superType: M3AST_AS.Object_type)
    : BOOLEAN
    RAISES {}=
  VAR
    ts: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF SimpleSuperType(object, ts) THEN
      LOOP
        IF ts = NIL THEN RETURN FALSE
        ELSIF ISTYPE(ts, M3AST_AS.Object_type) THEN
          superType := ts;
          RETURN TRUE
        ELSIF ISTYPE(ts, M3AST_AS.Opaque_type) THEN
          ts := M3CConcTypeSpec.CurrentReveal(ts);
          (* loop *)
        ELSE
          RETURN FALSE;
        END;
      END;
    ELSE
      RETURN FALSE;
    END; (* if *)
  END SuperType;


REVEAL
  IterObjectField = BRANDED OBJECT
    iterFields: IterField := NIL; 
    baseType, currentSuperType: M3AST_AS.Object_type;
  END;


PROCEDURE NewIterObjectField(
    o: M3AST_AS.Object_type)
    : IterObjectField
    RAISES {}=
  VAR
    st: M3AST_AS.Object_type;
  BEGIN
    st := o;
    WHILE SuperType(st, st) DO (* nothing *) END;
    RETURN NEW(IterObjectField,
        iterFields := NewIterField(st.as_fields_s),
        baseType := o,
        currentSuperType := st);
  END NewIterObjectField;


PROCEDURE ObjectField(
    VAR (* INOUT *) iter: IterObjectField;
    VAR (* OUT *) field_id: M3AST_AS.Field_id)
    : BOOLEAN
    RAISES {}=
  VAR
    st, stPrev: M3AST_AS.Object_type;
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    LOOP
      IF Field(iter.iterFields, field_id) THEN
        RETURN TRUE;
      ELSIF iter.baseType = iter.currentSuperType THEN
        RETURN FALSE;
      ELSE
        (* find PREVIOUS super type and its fields *)
        stPrev := iter.baseType;
        WHILE SuperType(stPrev, st) AND (st # iter.currentSuperType) DO
          stPrev := st;
        END;
        iter.iterFields := NewIterField(stPrev.as_fields_s);
        iter.currentSuperType := stPrev;
      END;
    END; (* loop *)
  END ObjectField;


REVEAL
  IterObjectMethod = BRANDED OBJECT
    iterMethods: SeqM3AST_AS_Method.Iter := NIL;
    iterOverrides: SeqM3AST_AS_Override.Iter := NIL;
    baseType, currentSuperType: M3AST_AS.Object_type;
  END;


PROCEDURE NewIterObjectMethod(
    o: M3AST_AS.Object_type)
    : IterObjectMethod
    RAISES {}=
  VAR
    st: M3AST_AS.Object_type;
  BEGIN
    st := o;
    WHILE SuperType(st, st) DO (* nothing *) END;
    RETURN NEW(IterObjectMethod,
        iterMethods := SeqM3AST_AS_Method.NewIter(st.as_method_s),
        iterOverrides := SeqM3AST_AS_Override.NewIter(st.as_override_s),
        baseType := o,
        currentSuperType := st);
  END NewIterObjectMethod;


PROCEDURE ObjectMethod(
    VAR (* INOUT *) iter: IterObjectMethod;
    VAR (* OUT *) method: M3AST_AS.METHOD_OVERRIDE;
    VAR (* OUT *) overrides: BOOLEAN)
    : BOOLEAN
    RAISES {}=
  VAR
    st, stPrev: M3AST_AS.Object_type;
    method_l: M3AST_AS.Method;  override_l: M3AST_AS.Override;
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    LOOP
      IF SeqM3AST_AS_Method.Next(iter.iterMethods, method_l) THEN
        method := method_l; overrides := FALSE;
        RETURN TRUE;
      ELSIF SeqM3AST_AS_Override.Next(iter.iterOverrides, override_l) THEN
        method := override_l; overrides := TRUE;
        RETURN TRUE;
      ELSIF iter.baseType = iter.currentSuperType THEN
        RETURN FALSE;
      ELSE
        (* find PREVIOUS super type and its fields *)
        stPrev := iter.baseType;
        WHILE SuperType(stPrev, st) AND (st # iter.currentSuperType) DO
          stPrev := st;
        END;
        iter.iterMethods := SeqM3AST_AS_Method.NewIter(stPrev.as_method_s);
        iter.iterOverrides := SeqM3AST_AS_Override.NewIter(stPrev.as_override_s);
        iter.currentSuperType := stPrev;
      END;
    END; (* loop *)
  END ObjectMethod;


REVEAL
  IterFieldOrMethod = BRANDED OBJECT
    iterFields: IterField := NIL;
    iterMethods: SeqM3AST_AS_Method.Iter := NIL;
    obj: M3AST_AS.Object_type;
  END;


PROCEDURE NewIterFieldOrMethod(
    o: M3AST_AS.Object_type)
    : IterFieldOrMethod
    RAISES {}=
  BEGIN
    RETURN NEW(IterFieldOrMethod, iterFields := NewIterField(o.as_fields_s), 
        obj := o);
  END NewIterFieldOrMethod;


PROCEDURE FieldOrMethod(
    VAR iter: IterFieldOrMethod;
    VAR field: M3AST_AS.Field_id;
    VAR method: M3AST_AS.Method;
    VAR symrep: M3AST_LX.Symbol_rep)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF iter = NIL THEN RETURN FALSE END;
    LOOP
      IF iter.iterFields # NIL THEN
        IF Field(iter.iterFields, field) THEN
          method := NIL;
          symrep := field.lx_symrep;
          RETURN TRUE;
        ELSE
          iter.iterMethods :=
              SeqM3AST_AS_Method.NewIter(iter.obj.as_method_s);
          iter.iterFields := NIL;
          (* loop and move on to methods *)
        END;
      ELSE
        WHILE SeqM3AST_AS_Method.Next(iter.iterMethods, method) DO
          IF method.as_type # NIL THEN
            field := NIL;
            symrep := method.as_id.lx_symrep;
            RETURN TRUE;
          END;
        END;
        (* If we get here we have run out of fields and methods *)
        IF SuperType(iter.obj, iter.obj) THEN
          iter.iterFields := NewIterField(iter.obj.as_fields_s);
          (* loop and move on to fields of super type *)
        ELSE
          RETURN FALSE;
        END;
      END; (* if *)
    END; (* loop *)
  END FieldOrMethod;


(*-------------*)
(* ARRAY TYPES *)
(*-------------*)

PROCEDURE Array(
    array: M3AST_AS.Array_type;
    VAR (* OUT *) elementType: M3AST_SM.TYPE_SPEC_UNSET;
    VAR (* OUT *) openArray: BOOLEAN;
    VAR (* OUT *) indexType: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN (* elementtype is an array i.e. array is multi-dimensional *)
    RAISES {}=
  VAR
    iter: SeqM3AST_AS_M3TYPE.Iter;
    it: M3AST_AS.M3TYPE;
  BEGIN
    array := array.sm_norm_type;

    iter := SeqM3AST_AS_M3TYPE.NewIter(array.as_indextype_s);
    IF SeqM3AST_AS_M3TYPE.Next(iter, it) THEN
      M3CTypesMisc.GetTYPE_SPECFromM3TYPE(it, indexType);
      openArray := FALSE;
    ELSE
      openArray := TRUE;
    END;

    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(array.as_elementtype, elementType);
    RETURN elementType # NIL AND ISTYPE(elementType, M3AST_AS.Array_type);
  END Array;


BEGIN
END M3ASTNext.
