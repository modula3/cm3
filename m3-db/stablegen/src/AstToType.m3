(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Created by Susan Owicki                                     *)
(* Last modified on Tue Sep 27 14:31:18 PDT 1994 by weich      *)

(* Module contains procedures for converting AST types into
   "Type.T"'s. The main procedure is "Convert()" *)

MODULE AstToType;

IMPORT Atom, PropertyV, RefRefTbl, FRefRefTbl;
IMPORT M3CId, Type, Value;
IMPORT M3AST_AS, M3AST_AS_F, M3AST_SM, M3AST_SM_F,
       M3AST_TM_F, M3ASTNext, M3AST_TL_F, M3AST_LX,
       M3CConcTypeSpec, M3CStdTypes;
IMPORT SeqM3AST_AS_Enum_id, SeqM3AST_AS_Fields,
       SeqM3AST_AS_Field_id, SeqM3AST_AS_Method,
       SeqM3AST_AS_Qual_used_id, M3CTypesMisc;
IMPORT M3CBackEnd, M3CBackEnd_C;


(* \subsection{Procedure Convert}
   Allocate the cache-table with already parsed types specifications,
   initialize it and call "ProcessM3Type()".
*)
PROCEDURE Convert(m3type: M3AST_AS.M3TYPE): Type.T =
  VAR
    astMap:= NEW(FRefRefTbl.Default).init();
  BEGIN
    InitAstTable(astMap);
    RETURN ProcessM3Type(astMap, m3type);
  END Convert;

PROCEDURE InitAstTable (astTable: RefRefTbl.T) =
  BEGIN
    EVAL astTable.put(M3CStdTypes.Integer(), Type.integer);
    EVAL astTable.put(M3CStdTypes.Real(), Type.real);
    EVAL astTable.put(M3CStdTypes.LongReal(), Type.longreal);
    EVAL astTable.put(M3CStdTypes.Extended(), Type.extended);
    EVAL astTable.put(M3CStdTypes.Null(), Type.null);
    EVAL astTable.put(M3CStdTypes.RefAny(), Type.refany);
    EVAL astTable.put(M3CStdTypes.Address(), Type.address);
    EVAL astTable.put(M3CStdTypes.Root(), Type.root);
    EVAL astTable.put(M3CStdTypes.Untraced_Root(), Type.untracedRoot);
    EVAL astTable.put(M3CStdTypes.Char(), Type.char);
    EVAL astTable.put(M3CStdTypes.Text(), Type.text);
    EVAL astTable.put(M3CStdTypes.Cardinal(), Type.cardinal);
    EVAL astTable.put(M3CStdTypes.Boolean(), Type.boolean);
    EVAL astTable.put(M3CStdTypes.Mutex(), Type.mutex);
  END InitAstTable;


(* \subsection{Conversion from M3AST type to Type.T}
   The procedures "ProcessM3Type()" and "ProcessTypeSpec()" recursivly
   descend a M3AST-type specification node and generate a "Type.T" out
   of it. The procedures "ProcessObject()", "ProcessFields()",
   "ProcessExp()" and "ProcessMethods()" are auxilary procedures for
   that purpose.  All of these have a parameter "astTable" which is
   used to save all already parsed types. "AddToTable()" is called to
   add another entry to that table. This procedure also looks up the
   property list of the node to gather a name for the type (see
   "TypeNames.Preprocess()").
*)

PROCEDURE ProcessM3Type (astMap: RefRefTbl.T;
                         m3type: M3AST_AS.M3TYPE): Type.T =
  VAR ts: M3AST_AS.TYPE_SPEC;
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m3type, ts);
    RETURN ProcessTypeSpec(astMap, ts);
  END ProcessM3Type;

PROCEDURE ProcessTypeSpec (astMap: RefRefTbl.T;
                           ts    : M3AST_AS.TYPE_SPEC): Type.T =
  VAR
    r: REFANY;
    t: Type.T;
  BEGIN
    IF astMap.get(ts, r) THEN
      RETURN NARROW(r, Type.T);
    ELSE
      TYPECASE ts OF
      | M3AST_AS.Real_type => t := Type.real;
      | M3AST_AS.LongReal_type => t := Type.longreal;
      | M3AST_AS.Extended_type => t := Type.extended;
      | M3AST_AS.Integer_type => t := Type.integer;
      | M3AST_AS.Null_type => t := Type.null;
      | M3AST_AS.RefAny_type => t := Type.refany;
      | M3AST_AS.Address_type => t := Type.address;
      | M3AST_AS.Root_type (rt) =>
          TYPECASE rt.as_trace_mode OF
          | NULL => t := Type.root
          ELSE
            t := Type.untracedRoot
          END;
      | M3AST_AS.Packed_type (pt) =>
          t := NEW(Type.Packed,
                   size := NARROW(pt.as_exp.sm_exp_value,
                                  M3CBackEnd_C.Integer_value).sm_value,
                   base := ProcessM3Type(astMap, pt.as_type));
      | M3AST_AS.Array_type (at) =>
          VAR
            ASTindexType: M3AST_SM.TYPE_SPEC_UNSET;
            eltTypeSpec : M3AST_SM.TYPE_SPEC_UNSET;
            openArray   : BOOLEAN;
          BEGIN
            EVAL M3ASTNext.Array(at, eltTypeSpec, openArray, ASTindexType);
            IF openArray THEN
              t := NEW(Type.OpenArray, index := NIL,
                       element := ProcessTypeSpec(astMap, eltTypeSpec));
              WITH openA    = NARROW(t, Type.OpenArray),
                   refArray = NEW(Type.Ref, traced := TRUE, target := t) DO
                openA.refArray := refArray;
                TYPECASE openA.element OF
                | Type.OpenArray (element) =>
                    openA.openDimensions := element.openDimensions + 1;
                ELSE
                  openA.openDimensions := 1;
                END;
              END;
            ELSE
              t := NEW(Type.Array,
                       index := ProcessM3Type(astMap, ASTindexType),
                       element := ProcessTypeSpec(astMap, eltTypeSpec));
            END;
          END;
      | M3AST_AS.Enumeration_type (enum) =>
          VAR
            enumt := NEW(
                       Type.UserDefined, elts := NEW(REF ARRAY OF Atom.T,
                                                     enum.sm_num_elements));
            iter := SeqM3AST_AS_Enum_id.NewIter(enum.as_id_s);
            elem: M3AST_AS.Enum_id;
          BEGIN
            FOR i := 1 TO enum.sm_num_elements DO
              EVAL SeqM3AST_AS_Enum_id.Next(iter, elem);
              enumt.elts[i - 1] :=
                Atom.FromText(M3CId.ToText(elem.lx_symrep));
            END;
            t := enumt;
          END;
      | M3AST_AS.Set_type (set) =>
          t := NEW(Type.Set, range := ProcessM3Type(astMap, set.as_type));
      | M3AST_AS.Subrange_type (sub) =>
          VAR
            e1, e2  : M3AST_AS.EXP;
            i1, i2  : INTEGER;
            baseType: Type.T;
          BEGIN
            baseType := ProcessTypeSpec(astMap, sub.sm_base_type_spec);
            e1 := NARROW(sub.as_range, M3AST_AS.Range).as_exp1;
            e2 := NARROW(sub.as_range, M3AST_AS.Range).as_exp2;
            EVAL M3CBackEnd.Ord(e1.sm_exp_value, i1);
            EVAL M3CBackEnd.Ord(e2.sm_exp_value, i2);
            t := NEW(Type.Subrange, base := baseType,
                     min := NEW(Value.Ordinal, ord := i1),
                     max := NEW(Value.Ordinal, ord := i2));
          END
      | M3AST_AS.Record_type (rec) =>
          t :=
            NEW(Type.Record, fields := ProcessFields(astMap, rec.as_fields_s));
      | M3AST_AS.BRANDED_TYPE (bt) =>
          VAR
            brandName: Atom.T  := NIL;
            branded            := FALSE;
            trace    : BOOLEAN;
          BEGIN
            IF bt.as_brand # NIL THEN
              IF bt.as_brand.as_exp # NIL THEN
                brandName :=
                  Atom.FromText(NARROW(bt.as_brand.as_exp.sm_exp_value,
                                       M3CBackEnd_C.Text_value).sm_value);
              END;
              branded := TRUE
            END;
            TYPECASE bt OF
            | M3AST_AS.Ref_type (ref) =>
                TYPECASE ref.as_trace_mode OF
                | NULL => trace := TRUE;
                ELSE
                  trace := FALSE;
                END;
                t := NEW(Type.Ref, traced := trace, branded := branded,
                         brand := brandName);
                AddToTable(astMap, ts, t);
                NARROW(t, Type.Ref).target :=
                  ProcessM3Type(astMap, ref.as_type);
            | M3AST_AS.Object_type (ob) =>
                t := ProcessObject(astMap, ob, branded, brandName, trace);
            ELSE
              <*ASSERT FALSE*>
            END;
          END;
      | M3AST_AS.Opaque_type (o) =>
          IF o.sm_concrete_type_spec = NIL THEN
            WITH revSuperTs = M3CConcTypeSpec.CurrentReveal(o),
                 revSuperType = NARROW(ProcessTypeSpec(astMap, revSuperTs),
                                       Type.Reference) DO
              t := NEW(Type.Opaque, revealedSuperType := revSuperType);
            END;
          ELSE
            WITH revTs = o.sm_concrete_type_spec DO
              t := ProcessTypeSpec(astMap, revTs);
              WITH tt = NARROW(t, Type.Object) DO
                tt.revIntf :=
                  Atom.FromText(M3CId.ToText(revTs.tmp_unit_id.lx_symrep));
              END;
            END;
          END;
      | M3AST_AS.Procedure_type (proc) =>
          VAR
            formals : REF ARRAY OF Type.Formal;
            nFormals: INTEGER                  := 0;
            iter := M3ASTNext.NewIterFormal(proc.as_formal_param_s);
            formalParam: M3AST_AS.Formal_param;
            formalId   : M3AST_AS.FORMAL_ID;
            signature  : Type.Signature;
          BEGIN
            WHILE M3ASTNext.Formal(iter, formalParam, formalId) DO
              INC(nFormals)
            END;
            formals := NEW(REF ARRAY OF Type.Formal, nFormals);
            iter := M3ASTNext.NewIterFormal(proc.as_formal_param_s);
            FOR i := 0 TO nFormals - 1 DO
              EVAL M3ASTNext.Formal(iter, formalParam, formalId);
              formals[i] := NEW(Type.Formal);
              formals[i].name :=
                Atom.FromText(M3CId.ToText(formalId.lx_symrep));
              formals[i].type :=
                ProcessM3Type(astMap, formalId.sm_type_spec);
              TYPECASE formalId OF
                M3AST_AS.F_Value_id => formals[i].mode := Type.Mode.Value;
              | M3AST_AS.F_Var_id => formals[i].mode := Type.Mode.Var;
              | M3AST_AS.F_Readonly_id =>
                  formals[i].mode := Type.Mode.Readonly;
              ELSE
                <*ASSERT FALSE*>
              END;
              formals[i].outOnly := FALSE;
              (* Change to depend on <*OUTPUT*> *)
            END;
            signature.formals := formals;
            IF proc.as_result_type # NIL THEN
              signature.result := ProcessM3Type(astMap, proc.as_result_type);
            END;
            IF proc.as_raises = NIL THEN
              signature.raises := NEW(REF ARRAY OF Type.Exception, 0)
            ELSE
              TYPECASE proc.as_raises OF
                M3AST_AS.Raisees_some (r) =>
                  VAR
                    iter := SeqM3AST_AS_Qual_used_id.NewIter(
                              r.as_raisees_s);
                    nRaises := SeqM3AST_AS_Qual_used_id.Length(
                                 r.as_raisees_s);
                    raisee : M3AST_AS.Qual_used_id;
                    arg    : M3AST_AS.Exc_id;
                    argType: Type.T;
                  BEGIN
                    signature.raises :=
                      NEW(REF ARRAY OF Type.Exception, nRaises);
                    FOR i := 0 TO nRaises - 1 DO
                      EVAL SeqM3AST_AS_Qual_used_id.Next(iter, raisee);
                      signature.raises[i] := NEW(Type.Exception);
                      signature.raises[i].qid := NEW(Type.Qid);
                      WITH qid = signature.raises[i].qid DO
                        qid.intf :=
                          Atom.FromText(
                            M3CId.ToText(
                              raisee.as_id.sm_def.tmp_unit_id.lx_symrep));
                        qid.item :=
                          Atom.FromText(
                            M3CId.ToText(raisee.as_id.lx_symrep));
                      END;
                      IF raisee.as_id.sm_def = NIL THEN
                        signature.raises[i].arg := NIL
                      ELSE
                        arg :=
                          NARROW(raisee.as_id.sm_def, M3AST_AS.Exc_id);
                        IF arg.tmp_type = NIL THEN
                          signature.raises[i].arg := NIL;
                        ELSE
                          argType := ProcessM3Type(astMap, arg.tmp_type);
                          signature.raises[i].arg := argType;
                        END;
                      END;
                    END;
                  END;
              | M3AST_AS.Raisees_any =>
              ELSE
                signature.raises := NEW(REF ARRAY OF Type.Exception, 0)
              END;
            END;
            t := NEW(Type.Procedure, sig := signature);
          END;
      ELSE
      END;
    END;
    AddToTable(astMap, ts, t);
    RETURN t;
  END ProcessTypeSpec;

PROCEDURE ProcessObject (astMap : RefRefTbl.T;
                         o      : M3AST_AS.Object_type;
                         branded: BOOLEAN;
                         brand  : Atom.T;
                         traced : BOOLEAN               ): Type.T =
  VAR t := NEW(Type.Object);
  BEGIN
    AddToTable(astMap, o, t);
    t.branded := branded;
    t.brand := brand;
    t.traced := traced;
    IF o.as_ancestor # NIL THEN
      t.super := ProcessM3Type(astMap, o.as_ancestor);
    END;
    t.fields := ProcessFields(astMap, o.as_fields_s);
    t.methods := ProcessMethods(astMap, o.as_method_s);
    RETURN t;
  END ProcessObject;

PROCEDURE ProcessFields (astMap: RefRefTbl.T; f: SeqM3AST_AS_Fields.T):
  REF ARRAY OF Type.Field =
  VAR
    nFields  : INTEGER                   := 0;
    fields   : REF ARRAY OF Type.Field;
    iter                                 := M3ASTNext.NewIterField(f);
    iterItems                            := SeqM3AST_AS_Fields.NewIter(f);
    iterIds  : SeqM3AST_AS_Field_id.Iter;
    astFields: M3AST_AS.Fields;
    fieldId  : M3AST_AS.Field_id;
    j        : INTEGER                   := 0;
  BEGIN
    WHILE M3ASTNext.Field(iter, fieldId) DO INC(nFields) END;
    fields := NEW(REF ARRAY OF Type.Field, nFields);
    WHILE SeqM3AST_AS_Fields.Next(iterItems, astFields) DO
      iterIds := SeqM3AST_AS_Field_id.NewIter(astFields.as_id_s);
      WHILE SeqM3AST_AS_Field_id.Next(iterIds, fieldId) DO
        fields[j] := NEW(Type.Field);
        fields[j].name := Atom.FromText(M3CId.ToText(fieldId.lx_symrep));
        IF astFields.as_type = NIL THEN
          fields[j].type := ProcessTypeSpec(astMap, fieldId.sm_type_spec)
        ELSE
          fields[j].type := ProcessM3Type(astMap, astFields.as_type);
        END;
        IF (*fieldId.vINIT_ID.sm_init_exp*) astFields.as_default # NIL THEN
          fields[j].default := ProcessExp(astFields.as_default);
        END;
        INC(j);
      END;
    END;
    RETURN fields;
  END ProcessFields;

PROCEDURE ProcessMethods (astMap: RefRefTbl.T; m: SeqM3AST_AS_Method.T):
  REF ARRAY OF Type.Method =
  VAR
    nMethods                            := SeqM3AST_AS_Method.Length(m);
    methods  : REF ARRAY OF Type.Method;
    iter                                := SeqM3AST_AS_Method.NewIter(m);
    astMethod: M3AST_AS.Method;
  BEGIN
    methods := NEW(REF ARRAY OF Type.Method, nMethods);
    FOR i := 0 TO nMethods - 1 DO
      EVAL SeqM3AST_AS_Method.Next(iter, astMethod);
      methods[i] := NEW(Type.Method);
      methods[i].name :=
        Atom.FromText(M3CId.ToText(astMethod.as_id.lx_symrep));
      methods[i].sig :=
        NARROW(ProcessTypeSpec(astMap, astMethod.as_type),
               Type.Procedure).sig;
    END;
    RETURN methods;
  END ProcessMethods;

PROCEDURE ProcessExp(exp: M3AST_AS.EXP): Value.T =
  BEGIN
    TYPECASE exp.sm_exp_value OF
      |  M3CBackEnd_C.Integer_value (int) =>
           RETURN NEW(Value.Ordinal, ord := int.sm_value)
      |  M3CBackEnd_C.Text_value (txt) => 
           RETURN NEW(Value.Txt, val := txt.sm_value)
      |  M3CBackEnd_C.Real_value (real) => 
           RETURN NEW(Value.Float, val := real.sm_value);
      |  M3CBackEnd_C.LongReal_value (lreal) => 
           RETURN NEW(Value.LongFloat, val := lreal.sm_value);
      |  M3CBackEnd_C.Extended_value (ereal) => 
           RETURN NEW(Value.Extended, val := ereal.sm_value);
      (*
      |  M3CBackEnd_C.Set_constructor_value => 
      |  M3CBackEnd_C.Array_or_record_constructor_value => 
      |  M3CBackEnd_C.Proc_value => 
      *)
      ELSE
        <*ASSERT FALSE*>
    END;
  END ProcessExp;

PROCEDURE AddToTable (astMap: RefRefTbl.T;
                      ts    : M3AST_AS.TYPE_SPEC;
                      t     : Type.T              ) =
  BEGIN
    EVAL astMap.put(ts, t);
    IF t.name = NIL THEN
      WITH symrep = NARROW(PropertyV.Get(
                             ts.tl_pset,
                             TYPECODE(M3AST_LX.Symbol_rep)),
                           M3AST_LX.Symbol_rep) DO
        IF symrep # NIL THEN
          t.name := NEW(Type.Qid);
          t.name.intf :=
            Atom.FromText(
              M3CId.ToText(ts.tmp_unit_id.lx_symrep));
          t.name.item :=
            Atom.FromText(M3CId.ToText(symrep));
        END;
      END;
    END;
  END AddToTable;

BEGIN
END AstToType.

