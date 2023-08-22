(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE AstToType;

IMPORT Atom, PropertyV, RefRefTbl, FRefRefTbl, TextRefTbl;
IMPORT M3AST_AS, M3CId;
IMPORT AstToVal, Type, Value;
IMPORT M3AST_AS_F, M3AST_SM, M3AST_SM_F, M3AST_TM_F, M3ASTNext,
       M3AST_TL_F, M3AST_LX, 
       M3Context, M3CConcTypeSpec, M3CStdTypes;
IMPORT SeqM3AST_AS_Enum_id, SeqM3AST_AS_Fields,
       SeqM3AST_AS_Field_id, SeqM3AST_AS_Method, 
       SeqM3AST_AS_Qual_used_id; (*SeqM3AST_AS_TYPE_SPEC,*)
IMPORT M3CBackEnd_C, M3CTypesMisc;
IMPORT Wr, Stdio;

<*FATAL ANY*>

REVEAL

   Handle = Public BRANDED OBJECT
     astMap: RefRefTbl.T;
     nameMap: TextRefTbl.T;
   END;

PROCEDURE Error(m : TEXT) =
  BEGIN
    Wr.PutText(Stdio.stderr, "AstToType error:" & m & "\n");
  END Error;

PROCEDURE NewHandle (context: M3Context.T): Handle =
  VAR
    astTable: RefRefTbl.T;
    h       : Handle;
  BEGIN
    astTable := NEW(FRefRefTbl.Default).init();
    InitAstTable(astTable);
    h := NEW(Handle, context := context, astMap := astTable,
             nameMap := NEW(TextRefTbl.Default).init());
    RETURN h;
  END NewHandle;

PROCEDURE InitAstTable (astTable: RefRefTbl.T) =
  BEGIN
    EVAL astTable.put(M3CStdTypes.Integer(), Type.integer);
    EVAL astTable.put(M3CStdTypes.Longint(), Type.longint);
    EVAL astTable.put(M3CStdTypes.Real(), Type.real);
    EVAL astTable.put(M3CStdTypes.LongReal(), Type.longreal);
    EVAL astTable.put(M3CStdTypes.Extended(), Type.extended);
    EVAL astTable.put(M3CStdTypes.Null(), Type.null);
    EVAL astTable.put(M3CStdTypes.RefAny(), Type.refany);
    EVAL astTable.put(M3CStdTypes.Address(), Type.address);
    EVAL astTable.put(M3CStdTypes.Root(), Type.root);
    EVAL astTable.put(M3CStdTypes.Untraced_Root(), Type.untracedRoot);
    EVAL astTable.put(M3CStdTypes.Char(), Type.char);
    EVAL astTable.put(M3CStdTypes.WideChar(), Type.widechar);
    EVAL astTable.put(M3CStdTypes.Text(), Type.text);
    EVAL astTable.put(M3CStdTypes.Cardinal(), Type.cardinal);
    EVAL astTable.put(M3CStdTypes.Longcard(), Type.longcard);
    EVAL astTable.put(M3CStdTypes.Boolean(), Type.boolean);
    EVAL astTable.put(M3CStdTypes.Mutex(), Type.mutex);
  END InitAstTable;

PROCEDURE ProcessM3Type (h: Handle; m3type: M3AST_AS.M3TYPE): Type.T =
  VAR ts: M3AST_AS.TYPE_SPEC;
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m3type, ts);
    RETURN ProcessTypeSpec(h, ts);
  END ProcessM3Type;

PROCEDURE ProcessTypeSpec (h: Handle; ts: M3AST_AS.TYPE_SPEC): Type.T =
  VAR r: REFANY;
  VAR t: Type.T;
  BEGIN
    IF h.astMap.get(ts, r) THEN
      RETURN NARROW(r, Type.T);
    ELSE
      TYPECASE ts OF
      | M3AST_AS.Real_type => t := Type.real;
      | M3AST_AS.LongReal_type => t := Type.longreal;
      | M3AST_AS.Extended_type => t := Type.extended;
      | M3AST_AS.Integer_type => t := Type.integer;
      | M3AST_AS.Longint_type => t := Type.longint;
      | M3AST_AS.WideChar_type => t := Type.widechar;
      | M3AST_AS.Null_type => t := Type.null;
      | M3AST_AS.RefAny_type => t := Type.refany;
      | M3AST_AS.Address_type => t := Type.address;
      | M3AST_AS.Root_type(rt) => 
        TYPECASE rt.as_trace_mode OF
        | NULL => t := Type.root
        ELSE t :=  Type.untracedRoot
        END;
      | M3AST_AS.Packed_type (pt) =>
        t := NEW(Type.Packed,
                 size := NARROW(pt.as_exp.sm_exp_value,
                                M3CBackEnd_C.Integer_value).sm_value,
                 base := ProcessM3Type(h, pt.as_type));
      | M3AST_AS.Array_type (at) =>
        VAR
          ASTindexType: M3AST_SM.TYPE_SPEC_UNSET;
          eltTypeSpec : M3AST_SM.TYPE_SPEC_UNSET;
          openArray   : BOOLEAN;
        BEGIN
          EVAL M3ASTNext.Array(at, eltTypeSpec, openArray, ASTindexType);
          IF openArray THEN
            t := NEW(Type.OpenArray, index := NIL,
                     element := ProcessTypeSpec(h, eltTypeSpec));
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
            t := NEW(Type.Array, index := ProcessM3Type(h, ASTindexType),
                     element := ProcessTypeSpec(h, eltTypeSpec));
          END;
        END;
      | M3AST_AS.Enumeration_type (enum) =>
        VAR
          enumt := NEW(Type.UserDefined,
                       elts := NEW(REF ARRAY OF Atom.T, enum.sm_num_elements));
          iter := SeqM3AST_AS_Enum_id.NewIter(enum.as_id_s);
          elem: M3AST_AS.Enum_id;
        BEGIN
          FOR i := 1 TO enum.sm_num_elements DO
            EVAL SeqM3AST_AS_Enum_id.Next(iter, elem);
            enumt.elts[i-1] := Atom.FromText(M3CId.ToText(elem.lx_symrep));
          END;
          t := enumt;
        END;
      | M3AST_AS.Set_type (set) =>
        t := NEW(Type.Set, range := ProcessM3Type(h, set.as_type));
      | M3AST_AS.Subrange_type (sub) =>
        TYPECASE sub.sm_base_type_spec OF
        | M3AST_AS.Longint_type =>
          WITH baseType = ProcessTypeSpec(h, sub.sm_base_type_spec),
               e1 = NARROW(sub.as_range, M3AST_AS.Range).as_exp1,
               e2 = NARROW(sub.as_range, M3AST_AS.Range).as_exp2,
               i1 = NARROW(e1.sm_exp_value,
                           M3CBackEnd_C.Longint_value).sm_value,
               i2 = NARROW(e2.sm_exp_value,
                           M3CBackEnd_C.Longint_value).sm_value DO
            t := NEW(Type.Subrange, base := baseType,
                     min := NEW(Value.Longint, val := i1),
                     max := NEW(Value.Longint, val := i2));
          END;
        ELSE
          WITH baseType = ProcessTypeSpec(h, sub.sm_base_type_spec),
               e1 = NARROW(sub.as_range, M3AST_AS.Range).as_exp1,
               e2 = NARROW(sub.as_range, M3AST_AS.Range).as_exp2,
               i1 = NARROW(e1.sm_exp_value,
                           M3CBackEnd_C.Integer_value).sm_value,
               i2 = NARROW(e2.sm_exp_value,
                           M3CBackEnd_C.Integer_value).sm_value DO
            t := NEW(Type.Subrange, base := baseType,
                     min := NEW(Value.Integer, val := i1),
                     max := NEW(Value.Integer, val := i2));
          END;
        END;
      | M3AST_AS.Record_type (rec) => 
        t := NEW(Type.Record, fields := ProcessFields(h, rec.as_fields_s));
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
            ELSE trace := FALSE;
            END;
            t := NEW(Type.Ref, traced := trace, branded := branded,
                     brand := brandName);
            AddToTable(h, ts, t);
            NARROW(t, Type.Ref).target := ProcessM3Type(h, ref.as_type);
          | M3AST_AS.Object_type (ob) =>
            t := ProcessObject(h, ob, branded, brandName, trace);
          ELSE
            Error("AstToType.ProcessTypeSpec: unrecognized reference type");
          END;
        END;
      | M3AST_AS.Opaque_type (o) =>
        IF o.sm_concrete_type_spec = NIL THEN
          WITH revSuperTs = M3CConcTypeSpec.CurrentReveal(o),
               revSuperType = NARROW(ProcessTypeSpec(h, revSuperTs),
                                     Type.Reference) DO
            t := NEW(Type.Opaque, revealedSuperType := revSuperType);
          END;
        ELSE
          WITH revTs = o.sm_concrete_type_spec DO
            t := ProcessTypeSpec(h, revTs);
            WITH tt = NARROW(t, Type.Object) DO
              tt.revIntf :=
                  Atom.FromText(M3CId.ToText(revTs.tmp_unit_id.lx_symrep));
            END;
          END;
        END;
      | M3AST_AS.Procedure_type (proc) =>
        VAR
          formals : REF ARRAY OF Type.Formal;
          nFormals: INTEGER := 0;
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
          FOR i := 0 TO nFormals-1 DO
            EVAL M3ASTNext.Formal(iter, formalParam, formalId);
            formals[i] := NEW(Type.Formal);
            formals[i].name :=
                Atom.FromText(M3CId.ToText(formalId.lx_symrep));
            formals[i].type := ProcessM3Type(h, formalId.sm_type_spec);
(*
            formals[i].type := ProcessM3Type(h, formalParam.as_formal_type);
            IF formalParam.as_default # NIL THEN
              formals[i].default := AstToVal.Val(
                                        formalParam.as_default.sm_exp.value,
                                        formals[i].type);
            END;
*)
            TYPECASE formalId OF
            | M3AST_AS.F_Value_id => formals[i].mode := Type.Mode.Value;
            | M3AST_AS.F_Var_id => formals[i].mode := Type.Mode.Var;
            | M3AST_AS.F_Readonly_id =>
              formals[i].mode := Type.Mode.Readonly;
            ELSE
              Error("AstToType.ProcessTypeSpec: unrecognized parameter mode");
            END;
            formals[i].outOnly := FALSE;
            (* Change to depend on <*OUTPUT*> *)
          END;
          signature.formals := formals;
          IF proc.as_result_type # NIL THEN
            signature.result := ProcessM3Type(h, proc.as_result_type);
          END;
          IF proc.as_raises = NIL THEN
            signature.raises := NEW(REF ARRAY OF Type.Exception, 0)
          ELSE
            TYPECASE proc.as_raises OF
            | M3AST_AS.Raisees_some (r) =>
              VAR
                iter    := SeqM3AST_AS_Qual_used_id.NewIter(r.as_raisees_s);
                nRaises := SeqM3AST_AS_Qual_used_id.Length(r.as_raisees_s);
                raisee : M3AST_AS.Qual_used_id;
                arg    : M3AST_AS.Exc_id;
                argType: Type.T;
              BEGIN
                signature.raises := NEW(REF ARRAY OF Type.Exception, nRaises);
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
                        Atom.FromText(M3CId.ToText(raisee.as_id.lx_symrep));
                  END;
                  IF raisee.as_id.sm_def = NIL THEN
                    signature.raises[i].arg := NIL
                  ELSE
                    arg := NARROW(raisee.as_id.sm_def, M3AST_AS.Exc_id);
                    IF arg.tmp_type = NIL THEN
                      signature.raises[i].arg := NIL;
                    ELSE
                      argType := ProcessM3Type(h, arg.tmp_type);
                      signature.raises[i].arg := argType;
(*
                      IF argType.name = NIL THEN
                        argType.name := NEW(Type.Qid);
                        WITH id = NARROW(arg, M3AST_AS.Qual_used_id) DO
                          FillInIntf(h, argType.name, id.as_intf_id);
                          argType.name.item :=
                              Atom.FromText(M3CId.ToText(id.as_id.lx_symrep));
                        END;
                      END;
*)
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
    AddToTable(h, ts, t);
    RETURN t;
  END ProcessTypeSpec;

PROCEDURE AddToTable (h: Handle; ts: M3AST_AS.TYPE_SPEC; t: Type.T) =
  BEGIN
    IF t = NIL THEN
      RETURN;
    END;
    EVAL h.astMap.put(ts, t);
    IF t.name = NIL THEN
      WITH symrep = NARROW(PropertyV.Get(
                             ts.tl_pset, TYPECODE(M3AST_LX.Symbol_rep)),
                           M3AST_LX.Symbol_rep) DO
        IF symrep # NIL THEN
          t.name := NEW(Type.Qid);
          t.name.intf := Atom.FromText(M3CId.ToText(ts.tmp_unit_id.lx_symrep));
          t.name.item := Atom.FromText(M3CId.ToText(symrep));
        END;
      END;
    END;
  END AddToTable;

(*
PROCEDURE FillInIntf(h: Handle; qid: Type.Qid;
                     intfId: M3AST_AS.Used_interface_id) =
  BEGIN
    IF intfId = NIL THEN
      qid.intf := h.intf
    ELSE
      qid.intf := Atom.FromText(M3CId.ToText(intfId.lx_symrep));
    END;
  END FillInIntf;
*)

PROCEDURE ProcessObject (h      : Handle;
                         o      : M3AST_AS.Object_type;
                         branded: BOOLEAN;
                         brand  : Atom.T;
                         traced : BOOLEAN): Type.T =
  VAR t := NEW(Type.Object);
  BEGIN
    AddToTable(h, o, t);
    t.branded := branded;
    t.brand := brand;
    t.traced := traced;
    IF o.as_ancestor # NIL THEN
      t.super := ProcessM3Type(h, o.as_ancestor);
    END;
    t.fields := ProcessFields(h, o.as_fields_s);
    t.methods := ProcessMethods(h, o.as_method_s);
    RETURN t;
  END ProcessObject;


PROCEDURE ProcessFields (h: Handle; f: SeqM3AST_AS_Fields.T):
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
          fields[j].type := ProcessTypeSpec(h, fieldId.sm_type_spec)
        ELSE
          fields[j].type := ProcessM3Type(h, astFields.as_type);
        END;
        IF (*fieldId.vINIT_ID.sm_init_exp*) astFields.as_default # NIL THEN
          fields[j].default :=
              AstToVal.ProcessExp((*h,*)
                                  (*fieldId.vINIT_ID.sm_init_exp);*)
                                  astFields.as_default);
        END;
        INC(j);
      END;
    END;
    RETURN fields;
  END ProcessFields;

PROCEDURE ProcessMethods (h: Handle; m: SeqM3AST_AS_Method.T):
  REF ARRAY OF Type.Method =
  VAR
    nMethods                            := SeqM3AST_AS_Method.Length(m);
    methods  : REF ARRAY OF Type.Method;
    iter                                := SeqM3AST_AS_Method.NewIter(m);
    astMethod: M3AST_AS.Method;
  BEGIN
    methods := NEW(REF ARRAY OF Type.Method, nMethods);
    FOR i := 0 TO nMethods-1 DO
      EVAL SeqM3AST_AS_Method.Next(iter, astMethod);
      methods[i] := NEW(Type.Method);
      methods[i].name :=
          Atom.FromText(M3CId.ToText(astMethod.as_id.lx_symrep));
      methods[i].sig :=
          NARROW(ProcessTypeSpec(h, astMethod.as_type), Type.Procedure).sig;
      IF astMethod.as_default # NIL THEN
(*
        methods[i].default := AstToVal.Val(
                                  astMethod.vINIT_ID.sm_init_exp,
                                  methods[i].type).sm_exp_value
*)
      END;
    END;
    RETURN methods;
  END ProcessMethods;

BEGIN
END AstToType.
