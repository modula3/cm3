(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE AstToType;

IMPORT Atom, Wr, PropertyV, RefRefTbl, FRefRefTbl, TextRefTbl,
       StubCode, StubUtils;
IMPORT AST, ASTWalk;
IMPORT M3AST_AS, M3ASTScope;
IMPORT M3CId;
IMPORT AstToVal, Type, Value;
IMPORT M3AST_AS_F, M3AST_SM, M3AST_SM_F, M3AST_TM_F, M3ASTNext,
       M3AST_TL_F, M3AST_LX, 
       M3Context, M3CConcTypeSpec,
       M3CSearch, M3CStdTypes, M3CUnit;
IMPORT SeqM3AST_AS_Enum_id, SeqM3AST_AS_Fields,
       SeqM3AST_AS_Field_id, SeqM3AST_AS_Method, 
       SeqM3AST_AS_Qual_used_id, SeqM3AST_AS_TYPE_SPEC, M3CTypesMisc;
IMPORT M3CBackEnd,M3CBackEnd_C;

REVEAL Handle = Public BRANDED OBJECT
     netobjTypeSpec: M3AST_AS.TYPE_SPEC;
     wrTypeSpec: M3AST_AS.TYPE_SPEC := NIL;
     rdTypeSpec: M3AST_AS.TYPE_SPEC := NIL;
     astMap: RefRefTbl.T;
     nameMap: TextRefTbl.T;
     OVERRIDES callback := Node END;

PROCEDURE NewHandle(wr: Wr.T; intfName: TEXT; context: M3Context.T): Handle=
  VAR ts: M3AST_AS.TYPE_SPEC; 
      used_id: M3AST_AS.USED_ID := NEW(M3AST_AS.USED_ID).init();
      cu : M3AST_AS.Compilation_Unit;
      astTable: RefRefTbl.T;
      h: Handle;
      netObjType: Type.T;
  BEGIN
    used_id.lx_symrep := M3CId.Enter("T");
    astTable := NEW(FRefRefTbl.Default).init();
    IF M3Context.FindExact(context, "NetObj", M3CUnit.Type.Interface, 
                       cu) THEN
      M3CSearch.Export(cu.as_root, used_id);
      ts := NARROW(used_id.sm_def, M3AST_AS.TYPED_ID).sm_type_spec;
    ELSE
      (* NetObj not in context, i.e. not imported, must be error *)
      StubUtils.Message("NetObj.T not known in this scope.");
      RETURN NIL;
    END; 
    InitAstTable(astTable);
    h := NEW(Handle, wr := wr, intf := Atom.FromText(intfName), 
              context := context, 
              astMap := astTable, nameMap := NEW(TextRefTbl.Default).init(),
              netobjTypeSpec := ts);
    netObjType := ProcessTypeSpec(h, ts);
    netObjType.name := NEW(Type.Qid, intf := Atom.FromText("NetObj"),
                           item := Atom.FromText("T"));
    EVAL astTable.put(ts, netObjType);
    IF M3Context.FindExact(context, "Wr", M3CUnit.Type.Interface, 
                       cu) THEN
      M3CSearch.Export(cu.as_root, used_id);
      h.wrTypeSpec := NARROW(used_id.sm_def, M3AST_AS.TYPED_ID).sm_type_spec;
    END; 
    IF M3Context.FindExact(context, "Rd", M3CUnit.Type.Interface, 
                       cu) THEN
      M3CSearch.Export(cu.as_root, used_id);
      h.rdTypeSpec := NARROW(used_id.sm_def, M3AST_AS.TYPED_ID).sm_type_spec;
    END; 
    RETURN h;
  END NewHandle;

PROCEDURE OneStub(c: M3Context.T; qid: Type.Qid; wr: Wr.T): INTEGER =
  VAR h := NewHandle(wr, Atom.ToText(qid.intf), c);
      used_id: M3AST_AS.USED_ID := NEW(M3AST_AS.USED_ID).init();
      def_id: M3AST_AS.DEF_ID;
      cu : M3AST_AS.Compilation_Unit;
      ts: M3AST_AS.TYPE_SPEC; 
      valid: BOOLEAN;
      returnCode := 0;
  BEGIN
    IF h = NIL THEN RETURN 1 END;
       (* Can't continue if NetObj.T not in current context *)
    used_id.lx_symrep := M3CId.Enter(Atom.ToText(qid.item));
    IF M3Context.FindExact(c, Atom.ToText(qid.intf), 
                           M3CUnit.Type.Interface, cu) THEN
      M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Entry);
      (* M3CSearch.Export(cu.as_root, used_id);*)
      (* used_id.sm_def is now bound to the Type_id for the object
         named qid. *)
      def_id := M3ASTScope.Lookup(cu.as_root.as_id.vSCOPE, used_id);
      IF def_id = NIL THEN
        StubUtils.Message(Atom.ToText(qid.intf) & "." & Atom.ToText(qid.item) &
          " not defined.");
        RETURN 1;
      END;
      ts := NARROW(def_id, M3AST_AS.TYPED_ID).sm_type_spec;
      IF CheckNetObj(ts, valid, h) THEN
        returnCode := ProcessNetObj(h, ts, valid, qid);
      ELSE
        StubUtils.Message(Atom.ToText(qid.intf) & "." &
          Atom.ToText(qid.item) & " Not a legal network object type" &
          " \n not a subtype of NetObj.T or opaque or " &
          " contains data fields.");
        returnCode :=1;
      END; 
    ELSE
      returnCode := 1;
    END;
    M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Exit);
    RETURN returnCode;
  END OneStub;

PROCEDURE ProcessNetObj(h: Handle; 
                        ts: M3AST_AS.TYPE_SPEC;
                        valid: BOOLEAN; 
                        qid: Type.Qid): INTEGER =
  VAR t: Type.T;
  BEGIN
    StubUtils.Message("Processing " & Atom.ToText(qid.intf) & "." &
      Atom.ToText(qid.item) );
    IF valid THEN
      t := ProcessM3Type(h, ts);
      TYPECASE t OF
      | Type.Object => 
          IF StubCode.BrandsOK(t) THEN
            StubCode.GenStub(t, qid);
          ELSE
            StubUtils.Message("Brand errors -- no stubs produced");
          END;
      | Type.Opaque =>
        StubUtils.Message("WARNING.  This type equivalent to " &
          "NetObj.T.\n       No stubs generated: default stubs exist.");
      ELSE StubUtils.Die("AstToType.ProcessNetObj: unsupported type");
      END;
      RETURN 0;
    ELSE
      StubUtils.Message(
          "Not a legal network object type: opaque or contains data fields.");
      RETURN 1;
    END;    
  END ProcessNetObj;

PROCEDURE InitAstTable(astTable: RefRefTbl.T) =
  BEGIN
    EVAL astTable.put(M3CStdTypes.Integer(), Type.integer);
    EVAL astTable.put(M3CStdTypes.Real(), Type.real);
    EVAL astTable.put(M3CStdTypes.LongReal(), Type.longreal);
    EVAL astTable.put(M3CStdTypes.Extended(), Type.extended);
    EVAL astTable.put(M3CStdTypes.Null(), Type.null);
    EVAL astTable.put(M3CStdTypes.RefAny(), Type.refany);
    EVAL astTable.put(M3CStdTypes.Address(), Type.address);
    EVAL astTable.put(M3CStdTypes.Root(), Type.root);
    EVAL astTable.put(M3CStdTypes.Untraced_Root(),
                      Type.untracedRoot);
    EVAL astTable.put(M3CStdTypes.Char(), Type.char);
    EVAL astTable.put(M3CStdTypes.WideChar(), Type.widechar);
    EVAL astTable.put(M3CStdTypes.Text(), Type.text);
    EVAL astTable.put(M3CStdTypes.Cardinal(), Type.cardinal);
    EVAL astTable.put(M3CStdTypes.Boolean(), Type.boolean);
    EVAL astTable.put(M3CStdTypes.Mutex(), Type.mutex);
  END InitAstTable;

PROCEDURE Node(h: Handle; n: AST.NODE; vm: ASTWalk.VisitMode)=
  VAR typeSpec: M3AST_AS.TYPE_SPEC;
      typeName: TEXT;
      valid: BOOLEAN;
      qid: Type.Qid;
  BEGIN
    IF vm = ASTWalk.VisitMode.Entry THEN
      TYPECASE n OF
      | M3AST_AS.TYPE_DECL(p) =>
          typeName := M3CId.ToText(p.as_id.lx_symrep);
          M3CTypesMisc.GetTYPE_SPECFromM3TYPE(p.as_type, typeSpec);
          IF CheckNetObj(typeSpec, valid, h) THEN
            qid := NEW(Type.Qid, intf := h.intf, 
                       item := Atom.FromText(typeName));
            EVAL ProcessNetObj(h, typeSpec, valid, qid);
          END;
      ELSE
      END;
    END;
  END Node;

PROCEDURE ProcessM3Type(h: Handle; m3type: M3AST_AS.M3TYPE): Type.T =
  VAR ts: M3AST_AS.TYPE_SPEC;
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m3type, ts);
    RETURN ProcessTypeSpec(h, ts);
  END ProcessM3Type;

  PROCEDURE ProcessTypeSpec(h: Handle; ts: M3AST_AS.TYPE_SPEC): Type.T =
    VAR r: REFANY;
    VAR t: Type.T;
    BEGIN
      IF h.astMap.get(ts, r) THEN
        RETURN NARROW(r, Type.T);
      ELSE
        TYPECASE ts OF
          |  M3AST_AS.Real_type => t := Type.real;
          |  M3AST_AS.LongReal_type => t := Type.longreal;
          |  M3AST_AS.Extended_type => t := Type.extended;
          |  M3AST_AS.Integer_type => t := Type.integer;
          |  M3AST_AS.WideChar_type => t := Type.widechar;
          |  M3AST_AS.Null_type => t := Type.null;
          |  M3AST_AS.RefAny_type => t := Type.refany;
          |  M3AST_AS.Address_type => t := Type.address;
          |  M3AST_AS.Root_type(rt) => 
              TYPECASE rt.as_trace_mode OF
              | NULL => t := Type.root
              ELSE t :=  Type.untracedRoot
              END;
          |  M3AST_AS.Packed_type (pt) =>
              t := NEW(Type.Packed, 
                       size := NARROW(pt.as_exp.sm_exp_value,
                                M3CBackEnd_C.Integer_value).sm_value,
                       base := ProcessM3Type(h, pt.as_type));
          |  M3AST_AS.Array_type (at) => 
              VAR ASTindexType: M3AST_SM.TYPE_SPEC_UNSET;
                  eltTypeSpec: M3AST_SM.TYPE_SPEC_UNSET;
                  openArray: BOOLEAN;
              BEGIN
                EVAL M3ASTNext.Array(at, eltTypeSpec, openArray, ASTindexType);
                IF openArray THEN
                  t := NEW(Type.OpenArray, 
                           index := NIL,
                           element := ProcessTypeSpec(h, eltTypeSpec));
                  WITH openA = NARROW(t, Type.OpenArray),
                       refArray = NEW(Type.Ref, traced := TRUE,
                                     target := t) DO
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
                           index := ProcessM3Type(h, ASTindexType),
                           element := ProcessTypeSpec(h, eltTypeSpec));
                END;
              END;
          |  M3AST_AS.Enumeration_type (enum) =>
               VAR enumt:= NEW(Type.UserDefined, elts :=
                        NEW(REF ARRAY OF Atom.T, enum.sm_num_elements));
               iter := SeqM3AST_AS_Enum_id.NewIter(enum.as_id_s);
               elem: M3AST_AS.Enum_id; 
               BEGIN
                 FOR i := 1 TO enum.sm_num_elements DO
                   EVAL SeqM3AST_AS_Enum_id.Next(iter, elem);
                   enumt.elts[i-1] := Atom.FromText(M3CId.ToText(elem.lx_symrep));
                 END;
                 t := enumt;
               END;
          |  M3AST_AS.Set_type (set) => 
               t := NEW(Type.Set, range := ProcessM3Type(h, set.as_type));
          |  M3AST_AS.Subrange_type (sub) =>
             VAR  e1, e2: M3AST_AS.EXP;
                  i1, i2: INTEGER;
                  baseType: Type.T;
             BEGIN 
               baseType := ProcessTypeSpec(h, sub.sm_base_type_spec);
               e1 := NARROW(sub.as_range, M3AST_AS.Range).as_exp1;
               e2 := NARROW(sub.as_range,  M3AST_AS.Range).as_exp2;
               EVAL M3CBackEnd.Ord(e1.sm_exp_value, i1);
               EVAL M3CBackEnd.Ord(e2.sm_exp_value, i2);
               t := NEW(Type.Subrange, base := baseType,
                        min := NEW(Value.Ordinal, ord := i1),
                        max := NEW(Value.Ordinal, ord := i2));
             END
          |  M3AST_AS.Record_type (rec) => 
               t := NEW(Type.Record, 
                        fields := ProcessFields(h, rec.as_fields_s));
          |  M3AST_AS.BRANDED_TYPE (bt) =>
              VAR brandName: Atom.T := NIL;
                  branded := FALSE;
                  trace: BOOLEAN;
              BEGIN
                IF bt.as_brand # NIL THEN
                  IF bt.as_brand.as_exp # NIL THEN
                    brandName := Atom.FromText(NARROW(bt.as_brand.as_exp.
                                     sm_exp_value,
                                M3CBackEnd_C.Text_value).sm_value);
                  END;
                  branded := TRUE
                END;
                TYPECASE bt OF
                |  M3AST_AS.Ref_type (ref) => 
                   TYPECASE ref.as_trace_mode OF
                   | NULL => trace := TRUE;
                   ELSE trace := FALSE;
                   END;
                   t := NEW(Type.Ref, traced := trace, branded := branded,
                            brand := brandName);
                   AddToTable(h, ts, t);
                   NARROW(t, Type.Ref).target := 
                       ProcessM3Type(h, ref.as_type);
                |  M3AST_AS.Object_type (ob) => 
                  t := ProcessObject(h, ob, branded, brandName, trace);
                ELSE StubUtils.Die("AstToType.ProcessTypeSpec: unrecognized reference type");
                END;
              END;
          |  M3AST_AS.Opaque_type (o) => 
             IF o.sm_concrete_type_spec = NIL THEN
               WITH revSuperTs = M3CConcTypeSpec.CurrentReveal(o),
                    revSuperType = 
                      NARROW(ProcessTypeSpec(h, revSuperTs), Type.Reference) DO
                 t := NEW(Type.Opaque, 
                        revealedSuperType := revSuperType);
               END;
             ELSE
               WITH revTs = o.sm_concrete_type_spec DO
                 t := ProcessTypeSpec(h, revTs);
                 WITH tt = NARROW(t, Type.Object) DO
                   tt.revIntf := Atom.FromText(
                                 M3CId.ToText(revTs.tmp_unit_id.lx_symrep));
                 END;
               END;
             END;
          |  M3AST_AS.Procedure_type (proc) =>
             VAR formals: REF ARRAY OF Type.Formal;
                 nFormals: INTEGER := 0;
                 iter := M3ASTNext.NewIterFormal(proc.as_formal_param_s);
                 formalParam: M3AST_AS.Formal_param;
                 formalId: M3AST_AS.FORMAL_ID;
                 signature: Type.Signature;
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
                 formals[i].type := 
(*                     ProcessM3Type(h, formalParam.as_formal_type); *)
                     ProcessM3Type(h, formalId.sm_type_spec);
(*
                 IF formalParam.as_default # NIL THEN
                   formals[i].default := AstToVal.Val(
                              formalParam.as_default.sm_exp.value,
                              formals[i].type);
                 END;
*)
                 TYPECASE formalId OF
                   M3AST_AS.F_Value_id => formals[i].mode := Type.Mode.Value;
                 | M3AST_AS.F_Var_id => formals[i].mode := Type.Mode.Var;
                 | M3AST_AS.F_Readonly_id => formals[i].mode := 
                     Type.Mode.Readonly;
                 ELSE
                   StubUtils.Die("AstToType.ProcessTypeSpec: unrecognized parameter mode");
                 END;
                 formals[i].outOnly := FALSE;
                   (* Change to depend on  <*OUTPUT*> *)
               END;
               signature.formals := formals;
               IF proc.as_result_type # NIL THEN
                 signature.result := ProcessM3Type(h, proc.as_result_type);
               END;
               IF proc.as_raises = NIL THEN
                 signature.raises := NEW(REF ARRAY OF Type.Exception, 0)
               ELSE
                 TYPECASE proc.as_raises OF
                   M3AST_AS.Raisees_some (r) => 
                     VAR iter:= 
                           SeqM3AST_AS_Qual_used_id.NewIter(r.as_raisees_s);
                         nRaises := 
                             SeqM3AST_AS_Qual_used_id.Length(r.as_raisees_s);
                         raisee: M3AST_AS.Qual_used_id;
                         arg: M3AST_AS.Exc_id;
                         argType: Type.T;
                     BEGIN
                       signature.raises := NEW(REF ARRAY OF 
                           Type.Exception, nRaises);
                       FOR i := 0 TO nRaises-1 DO
                         EVAL SeqM3AST_AS_Qual_used_id.Next(iter, raisee);
                         signature.raises[i] := NEW(Type.Exception);
                         signature.raises[i].qid := NEW(Type.Qid);
                         WITH qid = signature.raises[i].qid DO
                           qid.intf := Atom.FromText(M3CId.ToText(
                                  raisee.as_id.sm_def.tmp_unit_id.lx_symrep));
                           qid.item := Atom.FromText(
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
                             argType := ProcessM3Type(h, arg.tmp_type);
                             signature.raises[i].arg := argType;
                         (*    IF argType.name = NIL THEN
                               argType.name := NEW(Type.Qid);
                               WITH id = NARROW(arg, M3AST_AS.Qual_used_id) DO
                                 FillInIntf(h, argType.name, id.as_intf_id);
                                 argType.name.item := Atom.FromText(
                                   M3CId.ToText(id.as_id.lx_symrep));  
                               END;
                             END;  *)
                           END;
                         END;
                       END;
                     END;
                 | M3AST_AS.Raisees_any => 
                 ELSE signature.raises := NEW(REF ARRAY OF Type.Exception, 0)
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

PROCEDURE AddToTable(h: Handle; ts: M3AST_AS.TYPE_SPEC; t: Type.T) =
  BEGIN
    EVAL h.astMap.put(ts, t);
    IF t.name = NIL THEN
      WITH symrep =  NARROW(PropertyV.Get(ts.tl_pset, 
                            TYPECODE(M3AST_LX.Symbol_rep)), 
                            M3AST_LX.Symbol_rep)  DO 
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

(* Check whether o is a subtype of NetObj.T, and whether it is valid
   for stub generation.  To be valid, o must be a subtype of NetObj.t,
   its supertypes must be fully revealed in the scope where stubs are 
   generated, and o must be a pure object, i.e. must have no fields. *)

  PROCEDURE CheckNetObj(o: M3AST_AS.TYPE_SPEC; VAR valid: BOOLEAN; h: Handle):
      BOOLEAN =
    VAR 
        checkType, ts: M3AST_AS.TYPE_SPEC;
        fieldsFound := FALSE;
        iter: SeqM3AST_AS_TYPE_SPEC.Iter;
    BEGIN
      valid := FALSE;
      checkType := o;
      LOOP
        IF checkType = NIL THEN RETURN FALSE END;
        TYPECASE checkType OF
        | M3AST_AS.Root_type => RETURN FALSE
        | M3AST_AS.Opaque_type (ot) =>
            IF ot = h.netobjTypeSpec THEN
              valid := NOT fieldsFound;
              RETURN TRUE;
            END;
            IF ot.sm_concrete_type_spec = NIL THEN
              iter := SeqM3AST_AS_TYPE_SPEC.NewIter(ot.sm_type_spec_s);
              WHILE SeqM3AST_AS_TYPE_SPEC.Next(iter, ts) DO
                IF CheckNetObj(ts, valid, h) THEN
                  valid := FALSE;
                  RETURN TRUE;
                END;
              END;
              RETURN FALSE;
            ELSE
              checkType := ot.sm_concrete_type_spec;
            END;
        | M3AST_AS.Object_type (objt) =>
          IF objt = h.netobjTypeSpec THEN
            valid := NOT fieldsFound;
            RETURN TRUE;
          END;
          IF NOT SeqM3AST_AS_Fields.Empty(objt.as_fields_s) THEN
            fieldsFound := TRUE;
          END;
          IF objt.as_ancestor = NIL THEN RETURN FALSE END;
          M3CTypesMisc.GetTYPE_SPECFromM3TYPE(objt.as_ancestor, checkType);
        ELSE RETURN FALSE;
        END;
      END;
    END CheckNetObj;

  PROCEDURE ProcessObject(h: Handle; 
                          o: M3AST_AS.Object_type; 
                          branded: BOOLEAN;
                          brand: Atom.T;
                          traced: BOOLEAN) : Type.T =
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


  PROCEDURE ProcessFields(h: Handle; f: SeqM3AST_AS_Fields.T): 
      REF ARRAY OF Type.Field =
    VAR nFields: INTEGER := 0;
        fields: REF ARRAY OF Type.Field;
        iter := M3ASTNext.NewIterField(f);
        iterItems := SeqM3AST_AS_Fields.NewIter(f);
        iterIds : SeqM3AST_AS_Field_id.Iter; 
        astFields: M3AST_AS.Fields; 
        fieldId: M3AST_AS.Field_id;
        j: INTEGER := 0;
    BEGIN
       WHILE M3ASTNext.Field(iter, fieldId) DO INC(nFields) END;
       fields := NEW(REF ARRAY OF Type.Field, nFields);
       WHILE SeqM3AST_AS_Fields.Next(iterItems, astFields) DO 
         iterIds := SeqM3AST_AS_Field_id.NewIter(astFields.as_id_s);
         WHILE SeqM3AST_AS_Field_id.Next(iterIds, fieldId) DO
           fields[j] := NEW(Type.Field);
           fields[j].name := 
              Atom.FromText(M3CId.ToText(fieldId.lx_symrep));
           IF astFields.as_type = NIL THEN
             fields[j].type := ProcessTypeSpec(h, fieldId.sm_type_spec)
           ELSE
             fields[j].type := ProcessM3Type(h, astFields.as_type);
           END;
           IF (*fieldId.vINIT_ID.sm_init_exp*) astFields.as_default # NIL THEN 
              fields[j].default := AstToVal.ProcessExp(h,
                        (*fieldId.vINIT_ID.sm_init_exp);*)
                        astFields.as_default);
           END;
           INC(j);
         END;
       END;
       RETURN fields;
     END ProcessFields;

  PROCEDURE ProcessMethods(h: Handle; m: SeqM3AST_AS_Method.T): 
    REF ARRAY OF Type.Method =
    VAR nMethods:= SeqM3AST_AS_Method.Length(m);
        methods: REF ARRAY OF Type.Method;
        iter := SeqM3AST_AS_Method.NewIter(m);
        astMethod: M3AST_AS.Method;
    BEGIN
       methods := NEW(REF ARRAY OF Type.Method, nMethods);
       FOR i := 0 TO nMethods-1 DO
         EVAL SeqM3AST_AS_Method.Next(iter, astMethod);
         methods[i] := NEW(Type.Method);
         methods[i].name := 
            Atom.FromText(M3CId.ToText(astMethod.as_id.lx_symrep));
         methods[i].sig := NARROW(ProcessTypeSpec(h, astMethod.as_type), 
                                  Type.Procedure).sig;
         IF astMethod.as_default # NIL THEN
            (*methods[i].default := AstToVal.Val(
                     astMethod.vINIT_ID.sm_init_exp,
                      methods[i].type).sm_exp_value   *)
         END;
       END;
       RETURN methods;
  END ProcessMethods;

BEGIN


END AstToType.


