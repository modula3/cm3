(* Copyright (C) 1991, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

MODULE AstToType;

IMPORT Atom, Wr, PropertyV, RefRefTbl, FRefRefTbl, TextRefTbl,
       StubUtils;
IMPORT ASTWalk;
IMPORT M3AST_AS, M3ASTScope, M3AST_FE_F;
IMPORT M3CId;
IMPORT AstToVal, Type, Value;
IMPORT M3AST_AS_F, M3AST_SM, M3AST_SM_F, M3AST_TM_F, M3ASTNext,
       M3AST_TL_F, M3AST_LX, 
       M3Context, M3CConcTypeSpec,
       M3CStdTypes, M3CUnit, M3CUnitRep;
IMPORT SeqM3AST_AS_Enum_id, SeqM3AST_AS_Fields,
       SeqM3AST_AS_Field_id, SeqM3AST_AS_Method, 
       SeqM3AST_AS_Override,
       SeqM3AST_AS_Qual_used_id, M3CTypesMisc;
IMPORT M3CBackEnd,M3CBackEnd_C;
IMPORT SchemePair;
IMPORT TypeTranslator;
IMPORT RTBrand;
IMPORT SchemeObject, SchemeSymbol;
IMPORT RefSeq, M3ASTScopeNames;
FROM SchemeUtils IMPORT Cons, List2;
IMPORT SchemeString;
IMPORT ValueTranslator;
IMPORT Debug;
IMPORT CM3Extensions;
IMPORT Text;

REVEAL 
  Handle = Public BRANDED OBJECT
    wrTypeSpec: M3AST_AS.TYPE_SPEC := NIL;
    rdTypeSpec: M3AST_AS.TYPE_SPEC := NIL;
    astMap: RefRefTbl.T;
    nameMap: TextRefTbl.T;
  OVERRIDES 
    callback := NIL (* ??? *) 
  END;

PROCEDURE NewHandle(wr: Wr.T; intfName: TEXT; context: M3Context.T): Handle=
  VAR used_id: M3AST_AS.USED_ID := NEW(M3AST_AS.USED_ID).init();
      astTable: RefRefTbl.T;
      h: Handle;
  BEGIN
    used_id.lx_symrep := M3CId.Enter("T");
    astTable := NEW(FRefRefTbl.Default).init();

    InitAstTable(astTable);

    h := NEW(Handle, wr := wr, intf := Atom.FromText(intfName), 
             context := context, 
             astMap := astTable, nameMap := NEW(TextRefTbl.Default).init());
    RETURN h;
  END NewHandle;

PROCEDURE Tag(tag : TEXT; what : SchemeObject.T) : SchemePair.T =
  BEGIN
    RETURN NEW(SchemePair.T, 
               first := SchemeSymbol.FromText(tag),
               rest := what)
  END Tag;

PROCEDURE GetNames(c : M3Context.T; 
                   qid: Type.Qid) : RefSeq.T =
  <*FATAL RTBrand.NotBranded*>
  CONST Msg = StubUtils.Message;

  PROCEDURE DoFileNames(str : TEXT) =
    (* generic instances look like so: 

       ("/home/mika/t/calarm/twslib/src/TWSBridgeG.ig[/home/mika/t/calarm/twslib/src/TWSDefBridge.i3]")

    *)

    CONST Delims = SET OF CHAR { '[', ']', ',' };
    VAR q   := 0;   (* beginning of current string *)
        len := Text.Length(str);
    BEGIN 
      FOR p := 0 TO len-1 DO
        IF Text.GetChar(str,p) IN Delims THEN
          Append(Text.Sub(str,q,p-q)); q := p+1
        END
      END;
      IF q # len THEN Append(Text.Sub(str,q,len-q)) END
    END DoFileNames;

  PROCEDURE Append(fn : TEXT) =
    BEGIN filenames := Cons(SchemeString.FromText(fn),filenames) END Append;

  PROCEDURE ProcessUnit() =
    BEGIN
      Debug.Out("AstToType.GetNames : cu is " & RTBrand.GetName(TYPECODE(cu)));
      Debug.Out("AstToType.GetNames : cu.as_root is " & 
        RTBrand.GetName(TYPECODE(cu.as_root)));

      DoFileNames(NARROW(cu.fe_uid,M3CUnit.Uid).filename);

      WITH syms = M3ASTScopeNames.Names(cu.as_root.as_id.vSCOPE) DO
        IF syms = NIL THEN 
          res := NIL
        ELSE
          res := NEW(RefSeq.T).init();
          FOR i := 0 TO syms.size() - 1 DO
            WITH def_id = NARROW(syms.get(i),M3AST_AS.DEF_ID) DO
              res.addhi(Atom.FromText(M3CId.ToText(def_id.lx_symrep)))
            END
          END
        END
      END
    END ProcessUnit;

  VAR cu : M3AST_AS.Compilation_Unit;
      res : RefSeq.T := NIL;
  BEGIN
    Debug.Out("GetNames processing " & Atom.ToText(qid.intf));

    (* the following code is a bit fishy..

       M3ConcTypeSpec.SetCurrentReveal crashes on a generic 
       instantiation (even though I think this ought to be legal?
       -- so we skip it for those.

       But the code above (in ProcessUnit()) doesn't seem to find
       any names for generic instances, anyhow, so it doesn't help
       much. 

       What's going on?
    *)


    IF M3Context.FindExact(c, 
                           Atom.ToText(qid.intf), 
                           M3CUnit.Type.Interface, cu) THEN
      isUnsafe := NARROW(cu.as_root,M3AST_AS.Interface).as_unsafe # NIL;
      M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Entry);
      ProcessUnit();
      M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Exit)

    ELSIF M3Context.FindExact(c, 
                           Atom.ToText(qid.intf), 
                           M3CUnit.Type.Interface_gen_ins, cu) THEN

      isUnsafe := FALSE; (* no false generics *)
      cu := NARROW(cu.as_root,M3AST_AS.Interface_gen_ins).sm_ins_comp_unit;
      (* this seems to give us access to all the right names... *)

      M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Entry);
      ProcessUnit();
      M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Exit)

    ELSE
      StubUtils.Die("AstToType.GetNames: M3Context.FindExact failed")
    END;

    IF cu = NIL THEN
      StubUtils.Die("AstToType.GetNames: cu is NIL; no such interface/type?")
    END;

    RETURN res
  END GetNames;

PROCEDURE OneStubScm(c: M3Context.T; qid: Type.Qid; wr: Wr.T): INTEGER =
  <*FATAL RTBrand.NotBranded*>

  PROCEDURE ProcessOne() : CARDINAL = 
    BEGIN
      M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Entry);
      (* M3CSearch.Export(cu.as_root, used_id);*)
      (* used_id.sm_def is now bound to the Type_id for the object
         named qid. *)
      def_id := M3ASTScope.Lookup(cu.as_root.as_id.vSCOPE, used_id);
      IF def_id = NIL THEN
        Debug.Out(Atom.ToText(qid.intf) & "." & Atom.ToText(qid.item) &
          " not defined.");
        RETURN 1;
      END;
      Debug.Out("OneStub : " & Atom.ToText(qid.intf) & "." &
        Atom.ToText(qid.item) & 
        " : def_id is " & RTBrand.GetName(TYPECODE(def_id)));
      IF    ISTYPE(def_id, M3AST_AS.Interface_id) OR 
            ISTYPE(def_id, M3AST_AS.Interface_AS_id) THEN
        (* we don't remember interfaces, no need to, since they're 
           all factored out *)
        Debug.Out("OneStub found an interface, skipping "  & 
          Atom.ToText(qid.intf) & "." & Atom.ToText(qid.item))
      ELSIF ISTYPE(def_id, M3AST_AS.Exc_id) THEN
        VAR exception : Type.Exception;
        BEGIN
          FillInException(h, def_id, qid.item, exception);
          exceptionList := NEW(SchemePair.T,
                               first := TypeTranslator.TranslateException(exception),
                               rest := exceptionList)
        END
      ELSE
        ts := NARROW(def_id, M3AST_AS.TYPED_ID).sm_type_spec;

        WITH tkType = TypeTranslator.Translate(ProcessScmObj(h, 
                                                             ts, 
                                                             qid)) DO
          TYPECASE def_id OF
            M3AST_AS.Proc_id  =>  AddToList(procList,  qid, tkType)
          |
            M3AST_AS.Var_id   =>  AddToList(varList,   qid, tkType)
          |
            M3AST_AS.Const_id(cid) =>  
            WITH astValue = cid.vINIT_ID.sm_init_exp,
                 valValue = AstToVal.ProcessExp(h, astValue),
                 scmValue = ValueTranslator.Translate(valValue),
                 typeSYM = Atom.FromText("type"),
                 valueSYM = Atom.FromText("value") DO
              
              AddToList(constList, 
                        qid, 
                        List2(
                            Cons(typeSYM,tkType),
                            Cons(valueSYM,scmValue)))
            END
          ELSE
            Debug.Out("Not Proc/Var/Const, must be a type");
            
            (* ok this is all a bit screwy.  The type returned from the
               toolkit may use a base name, or some other "canonical" name.
               However, the canonical name need not be in a one-to-one mapping
               with the Modula-3 "type" (anyhow).  
               So we can add our own field, creating more unique types.
               
               This means that type equality checking is going to be 
               trickier. *)
            
            WITH
              head = tkType.first,
              rest = tkType.rest,
              
              (* so we insert the alias *)
              
              ours = NEW(SchemePair.T, 
                         first := head,
                         rest := NEW(SchemePair.T,
                                     first := Tag("alias",
                                                  TypeTranslator.TranslateQid(qid)),
                                     rest := rest))
             DO
            
              typeList := NEW(SchemePair.T, 
                              first := ours,
                              rest := typeList)
            END
          END
        END
      END;
      M3CConcTypeSpec.SetCurrentReveal(cu, ASTWalk.VisitMode.Exit);

      RETURN 0
    END ProcessOne;

  CONST Msg = StubUtils.Message;
  VAR h := NewHandle(wr, Atom.ToText(qid.intf), c);
      used_id: M3AST_AS.USED_ID := NEW(M3AST_AS.USED_ID).init();
      def_id: M3AST_AS.DEF_ID;
      cu : M3AST_AS.Compilation_Unit;
      ts: M3AST_AS.TYPE_SPEC; 
  BEGIN
    Debug.Out("OneStub processing " & Atom.ToText(qid.intf) & "." &
      Atom.ToText(qid.item) );

    used_id.lx_symrep := M3CId.Enter(Atom.ToText(qid.item));
    IF M3Context.FindExact(c, Atom.ToText(qid.intf), 
                           M3CUnit.Type.Interface, cu) THEN
      RETURN ProcessOne()
    ELSIF M3Context.FindExact(c, 
                           Atom.ToText(qid.intf), 
                           M3CUnit.Type.Interface_gen_ins, cu) THEN

      cu := NARROW(cu.as_root,M3AST_AS.Interface_gen_ins).sm_ins_comp_unit;
      RETURN ProcessOne()
    ELSE
      RETURN 1
    END
  END OneStubScm;

PROCEDURE AddToList(VAR list : SchemePair.T;
                    qid : Type.Qid;
                    type : SchemeObject.T) =
  BEGIN
    list := 
        Cons(Cons(TypeTranslator.TranslateQid(qid),
                  type),
             list)
  END AddToList;

PROCEDURE ProcessScmObj(h: Handle; 
                        ts: M3AST_AS.TYPE_SPEC;
                        qid: Type.Qid) : Type.T =
  CONST Msg = StubUtils.Message;
  BEGIN
    Debug.Out("Processing " & Atom.ToText(qid.intf) & "." &
      Atom.ToText(qid.item) );
    WITH res = ProcessM3Type(h, ts) DO
      Debug.Out("Type-type is " & RTBrand.GetName(TYPECODE(res)));
      RETURN res
    END
  END ProcessScmObj;

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
    EVAL astTable.put(M3CStdTypes.Text(), Type.text);
    EVAL astTable.put(M3CStdTypes.Cardinal(), Type.cardinal);
    EVAL astTable.put(M3CStdTypes.Boolean(), Type.boolean);
    EVAL astTable.put(M3CStdTypes.Mutex(), Type.mutex);

    CM3Extensions.InitAstTable(astTable);
  END InitAstTable;

PROCEDURE ProcessM3Type(h: Handle; m3type: M3AST_AS.M3TYPE): Type.T =
  VAR ts: M3AST_AS.TYPE_SPEC;
  BEGIN
    M3CTypesMisc.GetTYPE_SPECFromM3TYPE(m3type, ts);
    RETURN ProcessTypeSpec(h, ts);
  END ProcessM3Type;

PROCEDURE FillInException(h : Handle;
                          sm_def : M3AST_AS.Exc_id;
                          item : Atom.T;
                          VAR exception : Type.Exception) =

  VAR  argType: Type.T;
  BEGIN
    exception := NEW(Type.Exception);
    exception.qid := NEW(Type.Qid);
    WITH qid = exception.qid DO
      qid.intf := Atom.FromText(M3CId.ToText(
                                    sm_def.tmp_unit_id.lx_symrep));
      qid.item := item;
    END;
    IF sm_def.tmp_type = NIL THEN
      exception.arg := NIL;
    ELSE
      argType := ProcessM3Type(h, sm_def.tmp_type);
      exception.arg := argType
    END
  END FillInException;
                          
PROCEDURE ProcessTypeSpec(h: Handle; ts: M3AST_AS.TYPE_SPEC): Type.T =
  VAR r: REFANY;
  VAR t: Type.T;
      
  PROCEDURE DoArray_type(at : M3AST_AS.Array_type) =
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
        END DoArray_type;

  PROCEDURE DoEnumeration_type(enum : M3AST_AS.Enumeration_type) =

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
    END DoEnumeration_type;

  PROCEDURE DoProcedure_type(proc : M3AST_AS.Procedure_type) =
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
        

        IF formalParam.as_default # NIL THEN
          Debug.Out("formal type: " &
            RTBrand.GetName(TYPECODE(formalParam)));
          Debug.Out("formal name: " &
            Atom.ToText(formals[i].name));
          formals[i].default := AstToVal.ProcessExp(
                                    h,
                                    NARROW(formalParam, 
                                           M3AST_AS.Formal_param)
          .as_default);

          WITH def = NARROW(formalParam, M3AST_AS.Formal_param).as_default DO
            Debug.Out("formal.as_default type : "&
              RTBrand.GetName(TYPECODE(def)));

            Debug.Out("formal.as_default.sm_exp_value type : " &
              RTBrand.GetName(TYPECODE(def.sm_exp_value)));
            
            Debug.Out("formal.as_default.sm_exp_type_spec type : " &
              RTBrand.GetName(TYPECODE(def.sm_exp_type_spec)));
(*
            Debug.Out("formal.as_default.as_callexp type : " &
              RTBrand.GetName(TYPECODE(def.as_callexp)));
*)
            
            
          END

        END;
        
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
          BEGIN
            signature.raises := NEW(REF ARRAY OF 
            Type.Exception, nRaises);
            FOR i := 0 TO nRaises-1 DO
              EVAL SeqM3AST_AS_Qual_used_id.Next(iter, raisee);
              FillInException(h, 
                              raisee.as_id.sm_def,
                              Atom.FromText(M3CId.ToText(raisee.as_id.lx_symrep)),
                              signature.raises[i])
            END
          END;
        | M3AST_AS.Raisees_any => 
        ELSE signature.raises := NEW(REF ARRAY OF Type.Exception, 0)
        END;
      END;
      t := NEW(Type.Procedure, sig := signature);
    END DoProcedure_type;

  BEGIN
    IF h.astMap.get(ts, r) THEN
      RETURN NARROW(r, Type.T);
    ELSE
      TYPECASE ts OF
      |  M3AST_AS.Real_type => t := Type.real;
      |  M3AST_AS.LongReal_type => t := Type.longreal;
      |  M3AST_AS.Extended_type => t := Type.extended;
      |  M3AST_AS.Integer_type => t := Type.integer;
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
      |  M3AST_AS.Array_type (at) => DoArray_type(at)
        
      |  M3AST_AS.Enumeration_type (enum) =>
        DoEnumeration_type(enum)
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
        DoProcedure_type(proc)
      ELSE 
        WITH res = CM3Extensions.ProcessTypeSpec(h, ts) DO
          IF res = NIL THEN
            StubUtils.Die("AstToType.ProcessTypeSpec: unrecognized Modula-3 construct : " & RTBrand.GetName(TYPECODE(ts)))
          ELSE
            t := res
          END
        END
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
    t.overrides := ProcessOverrides(o.as_override_s);
    RETURN t;
  END ProcessObject;

PROCEDURE ProcessOverrides(o : SeqM3AST_AS_Override.T):
  REF ARRAY OF Type.Override =
  VAR nOverrides:= SeqM3AST_AS_Override.Length(o);
      overrides: REF ARRAY OF Type.Override;
      iter := SeqM3AST_AS_Override.NewIter(o);
      astOverride: M3AST_AS.Override;
  BEGIN
    overrides := NEW(REF ARRAY OF Type.Override, nOverrides);
    FOR i := 0 TO nOverrides-1 DO
      EVAL SeqM3AST_AS_Override.Next(iter, astOverride);
      overrides[i] := NEW(Type.Override);
      overrides[i].name := 
          Atom.FromText(M3CId.ToText(astOverride.as_id.lx_symrep));
      IF astOverride.as_default # NIL THEN
        overrides[i].default := ProcessMethodDefault(astOverride.as_default)
      END
      (*overrides[i].default := AstToVal.Val(
        astOverride.vINIT_ID.sm_init_exp,
        overrides[i].type).sm_exp_value   *)
    END;
    RETURN overrides;
  END ProcessOverrides;

PROCEDURE ProcessMethodDefault(as_default : M3AST_AS_F.Exp_used_id) : Type.MethodDefault =
  BEGIN
    WITH used_id = as_default.vUSED_ID,
         intf = Atom.FromText(M3CId.ToText(used_id.sm_def.tmp_unit_id.lx_symrep)),
         item = Atom.FromText(M3CId.ToText(used_id.lx_symrep)) DO
      RETURN NEW(Type.MethodDefault1,
                 qid := NEW(Type.Qid, intf:= intf, item:= item))
    END
  END ProcessMethodDefault;

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
        methods[i].default := ProcessMethodDefault(astMethod.as_default)
        (*methods[i].default := AstToVal.Val(
          astMethod.vINIT_ID.sm_init_exp,
          methods[i].type).sm_exp_value   *)
      END;
    END;
    RETURN methods;
  END ProcessMethods;

BEGIN
END AstToType.


