MODULE M3CTypeSpec;

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

IMPORT AST, M3AST_LX, M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_Qual_used_id, SeqM3AST_AS_M3TYPE, SeqM3AST_AS_Var_id,
    SeqM3AST_AS_FORMAL_ID, SeqM3AST_AS_Field_id, SeqM3AST_AS_Enum_id;

IMPORT ASTWalk;
IMPORT M3Error, M3Assert; 
IMPORT M3CStdTypes; 
IMPORT M3ASTNext;
IMPORT M3CTypesMisc;
IMPORT M3CExpTypeSpec;
IMPORT M3CBackEnd; (* for enumeration-ids *)

EXCEPTION UnresolvedName;
(* We have to be prepared for type names that were not defined, in which
case the sm_type_spec for a declaration of that type remains Unset.
This exception is used to handle this (unusual case).
*)

(* The only difficulty in setting the sm_type_spec attribute is Named_types
   and Ids with types implied by their initialising expressions;
   for all others, it is manifest in the AST.  All Named_types have been
   resolved to their DEF_IDs (a Type_Id); however since a Type_id is a
   TYPED_ID also it should be obvious that we cant rely on sm_type_spec having
   been set, in the case of forward references.  The solution is to make
   two passes over tree. On the first pass all TYPE_DECL nodes are examined
   and a temporary attribute is set which relates the Type_id to its rhs, 
   to allow the chain to be followed in the second pass.  This is handled
   in M3CTmpAtt.Set. On the second pass all members of TYPED_ID, which are
   explicitly typed, are handled, with Named_types being resolved by
   following the chain until reaching the concrete declaration.

   A third pass is used to set the attribute for declarations typed by
   their initialising expressions, which requires the sm_exp_type_spec
   to have been computed. It also computes the type_spec of overridden
   methods, which relies on names and revelations having been resolved.
*)

PROCEDURE Handler(h: M3AST_AS.Handler) RAISES {UnresolvedName}=
  VAR
    handlerId: M3AST_AS.Handler_id;
    count: CARDINAL;
    iterExc: SeqM3AST_AS_Qual_used_id.Iter;
    qualId: M3AST_AS.Qual_used_id;
    usedId: M3AST_AS.Used_def_id;
    typeSpec: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF h.as_id # NIL THEN
      handlerId := h.as_id;
    ELSE
      handlerId := NIL;
    END;
    count := 0;
    iterExc := SeqM3AST_AS_Qual_used_id.NewIter(h.as_qual_id_s);
    WHILE SeqM3AST_AS_Qual_used_id.Next(iterExc, qualId) DO
      usedId := qualId.as_id;
      TYPECASE usedId.sm_def OF
      | NULL =>
          (* nothing can be done *)
      | M3AST_AS.Exc_id(excId) =>
          IF (count = 0) AND (handlerId # NIL) THEN
            (* now we can get via tmp_type to the TYPE_SPEC *)
            IF TypeIsSpecified(excId.tmp_type, typeSpec) THEN
              handlerId.sm_type_spec := typeSpec;
            ELSE
              M3Error.ReportWithId(usedId,
                  "exception %s does not have an argument", excId.lx_symrep);
            END; (* if *)
          END;
      ELSE
        M3Error.ReportWithId(usedId,
           "%s is not an exception", usedId.lx_symrep);
      END; (* if *)
      INC(count);
    END; (* while *)
    IF (count > 1) AND (handlerId # NIL) THEN
      M3Error.Report(handlerId, "variable not allowed with exception list");
    END;
  END Handler;


PROCEDURE Tcase(t: M3AST_AS.Tcase) RAISES {UnresolvedName}=
  VAR
    tcaseId: M3AST_AS.Tcase_id;
    count: CARDINAL;
    iterTypes: SeqM3AST_AS_M3TYPE.Iter;
    type: M3AST_AS.M3TYPE;
    ts: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    IF t.as_id # NIL THEN
      tcaseId := t.as_id;
    ELSE
      tcaseId := NIL;
    END;
    count := 0;
    iterTypes := SeqM3AST_AS_M3TYPE.NewIter(t.as_type_s);
    WHILE SeqM3AST_AS_M3TYPE.Next(iterTypes, type) DO
      TYPECASE type OF <*NOWARN*>
      | M3AST_AS.Named_type(namedType) =>
          GetTYPE_SPEC_For_Named_type(namedType, ts);
      | M3AST_AS.TYPE_SPEC(typeSpec) =>
          ts := typeSpec;
      | M3AST_AS.Bad_M3TYPE =>
          ts := NIL;
      END; (* if *)
      IF (count = 0) AND (tcaseId # NIL) THEN
        tcaseId.sm_type_spec := ts;
      END;
      INC(count);
    END;
    IF (count > 1) AND (tcaseId # NIL) THEN
      M3Error.Report(t, "variable not allowed with type list");
    END; (* if *)
  END Tcase;


PROCEDURE SetPass1(an: AST.NODE) RAISES {}=
  VAR
    as_type_void: M3AST_AS.M3TYPE_NULL;
    type_spec: M3AST_SM.TYPE_SPEC_UNSET;
    typed_id: M3AST_AS.TYPED_ID;
    var_id: M3AST_AS.Var_id;
    formal_id: M3AST_AS.FORMAL_ID;
    field_id: M3AST_AS.Field_id;
    iterVar: SeqM3AST_AS_Var_id.Iter;
    iterField: SeqM3AST_AS_Field_id.Iter;
  BEGIN  
    TRY
      type_spec := NIL;  (* default *)

      TYPECASE an OF
      | M3AST_AS.Const_decl(constDecl) =>
            IF TypeIsSpecified(constDecl.as_type, type_spec) AND
                NOT constDecl.as_id.tmp_recursive THEN
              constDecl.as_id.sm_type_spec := type_spec;
            ELSE
              (* implied by as_exp *)
            END; (* if *)

      | M3AST_AS.TYPE_DECL(type_decl) =>
          TYPECASE type_decl OF <*NOWARN*>
          | M3AST_AS.Concrete_decl(concreteDecl) =>
              typed_id := concreteDecl.as_id;
          | M3AST_AS.Subtype_decl(subtypeDecl) =>
              typed_id := subtypeDecl.as_id;
          END;
          as_type_void := type_decl.as_type;
          IF ISTYPE(type_decl, M3AST_AS.Subtype_decl) THEN
            (* level of indirection via an Opaque_type *)
            M3Assert.Check(ISTYPE(as_type_void, M3AST_AS.Opaque_type));
            typed_id.sm_type_spec := as_type_void;
            (* Now check the rhs of the <: *)
            as_type_void := NARROW(as_type_void, M3AST_AS.Opaque_type).as_type;
          END; (* if *)
          M3Assert.Check(TypeIsSpecified(as_type_void, type_spec));
          (* The above may raise Unresolvedname so the following may not *)
          IF ISTYPE(type_decl, M3AST_AS.Subtype_decl) THEN
            IF NOT M3CTypesMisc.IsRef(type_spec) THEN
              M3Error.Report(NARROW(type_decl, M3AST_AS.Subtype_decl).as_type,
                  "type following \'<:\' is not a reference type");
            END; (* if *)
          ELSIF NOT typed_id.tmp_recursive THEN
            typed_id.sm_type_spec := type_spec;
          END;

      | M3AST_AS.Var_decl(varDecl) =>
            IF TypeIsSpecified(varDecl.as_type, type_spec) THEN
              iterVar := SeqM3AST_AS_Var_id.NewIter(varDecl.as_id_s);
              WHILE SeqM3AST_AS_Var_id.Next(iterVar, var_id) DO
                IF NOT var_id.tmp_recursive THEN
                  var_id.sm_type_spec := type_spec;
                END;
              END; (* while *)
            END;

      | M3AST_AS.Formal_param(formalParam) =>
          (* very like a Var_decl, where is Landin? *)
          VAR
            expVoid := formalParam.as_default;
          BEGIN
            IF TypeIsSpecified(formalParam.as_formal_type, type_spec) THEN END;
            (* check for no default on VAR parameters and set up typeSpec *)
            VAR
              first := TRUE;
              iterFormal :=
                  SeqM3AST_AS_FORMAL_ID.NewIter(formalParam.as_id_s);
            BEGIN
              WHILE SeqM3AST_AS_FORMAL_ID.Next(iterFormal, formal_id) DO
                IF first THEN
                  IF ISTYPE(formal_id, M3AST_AS.F_Var_id) AND
                      expVoid # NIL THEN
                    M3Error.Report(formalParam,
                        "VAR parameter cannot have a default value");
                  END;
                  IF type_spec = NIL THEN EXIT END;
                  first := FALSE;
                END; (* if *)
                formal_id.sm_type_spec := type_spec;
              END; (* while *)
            END;
          END;

      | M3AST_AS.Fields(fields) =>
            IF TypeIsSpecified(fields.as_type, type_spec) THEN
              iterField := SeqM3AST_AS_Field_id.NewIter(fields.as_id_s);
              WHILE SeqM3AST_AS_Field_id.Next(iterField, field_id) DO
                field_id.sm_type_spec := type_spec;
              END; (* while *)
            END;


      | M3AST_AS.Exc_decl(excDecl) =>
            IF NOT TypeIsSpecified(excDecl.as_type, type_spec) THEN
              type_spec := M3CStdTypes.Void();
            END;
            excDecl.as_id.sm_type_spec := type_spec;

      | M3AST_AS.Proc_decl(procDecl) =>
          (* Very easy *)
            procDecl.as_id.sm_type_spec := procDecl.as_type;
            procDecl.as_type.sm_def_id := procDecl.as_id;
          
      | M3AST_AS.Tcase(tcase) =>
          Tcase(tcase);

      | M3AST_AS.Handler(handler) =>
          Handler(handler);

      | M3AST_AS.Named_type(namedType) =>
            GetTYPE_SPEC_For_Named_type(namedType, type_spec);
            namedType.sm_type_spec := type_spec;
          
      | M3AST_AS.Enumeration_type(t) =>
          SetEnumIds(t);

      | M3AST_AS.Procedure_type(procType) =>
            IF procType.sm_def_id = M3AST_SM.UNSET_DEF_ID() THEN
              procType.sm_def_id := NIL;
            END; (* if *)

      | M3AST_AS.Method(method) =>
            NARROW(method.as_type, M3AST_AS.Procedure_type).sm_def_id :=
                method.as_id;
            method.as_id.sm_type_spec := method.as_type;
            method.as_id.vREDEF_ID.sm_int_def := method.as_id;


      (* others all get their types implicitly *)         
      ELSE
      END; (* case *)
    EXCEPT
    | UnresolvedName =>
        (* leave things the way they are *)
    END;
  END SetPass1;


PROCEDURE SetEnumIds(enum_type: M3AST_AS.Enumeration_type) RAISES {}=
  VAR
    iter := SeqM3AST_AS_Enum_id.NewIter(enum_type.as_id_s);
    e: M3AST_AS.Enum_id;
    ord: CARDINAL := 0;
  BEGIN
    WHILE SeqM3AST_AS_Enum_id.Next(iter, e) DO
      e.sm_type_spec := enum_type;    
      M3Assert.Check(M3CBackEnd.Val(ord, enum_type, e.vCCV_ID.sm_exp_value) =
          M3CBackEnd.NumStatus.Valid);
      INC(ord);
    END; (* while *)
    enum_type.sm_num_elements := ord;
  END SetEnumIds;


PROCEDURE TypeIsSpecified(
    t: M3AST_AS.M3TYPE_NULL;
    VAR (*out*) ts: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {UnresolvedName}=
  BEGIN
    TYPECASE t OF <*NOWARN*>
    | NULL =>
        RETURN FALSE;  (* not given *)
    | M3AST_AS.Named_type(namedType) =>
        GetTYPE_SPEC_For_Named_type(namedType, ts);
    | M3AST_AS.TYPE_SPEC(typeSpec) =>
        ts := typeSpec;
    | M3AST_AS.Bad_M3TYPE =>
        ts := NIL;
    END; (* if *)
    RETURN TRUE;
  END TypeIsSpecified;


PROCEDURE GetTYPE_SPEC_For_Named_type(
    t: M3AST_AS.Named_type;
    VAR (*out*) type_spec: M3AST_SM.TYPE_SPEC_UNSET )
    RAISES {UnresolvedName}=
  VAR
    used_id: M3AST_AS.Used_def_id;
    def_id: M3AST_AS.DEF_ID;
  BEGIN
    (* This sets t.sm_type_spec and returns the same value in 'type_spec'.
     In the case of a chain on names, it calls itself recursively. *)

    IF t.sm_type_spec # NIL THEN
      type_spec := t.sm_type_spec;
    ELSE
      used_id := t.as_qual_id.as_id;
      (* look at sm_def of this used_id *)
      IF used_id.sm_def = NIL THEN
        RAISE UnresolvedName;
      ELSE
        def_id := used_id.sm_def;
        IF def_id.tmp_recursive THEN RETURN END;

        TYPECASE def_id OF
        | NULL =>
        | M3AST_AS.Type_id(typeId) =>
            IF typeId.sm_type_spec # NIL THEN
              (* this is the answer! *)
              type_spec := typeId.sm_type_spec;
            ELSE
              (* either a forward reference or a named type *)
              TYPECASE typeId.tmp_type OF
              | NULL =>
                  RETURN;
              | M3AST_AS.Named_type(namedType) =>
                  GetTYPE_SPEC_For_Named_type(namedType, type_spec);
              | M3AST_AS.TYPE_SPEC(typeSpec) =>
                  type_spec := typeSpec;
              ELSE
                RETURN; (* 'BadM3TYPE' *)
              END; (* if *)
            END; (* if *)
            (* set attribute for incoming Named_type *)
            t.sm_type_spec := type_spec;
        ELSE
          M3Error.ReportWithId(t, 
              "%s is not a type name", def_id.lx_symrep);
        END; (* if *)
      END; (* if *)
    END; (* if *)
  END GetTYPE_SPEC_For_Named_type;


(* SetPass2 code *)

PROCEDURE GetTYPE_SPECForOverRide(
    ot: M3AST_AS.Object_type;
    override_id: M3AST_AS.Override_id)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  VAR
    iter: M3ASTNext.IterFieldOrMethod;
    method: M3AST_AS.Method;
    field_id: M3AST_AS.Field_id;
    overrideSymrep := override_id.lx_symrep;
    symrep: M3AST_LX.Symbol_rep;
  BEGIN
    IF overrideSymrep = NIL THEN RETURN NIL END;
    iter := M3ASTNext.NewIterFieldOrMethod(ot);
    WHILE M3ASTNext.FieldOrMethod(iter, field_id, method, symrep) DO
      IF overrideSymrep = symrep THEN
        (* check its not a field *)
        IF method = NIL THEN
          M3Error.ReportWithId(override_id,
              "method name \'%s\' clashes with existing field name",
              overrideSymrep);
        ELSE
          (* Is a method; must be the original declaration because the
           'FieldOrMethod' iterator ignores overrides *)
          (* set sm_int_def also *)
          override_id.vREDEF_ID.sm_int_def := method.as_id;
          RETURN method.as_type;
        END; (* if *)
      END; (* if *)
    END; (* while *)
    (* didnt find it all! *)
    M3Error.ReportWithId(override_id,
        "cannot override \'%s\'; no previous definition",
        overrideSymrep);    
    RETURN NIL;
  END GetTYPE_SPECForOverRide;


PROCEDURE OfOverride(
    override: M3AST_AS.Override)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  BEGIN
    WITH typeSpec = override.as_id.sm_type_spec DO
      IF typeSpec = NIL THEN
        typeSpec := GetTYPE_SPECForOverRide(override.tmp_type, override.as_id);
      END;
      RETURN typeSpec;
    END;
  END OfOverride;



TYPE
  Pass2Closure =
    ASTWalk.Closure OBJECT
      unit: M3AST_AS.UNIT;
    OVERRIDES
      callback := SetPass2;
    END; (* object *)


PROCEDURE NewSetPass2Closure(unit: M3AST_AS.UNIT): ASTWalk.Closure RAISES {}=
  BEGIN
    RETURN NEW(Pass2Closure, unit := unit);
  END NewSetPass2Closure;


PROCEDURE SetPass2(
    cl: Pass2Closure;
    an: AST.NODE;
    <*UNUSED*> vm := ASTWalk.VisitMode.Exit)
    RAISES {}=
  BEGIN
    TYPECASE an OF
    | M3AST_AS.EXP(exp) =>
        M3CExpTypeSpec.Set(exp, cl.unit);
        RETURN;
    ELSE
    END;

    TYPECASE an OF
    | M3AST_AS.Const_decl(constDecl) =>
          IF constDecl.as_type = NIL THEN
            constDecl.as_id.sm_type_spec := constDecl.as_exp.sm_exp_type_spec;
          END; (* if *)

    | M3AST_AS.Var_decl(varDecl) =>
        VAR
          expVoid := varDecl.as_default;
        BEGIN
          IF varDecl.as_type = NIL AND expVoid # NIL THEN
            VAR
              type_spec := expVoid.sm_exp_type_spec;
              iterVar := SeqM3AST_AS_Var_id.NewIter(varDecl.as_id_s);
              var_id: M3AST_AS.Var_id;
              checkIfRecursive := 
                  type_spec # NIL AND
                  type_spec.tmp_unit_id = cl.unit.as_id;
            BEGIN
              WHILE SeqM3AST_AS_Var_id.Next(iterVar, var_id) DO
                IF checkIfRecursive AND NOT var_id.tmp_recursive THEN
                  (* The following may set 'var_id.tmp_recursive' *)
                  EVAL M3CExpTypeSpec.RecursiveVariableType(var_id, type_spec);
                END;
                IF NOT var_id.tmp_recursive THEN
                  var_id.sm_type_spec := type_spec;
                END;
              END; (* while *)
            END;
          END; (* if *)
        END;

    | M3AST_AS.Formal_param(formalParam) =>
        (* very like a Var_decl, where is Landin? *)
        VAR
          expVoid := formalParam.as_default;
        BEGIN
          IF formalParam.as_formal_type = NIL AND expVoid # NIL THEN
            VAR
              type_spec := expVoid.sm_exp_type_spec;
              iterFormal :=
                  SeqM3AST_AS_FORMAL_ID.NewIter(formalParam.as_id_s);
              formal_id: M3AST_AS.FORMAL_ID;
            BEGIN
              WHILE SeqM3AST_AS_FORMAL_ID.Next(iterFormal, formal_id) DO
                formal_id.sm_type_spec := type_spec;
              END; (* while *)
            END;
          END; (* if *)
        END;

    | M3AST_AS.Fields(fields) =>
        VAR
          expVoid := fields.as_default;
        BEGIN
          IF fields.as_type = NIL AND expVoid # NIL THEN
            VAR
              type_spec := expVoid.sm_exp_type_spec;
              iterField := SeqM3AST_AS_Field_id.NewIter(fields.as_id_s);
              field_id: M3AST_AS.Field_id;
            BEGIN
              WHILE SeqM3AST_AS_Field_id.Next(iterField, field_id) DO
                field_id.sm_type_spec := type_spec;
              END; (* while *)
            END;
          END; (* if *)
        END;

    | M3AST_AS.For_st(forSt) =>
          forSt.as_id.sm_type_spec :=
              M3CExpTypeSpec.BaseType(forSt.as_from.sm_exp_type_spec);

    | M3AST_AS.Binding(binding) =>
          binding.as_id.sm_type_spec := binding.as_exp.sm_exp_type_spec;

    | M3AST_AS.Override(override) =>
          IF override.as_id.sm_type_spec = NIL THEN
            EVAL OfOverride(override);
          END; (* if *)

    ELSE
    END (* case *)
  END SetPass2;


BEGIN
END M3CTypeSpec.
