MODULE M3CExpTypeSpec;

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

IMPORT AST, M3AST_AS, M3AST_SM;

IMPORT M3AST_LX_F, M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_Actual, SeqM3AST_AS_M3TYPE, SeqM3AST_AS_EXP;

IMPORT ASTWalk;
IMPORT M3Error;
IMPORT M3CTypesMisc, M3CExpsMisc;
IMPORT M3CStdProcs, M3CStdTypes;
IMPORT M3CDef, M3CTypeSpec;
IMPORT M3CNormType;


(* Map structure, used to keep track of where we are so we can avoid horrible
variable declarations whose implied type depends on themselves e.g.

VAR
  i := i;
  j := k;
  k: [0..BITSIZE(j)] := 0;
*)

TYPE
  MapList = REF RECORD
    next: MapList := NIL;
    list: ARRAY [0..7] OF M3AST_AS.Var_id;
  END;

  Mode = {TreeWalk,            (* Called by tree walker *)
          Recurse,             (* Recursive call, resolve forward reference *)
          RecurseButDontSet};  (* Recursive call, searching for illegal *)
                               (* recursion through a variable *)

  Map = RECORD
    mode := Mode.Recurse;
    count: CARDINAL := 0;
    recursedTo: M3AST_AS.Var_id := NIL;
    unit: M3AST_AS.UNIT_NORMAL;
    entries: MapList := NIL;
  END; (* record *)


PROCEDURE InMap(
    id: M3AST_AS.Var_id;
    add: BOOLEAN;
    VAR map: Map)
    : BOOLEAN
    RAISES {}=
  VAR
    last: MapList := NIL;
    e := map.entries;
    i: CARDINAL := 0;
  BEGIN
    FOR j := 0 TO map.count - 1 DO
      IF e.list[i] = id THEN RETURN TRUE END;
      INC(i);
      IF i > LAST(e.list) THEN i := 0; last := e; e := e.next END;
    END; (* for *)
    IF add THEN
      IF e = NIL THEN
        e := NEW(MapList);
        IF last = NIL THEN map.entries := e ELSE last.next := e END;
      END;
      e.list[i] := id;
      INC(map.count);
    END;
    RETURN FALSE;
  END InMap;


(* Simple utility routines *)

<*INLINE*> PROCEDURE SetComponent(
    e: M3AST_AS.EXP;
    VAR map: Map)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  BEGIN
    IF map.mode = Mode.TreeWalk THEN
      RETURN e.sm_exp_type_spec;
    ELSE
      RETURN InternalSet(e, map);
    END;
  END SetComponent;


PROCEDURE IsUntracedRef(
    ts: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  BEGIN
    RETURN M3CTypesMisc.IsTracedRef(M3CTypesMisc.CheckedUnpack(ts)) IN
        M3CTypesMisc.RefSet{M3CTypesMisc.Ref.Untraced,M3CTypesMisc.Ref.Null};
  END IsUntracedRef;


PROCEDURE IRL(
    typeSpec: M3AST_SM.TYPE_SPEC_UNSET;
    intok := TRUE)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  VAR
    ts := M3CTypesMisc.CheckedUnpack(typeSpec);
  BEGIN
    TYPECASE ts OF
    | M3AST_AS.FLOAT_TYPE =>
        (* Includes null case; result type is argument type *)
        RETURN ts;
    | M3AST_AS.Subrange_type, M3AST_AS.Integer_type =>
        IF intok THEN
          RETURN M3CStdTypes.Integer();
        ELSE
          RETURN NIL;
        END;
    ELSE
      RETURN NIL;
    END;
  END IRL;


(* Exported utility routine *)

PROCEDURE BaseType(
     ts: M3AST_SM.TYPE_SPEC_UNSET)
     : M3AST_SM.TYPE_SPEC_UNSET
     RAISES {}=
   BEGIN
     LOOP
       TYPECASE ts OF
       | M3AST_AS.Integer_type, M3AST_AS.Enumeration_type =>
           RETURN ts; (* includes the NULL case *)
       | M3AST_AS.Packed_type(packedType) =>
           ts := M3CTypesMisc.Unpack(packedType);
           (* loop *)
       | M3AST_AS.Subrange_type(subrangeType) =>
           VAR
             map := Map{unit := subrangeType.tmp_unit_id.sm_spec};
           BEGIN
             ts := InternalSet(subrangeType.as_range.as_exp1, map);
             (* loop *)
           END;
       ELSE
         RETURN NIL;
       END;
     END;
   END BaseType;


(* Look through an ids declaration to discover its type, watching out for
recursion *)

 TYPE
  TypeClosure = ASTWalk.Closure
    OBJECT
      map: Map;
      varId: M3AST_AS.Var_id;
      recursive := FALSE;
    OVERRIDES
      callback := WalkType;
    END;


PROCEDURE RecursionViaType(cl: TypeClosure) RAISES {ASTWalk.Aborted}=
  BEGIN
    M3Error.ReportWithId(cl.varId,
        "recursive declaration of \'%s\'", cl.varId.lx_symrep);
    cl.varId.tmp_recursive := TRUE;
    cl.recursive := TRUE;
    ASTWalk.Abort();
  END RecursionViaType;


PROCEDURE WalkComponentType(
    cl: TypeClosure;
    ts: M3AST_SM.TYPE_SPEC_UNSET)
    RAISES {ASTWalk.Aborted}=
  BEGIN
    IF ts # NIL AND ts.tmp_unit_id = cl.map.unit.as_id AND
        RecursiveType(ts, cl.varId, cl.map) THEN
      cl.recursive := TRUE;
      ASTWalk.Abort();
    END;
  END WalkComponentType;


PROCEDURE WalkType(
    cl: TypeClosure;
    an: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode)
    RAISES {ASTWalk.Aborted}=
  BEGIN
    TYPECASE an OF
    | M3AST_AS.Enumeration_type, M3AST_AS.Object_type,
      M3AST_AS.Procedure_type, M3AST_AS.Ref_type,
      M3AST_AS.Opaque_type =>
        ASTWalk.IgnoreChildren(cl);
    ELSE
      VAR
        usedId: M3AST_AS.USED_ID;
      BEGIN
        IF an.IsA_USED_ID(usedId) THEN
          IF usedId.sm_def # NIL AND
              usedId.sm_def.tmp_unit_id = cl.map.unit.as_id THEN
            TYPECASE usedId.sm_def OF
            | NULL =>
            | M3AST_AS.Var_id(varId) =>
                IF varId = cl.varId THEN
                  (* Recursion! *)
                  RecursionViaType(cl);
                ELSIF InMap(varId, TRUE, cl.map) THEN
                  (* We have already dealt with this one; nothing more to do.
                   This avoids infinite recursion if the type contains
                   a recursive variable other than 'varId'  *)
                ELSE
                  VAR
                    varType := varId.sm_type_spec;
                  BEGIN
                    IF varType = NIL THEN
                      VAR
                        map :=
                            Map{mode := Mode.RecurseButDontSet,
                                unit := cl.map.unit};
                      BEGIN
                        (* Put 'cl.varId' in 'map'; this ensures that if the
                         type of 'varId' depends directly on the type of
                         'cl.varId' we will stop quickly *)
                        EVAL InMap(cl.varId, TRUE, map);
                        varType := GetExp_typeOfId(varId, map);
                        IF map.recursedTo = cl.varId THEN
                          (* Type of 'varId' does directly depend on 'cl.varId'
                           so we have recursion *)
                          RecursionViaType(cl);
                        END;
                      END;
                    END;
                    WalkComponentType(cl, varType);
                  END;
                END;
            | M3AST_AS.Const_id(constId) =>
                <*FATAL ANY*>
                VAR
                  walkExp := NEW(TypeClosure, map := cl.map);
                BEGIN
                  ASTWalk.VisitNodes(constId.vINIT_ID.sm_init_exp, walkExp);
                  IF walkExp.recursive THEN
                    cl.recursive := TRUE; 
                    ASTWalk.Abort();
                  END;
                END;
            | M3AST_AS.Type_id(typeId) =>
               WalkComponentType(cl, typeId.sm_type_spec);
            ELSE
            END;
          END;
        END;
      END;
    END;
  END WalkType;


PROCEDURE RecursiveType(
    ts: M3AST_AS.TYPE_SPEC;
    varId: M3AST_AS.Var_id;
    VAR map: Map)
    : BOOLEAN
    RAISES {}=
  BEGIN
    TYPECASE ts OF
    | M3AST_AS.Subrange_type, M3AST_AS.Array_type,
      M3AST_AS.Record_type, M3AST_AS.Set_type,
      M3AST_AS.Packed_type =>
        <*FATAL ANY*>
        VAR
          cl := NEW(TypeClosure, map := map, varId := varId);
        BEGIN
          ASTWalk.VisitNodes(ts, cl);
          map := cl.map;
          RETURN cl.recursive;
        END;
    ELSE
      RETURN FALSE;
    END;
  END RecursiveType;


<*INLINE*> PROCEDURE RecursiveVariableType(
    varId: M3AST_AS.Var_id;
    ts: M3AST_AS.TYPE_SPEC)
    : BOOLEAN
    RAISES {}=
  BEGIN
    IF varId.tmp_unit_id # ts.tmp_unit_id THEN RETURN FALSE END;
    VAR
      map := Map{unit := varId.tmp_unit_id.sm_spec};
    BEGIN
      RETURN RecursiveType(ts, varId, map);
    END;
  END RecursiveVariableType;


PROCEDURE GetExp_typeOfId(
    t: M3AST_AS.TYPED_ID;
    VAR map: Map)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
(* Note that this is only called if 't.sm_type_spec' is NIL. We know that
'sm_type_spec' should be set up if the id is explicitly typed but can validly
be NIL if the id is typed by an initializing expression and has not been
processed yet. *)
  VAR
    initId: M3AST_SM.INIT_ID;
  BEGIN
    (* Only a member of the INIT_ID class can be implicitly typed.
       It is pointless and dangerous to proceed if the identifier
       has an illegal recursive definition.
       We also handle method overrides here, since they are not
       resolved until pass 2 of M3CTypeSpec. (they need REVEAL).
     *)
    IF t.IsA_INIT_ID(initId) THEN
      IF t.tmp_recursive OR initId.sm_init_exp = NIL THEN RETURN NIL END;
      TYPECASE t OF
      | M3AST_AS.Var_id(varId) =>
          (* Check for horrible recursions via the init expression *)
          IF InMap(varId, TRUE, map) THEN
            map.recursedTo := varId;
            RETURN NIL;
          END;
      ELSE
      END;
      IF map.mode = Mode.TreeWalk THEN map.mode := Mode.Recurse END;
      VAR
        ts := InternalSet(initId.sm_init_exp, map);
      BEGIN
        TYPECASE t OF
        | M3AST_AS.Var_id(varId) =>
            DEC(map.count);
            IF varId = map.recursedTo THEN
              M3Error.ReportWithId(varId, "recursive declaration of \'%s\'",
                  varId.lx_symrep);
              varId.tmp_recursive := TRUE;
              map.recursedTo := NIL;
            END;
            (* Possibility that type depends on size of variable - i.e.
             more nasty recursion. Check it out (unless we are already in
             the middle of a call of 'RecursiveVariableType' in which case
             'map.mode' will be 'RecurseButDontSet': *)
            IF map.mode # Mode.RecurseButDontSet AND
                ts # NIL AND RecursiveVariableType(varId, ts) THEN
              ts := NIL;
            END;
        | M3AST_AS.For_id =>
            ts := BaseType(ts);
        ELSE
        END;
        RETURN ts;
      END;
    ELSE
      TYPECASE t OF
      | M3AST_AS.Override_id(overrideId) =>
          RETURN M3CTypeSpec.OfOverride(overrideId.sm_spec);
      ELSE
        RETURN NIL;
      END;
    END; (* if *)
  END GetExp_typeOfId;


(* Utility for determining if selection is of the form 'T.m' where 'T' is an
object type and 'm' a method *)

PROCEDURE TypeDotMethod(
    b: M3AST_AS.Select;
    rhsType: M3AST_SM.TYPE_SPEC_UNSET;
    VAR (*out*) ts:M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {}=
  VAR
    defId: M3AST_AS.DEF_ID;
  BEGIN
    (* We have to do something tricky for T.m; we want a Procedure_type that
     has sm_def_id that refers to the Type_id for T. First we check if we have
     a T.m *)
    IF NOT M3CExpsMisc.IsId(b.as_exp, defId) THEN RETURN FALSE END;
    TYPECASE defId OF
    | M3AST_AS.Type_id(typeId) =>
        TYPECASE typeId.sm_type_spec OF
        | NULL =>
            RETURN FALSE;
        | M3AST_AS.Object_type, M3AST_AS.Opaque_type =>
            TYPECASE rhsType OF
            | NULL =>
            | M3AST_AS.Procedure_type(procType) =>
                VAR
                  new: M3AST_AS.Procedure_type :=
                      NEW(M3AST_AS.Procedure_type).init();
                BEGIN
                  new.lx_srcpos := procType.lx_srcpos;
                  new.as_formal_param_s := procType.as_formal_param_s;
                  new.sm_def_id := defId;
                  new.as_result_type := procType.as_result_type;
                  new.as_raises := procType.as_raises;
                  new.tmp_unit_id := procType.tmp_unit_id;
                  ts := new;
                END;
            ELSE
            END;
            RETURN TRUE;
        ELSE
          RETURN FALSE;
        END;
    ELSE
      RETURN FALSE;
    END; (* if *)
  END TypeDotMethod;


(* Routine used to discover the type of an actual; used when evaluating the
result type of a polymorphic standard function *)

CONST
  TypeOnly = M3CExpsMisc.ClassSet{M3CExpsMisc.Class.Type};
  ExpOnly = M3CExpsMisc.ClassSet{M3CExpsMisc.Class.Normal};
  ExpOrType =
      M3CExpsMisc.ClassSet{M3CExpsMisc.Class.Normal,M3CExpsMisc.Class.Type};


PROCEDURE GetActual(
    call: M3AST_AS.Call;
    pos: CARDINAL;
    classes: M3CExpsMisc.ClassSet;
    VAR map: Map)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  VAR
    s: SeqM3AST_AS_Actual.T := NIL;
    iter: SeqM3AST_AS_Actual.Iter;
    actual: M3AST_AS.Actual;
    count := 0;
  BEGIN
    TYPECASE call OF
    | M3AST_AS.NEWCall(newcall) => s := newcall.sm_norm_actual_s;
    ELSE
    END;
    IF s = NIL THEN s := call.as_param_s END;
    iter := SeqM3AST_AS_Actual.NewIter(s);
    WHILE SeqM3AST_AS_Actual.Next(iter, actual) DO
      INC(count);
      IF count = pos THEN
        TYPECASE actual.as_exp_type OF <*NOWARN*>
        | M3AST_AS.Bad_M3TYPE =>
        | M3AST_AS.TYPE_SPEC(typeSpec) =>
            IF M3CExpsMisc.Class.Type IN classes THEN
              RETURN typeSpec;
            END;
        | M3AST_AS.EXP(exp) =>
            WITH result = SetComponent(exp, map) DO
              IF M3CExpsMisc.Classify(exp) IN classes THEN
                RETURN result;
              END;
            END;
        END;
        RETURN NIL;
      END;
    END; (* while *)
    RETURN NIL;
  END GetActual;


PROCEDURE InternalSet(
    e: M3AST_AS.EXP;
    VAR map: Map)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {}=
  VAR
    ts: M3AST_SM.TYPE_SPEC_UNSET;
  BEGIN
    ts := e.sm_exp_type_spec;  (* default is no change *)
    IF ts # NIL THEN RETURN ts END; (* already done *)

    TYPECASE e OF <*NOWARN*>
    | M3AST_AS.Bad_EXP =>
        (* leave 'ts' NIL *)

    | M3AST_AS.Integer_literal =>
        ts := M3CStdTypes.Integer();
    | M3AST_AS.Real_literal =>
        ts := M3CStdTypes.Real();
    | M3AST_AS.LongReal_literal =>
        ts := M3CStdTypes.LongReal();
    | M3AST_AS.Extended_literal =>
        ts := M3CStdTypes.Extended();
    | M3AST_AS.Char_literal =>
        ts := M3CStdTypes.Char();
    | M3AST_AS.Text_literal =>
        ts := M3CStdTypes.Text();
    | M3AST_AS.Nil_literal =>
        ts := M3CStdTypes.Null();

    | M3AST_AS.Exp_used_id(exp_used_id) =>
        VAR
          defId := exp_used_id.vUSED_ID.sm_def;
        BEGIN
          TYPECASE defId OF
          | NULL =>
          | M3AST_AS.TYPED_ID(typedId)=>
              ts := typedId.sm_type_spec;
              (* It may be that this id is itself implicitly typed by
               its expression, so we have to recurse (providing the id
               is in the same unit) *)
              IF ts = NIL AND defId.tmp_unit_id = map.unit.as_id THEN
                ts := GetExp_typeOfId(typedId, map);
              END;
          ELSE
            ts := M3CStdTypes.Void();
          END;
        END;

    | M3AST_AS.BINARY(binary) =>
        BEGIN
          TYPECASE binary OF <*NOWARN*>
          (* First the simple cases where the operation alone determines the
           type of the result *)
          | M3AST_AS.Eq, M3AST_AS.Ne, M3AST_AS.Le,
            M3AST_AS.Lt, M3AST_AS.Ge, M3AST_AS.Gt,
            M3AST_AS.In, M3AST_AS.And, M3AST_AS.Or =>
              ts := M3CStdTypes.Boolean();
          | M3AST_AS.Div =>
              ts := M3CStdTypes.Integer();
          | M3AST_AS.Textcat =>
              ts := M3CStdTypes.Text();

          | M3AST_AS.Plus, M3AST_AS.Minus,
            M3AST_AS.Times, M3AST_AS.Rdiv, M3AST_AS.Mod =>
              (* this is optimistic, we have to invoke the subtype
              relation to check the result and we cant do that yet. *)
              VAR
                componentTypeSpec := M3CTypesMisc.CheckedUnpack(
                    SetComponent(binary.as_exp1, map));
                lhsRecursive := map.recursedTo # NIL;
                safe := map.unit.as_unsafe = NIL;
                addressOp := ISTYPE(binary, M3AST_AS.Plus) OR
                    ISTYPE(binary, M3AST_AS.Minus);
              BEGIN
                IF lhsRecursive THEN
                  VAR
                    save := map.recursedTo;
                  BEGIN
                    map.recursedTo := NIL;
                    componentTypeSpec := SetComponent(binary.as_exp2, map);
                    IF NOT safe AND addressOp THEN
                      TYPECASE componentTypeSpec OF
                      | NULL =>
                      | M3AST_AS.Subrange_type, M3AST_AS.Integer_type =>
                          (* Int on rhs is not enough to resolve recursion *)
                          map.recursedTo := save;
                          componentTypeSpec := NIL;
                      ELSE
                      END;
                    END;
                  END;
                END;
                IF componentTypeSpec = NIL THEN
                  (* Leave 'ts' at NIL *)
                ELSIF NOT safe AND addressOp AND
                    IsUntracedRef(componentTypeSpec) THEN
                  IF lhsRecursive THEN
                    IF ISTYPE(binary, M3AST_AS.Minus) THEN
                      ts := M3CStdTypes.Integer();
                    END;
                  ELSE
                    IF ISTYPE(binary, M3AST_AS.Minus) AND
                        IsUntracedRef(SetComponent(binary.as_exp2, map)) THEN
                      ts := M3CStdTypes.Integer();
                    ELSE
                      ts := M3CStdTypes.Address();
                    END;
                  END;
                ELSIF ISTYPE(componentTypeSpec, M3AST_AS.Set_type) THEN
                  ts := componentTypeSpec;
                ELSE
                  ts := IRL(componentTypeSpec, NOT ISTYPE(binary, M3AST_AS.Rdiv));
                END; (* if *)
              END;

          END; (* case *)
        END;

        | M3AST_AS.Select(select) =>
            (* The answer is the type of the field.  There is a fun
             interaction here: we only know if the field is valid (M3CDef)
             after we have computed the type of the lhs, so we must call
             M3CDef to check this and (as a side effect) set the sm_def
             attribute. *)
            EVAL SetComponent(select.as_exp, map);
            (* Type of 'lhs' should now be set; we can use 'M3CDef' *)
            M3CDef.SelectPass2(select);
            (* Selection is a special case; type of 'as_exp2' cannot be
             already known because it depends on the selection being
             resolved, and we have only just done that *)
            WITH ts2 = InternalSet(select.as_id, map) DO
              IF NOT TypeDotMethod(select, ts2, ts) THEN
                ts := ts2;
              END;
            END;

    | M3AST_AS.UNARY(unary) =>
          TYPECASE unary OF <*NOWARN*>
          | M3AST_AS.Deref =>
              TYPECASE M3CTypesMisc.Concrete(M3CTypesMisc.CheckedUnpack(
                  SetComponent(unary.as_exp, map))) OF
              | NULL =>
              | M3AST_AS.Ref_type(rt) =>
                  M3CTypesMisc.GetTYPE_SPECFromM3TYPE(rt.as_type, ts);
              ELSE
                M3Error.Report(e, "illegal dereference");
              END;
          | M3AST_AS.Not =>
              ts := M3CStdTypes.Boolean();
          | M3AST_AS.Unaryplus, M3AST_AS.Unaryminus =>
              ts := IRL(SetComponent(unary.as_exp, map));
          END; (* case *)

    | M3AST_AS.Call(call) =>
        VAR
          pf: M3CStdProcs.T;
          polymorphicResult := M3CStdProcs.IsStandardCall(call, pf) AND
             pf IN M3CStdProcs.PolymorphicResult;
        BEGIN
          IF NOT polymorphicResult THEN
            (* We set up the type of the call now just in case this is part of
             a recursive declaration in which the type of one of args depends
             on the type of the call e.g CONST N = BYTESIZE(REF[0..N]) *)
            TYPECASE SetComponent(call.as_callexp, map) OF
            | NULL =>
            | M3AST_AS.Procedure_type(procType) =>
                IF procType.as_result_type # NIL THEN
                  M3CTypesMisc.GetTYPE_SPECFromM3TYPE(
                      procType.as_result_type, ts);
                ELSE
                  ts := M3CStdTypes.Void();
                END; (* if *)
            ELSE
            END; (* typecase *)
          ELSE
            CASE pf OF <*NOWARN*>
            | M3CStdProcs.T.New =>
                ts := M3CTypesMisc.CheckedUnpack(
                    GetActual(call, 1, TypeOnly, map));
                
            | M3CStdProcs.T.Abs =>
                ts := IRL(GetActual(call, 1, ExpOnly, map));

            | M3CStdProcs.T.Max, M3CStdProcs.T.Min =>
                ts := M3CTypesMisc.CheckedUnpack(
                    GetActual(call, 1, ExpOnly, map));
                IF map.recursedTo # NIL THEN
                  map.recursedTo := NIL;
                  ts := M3CTypesMisc.CheckedUnpack(
                      GetActual(call, 2, ExpOnly, map));
                END;
                TYPECASE ts OF
                | M3AST_AS.FLOAT_TYPE =>
                    (* Includes NIL case; result type is argument type *)
                ELSE
                  ts := BaseType(ts);
                END;

            | M3CStdProcs.T.First, M3CStdProcs.T.Last =>
                VAR
                  actualTypeSpec := M3CTypesMisc.CheckedUnpack(
                      GetActual(call, 1, ExpOrType, map));
                  index: M3AST_SM.TYPE_SPEC_UNSET;
                BEGIN
                  TYPECASE actualTypeSpec OF
                  | M3AST_AS.Integer_type, M3AST_AS.Subrange_type,
                    M3AST_AS.FLOAT_TYPE,
                    M3AST_AS.Enumeration_type =>
                      (* Result type is argument type; NIL case harmless *)
                      ts := actualTypeSpec;
                  | M3AST_AS.Array_type(arrayType) =>
                      CASE M3CTypesMisc.Index(arrayType, index) OF
                      | M3CTypesMisc.Ix.Open =>
                          ts := M3CStdTypes.Integer();
                      | M3CTypesMisc.Ix.Ordinal =>
                          ts := index;
                      ELSE
                      END;
                  ELSE
                  END; (* if *)
                END;

            | M3CStdProcs.T.Float =>
                (* if there is a second (type) argument, that is
                the type, else it is REAL. *)
                VAR 
                  actualTypeSpec := M3CTypesMisc.CheckedUnpack(
                      GetActual(call, 2, TypeOnly, map));
                BEGIN
                  TYPECASE actualTypeSpec OF
                  | NULL => ts := M3CStdTypes.Real();
                  | M3AST_AS.FLOAT_TYPE =>
                      ts := actualTypeSpec;
                  ELSE    
                  END; (* typecase *)
                END;

            | M3CStdProcs.T.Val, M3CStdProcs.T.Narrow =>
                ts := M3CTypesMisc.CheckedUnpack(
                    GetActual(call, 2, TypeOnly, map));

            | M3CStdProcs.T.Loophole =>
                ts := GetActual(call, 2, TypeOnly, map);

            | M3CStdProcs.T.Subarray =>
                TYPECASE M3CTypesMisc.CheckedUnpack(
                    GetActual(call, 1, ExpOnly, map)) OF
                | NULL =>
                | M3AST_AS.Array_type(arrType) =>
                    VAR
                      newArrType: M3AST_AS.Array_type :=
                          NEW(M3AST_AS.Array_type).init();
                    BEGIN
                      newArrType.as_indextype_s :=
                          SeqM3AST_AS_M3TYPE.Null;
                      newArrType.as_elementtype :=
                          arrType.sm_norm_type.as_elementtype;
                      M3CNormType.Set(newArrType);  (* normalise *)
                      ts := newArrType;
                    END;
                ELSE
                END;                 

            END; (* case - of polymorphic functions *)
          END; (* if not polymorphic *)
        END;

    | M3AST_AS.Constructor(cons) =>
        M3CTypesMisc.GetTYPE_SPECFromM3TYPE(cons.as_type, ts);

    | M3AST_AS.Index(index) =>
        VAR
          indices := 0;
          iterExps := SeqM3AST_AS_EXP.NewIter(index.as_exp_s);
          indexExp: M3AST_AS.EXP;
          arrType: M3AST_AS.Array_type := NIL;
        BEGIN
          (* The type of the index expression is type of the (normalised)
           element. It is legal for the array base to have REF Array_type. *)
          IF NOT M3CTypesMisc.Indexable(
              SetComponent(index.as_array, map), arrType) THEN
            M3Error.Report(index.as_array, "expression is not indexable");
          ELSIF arrType # NIL THEN
            WHILE SeqM3AST_AS_EXP.Next(iterExps, indexExp) DO
              INC(indices);
            END; (* while *)
            arrType := arrType.sm_norm_type;
            LOOP
              M3CTypesMisc.GetTYPE_SPECFromM3TYPE(arrType.as_elementtype, ts);
              IF indices <= 1 THEN EXIT END;
              IF M3CTypesMisc.Indexable(ts, arrType) THEN
                IF arrType = NIL THEN EXIT END;
                DEC(indices);
              ELSE
                M3Error.Report(index.as_array,
                    "too many index expressions for array type");
                ts := NIL;
                EXIT;
              END; (* if *)
            END; (* loop *)
          END;
        END;

    END; (* case *)
    IF map.mode # Mode.RecurseButDontSet THEN e.sm_exp_type_spec := ts END;
    RETURN ts;
  END InternalSet;


PROCEDURE Set(exp: M3AST_AS.EXP; unit: M3AST_AS.UNIT) RAISES {}=
  VAR
    map := Map{mode := Mode.TreeWalk, unit := unit};
  BEGIN
    EVAL InternalSet(exp, map);
  END Set;


BEGIN
END M3CExpTypeSpec.
