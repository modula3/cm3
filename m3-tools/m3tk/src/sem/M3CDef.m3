MODULE M3CDef;

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

IMPORT AST, M3AST_AS,  M3AST_SM, ASTWalk;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_Binding, SeqM3AST_AS_STM, SeqM3AST_AS_Actual,
    SeqM3AST_AS_CONS_ELEM;
IMPORT M3Context;

IMPORT M3CScope, M3Error, (* M3Assert, *) M3CSearch, M3CTypesMisc, M3CExpsMisc;


PROCEDURE PopWithBindings(
    bindings: SeqM3AST_AS_Binding.T;
    i: SeqM3AST_AS_Binding.Iter)
    RAISES {}=
  VAR
    b: M3AST_AS.Binding;
  BEGIN
    IF SeqM3AST_AS_Binding.Next(i, b) THEN
      PopWithBindings(bindings, i);
      M3CScope.DefId(b.as_id, M3CScope.Change.Exit);
    END; (* if *)
  END PopWithBindings;


PROCEDURE For(cl: ASTWalk.Closure; f: M3AST_AS.For_st) RAISES {}=
  VAR
    iter: SeqM3AST_AS_STM.Iter;
    stm: M3AST_AS.STM;
  BEGIN
    ASTWalk.IgnoreChildren(cl);
    ASTWalk.ModeVisitNodes(		 <* NOWARN *>
        f.as_from, SetPass1Closure(),
        ASTWalk.OnEntryAndExit);
    ASTWalk.ModeVisitNodes(              <* NOWARN *>
        f.as_to, SetPass1Closure(),
        ASTWalk.OnEntryAndExit);
    IF f.as_by # NIL THEN
      ASTWalk.ModeVisitNodes(            <* NOWARN *>
          f.as_by, SetPass1Closure(),
          ASTWalk.OnEntryAndExit);
    END;
    M3CScope.DefId(f.as_id, M3CScope.Change.Enter);
    iter := SeqM3AST_AS_STM.NewIter(f.as_stm_s);
    WHILE SeqM3AST_AS_STM.Next(iter, stm) DO
      ASTWalk.ModeVisitNodes(            <* NOWARN *>
          stm, SetPass1Closure(),
          ASTWalk.OnEntryAndExit);
    END; (* while *)
    M3CScope.DefId(f.as_id, M3CScope.Change.Exit);
  END For;

PROCEDURE SetPass1Closure(): ASTWalk.Closure RAISES {}=
  TYPE T = ASTWalk.Closure OBJECT OVERRIDES  callback := SetPass1 END;
  BEGIN
    RETURN NEW(T).init();
  END SetPass1Closure;


PROCEDURE SetPass1(cl: ASTWalk.Closure;
    a: AST.NODE; 
    e: ASTWalk.VisitMode)
    RAISES {}=
  VAR
    m: M3CScope.Change;
  BEGIN
    IF e = ASTWalk.VisitMode.Entry THEN
      m := M3CScope.Change.Enter;
    ELSE
      m := M3CScope.Change.Exit;
    END;
    TYPECASE a OF
    | M3AST_AS.Compilation_Unit(cu) =>
          (* If is the standard unit we call 'M3CScope.Standard' instead of 
          'M3CScope.CompUnit' *)
          IF cu = M3Context.Standard() THEN 
            IF m = M3CScope.Change.Enter THEN M3CScope.Standard(cu) END;
          ELSE
            M3CScope.CompilationUnit(cu, m) 
          END;
          (* We dont want to try to resolve imports/exports, so we truncate
          this walk and start a recursive walk of the block.  *)
          IF m = M3CScope.Change.Enter THEN
            ASTWalk.IgnoreChildren(cl);
            ASTWalk.ModeVisitNodes(      <* NOWARN *>
                NARROW(cu.as_root, M3AST_AS.UNIT_NORMAL).as_block,
                SetPass1Closure(), ASTWalk.OnEntryAndExit);
          END;

    | M3AST_AS.Proc_decl(procDecl) =>
          IF m = M3CScope.Change.Enter AND procDecl.as_body # NIL THEN
              ASTWalk.IgnoreChildren(cl);
              (* Resolve signature before entering procedure scope *)
            ASTWalk.ModeVisitNodes(procDecl.as_type,       <* NOWARN *>
                SetPass1Closure(), ASTWalk.OnEntryAndExit);
            (* Now enter procedure scope and resolve body *)
            M3CScope.Procedure(procDecl, m);
            ASTWalk.ModeVisitNodes(procDecl.as_body,       <* NOWARN *>
                SetPass1Closure(), ASTWalk.OnEntryAndExit);
          ELSE
            (* Covers 'Enter' if no body and both 'Exit' cases *)
            M3CScope.Procedure(procDecl, m);
          END;

    | M3AST_AS.Method(meth) =>
        M3CScope.Method(meth, m);
    | M3AST_AS.Block(block) =>
        M3CScope.Block(block, m);
    | M3AST_AS.For_st(for_st) =>
        IF m = M3CScope.Change.Enter THEN For(cl, for_st) END;
    | M3AST_AS.Handler_id =>
        IF m = M3CScope.Change.Enter THEN
          M3CScope.DefId(a, M3CScope.Change.Enter);
        END;
    | M3AST_AS.Handler(handler) =>
          IF m = M3CScope.Change.Exit AND handler.as_id # NIL THEN
            M3CScope.DefId(handler.as_id, M3CScope.Change.Exit);
          END;

    | M3AST_AS.Tcase_id =>
        IF m = M3CScope.Change.Enter THEN
          M3CScope.DefId(a, M3CScope.Change.Enter);
        END;
    | M3AST_AS.Tcase(tcase) =>
          IF m = M3CScope.Change.Exit AND tcase.as_id # NIL THEN
            M3CScope.DefId(tcase.as_id, M3CScope.Change.Exit);
          END; (* if *)

    | M3AST_AS.Binding(binding) =>
        IF m = M3CScope.Change.Enter THEN
            ASTWalk.IgnoreChildren(cl);
            (* Resolve expression being bound in outer scope *)
            ASTWalk.ModeVisitNodes(binding.as_exp,     <* NOWARN *>
                SetPass1Closure(), ASTWalk.OnEntryAndExit);
            (* Then add name being bound to scope *)
            M3CScope.DefId(binding.as_id, M3CScope.Change.Enter);
        END; (* if *)
    | M3AST_AS.With_st(with) =>
        IF m = M3CScope.Change.Exit THEN
          VAR
            iterBindings := SeqM3AST_AS_Binding.NewIter(with.as_binding_s);
          BEGIN
            (* following proc pops the bindings in reverse order *)
            PopWithBindings(with.as_binding_s, iterBindings);
          END;
        END; (* if *)
    | M3AST_AS.From_import, M3AST_AS.Simple_import =>
        (* should be resolved already *)
        IF m = M3CScope.Change.Enter THEN ASTWalk.IgnoreChildren(cl) END;

    | M3AST_AS.Qual_used_id(qualId) =>
        IF m = M3CScope.Change.Enter THEN QualIdPass1(cl, qualId) END;

    | M3AST_AS.BINARY, M3AST_AS.Select,
      M3AST_AS.Call,
      M3AST_AS.Constructor,
      M3AST_AS.Used_interface_id, M3AST_AS.Used_def_id, 
      M3AST_AS.Exp_used_id =>
        IF m = M3CScope.Change.Enter THEN Resolve(cl, a, e); END;

    ELSE
      (* nothing *)
    END; (* case *)
  END SetPass1;


PROCEDURE Resolve(
    cl: ASTWalk.Closure;
    a: AST.NODE;
    <*UNUSED*> vm: ASTWalk.VisitMode)
    RAISES {}=
  BEGIN
    TYPECASE a OF
    | M3AST_AS.Select(select) =>
        SelectPass1(cl, select);
    | M3AST_AS.Call(call) =>
        (* The keyword parameters can't be handled until the second pass *)
        CallPass1(cl, call);
    | M3AST_AS.Constructor(constructor) =>
        (* The keyword elements can't be handled until the second pass *)
        ConstructorPass1(cl, constructor);
    | M3AST_AS.Qual_used_id(qualId) =>
        QualIdPass1(cl, qualId);
    | M3AST_AS.Used_interface_id, M3AST_AS.Used_def_id =>
        M3CScope.Lookup(a);
    | M3AST_AS.Exp_used_id(exp_used_id) =>
        M3CScope.Lookup(exp_used_id.vUSED_ID);
    ELSE (* no action *)
    END;
  END Resolve;


PROCEDURE QualIdPass1(
    cl: ASTWalk.Closure;
    q: M3AST_AS.Qual_used_id)
    RAISES {}=
  VAR
    intfId := q.as_intf_id;
  BEGIN
    ASTWalk.IgnoreChildren(cl);
    IF intfId = NIL THEN
      M3CScope.Lookup(q.as_id);
    ELSE
      M3CScope.Lookup(intfId);
      ResolveInterfaceId(intfId.sm_def, q.as_id);
    END;
  END QualIdPass1;


PROCEDURE CallPass1(
    cl: ASTWalk.Closure;
    call: M3AST_AS.Call)
    RAISES {}=
  BEGIN
    ASTWalk.IgnoreChildren(cl);
    ASTWalk.VisitNodes(call.as_callexp,     <* NOWARN *>
                       NEW(ASTWalk.Closure, callback := Resolve).init());
    VAR
      iterActuals := SeqM3AST_AS_Actual.NewIter(call.as_param_s);
      actual: M3AST_AS.Actual;
    BEGIN
      WHILE SeqM3AST_AS_Actual.Next(iterActuals, actual) DO
        ASTWalk.VisitNodes(                 <* NOWARN *>
            actual.as_exp_type,
            NEW(ASTWalk.Closure, callback := Resolve).init());
      END; (* while *)
    END;
  END CallPass1;


PROCEDURE ConstructorPass1(
    cl: ASTWalk.Closure;
    cons: M3AST_AS.Constructor)
    RAISES {}=
  VAR
    a: AST.NODE;
    iter: SeqM3AST_AS_CONS_ELEM.Iter;
    element: M3AST_AS.CONS_ELEM;
  BEGIN
    ASTWalk.IgnoreChildren(cl);
    ASTWalk.VisitNodes(cons.as_type,       <* NOWARN *>
                       NEW(ASTWalk.Closure, callback := Resolve).init());
    iter := SeqM3AST_AS_CONS_ELEM.NewIter(cons.as_element_s);
    WHILE SeqM3AST_AS_CONS_ELEM.Next(iter, element) DO
      TYPECASE element OF
      | M3AST_AS.Actual_elem(actualElem) =>
          (* Avoid resolving keywords of any keyword actuals.. *)
          a := actualElem.as_actual.as_exp_type;
      ELSE
        a := element;
      END; (* if *)
      ASTWalk.VisitNodes(a, NEW(ASTWalk.Closure,      <* NOWARN *>
                                callback := Resolve).init());
    END; (* while *)
  END ConstructorPass1;


PROCEDURE SelectPass1(
    cl: ASTWalk.Closure;
    select: M3AST_AS.Select)
    RAISES {}=
  BEGIN
    (* We cannot resolve the identifier to the right of the '.',
    since the search depends on the type of the left hand side, which
    is not known at this point (it depends on the resolution of the names
    in the left hand side).  So we truncate the walk and recursively resolve
    the left hand side. *)
    ASTWalk.IgnoreChildren(cl);
    ASTWalk.VisitNodes(select.as_exp,              <* NOWARN *>
                       NEW(ASTWalk.Closure, callback := Resolve).init());
  END SelectPass1;


PROCEDURE SelectPass2(select: M3AST_AS.Select) RAISES {}=
  VAR
    exp_type, type_spec: M3AST_SM.TYPE_SPEC_UNSET;
    used_id: M3AST_AS.USED_ID;
    valid: BOOLEAN;
    a1: M3AST_AS.EXP;
  BEGIN
    (* Now we have to resolve the identifier on the right hand side of the
     '.' based on the type of the left. *)

    a1 := select.as_exp;
    exp_type :=
        M3CTypesMisc.Reveal(M3CTypesMisc.CheckedUnpack(a1.sm_exp_type_spec));

    (* If there has been an error already, leaving the exp type unset, then
     we don't bother with any further checking or resolving *)
    IF exp_type = NIL THEN RETURN END;

    used_id := select.as_id.vUSED_ID;

    (* If 'used_id' has a NIL name we have no hope of resolving it *)
    IF used_id.lx_symrep = NIL THEN RETURN END;

    valid := TRUE; (* we're optimistic to start with *)

    CASE M3CExpsMisc.Classify(a1) OF

    | M3CExpsMisc.Class.Interface =>
        (* assert 'a1' must be an Exp_used_id, resolved to an interface id *)
        ResolveInterfaceId(NARROW(a1, M3AST_AS.Exp_used_id).vUSED_ID.sm_def, used_id);

    | M3CExpsMisc.Class.Type =>
        TYPECASE exp_type OF
        | M3AST_AS.Enumeration_type(enumType) =>
            M3CSearch.Member(enumType, used_id);
        | M3AST_AS.Object_type(objectType) =>
            M3CSearch.FieldOrMethod(objectType, TRUE, used_id);
        ELSE
          valid := FALSE;
        END;

    | M3CExpsMisc.Class.Normal =>
        TYPECASE exp_type OF
        | M3AST_AS.Record_type(record_type) =>
            M3CSearch.Field(record_type, used_id);
        | M3AST_AS.Ref_type(ref_type) =>
            M3CTypesMisc.GetTYPE_SPECFromM3TYPE(ref_type.as_type, type_spec);
            TYPECASE type_spec OF
            | NULL =>
            | M3AST_AS.Record_type(recordType) =>
                M3CSearch.Field(recordType, used_id);
            ELSE
              valid := FALSE;
            END;
        | M3AST_AS.Object_type(object_type) =>
            M3CSearch.FieldOrMethod(object_type, FALSE, used_id);
        ELSE
          valid := FALSE;
        END;

    ELSE (* illegal, no action *)
        valid := FALSE;
    END; (* case *)

    IF NOT valid THEN
      M3Error.Report(a1,
          "expression on the left of \'.\' cannot be selected");
    END;
  END SelectPass2;


PROCEDURE ResolveInterfaceId(
    defId: M3AST_SM.DEF_ID_UNSET;
    usedId: M3AST_AS.USED_ID)
    RAISES {}=
  BEGIN
    TYPECASE defId OF
    | NULL =>
    | M3AST_AS.Interface_id(intfId) =>
        (* qualified name *)
        M3CSearch.Export(intfId.sm_spec, usedId);
    | M3AST_AS.Interface_AS_id(localId) =>
        ResolveInterfaceId(localId.tmp_used_id.sm_def, usedId);
    ELSE
      M3Error.ReportWithId(usedId,
          "identifer \'%s\' on left of \'.\' is not an interface",
          defId.lx_symrep);
    END;
  END ResolveInterfaceId;


PROCEDURE ResolveActualKeyword(
    keyword: M3AST_AS.Exp_used_id;
    defId: M3AST_AS.DEF_ID)
    RAISES {}=
  BEGIN
    keyword.vUSED_ID.sm_def := defId;
  END ResolveActualKeyword;


BEGIN
END M3CDef.
