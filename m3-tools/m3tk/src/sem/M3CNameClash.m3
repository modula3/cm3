MODULE M3CNameClash;

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


IMPORT M3AST, M3AST_AS;

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT M3ASTNext;
IMPORT SeqM3AST_AS_Enum_id, SeqM3AST_AS_Fields, SeqM3AST_AS_Method,
    SeqM3AST_AS_Override;

IMPORT M3CId, M3Error, M3Assert, M3CScope_priv, M3Context;


TYPE
  Definitions = M3CScope_priv.Definitions;


PROCEDURE IsStandard(d: M3AST_AS.DEF_ID): BOOLEAN RAISES {}=
  VAR
    unit: M3AST_AS.UNIT;
  BEGIN
    unit := d.tmp_unit_id.sm_spec;
    RETURN unit.sm_comp_unit = M3Context.Standard();
  END IsStandard;


(***************************************************************
PROCEDURE AssertIsStandard(old, new: M3AST_AS.DEF_ID) RAISES {}=
  VAR
  BEGIN
    IF NOT IsStandard(old) THEN M3Assert.Fail() END;
    IF NOT IsStandard(new) THEN
      M3Error.ReportWithId(new,
          "illegal redefinition of standard identifier \'%s\'", new.lx_symrep);
    END;
  END AssertIsStandard;
*******************************************)


PROCEDURE Enter(
    node: M3AST.NODE;
    defId: M3AST_AS.DEF_ID;
    VAR list: Definitions)
    : BOOLEAN
    RAISES {}=
  VAR
    id := defId.lx_symrep;
    old, new: Definitions;
  BEGIN
    IF id = NIL THEN RETURN TRUE END;
    old := id.defs;
    IF old # NIL THEN
      IF old.hook = node THEN
        (* must be due to previous call of 'Enter', while processing
         the current type *)
        RETURN FALSE;
      ELSE
        VAR
          d := old;
        BEGIN
          WHILE d.enclosing # NIL DO d := d.enclosing END;
          IF IsStandard(d.defId) AND NOT IsStandard(defId) THEN
            M3Error.ReportWithId(defId,
                "illegal redefinition of standard identifier \'%s\'",
                defId.lx_symrep);
            RETURN TRUE;
          END;
        END;
      END; (* if *)
    END; (* if *)

    new := NEW(Definitions, next := list, enclosing := old, id := id, 
        defId := defId,
        scope := 0, hook := node);
    list := new;
    id.defs := new;
    RETURN TRUE;
  END Enter;


PROCEDURE DisposeList(list: Definitions) RAISES {}=
  BEGIN
    WHILE list # NIL DO
      IF list # list.id.defs THEN M3Assert.Fail() END;
      list.id.defs := list.enclosing;
      list := list.next;
    END; (* while *)
  END DisposeList;


PROCEDURE Enumeration(e: M3AST_AS.Enumeration_type) RAISES {}=
  VAR
    list: Definitions;
    iter: SeqM3AST_AS_Enum_id.Iter;
    enumId: M3AST_AS.Enum_id;
  BEGIN
    list := NIL;
    iter := SeqM3AST_AS_Enum_id.NewIter(e.as_id_s);
    WHILE SeqM3AST_AS_Enum_id.Next(iter, enumId) DO
      IF NOT Enter(e, enumId, list) THEN
        M3Error.ReportWithId(enumId,
            "name \'%s\' appears more than once in enumeration",
            enumId.lx_symrep);
      END; (* if *)
    END; (* while *)
    DisposeList(list);
  END Enumeration;


PROCEDURE Fields(
    node: M3AST.NODE;
    f: SeqM3AST_AS_Fields.T)
    : Definitions
    RAISES {}=
  VAR
    list: Definitions;
    iter: M3ASTNext.IterField;
    fieldId: M3AST_AS.Field_id;
  BEGIN
    list := NIL;
    iter := M3ASTNext.NewIterField(f);
    WHILE M3ASTNext.Field(iter, fieldId) DO
      IF NOT Enter(node, fieldId, list) THEN
        M3Error.ReportWithId(fieldId,
            "field name \'%s\' clashes with existing field name",
            fieldId.lx_symrep);
      END; (* if *)
    END; (* while *)
    RETURN list;
  END Fields;


PROCEDURE Record(r: M3AST_AS.Record_type) RAISES {}=
  VAR
    list: Definitions;
  BEGIN
    list := Fields(r, r.as_fields_s);
    DisposeList(list);
  END Record;


PROCEDURE Object(o: M3AST_AS.Object_type) RAISES {}=
  VAR
    list: Definitions;
    iter: SeqM3AST_AS_Method.Iter;
    defId: M3AST_AS.DEF_ID;
    method: M3AST_AS.Method;
  BEGIN
    list := Fields(o, o.as_fields_s);

    iter := SeqM3AST_AS_Method.NewIter(o.as_method_s);
    WHILE SeqM3AST_AS_Method.Next(iter, method) DO
      defId := method.as_id;
      IF NOT Enter(o, defId, list) THEN
        M3Error.ReportWithId(defId,
            "method name \'%s\' clashes with existing field or method name",
            defId.lx_symrep);
      END; (* if *)
    END; (* while *)

    VAR
      iter := SeqM3AST_AS_Override.NewIter(o.as_override_s);
      override: M3AST_AS.Override;
    BEGIN
      WHILE SeqM3AST_AS_Override.Next(iter, override) DO
        defId := override.as_id;
        IF NOT Enter(o, defId, list) THEN
          M3Error.ReportWithId(defId,
              "override method name \'%s\' clashes with existing field or method name",
              defId.lx_symrep);
        END; (* if *)
      END; (* while *)
    END;
    DisposeList(list);
  END Object;


PROCEDURE Procedure(p: M3AST_AS.Procedure_type) RAISES {}=
  VAR
    list: Definitions;
    iter: M3ASTNext.IterFormal;
    formal: M3AST_AS.Formal_param;
    formalId: M3AST_AS.FORMAL_ID;
  BEGIN
    TYPECASE p.sm_def_id OF
    | NULL =>
        (* Procedure type - needs checking *)
    | M3AST_AS.Proc_id =>
        (* part of a 'Proc_decl'; already checked by 'M3CScope' code *)
        RETURN;
    ELSE
      (* Method procedure type - needs checking *)
    END;
    list := NIL;
    iter := M3ASTNext.NewIterFormal(p.as_formal_param_s);
    WHILE M3ASTNext.Formal(iter, formal, formalId) DO
      IF NOT Enter(p, formalId, list) THEN
        M3Error.ReportWithId(formalId,
            "parameter name \'%s\' clashes with existing parameter name",
            formalId.lx_symrep);
      END; (* if *)
    END; (* while *)
    DisposeList(list);
  END Procedure;


BEGIN
END M3CNameClash.
