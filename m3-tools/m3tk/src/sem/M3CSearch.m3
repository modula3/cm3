MODULE M3CSearch;

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

IMPORT M3AST_LX, M3AST_AS;
IMPORT M3CId; (* to reveal an M3AST_LX.Symrep *)

IMPORT M3AST_AS_F, M3AST_SM_F, M3AST_TM_F;

IMPORT SeqM3AST_AS_Enum_id;
IMPORT M3Error;
IMPORT M3ASTNext;


PROCEDURE Export(
    int: M3AST_AS.Interface;
    id: M3AST_AS.USED_ID)
    RAISES {}=
  VAR
    symrep := id.lx_symrep;
    hashId: REFANY;
  BEGIN
    IF symrep = NIL THEN RETURN END;
    IF int.tmp_def_id_table.get(symrep.atom, hashId) THEN
      id.sm_def := hashId;
    ELSE
      (* if we get here we failed *)
      M3Error.ReportWithId(id, "\'%s\' not found in interface \'%s\'",
          symrep, NARROW(int.as_id, M3AST_AS.Interface_id).lx_symrep);
    END;
  END Export;


PROCEDURE Member(
    enum: M3AST_AS.Enumeration_type;
    id: M3AST_AS.USED_ID)
    RAISES {}=
  VAR
    symrep := id.lx_symrep;
    iterEnums: SeqM3AST_AS_Enum_id.Iter;
    member: M3AST_AS.Enum_id;
  BEGIN
    IF symrep = NIL THEN RETURN END;
    iterEnums := SeqM3AST_AS_Enum_id.NewIter(enum.as_id_s);
    WHILE SeqM3AST_AS_Enum_id.Next(iterEnums, member) DO
      IF member.lx_symrep = symrep THEN
        id.sm_def := member;
        RETURN;
      ELSE
        (* continue search *)
      END; (* if *)
    END; (* while *)
    M3Error.ReportWithId(id,
        "Enumeration member \'%s\' not found", symrep);
  END Member;


PROCEDURE Field(
    record: M3AST_AS.Record_type;
    id: M3AST_AS.USED_ID)
    RAISES {}=
  VAR
    symrep := id.lx_symrep;
    iter: M3ASTNext.IterField;
    fieldId: M3AST_AS.Field_id;
  BEGIN
    IF symrep = NIL THEN RETURN END;
    iter := M3ASTNext.NewIterField(record.as_fields_s);
    WHILE M3ASTNext.Field(iter, fieldId) DO
      IF fieldId.lx_symrep = symrep THEN
        id.sm_def := fieldId;
        RETURN;
      ELSE
        (* continue search *)
      END; (* if *)
    END; (* while *)
    M3Error.ReportWithId(id, "Record field \'%s\' not found", symrep);
  END Field;


PROCEDURE FieldOrMethod(
    object: M3AST_AS.Object_type;
    methodsOnly: BOOLEAN;
    id: M3AST_AS.USED_ID)
    RAISES {}=
  VAR
    symrep := id.lx_symrep;
    iter: M3ASTNext.IterFieldOrMethod;
    fieldId: M3AST_AS.Field_id;
    meth: M3AST_AS.Method;
    fieldOrMethodSymrep: M3AST_LX.Symbol_rep;
  BEGIN
    IF symrep = NIL THEN RETURN END;
    iter := M3ASTNext.NewIterFieldOrMethod(object);
    WHILE M3ASTNext.FieldOrMethod(iter, fieldId, meth, fieldOrMethodSymrep) DO
      IF symrep = fieldOrMethodSymrep THEN
        IF meth = NIL THEN
          IF methodsOnly THEN
            M3Error.ReportWithId(id,
                "\'%s\' is a field not a method", symrep);
          ELSE
            id.sm_def := fieldId;
          END;
        ELSE
          id.sm_def := meth.as_id;
        END;
        RETURN
      END;
    END; (* while *)
    M3Error.ReportWithId(id,
        "object field or method \'%s\' not found", symrep);
  END FieldOrMethod;


PROCEDURE Formal(
    proc: M3AST_AS.Procedure_type;
    id: M3AST_AS.USED_ID)
    RAISES {}=
  VAR
    symrep := id.lx_symrep;
    iter: M3ASTNext.IterFormal;
    formal: M3AST_AS.Formal_param;
    formalId: M3AST_AS.FORMAL_ID;
  BEGIN
    IF symrep = NIL THEN RETURN END;
    iter := M3ASTNext.NewIterFormal(proc.as_formal_param_s);
    WHILE M3ASTNext.Formal(iter, formal, formalId) DO
      IF formalId.lx_symrep = symrep THEN
        id.sm_def := formalId;
        RETURN;
      ELSE
        (* continue search *)
      END; (* if *)
    END; (* while *)
    M3Error.ReportWithId(id,
        "Formal parameter \'%s\' not found", symrep);
  END Formal;


BEGIN
END M3CSearch.
