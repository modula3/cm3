MODULE M3CEncTypeSpec;

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

IMPORT M3AST, M3AST_AS, M3AST_SM;

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_Fields, SeqM3AST_AS_Method, SeqM3AST_AS_Override;

IMPORT M3ASTNext;


PROCEDURE Set(an: M3AST.NODE) RAISES {}=
  VAR
    fields: SeqM3AST_AS_Fields.T;
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    o: M3AST_AS.Object_type;
  BEGIN
    TYPECASE an OF
    | M3AST_AS.Record_type(recordType) =>
        fields := recordType.as_fields_s;
        ts := recordType;
        o := NIL;
    | M3AST_AS.Object_type(objectType) =>
        fields := objectType.as_fields_s;
        ts := objectType;
        o := objectType;
    ELSE
      RETURN
    END; (* if *)

    VAR
      iter := M3ASTNext.NewIterField(fields);
      field_id: M3AST_AS.Field_id;
    BEGIN
      WHILE M3ASTNext.Field(iter, field_id) DO
        field_id.vRECOBJ_ID.sm_enc_type_spec := ts;
      END; (* while *)
    END;

    IF o # NIL THEN
      VAR
        iter := SeqM3AST_AS_Method.NewIter(o.as_method_s);
        iter2 := SeqM3AST_AS_Override.NewIter(o.as_override_s);
        method: M3AST_AS.Method; override: M3AST_AS.Override;
      BEGIN
        WHILE SeqM3AST_AS_Method.Next(iter, method) DO
          method.as_id.vRECOBJ_ID.sm_enc_type_spec := ts;
        END; (* while *)     
        WHILE SeqM3AST_AS_Override.Next(iter2, override) DO
          override.as_id.vRECOBJ_ID.sm_enc_type_spec := ts;
        END; (* while *)     
      END;
    END; (* if *)

  END Set;


BEGIN
END M3CEncTypeSpec.
