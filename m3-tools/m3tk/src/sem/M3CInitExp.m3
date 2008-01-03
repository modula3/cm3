MODULE M3CInitExp;

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

IMPORT M3AST_AS_F, M3AST_SM_F;

IMPORT SeqM3AST_AS_Var_id, SeqM3AST_AS_FORMAL_ID, SeqM3AST_AS_Field_id;

PROCEDURE Set(an: M3AST.NODE) RAISES {}=
  VAR
    varId: M3AST_AS.Var_id;
    formalId: M3AST_AS.FORMAL_ID;
    fieldId: M3AST_AS.Field_id;
    expVoid: M3AST_AS.EXP_NULL;
    iterVarIds: SeqM3AST_AS_Var_id.Iter;
    iterFormalIds: SeqM3AST_AS_FORMAL_ID.Iter;
    iterFieldIds: SeqM3AST_AS_Field_id.Iter;
  BEGIN  
    TYPECASE an OF

    | M3AST_AS.Const_decl(constDecl) =>
        constDecl.as_id.vINIT_ID.sm_init_exp := constDecl.as_exp;

    | M3AST_AS.Var_decl(varDecl) =>
        expVoid := varDecl.as_default;
        iterVarIds := SeqM3AST_AS_Var_id.NewIter(varDecl.as_id_s);
        WHILE SeqM3AST_AS_Var_id.Next(iterVarIds, varId) DO
          varId.vINIT_ID.sm_init_exp := expVoid;
        END; (* while *)

    | M3AST_AS.Formal_param(formalParam) => (* very like a Var_decl *)
        expVoid := formalParam.as_default;
        iterFormalIds := SeqM3AST_AS_FORMAL_ID.NewIter(formalParam.as_id_s);
        WHILE SeqM3AST_AS_FORMAL_ID.Next(iterFormalIds, formalId) DO
          TYPECASE formalId OF
          | M3AST_AS.F_Value_id(valueId) =>
              valueId.vINIT_ID.sm_init_exp := expVoid;
          | M3AST_AS.F_Readonly_id(readonlyId) =>
              readonlyId.vINIT_ID.sm_init_exp := expVoid;
          ELSE
          END;
        END; (* while *)

    | M3AST_AS.Fields(fields) =>
        expVoid := fields.as_default;
        iterFieldIds := SeqM3AST_AS_Field_id.NewIter(fields.as_id_s);
        WHILE SeqM3AST_AS_Field_id.Next(iterFieldIds, fieldId) DO
          fieldId.vINIT_ID.sm_init_exp := expVoid;
        END; (* while *)

    | M3AST_AS.Binding(binding) =>
        binding.as_id.vINIT_ID.sm_init_exp := binding.as_exp;

    | M3AST_AS.For_st(for) =>
        for.as_id.vINIT_ID.sm_init_exp := for.as_from;

    | M3AST_AS.METHOD_OVERRIDE(m_o) =>
        m_o.as_id.vINIT_ID.sm_init_exp := m_o.as_default;
    ELSE
    END; (* case *)
  END Set;


BEGIN
END M3CInitExp.
