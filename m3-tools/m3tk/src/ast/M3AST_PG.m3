MODULE M3AST_PG;

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

PROCEDURE IsA_EXTERNAL_DECL(n: M3AST.NODE;
    VAR(*out*) external_decl: EXTERNAL_DECL): BOOLEAN RAISES {}=
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Interface(t) => external_decl := t.vEXTERNAL_DECL; RETURN TRUE;
    | M3AST_AS.DECL(t) => external_decl := t.vEXTERNAL_DECL; RETURN TRUE;
    ELSE RETURN FALSE;
    END;
  END IsA_EXTERNAL_DECL;

PROCEDURE IsA_EXTERNAL_ID(n: M3AST.NODE;
    VAR(*out*) external_id: EXTERNAL_ID): BOOLEAN RAISES {} =
  BEGIN
    TYPECASE n OF
    | M3AST_AS.Interface_id(t) => external_id := t.vEXTERNAL_ID; RETURN TRUE
    | M3AST_AS.Var_id(t) => external_id := t.vEXTERNAL_ID; RETURN TRUE
    | M3AST_AS.Proc_id(t) => external_id := t.vEXTERNAL_ID; RETURN TRUE
    | M3AST_AS.Exc_id(t) => external_id := t.vEXTERNAL_ID; RETURN TRUE
    | M3AST_AS.Type_id(t) => external_id := t.vEXTERNAL_ID; RETURN TRUE
    | M3AST_AS.Const_id(t) => external_id := t.vEXTERNAL_ID; RETURN TRUE
    ELSE RETURN FALSE
    END;
  END IsA_EXTERNAL_ID;

(*************************************************
PROCEDURE NewEXTERNAL_DECL(): EXTERNAL_DECL RAISES {} =
  BEGIN
    RETURN NEW(EXTERNAL_DECL).init();
  END NewEXTERNAL_DECL;

PROCEDURE NewInline(): Inline RAISES {} =
  BEGIN
    RETURN NEW(Inline).init();
  END NewInline;

PROCEDURE NewExternal(): External RAISES {} =
  BEGIN
    RETURN NEW(External).init();
  END NewExternal;

PROCEDURE NewEXTERNAL_ID(): EXTERNAL_ID RAISES {} =
  BEGIN
    RETURN NEW(EXTERNAL_ID);
  END NewEXTERNAL_ID;
********************************)


VAR
  ux_g := NEW(External_NULL_UNSET OBJECT END);

PROCEDURE UNSET_External(): External_NULL_UNSET RAISES {}=
  BEGIN
    RETURN ux_g;
  END UNSET_External;

BEGIN
END M3AST_PG.
