INTERFACE M3CTypeSpec;

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

IMPORT AST, M3AST_AS, M3AST_SM, ASTWalk;


PROCEDURE SetPass1(an: AST.NODE) RAISES {};
(* This pass relies on 'sm_def' being set (for TYPED_IDs). It sets up the
'sm_type_spec' attribute for all explicitly typed ids and for 'Named_type's. *)

PROCEDURE NewSetPass2Closure(unit: M3AST_AS.UNIT): ASTWalk.Closure RAISES {};
(* Creates a closure for use in the second pass. Pass 2 calls 'M3CExpTypeSpec'
to set up the 'sm_exp_type_spec' attribute of expressions and also sets the
'sm_type_spec' of all ids which are implicitly typed by their initializing
expressions.  Should only be called with 'vm = ASTWalk.VisitMode.Exit' *)

PROCEDURE OfOverride(
    override: M3AST_AS.Override)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {};
(* Returns the type spec of 'override' and sets the 'sm_type_spec' field of the
corresponding override_id (override.as_id). Assumes 'SetPass1' has already 
been called and uses 'M3CConcTypeSpec.CurrentReveal'. *)

END M3CTypeSpec.







