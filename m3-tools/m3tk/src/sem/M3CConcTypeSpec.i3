INTERFACE M3CConcTypeSpec;

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

IMPORT M3AST_AS, M3AST_SM, ASTWalk, M3Context;

PROCEDURE Set(cu: M3AST_AS.Compilation_Unit) RAISES {};
(* Set the sm_concrete_type_spec attribute for opaque types, which depends 
on sm_def and sm_type_spec. This also computes the list of direct/indirect
partial revelations for this scope. *)

PROCEDURE CurrentReveal(
    ts: M3AST_SM.TYPE_SPEC_UNSET)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {};
(* If 'ts' is an Opaque_type return the current revelation, else
return 'ts'. *)

PROCEDURE SetCurrentReveal(
    cu: M3AST_AS.Compilation_Unit;
    mode: ASTWalk.VisitMode)
    RAISES {};
(* Given that new revelations may enter/exit the picture as we enter/exit
this unit, recompute the current revelation for affected opaque types.
On exit we also check that all partial revelations in this scope were
consistent.
*)

PROCEDURE Validate(c: M3Context.T) RAISES {};
(* This procedure checks that the sm_concrete_type_spec etc. in an Opaque_type
is still valid.  If the unit making the revelation has been removed from
the context, for whatever reason, it wont be and needs to be reset. *)

END M3CConcTypeSpec.
