INTERFACE M3CDef;

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


PROCEDURE SetPass1(cl: ASTWalk.Closure;
                   an: AST.NODE;
                   e: ASTWalk.VisitMode) RAISES {};
(* Set the (simple) 'sm_def' attribute in 'an'.  This depends on no other
attributes having been computed, but requires an OnEntryAndExit tree walk.
Only the leftmost id in a select operation is resolved - those to the right
depend on the type of this id, which is not known until 'sm_type_spec'
is known (which in turn depends on simple 'sm_def's being set). Note that
qualids which are not in expressions are fully resolved by 'SetPass1'; they,
unlike qualids in expressions, cannot be confused with select operations.
'SetPass1' calls 'IgnoreChildren' and hence is not suitable for a "parallel"
walk with another procedure.
*)

PROCEDURE SelectPass2(s: M3AST_AS.Select) RAISES {};
(* Set the 'sm_def' attribute of the rhs of the given selection. This depends
on the 'sm_def' and 'sm_exp_type_spec' of the left hand side being set up *)

PROCEDURE ResolveInterfaceId(
    defId: M3AST_SM.DEF_ID_UNSET;
    usedId: M3AST_AS.USED_ID)
    RAISES {};
(* This also behaves like 'SelectPass2', in that it sets the 'sm_def' 
attribute of 'usedId', given that 'defId' is an Interface_id (or
Interface_AS_id). *)

PROCEDURE ResolveActualKeyword(
    keyword: M3AST_AS.Exp_used_id;
    defId: M3AST_AS.DEF_ID)
    RAISES {};
(* 'keyword' is the keyword in a keyword procedure argument or constructor
element. 'ResolveActualKeyword' is called to bind it to its definition. This
is done by callback because the effort involved in resolving keyword arguments
is quite large, especially for NEW, so it is worth trying to avoid duplicating
it *)

END M3CDef.
