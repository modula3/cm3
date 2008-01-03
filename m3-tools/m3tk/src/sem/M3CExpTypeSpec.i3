INTERFACE M3CExpTypeSpec;

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

IMPORT M3AST_AS, M3AST_SM;

PROCEDURE Set(exp: M3AST_AS.EXP; unit: M3AST_AS.UNIT) RAISES {};
(* Set 'sm_exp_type_spec' attribute of 'exp' and return it as the result.
  It is assumed that the components of 'exp' already have their
'sm_exp_type_spec' fields set. Thus this procedure is suitable for use by an
'ASTWalk.VisitMode.Exit' mode tree walk. Internally 'Set' may have to call
itself recursively to evaluate forward references; this does not concern the
tree walk as 'Set' can safely be called twice on the same expression.
  If the 'sm_exp_type_spec' attribute of 'exp' is already set, 'Set' does
nothing.
  'unit' gives the compilation unit in which 'exp' occurs.
  Assumes 'sm_def' attribute is set up. *)

PROCEDURE RecursiveVariableType(
    varId: M3AST_AS.Var_id;
    ts: M3AST_AS.TYPE_SPEC)
    : BOOLEAN
    RAISES {};
(* Unfortunately the rules for variable recursion when a variable is typed by
its initializing expression are somewhat messy. This procedure is used if
'varId' has an initializing expression whose type is 'ts'. 'ts' must be non
NIL. 'RecursiveVariableType' returns TRUE and sets 'varId.tmp_recursive' to
TRUE if 'ts' recursively uses 'varId'. Otherwise FALSE is returned *)

PROCEDURE BaseType(
    ts: M3AST_SM.TYPE_SPEC_UNSET)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {};
(* If 'ts' is unset, integer or an enumeration returns 'ts'
If 'ts' is a subrange type, returns the base type of the subrange.
If 'ts' is packed unpacks it and the returns to the beginning (i.e. checks if
it is unset, integer etc.)
Otherwise returns NIL.
  May call 'Set' to find the type of the bounding expressions of a subrange.
  This function is useful while the types of expressions are being set up.
Later passes of the compiler can use the 'sm_base_type_spec' attribute of a
subrange, which is set up AFTER expressions have been typed. *)

END M3CExpTypeSpec.
