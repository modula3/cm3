INTERFACE M3CExpValue;

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

(* Setting/checking the 'sm_exp_value' attribute, which depends on the 
previous computation of 'sm_def', 'sm_type_spec' (1), 'sm_exp_type_spec' and
'sm_actual_s' for constructors and standard procedures.

Also provides utility procedure which evaluate ordinal expressions, as
integers. *)

IMPORT AST, M3AST_AS, M3AST_SM, ASTWalk;
IMPORT M3CBackEnd;

TYPE
  Closure <: ASTWalk.Closure;


PROCEDURE NewClosure(
    interface: BOOLEAN;
    node: AST.NODE := NIL)
    : Closure
    RAISES {};
(* Create a new closure for use with the tree walker and the 'Set' procedure
below. The two arguments are used to determine whether it is necessary to check
that any expressions found in the tree walk are constant.
 'interface' is needed because var declarations in interfaces must have
constant defaults.
 The 'node' argument is necessary if the tree walk is to be started inside:
1) a TYPE_SPEC (all expressions in types are constant)
2) a Const_decl
3) the label list of a Case node
4) a Var_decl node in an interface
If so 'node' should be set to the TYPE_SPEC, Const_decl, Case or Var_decl node
in question. The walk should never be started inside an expression as there
are some special cases which cannot be handled without knowing the whole
expression (e.g. arguments to LAST etc. can be variable even in a constant
expression). *)

PROCEDURE Set(
    cl: Closure;
    an: AST.NODE;
    vm: ASTWalk.VisitMode)
    RAISES {};
(* Sets the 'sm_exp_value' attribute for all constant expressions. Should only
be used with a treewalker visiting 'OnEntryAndExit'. Also checks that any
expressions which must be constant really are  *)


(* Support procedures for other compiler passes. *)

PROCEDURE Number(
    ts: M3AST_SM.TYPE_SPEC_UNSET; 
    VAR (*out*) number: M3AST_SM.Exp_value): M3CBackEnd.NumStatus RAISES {};
(* An interface which translates the resulting sm_exp_value and
returns an INTEGER to aid the compiler in implementing the subtype
rule for arrays. *)

PROCEDURE Ordinal(e: M3AST_AS.EXP; 
    VAR (*out*) i: INTEGER): M3CBackEnd.NumStatus RAISES {};
(* If 'e' is an ordinal expression of known value, converts its value into an
integer and returns 'Valid'. If overflow occurs 'i' is not updated and
'Overflow' is returned. If 'e' is not an ordinal expression or has unknown
value 'i' is not updated and 'Unknown' is returned *)

PROCEDURE GetBounds(
    ts: M3AST_SM.TYPE_SPEC_UNSET; 
    VAR (*out*) low, high: M3AST_SM.Exp_value): M3CBackEnd.NumStatus RAISES {};
(* Return the bounds of an ordinal or packed ordinal type. A return of
'Unknown' means one of many things was incorrect (e.g. type NIL or not ordinal,
error evaluating subrange bounds etc.) and it leaves 'low' and 'high'
unchanged. *)

PROCEDURE Equal(e1, e2: M3AST_AS.EXP): BOOLEAN RAISES {};
(* If 'e1' and 'e2' are constant expressions and appear to be equal returns
TRUE, otherwise returns FALSE. Is sometimes optimistic; for a thorough check
a call of equal must be guarded by a check that the types of 'e1' and 'e2'
are compatible.
  'Equal' handles all kinds of expressions, including constructors *)

END M3CExpValue.
