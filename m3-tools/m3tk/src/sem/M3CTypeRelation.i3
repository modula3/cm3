INTERFACE M3CTypeRelation;

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


<*INLINE*> PROCEDURE Identical(
    type1, type2: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
(* succeeds if the given types are identical. Is optimistic - succeeds if
either type is unset *)

PROCEDURE SubType(type1, type2: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};
(* succeeds if 'type1 <: type2'. Is optimistic - succeeds if either type is
unset *)

PROCEDURE Assignable(
    lhs, rhs: M3AST_SM.TYPE_SPEC_UNSET;
    safe: BOOLEAN)
    : BOOLEAN
    RAISES {};
(* succeeds if 'rhs' is assignable to 'lhs'. 'safe' determines whether it is
ok to assign an ADDRESS to an untraced REF. Is optimistic - succeeds if either
type is unset *)

PROCEDURE VARPassable(
    formalType, actualType: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
(* succeeds if 'actualType' can be passed to a VAR formal of 'formalType'. Is
optimistic - succeeds if either type is unset *)

PROCEDURE Covered(p1, p2: M3AST_AS.Procedure_type): BOOLEAN RAISES {};
(* succeeds if the signature of 'p1' is covered by the signature of 'p2' *)

PROCEDURE Satisfies(
    procedure: M3AST_AS.Procedure_type;
    object: M3AST_AS.Object_type;
    method: M3AST_AS.Procedure_type)
    : BOOLEAN
    RAISES {};
(* succeeds if 'procedure' is a suitable type for the default of the given
method in the given object type. *)

PROCEDURE SameReferenceClass(
    t1, t2: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
(* succeeds if both types are of the same reference class. This routine is
optimistic; it only fails if:
i) both 't1' and 't2' are reference types only one of which is traced
ii) either 't1' or 't2' is a non reference type e.g. an ordinal or record.
Any combination of unset and a reference type will succeed. Note that NULL,
the type of NIL, is considered to be both a traced and an untraced reference
type so any combination of NULL and another reference type will also succeed *)

<*INLINE*> PROCEDURE SameOrdinalSupertype(
    type1, type2: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
(* succeeds if both types are ordinal and have identical base types. Is a
little optimistic - succeeds if one type is unset and the other is ordinal. *)

END M3CTypeRelation.
