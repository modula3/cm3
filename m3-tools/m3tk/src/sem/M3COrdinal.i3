INTERFACE M3COrdinal;

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


PROCEDURE Is(
    t: M3AST_SM.TYPE_SPEC_UNSET;
    VAR (*out*) baseType: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
(* TRUE if 't' is an ordinal type. If 't' is a subrange or packed ordinal type
sets 'baseType' to the base type of 't' otherwise sets it to 't'. Note that
'baseType' can only be set to one of INTEGER, an enumeration or unset; e.g.
the base type of BITS 8 FOR [0..255] is INTEGER.
  If 't' is Unset optimistically returns TRUE, 'baseType' Unset.
  Relies on the sm_base_type_spec attribute of subranges *)

PROCEDURE SameSupertype(
    type1, type2: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
(* succeeds if both types are ordinal and have identical base types. Is a
little optimistic - succeeds if one type is unset and the other is ordinal.
  Relies on the sm_base_type_spec attribute of subranges *)

PROCEDURE ValidBounds(
    subrange: M3AST_AS.Subrange_type;
    VAR first, last: M3AST_SM.Exp_value)
    : BOOLEAN
    RAISES {};
(* return the bounds of a subrange providing that they are compatible with
each other (i.e. are ordinal and share the same supertype). If not 'first' and
'last' are left unchanged and FALSE is returned. Fails if the base type or the
type of either bound expression is unset.
  Relies on the sm_base_type_spec attribute of subranges
  Relies on the sm_exp_value attribute of subrange bound expressions
  Relies on the sm_exp_type_spec attribute of subrange bound expressions *)

PROCEDURE Identical(t1, t2: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};
(* succeeds if given types are identical ordinal types. 't1' must be an ordinal
or set type (it cannot even be unset) - if it is not a fatal error will be
raised (note 't1' can be packed, provided it is a packed ordinal). 't2' can be
any type. Special case: if 't2' is unset 'Identical' will optimistically
succeed even if 't1' is not an ordinal or set type.
  'Identical' deals with sets in addition to ordinals because it is used by
the 'SubType' procedure, below. Why 'SubType' deals with sets is also explained
below.
  Relies on the sm_base_type_spec attribute of subranges.
  Calls 'M3CBackEnd.Compare' on subrange bounds *)

PROCEDURE SubType(t1, t2: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};
(* succeeds if t1 <: t2 and t1 is an ordinal or set type. 't1' must be an
ordinal or set (it cannot even be unset) - if it is not a fatal error will be
raised (note 't1' can be packed, provided it is a packed ordinal). 't2' can be
any type. Special case: if 't2' is unset 'SubType' will always optimistically
succeed, even if 't1' is not an ordinal or set type.
  'SubType' deals with sets in addition to ordinals because set subtyping is
needed when the compiler is typing expressions (the 'M3CTypeRelation' subtype
routine assumes that expression typing has been done).
  Relies on the sm_base_type_spec attribute of subranges.
  Relies on the sm_type_spec attribute of named types
  Calls 'M3CBackEnd.Compare' on subrange bounds *)

PROCEDURE Overlap(t1, t2: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};
(* succeeds if both types are ordinal and they overlap (i.e. have at least
one member in common). Is optimistic and succeeds if one type is ordinal and
the other is unset.
  Relies on the sm_base_type_spec attribute of subranges.
  Calls 'M3CBackEnd.Compare' on subrange bounds *)

PROCEDURE IsMemberOf(
    sub: M3AST_AS.Subrange_type;
    exp: M3AST_AS.EXP)
    : BOOLEAN
    RAISES {};
(* checks if the given expression is a member of the given subrange. Is
optimistic - succeeds if the expression is not constant or if the subrange
base type is unset.
  Relies on the sm_base_type_spec attribute of subranges.
  Calls 'M3CBackEnd.Compare' on subrange bounds and 'exp' *)

END M3COrdinal.
