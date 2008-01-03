INTERFACE M3CTypesMisc;

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
IMPORT SeqM3AST_AS_Fields;


PROCEDURE GetTYPE_SPECFromM3TYPE(
    t: M3AST_AS.M3TYPE; 
    VAR (*out*) ts: M3AST_SM.TYPE_SPEC_UNSET)
    RAISES {};
(* Gets the TYPE_SPEC associated with 't', going through a named type
if necessary. *)


PROCEDURE Unpack(
    p: M3AST_AS.Packed_type)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {};
(* take a packed type and return the type being packed *)

PROCEDURE CheckedUnpack(
    t: M3AST_SM.TYPE_SPEC_UNSET)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {};
(* if 't' is a packed type unpack it and put the result in 'unpacked'.
Otherwise assign 't' to 'unpacked'. Returns the type of 'unpacked' *)

PROCEDURE Reveal(
    ts: M3AST_SM.TYPE_SPEC_UNSET)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {};
(* a layer on 'M3CConcTypeSpec.CurrentReveal'; if 'ts' is an opaque type finds
its current revelation. If the current revelation is another opaque type finds
its revelation and so on. The result of a call to 'Reveal' is guaranteed not
to be an opaque type *)

PROCEDURE Concrete(
    ts: M3AST_SM.TYPE_SPEC_UNSET)
    : M3AST_SM.TYPE_SPEC_UNSET
    RAISES {};
(* If 'ts' is an opaque type and the revelation currently in scope is the
ultimate revelation (i.e. REVEAL Type =) returns the concrete type of 'ts' as
revealed in the ultimate revelation. Otherwise returns 'ts' *)

PROCEDURE IsConcrete(
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    fully: BOOLEAN)
    : BOOLEAN
    RAISES {};
(* Is 'ts' a concrete type i.e. NOT an opaque type for with no ultimate
revelation in scope. If 'fully' is TRUE and 'ts' is an object type (or an
opaque type revealed to be an object type) 'IsConcrete' also checks that all
supertypes of the object are concrete *)

TYPE
  Ix = {Unknown, Ordinal, Open, Bad};

PROCEDURE Index(
    a: M3AST_AS.Array_type;
    VAR index: M3AST_SM.TYPE_SPEC_UNSET)
    : Ix
    RAISES {};
(* Returns an 'Index' for the (first) index in 'a'. If the result is
'Ix.Open' 'index' is left unchanged. Otherwise 'index' is set to index type
found (even if it is bad or unset). Note that 'Ix.Unknown' indicates an
unset index type or an index type which has some component unset but is not
obviously a bad index. e.g. a reference type with an unset referent is
a bad index; a packed type with an unset base type is an unknown index *)

PROCEDURE Indexable(
    ts: M3AST_SM.TYPE_SPEC_UNSET;
    VAR a: M3AST_AS.Array_type)
    : BOOLEAN
    RAISES {};
(* Checks if 'ts' is suitable for indexing i.e. a (possibly packed) array type
or a reference to a (possibly packed) array type. If so sets 'a' to be the
underlying array type and returns TRUE, otherwise leaves 'a' unchanges and
returns FALSE. If 'ts' is NIL or the process of unpacking/dereferencing
reaches a NIL type 'a' is set to NIL and TRUE is returned *)


PROCEDURE IsRef(ts: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};
(* Checks if 'ts' is a reference type (traced or untraced). Optimistic -
returns TRUE if ts is NIL *)


(* Note that the following 'IsTracedX' procedures and 'ContainsTracedFields'
may use the procedure 'M3CConcTypeSpec.CurrentReveal' to discover the current
revelation for an opaque type *)

TYPE
  Ref = {Traced, Untraced, Null, Not, Unknown};
  RefSet = SET OF Ref;

CONST
  ProbablyTraced = RefSet{Ref.Traced, Ref.Null, Ref.Unknown};
  ProbablyUntraced = RefSet{Ref.Untraced, Ref.Null, Ref.Unknown};
  DefinitelyRef = RefSet{Ref.Traced, Ref.Untraced, Ref.Null};
  ProbablyRef = DefinitelyRef + RefSet{Ref.Unknown};

PROCEDURE IsTracedRef(type: M3AST_SM.TYPE_SPEC_UNSET): Ref RAISES {};
(* if 'type' is a reference or object type returns 'Ref.Traced' or
'Ref.Untraced' in the obvious way. If 'type' is unset or is an errant reference
type whose traceability cannot be discovered returns 'Ref'. Otherwise
returns 'Ref.Not' *)

PROCEDURE IsTracedObject(o: M3AST_AS.Object_type): Ref RAISES {};
(* is 'o' traced? Never returns 'Ref.Not' or 'Ref.Null' but may return
'Ref.Unknown' if the super type chain for 'o' is errant in some way *)

PROCEDURE IsTraced(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};
(* is the given type traced? Uses the more general definition of traced
used in the report which includes not only traced reference types but also e.g.
records containing traced reference types. Returns FALSE if 'type' is unset *)

PROCEDURE ContainsTracedFields(fields: SeqM3AST_AS_Fields.T): BOOLEAN RAISES {};
(* really a sub component of 'IsTraced', this procedure is exported separately
because it is useful for checking that there are no traced fields in an
untraced object. Returns TRUE if 'IsTraced' returns TRUE when called on the
type of any of the fields in 'fields' *)


PROCEDURE IsOpenArray(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};
(* returns TRUE if 'type' is an open array. This is a pessimistic procedure
in the sense that if 'type' is unset it returns FALSE. This departure from
the normal "optimistic" approach is because most uses of this procedure occur
when checking that something is NOT an open array rather than checking that it
is an open array. Hence, for this procedure, pessimistic is optimistic
(convinced?) *)


PROCEDURE IsEmpty(type: M3AST_SM.TYPE_SPEC_UNSET): BOOLEAN RAISES {};
(* is the given type empty i.e. a type with no possible values. Returns FALSE
if 'type' is unset. Here are the rules for empty types:
1) an enumeration is empty if it has no members
2) a subrange is empty if its lower bound is greater than its upper bound
3) an array is empty if its element type is empty but its index type is not
empty (think about trying to write a constructor for it!).
4) a packed type is empty if the type it is packing is empty
5) a record is never empty because you can always write a constructor for it.
The only exception is a record containing a field with an empty type which is
illegal anyway.
6) a set is never empty (it can always contain the empty set)
7) object and reference types are never empty (they can always be NIL)
8) a procedure type is never empty (it can always have a body) *)


PROCEDURE NoDefaultForMethod(
    o: M3AST_AS.Object_type;
    method: M3AST_AS.Method_id)
    : BOOLEAN
    RAISES {};
(* Checks 'o' to see if there is a default for method 'method'. If there is
definitely no default returns TRUE. Note that if 'o' has an opaque supertype
it may be impossible to tell if 'method' has a default *)


PROCEDURE HiddenObjectParameter(
    p: M3AST_AS.Procedure_type;
    VAR ts: M3AST_SM.TYPE_SPEC_UNSET)
    : BOOLEAN
    RAISES {};
(* Checks to see if 'p' is a method procedure type; if it is returns TRUE and
sets 'ts' to be the type of the hidden first parameter. Otherwise return FALSE.
e.g. the type 'T.m', where 'T' is an object type and 'm' is a method, is a
procedure type but the type does not include the "hidden" first parameter - a
VALUE parameter of type T, with unknown name. This procedure would return 'T'
if given the type of 'T.m' *)

END M3CTypesMisc.
